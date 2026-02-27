;;; disco-gateway.el --- Discord Gateway transport for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; WebSocket-based Discord Gateway transport.
;;
;; Public contract intentionally stays stable:
;; - `disco-gateway-watch-channel'
;; - `disco-gateway-unwatch-channel'
;; - `disco-gateway-event-hook' events with
;;   :type one of message-create/message-update/message-delete,
;;   :channel-id and :message / :message-id payload.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'disco-api)
(require 'disco-customize)
(require 'disco-state)
(require 'websocket)

(defvar disco-gateway-event-hook nil
  "Hook called with one event plist argument.

Event schema:
- :type one of `message-create' `message-update' `message-delete'
- :channel-id string
- :message message object for create/update
- :message-id string for delete")

(defvar disco-gateway--watch-counts (make-hash-table :test #'equal))

(defvar disco-gateway--ws nil)
(defvar disco-gateway--connecting nil)
(defvar disco-gateway--stopping nil)

(defvar disco-gateway--seq nil)
(defvar disco-gateway--session-id nil)
(defvar disco-gateway--resume-url nil)

(defvar disco-gateway--heartbeat-interval-ms nil)
(defvar disco-gateway--heartbeat-timer nil)
(defvar disco-gateway--awaiting-heartbeat-ack nil)

(defvar disco-gateway--reconnect-timer nil)
(defvar disco-gateway--reconnect-attempt 0)

(defconst disco-gateway--zlib-suffix (string 0 0 255 255)
  "Z_SYNC_FLUSH suffix used by Discord zlib-stream transport.")

(defvar disco-gateway--zlib-stream-buffer ""
  "Accumulated compressed bytes for current gateway connection.")

(defvar disco-gateway--zlib-stream-output-bytes 0
  "Previously produced decompressed byte count from stream buffer.")

(defun disco-gateway-running-p ()
  "Return non-nil when gateway transport is active or connecting."
  (or disco-gateway--connecting
      (and disco-gateway--ws (websocket-openp disco-gateway--ws))))

(defun disco-gateway--emit (event)
  "Emit EVENT plist to `disco-gateway-event-hook'."
  (run-hook-with-args 'disco-gateway-event-hook event))

(defun disco-gateway--json-encode (obj)
  "Encode OBJ to compact JSON string."
  (let ((json-encoding-pretty-print nil)
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-encode obj)))

(defun disco-gateway--json-decode (text)
  "Decode TEXT JSON into alist/list object."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false :false))
    (json-read-from-string text)))

(defun disco-gateway--zlib-reset-state ()
  "Reset zlib-stream state for one websocket connection."
  (setq disco-gateway--zlib-stream-buffer "")
  (setq disco-gateway--zlib-stream-output-bytes 0))

(defun disco-gateway--transport-compression-string ()
  "Return gateway transport compression query value as string or nil."
  (when disco-gateway-transport-compression
    (if (symbolp disco-gateway-transport-compression)
        (symbol-name disco-gateway-transport-compression)
      disco-gateway-transport-compression)))

(defun disco-gateway--bytes-end-with-zlib-suffix-p (bytes)
  "Return non-nil when BYTES ends with Z_SYNC_FLUSH suffix."
  (let ((len (length bytes)))
    (and (>= len 4)
         (= (aref bytes (- len 4)) 0)
         (= (aref bytes (- len 3)) 0)
         (= (aref bytes (- len 2)) 255)
         (= (aref bytes (- len 1)) 255))))

(defun disco-gateway--zlib-stream-decompress-delta ()
  "Decompress accumulated zlib-stream bytes and return newly produced JSON text."
  (let ((buf (generate-new-buffer " *disco-gateway-zlib*")))
    (unwind-protect
        (with-current-buffer buf
          (set-buffer-multibyte nil)
          (insert disco-gateway--zlib-stream-buffer)
          (let ((ret (zlib-decompress-region (point-min) (point-max) t)))
            (unless ret
              (error "disco: zlib-stream decompression failed"))
            (let* ((output (buffer-string))
                   (output-len (length output))
                   (previous-len (min disco-gateway--zlib-stream-output-bytes output-len))
                   (delta (substring output previous-len)))
              (setq disco-gateway--zlib-stream-output-bytes output-len)
              (decode-coding-string delta 'utf-8 t))))
      (kill-buffer buf))))

(defun disco-gateway--handle-zlib-stream-bytes (bytes)
  "Process received compressed BYTES chunk for zlib-stream mode.

Return decoded JSON text when a full payload is ready, otherwise nil."
  (let ((raw bytes))
    (setq disco-gateway--zlib-stream-buffer
          (concat disco-gateway--zlib-stream-buffer raw))

    (if (and (integerp disco-gateway-zlib-max-buffer-bytes)
             (> (length disco-gateway--zlib-stream-buffer)
                disco-gateway-zlib-max-buffer-bytes))
        (progn
          (message "disco: zlib buffer exceeded %d bytes, reconnecting"
                   disco-gateway-zlib-max-buffer-bytes)
          (disco-gateway--reconnect 1 nil)
          (disco-gateway--zlib-reset-state)
          nil)
      (when (disco-gateway--bytes-end-with-zlib-suffix-p disco-gateway--zlib-stream-buffer)
        (disco-gateway--zlib-stream-decompress-delta)))))

(defun disco-gateway--frame-json-text (frame)
  "Decode websocket FRAME into JSON text when possible.

Return nil if FRAME does not yet contain a complete payload."
  (pcase (websocket-frame-opcode frame)
    ('text
     (websocket-frame-text frame))
    ('binary
     (if (eq disco-gateway-transport-compression 'zlib-stream)
         (disco-gateway--handle-zlib-stream-bytes (websocket-frame-payload frame))
       (decode-coding-string (websocket-frame-payload frame) 'utf-8 t)))
    (_ nil)))

(defun disco-gateway--disconnect-internal (&optional clear-session preserve-reconnect-timer)
  "Disconnect gateway transport.

If CLEAR-SESSION is non-nil, drop resume-related values too."
  (when (timerp disco-gateway--heartbeat-timer)
    (cancel-timer disco-gateway--heartbeat-timer)
    (setq disco-gateway--heartbeat-timer nil))
  (when (and (not preserve-reconnect-timer)
             (timerp disco-gateway--reconnect-timer))
    (cancel-timer disco-gateway--reconnect-timer)
    (setq disco-gateway--reconnect-timer nil))

  (setq disco-gateway--connecting nil)
  (setq disco-gateway--heartbeat-interval-ms nil)
  (setq disco-gateway--awaiting-heartbeat-ack nil)
  (disco-gateway--zlib-reset-state)

  (when disco-gateway--ws
    (ignore-errors (websocket-close disco-gateway--ws))
    (setq disco-gateway--ws nil))

  (when clear-session
    (setq disco-gateway--seq nil)
    (setq disco-gateway--session-id nil)
    (setq disco-gateway--resume-url nil)
    (disco-gateway--reset-reconnect-backoff)))

(defun disco-gateway--reconnect (&optional delay clear-session)
  "Reconnect websocket transport after optional DELAY.

If CLEAR-SESSION is non-nil, clear resume values first."
  (disco-gateway--schedule-reconnect delay)
  (disco-gateway--disconnect-internal clear-session t))

(defun disco-gateway--rand-unit ()
  "Return pseudo-random float in range [0.0, 1.0)."
  (/ (float (random 10000)) 10000.0))

(defun disco-gateway--random-between (min-value max-value)
  "Return pseudo-random float between MIN-VALUE and MAX-VALUE."
  (let ((min-num (float min-value))
        (max-num (float max-value)))
    (if (<= max-num min-num)
        min-num
      (+ min-num (* (disco-gateway--rand-unit) (- max-num min-num))))))

(defun disco-gateway--reset-reconnect-backoff ()
  "Reset reconnect backoff state after stable session recovery."
  (setq disco-gateway--reconnect-attempt 0))

(defun disco-gateway--backoff-delay-for-attempt (attempt)
  "Compute jittered reconnect delay for ATTEMPT (1-based)."
  (let* ((exp-index (max 0 (1- attempt)))
         (base (max 0.1 (float disco-gateway-reconnect-delay)))
         (multiplier (max 1.0 (float disco-gateway-reconnect-multiplier)))
         (max-delay (max base (float disco-gateway-reconnect-max-delay)))
         (raw (min max-delay (* base (expt multiplier exp-index))))
         (jitter-ratio (max 0.0 (float disco-gateway-reconnect-jitter)))
         (jitter-amplitude (* raw jitter-ratio))
         (offset (* (- (* 2.0 (disco-gateway--rand-unit)) 1.0)
                    jitter-amplitude)))
    (max 0.1 (+ raw offset))))

(defun disco-gateway--next-reconnect-delay ()
  "Compute exponential backoff reconnect delay with jitter."
  (setq disco-gateway--reconnect-attempt (1+ disco-gateway--reconnect-attempt))
  (disco-gateway--backoff-delay-for-attempt disco-gateway--reconnect-attempt))

(defun disco-gateway--send-op (op d)
  "Send one gateway payload with OP and D."
  (when (and disco-gateway--ws (websocket-openp disco-gateway--ws))
    (websocket-send-text
     disco-gateway--ws
     (disco-gateway--json-encode `((op . ,op) (d . ,d))))))

(defun disco-gateway--send-heartbeat ()
  "Send heartbeat (op 1) with latest sequence value."
  (when (and disco-gateway--ws (websocket-openp disco-gateway--ws))
    (setq disco-gateway--awaiting-heartbeat-ack t)
    (disco-gateway--send-op 1 (or disco-gateway--seq :null))))

(defun disco-gateway--heartbeat-tick ()
  "Heartbeat timer callback."
  (if disco-gateway--awaiting-heartbeat-ack
      (progn
        (message "disco: gateway heartbeat ACK timeout, reconnecting")
        (disco-gateway--reconnect nil nil))
    (disco-gateway--send-heartbeat)))

(defun disco-gateway--start-heartbeat (interval-ms)
  "Start heartbeat loop using INTERVAL-MS."
  (when (timerp disco-gateway--heartbeat-timer)
    (cancel-timer disco-gateway--heartbeat-timer))
  (setq disco-gateway--heartbeat-interval-ms interval-ms)
  (setq disco-gateway--awaiting-heartbeat-ack nil)
  (let ((interval-sec (/ (max interval-ms 1000) 1000.0)))
    (setq disco-gateway--heartbeat-timer
          (run-at-time interval-sec interval-sec #'disco-gateway--heartbeat-tick))
    (disco-gateway--send-heartbeat)))

(defun disco-gateway--identify-properties ()
  "Build identify properties payload.

This shape follows Discord gateway identify expectations."
  `((os . ,(symbol-name system-type))
    (browser . "disco.el")
    (device . "disco.el")))

(defun disco-gateway--identify-payload ()
  "Build identify payload body for Gateway opcode 2."
  (let ((payload
         `((token . ,(or disco-token ""))
           (properties . ,(disco-gateway--identify-properties))
           (compress . :false)
           (large_threshold . 250))))
    (when disco-gateway-identify-intents
      (setq payload (append payload `((intents . ,disco-gateway-identify-intents)))))
    (when disco-gateway-identify-capabilities
      (setq payload
            (append payload `((capabilities . ,disco-gateway-identify-capabilities)))))
    (when disco-gateway-identify-presence
      (setq payload (append payload `((presence . ,disco-gateway-identify-presence)))))
    payload))

(defun disco-gateway--send-identify ()
  "Send identify payload (op 2)."
  (disco-gateway--send-op 2 (disco-gateway--identify-payload)))

(defun disco-gateway--can-resume-p ()
  "Return non-nil if resume payload can be sent."
  (and disco-gateway--session-id disco-gateway--seq))

(defun disco-gateway--send-resume ()
  "Send resume payload (op 6)."
  (disco-gateway--send-op
   6
   `((token . ,(or disco-token ""))
     (session_id . ,disco-gateway--session-id)
     (seq . ,disco-gateway--seq))))

(defun disco-gateway--schedule-reconnect (&optional delay)
  "Schedule reconnect after DELAY seconds."
  (let* ((attempt (1+ disco-gateway--reconnect-attempt))
         (max-attempts disco-gateway-max-reconnect-attempts))
    (if (and (integerp max-attempts)
             (> attempt max-attempts))
        (progn
          (setq disco-gateway--stopping t)
          (disco-gateway--disconnect-internal nil)
          (message "disco: reached max reconnect attempts (%d), gateway stopped"
                   max-attempts))
      (setq disco-gateway--reconnect-attempt attempt)
      (let ((effective-delay (or delay (disco-gateway--backoff-delay-for-attempt attempt))))
        (when (timerp disco-gateway--reconnect-timer)
          (cancel-timer disco-gateway--reconnect-timer))
        (setq disco-gateway--reconnect-timer
              (run-at-time
               effective-delay
               nil
               (lambda ()
                 (setq disco-gateway--reconnect-timer nil)
                 (unless disco-gateway--stopping
                   (disco-gateway--connect)))))
        (message "disco: scheduling gateway reconnect in %.2fs (attempt %d)"
                 effective-delay
                 attempt)))))

(defun disco-gateway--channel-watched-p (channel-id)
  "Return non-nil if CHANNEL-ID has active watchers."
  (> (gethash channel-id disco-gateway--watch-counts 0) 0))

(defun disco-gateway--upsert-message (channel-id message)
  "Insert or replace MESSAGE in channel CHANNEL-ID cache.

State is kept newest-first to match REST message list ordering."
  (cl-labels ((alist-merge (old new)
                (let ((merged (copy-tree old)))
                  (dolist (pair new)
                    (setf (alist-get (car pair) merged nil 'remove) (cdr pair)))
                  merged)))
    (let* ((message-id (alist-get 'id message))
           (old (or (disco-state-messages channel-id) '()))
           (found nil)
           (updated
            (mapcar
             (lambda (msg)
               (if (and message-id (equal (alist-get 'id msg) message-id))
                   (progn (setq found t) (alist-merge msg message))
                 msg))
             old)))
      (unless found
        (setq updated (cons message updated)))
      (disco-state-put-messages channel-id updated))))

(defun disco-gateway--delete-message (channel-id message-id)
  "Delete MESSAGE-ID from CHANNEL-ID cache."
  (let* ((old (or (disco-state-messages channel-id) '()))
         (updated (cl-remove-if (lambda (msg)
                                  (equal (alist-get 'id msg) message-id))
                                old)))
    (disco-state-put-messages channel-id updated)))

(defun disco-gateway--handle-dispatch (event-type data)
  "Handle one dispatch EVENT-TYPE with DATA payload."
  (pcase event-type
    ("READY"
     (setq disco-gateway--session-id (alist-get 'session_id data))
     (setq disco-gateway--resume-url (alist-get 'resume_gateway_url data))
     (disco-gateway--reset-reconnect-backoff)
     (message "disco: gateway READY"))
    ("RESUMED"
     (disco-gateway--reset-reconnect-backoff)
     (message "disco: gateway RESUMED"))
    ("MESSAGE_CREATE"
     (let ((channel-id (alist-get 'channel_id data)))
       (when (and channel-id (disco-gateway--channel-watched-p channel-id))
         (disco-gateway--upsert-message channel-id data)
         (disco-gateway--emit
          (list :type 'message-create :channel-id channel-id :message data)))))
    ("MESSAGE_UPDATE"
     (let ((channel-id (alist-get 'channel_id data)))
       (when (and channel-id (disco-gateway--channel-watched-p channel-id))
         (disco-gateway--upsert-message channel-id data)
         (disco-gateway--emit
          (list :type 'message-update :channel-id channel-id :message data)))))
    ("MESSAGE_DELETE"
     (let ((channel-id (alist-get 'channel_id data))
           (message-id (alist-get 'id data)))
       (when (and channel-id message-id (disco-gateway--channel-watched-p channel-id))
         (disco-gateway--delete-message channel-id message-id)
         (disco-gateway--emit
          (list :type 'message-delete :channel-id channel-id :message-id message-id)))))
    ("THREAD_CREATE"
     (disco-state-upsert-channel data))
    ("THREAD_UPDATE"
     (disco-state-upsert-channel data))
    ("THREAD_DELETE"
     (let ((thread-id (alist-get 'id data)))
       (when thread-id
         (disco-state-delete-channel thread-id))))
    ("THREAD_LIST_SYNC"
     (let ((guild-id (alist-get 'guild_id data))
           (channel-ids (alist-get 'channel_ids data))
           (threads (alist-get 'threads data)))
       (when guild-id
         (disco-state-sync-threads guild-id channel-ids (or threads '())))))))

(defun disco-gateway--handle-payload (payload)
  "Handle one decoded gateway PAYLOAD."
  (let ((op (alist-get 'op payload))
        (seq (alist-get 's payload))
        (event-type (alist-get 't payload))
        (data (alist-get 'd payload)))
    (when seq
      (setq disco-gateway--seq seq))
    (pcase op
      (10
       (let ((interval (alist-get 'heartbeat_interval data)))
         (disco-gateway--start-heartbeat interval)
         (if (disco-gateway--can-resume-p)
             (disco-gateway--send-resume)
           (disco-gateway--send-identify))))
      (11
       (setq disco-gateway--awaiting-heartbeat-ack nil))
      (0
       (disco-gateway--handle-dispatch event-type data))
      (7
       (message "disco: gateway requested reconnect")
       (disco-gateway--reconnect 1 nil))
      (9
       ;; Invalid Session can be resumable or require full identify restart.
       (let* ((resumable (eq data t))
              (delay (disco-gateway--random-between
                      disco-gateway-invalid-session-min-delay
                      disco-gateway-invalid-session-max-delay)))
         (message "disco: invalid session (resumable=%s), reconnecting in %.2fs"
                  resumable delay)
         (disco-gateway--reconnect delay (not resumable))))
      (_ nil))))

(defun disco-gateway--connect-url ()
  "Compute websocket URL for gateway connect."
  (let* ((source (or disco-gateway--resume-url
                     (alist-get 'url (disco-api-gateway))))
         (separator (if (and source (string-match-p "\\?" source)) "&" "?"))
         (compression (disco-gateway--transport-compression-string)))
    (unless (and source (stringp source))
      (error "disco: unable to resolve gateway websocket URL"))
    (concat source separator
            "v=" (number-to-string disco-gateway-version)
            "&encoding=" disco-gateway-encoding
            (if compression
                (concat "&compress=" compression)
              ""))))

(defun disco-gateway--ensure-token ()
  "Signal user error if gateway token is missing."
  (unless (and disco-token (not (string-empty-p disco-token)))
    (user-error "disco: token is not set; run M-x disco-set-token")))

(defun disco-gateway--connect ()
  "Connect websocket transport if needed."
  (unless (or disco-gateway--stopping
              disco-gateway--connecting
              (and disco-gateway--ws (websocket-openp disco-gateway--ws)))
    (disco-gateway--ensure-token)
    (setq disco-gateway--connecting t)
    (let ((url (disco-gateway--connect-url)))
      (setq disco-gateway--ws
            (websocket-open
             url
             :on-open (lambda (_ws)
                        (disco-gateway--zlib-reset-state)
                        (setq disco-gateway--connecting nil)
                        (message "disco: gateway websocket opened"))
             :on-message (lambda (_ws frame)
                           (condition-case err
                               (let ((payload-text (disco-gateway--frame-json-text frame)))
                                 (when (and payload-text
                                            (not (string-empty-p (string-trim payload-text))))
                                   (disco-gateway--handle-payload
                                    (disco-gateway--json-decode payload-text))))
                             (error
                              (message "disco: gateway payload error: %s"
                                       (error-message-string err)))))
             :on-close (lambda (_ws)
                         (setq disco-gateway--ws nil)
                         (setq disco-gateway--connecting nil)
                         (when (timerp disco-gateway--heartbeat-timer)
                           (cancel-timer disco-gateway--heartbeat-timer)
                           (setq disco-gateway--heartbeat-timer nil))
                         (setq disco-gateway--awaiting-heartbeat-ack nil)
                         (disco-gateway--zlib-reset-state)
                         (unless (or disco-gateway--stopping
                                     (timerp disco-gateway--reconnect-timer))
                           (message "disco: gateway websocket closed, reconnecting")
                           (disco-gateway--schedule-reconnect nil)))
             :on-error (lambda (_ws _type err)
                         (setq disco-gateway--connecting nil)
                         (disco-gateway--zlib-reset-state)
                         (message "disco: gateway websocket error: %s"
                                  (error-message-string err))
                         (unless (or disco-gateway--stopping
                                     (timerp disco-gateway--reconnect-timer))
                           (disco-gateway--schedule-reconnect nil))))))))

(defun disco-gateway-start ()
  "Start gateway transport if enabled and needed."
  (interactive)
  (setq disco-gateway--stopping nil)
  (unless (timerp disco-gateway--reconnect-timer)
    (disco-gateway--reset-reconnect-backoff))
  (when (and disco-enable-live-updates
             (> (hash-table-count disco-gateway--watch-counts) 0))
    (disco-gateway--connect)))

(defun disco-gateway-stop ()
  "Stop gateway transport and clear resume state."
  (interactive)
  (setq disco-gateway--stopping t)
  (disco-gateway--disconnect-internal t)
  (disco-gateway--reset-reconnect-backoff)
  (message "disco: live updates stopped"))

(defun disco-gateway-watch-channel (channel-id)
  "Start watching CHANNEL-ID for live message updates."
  (let ((count (gethash channel-id disco-gateway--watch-counts 0)))
    (puthash channel-id (1+ count) disco-gateway--watch-counts))
  (when disco-enable-live-updates
    (disco-gateway-start)))

(defun disco-gateway-unwatch-channel (channel-id)
  "Decrease watch count for CHANNEL-ID and stop when no channels remain."
  (let ((count (gethash channel-id disco-gateway--watch-counts 0)))
    (cond
     ((<= count 1)
      (remhash channel-id disco-gateway--watch-counts))
     (t
      (puthash channel-id (1- count) disco-gateway--watch-counts))))
  (when (= (hash-table-count disco-gateway--watch-counts) 0)
    (disco-gateway-stop)))

(defun disco-gateway-describe-status ()
  "Display current gateway transport status in minibuffer."
  (interactive)
  (message "disco-gateway: running=%s connecting=%s watched=%d seq=%s session=%s reconnect-attempt=%d reconnect-pending=%s"
           (and disco-gateway--ws (websocket-openp disco-gateway--ws))
           disco-gateway--connecting
           (hash-table-count disco-gateway--watch-counts)
           (or disco-gateway--seq "nil")
           (if disco-gateway--session-id "set" "nil")
           disco-gateway--reconnect-attempt
           (and (timerp disco-gateway--reconnect-timer) t)))

(provide 'disco-gateway)

;;; disco-gateway.el ends here
