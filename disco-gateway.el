;;; disco-gateway.el --- Discord Gateway transport for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; WebSocket-based Discord Gateway transport.
;;
;; Public contract intentionally stays stable:
;; - `disco-gateway-watch-channel'
;; - `disco-gateway-unwatch-channel'
;; - `disco-gateway-watch-global'
;; - `disco-gateway-unwatch-global'
;; - `disco-gateway-request-last-messages'
;; - `disco-gateway-request-guild-members'
;; - `disco-gateway-request-channel-statuses'
;; - `disco-gateway-request-channel-member-count'
;; - `disco-gateway-request-channel-info'
;; - `disco-gateway-send-queue-slot-available-p'
;; - `disco-gateway-event-hook' events with
;;   :type one of message/channel/guild/thread/typing event symbols,
;;   plus event-scoped payload fields (e.g., :channel-id, :guild-id).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'disco-api)
(require 'disco-customize)
(require 'disco-read-state)
(require 'disco-state)
(require 'disco-util)
(require 'websocket)

(defvar disco-gateway-enable-passive-guild-update-v2)

(defvar disco-gateway-event-hook nil
  "Hook called with one event plist argument.

Event schema:
- :type one of
  `message-create' `message-update' `message-delete' `message-ack'
  `message-reaction-add' `message-reaction-remove'
  `message-reaction-remove-all' `message-reaction-remove-emoji'
  `message-poll-vote-add' `message-poll-vote-remove'
  `guild-members-chunk'
  `channel-create' `channel-update' `channel-delete'
  `channel-update-partial' `channel-unread-update'
  `channel-pins-update' `channel-pins-ack'
  `channel-statuses' `channel-info' `channel-member-count-update'
  `passive-update-v1' `passive-update-v2'
  `guild-create' `guild-update' `guild-delete' `guild-sync'
  `guild-feature-ack' `user-non-channel-ack'
  `notification-center-items-ack'
  `rate-limited'
  `presence-update' `sessions-replace'
  `voice-state-update' `voice-channel-status-update'
  `voice-channel-start-time-update'
  `conversation-summary-update' `last-messages'
  `thread-create' `thread-update' `thread-delete' `thread-list-sync'
  `thread-member-update' `thread-members-update' `typing-start'
- :channel-id string for message/channel/thread/typing events
- :guild-id string for guild/channel/thread/typing events
- :thread-id string for thread and thread-member events
- :user-id string for typing/thread-member/reaction events when present
- :message message object for create/update
- :message-id string for message delete
- :mention-count integer for message ack when present
- :flags integer for message ack when present
- :last-viewed integer for message ack when present
- :timestamp integer for typing-start when present
- :member guild member object for typing-start in guild channels
- :members array of guild member objects for `guild-members-chunk'
- :nonce string for `guild-members-chunk' when present
- :emoji reaction emoji object/string for reaction events when present
- :answer-id integer for poll vote events when present
- :watched non-nil for message-create when channel has active room watcher
- :channel channel object for channel/thread events
- :guild guild object for guild events
- :guild-count integer for guild-sync events
- :guild-ids list of guild IDs for guild-sync events
- :channel-unread object for channel-update-partial
- :channel-unread-updates list for channel-unread-update
- :last-pin-timestamp string for channel-pins-update/channel-pins-ack
- :version integer for channel-pins-ack and feature ACK events when present
- :read-state-type integer for guild/user feature ACK events
- :resource-id string for guild/user feature ACK events
- :entity-id string for guild/user feature ACK events and notification-center ack
- :channels list for passive-update-v1
- :updated-channels list for passive-update-v2
- :threads list for thread-list-sync
- :thread-member thread member object for thread-member-update
- :added-members list for thread-members-update
- :removed-member-ids list for thread-members-update
- :member-count integer for thread-members-update when present
- :presence-count integer for channel-member-count-update when present
- :opcode integer for rate-limited events when present
- :retry-after float seconds for rate-limited events when present
- :meta metadata object for rate-limited events when present
- :presence presence object for presence-update
- :sessions session list for sessions-replace
- :previous-channel-id string for voice-state-update when moved/disconnected
- :status string for voice-channel-status-update/channel-statuses when present
- :voice-start-time string for voice-channel-start-time-update/channel-info
- :summaries list for conversation-summary-update
- :messages list for last-messages
- :channel-ids list of affected channel IDs for derived response events")

(defvar disco-gateway--watch-counts (make-hash-table :test #'equal))

(defvar disco-gateway--global-watch-count 0
  "Reference count for non-channel-specific gateway consumers.")

(defvar disco-gateway--ws nil)
(defvar disco-gateway--connecting nil)
(defvar disco-gateway--stopping nil)

(defvar disco-gateway--seq nil)
(defvar disco-gateway--session-id nil)
(defvar disco-gateway--resume-url nil)
(defvar disco-gateway--current-user-id nil
  "Cached current account user ID from READY/USER_UPDATE payloads.")
(defvar disco-gateway--lazy-subscribed-channels (make-hash-table :test #'equal)
  "Hash table of channel IDs already subscribed via Gateway op 14.")

(defvar disco-gateway--heartbeat-interval-ms nil)
(defvar disco-gateway--heartbeat-timer nil)
(defvar disco-gateway--awaiting-heartbeat-ack nil)

(defvar disco-gateway--reconnect-timer nil)
(defvar disco-gateway--reconnect-attempt 0)

(defvar disco-gateway--send-history nil
  "List of recent gateway send timestamps (float-time seconds).")

(defvar disco-gateway--send-queue-high nil
  "FIFO queue of high-priority outbound payloads.")

(defvar disco-gateway--send-queue-normal nil
  "FIFO queue of normal-priority outbound payloads.")

(defvar disco-gateway--send-queue-timer nil
  "Timer used to continue draining outbound send queue when rate limited.")

(defvar disco-gateway--send-opcode-cooldowns (make-hash-table :test #'equal)
  "Hash table of outbound opcode cooldown deadlines keyed by opcode scope.")

(defconst disco-gateway--send-priority-high 'high
  "Queue marker for high-priority outbound gateway payloads.")

(defconst disco-gateway--send-priority-normal 'normal
  "Queue marker for normal-priority outbound gateway payloads.")

(defconst disco-gateway--zlib-suffix (string 0 0 255 255)
  "Z_SYNC_FLUSH suffix used by Discord zlib-stream transport.")

(defconst disco-gateway--capability-passive-guild-update-v2 (ash 1 14)
  "Gateway capability bit for PASSIVE_GUILD_UPDATE_V2.")

(defvar disco-gateway--zlib-stream-buffer ""
  "Accumulated compressed bytes for current gateway connection.")

(defvar disco-gateway--zlib-stream-output-bytes 0
  "Previously produced decompressed byte count from stream buffer.")

(defun disco-gateway-running-p ()
  "Return non-nil when gateway transport is active or connecting."
  (or disco-gateway--connecting
      (and disco-gateway--ws (websocket-openp disco-gateway--ws))))

(defun disco-gateway-current-user-id ()
  "Return current Discord account user ID from gateway session, or nil."
  disco-gateway--current-user-id)

(defun disco-gateway-send-queue-slot-available-p (&optional slots)
  "Return non-nil when outbound send queue can accept SLOTS payloads."
  (let* ((required (max 1 (or slots 1)))
         (max-size (max 1 (or disco-gateway-send-queue-max-size 1))))
    (<= (+ (disco-gateway--send-queue-size) required)
        max-size)))

(defun disco-gateway--intent-enabled-p (bit)
  "Return non-nil when identify intents include BIT.

When `disco-gateway-identify-intents' is nil, intent filtering is disabled."
  (or (null disco-gateway-identify-intents)
      (/= 0 (logand disco-gateway-identify-intents bit))))

(defun disco-gateway--warn-missing-intent-hints ()
  "Log hints when custom intents disable optional live events."
  (when (integerp disco-gateway-identify-intents)
    (unless (disco-gateway--intent-enabled-p (ash 1 11))
      (message "disco: identify intents missing GUILD_MESSAGE_TYPING (1<<11); guild typing indicators will be unavailable"))
    (unless (disco-gateway--intent-enabled-p (ash 1 14))
      (message "disco: identify intents missing DIRECT_MESSAGE_TYPING (1<<14); DM typing indicators will be unavailable"))
    (unless (disco-gateway--intent-enabled-p (ash 1 24))
      (message "disco: identify intents missing GUILD_MESSAGE_POLLS (1<<24); guild poll vote events will be unavailable"))
    (unless (disco-gateway--intent-enabled-p (ash 1 25))
      (message "disco: identify intents missing DIRECT_MESSAGE_POLLS (1<<25); DM poll vote events will be unavailable"))))

(defun disco-gateway--channel-guild-id (channel-id)
  "Return guild ID for CHANNEL-ID from local state, or nil for non-guild channels."
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (and (listp channel)
         (alist-get 'guild_id channel))))

(defun disco-gateway--send-lazy-channel-subscription (guild-id channel-id)
  "Send Gateway op 14 subscription for one GUILD-ID/CHANNEL-ID pair."
  (disco-gateway--send-op
   14
   `((guild_id . ,guild-id)
     (typing . t)
     (activities . t)
     (threads . t)
     ;; Use vectors for range arrays to avoid alist ambiguity in `json-encode'.
     (channels . ((,channel-id . ,(vector (vector 0 99))))))))

(defun disco-gateway--maybe-subscribe-watched-channel (channel-id &optional force)
  "Send channel lazy-subscription for CHANNEL-ID when needed.

When FORCE is non-nil, resend even when CHANNEL-ID was already subscribed."
  (when (and disco-gateway-enable-lazy-channel-subscriptions
             (disco-gateway--intent-enabled-p (ash 1 11)))
    (let* ((normalized-channel-id (and channel-id (format "%s" channel-id)))
           (guild-id (and normalized-channel-id
                          (disco-gateway--channel-guild-id normalized-channel-id)))
           (already (and normalized-channel-id
                         (gethash normalized-channel-id
                                  disco-gateway--lazy-subscribed-channels))))
      (when (and normalized-channel-id
                 guild-id
                 (or force (not already)))
        (disco-gateway--send-lazy-channel-subscription guild-id normalized-channel-id)
        (puthash normalized-channel-id guild-id
                 disco-gateway--lazy-subscribed-channels)))))

(defun disco-gateway--subscribe-watched-guild-channels (&optional force)
  "Send lazy-subscription payloads for all watched guild channels.

When FORCE is non-nil, resend subscriptions even when already tracked."
  (when (disco-gateway--intent-enabled-p (ash 1 11))
    (maphash
     (lambda (channel-id _count)
       (disco-gateway--maybe-subscribe-watched-channel channel-id force))
     disco-gateway--watch-counts)))

(defun disco-gateway-request-last-messages (guild-id channel-ids)
  "Request last messages for GUILD-ID and CHANNEL-IDS via Gateway op 34.

CHANNEL-IDS is limited to 100 IDs per request by Discord."
  (let* ((normalized-guild-id (and guild-id (format "%s" guild-id)))
         (normalized-channel-ids (disco-util-normalize-id-list channel-ids 100)))
    (when (and normalized-guild-id normalized-channel-ids)
      (disco-gateway--send-op
       34
       `((guild_id . ,normalized-guild-id)
         (channel_ids . ,normalized-channel-ids))))))

(cl-defun disco-gateway-request-guild-members
    (guild-id &key query limit presences user-ids nonce)
  "Request guild members for GUILD-ID via Gateway op 8.

QUERY performs prefix search on username/nickname. LIMIT defaults to 25 when
QUERY is non-nil. USER-IDS requests exact members and is limited to 100 IDs."
  (let* ((normalized-guild-id (and guild-id (format "%s" guild-id)))
         (normalized-query (and (stringp query) (string-trim query)))
         (normalized-user-ids (disco-util-normalize-id-list user-ids 100))
         (payload `((guild_id . ,normalized-guild-id))))
    (unless normalized-guild-id
      (user-error "disco: guild id is required for member request"))
    (unless (or (and normalized-query (not (string-empty-p normalized-query)))
                normalized-user-ids)
      (user-error "disco: either query or user-ids is required for member request"))
    (if normalized-user-ids
        (push `(user_ids . ,normalized-user-ids) payload)
      (push `(query . ,normalized-query) payload)
      (push `(limit . ,(max 1 (min 100 (or limit 25)))) payload))
    (when (not (null presences))
      (push `(presences . ,(if presences t :false)) payload))
    (when (and (stringp nonce)
               (not (string-empty-p nonce)))
      (push `(nonce . ,nonce) payload))
    (disco-gateway--send-op 8 (nreverse payload))))

(defun disco-gateway-request-channel-statuses (guild-id)
  "Request voice channel statuses for GUILD-ID via Gateway op 36."
  (let ((normalized-guild-id (and guild-id (format "%s" guild-id))))
    (when normalized-guild-id
      (disco-gateway--send-op
       36
       `((guild_id . ,normalized-guild-id))))))

(defun disco-gateway-request-channel-member-count (guild-id channel-id)
  "Request member counts for GUILD-ID/CHANNEL-ID via Gateway op 39."
  (let ((normalized-guild-id (and guild-id (format "%s" guild-id)))
        (normalized-channel-id (and channel-id (format "%s" channel-id))))
    (when (and normalized-guild-id normalized-channel-id)
      (disco-gateway--send-op
       39
       `((guild_id . ,normalized-guild-id)
         (channel_id . ,normalized-channel-id))))))

(defun disco-gateway-request-channel-info (guild-id fields)
  "Request extra channel info FIELDS for GUILD-ID via Gateway op 43.

FIELDS should be a list of strings such as `status' and
`voice_start_time'."
  (let* ((normalized-guild-id (and guild-id (format "%s" guild-id)))
         (normalized-fields
          (delq nil
                (mapcar (lambda (field)
                          (cond
                           ((symbolp field)
                            (symbol-name field))
                           ((stringp field)
                            field)
                           (t nil)))
                        (or fields '())))))
    (when (and normalized-guild-id normalized-fields)
      (disco-gateway--send-op
       43
       `((guild_id . ,normalized-guild-id)
         (fields . ,normalized-fields))))))

(defun disco-gateway--emit (event)
  "Emit EVENT plist to `disco-gateway-event-hook'."
  (run-hook-with-args 'disco-gateway-event-hook event))

(defun disco-gateway--emit-channel-event (event-type channel)
  "Emit CHANNEL mutation as EVENT-TYPE to `disco-gateway-event-hook'."
  (let ((channel-id (or (alist-get 'id channel)
                        (alist-get 'channel_id channel)))
        (guild-id (alist-get 'guild_id channel)))
    (disco-gateway--emit
     (list :type event-type
           :channel-id channel-id
           :guild-id guild-id
           :channel channel))))

(defun disco-gateway--emit-guild-event (event-type guild)
  "Emit GUILD mutation as EVENT-TYPE to `disco-gateway-event-hook'."
  (let ((guild-id (or (alist-get 'id guild)
                      (alist-get 'guild_id guild))))
    (disco-gateway--emit
     (list :type event-type
           :guild-id guild-id
           :guild guild))))

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
  (disco-gateway--reset-send-rate-state)
  (clrhash disco-gateway--lazy-subscribed-channels)

  (when disco-gateway--ws
    (ignore-errors (websocket-close disco-gateway--ws))
    (setq disco-gateway--ws nil))

  (when clear-session
    (setq disco-gateway--seq nil)
    (setq disco-gateway--session-id nil)
    (setq disco-gateway--resume-url nil)
    (setq disco-gateway--current-user-id nil)
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

(defun disco-gateway--send-rate-limit-max-events ()
  "Return max outbound gateway events allowed per rate-limit window."
  (max 1 (or disco-gateway-send-max-events-per-window 1)))

(defun disco-gateway--send-rate-limit-window-seconds ()
  "Return outbound gateway rate-limit window length in seconds."
  (max 1.0 (float (or disco-gateway-send-window-seconds 60))))

(defun disco-gateway--send-min-interval ()
  "Return minimum spacing in seconds between outbound gateway sends."
  (/ (disco-gateway--send-rate-limit-window-seconds)
     (float (disco-gateway--send-rate-limit-max-events))))

(defun disco-gateway--send-queue-size ()
  "Return number of outbound gateway payloads currently queued."
  (+ (length disco-gateway--send-queue-high)
     (length disco-gateway--send-queue-normal)))

(defun disco-gateway--cancel-send-queue-timer ()
  "Cancel pending outbound send queue drain timer when present."
  (when (timerp disco-gateway--send-queue-timer)
    (cancel-timer disco-gateway--send-queue-timer)
    (setq disco-gateway--send-queue-timer nil)))

(defun disco-gateway--reset-send-rate-state ()
  "Reset outbound send history, cooldowns, and pending queue state."
  (disco-gateway--cancel-send-queue-timer)
  (setq disco-gateway--send-history nil)
  (setq disco-gateway--send-queue-high nil)
  (setq disco-gateway--send-queue-normal nil)
  (clrhash disco-gateway--send-opcode-cooldowns))

(defun disco-gateway--prune-send-history ()
  "Drop outbound send history entries that are outside rate-limit window."
  (let ((cutoff (- (float-time)
                   (disco-gateway--send-rate-limit-window-seconds))))
    (setq disco-gateway--send-history
          (seq-filter
           (lambda (timestamp)
             (> timestamp cutoff))
           disco-gateway--send-history))))

(defun disco-gateway--send-capacity-remaining ()
  "Return outbound send slots currently available in rate-limit window."
  (disco-gateway--prune-send-history)
  (max 0 (- (disco-gateway--send-rate-limit-max-events)
            (length disco-gateway--send-history))))

(defun disco-gateway--next-send-delay ()
  "Return seconds until next outbound send slot becomes available."
  (disco-gateway--prune-send-history)
  (if (< (length disco-gateway--send-history)
         (disco-gateway--send-rate-limit-max-events))
      0.0
    (let* ((oldest (car (last disco-gateway--send-history)))
           (window (disco-gateway--send-rate-limit-window-seconds))
           (now (float-time)))
      (max 0.01 (- (+ oldest window) now)))))

(defun disco-gateway--next-send-spacing-delay ()
  "Return seconds to wait to respect minimum outbound send spacing."
  (let ((latest (car disco-gateway--send-history)))
    (if (null latest)
        0.0
      (max 0.0 (- (+ latest (disco-gateway--send-min-interval))
                  (float-time))))))

(defun disco-gateway--enqueue-send-op (op d priority)
  "Push outbound payload OP/D into PRIORITY queue.

Return non-nil when payload is queued."
  (let ((max-size (max 1 (or disco-gateway-send-queue-max-size 1)))
        (accept t))
    (when (>= (disco-gateway--send-queue-size) max-size)
      (if (and (eq priority disco-gateway--send-priority-high)
               disco-gateway--send-queue-normal)
          (setq disco-gateway--send-queue-normal
                (cdr disco-gateway--send-queue-normal))
        (setq accept nil)
        (message "disco: gateway send queue full (%d), dropping op %s"
                 max-size op)))
    (when accept
      (let ((entry (cons op d)))
        (if (eq priority disco-gateway--send-priority-high)
            (setq disco-gateway--send-queue-high
                  (nconc disco-gateway--send-queue-high (list entry)))
          (setq disco-gateway--send-queue-normal
                (nconc disco-gateway--send-queue-normal (list entry))))
        t))))

(defun disco-gateway--dequeue-send-op ()
  "Pop next outbound payload entry, preferring high-priority queue."
  (cond
   (disco-gateway--send-queue-high
    (prog1 (car disco-gateway--send-queue-high)
      (setq disco-gateway--send-queue-high
            (cdr disco-gateway--send-queue-high))))
   (disco-gateway--send-queue-normal
    (prog1 (car disco-gateway--send-queue-normal)
      (setq disco-gateway--send-queue-normal
            (cdr disco-gateway--send-queue-normal))))
   (t nil)))

(defun disco-gateway--send-priority-for-op (op)
  "Return send queue priority symbol for gateway OP."
  (if (memq op '(1 2 6))
      disco-gateway--send-priority-high
    disco-gateway--send-priority-normal))

(defun disco-gateway--send-cooldown-key-for-op (op &optional d)
  "Return cooldown key for outbound OP and payload D."
  (let ((guild-id (and (listp d) (alist-get 'guild_id d))))
    (if guild-id
        (cons op (format "%s" guild-id))
      op)))

(defun disco-gateway--send-cooldown-delay (op &optional d)
  "Return remaining cooldown delay in seconds for outbound OP and D."
  (let* ((key (disco-gateway--send-cooldown-key-for-op op d))
         (deadline (gethash key disco-gateway--send-opcode-cooldowns))
         (remaining (and deadline (- deadline (float-time)))))
    (if (and (numberp remaining) (> remaining 0))
        remaining
      0.0)))

(defun disco-gateway--set-send-cooldown (opcode retry-after &optional meta)
  "Store outbound cooldown for OPCODE using RETRY-AFTER seconds and META."
  (when (and (integerp opcode)
             (numberp retry-after)
             (> retry-after 0))
    (let* ((guild-id (and (listp meta) (alist-get 'guild_id meta)))
           (key (if guild-id
                    (cons opcode (format "%s" guild-id))
                  opcode))
           (deadline (+ (float-time) retry-after)))
      (puthash key deadline disco-gateway--send-opcode-cooldowns))))

(defun disco-gateway--send-op-now (op d)
  "Send gateway payload OP/D immediately.

Return non-nil when payload is written to websocket process."
  (when (and disco-gateway--ws (websocket-openp disco-gateway--ws))
    (condition-case err
        (progn
          (websocket-send-text
           disco-gateway--ws
           (disco-gateway--json-encode `((op . ,op) (d . ,d))))
          (when (= op 1)
            (setq disco-gateway--awaiting-heartbeat-ack t))
          (push (float-time) disco-gateway--send-history)
          t)
      (error
       (message "disco: gateway send op %s failed: %s"
                op
                (error-message-string err))
       (unless (or disco-gateway--stopping
                   (timerp disco-gateway--reconnect-timer))
         (disco-gateway--schedule-reconnect nil))
       nil))))

(defun disco-gateway--schedule-send-queue-drain (delay)
  "Schedule queue drain attempt after DELAY seconds."
  (disco-gateway--cancel-send-queue-timer)
  (setq disco-gateway--send-queue-timer
        (run-at-time (max 0.01 delay)
                     nil
                     #'disco-gateway--flush-send-queue)))

(defun disco-gateway--flush-send-queue ()
  "Attempt to drain outbound gateway queue under rate limits."
  (setq disco-gateway--send-queue-timer nil)
  (when (and disco-gateway--ws (websocket-openp disco-gateway--ws)
             (> (disco-gateway--send-queue-size) 0))
    (let ((delay (max (disco-gateway--next-send-delay)
                      (disco-gateway--next-send-spacing-delay))))
      (if (> delay 0)
          (disco-gateway--schedule-send-queue-drain delay)
        (let* ((entry (disco-gateway--dequeue-send-op))
               (op (car entry))
               (d (cdr entry))
               (cooldown (and entry (disco-gateway--send-cooldown-delay op d))))
          (cond
           ((null entry)
            nil)
           ((> cooldown 0)
            (disco-gateway--enqueue-send-op
             op d (disco-gateway--send-priority-for-op op))
            (disco-gateway--schedule-send-queue-drain
             (max cooldown
                  (disco-gateway--next-send-delay)
                  (disco-gateway--next-send-spacing-delay))))
           ((disco-gateway--send-op-now op d)
            (when (> (disco-gateway--send-queue-size) 0)
              (disco-gateway--schedule-send-queue-drain
               (max (disco-gateway--next-send-delay)
                    (disco-gateway--next-send-spacing-delay)))))))))))

(defun disco-gateway--send-op (op d)
  "Queue one gateway payload OP/D for rate-limited send.

Return non-nil when payload is queued for send."
  (when (and disco-gateway--ws (websocket-openp disco-gateway--ws))
    (when (disco-gateway--enqueue-send-op
           op
           d
           (disco-gateway--send-priority-for-op op))
      (disco-gateway--flush-send-queue)
      t)))

(defun disco-gateway--send-heartbeat ()
  "Send heartbeat (op 1) with latest sequence value."
  (disco-gateway--send-op 1 (or disco-gateway--seq :null)))

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

(defun disco-gateway--effective-identify-capabilities ()
  "Return effective capabilities bitmask for Identify payload, or nil."
  (let ((capabilities (and (integerp disco-gateway-identify-capabilities)
                           disco-gateway-identify-capabilities)))
    (when disco-gateway-enable-passive-guild-update-v2
      (setq capabilities
            (logior (or capabilities 0)
                    disco-gateway--capability-passive-guild-update-v2)))
    capabilities))

(defun disco-gateway--identify-payload ()
  "Build identify payload body for Gateway opcode 2."
  (let ((payload
         `((token . ,(or (disco-current-token) ""))
           (properties . ,(disco-gateway--identify-properties))
           (compress . :false)
           (large_threshold . 250)))
        (capabilities (disco-gateway--effective-identify-capabilities)))
    (when disco-gateway-identify-intents
      (setq payload (append payload `((intents . ,disco-gateway-identify-intents)))))
    (when capabilities
      (setq payload
            (append payload `((capabilities . ,capabilities)))))
    (when disco-gateway-identify-presence
      (setq payload (append payload `((presence . ,disco-gateway-identify-presence)))))
    payload))

(defun disco-gateway--send-identify ()
  "Send identify payload (op 2)."
  (disco-gateway--warn-missing-intent-hints)
  (disco-gateway--send-op 2 (disco-gateway--identify-payload)))

(defun disco-gateway--can-resume-p ()
  "Return non-nil if resume payload can be sent."
  (and disco-gateway--session-id disco-gateway--seq))

(defun disco-gateway--send-resume ()
  "Send resume payload (op 6)."
  (disco-gateway--send-op
   6
   `((token . ,(or (disco-current-token) ""))
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

(defun disco-gateway--watchers-active-p ()
  "Return non-nil when any gateway consumer is currently active."
  (or (> (hash-table-count disco-gateway--watch-counts) 0)
      (> disco-gateway--global-watch-count 0)))

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

(defun disco-gateway--versioned-entries (value)
  "Normalize VALUE into a list of entries.

Discord Ready may deliver some fields as versioned structures
`((entries . [...]) (partial . ...) (version . ...))'."
  (cond
   ((null value) '())
   ((and (listp value) (assq 'entries value))
    (or (alist-get 'entries value) '()))
   ((listp value)
    value)
   (t '())))

(defun disco-gateway--ingest-ready-read-states (read-state)
  "Ingest Ready READ-STATE payload into local read-state store."
  (dolist (entry (disco-gateway--versioned-entries read-state))
    (disco-state-apply-ready-read-state-entry entry)))

(defun disco-gateway--ingest-ready-private-channels (private-channels)
  "Ingest Ready PRIVATE-CHANNELS payload into local state."
  (disco-state-set-private-channels
   (disco-gateway--versioned-entries private-channels)))

(defun disco-gateway--ingest-ready-guilds (guilds)
  "Ingest Ready GUILDS payload into local guild/channel state."
  (let ((ready-guilds (cl-remove-if-not #'listp
                                        (disco-gateway--versioned-entries guilds))))
    (disco-state-set-guilds ready-guilds)
    (dolist (guild ready-guilds)
      (let* ((guild-id (alist-get 'id guild))
             (has-channels (assq 'channels guild))
             (has-threads (assq 'threads guild))
             (channels (and has-channels
                            (disco-gateway--versioned-entries
                             (alist-get 'channels guild))))
             (threads (and has-threads
                           (disco-gateway--versioned-entries
                            (alist-get 'threads guild)))))
        (when (and guild-id has-channels)
          (disco-state-put-channels guild-id channels))
        (when has-threads
          (dolist (thread threads)
            (disco-state-upsert-channel thread)))))
    (disco-gateway--emit
     (list :type 'guild-sync
           :source 'ready
           :guild-ids (delq nil (mapcar (lambda (it) (alist-get 'id it)) ready-guilds))
           :guild-count (length ready-guilds)))))

(defun disco-gateway--upsert-channel-and-emit (event-type channel)
  "Upsert CHANNEL and emit EVENT-TYPE.

CHANNEL watchers are also re-subscribed using Gateway opcode 14."
  (disco-state-upsert-channel channel)
  (let ((channel-id (alist-get 'id channel)))
    (when (and channel-id (disco-gateway--channel-watched-p channel-id))
      (disco-gateway--maybe-subscribe-watched-channel channel-id t)))
  (disco-gateway--emit-channel-event event-type channel))

(defun disco-gateway--delete-channel-and-emit (event-type channel)
  "Delete CHANNEL from indexes and emit EVENT-TYPE."
  (let ((channel-id (alist-get 'id channel)))
    (when channel-id
      (disco-state-delete-channel channel-id))
    (disco-gateway--emit-channel-event event-type channel)))

(defun disco-gateway--thread-id-from-payload (payload)
  "Return normalized thread ID from gateway PAYLOAD."
  (or (alist-get 'id payload)
      (alist-get 'thread_id payload)))

(defun disco-gateway--reaction-emoji (payload)
  "Return emoji object/name from reaction PAYLOAD."
  (or (alist-get 'emoji payload)
      (alist-get 'emoji_name payload)))

(defun disco-gateway--emit-reaction-event (event-type payload)
  "Emit reaction EVENT-TYPE from gateway PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload))
        (message-id (alist-get 'message_id payload)))
    (when (and channel-id message-id)
      (disco-gateway--emit
       (append
        (list :type event-type
              :channel-id channel-id
              :message-id message-id)
        (when (assq 'user_id payload)
          (list :user-id (alist-get 'user_id payload)))
        (when (or (assq 'emoji payload)
                  (assq 'emoji_name payload))
          (list :emoji (disco-gateway--reaction-emoji payload)))
        (when (assq 'type payload)
          (list :reaction-type (alist-get 'type payload))))))))

(defun disco-gateway--emit-poll-vote-event (event-type payload)
  "Emit poll vote EVENT-TYPE from gateway PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload))
        (message-id (alist-get 'message_id payload))
        (answer-id (alist-get 'answer_id payload)))
    (when (and channel-id message-id)
      (disco-gateway--emit
       (list :type event-type
             :channel-id channel-id
             :guild-id (alist-get 'guild_id payload)
             :message-id message-id
             :user-id (alist-get 'user_id payload)
             :answer-id answer-id)))))

(defun disco-gateway--dispatch-ready (payload)
  "Handle READY dispatch PAYLOAD."
  (setq disco-gateway--session-id (alist-get 'session_id payload))
  (setq disco-gateway--resume-url (alist-get 'resume_gateway_url payload))
  (let ((ready-user (alist-get 'user payload)))
    (setq disco-gateway--current-user-id
          (and (listp ready-user)
               (alist-get 'id ready-user))))
  (disco-gateway--ingest-ready-read-states (alist-get 'read_state payload))
  (when (assq 'guilds payload)
    (disco-gateway--ingest-ready-guilds
     (alist-get 'guilds payload)))
  (when (assq 'private_channels payload)
    (disco-gateway--ingest-ready-private-channels
     (alist-get 'private_channels payload)))
  (disco-gateway--subscribe-watched-guild-channels t)
  (disco-gateway--reset-reconnect-backoff)
  (disco-gateway--emit
   (list :type 'ready
         :user-id disco-gateway--current-user-id))
  (message "disco: gateway READY"))

(defun disco-gateway--dispatch-resumed (_payload)
  "Handle RESUMED dispatch event."
  (disco-gateway--subscribe-watched-guild-channels t)
  (disco-gateway--reset-reconnect-backoff)
  (message "disco: gateway RESUMED"))

(defun disco-gateway--dispatch-guild-create (payload)
  "Handle GUILD_CREATE dispatch PAYLOAD."
  (disco-state-upsert-guild payload)
  (disco-gateway--emit-guild-event 'guild-create payload))

(defun disco-gateway--dispatch-guild-update (payload)
  "Handle GUILD_UPDATE dispatch PAYLOAD."
  (disco-state-upsert-guild payload)
  (disco-gateway--emit-guild-event 'guild-update payload))

(defun disco-gateway--dispatch-guild-delete (payload)
  "Handle GUILD_DELETE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'id payload)))
    (when guild-id
      (disco-state-delete-guild guild-id))
    (disco-gateway--emit-guild-event 'guild-delete payload)))

(defun disco-gateway--dispatch-channel-create (payload)
  "Handle CHANNEL_CREATE dispatch PAYLOAD."
  (disco-gateway--upsert-channel-and-emit 'channel-create payload))

(defun disco-gateway--dispatch-channel-update (payload)
  "Handle CHANNEL_UPDATE dispatch PAYLOAD."
  (disco-gateway--upsert-channel-and-emit 'channel-update payload))

(defun disco-gateway--dispatch-channel-delete (payload)
  "Handle CHANNEL_DELETE dispatch PAYLOAD."
  (disco-gateway--delete-channel-and-emit 'channel-delete payload))

(defun disco-gateway--dispatch-channel-update-partial (payload)
  "Handle CHANNEL_UPDATE_PARTIAL dispatch PAYLOAD."
  (let ((channel-id (alist-get 'id payload)))
    (disco-state-apply-channel-unread payload)
    (when channel-id
      (disco-gateway--emit
       (list :type 'channel-update-partial
             :channel-id channel-id
             :channel-unread payload)))))

(defun disco-gateway--dispatch-channel-unread-update (payload)
  "Handle CHANNEL_UNREAD_UPDATE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-unread-updates (or (alist-get 'channel_unread_updates payload) '())))
    (disco-state-apply-channel-unread-updates channel-unread-updates)
    (disco-gateway--emit
     (list :type 'channel-unread-update
           :guild-id guild-id
           :channel-unread-updates channel-unread-updates))))

(defun disco-gateway--channel-unread-channel-ids (updates)
  "Extract channel IDs from channel unread UPDATES list."
  (disco-util-normalize-id-list
   (mapcar (lambda (it)
             (and (listp it)
                  (alist-get 'id it)))
           (or updates '()))))

(defun disco-gateway--dispatch-passive-update-v1 (payload)
  "Handle PASSIVE_UPDATE_V1 dispatch PAYLOAD."
  (let* ((guild-id (alist-get 'guild_id payload))
         (channels (or (alist-get 'channels payload) '()))
         (voice-states (or (alist-get 'voice_states payload) '()))
         (members (or (alist-get 'members payload) '()))
         (voice-channel-ids
          (disco-state-apply-passive-voice-state-snapshot guild-id voice-states))
         (channel-ids
          (disco-util-normalize-id-list
           (append (disco-gateway--channel-unread-channel-ids channels)
                   voice-channel-ids))))
    (disco-state-apply-channel-unread-updates channels)
    (disco-gateway--emit
     (list :type 'passive-update-v1
           :guild-id guild-id
           :channels channels
           :voice-states voice-states
           :members members
           :channel-ids channel-ids))))

(defun disco-gateway--dispatch-passive-update-v2 (payload)
  "Handle PASSIVE_UPDATE_V2 dispatch PAYLOAD."
  (let* ((guild-id (alist-get 'guild_id payload))
         (updated-channels (or (alist-get 'updated_channels payload) '()))
         (updated-voice-states (or (alist-get 'updated_voice_states payload) '()))
         (removed-voice-states (or (alist-get 'removed_voice_states payload) '()))
         (updated-members (or (alist-get 'updated_members payload) '()))
         (voice-channel-ids
          (disco-state-apply-passive-voice-state-updates
           guild-id
           updated-voice-states
           removed-voice-states))
         (channel-ids
          (disco-util-normalize-id-list
           (append (disco-gateway--channel-unread-channel-ids updated-channels)
                   voice-channel-ids))))
    (disco-state-apply-channel-unread-updates updated-channels)
    (disco-gateway--emit
     (list :type 'passive-update-v2
           :guild-id guild-id
           :updated-channels updated-channels
           :updated-voice-states updated-voice-states
           :removed-voice-states removed-voice-states
           :updated-members updated-members
           :channel-ids channel-ids))))

(defun disco-gateway--dispatch-channel-statuses (payload)
  "Handle CHANNEL_STATUSES dispatch PAYLOAD."
  (let* ((guild-id (alist-get 'guild_id payload))
         (channels (or (alist-get 'channels payload) '()))
         (channel-ids (or (disco-state-apply-channel-statuses channels) '())))
    (disco-gateway--emit
     (list :type 'channel-statuses
           :guild-id guild-id
           :channels channels
           :channel-ids channel-ids))))

(defun disco-gateway--dispatch-channel-info (payload)
  "Handle CHANNEL_INFO dispatch PAYLOAD."
  (let* ((guild-id (alist-get 'guild_id payload))
         (channels (or (alist-get 'channels payload) '()))
         (channel-ids (or (disco-state-apply-channel-info channels) '())))
    (disco-gateway--emit
     (list :type 'channel-info
           :guild-id guild-id
           :channels channels
           :channel-ids channel-ids))))

(defun disco-gateway--dispatch-channel-member-count-update (payload)
  "Handle CHANNEL_MEMBER_COUNT_UPDATE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-id (alist-get 'channel_id payload))
        (member-count (alist-get 'member_count payload))
        (presence-count (alist-get 'presence_count payload)))
    (when channel-id
      (disco-state-apply-channel-member-count-update
       channel-id
       member-count
       presence-count))
    (disco-gateway--emit
     (list :type 'channel-member-count-update
           :guild-id guild-id
           :channel-id channel-id
           :member-count member-count
           :presence-count presence-count
           :channel-ids (and channel-id (list channel-id))))))

(defun disco-gateway--dispatch-last-messages (payload)
  "Handle LAST_MESSAGES dispatch PAYLOAD."
  (let* ((guild-id (alist-get 'guild_id payload))
         (messages (or (alist-get 'messages payload) '()))
         (channel-ids (or (disco-state-apply-last-messages messages) '())))
    (disco-gateway--emit
     (list :type 'last-messages
           :guild-id guild-id
           :messages messages
           :channel-ids channel-ids))))

(defun disco-gateway--dispatch-conversation-summary-update (payload)
  "Handle CONVERSATION_SUMMARY_UPDATE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-id (alist-get 'channel_id payload))
        (summaries (or (alist-get 'summaries payload) '())))
    (when channel-id
      (disco-state-apply-conversation-summary-update channel-id summaries))
    (disco-gateway--emit
     (list :type 'conversation-summary-update
           :guild-id guild-id
           :channel-id channel-id
           :summaries summaries
           :channel-ids (and channel-id (list channel-id))))))

(defun disco-gateway--dispatch-presence-update (payload)
  "Handle PRESENCE_UPDATE dispatch PAYLOAD."
  (let* ((user (alist-get 'user payload))
         (user-id (or (alist-get 'user_id payload)
                      (and (listp user) (alist-get 'id user))))
         (guild-id (alist-get 'guild_id payload)))
    (disco-state-apply-presence-update payload)
    (disco-gateway--emit
     (list :type 'presence-update
           :guild-id guild-id
           :user-id user-id
           :presence payload))))

(defun disco-gateway--dispatch-sessions-replace (payload)
  "Handle SESSIONS_REPLACE dispatch PAYLOAD."
  (let ((sessions (or payload '())))
    (disco-state-set-sessions sessions)
    (disco-gateway--emit
     (list :type 'sessions-replace
           :sessions sessions))))

(defun disco-gateway--dispatch-voice-state-update (payload)
  "Handle VOICE_STATE_UPDATE dispatch PAYLOAD."
  (let* ((delta (disco-state-apply-voice-state-update payload))
         (guild-id (or (plist-get delta :guild-id)
                       (alist-get 'guild_id payload)))
         (channel-id (or (plist-get delta :channel-id)
                         (alist-get 'channel_id payload)))
         (previous-channel-id (plist-get delta :previous-channel-id))
         (channel-ids (or (plist-get delta :channel-ids)
                          (and channel-id (list channel-id))))
         (user-id (or (plist-get delta :user-id)
                      (alist-get 'user_id payload))))
    (disco-gateway--emit
     (list :type 'voice-state-update
           :guild-id guild-id
           :channel-id channel-id
           :previous-channel-id previous-channel-id
           :channel-ids channel-ids
           :user-id user-id
           :voice-state payload))))

(defun disco-gateway--dispatch-voice-channel-start-time-update (payload)
  "Handle VOICE_CHANNEL_START_TIME_UPDATE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-id (alist-get 'id payload))
        (voice-start-time (alist-get 'voice_start_time payload)))
    (when channel-id
      (disco-state-apply-channel-voice-start-time-update channel-id voice-start-time))
    (disco-gateway--emit
     (list :type 'voice-channel-start-time-update
           :guild-id guild-id
           :channel-id channel-id
           :voice-start-time voice-start-time
           :channel-ids (and channel-id (list channel-id))))))

(defun disco-gateway--dispatch-voice-channel-status-update (payload)
  "Handle VOICE_CHANNEL_STATUS_UPDATE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-id (alist-get 'id payload))
        (status (alist-get 'status payload)))
    (when channel-id
      (disco-state-apply-channel-status-update channel-id status))
    (disco-gateway--emit
     (list :type 'voice-channel-status-update
           :guild-id guild-id
           :channel-id channel-id
           :status status
           :channel-ids (and channel-id (list channel-id))))))

(defun disco-gateway--dispatch-channel-pins-update (payload)
  "Handle CHANNEL_PINS_UPDATE dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-id (alist-get 'channel_id payload))
        (last-pin-timestamp (alist-get 'last_pin_timestamp payload)))
    (disco-state-apply-channel-pins-update channel-id last-pin-timestamp)
    (disco-gateway--emit
     (list :type 'channel-pins-update
           :guild-id guild-id
           :channel-id channel-id
           :last-pin-timestamp last-pin-timestamp))))

(defun disco-gateway--dispatch-channel-pins-ack (payload)
  "Handle CHANNEL_PINS_ACK dispatch PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload))
        (timestamp (alist-get 'timestamp payload))
        (version (alist-get 'version payload)))
    (disco-state-apply-channel-pins-ack channel-id timestamp version)
    (disco-gateway--emit
     (list :type 'channel-pins-ack
           :channel-id channel-id
           :last-pin-timestamp timestamp
           :version version))))

(defun disco-gateway--dispatch-message-create (payload)
  "Handle MESSAGE_CREATE dispatch PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload)))
    (when channel-id
      (let ((watched (disco-gateway--channel-watched-p channel-id)))
        (if watched
            (disco-gateway--upsert-message channel-id payload)
          (disco-state-upsert-message channel-id payload))
        (disco-state-apply-message-create
         channel-id
         payload
         disco-gateway--current-user-id
         watched)
        (disco-gateway--emit
         (list :type 'message-create
               :channel-id channel-id
               :message payload
               :watched watched))))))

(defun disco-gateway--dispatch-message-update (payload)
  "Handle MESSAGE_UPDATE dispatch PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload)))
    (when (and channel-id (disco-gateway--channel-watched-p channel-id))
      (disco-gateway--upsert-message channel-id payload)
      (disco-gateway--emit
       (list :type 'message-update :channel-id channel-id :message payload)))))

(defun disco-gateway--dispatch-message-delete (payload)
  "Handle MESSAGE_DELETE dispatch PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload))
        (message-id (alist-get 'id payload)))
    (when (and channel-id message-id (disco-gateway--channel-watched-p channel-id))
      (disco-gateway--delete-message channel-id message-id)
      (disco-gateway--emit
       (list :type 'message-delete :channel-id channel-id :message-id message-id)))))

(defun disco-gateway--dispatch-message-reaction-add (payload)
  "Handle MESSAGE_REACTION_ADD dispatch PAYLOAD."
  (disco-gateway--emit-reaction-event 'message-reaction-add payload))

(defun disco-gateway--dispatch-message-reaction-remove (payload)
  "Handle MESSAGE_REACTION_REMOVE dispatch PAYLOAD."
  (disco-gateway--emit-reaction-event 'message-reaction-remove payload))

(defun disco-gateway--dispatch-message-reaction-remove-all (payload)
  "Handle MESSAGE_REACTION_REMOVE_ALL dispatch PAYLOAD."
  (disco-gateway--emit-reaction-event 'message-reaction-remove-all payload))

(defun disco-gateway--dispatch-message-reaction-remove-emoji (payload)
  "Handle MESSAGE_REACTION_REMOVE_EMOJI dispatch PAYLOAD."
  (disco-gateway--emit-reaction-event 'message-reaction-remove-emoji payload))

(defun disco-gateway--dispatch-message-poll-vote-add (payload)
  "Handle MESSAGE_POLL_VOTE_ADD dispatch PAYLOAD."
  (disco-gateway--emit-poll-vote-event 'message-poll-vote-add payload))

(defun disco-gateway--dispatch-message-poll-vote-remove (payload)
  "Handle MESSAGE_POLL_VOTE_REMOVE dispatch PAYLOAD."
  (disco-gateway--emit-poll-vote-event 'message-poll-vote-remove payload))

(defun disco-gateway--dispatch-message-ack (payload)
  "Handle MESSAGE_ACK dispatch PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload))
        (message-id (alist-get 'message_id payload))
        (mention-count (alist-get 'mention_count payload))
        (flags (alist-get 'flags payload))
        (last-viewed (alist-get 'last_viewed payload))
        (version (alist-get 'version payload)))
    (when channel-id
      (disco-state-apply-message-ack
       channel-id
       message-id
       mention-count
       flags
       last-viewed
       version)
      (disco-gateway--emit
       (list :type 'message-ack
             :channel-id channel-id
             :message-id message-id
             :mention-count mention-count
             :flags flags
             :last-viewed last-viewed
             :version version)))))

(defun disco-gateway--dispatch-guild-feature-ack (payload)
  "Handle GUILD_FEATURE_ACK dispatch PAYLOAD."
  (let ((read-state-type (alist-get 'ack_type payload))
        (resource-id (alist-get 'resource_id payload))
        (entity-id (alist-get 'entity_id payload))
        (version (alist-get 'version payload)))
    (disco-state-apply-feature-ack
     read-state-type
     resource-id
     entity-id
     version)
    (disco-gateway--emit
     (list :type 'guild-feature-ack
           :read-state-type read-state-type
           :resource-id resource-id
           :entity-id entity-id
           :version version))))

(defun disco-gateway--dispatch-user-non-channel-ack (payload)
  "Handle USER_NON_CHANNEL_ACK dispatch PAYLOAD."
  (let ((read-state-type (alist-get 'ack_type payload))
        (resource-id (alist-get 'resource_id payload))
        (entity-id (alist-get 'entity_id payload))
        (version (alist-get 'version payload)))
    (disco-state-apply-feature-ack
     read-state-type
     resource-id
     entity-id
     version)
    (disco-gateway--emit
     (list :type 'user-non-channel-ack
           :read-state-type read-state-type
           :resource-id resource-id
           :entity-id entity-id
           :version version))))

(defun disco-gateway--dispatch-notification-center-items-ack (payload)
  "Handle NOTIFICATION_CENTER_ITEMS_ACK dispatch PAYLOAD."
  (let ((entity-id (alist-get 'id payload))
        (resource-id disco-gateway--current-user-id))
    (disco-state-apply-feature-ack
     disco-read-state-type-notification-center
     resource-id
     entity-id)
    (disco-gateway--emit
     (list :type 'notification-center-items-ack
           :read-state-type disco-read-state-type-notification-center
           :resource-id resource-id
           :entity-id entity-id))))

(defun disco-gateway--dispatch-rate-limited (payload)
  "Handle RATE_LIMITED dispatch PAYLOAD."
  (let ((opcode (alist-get 'opcode payload))
        (retry-after (alist-get 'retry_after payload))
        (meta (alist-get 'meta payload)))
    (disco-gateway--set-send-cooldown opcode retry-after meta)
    (message "disco: gateway rate-limited opcode=%s retry-after=%s"
             (or opcode "?")
             (or retry-after "?"))
    (disco-gateway--emit
     (list :type 'rate-limited
           :opcode opcode
           :retry-after retry-after
           :meta meta))))

(defun disco-gateway--dispatch-typing-start (payload)
  "Handle TYPING_START dispatch PAYLOAD."
  (let ((channel-id (alist-get 'channel_id payload))
        (guild-id (alist-get 'guild_id payload))
        (user-id (alist-get 'user_id payload))
        (timestamp (alist-get 'timestamp payload))
        (member (alist-get 'member payload)))
    (when channel-id
      (disco-gateway--emit
       (list :type 'typing-start
             :channel-id channel-id
             :guild-id guild-id
             :user-id user-id
             :timestamp timestamp
             :member member)))))

(defun disco-gateway--dispatch-thread-create (payload)
  "Handle THREAD_CREATE dispatch PAYLOAD."
  (disco-state-apply-thread-create payload disco-gateway--current-user-id)
  (disco-gateway--upsert-channel-and-emit 'thread-create payload))

(defun disco-gateway--dispatch-thread-update (payload)
  "Handle THREAD_UPDATE dispatch PAYLOAD."
  (disco-gateway--upsert-channel-and-emit 'thread-update payload))

(defun disco-gateway--dispatch-thread-delete (payload)
  "Handle THREAD_DELETE dispatch PAYLOAD."
  (disco-gateway--delete-channel-and-emit 'thread-delete payload))

(defun disco-gateway--dispatch-thread-list-sync (payload)
  "Handle THREAD_LIST_SYNC dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (channel-ids (alist-get 'channel_ids payload))
        (threads (alist-get 'threads payload)))
    (when guild-id
      (disco-state-sync-threads guild-id channel-ids (or threads '()))
      (disco-gateway--emit
       (list :type 'thread-list-sync
             :guild-id guild-id
             :channel-ids channel-ids
             :threads (or threads '()))))))

(defun disco-gateway--dispatch-thread-member-update (payload)
  "Handle THREAD_MEMBER_UPDATE dispatch PAYLOAD."
  (let* ((thread-id (disco-gateway--thread-id-from-payload payload))
         (guild-id (alist-get 'guild_id payload))
         (member (alist-get 'member payload))
         (member-user (and (listp member)
                           (alist-get 'user member)))
         (user-id (or (alist-get 'user_id payload)
                      (and (listp member-user)
                           (alist-get 'id member-user)))))
    (when (and thread-id user-id)
      (disco-state-upsert-thread-member thread-id user-id))
    (disco-gateway--emit
     (list :type 'thread-member-update
           :channel-id thread-id
           :thread-id thread-id
           :guild-id guild-id
           :user-id user-id
           :thread-member payload))))

(defun disco-gateway--dispatch-thread-members-update (payload)
  "Handle THREAD_MEMBERS_UPDATE dispatch PAYLOAD."
  (let* ((thread-id (disco-gateway--thread-id-from-payload payload))
         (guild-id (alist-get 'guild_id payload))
         (added-members (or (alist-get 'added_members payload) '()))
         (removed-member-ids (or (alist-get 'removed_member_ids payload) '()))
         (member-count (alist-get 'member_count payload)))
    (when thread-id
      (disco-state-apply-thread-members-update
       thread-id
       added-members
       removed-member-ids
       member-count))
    (disco-gateway--emit
     (list :type 'thread-members-update
           :channel-id thread-id
           :thread-id thread-id
           :guild-id guild-id
           :added-members added-members
           :removed-member-ids removed-member-ids
           :member-count member-count))))

(defun disco-gateway--dispatch-user-update (payload)
  "Handle USER_UPDATE dispatch PAYLOAD."
  (disco-state-apply-user-update)
  (let ((user-id (alist-get 'id payload)))
    (when user-id
      (setq disco-gateway--current-user-id user-id))))

(defun disco-gateway--dispatch-guild-members-chunk (payload)
  "Handle GUILD_MEMBERS_CHUNK dispatch PAYLOAD."
  (let ((guild-id (alist-get 'guild_id payload))
        (members (or (alist-get 'members payload) '()))
        (presences (or (alist-get 'presences payload) '()))
        (chunk-index (alist-get 'chunk_index payload))
        (chunk-count (alist-get 'chunk_count payload))
        (nonce (alist-get 'nonce payload)))
    (when guild-id
      (disco-state-apply-guild-members-chunk guild-id members presences))
    (disco-gateway--emit
     (list :type 'guild-members-chunk
           :guild-id guild-id
           :members members
           :presences presences
           :chunk-index chunk-index
           :chunk-count chunk-count
           :nonce nonce))))

;; Gateway dispatch event names are UPPER_CASE in Discord docs.
(defconst disco-gateway--dispatch-handler-alist
  '(("READY" . disco-gateway--dispatch-ready)
    ("RESUMED" . disco-gateway--dispatch-resumed)
    ("GUILD_MEMBERS_CHUNK" . disco-gateway--dispatch-guild-members-chunk)
    ("GUILD_CREATE" . disco-gateway--dispatch-guild-create)
    ("GUILD_UPDATE" . disco-gateway--dispatch-guild-update)
    ("GUILD_DELETE" . disco-gateway--dispatch-guild-delete)
    ("CHANNEL_CREATE" . disco-gateway--dispatch-channel-create)
    ("CHANNEL_UPDATE" . disco-gateway--dispatch-channel-update)
    ("CHANNEL_DELETE" . disco-gateway--dispatch-channel-delete)
    ("CHANNEL_UPDATE_PARTIAL" . disco-gateway--dispatch-channel-update-partial)
    ("CHANNEL_UNREAD_UPDATE" . disco-gateway--dispatch-channel-unread-update)
    ("CHANNEL_STATUSES" . disco-gateway--dispatch-channel-statuses)
    ("CHANNEL_INFO" . disco-gateway--dispatch-channel-info)
    ("CHANNEL_MEMBER_COUNT_UPDATE" . disco-gateway--dispatch-channel-member-count-update)
    ("PASSIVE_UPDATE_V1" . disco-gateway--dispatch-passive-update-v1)
    ("PASSIVE_UPDATE_V2" . disco-gateway--dispatch-passive-update-v2)
    ("VOICE_STATE_UPDATE" . disco-gateway--dispatch-voice-state-update)
    ("VOICE_CHANNEL_START_TIME_UPDATE" . disco-gateway--dispatch-voice-channel-start-time-update)
    ("VOICE_CHANNEL_STATUS_UPDATE" . disco-gateway--dispatch-voice-channel-status-update)
    ("CHANNEL_PINS_UPDATE" . disco-gateway--dispatch-channel-pins-update)
    ("CHANNEL_PINS_ACK" . disco-gateway--dispatch-channel-pins-ack)
    ("MESSAGE_CREATE" . disco-gateway--dispatch-message-create)
    ("MESSAGE_UPDATE" . disco-gateway--dispatch-message-update)
    ("MESSAGE_DELETE" . disco-gateway--dispatch-message-delete)
    ("MESSAGE_REACTION_ADD" . disco-gateway--dispatch-message-reaction-add)
    ("MESSAGE_REACTION_REMOVE" . disco-gateway--dispatch-message-reaction-remove)
    ("MESSAGE_REACTION_REMOVE_ALL" . disco-gateway--dispatch-message-reaction-remove-all)
    ("MESSAGE_REACTION_REMOVE_EMOJI" . disco-gateway--dispatch-message-reaction-remove-emoji)
    ("MESSAGE_POLL_VOTE_ADD" . disco-gateway--dispatch-message-poll-vote-add)
    ("MESSAGE_POLL_VOTE_REMOVE" . disco-gateway--dispatch-message-poll-vote-remove)
    ("MESSAGE_ACK" . disco-gateway--dispatch-message-ack)
    ("LAST_MESSAGES" . disco-gateway--dispatch-last-messages)
    ("CONVERSATION_SUMMARY_UPDATE" . disco-gateway--dispatch-conversation-summary-update)
    ("PRESENCE_UPDATE" . disco-gateway--dispatch-presence-update)
    ("SESSIONS_REPLACE" . disco-gateway--dispatch-sessions-replace)
    ("GUILD_FEATURE_ACK" . disco-gateway--dispatch-guild-feature-ack)
    ("USER_NON_CHANNEL_ACK" . disco-gateway--dispatch-user-non-channel-ack)
    ("NOTIFICATION_CENTER_ITEMS_ACK" . disco-gateway--dispatch-notification-center-items-ack)
    ("RATE_LIMITED" . disco-gateway--dispatch-rate-limited)
    ("TYPING_START" . disco-gateway--dispatch-typing-start)
    ("THREAD_CREATE" . disco-gateway--dispatch-thread-create)
    ("THREAD_UPDATE" . disco-gateway--dispatch-thread-update)
    ("THREAD_DELETE" . disco-gateway--dispatch-thread-delete)
    ("THREAD_LIST_SYNC" . disco-gateway--dispatch-thread-list-sync)
    ("THREAD_MEMBER_UPDATE" . disco-gateway--dispatch-thread-member-update)
    ("THREAD_MEMBERS_UPDATE" . disco-gateway--dispatch-thread-members-update)
    ("USER_UPDATE" . disco-gateway--dispatch-user-update))
  "Declarative map of Dispatch event name to handler function.")

(defun disco-gateway--handle-dispatch (event-type payload)
  "Handle one dispatch EVENT-TYPE with PAYLOAD data."
  (let ((handler (cdr (assoc event-type disco-gateway--dispatch-handler-alist))))
    (when (functionp handler)
      (funcall handler payload))))

(defun disco-gateway--handle-op-dispatch (payload)
  "Handle Gateway opcode 0 Dispatch PAYLOAD."
  (disco-gateway--handle-dispatch
   (alist-get 't payload)
   (alist-get 'd payload)))

(defun disco-gateway--handle-op-heartbeat-request (_payload)
  "Handle Gateway opcode 1 Heartbeat request.

Discord may request immediate heartbeats outside the regular interval."
  (disco-gateway--send-heartbeat))

(defun disco-gateway--handle-op-hello (payload)
  "Handle Gateway opcode 10 Hello PAYLOAD."
  (let ((interval (alist-get 'heartbeat_interval (alist-get 'd payload))))
    (disco-gateway--start-heartbeat interval)
    (if (disco-gateway--can-resume-p)
        (disco-gateway--send-resume)
      (disco-gateway--send-identify))))

(defun disco-gateway--handle-op-heartbeat-ack (_payload)
  "Handle Gateway opcode 11 Heartbeat ACK."
  (setq disco-gateway--awaiting-heartbeat-ack nil))

(defun disco-gateway--handle-op-reconnect (_payload)
  "Handle Gateway opcode 7 Reconnect."
  (message "disco: gateway requested reconnect")
  (disco-gateway--reconnect 1 nil))

(defun disco-gateway--handle-op-invalid-session (payload)
  "Handle Gateway opcode 9 Invalid Session PAYLOAD."
  (let* ((resumable (eq (alist-get 'd payload) t))
         (delay (disco-gateway--random-between
                 disco-gateway-invalid-session-min-delay
                 disco-gateway-invalid-session-max-delay)))
    (message "disco: invalid session (resumable=%s), reconnecting in %.2fs"
             resumable delay)
    (disco-gateway--reconnect delay (not resumable))))

;; Includes opcode 1 handler for heartbeat requests from server.
(defconst disco-gateway--opcode-handler-alist
  '((0 . disco-gateway--handle-op-dispatch)
    (1 . disco-gateway--handle-op-heartbeat-request)
    (7 . disco-gateway--handle-op-reconnect)
    (9 . disco-gateway--handle-op-invalid-session)
    (10 . disco-gateway--handle-op-hello)
    (11 . disco-gateway--handle-op-heartbeat-ack))
  "Declarative map of Gateway opcode integer to handler function.")

(defun disco-gateway--handle-payload (payload)
  "Handle one decoded gateway PAYLOAD."
  (let ((seq (alist-get 's payload)))
    (when seq
      (setq disco-gateway--seq seq))
    (let* ((op (alist-get 'op payload))
           (handler (cdr (assoc op disco-gateway--opcode-handler-alist))))
      (when (functionp handler)
        (funcall handler payload)))))

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
  (unless (disco-current-token)
    (user-error "disco: token is not set; use M-x disco-set-token or DISCO_TOKEN")))

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
                        (disco-gateway--reset-send-rate-state)
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
                         (disco-gateway--reset-send-rate-state)
                         (unless (or disco-gateway--stopping
                                     (timerp disco-gateway--reconnect-timer))
                           (message "disco: gateway websocket closed, reconnecting")
                           (disco-gateway--schedule-reconnect nil)))
             :on-error (lambda (_ws _type err)
                         (setq disco-gateway--connecting nil)
                         (disco-gateway--zlib-reset-state)
                         (disco-gateway--reset-send-rate-state)
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
             (disco-gateway--watchers-active-p))
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
    (disco-gateway-start)
    (disco-gateway--maybe-subscribe-watched-channel channel-id)))

(defun disco-gateway-watch-global ()
  "Start watching gateway dispatch globally for non-channel consumers."
  (setq disco-gateway--global-watch-count (1+ disco-gateway--global-watch-count))
  (when disco-enable-live-updates
    (disco-gateway-start)))

(defun disco-gateway-unwatch-channel (channel-id)
  "Decrease watch count for CHANNEL-ID and stop when no channels remain."
  (let ((count (gethash channel-id disco-gateway--watch-counts 0)))
    (cond
     ((<= count 1)
      (remhash channel-id disco-gateway--watch-counts)
      (remhash channel-id disco-gateway--lazy-subscribed-channels))
     (t
      (puthash channel-id (1- count) disco-gateway--watch-counts))))
  (unless (disco-gateway--watchers-active-p)
    (disco-gateway-stop)))

(defun disco-gateway-unwatch-global ()
  "Decrease global watch count and stop if no gateway consumers remain."
  (setq disco-gateway--global-watch-count
        (max 0 (1- disco-gateway--global-watch-count)))
  (unless (disco-gateway--watchers-active-p)
    (disco-gateway-stop)))

(defun disco-gateway-describe-status ()
  "Display current gateway transport status in minibuffer."
  (interactive)
  (message "disco-gateway: running=%s connecting=%s watched-channels=%d watched-global=%d seq=%s session=%s reconnect-attempt=%d reconnect-pending=%s"
           (and disco-gateway--ws (websocket-openp disco-gateway--ws))
           disco-gateway--connecting
           (hash-table-count disco-gateway--watch-counts)
           disco-gateway--global-watch-count
           (or disco-gateway--seq "nil")
           (if disco-gateway--session-id "set" "nil")
           disco-gateway--reconnect-attempt
           (and (timerp disco-gateway--reconnect-timer) t)))

(provide 'disco-gateway)

;;; disco-gateway.el ends here
