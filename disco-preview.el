;;; disco-preview.el --- Shared Discord channel preview hydration -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Owns the request lifecycle for Discord client Gateway opcode 34 (Request
;; Last Messages).  Views submit channels whose `last_message_id' is known but
;; whose message object is absent from local state.  Requests are deduplicated,
;; grouped by guild, limited to one in-flight batch per guild, and retried after
;; Gateway reconnects, rate limits, or response timeouts.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-util)

(defcustom disco-preview-fetch-enabled t
  "When non-nil, hydrate missing channel previews through Gateway opcode 34."
  :type 'boolean
  :group 'disco)

(defcustom disco-preview-fetch-debounce 0.35
  "Seconds to debounce batches of channel preview requests."
  :type 'number
  :group 'disco)

(defcustom disco-preview-response-timeout 15
  "Seconds to wait for a LAST_MESSAGES response before retrying its batch."
  :type 'number
  :group 'disco)

(defconst disco-preview--gateway-batch-limit 100
  "Maximum channel IDs accepted by one Gateway opcode 34 request.")

(defvar disco-preview--timer nil
  "Timer scheduled to flush pending channel preview requests.")

(defvar disco-preview--timer-deadline nil
  "Absolute time when `disco-preview--timer' is due to run.")

(defvar disco-preview--pending-by-guild (make-hash-table :test #'equal)
  "Hash table mapping guild IDs to pending channel ID lists.")

(defvar disco-preview--requested-message-id-by-channel
  (make-hash-table :test #'equal)
  "Hash table mapping channel IDs to the latest requested message ID.")

(defvar disco-preview--in-flight-by-guild (make-hash-table :test #'equal)
  "Hash table mapping guild IDs to in-flight request plists.")

(defvar disco-preview--blocked-until-by-guild (make-hash-table :test #'equal)
  "Hash table mapping rate-limited guild IDs to retry deadlines.")

(defun disco-preview--cancel-scheduled-pass ()
  "Cancel the currently scheduled lifecycle pass, if any."
  (when (timerp disco-preview--timer)
    (cancel-timer disco-preview--timer))
  (setq disco-preview--timer nil
        disco-preview--timer-deadline nil))

(defun disco-preview-reset ()
  "Reset all preview request lifecycle state."
  (disco-preview--cancel-scheduled-pass)
  (clrhash disco-preview--pending-by-guild)
  (clrhash disco-preview--requested-message-id-by-channel)
  (clrhash disco-preview--in-flight-by-guild)
  (clrhash disco-preview--blocked-until-by-guild))

(defun disco-preview--pending-p ()
  "Return non-nil when at least one guild has queued channel IDs."
  (> (hash-table-count disco-preview--pending-by-guild) 0))

(defun disco-preview--work-p ()
  "Return non-nil when pending or in-flight preview work exists."
  (or (disco-preview--pending-p)
      (> (hash-table-count disco-preview--in-flight-by-guild) 0)))

(defun disco-preview--blocked-until (guild-id)
  "Return the effective rate-limit deadline for GUILD-ID, or nil."
  (let ((guild-deadline
         (gethash guild-id disco-preview--blocked-until-by-guild))
        (global-deadline
         (gethash :global disco-preview--blocked-until-by-guild)))
    (cond
     ((and (numberp guild-deadline) (numberp global-deadline))
      (max guild-deadline global-deadline))
     ((numberp guild-deadline) guild-deadline)
     ((numberp global-deadline) global-deadline))))

(defun disco-preview--retry-delay ()
  "Return seconds until the next useful lifecycle pass."
  (let ((now (float-time))
        deadline
        sendable)
    (maphash
     (lambda (guild-id pending)
       (when pending
         (let ((request (gethash guild-id disco-preview--in-flight-by-guild))
               (blocked-until (disco-preview--blocked-until guild-id)))
           (cond
            (request
             nil)
            ((and (numberp blocked-until) (> blocked-until now))
             (setq deadline (min (or deadline most-positive-fixnum)
                                 blocked-until)))
            (t
             (setq sendable t))))))
     disco-preview--pending-by-guild)
    (maphash
     (lambda (_guild-id request)
       (when-let* ((sent-at (plist-get request :sent-at)))
         (setq deadline
               (min (or deadline most-positive-fixnum)
                    (+ sent-at (max 0.1 disco-preview-response-timeout))))))
     disco-preview--in-flight-by-guild)
    (if sendable
        (max 0 disco-preview-fetch-debounce)
      (max 0.01 (- (or deadline
                       (+ now (max 0 disco-preview-fetch-debounce)))
                   now)))))

(defun disco-preview--schedule ()
  "Schedule the next useful request or timeout lifecycle pass."
  (if (not (and disco-preview-fetch-enabled
                (disco-preview--work-p)
                (disco-gateway-running-p)))
      (disco-preview--cancel-scheduled-pass)
    (let* ((delay (disco-preview--retry-delay))
           (deadline (+ (float-time) delay)))
      (when (and (timerp disco-preview--timer)
                 (or (not (numberp disco-preview--timer-deadline))
                     (< deadline disco-preview--timer-deadline)))
        (disco-preview--cancel-scheduled-pass))
      (unless (timerp disco-preview--timer)
        (setq disco-preview--timer-deadline deadline
              disco-preview--timer
              (run-with-timer delay nil #'disco-preview--flush))))))

(defun disco-preview--enqueue-channel-id (guild-id channel-id)
  "Queue CHANNEL-ID under GUILD-ID while preserving insertion order."
  (let ((pending (gethash guild-id disco-preview--pending-by-guild)))
    (unless (member channel-id pending)
      (puthash guild-id
               (append pending (list channel-id))
               disco-preview--pending-by-guild)
      t)))

(defun disco-preview-request-channel (channel)
  "Queue CHANNEL preview hydration.

Return non-nil when CHANNEL was newly queued."
  (when (and disco-preview-fetch-enabled (listp channel))
    (let* ((guild-id (and (alist-get 'guild_id channel)
                          (format "%s" (alist-get 'guild_id channel))))
           (channel-id (and (alist-get 'id channel)
                            (format "%s" (alist-get 'id channel))))
           (message-id (and (alist-get 'last_message_id channel)
                            (format "%s" (alist-get 'last_message_id channel))))
           (requested-id
            (and channel-id
                 (gethash channel-id
                          disco-preview--requested-message-id-by-channel))))
      (when (and guild-id
                 channel-id
                 message-id
                 (not (disco-msg-channel-last-cached-message channel))
                 (not (equal requested-id message-id)))
        (puthash channel-id message-id
                 disco-preview--requested-message-id-by-channel)
        (let ((queued (disco-preview--enqueue-channel-id guild-id channel-id)))
          (disco-preview--schedule)
          queued)))))

(defun disco-preview--requeue-batch (guild-id channel-ids)
  "Return CHANNEL-IDS to the front of GUILD-ID's pending queue."
  (let ((pending (gethash guild-id disco-preview--pending-by-guild)))
    (puthash guild-id
             (disco-util-normalize-id-list (append channel-ids pending))
             disco-preview--pending-by-guild)))

(defun disco-preview--in-flight-expired-p (request now)
  "Return non-nil when in-flight REQUEST has expired at NOW."
  (let ((sent-at (plist-get request :sent-at)))
    (and (numberp sent-at)
         (>= (- now sent-at)
             (max 0.1 disco-preview-response-timeout)))))

(defun disco-preview--expire-in-flight (now)
  "Requeue in-flight requests expired at NOW."
  (let (expired-guilds)
    (maphash
     (lambda (guild-id request)
       (when (disco-preview--in-flight-expired-p request now)
         (disco-preview--requeue-batch guild-id (plist-get request :channel-ids))
         (push guild-id expired-guilds)))
     disco-preview--in-flight-by-guild)
    (dolist (guild-id expired-guilds)
      (remhash guild-id disco-preview--in-flight-by-guild))))

(defun disco-preview--expire-rate-limits (now)
  "Forget guild rate-limit deadlines reached at NOW."
  (let (expired-guilds)
    (maphash
     (lambda (guild-id deadline)
       (when (or (not (numberp deadline)) (<= deadline now))
         (push guild-id expired-guilds)))
     disco-preview--blocked-until-by-guild)
    (dolist (guild-id expired-guilds)
      (remhash guild-id disco-preview--blocked-until-by-guild))))

(defun disco-preview--guild-blocked-p (guild-id now)
  "Return non-nil when GUILD-ID remains rate-limited at NOW."
  (when-let* ((deadline (disco-preview--blocked-until guild-id)))
    (and (numberp deadline) (> deadline now))))

(defun disco-preview--flush ()
  "Send one pending opcode 34 batch per available guild."
  (setq disco-preview--timer nil
        disco-preview--timer-deadline nil)
  (when (and disco-preview-fetch-enabled
             (disco-gateway-running-p))
    (let ((now (float-time))
          updates)
      (disco-preview--expire-in-flight now)
      (disco-preview--expire-rate-limits now)
      (maphash
       (lambda (guild-id pending)
         (let ((ordered (disco-util-normalize-id-list pending)))
           (cond
            ((null ordered)
             (push (cons guild-id nil) updates))
            ((gethash guild-id disco-preview--in-flight-by-guild)
             nil)
            ((disco-preview--guild-blocked-p guild-id now)
             nil)
            ((not (disco-gateway-send-queue-slot-available-p))
             nil)
            (t
             (let* ((batch (seq-take ordered
                                     disco-preview--gateway-batch-limit))
                    (remaining (nthcdr (length batch) ordered)))
               (when (disco-gateway-request-last-messages guild-id batch)
                 (puthash guild-id
                          (list :channel-ids batch :sent-at now)
                          disco-preview--in-flight-by-guild)
                 (push (cons guild-id remaining) updates)))))))
       disco-preview--pending-by-guild)
      (dolist (update updates)
        (if (cdr update)
            (puthash (car update) (cdr update)
                     disco-preview--pending-by-guild)
          (remhash (car update) disco-preview--pending-by-guild))))
    (disco-preview--schedule)))

(defun disco-preview--requeue-all-in-flight ()
  "Return every in-flight request to its guild queue."
  (maphash
   (lambda (guild-id request)
     (disco-preview--requeue-batch guild-id (plist-get request :channel-ids)))
   disco-preview--in-flight-by-guild)
  (clrhash disco-preview--in-flight-by-guild))

(defun disco-preview--handle-gateway-event (event)
  "Advance preview requests after Gateway EVENT."
  (pcase (plist-get event :type)
    ('ready
     (disco-preview--requeue-all-in-flight)
     (clrhash disco-preview--blocked-until-by-guild)
     (disco-preview--schedule))
    ('last-messages
     (when-let* ((guild-id (plist-get event :guild-id)))
       (remhash (format "%s" guild-id) disco-preview--in-flight-by-guild)
       (disco-preview--schedule)))
    ('rate-limited
     (when (equal (format "%s" (plist-get event :opcode)) "34")
       (let* ((meta (plist-get event :meta))
              (guild-id (and (listp meta) (alist-get 'guild_id meta)))
              (retry-after (plist-get event :retry-after)))
         (if guild-id
             (let* ((normalized-guild-id (format "%s" guild-id))
                    (request (gethash normalized-guild-id
                                      disco-preview--in-flight-by-guild)))
               (when request
                 (disco-preview--requeue-batch
                  normalized-guild-id (plist-get request :channel-ids))
                 (remhash normalized-guild-id
                          disco-preview--in-flight-by-guild)))
           (disco-preview--requeue-all-in-flight))
         (when (and (numberp retry-after) (> retry-after 0))
           (puthash (if guild-id (format "%s" guild-id) :global)
                    (+ (float-time) retry-after)
                    disco-preview--blocked-until-by-guild))
         (disco-preview--schedule))))))

(add-hook 'disco-gateway-event-hook #'disco-preview--handle-gateway-event)

(provide 'disco-preview)

;;; disco-preview.el ends here
