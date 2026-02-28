;;; disco-room.el --- Channel room buffers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Per-channel room buffer with simple timeline rendering and message sending.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'seq)
(require 'ring)
(require 'cl-lib)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-transient)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--oldest-message-id nil)
(defvar-local disco-room--newest-message-id nil)
(defvar-local disco-room--history-exhausted nil)
(defvar-local disco-room--pending-reply-to nil)
(defvar-local disco-room--gateway-handler nil)
(defvar-local disco-room--refresh-generation 0)
(defvar-local disco-room--refresh-in-flight nil)
(defvar-local disco-room--older-in-flight nil)
(defvar-local disco-room--draft-input "")
(defvar-local disco-room--input-ring nil)
(defvar-local disco-room--input-index nil)
(defvar-local disco-room--input-pending nil)
(defvar-local disco-room--send-in-flight nil)

(defcustom disco-room-input-history-size 30
  "Maximum number of draft entries kept in room input history."
  :type 'integer
  :group 'disco)

(defcustom disco-room-send-on-return t
  "When non-nil, `RET' in room buffer sends current draft."
  :type 'boolean
  :group 'disco)

(defun disco-room--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-room--json-true-p (value)
  "Return non-nil when VALUE semantically represents JSON true."
  (or (eq value t)
      (eq value 'true)
      (equal value "true")))

(defun disco-room--thread-channel-p (&optional channel)
  "Return non-nil when CHANNEL (or current room channel) is a thread."
  (let ((target (or channel (disco-room--channel-object))))
    (and target (disco-state-channel-thread-p target))))

(defun disco-room--thread-metadata (&optional channel)
  "Return thread metadata alist for CHANNEL or current room channel."
  (let ((target (or channel (disco-room--channel-object))))
    (or (alist-get 'thread_metadata target) '())))

(defun disco-room--thread-archived-p (&optional channel)
  "Return non-nil when CHANNEL thread is archived."
  (let* ((target (or channel (disco-room--channel-object)))
         (meta (disco-room--thread-metadata target)))
    (or (disco-room--json-true-p (alist-get 'archived meta))
        (disco-room--json-true-p (alist-get 'archived target)))))

(defun disco-room--thread-locked-p (&optional channel)
  "Return non-nil when CHANNEL thread is locked."
  (let* ((target (or channel (disco-room--channel-object)))
         (meta (disco-room--thread-metadata target)))
    (or (disco-room--json-true-p (alist-get 'locked meta))
        (disco-room--json-true-p (alist-get 'locked target)))))

(defun disco-room--thread-header-suffix ()
  "Return human-readable status suffix for thread header."
  (if (not (disco-room--thread-channel-p))
      ""
    (let (tags)
      (when (disco-room--thread-archived-p)
        (push "archived" tags))
      (when (disco-room--thread-locked-p)
        (push "locked" tags))
      (when (= (alist-get 'type (disco-room--channel-object)) 12)
        (push "private" tags))
      (if tags
          (format " [thread: %s]" (mapconcat #'identity (nreverse tags) ", "))
        " [thread]"))))

(defun disco-room--ensure-thread-channel ()
  "Signal user error unless current room channel is a thread."
  (unless (disco-room--thread-channel-p)
    (user-error "disco: current room is not a thread")))

(defun disco-room--ensure-parent-channel ()
  "Signal user error when current room channel is itself a thread."
  (when (disco-room--thread-channel-p)
    (user-error "disco: open a parent channel room to create a new thread")))

(defun disco-room--latest-message-id ()
  "Return newest known message ID for current room, or nil."
  (alist-get 'id (car (disco-state-messages disco-room--channel-id))))

(defun disco-room--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (format "%S" err)))

(defun disco-room--callback-active-p (room-buffer channel-id generation)
  "Return non-nil when async callback state still matches ROOM-BUFFER context."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (= disco-room--refresh-generation generation)))))

(defun disco-room--channel-buffer-p (room-buffer channel-id)
  "Return non-nil when ROOM-BUFFER is alive and still bound to CHANNEL-ID."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)))))

(defun disco-room--current-draft ()
  "Return current room draft string."
  (or disco-room--draft-input ""))

(defun disco-room--set-draft (text)
  "Set room draft TEXT and re-render room."
  (setq disco-room--draft-input (or text ""))
  (disco-room-render))

(defun disco-room--clear-draft ()
  "Clear room draft and reset draft history navigation state."
  (setq disco-room--draft-input "")
  (setq disco-room--input-index nil)
  (setq disco-room--input-pending nil))

(defun disco-room--input-history-push (input)
  "Push INPUT into draft history ring when non-empty and distinct."
  (let ((normalized (string-trim-right (or input ""))))
    (unless (or (string-empty-p normalized)
                (and disco-room--input-ring
                     (> (ring-length disco-room--input-ring) 0)
                     (equal normalized (ring-ref disco-room--input-ring 0))))
      (ring-insert disco-room--input-ring normalized)))
  (setq disco-room--input-index nil)
  (setq disco-room--input-pending nil))

(defun disco-room--input-history-goto (index)
  "Switch draft view to history entry INDEX.

When INDEX is nil, restore pending draft text."
  (setq disco-room--input-index index)
  (if (null index)
      (setq disco-room--draft-input (or disco-room--input-pending ""))
    (setq disco-room--draft-input (ring-ref disco-room--input-ring index)))
  (disco-room-render))

(defun disco-room-draft-prev (&optional n)
  "Replace draft with N previous entries from draft history."
  (interactive "p")
  (let* ((step (max 1 (or n 1)))
         (ring-size (and disco-room--input-ring (ring-length disco-room--input-ring))))
    (cond
     ((or (null ring-size) (= ring-size 0))
      (message "disco: draft history is empty"))
     (t
      (unless (integerp disco-room--input-index)
        (setq disco-room--input-pending (disco-room--current-draft))
        (setq disco-room--input-index -1))
      (let ((target (min (1- ring-size) (+ disco-room--input-index step))))
        (disco-room--input-history-goto target))))))

(defun disco-room-draft-next (&optional n)
  "Replace draft with N newer entries from draft history."
  (interactive "p")
  (let ((step (max 1 (or n 1))))
    (if (not (integerp disco-room--input-index))
        (message "disco: already at latest draft")
      (let ((target (- disco-room--input-index step)))
        (if (< target 0)
            (disco-room--input-history-goto nil)
          (disco-room--input-history-goto target))))))

(defun disco-room-edit-draft ()
  "Edit current room draft in minibuffer and re-render room."
  (interactive)
  (let ((updated (read-from-minibuffer "Draft: " (disco-room--current-draft))))
    (setq disco-room--input-index nil)
    (setq disco-room--input-pending nil)
    (disco-room--set-draft updated)))

(defun disco-room--mark-read (&optional message-id)
  "Mark current room as read and acknowledge MESSAGE-ID.

When MESSAGE-ID is nil, acknowledge the newest known message in the room.
Unread counters are always cleared locally."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (generation disco-room--refresh-generation)
         (channel (disco-room--channel-object))
         (target-id (or message-id
                        (disco-room--latest-message-id)
                        (and channel (alist-get 'last_message_id channel))))
         (last-read-id (disco-state-channel-last-read-message-id channel-id))
         (should-ack (and target-id
                          (or (null last-read-id)
                              (disco-state-snowflake< last-read-id target-id)))))
    (disco-state-clear-channel-unread channel-id)
    (when should-ack
      (let ((token (disco-state-channel-ack-token channel-id)))
        (disco-api-ack-message-async
         channel-id
         target-id
         :token token
         :on-success
         (lambda (response)
           (when (disco-room--callback-active-p room-buffer channel-id generation)
             (disco-state-set-channel-last-read-message-id channel-id target-id)
             (let ((token-pair (and (listp response) (assq 'token response))))
               (when token-pair
                 (disco-state-set-channel-ack-token channel-id (cdr token-pair))))))
         :on-error
         (lambda (err)
           (message "disco: read-state ack failed for %s: %s"
                    channel-id
                    (disco-room--async-error-message err))))))))

(defun disco-room--update-message-window-state (messages)
  "Update pagination cursors from MESSAGES (newest-first list)."
  (setq disco-room--newest-message-id (and messages (alist-get 'id (car messages))))
  (setq disco-room--oldest-message-id
        (and messages (alist-get 'id (car (last messages))))))

(defun disco-room--merge-message-pages (existing older)
  "Merge EXISTING newest-first messages with OLDER page, de-duplicated.

Both EXISTING and OLDER are newest-first lists."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (msg (append existing older))
      (let ((message-id (alist-get 'id msg)))
        (unless (and message-id (gethash message-id seen))
          (when message-id
            (puthash message-id t seen))
          (push msg merged))))
    (nreverse merged)))

(defun disco-room--message-id-at-point ()
  "Return message ID at point, or signal a user error.

Message lines carry the `disco-message-id' text property."
  (or (get-text-property (point) 'disco-message-id)
      (get-text-property (line-beginning-position) 'disco-message-id)
      (user-error "disco: point is not on a message line")))

(defun disco-room--message-by-id (message-id)
  "Return room message object for MESSAGE-ID, or nil."
  (seq-find (lambda (msg)
              (equal (alist-get 'id msg) message-id))
            (or (disco-state-messages disco-room--channel-id) '())))

(defun disco-room--message-at-point ()
  "Return message object at point, or signal user error."
  (let* ((message-id (disco-room--message-id-at-point))
         (msg (disco-room--message-by-id message-id)))
    (or msg
        (user-error "disco: message not found in local room cache"))))

(defun disco-room--read-thread-auto-archive-duration ()
  "Prompt for optional auto archive duration in minutes.

Returns nil when left blank."
  (let* ((choices '("" "60" "1440" "4320" "10080"))
         (raw (completing-read
               "Auto archive minutes (empty for default): "
               choices nil t nil nil "")))
    (unless (string-empty-p raw)
      (string-to-number raw))))

(defun disco-room--read-optional-nonnegative-int (prompt)
  "Read optional non-negative integer using PROMPT.

Returns nil when left blank."
  (let ((raw (read-string prompt)))
    (unless (string-empty-p raw)
      (let ((n (string-to-number raw)))
        (when (< n 0)
          (user-error "disco: value must be >= 0"))
        n))))

(defun disco-room--read-detached-thread-type ()
  "Prompt for detached thread type; return numeric channel type or nil."
  (let ((choice (completing-read
                 "Thread type (empty/public/private): "
                 '("" "public" "private") nil t nil nil "")))
    (pcase choice
      ("public" 11)
      ("private" 12)
      (_ nil))))

(defun disco-room--forum-or-media-channel-p (&optional channel)
  "Return non-nil when CHANNEL (or current room channel) is forum/media."
  (let* ((target (or channel (disco-room--channel-object)))
         (type (and target (alist-get 'type target))))
    (memq type '(15 16))))

(defun disco-room--thread-with-meta-field (channel key value)
  "Return CHANNEL with thread metadata KEY set to VALUE."
  (let* ((updated (copy-tree channel))
         (meta (copy-tree (or (alist-get 'thread_metadata updated) '()))))
    (setf (alist-get key meta nil 'remove) value)
    (setf (alist-get 'thread_metadata updated nil 'remove) meta)
    updated))

(defun disco-room--buffer-name (channel-name channel-id)
  "Build room buffer name for CHANNEL-NAME and CHANNEL-ID."
  (format "*disco:%s (%s)*" channel-name channel-id))

(defun disco-room--format-time (iso8601)
  "Format ISO8601 into a compact local string."
  (condition-case _
      (format-time-string "%Y-%m-%d %H:%M"
                          (date-to-time iso8601))
    (error "unknown-time")))

(defun disco-room--message-author (msg)
  "Extract author name from message MSG alist."
  (let* ((author (alist-get 'author msg))
         (global-name (and (listp author) (alist-get 'global_name author)))
         (username (and (listp author) (alist-get 'username author))))
    (or global-name username "unknown")))

(defun disco-room--insert-message (msg)
  "Insert one message MSG in current buffer."
  (let ((timestamp (disco-room--format-time (or (alist-get 'timestamp msg) "")))
        (author (disco-room--message-author msg))
        (content (or (alist-get 'content msg) ""))
        (message-id (alist-get 'id msg))
        (line-start (point)))
    (insert (format "[%s] %s: %s\n" timestamp author content))
    (add-text-properties
     line-start
     (point)
     (list 'disco-message-id message-id))))

(defun disco-room-render ()
  "Render timeline for current room buffer."
  (let ((inhibit-read-only t)
        (messages (disco-state-messages disco-room--channel-id))
        (draft (disco-room--current-draft)))
    (erase-buffer)
    (insert (format "Channel: %s%s\n"
                    disco-room--channel-name
                    (disco-room--thread-header-suffix)))
    (insert "g: refresh   M-<: older   r/e/d: reply/edit/delete   RET/C-c C-c: send   C-c ': draft   M-p/M-n: history   q: quit")
    (when disco-room--refresh-in-flight
      (insert "   [refreshing...]"))
    (when disco-room--older-in-flight
      (insert "   [loading older...]"))
    (when disco-room--send-in-flight
      (insert "   [sending...]"))
    (insert "\n")
    (when disco-room--pending-reply-to
      (insert (format "Replying to: %s (C-c C-k to cancel)\n" disco-room--pending-reply-to)))
    (insert (format ">>> %s\n" draft))
    (when disco-room--history-exhausted
      (insert "(older history exhausted)\n"))
    (insert "\n")
    ;; API returns newest-first by default; reverse for chat-like display.
    (dolist (msg (reverse messages))
      (disco-room--insert-message msg))
    (goto-char (point-max))))

(defun disco-room-refresh ()
  "Fetch and redraw latest messages for current room asynchronously."
  (interactive)
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (generation (1+ disco-room--refresh-generation)))
    (setq disco-room--refresh-generation generation)
    (setq disco-room--refresh-in-flight t)
    (disco-api-channel-messages-async
     channel-id
     :on-success
     (lambda (messages)
       (when (disco-room--callback-active-p room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq disco-room--history-exhausted nil)
           (disco-state-put-messages channel-id messages)
           (disco-room--update-message-window-state messages)
           (disco-room--mark-read)
           (setq disco-room--refresh-in-flight nil)
           (disco-room-render)
           (message "disco: loaded %d messages" (length messages)))))
     :on-error
     (lambda (err)
       (when (disco-room--callback-active-p room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq disco-room--refresh-in-flight nil)
           (message "disco: room refresh failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room--close-for-deleted-channel (reason)
  "Close current room because its backing channel is no longer valid.

REASON is shown in the minibuffer."
  (let ((buf (current-buffer)))
    (disco-room--detach-live-updates)
    (kill-buffer buf)
    (message "%s" reason)))

(defun disco-room--handle-gateway-event (event)
  "Handle one EVENT plist from `disco-gateway-event-hook'."
  (let ((event-type (plist-get event :type))
        (event-channel-id (plist-get event :channel-id))
        (event-guild-id (plist-get event :guild-id)))
    (cond
     ((and (memq event-type '(channel-delete thread-delete))
           (equal event-channel-id disco-room--channel-id))
      (disco-room--close-for-deleted-channel
       (format "disco: channel %s was deleted"
               (or disco-room--channel-name disco-room--channel-id))))
     ((and (eq event-type 'guild-delete)
           disco-room--guild-id
           (equal event-guild-id disco-room--guild-id))
      (disco-room--close-for-deleted-channel
       (format "disco: guild for channel %s was deleted"
               (or disco-room--channel-name disco-room--channel-id))))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type
                 '(message-create message-update message-delete
                   channel-update thread-update)))
      (let ((at-bottom (= (point) (point-max)))
            (channel (disco-room--channel-object)))
        (when (and channel (alist-get 'name channel))
          (setq disco-room--channel-name (alist-get 'name channel)))
        (disco-room-render)
        (when (eq event-type 'message-create)
          (let* ((message (plist-get event :message))
                 (message-id (and (listp message) (alist-get 'id message))))
            (disco-room--mark-read message-id)))
        (when at-bottom
          (goto-char (point-max))))))))

(defun disco-room--attach-live-updates ()
  "Attach this room buffer to live update event stream."
  (when disco-room--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-room--gateway-handler)
    (when disco-room--channel-id
      (disco-gateway-unwatch-channel disco-room--channel-id)))
  (let ((room-buffer (current-buffer)))
    (setq disco-room--gateway-handler
          (lambda (event)
            (when (buffer-live-p room-buffer)
              (with-current-buffer room-buffer
                (disco-room--handle-gateway-event event))))))
  (add-hook 'disco-gateway-event-hook disco-room--gateway-handler)
  (disco-gateway-watch-channel disco-room--channel-id)
  (add-hook 'kill-buffer-hook #'disco-room--detach-live-updates nil t))

(defun disco-room--detach-live-updates ()
  "Detach this room buffer from live update event stream."
  (when disco-room--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-room--gateway-handler)
    (setq disco-room--gateway-handler nil))
  (when disco-room--channel-id
    (disco-gateway-unwatch-channel disco-room--channel-id)))

(defun disco-room-send-message ()
  "Send current draft message to this room asynchronously.

When called with prefix argument, force draft edit in minibuffer first."
  (interactive)
  (cond
   (disco-room--send-in-flight
    (message "disco: send already in progress"))
   (t
    (let* ((current-draft (disco-room--current-draft))
           (content (if (or current-prefix-arg
                            (string-empty-p (string-trim-right current-draft)))
                        (read-from-minibuffer "Message: " current-draft)
                      current-draft))
           (normalized (string-trim-right (or content ""))))
      (if (string-empty-p normalized)
          (message "disco: draft is empty")
        (let ((room-buffer (current-buffer))
              (channel-id disco-room--channel-id)
              (reply-to disco-room--pending-reply-to))
          (disco-room--input-history-push normalized)
          (setq disco-room--draft-input "")
          (setq disco-room--send-in-flight t)
          (disco-room-render)
          (disco-api-send-message-async
           channel-id
           normalized
           :reply-to-message-id reply-to
           :on-success
           (lambda (_response)
             (when (disco-room--channel-buffer-p room-buffer channel-id)
               (with-current-buffer room-buffer
                 (setq disco-room--send-in-flight nil)
                 (setq disco-room--pending-reply-to nil)
                 (disco-room-refresh)
                 (message "disco: message sent"))))
           :on-error
           (lambda (err)
             (when (disco-room--channel-buffer-p room-buffer channel-id)
               (with-current-buffer room-buffer
                 (setq disco-room--send-in-flight nil)
                 (setq disco-room--draft-input normalized)
                 (disco-room-render)
                 (message "disco: send failed: %s"
                          (disco-room--async-error-message err))))))))))))

(defun disco-room-load-older-messages ()
  "Load one older message page before the oldest loaded message asynchronously."
  (interactive)
  (cond
   (disco-room--history-exhausted
    (message "disco: no older messages available"))
   (disco-room--older-in-flight
    (message "disco: older history load already in progress"))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (generation disco-room--refresh-generation)
           (before (or disco-room--oldest-message-id
                       (user-error "disco: no oldest message cursor; refresh first"))))
      (setq disco-room--older-in-flight t)
      (disco-api-channel-messages-async
       channel-id
       :before before
       :on-success
       (lambda (older)
         (when (buffer-live-p room-buffer)
           (with-current-buffer room-buffer
             (when (equal channel-id disco-room--channel-id)
               (setq disco-room--older-in-flight nil)
               (if (/= generation disco-room--refresh-generation)
                   (message "disco: discarded stale older-history page")
                 (let ((existing (or (disco-state-messages channel-id) '())))
                   (if (null older)
                       (progn
                         (setq disco-room--history-exhausted t)
                         (disco-room-render)
                         (message "disco: reached beginning of history"))
                     (let ((merged (disco-room--merge-message-pages existing older)))
                       (disco-state-put-messages channel-id merged)
                       (disco-room--update-message-window-state merged)
                       (disco-room-render)
                       (message "disco: loaded %d older messages" (length older))))))))))
       :on-error
       (lambda (err)
         (when (buffer-live-p room-buffer)
           (with-current-buffer room-buffer
             (when (equal channel-id disco-room--channel-id)
               (setq disco-room--older-in-flight nil)
               (message "disco: older history load failed: %s"
                        (disco-room--async-error-message err)))))))))))

(defun disco-room-reply-to-message (&optional message-id)
  "Set pending reply target MESSAGE-ID for next send.

When called interactively, defaults to message under point."
  (interactive
   (let* ((at-point (ignore-errors (disco-room--message-id-at-point)))
          (fallback (or at-point (disco-room--latest-message-id)))
          (raw (read-string
                (if fallback
                    (format "Reply to message ID (default %s): " fallback)
                  "Reply to message ID: "))))
     (list (if (string-empty-p raw)
               (or fallback
                   (user-error "disco: no target message available"))
             raw))))
  (setq disco-room--pending-reply-to message-id)
  (disco-room-render)
  (message "disco: next message will reply to %s" message-id))

(defun disco-room-cancel-reply ()
  "Cancel pending reply target for next send."
  (interactive)
  (setq disco-room--pending-reply-to nil)
  (disco-room-render)
  (message "disco: reply target cleared"))

(defun disco-room-return-dwim ()
  "RET behavior for room buffer.

When `disco-room-send-on-return' is non-nil, send current draft.
Otherwise open draft editor."
  (interactive)
  (if disco-room-send-on-return
      (disco-room-send-message)
    (disco-room-edit-draft)))

(defun disco-room-edit-message ()
  "Edit message at point in current room."
  (interactive)
  (let* ((msg (disco-room--message-at-point))
         (message-id (alist-get 'id msg))
         (old-content (or (alist-get 'content msg) ""))
         (new-content (read-string (format "Edit message %s: " message-id) old-content))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id))
    (disco-api-edit-message-async
     channel-id
     message-id
     new-content
     :on-success
     (lambda (_response)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (disco-room-refresh)
           (message "disco: edited message %s" message-id))))
     :on-error
     (lambda (err)
       (message "disco: edit failed for %s: %s"
                message-id
                (disco-room--async-error-message err))))))

(defun disco-room-delete-message ()
  "Delete message at point in current room."
  (interactive)
  (let* ((message-id (disco-room--message-id-at-point)))
    (when (y-or-n-p (format "Delete message %s? " message-id))
      (let ((room-buffer (current-buffer))
            (channel-id disco-room--channel-id))
        (disco-api-delete-message-async
         channel-id
         message-id
         :on-success
         (lambda (_response)
           (when (disco-room--channel-buffer-p room-buffer channel-id)
             (with-current-buffer room-buffer
               (disco-room-refresh)
               (message "disco: deleted message %s" message-id))))
         :on-error
         (lambda (err)
           (message "disco: delete failed for %s: %s"
                    message-id
                    (disco-room--async-error-message err))))))))

(defun disco-room-create-thread-from-message (name message-id
                                                   &optional auto-archive-duration
                                                   rate-limit-per-user)
  "Create thread NAME from MESSAGE-ID in current channel.

AUTO-ARCHIVE-DURATION is optional minutes.
RATE-LIMIT-PER-USER is optional slowmode seconds."
  (interactive
   (let* ((name (read-string "Thread name: "))
          (default-message-id (disco-room--latest-message-id))
          (message-raw (read-string
                        (if default-message-id
                            (format "Message ID (default %s): " default-message-id)
                          "Message ID: ")))
          (message-id (if (string-empty-p message-raw)
                          (or default-message-id
                              (user-error "disco: no message id provided and no loaded messages"))
                        message-raw))
          (auto-archive-duration (disco-room--read-thread-auto-archive-duration))
          (rate-limit-per-user
           (disco-room--read-optional-nonnegative-int
            "Slowmode seconds (empty for none): ")))
     (list name message-id auto-archive-duration rate-limit-per-user)))
  (disco-room--ensure-parent-channel)
  (let* ((thread (disco-api-create-thread-from-message
                  disco-room--channel-id
                  message-id
                  name
                  auto-archive-duration
                  rate-limit-per-user))
         (thread-id (and (listp thread) (alist-get 'id thread)))
         (thread-name (or (and (listp thread) (alist-get 'name thread)) name)))
    (when thread-id
      (disco-state-upsert-channel thread)
      (disco-room-open thread-id thread-name))
    (message "disco: created thread %s" name)))

(defun disco-room-create-thread (name &optional type auto-archive-duration
                                      invitable rate-limit-per-user)
  "Create detached thread NAME in current channel.

TYPE is optional thread channel type.
AUTO-ARCHIVE-DURATION is optional minutes.
INVITABLE controls private-thread invites when TYPE is 12.
RATE-LIMIT-PER-USER is optional slowmode seconds."
  (interactive
   (let* ((name (read-string "Thread name: "))
          (type (unless (disco-room--forum-or-media-channel-p)
                  (disco-room--read-detached-thread-type)))
          (auto-archive-duration (disco-room--read-thread-auto-archive-duration))
          (invitable (when (equal type 12)
                       (y-or-n-p "Invitable by non-moderators? ")))
          (rate-limit-per-user
           (disco-room--read-optional-nonnegative-int
            "Slowmode seconds (empty for none): ")))
     (list name type auto-archive-duration invitable rate-limit-per-user)))
  (disco-room--ensure-parent-channel)
  (let* ((thread (disco-api-create-thread
                  disco-room--channel-id
                  name
                  type
                  auto-archive-duration
                  invitable
                  rate-limit-per-user))
         (thread-id (and (listp thread) (alist-get 'id thread)))
         (thread-name (or (and (listp thread) (alist-get 'name thread)) name)))
    (when thread-id
      (disco-state-upsert-channel thread)
      (disco-room-open thread-id thread-name))
    (message "disco: created detached thread %s" name)))

(defun disco-room-join-thread ()
  "Join current thread room as current user."
  (interactive)
  (disco-room--ensure-thread-channel)
  (disco-api-join-thread disco-room--channel-id)
  (message "disco: joined thread %s" disco-room--channel-name))

(defun disco-room-leave-thread ()
  "Leave current thread room as current user."
  (interactive)
  (disco-room--ensure-thread-channel)
  (disco-api-leave-thread disco-room--channel-id)
  (message "disco: left thread %s" disco-room--channel-name))

(defun disco-room-toggle-thread-archived ()
  "Toggle archived state for current thread."
  (interactive)
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-archived (not (disco-room--thread-archived-p channel)))
         (updated (disco-api-set-thread-archived disco-room--channel-id next-archived nil)))
    (if (and (listp updated) (alist-get 'id updated))
        (disco-state-upsert-channel updated)
      ;; Fallback when API returns empty body.
      (disco-state-upsert-channel
       (disco-room--thread-with-meta-field
        channel
        'archived
        (if next-archived t :false))))
    (disco-room-render)
    (message "disco: thread %s" (if next-archived "archived" "unarchived"))))

(defvar disco-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-room-refresh)
    (define-key map (kbd "M-<") #'disco-room-load-older-messages)
    (define-key map (kbd "RET") #'disco-room-return-dwim)
    (define-key map (kbd "C-c '") #'disco-room-edit-draft)
    (define-key map (kbd "M-p") #'disco-room-draft-prev)
    (define-key map (kbd "M-n") #'disco-room-draft-next)
    (define-key map (kbd "r") #'disco-room-reply-to-message)
    (define-key map (kbd "e") #'disco-room-edit-message)
    (define-key map (kbd "d") #'disco-room-delete-message)
    (define-key map (kbd "C-c C-c") #'disco-room-send-message)
    (define-key map (kbd "C-c C-k") #'disco-room-cancel-reply)
    (define-key map (kbd "C-c C-t m") #'disco-room-create-thread-from-message)
    (define-key map (kbd "C-c C-t c") #'disco-room-create-thread)
    (define-key map (kbd "C-c C-j") #'disco-room-join-thread)
    (define-key map (kbd "C-c C-l") #'disco-room-leave-thread)
    (define-key map (kbd "C-c C-a") #'disco-room-toggle-thread-archived)
    (define-key map (kbd "?") #'disco-room-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-room-mode'.")

(define-derived-mode disco-room-mode special-mode "Disco-Room"
  "Major mode for disco.el room buffers."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local disco-room--draft-input "")
  (setq-local disco-room--input-ring (make-ring (max 1 disco-room-input-history-size)))
  (setq-local disco-room--input-index nil)
  (setq-local disco-room--input-pending nil)
  (setq-local disco-room--send-in-flight nil))

(defun disco-room-open (channel-id channel-name)
  "Open room for CHANNEL-ID with CHANNEL-NAME."
  (let ((buf (get-buffer-create (disco-room--buffer-name channel-name channel-id))))
    (with-current-buffer buf
      (disco-room-mode)
      (setq disco-room--channel-id channel-id)
      (setq disco-room--channel-name channel-name)
      (let ((channel (disco-state-channel channel-id)))
        (setq disco-room--guild-id (and channel (alist-get 'guild_id channel))))
      (disco-room--attach-live-updates)
      (disco-room-refresh))
    (pop-to-buffer buf)))

(provide 'disco-room)

;;; disco-room.el ends here
