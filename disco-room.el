;;; disco-room.el --- Channel room buffers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Per-channel room buffer with simple timeline rendering and message sending.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-transient)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--gateway-handler nil)

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
        (content (or (alist-get 'content msg) "")))
    (insert (format "[%s] %s: %s\n" timestamp author content))))

(defun disco-room-render ()
  "Render timeline for current room buffer."
  (let ((inhibit-read-only t)
        (messages (disco-state-messages disco-room--channel-id)))
    (erase-buffer)
    (insert (format "Channel: %s%s\n\n"
                    disco-room--channel-name
                    (disco-room--thread-header-suffix)))
    ;; API returns newest-first by default; reverse for chat-like display.
    (dolist (msg (reverse messages))
      (disco-room--insert-message msg))
    (goto-char (point-max))))

(defun disco-room-refresh ()
  "Fetch and redraw latest messages for current room."
  (interactive)
  (let ((messages (disco-api-channel-messages disco-room--channel-id nil nil)))
    (disco-state-put-messages disco-room--channel-id messages)
    (disco-room-render)
    (message "disco: loaded %d messages" (length messages))))

(defun disco-room--handle-gateway-event (event)
  "Handle one EVENT plist from `disco-gateway-event-hook'."
  (when (and (equal (plist-get event :channel-id) disco-room--channel-id)
             (memq (plist-get event :type)
                   '(message-create message-update message-delete)))
    (let ((at-bottom (= (point) (point-max))))
      (disco-room-render)
      (when at-bottom
        (goto-char (point-max))))))

(defun disco-room--attach-live-updates ()
  "Attach this room buffer to live update event stream."
  (when disco-room--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-room--gateway-handler))
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
  "Prompt and send a message to current room."
  (interactive)
  (let ((content (read-string "Message: ")))
    (unless (string-empty-p content)
      (disco-api-send-message disco-room--channel-id content)
      (disco-room-refresh)
      (message "disco: message sent"))))

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
    (define-key map (kbd "C-c C-c") #'disco-room-send-message)
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
  (setq truncate-lines t))

(defun disco-room-open (channel-id channel-name)
  "Open room for CHANNEL-ID with CHANNEL-NAME."
  (let ((buf (get-buffer-create (disco-room--buffer-name channel-name channel-id))))
    (with-current-buffer buf
      (disco-room-mode)
      (setq disco-room--channel-id channel-id)
      (setq disco-room--channel-name channel-name)
      (disco-room--attach-live-updates)
      (disco-room-refresh))
    (pop-to-buffer buf)))

(provide 'disco-room)

;;; disco-room.el ends here
