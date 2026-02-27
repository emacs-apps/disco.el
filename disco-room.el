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

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--gateway-handler nil)

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
    (insert (format "Channel: %s\n\n" disco-room--channel-name))
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

(defvar disco-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-room-refresh)
    (define-key map (kbd "C-c C-c") #'disco-room-send-message)
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
