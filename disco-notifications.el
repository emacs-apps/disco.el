;;; disco-notifications.el --- Desktop notifications for disco.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Delayed, visibility-aware Discord desktop notifications.

;;; Code:

(require 'notifications)
(require 'ring)
(require 'seq)
(require 'subr-x)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-room)
(require 'disco-root-view)
(require 'disco-state)

(defvar disco-notifications--last-id nil)
(defvar disco-notifications--seen (make-hash-table :test #'equal))
(defvar disco-notifications--seen-order nil)
(defconst disco-notifications--seen-limit 512)
(defvar disco-notifications--history nil)

(defun disco-notifications--history-ring ()
  "Return notification history ring at its configured size."
  (unless (and (ring-p disco-notifications--history)
               (= (ring-size disco-notifications--history)
                  disco-notifications-history-ring-size))
    (setq disco-notifications--history
          (make-ring (max 1 disco-notifications-history-ring-size))))
  disco-notifications--history)

(defun disco-notifications--remember (message-id)
  "Remember MESSAGE-ID in bounded deduplication state."
  (puthash message-id t disco-notifications--seen)
  (push message-id disco-notifications--seen-order)
  (when (> (length disco-notifications--seen-order)
           disco-notifications--seen-limit)
    (let ((expired (car (last disco-notifications--seen-order))))
      (setq disco-notifications--seen-order
            (butlast disco-notifications--seen-order))
      (remhash expired disco-notifications--seen))))

(defun disco-notifications--message-time (message)
  "Return MESSAGE timestamp as float seconds, or nil."
  (when-let* ((timestamp (alist-get 'timestamp message)))
    (condition-case nil
        (float-time (date-to-time timestamp))
      (error nil))))

(defun disco-notifications--message-ping-p (message)
  "Return non-nil when MESSAGE actually pings the current user."
  (disco-state--message-mentions-user-p
   message (disco-gateway-current-user-id)))

(defun disco-notifications--room-visible-p (channel-id)
  "Return non-nil when CHANNEL-ID is visible on a focused frame."
  (seq-some
   (lambda (frame)
     (and (frame-live-p frame)
          (eq (frame-focus-state frame) t)
          (seq-some
           (lambda (window)
             (when-let* ((buffer (window-buffer window)))
               (with-current-buffer buffer
                 (and (derived-mode-p 'disco-room-mode)
                      (equal disco-room--channel-id channel-id)))))
           (window-list frame 'no-minibuffer))))
   (frame-list)))

(defun disco-notifications-message-notify-p (message)
  "Return non-nil when incoming MESSAGE should produce a notification."
  (let* ((channel-id (disco-msg-normalize-id (alist-get 'channel_id message)))
         (channel (and channel-id (disco-state-channel channel-id)))
         (author (alist-get 'author message))
         (author-id (and (listp author) (alist-get 'id author)))
         (ping-p (disco-notifications--message-ping-p message))
         (time (disco-notifications--message-time message)))
    (and channel
         (not (equal (disco-msg-normalize-id author-id)
                     (disco-msg-normalize-id (disco-gateway-current-user-id))))
         (or (null time)
             (<= (- (float-time) time) disco-notifications-max-message-age))
         (or ping-p
             (and (not (disco-state-channel-muted-p channel))
                  (= 0 (disco-state-channel-notification-level channel))))
         (not (disco-notifications--room-visible-p channel-id)))))

(defun disco-notifications--guild-name (channel)
  "Return guild name for CHANNEL, or nil."
  (when-let* ((guild-id (alist-get 'guild_id channel))
              (guild (seq-find (lambda (it) (equal (alist-get 'id it) guild-id))
                               (disco-state-guilds))))
    (alist-get 'name guild)))

(defun disco-notifications--title (message channel)
  "Return notification title for MESSAGE in CHANNEL."
  (let ((author (or (disco-msg-author-display-name message) "Discord"))
        (channel-name (disco-root--channel-display-name channel))
        (guild-name (disco-notifications--guild-name channel)))
    (if guild-name
        (format "%s — #%s · %s" author channel-name guild-name)
      (format "%s — %s" author channel-name))))

(defun disco-notifications--body (message)
  "Return compact notification body for MESSAGE."
  (if disco-notifications-show-preview
      (truncate-string-to-width
       (concat (if (disco-notifications--message-ping-p message) "@你  " "")
               (disco-msg-preview-content message))
       (max 1 disco-notifications-body-limit) nil nil "…")
    "New Discord message"))

(defun disco-notifications-open-message (channel-id message-id)
  "Open CHANNEL-ID and jump to MESSAGE-ID."
  (when (fboundp 'x-focus-frame)
    (ignore-errors (x-focus-frame (selected-frame))))
  (ignore-errors (raise-frame (selected-frame)))
  (let* ((channel (disco-state-channel channel-id))
         (name (if channel (disco-root--channel-display-name channel) channel-id)))
    (disco-room-open channel-id name)
    (when message-id
      (disco-room-jump-to-message message-id channel-id))))

(defun disco-notifications--close (id)
  "Close notification ID if it is current."
  (when (equal id disco-notifications--last-id)
    (setq disco-notifications--last-id nil)
    (ignore-errors (notifications-close-notification id))))

(defun disco-notifications--show (message)
  "Show desktop notification for MESSAGE after policy recheck."
  (when (and disco-notifications-mode
             (disco-notifications-message-notify-p message))
    (let* ((channel-id (disco-msg-normalize-id (alist-get 'channel_id message)))
           (message-id (disco-msg-normalize-id (alist-get 'id message)))
           (channel (disco-state-channel channel-id))
           (args (append
                  (list :app-name "disco.el"
                        :title (disco-notifications--title message channel)
                        :body (disco-notifications--body message)
                        :urgency (if (disco-notifications--message-ping-p message)
                                     "critical" "normal")
                        :timeout -1
                        :actions '("default" "Open")
                        :on-action (lambda (&rest _)
                                     (disco-notifications-open-message
                                      channel-id message-id)))
                  disco-notifications-extra-args)))
      (when disco-notifications--last-id
        (ignore-errors (notifications-close-notification
                        disco-notifications--last-id)))
      (ring-insert (disco-notifications--history-ring) (copy-tree message))
      (condition-case err
          (setq disco-notifications--last-id (apply #'notifications-notify args))
        (error (message "disco: desktop notification failed: %s"
                        (error-message-string err))))
      (when (and disco-notifications-timeout disco-notifications--last-id)
        (run-with-timer disco-notifications-timeout nil
                        #'disco-notifications--close
                        disco-notifications--last-id)))))

(defun disco-notifications--handle-event (event)
  "Schedule a notification for one Gateway EVENT."
  (when (eq (plist-get event :type) 'ready)
    (clrhash disco-notifications--seen)
    (setq disco-notifications--seen-order nil))
  (when (eq (plist-get event :type) 'message-create)
    (when-let* ((message (plist-get event :message))
                (message-id (disco-msg-normalize-id (alist-get 'id message))))
      (unless (gethash message-id disco-notifications--seen)
        (disco-notifications--remember message-id)
        (if (> disco-notifications-delay 0)
            (run-with-timer disco-notifications-delay nil
                            #'disco-notifications--show (copy-tree message))
          (disco-notifications--show message))))))

;;;###autoload
(define-minor-mode disco-notifications-mode
  "Toggle visibility-aware Discord desktop notifications."
  :global t :group 'disco-notifications
  (if disco-notifications-mode
      (add-hook 'disco-gateway-event-hook #'disco-notifications--handle-event)
    (remove-hook 'disco-gateway-event-hook #'disco-notifications--handle-event)))

;;;###autoload
(defun disco-notifications-history ()
  "Show recent disco desktop notifications."
  (interactive)
  (let ((buffer (get-buffer-create "*Disco Notifications*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (dolist (message (ring-elements (disco-notifications--history-ring)))
          (let* ((channel-id (alist-get 'channel_id message))
                 (message-id (alist-get 'id message))
                 (channel (disco-state-channel channel-id)))
            (insert-text-button
             (disco-notifications--title message channel)
             'follow-link t
             'action (lambda (_button)
                       (disco-notifications-open-message channel-id message-id)))
            (insert (format "  %s\n" (disco-notifications--body message)))))))
    (pop-to-buffer buffer)))

(provide 'disco-notifications)

;;; disco-notifications.el ends here
