;;; disco-room-test-support.el --- Shared room test fixtures -*- lexical-binding: t; -*-

(require 'disco-room)
(require 'disco-state)

(defun disco-room-test-establish-latest-window (&optional channel-id)
  "Establish a known latest window for CHANNEL-ID's canonical fixture rows."
  (let* ((channel-id (or channel-id disco-room--channel-id))
         (messages
          (disco-room--normalize-history-page
           (disco-state-messages channel-id)))
         (newest (disco-room--message-id (car messages)))
         (oldest (disco-room--message-id (car (last messages)))))
    (setq disco-room--remote-latest-message-id newest)
    (if newest
        (appkit-chat-history-window-set oldest nil)
      (appkit-chat-history-window-establish-empty))))

(defun disco-room-test-setup-channel (&optional channel-id)
  "Reset state and bind the current room buffer to CHANNEL-ID."
  (let ((channel-id (or channel-id "chan")))
    (disco-state-reset)
    (setq-local disco-room--channel-id channel-id)
    (setq-local disco-room--channel-name channel-id)
    (disco-state-upsert-channel
     `((id . ,channel-id)
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    channel-id))

(provide 'disco-room-test-support)

;;; disco-room-test-support.el ends here
