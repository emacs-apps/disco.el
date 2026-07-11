;;; disco-notifications-test.el --- Tests for desktop notifications -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco-notifications)

(defun disco-notifications-test--message (&optional mention)
  "Return incoming test message, with direct MENTION when non-nil."
  `((id . "100") (channel_id . "c") (content . "hello")
    (author (id . "other") (username . "Alice"))
    (mentions . ,(if mention '(((id . "me"))) nil))
    (mention_everyone . nil) (mention_roles . nil)))

(ert-deftest disco-notifications-policy-honors-level-mute-and-ping ()
  (let ((channel '((id . "c") (type . 0)))
        (level 0)
        muted)
    (cl-letf (((symbol-function 'disco-state-channel)
               (lambda (_id) channel))
              ((symbol-function 'disco-gateway-current-user-id)
               (lambda () "me"))
              ((symbol-function 'disco-state-channel-muted-p)
               (lambda (_channel) muted))
              ((symbol-function 'disco-state-channel-notification-level)
               (lambda (_channel) level))
              ((symbol-function 'disco-notifications--room-visible-p)
               (lambda (_id) nil)))
      (should (disco-notifications-message-notify-p
               (disco-notifications-test--message)))
      (setq level 1)
      (should-not (disco-notifications-message-notify-p
                   (disco-notifications-test--message)))
      (setq muted t)
      (should (disco-notifications-message-notify-p
               (disco-notifications-test--message t))))))

(ert-deftest disco-notifications-policy-suppresses-self-and-visible-room ()
  (let ((message (disco-notifications-test--message)))
    (cl-letf (((symbol-function 'disco-state-channel)
               (lambda (_id) '((id . "c") (type . 1))))
              ((symbol-function 'disco-state-channel-muted-p) (lambda (_) nil))
              ((symbol-function 'disco-state-channel-notification-level) (lambda (_) 0))
              ((symbol-function 'disco-gateway-current-user-id) (lambda () "other"))
              ((symbol-function 'disco-notifications--room-visible-p) (lambda (_) nil)))
      (should-not (disco-notifications-message-notify-p message)))
    (cl-letf (((symbol-function 'disco-state-channel)
               (lambda (_id) '((id . "c") (type . 1))))
              ((symbol-function 'disco-state-channel-muted-p) (lambda (_) nil))
              ((symbol-function 'disco-state-channel-notification-level) (lambda (_) 0))
              ((symbol-function 'disco-gateway-current-user-id) (lambda () "me"))
              ((symbol-function 'disco-notifications--room-visible-p) (lambda (_) t)))
      (should-not (disco-notifications-message-notify-p message)))))

(ert-deftest disco-notifications-event-hook-dedupes-live-creates ()
  (let ((disco-notifications--seen (make-hash-table :test #'equal))
        (disco-notifications--seen-order nil)
        (disco-notifications-delay 0.5)
        scheduled)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (_delay _repeat function message)
                 (push (list function message) scheduled))))
      (let ((event (list :type 'message-create
                         :message (disco-notifications-test--message t))))
        (disco-notifications--handle-event event)
        (disco-notifications--handle-event event))
      (should (= 1 (length scheduled)))
      (disco-notifications--handle-event '(:type ready))
      (should (= 0 (hash-table-count disco-notifications--seen))))))

(provide 'disco-notifications-test)

;;; disco-notifications-test.el ends here
