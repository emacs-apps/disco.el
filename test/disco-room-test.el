;;; disco-room-test.el --- Tests for disco-room pin ack flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-room)
(require 'disco-state)

(ert-deftest disco-room-ack-channel-pins-applies-state-on-success ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
    (let ((disco-room--channel-id "chan")
          (disco-room--refresh-generation 1)
          called-channel-id)
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (channel-id &rest args)
                   (setq called-channel-id channel-id)
                   (funcall (plist-get args :on-success) nil)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-ack-channel-pins)
        (should (equal "chan" called-channel-id))
        (should (equal "2026-03-04T01:00:00.000000+00:00"
                       (disco-state-channel-last-read-pin-timestamp "chan")))))))

(ert-deftest disco-room-ack-channel-pins-skips-when-already-acked ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
    (disco-state-apply-channel-pins-ack
     "chan"
     "2026-03-04T01:00:00.000000+00:00")
    (let ((disco-room--channel-id "chan")
          (api-called nil))
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (&rest _args)
                   (setq api-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-ack-channel-pins)
        (should-not api-called)))))

(provide 'disco-room-test)

;;; disco-room-test.el ends here
