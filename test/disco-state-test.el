;;; disco-state-test.el --- Tests for disco-state read-state helpers -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-state)

(ert-deftest disco-state-apply-message-ack-retains-unread-when-mention-omitted ()
  (disco-state-reset)
  (disco-state-set-channel-unread "chan" 5)
  (disco-state-apply-message-ack "chan" "9")
  (should (equal "9" (disco-state-channel-last-read-message-id "chan")))
  (should (= 5 (disco-state-channel-unread-count "chan"))))

(ert-deftest disco-state-apply-message-ack-updates-unread-when-mention-present ()
  (disco-state-reset)
  (disco-state-set-channel-unread "chan" 5)
  (disco-state-apply-message-ack "chan" "10" 0)
  (should (equal "10" (disco-state-channel-last-read-message-id "chan")))
  (should (= 0 (disco-state-channel-unread-count "chan")))
  (disco-state-apply-message-ack "chan" nil 3)
  (should (= 3 (disco-state-channel-unread-count "chan"))))

(ert-deftest disco-state-apply-ready-read-state-entry-channel-default-type ()
  (disco-state-reset)
  (should (disco-state-apply-ready-read-state-entry
           '((id . "chan")
             (last_message_id . "7")
             (mention_count . 2))))
  (should (equal "7" (disco-state-channel-last-read-message-id "chan")))
  (should (= 2 (disco-state-channel-unread-count "chan"))))

(ert-deftest disco-state-apply-ready-read-state-entry-non-channel-ignored ()
  (disco-state-reset)
  (disco-state-set-channel-unread "chan" 4)
  (should-not
   (disco-state-apply-ready-read-state-entry
    '((read_state_type . 2)
      (id . "chan")
      (last_message_id . "11")
      (mention_count . 0))))
  (should (= 4 (disco-state-channel-unread-count "chan")))
  (should (null (disco-state-channel-last-read-message-id "chan"))))

(provide 'disco-state-test)

;;; disco-state-test.el ends here
