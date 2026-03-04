;;; disco-state-test.el --- Tests for disco-state read-state helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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

(ert-deftest disco-state-apply-message-create-increments-unread-for-others ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "chan") (type . 0)))
  (disco-state-set-channel-unread "chan" 2)
  (disco-state-apply-message-create "chan" "100" nil nil)
  (should (= 3 (disco-state-channel-unread-count "chan")))
  (should (null (disco-state-channel-last-read-message-id "chan"))))

(ert-deftest disco-state-apply-message-create-acks-own-message ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "chan") (type . 0)))
  (disco-state-set-channel-unread "chan" 5)
  (disco-state-apply-message-create "chan" "101" nil t)
  (should (= 0 (disco-state-channel-unread-count "chan")))
  (should (equal "101" (disco-state-channel-last-read-message-id "chan"))))

(ert-deftest disco-state-apply-message-create-ignores-watched-channels ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "chan") (type . 0)))
  (disco-state-set-channel-unread "chan" 4)
  (disco-state-apply-message-create "chan" "102" t nil)
  (should (= 4 (disco-state-channel-unread-count "chan")))
  (should (null (disco-state-channel-last-read-message-id "chan"))))

(ert-deftest disco-state-channel-read-state-flags-guild-and-thread ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "guild-text") (guild_id . "g") (type . 0)))
  (disco-state-upsert-channel '((id . "thread") (guild_id . "g") (type . 11)))
  (should (= 1 (disco-state-channel-read-state-flags "guild-text")))
  (should (= 3 (disco-state-channel-read-state-flags "thread"))))

(ert-deftest disco-state-current-last-viewed-day-uses-discord-epoch ()
  (cl-letf (((symbol-function 'float-time)
             (lambda () (+ disco-state-discord-epoch-seconds
                           (* 5 86400)
                           123.0))))
    (should (= 5 (disco-state-current-last-viewed-day)))))

(ert-deftest disco-state-channel-ack-fields-returns-flags-and-last-viewed ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "thread") (guild_id . "g") (type . 11)))
  (cl-letf (((symbol-function 'float-time)
             (lambda () (+ disco-state-discord-epoch-seconds
                           (* 3 86400)
                           1.0))))
    (should (equal '(:flags 3 :last-viewed 3)
                   (disco-state-channel-ack-fields "thread")))))

(provide 'disco-state-test)

;;; disco-state-test.el ends here
