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

(ert-deftest disco-state-apply-message-create-increments-unread-private-unmuted ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "dm") (type . 1) (muted . :false)))
  (disco-state-set-channel-unread "dm" 2)
  (disco-state-apply-message-create
   "dm"
   '((id . "100")
     (type . 0)
     (author . ((id . "u2")))
     (mentions . [])
     (mention_roles . [])
     (mention_everyone . :false))
   "u1"
   nil)
  (should (= 3 (disco-state-channel-unread-count "dm")))
  (should (null (disco-state-channel-last-read-message-id "dm"))))

(ert-deftest disco-state-apply-message-create-increments-unread-guild-mention ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "guild") (guild_id . "g") (type . 0)))
  (disco-state-apply-message-create
   "guild"
   '((id . "101")
     (type . 0)
     (author . ((id . "u2")))
     (mentions . [((id . "u1"))])
     (mention_roles . [])
     (mention_everyone . :false)
     (member . ((roles . []))))
   "u1"
   nil)
  (should (= 1 (disco-state-channel-unread-count "guild"))))

(ert-deftest disco-state-apply-message-create-private-muted-requires-mention ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "dm") (type . 1) (muted . t)))
  (disco-state-apply-message-create
   "dm"
   '((id . "201")
     (type . 0)
     (author . ((id . "u2")))
     (mentions . [])
     (mention_roles . [])
     (mention_everyone . :false))
   "u1"
   nil)
  (should (= 0 (disco-state-channel-unread-count "dm")))

  (disco-state-apply-message-create
   "dm"
   '((id . "202")
     (type . 0)
     (author . ((id . "u2")))
     (mentions . [((id . "u1"))])
     (mention_roles . [])
     (mention_everyone . :false))
   "u1"
   nil)
  (should (= 1 (disco-state-channel-unread-count "dm"))))

(ert-deftest disco-state-apply-message-create-acks-own-message-except-poll-result ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "dm") (type . 1) (muted . :false)))
  (disco-state-set-channel-unread "dm" 5)
  (disco-state-apply-message-create
   "dm"
   '((id . "102")
     (type . 0)
     (author . ((id . "u1")))
     (mentions . [])
     (mention_roles . [])
     (mention_everyone . :false))
   "u1"
   nil)
  (should (= 0 (disco-state-channel-unread-count "dm")))
  (should (equal "102" (disco-state-channel-last-read-message-id "dm")))

  (disco-state-set-channel-unread "dm" 4)
  (disco-state-apply-message-create
   "dm"
   '((id . "103")
     (type . 46)
     (author . ((id . "u1")))
     (mentions . [])
     (mention_roles . [])
     (mention_everyone . :false))
   "u1"
   nil)
  (should (= 4 (disco-state-channel-unread-count "dm")))
  (should (equal "102" (disco-state-channel-last-read-message-id "dm"))))

(ert-deftest disco-state-apply-message-create-ignores-watched-channels ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "chan") (type . 1) (muted . :false)))
  (disco-state-set-channel-unread "chan" 4)
  (disco-state-apply-message-create
   "chan"
   '((id . "104")
     (type . 0)
     (author . ((id . "u2")))
     (mentions . [])
     (mention_roles . [])
     (mention_everyone . :false))
   "u1"
   t)
  (should (= 4 (disco-state-channel-unread-count "chan")))
  (should (null (disco-state-channel-last-read-message-id "chan"))))

(ert-deftest disco-state-channel-effective-unread-count-includes-child-threads ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "parent") (guild_id . "g") (type . 0)))
  (disco-state-upsert-channel
   '((id . "t1") (guild_id . "g") (parent_id . "parent") (type . 11)))
  (disco-state-upsert-channel
   '((id . "t2") (guild_id . "g") (parent_id . "parent") (type . 11)))
  (disco-state-set-channel-unread "parent" 2)
  (disco-state-set-channel-unread "t1" 4)
  (disco-state-set-channel-unread "t2" 1)
  (should (= 5 (disco-state-parent-thread-unread-total "parent")))
  (should (= 7
             (disco-state-channel-effective-unread-count
              (disco-state-channel "parent"))))
  (should (= 4
             (disco-state-channel-effective-unread-count
              (disco-state-channel "t1")))))

(ert-deftest disco-state-channels-unread-total-sums-own-unread ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "a") (type . 1)))
  (disco-state-upsert-channel '((id . "b") (type . 1)))
  (disco-state-set-channel-unread "a" 3)
  (disco-state-set-channel-unread "b" 6)
  (should (= 9
             (disco-state-channels-unread-total
              (list (disco-state-channel "a")
                    (disco-state-channel "b"))))))

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

(ert-deftest disco-state-channel-ack-request-fields-include-token ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "thread") (guild_id . "g") (type . 11)))
  (disco-state-set-channel-ack-token "thread" "tok")
  (cl-letf (((symbol-function 'float-time)
             (lambda () (+ disco-state-discord-epoch-seconds
                           (* 2 86400)
                           1.0))))
    (should (equal '(:token "tok" :flags 3 :last-viewed 2)
                   (disco-state-channel-ack-request-fields "thread")))))

(ert-deftest disco-state-apply-channel-ack-response-updates-token ()
  (disco-state-reset)
  (disco-state-set-channel-ack-token "chan" "old")
  (disco-state-apply-channel-ack-response "chan" '((token . "new")))
  (should (equal "new" (disco-state-channel-ack-token "chan")))
  (disco-state-apply-channel-ack-response "chan" '((token)))
  (should (null (disco-state-channel-ack-token "chan")))
  (disco-state-set-channel-ack-token "chan" "again")
  (disco-state-apply-channel-ack-response "chan" '((ok . t)))
  (should (equal "again" (disco-state-channel-ack-token "chan"))))

(ert-deftest disco-state-apply-user-update-resets-ack-tokens ()
  (disco-state-reset)
  (disco-state-set-channel-ack-token "a" "tok-a")
  (disco-state-set-channel-ack-token "b" "tok-b")
  (disco-state-apply-user-update)
  (should (null (disco-state-channel-ack-token "a")))
  (should (null (disco-state-channel-ack-token "b"))))

(provide 'disco-state-test)

;;; disco-state-test.el ends here
