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

(ert-deftest disco-state-apply-message-ack-updates-flags-last-viewed-and-version ()
  (disco-state-reset)
  (disco-state-apply-message-ack "chan" "12" 1 3 42 9)
  (let ((state (disco-state-read-state 0 "chan")))
    (should (equal "12" (alist-get 'last_message_id state)))
    (should (= 1 (alist-get 'mention_count state)))
    (should (= 3 (alist-get 'flags state)))
    (should (= 42 (alist-get 'last_viewed state)))
    (should (= 9 (alist-get 'version state)))))

(ert-deftest disco-state-apply-feature-ack-updates-generic-read-state ()
  (disco-state-reset)
  (should
   (disco-state-apply-feature-ack 1 "guild1" "entity7" 5))
  (let ((state (disco-state-read-state 1 "guild1")))
    (should (= 1 (alist-get 'read_state_type state)))
    (should (equal "guild1" (alist-get 'id state)))
    (should (equal "entity7" (alist-get 'last_acked_id state)))
    (should (= 0 (alist-get 'badge_count state)))
    (should (= 5 (alist-get 'version state)))))

(ert-deftest disco-state-apply-ready-read-state-entry-channel-default-type ()
  (disco-state-reset)
  (should (disco-state-apply-ready-read-state-entry
           '((id . "chan")
             (last_message_id . "7")
             (mention_count . 2))))
  (should (equal "7" (disco-state-channel-last-read-message-id "chan")))
  (should (= 2 (disco-state-channel-unread-count "chan"))))

(ert-deftest disco-state-apply-ready-read-state-entry-non-channel-stored ()
  (disco-state-reset)
  (should
   (disco-state-apply-ready-read-state-entry
    '((read_state_type . 2)
      (id . "u1")
      (last_acked_id . "11")
      (badge_count . 3)
      (version . 9))))
  (let ((state (disco-state-read-state 2 "u1")))
    (should (= 2 (alist-get 'read_state_type state)))
    (should (equal "u1" (alist-get 'id state)))
    (should (equal "11" (alist-get 'last_acked_id state)))
    (should (= 3 (alist-get 'badge_count state)))
    (should (= 9 (alist-get 'version state)))))

(ert-deftest disco-state-apply-ready-read-state-entry-sets-last-read-pin-timestamp ()
  (disco-state-reset)
  (disco-state-upsert-channel
   '((id . "chan")
     (type . 0)
     (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
  (should (disco-state-apply-ready-read-state-entry
           '((id . "chan")
             (last_message_id . "7")
             (mention_count . 0)
             (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00"))))
  (should (equal "2026-03-04T01:00:00.000000+00:00"
                 (disco-state-channel-last-read-pin-timestamp "chan")))
  (should-not (disco-state-channel-has-unread-pins-p
               (disco-state-channel "chan"))))

(ert-deftest disco-state-channel-pins-update-and-ack-drive-unread ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "chan") (type . 0)))
  (disco-state-apply-channel-pins-update
   "chan"
   "2026-03-04T01:00:00.000000+00:00")
  (should (disco-state-channel-has-unread-pins-p
           (disco-state-channel "chan")))
  (disco-state-apply-channel-pins-ack
   "chan"
   "2026-03-04T01:00:00.000000+00:00"
   2)
  (should-not (disco-state-channel-has-unread-pins-p
               (disco-state-channel "chan")))
  (should (= 2 (alist-get 'version
                          (disco-state-read-state 0 "chan"))))
  (disco-state-apply-channel-pins-update
   "chan"
   "2026-03-05T01:00:00.000000+00:00")
  (should (disco-state-channel-has-unread-pins-p
           (disco-state-channel "chan"))))

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

(ert-deftest disco-state-apply-thread-create-acks-own-thread-in-thread-only-parent ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "forum") (guild_id . "g") (type . 15)))
  (disco-state-set-channel-unread "forum" 3)
  (disco-state-apply-thread-create
   '((id . "th1")
     (parent_id . "forum")
     (owner_id . "u1")
     (type . 11))
   "u1")
  (should (= 0 (disco-state-channel-unread-count "forum")))
  (should (equal "th1" (disco-state-channel-last-read-message-id "forum"))))

(ert-deftest disco-state-apply-thread-create-ignores-non-own-or-non-thread-only-parent ()
  (disco-state-reset)
  (disco-state-upsert-channel '((id . "forum") (guild_id . "g") (type . 15)))
  (disco-state-upsert-channel '((id . "text") (guild_id . "g") (type . 0)))
  (disco-state-set-channel-unread "forum" 4)
  (disco-state-set-channel-unread "text" 5)
  (disco-state-apply-thread-create
   '((id . "th2")
     (parent_id . "forum")
     (owner_id . "u2")
     (type . 11))
   "u1")
  (disco-state-apply-thread-create
   '((id . "th3")
     (parent_id . "text")
     (owner_id . "u1")
     (type . 11))
   "u1")
  (should (= 4 (disco-state-channel-unread-count "forum")))
  (should (null (disco-state-channel-last-read-message-id "forum")))
  (should (= 5 (disco-state-channel-unread-count "text")))
  (should (null (disco-state-channel-last-read-message-id "text"))))

(ert-deftest disco-state-apply-channel-unread-updates-updates-channel-fields ()
  (disco-state-reset)
  (disco-state-upsert-channel
   '((id . "c1")
     (guild_id . "g")
     (type . 0)
     (last_message_id . "10")
     (last_pin_timestamp . "old")))
  (should (= 1
             (disco-state-apply-channel-unread-updates
              '(((id . "c1")
                 (last_message_id . "11")
                 (last_pin_timestamp . "new"))))))
  (let ((channel (disco-state-channel "c1")))
    (should (equal "11" (alist-get 'last_message_id channel)))
    (should (equal "new" (alist-get 'last_pin_timestamp channel)))))

(ert-deftest disco-state-channel-has-unread-p-uses-read-cursor-and-threads ()
  (disco-state-reset)
  (disco-state-upsert-channel
   '((id . "parent")
     (guild_id . "g")
     (type . 0)
     (last_message_id . "100")))
  (disco-state-upsert-channel
   '((id . "thread")
     (guild_id . "g")
     (parent_id . "parent")
     (type . 11)
     (last_message_id . "101")))
  (should (disco-state-channel-has-unread-p (disco-state-channel "parent")))
  (disco-state-apply-message-ack "parent" "100" 0)
  (should (disco-state-channel-has-unread-p (disco-state-channel "parent")))
  (disco-state-apply-message-ack "thread" "101" 0)
  (should-not (disco-state-channel-has-unread-p (disco-state-channel "parent"))))

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

(ert-deftest disco-state-read-state-counter-total-sums-by-type ()
  (disco-state-reset)
  (disco-state-apply-ready-read-state-entry
   '((read_state_type . 2)
     (id . "u1")
     (badge_count . 3)
     (last_acked_id . "n3")))
  (disco-state-apply-ready-read-state-entry
   '((read_state_type . 2)
     (id . "u2")
     (badge_count . 4)
     (last_acked_id . "n8")))
  (disco-state-apply-ready-read-state-entry
   '((read_state_type . 5)
     (id . "u1")
     (badge_count . 6)
     (last_acked_id . "m2")))
  (should (= 7 (disco-state-read-state-counter-total 2)))
  (should (= 6 (disco-state-read-state-counter-total 5))))

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
