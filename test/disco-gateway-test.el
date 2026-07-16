;;; disco-gateway-test.el --- Tests for disco-gateway read-state flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-gateway)

(ert-deftest disco-gateway-identify-payload-includes-native-unread-capabilities-by-default ()
  (let ((disco-gateway-identify-capabilities nil)
        (disco-gateway-enable-passive-guild-update-v2 t)
        (disco-gateway-identify-intents nil)
        (disco-gateway-identify-presence nil))
    (cl-letf (((symbol-function 'disco-current-token)
               (lambda () "tok")))
      (let* ((payload (disco-gateway--identify-payload))
             (capabilities (alist-get 'capabilities payload)))
        (should (= capabilities (logior (ash 1 14) (ash 1 15))))))))

(ert-deftest disco-gateway-identify-payload-merges-custom-and-native-capabilities ()
  (let ((disco-gateway-identify-capabilities (ash 1 2))
        (disco-gateway-enable-passive-guild-update-v2 t)
        (disco-gateway-identify-intents nil)
        (disco-gateway-identify-presence nil))
    (cl-letf (((symbol-function 'disco-current-token)
               (lambda () "tok")))
      (let* ((payload (disco-gateway--identify-payload))
             (capabilities (alist-get 'capabilities payload)))
        (should (= capabilities
                   (logior (ash 1 2) (ash 1 14) (ash 1 15))))))))

(ert-deftest disco-gateway-identify-payload-keeps-channel-obfuscation-when-passive-v2-disabled ()
  (let ((disco-gateway-identify-capabilities nil)
        (disco-gateway-enable-passive-guild-update-v2 nil)
        (disco-gateway-identify-intents nil)
        (disco-gateway-identify-presence nil))
    (cl-letf (((symbol-function 'disco-current-token)
               (lambda () "tok")))
      (let ((payload (disco-gateway--identify-payload)))
        (should (= (alist-get 'capabilities payload) (ash 1 15)))))))

(ert-deftest disco-gateway-ready-advances-logical-session-generation ()
  (disco-state-reset)
  (let ((disco-gateway--session-generation 7))
    (cl-letf (((symbol-function 'disco-gateway--subscribe-watched-guild-channels)
               #'ignore)
              ((symbol-function 'disco-gateway--reset-reconnect-backoff)
               #'ignore)
              ((symbol-function 'message) #'ignore))
      (disco-gateway--dispatch-ready
       '((session_id . "session")
         (user . ((id . "self")))
         (guilds . [])))
      (should (= 8 (disco-gateway-session-generation)))
      (disco-gateway--dispatch-resumed nil)
      (should (= 8 (disco-gateway-session-generation)))))
  (disco-state-reset))

(ert-deftest disco-gateway-dispatch-message-create-passes-current-user-id ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) nil))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message current-user-id watched)
                 (setq captured (list channel-id message current-user-id watched))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m1")
         (author . ((id . "u1")))))
      (should (equal '("c"
                       ((channel_id . "c")
                        (id . "m1")
                        (author (id . "u1")))
                       "u1"
                       nil)
                     captured)))))

(ert-deftest disco-gateway-dispatch-message-create-passes-watched-flag ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) t))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message current-user-id watched)
                 (setq captured (list channel-id message current-user-id watched))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m3")
         (author . ((id . "u2")))))
      (should (equal '("c"
                       ((channel_id . "c")
                        (id . "m3")
                        (author (id . "u2")))
                       "u1"
                       t)
                     captured)))))

(ert-deftest disco-gateway-dispatch-message-create-upserts-when-watched ()
  (let (upsert-called)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) t))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (_channel-id _message _current-user-id _watched) nil))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (channel-id payload)
                 (setq upsert-called (list channel-id payload)))))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m4")
         (author . ((id . "u2")))))
      (should (equal '("c"
                       ((channel_id . "c")
                        (id . "m4")
                        (author (id . "u2"))))
                     upsert-called)))))

(ert-deftest disco-gateway-dispatch-message-ack-applies-full-read-state-fields ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-message-ack)
               (lambda (channel-id message-id mention-count flags last-viewed version)
                 (setq captured (list channel-id
                                      message-id
                                      mention-count
                                      flags
                                      last-viewed
                                      version))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-message-ack
       '((channel_id . "c2")
         (message_id . "m9")
         (mention_count . 3)
         (flags . 1)
         (last_viewed . 77)
         (version . 4)))
      (should (equal '("c2" "m9" 3 1 77 4) captured))
      (should (equal '(:type message-ack
                       :channel-id "c2"
                       :message-id "m9"
                       :mention-count 3
                       :flags 1
                       :last-viewed 77
                       :version 4)
                     emitted)))))

(ert-deftest disco-gateway-dispatch-channel-unread-update-applies-state-and-emits ()
  (let (captured-updates emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-unread-updates)
               (lambda (updates)
                 (setq captured-updates updates)
                 1))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-channel-unread-update
       '((guild_id . "g")
         (channel_unread_updates
          . (((id . "c1")
              (last_message_id . "10"))))))
      (should (equal '(((id . "c1")
                        (last_message_id . "10")))
                     captured-updates))
      (should (equal '(:type channel-unread-update
                       :guild-id "g"
                       :channel-unread-updates
                       (((id . "c1")
                         (last_message_id . "10"))))
                     emitted)))))

(ert-deftest disco-gateway-dispatch-channel-update-partial-applies-state-and-emits ()
  (let (captured-payload emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-unread)
               (lambda (payload)
                 (setq captured-payload payload)
                 t))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-channel-update-partial
       '((id . "c0")
         (last_message_id . "8")))
      (should (equal '((id . "c0")
                       (last_message_id . "8"))
                     captured-payload))
      (should (equal '(:type channel-update-partial
                       :channel-id "c0"
                       :channel-unread
                       ((id . "c0")
                        (last_message_id . "8")))
                     emitted)))))

(ert-deftest disco-gateway-dispatch-channel-pins-update-applies-state-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-pins-update)
               (lambda (channel-id timestamp)
                 (setq captured (list channel-id timestamp))
                 t))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-channel-pins-update
       '((guild_id . "g")
         (channel_id . "c0")
         (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
      (should (equal '("c0" "2026-03-04T01:00:00.000000+00:00")
                     captured))
      (should (equal '(:type channel-pins-update
                       :guild-id "g"
                       :channel-id "c0"
                       :last-pin-timestamp "2026-03-04T01:00:00.000000+00:00")
                     emitted)))))

(ert-deftest disco-gateway-dispatch-channel-pins-ack-applies-state-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-pins-ack)
               (lambda (channel-id timestamp version)
                 (setq captured (list channel-id timestamp version))
                 t))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-channel-pins-ack
       '((channel_id . "c0")
         (timestamp . "2026-03-04T01:00:00.000000+00:00")
         (version . 2)))
      (should (equal '("c0" "2026-03-04T01:00:00.000000+00:00" 2)
                     captured))
      (should (equal '(:type channel-pins-ack
                       :channel-id "c0"
                       :last-pin-timestamp "2026-03-04T01:00:00.000000+00:00"
                       :version 2)
                     emitted)))))

(ert-deftest disco-gateway-delayed-channel-pins-ack-cannot-regress-state ()
  (disco-state-reset)
  (let (emitted)
    (cl-letf (((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (push event emitted))))
      (disco-gateway--dispatch-channel-pins-ack
       '((channel_id . "c0")
         (timestamp . "2026-03-04T02:00:00.000000+00:00")
         (version . 8)))
      ;; A delayed ACK event is still observable, but its state mutation is
      ;; rejected by the monotonic read-state merge.
      (disco-gateway--dispatch-channel-pins-ack
       '((channel_id . "c0")
         (timestamp . "2026-03-04T01:00:00.000000+00:00")
         (version . 7))))
    (let ((state (disco-state-read-state 0 "c0")))
      (should (equal "2026-03-04T02:00:00.000000+00:00"
                     (alist-get 'last_pin_timestamp state)))
      (should (= 8 (alist-get 'version state))))
    (should (= 2 (length emitted)))))

(ert-deftest disco-gateway-reaction-event-freezes-self-identity ()
  (let ((disco-gateway--current-user-id "self")
        emitted)
    (cl-letf (((symbol-function 'disco-gateway--emit)
               (lambda (event) (setq emitted event))))
      (disco-gateway--dispatch-message-reaction-add
       '((channel_id . "c0")
         (message_id . "m1")
         (user_id . "self")
         (emoji . ((name . "wave"))))))
    (setq disco-gateway--current-user-id nil)
    (should (plist-member emitted :self-p))
    (should (eq t (plist-get emitted :self-p)))
    (should (equal "self" (plist-get emitted :user-id)))))

(ert-deftest disco-gateway-poll-vote-event-freezes-other-user-identity ()
  (let ((disco-gateway--current-user-id "self")
        emitted)
    (cl-letf (((symbol-function 'disco-gateway--emit)
               (lambda (event) (setq emitted event))))
      (disco-gateway--dispatch-message-poll-vote-add
       '((channel_id . "c0")
         (message_id . "m1")
         (user_id . "other")
         (answer_id . 1))))
    (setq disco-gateway--current-user-id nil)
    (should (plist-member emitted :self-p))
    (should-not (plist-get emitted :self-p))
    (should (equal "other" (plist-get emitted :user-id)))))

(ert-deftest disco-gateway-dispatch-guild-feature-ack-applies-state-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-feature-ack)
               (lambda (read-state-type resource-id entity-id version)
                 (setq captured (list read-state-type resource-id entity-id version))
                 t))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-guild-feature-ack
       '((ack_type . 1)
         (resource_id . "guild1")
         (entity_id . "event9")
         (version . 7)))
      (should (equal '(1 "guild1" "event9" 7) captured))
      (should (equal '(:type guild-feature-ack
                       :read-state-type 1
                       :resource-id "guild1"
                       :entity-id "event9"
                       :version 7)
                     emitted)))))

(ert-deftest disco-gateway-dispatch-user-non-channel-ack-applies-state-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-feature-ack)
               (lambda (read-state-type resource-id entity-id version)
                 (setq captured (list read-state-type resource-id entity-id version))
                 t))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-user-non-channel-ack
       '((ack_type . 5)
         (resource_id . "user1")
         (entity_id . "mr4")
         (version . 2)))
      (should (equal '(5 "user1" "mr4" 2) captured))
      (should (equal '(:type user-non-channel-ack
                       :read-state-type 5
                       :resource-id "user1"
                       :entity-id "mr4"
                       :version 2)
                     emitted)))))

(ert-deftest disco-gateway-dispatch-notification-center-items-ack-applies-state-and-emits ()
  (let (captured emitted)
    (setq disco-gateway--current-user-id "user9")
    (cl-letf (((symbol-function 'disco-state-apply-feature-ack)
               (lambda (read-state-type resource-id entity-id &optional version)
                 (setq captured (list read-state-type resource-id entity-id version))
                 t))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-notification-center-items-ack
       '((id . "notif8")))
      (should (equal '(2 "user9" "notif8" nil) captured))
      (should (equal '(:type notification-center-items-ack
                       :read-state-type 2
                       :resource-id "user9"
                       :entity-id "notif8")
                     emitted)))))

(ert-deftest disco-gateway-dispatch-passive-update-v1-applies-state-and-emits ()
  (let (captured-updates emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-unread-updates)
               (lambda (updates)
                 (setq captured-updates updates)
                 1))
              ((symbol-function 'disco-state-apply-passive-voice-state-snapshot)
               (lambda (_guild-id _voice-states)
                 '("v1")))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-passive-update-v1
       '((guild_id . "g")
         (channels . (((id . "c2")
                       (last_message_id . "20"))))
         (voice_states . ())
         (members . ())))
      (should (equal '(((id . "c2")
                        (last_message_id . "20")))
                     captured-updates))
      (should (equal '(:type passive-update-v1
                       :guild-id "g"
                       :channels
                       (((id . "c2")
                         (last_message_id . "20")))
                       :voice-states nil
                       :members nil
                       :channel-ids ("c2" "v1"))
                     emitted)))))

(ert-deftest disco-gateway-dispatch-passive-update-v2-applies-state-and-emits ()
  (let (captured-updates emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-unread-updates)
               (lambda (updates)
                 (setq captured-updates updates)
                 2))
              ((symbol-function 'disco-state-apply-passive-voice-state-updates)
               (lambda (_guild-id _updated _removed)
                 '("v2")))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-passive-update-v2
       '((guild_id . "g")
         (updated_channels
          . (((id . "c2")
              (last_message_id . "20"))))
         (updated_voice_states . ())
         (removed_voice_states . ())
         (updated_members . ())))
      (should (equal '(((id . "c2")
                        (last_message_id . "20")))
                     captured-updates))
      (should (equal '(:type passive-update-v2
                       :guild-id "g"
                       :updated-channels
                       (((id . "c2")
                         (last_message_id . "20")))
                       :updated-voice-states nil
                       :removed-voice-states nil
                       :updated-members nil
                       :channel-ids ("c2" "v2"))
                     emitted)))))

(ert-deftest disco-gateway-dispatch-channel-sync-upserts-only-returned-channels ()
  (let (upserts emitted)
    (cl-letf (((symbol-function 'disco-state-upsert-gateway-channel)
               (lambda (channel &optional guild-id)
                 (push (list channel guild-id) upserts)))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-channel-sync
       '((guild_id . "g1")
         ;; An op38 request may have contained additional IDs.  Their omission
         ;; is meaningful and must not produce fabricated channel objects.
         (channels . (((id . "c1") (type . 0))))
         (integrity_check . "ok")))
      (setq upserts (nreverse upserts))
      (should (= 1 (length upserts)))
      (let ((channel (car (car upserts))))
        (should (equal "c1" (alist-get 'id channel)))
        (should (equal "g1" (alist-get 'guild_id channel))))
      (should (equal "g1" (cadr (car upserts))))
      (should (eq 'channel-sync (plist-get emitted :type)))
      (should (equal "g1" (plist-get emitted :guild-id)))
      (should (equal "ok" (plist-get emitted :integrity-check)))
      (should (= 1 (length (plist-get emitted :channels))))
      (should (equal "c1"
                     (alist-get 'id (car (plist-get emitted :channels))))))))

(ert-deftest disco-gateway-channel-sync-event-extractors-cover-response-scope ()
  (should
   (equal '("c1" "c2")
          (disco-gateway-event-channel-ids
           '(:type channel-sync
             :guild-id "g1"
             :channels (((id . "c1") (guild_id . "g1"))
                        ((id . "c2") (guild_id . "g1")))))))
  (should
   (equal '("g1")
          (disco-gateway-event-guild-ids
           '(:type channel-sync
             :guild-id "g1"
             :channels (((id . "c1") (guild_id . "g1"))))))))

(ert-deftest disco-gateway-channel-update-uses-gateway-specific-upsert ()
  (let (upserted emitted)
    (cl-letf (((symbol-function 'disco-state-upsert-gateway-channel)
               (lambda (channel &optional guild-id)
                 (setq upserted (list channel guild-id))))
              ((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) nil))
              ((symbol-function 'disco-gateway--emit-channel-event)
               (lambda (type channel)
                 (setq emitted (list type channel)))))
      (let ((channel '((id . "c1") (guild_id . "g1") (type . 0))))
        (disco-gateway--dispatch-channel-update channel)
        (should (equal (list channel nil) upserted))
        (should (equal (list 'channel-update channel) emitted))))))

(ert-deftest disco-gateway-thread-list-sync-uses-gateway-specific-batch ()
  (let (synced emitted)
    (cl-letf (((symbol-function 'disco-state-sync-gateway-threads)
               (lambda (guild-id parent-channel-ids threads)
                 (setq synced (list guild-id parent-channel-ids threads))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-thread-list-sync
       '((guild_id . "g1")
         (channel_ids . ("forum1"))
         (threads . (((id . "thread1") (parent_id . "forum1"))))))
      (should
       (equal '("g1" ("forum1")
                (((id . "thread1") (parent_id . "forum1"))))
              synced))
      (should
       (equal '(:type thread-list-sync
                :guild-id "g1"
                :channel-ids ("forum1")
                :threads (((id . "thread1") (parent_id . "forum1"))))
              emitted)))))

(ert-deftest disco-gateway-dispatch-user-update-applies-state-and-user-id ()
  (let ((applied nil))
    (setq disco-gateway--current-user-id nil)
    (cl-letf (((symbol-function 'disco-state-apply-user-update)
               (lambda () (setq applied t))))
      (disco-gateway--dispatch-user-update '((id . "u9")))
      (should applied)
      (should (equal "u9" disco-gateway--current-user-id)))))

(ert-deftest disco-gateway-dispatch-thread-create-applies-read-state-first ()
  (let (calls)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-state-apply-thread-create)
               (lambda (payload current-user-id)
                 (setq calls (append calls
                                     (list (list :apply payload current-user-id))))))
              ((symbol-function 'disco-gateway--upsert-channel-and-emit)
               (lambda (event-type payload)
                 (setq calls (append calls
                                     (list (list :upsert event-type payload)))))))
      (disco-gateway--dispatch-thread-create
       '((id . "th1")
         (parent_id . "forum")))
      (should (equal '((:apply ((id . "th1")
                                (parent_id . "forum"))
                               "u1")
                       (:upsert thread-create
                                ((id . "th1")
                                 (parent_id . "forum"))))
                     calls)))))

(ert-deftest disco-gateway-request-channel-info-sends-op43 ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-gateway--send-op)
               (lambda (op d)
                 (setq captured (list op d))
                 t)))
      (should (disco-gateway-request-channel-info "guild1" '(status "voice_start_time")))
      (should (equal '(43
                       ((guild_id . "guild1")
                        (fields "status" "voice_start_time")))
                     captured)))))

(ert-deftest disco-gateway-request-guild-channel-sync-sends-strict-deduplicated-op38 ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-gateway--send-op)
               (lambda (op data)
                 (setq captured (list op data))
                 t)))
      (should
       (disco-gateway-request-guild-channel-sync
        " 123 " [456 "456" " 789 "]))
      (should
       (equal '(38
                ((guild_id . "123")
                 (obfuscated_channel_ids "456" "789")))
              captured)))))

(ert-deftest disco-gateway-request-guild-channel-sync-rejects-incomplete-identities ()
  (cl-letf (((symbol-function 'disco-gateway--send-op)
             (lambda (&rest _args)
               (ert-fail "malformed channel sync request was sent"))))
    (should-error
     (disco-gateway-request-guild-channel-sync " " '("456"))
     :type 'user-error)
    (should-error
     (disco-gateway-request-guild-channel-sync "123" nil)
     :type 'user-error)
    (should-error
     (disco-gateway-request-guild-channel-sync "123" '("456" invalid))
     :type 'user-error)))

(ert-deftest disco-gateway-request-guild-members-sends-op8 ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-gateway--send-op)
               (lambda (op d)
                 (setq captured (list op d))
                 t)))
      (should (disco-gateway-request-guild-members
               "guild1"
               :query "ali"
               :limit 25
               :nonce "n1"))
      (should (equal '(8
                       ((guild_id . "guild1")
                        (query . "ali")
                        (limit . 25)
                        (nonce . "n1")))
                     captured)))))

(ert-deftest disco-gateway-dispatch-guild-members-chunk-applies-state-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-guild-members-chunk)
               (lambda (guild-id members presences)
                 (setq captured (list guild-id members presences))
                 '("u1")))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-guild-members-chunk
       '((guild_id . "g1")
         (members . (((user (id . "u1")
                            (username . "alice")))))
         (presences . (((user (id . "u1")))))
         (chunk_index . 0)
         (chunk_count . 1)
         (nonce . "n1")))
      (should (equal '("g1"
                       (((user (id . "u1")
                               (username . "alice"))))
                       (((user (id . "u1")))))
                     captured))
      (should (equal '(:type guild-members-chunk
                       :guild-id "g1"
                       :members (((user (id . "u1")
                                         (username . "alice"))))
                       :presences (((user (id . "u1"))))
                       :chunk-index 0
                       :chunk-count 1
                       :nonce "n1")
                     emitted)))))

(ert-deftest disco-gateway-dispatch-guild-emojis-update-replaces-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-set-guild-emojis)
               (lambda (guild-id emojis)
                 (setq captured (list guild-id emojis))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-guild-emojis-update
       '((guild_id . "g1")
         (emojis . [((id . "e1") (name . "wave"))])))
      (should (equal '("g1" [((id . "e1") (name . "wave"))])
                     captured))
      (should (equal '(:type guild-emojis-update
                       :guild-id "g1"
                       :emojis [((id . "e1") (name . "wave"))])
                     emitted)))))

(ert-deftest disco-gateway-guild-role-lifecycle-updates-state ()
  (disco-state-reset)
  (let (events)
    (cl-letf (((symbol-function 'disco-gateway--emit)
               (lambda (event) (push event events))))
      (disco-state-set-guild-roles "g1" [])
      (disco-gateway--dispatch-guild-role-create
       '((guild_id . "g1") (role . ((id . "r1") (name . "Admin")))))
      (should (equal "Admin"
                     (alist-get 'name
                                (car (disco-state-guild-roles "g1")))))
      (disco-gateway--dispatch-guild-role-update
       '((guild_id . "g1") (role . ((id . "r1") (name . "Moderator")))))
      (should (equal "Moderator"
                     (alist-get 'name
                                (car (disco-state-guild-roles "g1")))))
      (disco-gateway--dispatch-guild-role-delete
       '((guild_id . "g1") (role_id . "r1")))
      (should-not (disco-state-guild-roles "g1"))
      (should (equal '(guild-role-delete
                       guild-role-update
                       guild-role-create)
                     (mapcar (lambda (event) (plist-get event :type))
                             events)))))
  (disco-state-reset))

(ert-deftest disco-gateway-guild-member-lifecycle-updates-state ()
  (disco-state-reset)
  (let (events)
    (cl-letf (((symbol-function 'disco-gateway--emit)
               (lambda (event) (push event events))))
      (disco-gateway--dispatch-guild-member-add
       '((guild_id . "g1")
         (nick . "Old")
         (user (id . "u1") (username . "alice"))))
      (should (equal "Old"
                     (alist-get 'nick
                                (disco-state-guild-member "g1" "u1"))))
      (disco-gateway--dispatch-guild-member-update
       '((guild_id . "g1")
         (nick . "New")
         (user (id . "u1") (username . "alice"))))
      (should (equal "New"
                     (alist-get 'nick
                                (disco-state-guild-member "g1" "u1"))))
      (disco-gateway--dispatch-guild-member-remove
       '((guild_id . "g1")
         (user (id . "u1") (username . "alice"))))
      (should-not (disco-state-guild-member "g1" "u1"))
      (should (equal '(guild-member-remove
                       guild-member-update
                       guild-member-add)
                     (mapcar (lambda (event) (plist-get event :type))
                             events))))))

(ert-deftest disco-gateway-send-op-now-handles-write-errors ()
  (let ((disco-gateway--ws 'ws)
        (disco-gateway--stopping nil)
        (disco-gateway--reconnect-timer nil)
        scheduled)
    (cl-letf (((symbol-function 'websocket-openp)
               (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args)
                 (signal 'file-error '("Writing to process" "Invalid argument"))))
              ((symbol-function 'disco-gateway--schedule-reconnect)
               (lambda (&optional _delay)
                 (setq scheduled t)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (should-not (disco-gateway--send-op-now 34 '((guild_id . "g1"))))
      (should scheduled))))

(ert-deftest disco-gateway-send-queue-slot-available-p-reflects-capacity ()
  (let ((disco-gateway--send-queue-high '((1 . :null)))
        (disco-gateway--send-queue-normal '((34 . ((guild_id . "g1")))))
        (disco-gateway-send-queue-max-size 3))
    (should (disco-gateway-send-queue-slot-available-p))
    (should-not (disco-gateway-send-queue-slot-available-p 2))))

(ert-deftest disco-gateway-send-op-queues-when-rate-window-full ()
  (let ((disco-gateway--ws 'ws)
        (disco-gateway--send-history '(99.95 99.9))
        (disco-gateway--send-queue-high nil)
        (disco-gateway--send-queue-normal nil)
        (disco-gateway-send-max-events-per-window 2)
        (disco-gateway-send-window-seconds 60)
        (disco-gateway-send-queue-max-size 10)
        scheduled-delay
        send-called)
    (cl-letf (((symbol-function 'float-time)
               (lambda () 100.0))
              ((symbol-function 'websocket-openp)
               (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args)
                 (setq send-called t)))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest args)
                 (setq scheduled-delay delay)
                 (list :timer function args))))
      (should (disco-gateway--send-op 34 '((guild_id . "g1"))))
      (should-not send-called)
      (should (numberp scheduled-delay))
      (should (> scheduled-delay 0))
      (should (= 1 (disco-gateway--send-queue-size))))))

(ert-deftest disco-gateway-flush-send-queue-applies-min-spacing ()
  (let ((disco-gateway--ws 'ws)
        (disco-gateway--send-history nil)
        (disco-gateway--send-queue-high nil)
        (disco-gateway--send-queue-normal '((34 . ((guild_id . "g1")))
                                            (34 . ((guild_id . "g2")))))
        (disco-gateway-send-max-events-per-window 2)
        (disco-gateway-send-window-seconds 60)
        send-count
        scheduled-delay)
    (cl-letf (((symbol-function 'float-time)
               (lambda () 100.0))
              ((symbol-function 'websocket-openp)
               (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args)
                 (setq send-count (1+ (or send-count 0)))))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest args)
                 (setq scheduled-delay delay)
                 (list :timer function args))))
      (disco-gateway--flush-send-queue)
      (should (= 1 send-count))
      (should (numberp scheduled-delay))
      (should (>= scheduled-delay 29.9))
      (should (= 1 (disco-gateway--send-queue-size))))))

(ert-deftest disco-gateway-flush-send-queue-respects-opcode-cooldown ()
  (let ((disco-gateway--ws 'ws)
        (disco-gateway--send-history nil)
        (disco-gateway--send-queue-high nil)
        (disco-gateway--send-queue-normal '((34 . ((guild_id . "g1")))))
        (disco-gateway--send-opcode-cooldowns (make-hash-table :test #'equal))
        scheduled-delay
        send-called)
    (puthash '(34 . "g1")
             105.0
             disco-gateway--send-opcode-cooldowns)
    (cl-letf (((symbol-function 'float-time)
               (lambda () 100.0))
              ((symbol-function 'websocket-openp)
               (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args)
                 (setq send-called t)))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest args)
                 (setq scheduled-delay delay)
                 (list :timer function args))))
      (disco-gateway--flush-send-queue)
      (should-not send-called)
      (should (numberp scheduled-delay))
      (should (>= scheduled-delay 4.9))
      (should (= 1 (disco-gateway--send-queue-size))))))

(ert-deftest disco-gateway-dispatch-rate-limited-updates-send-cooldown ()
  (let ((disco-gateway--send-opcode-cooldowns (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'float-time)
               (lambda () 100.0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil)))
      (disco-gateway--dispatch-rate-limited
       '((opcode . 34)
         (retry_after . 7.5)
         (meta . ((guild_id . "g1"))))))
    (should (= 107.5
               (gethash '(34 . "g1")
                        disco-gateway--send-opcode-cooldowns)))))

(ert-deftest disco-gateway-enqueue-send-op-evicts-normal-for-high-priority ()
  (let ((disco-gateway--send-queue-high nil)
        (disco-gateway--send-queue-normal '((34 (guild_id . "g1"))) )
        (disco-gateway-send-queue-max-size 1))
    (should (disco-gateway--enqueue-send-op
             1
             :null
             disco-gateway--send-priority-high))
    (should (equal '((1 . :null)) disco-gateway--send-queue-high))
    (should-not disco-gateway--send-queue-normal)))

(ert-deftest disco-gateway-send-op-now-heartbeat-sets-awaiting-ack ()
  (let ((disco-gateway--ws 'ws)
        (disco-gateway--awaiting-heartbeat-ack nil)
        (disco-gateway--send-history nil))
    (cl-letf (((symbol-function 'websocket-openp)
               (lambda (_ws) t))
              ((symbol-function 'websocket-send-text)
               (lambda (&rest _args) t)))
      (should (disco-gateway--send-op-now 1 :null))
      (should disco-gateway--awaiting-heartbeat-ack))))

(ert-deftest disco-gateway-dispatch-voice-state-update-emits-channel-delta ()
  (let (emitted)
    (cl-letf (((symbol-function 'disco-state-apply-voice-state-update)
               (lambda (_payload)
                 '(:guild-id "g"
                   :channel-id "c2"
                   :previous-channel-id "c1"
                   :channel-ids ("c1" "c2")
                   :user-id "u1")))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-voice-state-update
       '((guild_id . "g")
         (channel_id . "c2")
         (user_id . "u1")))
      (should (equal '(:type voice-state-update
                       :guild-id "g"
                       :channel-id "c2"
                       :previous-channel-id "c1"
                       :channel-ids ("c1" "c2")
                       :user-id "u1"
                       :voice-state ((guild_id . "g")
                                     (channel_id . "c2")
                                     (user_id . "u1")))
                     emitted)))))

(ert-deftest disco-gateway-dispatch-last-messages-applies-state-and-emits ()
  (let (captured emitted)
    (cl-letf (((symbol-function 'disco-state-apply-last-messages)
               (lambda (messages)
                 (setq captured messages)
                 '("c1" "c2")))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event))))
      (disco-gateway--dispatch-last-messages
       '((guild_id . "g")
         (messages . (((id . "m1") (channel_id . "c1"))
                      ((id . "m2") (channel_id . "c2"))))))
      (should (equal '(((id . "m1") (channel_id . "c1"))
                       ((id . "m2") (channel_id . "c2")))
                     captured))
      (should (equal '(:type last-messages
                       :guild-id "g"
                       :messages (((id . "m1") (channel_id . "c1"))
                                  ((id . "m2") (channel_id . "c2")))
                       :channel-ids ("c1" "c2"))
                     emitted)))))

(ert-deftest disco-gateway-dispatch-ready-emits-after-state-ingestion ()
  (let (order emitted)
    (cl-letf (((symbol-function 'disco-state-begin-gateway-session)
               (lambda () (push 'session-boundary order)))
              ((symbol-function 'disco-gateway--ingest-ready-read-states)
               (lambda (_value) (push 'read-state order)))
              ((symbol-function 'disco-gateway--ingest-ready-guilds)
               (lambda (_value) (push 'guilds order)))
              ((symbol-function 'disco-gateway--ingest-ready-user-guild-settings)
               (lambda (_value) (push 'settings order)))
              ((symbol-function 'disco-gateway--ingest-ready-private-channels)
               (lambda (_value) (push 'private-channels order)))
              ((symbol-function 'disco-gateway--subscribe-watched-guild-channels)
               (lambda (&rest _) (push 'subscribe order)))
              ((symbol-function 'disco-gateway--reset-reconnect-backoff)
               (lambda () (push 'backoff order)))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event)
                 (setq emitted event)
                 (push 'emit order))))
      (disco-gateway--dispatch-ready
       '((user (id . "me"))
         (read_state . nil)
         (user_guild_settings . nil)
         (guilds . nil)
         (private_channels . nil)))
      (should (equal '(:type ready :user-id "me") emitted))
      (should (equal '(session-boundary read-state settings guilds private-channels
                       subscribe backoff emit)
                     (nreverse order))))))

(ert-deftest disco-gateway-ready-retires-old-access-before-ingesting-channels ()
  (disco-state-reset)
  (unwind-protect
      (let ((disco-gateway--session-generation 0))
        (disco-state-put-channels
         "g1"
         '(((id . "c1") (guild_id . "g1") (type . 0)
            (permissions . "0"))))
        (cl-letf (((symbol-function 'disco-gateway--subscribe-watched-guild-channels)
                   #'ignore)
                  ((symbol-function 'disco-gateway--reset-reconnect-backoff)
                   #'ignore)
                  ((symbol-function 'message) #'ignore))
          (disco-gateway--dispatch-ready
           '((session_id . "new-session")
             (user . ((id . "self")))
             (guilds . [((id . "g1")
                         (channels . [((id . "c1") (type . 0))]))]))))
        (should (= 1 (disco-gateway-session-generation)))
        (should (eq 'visible (disco-state-channel-access "c1")))
        (should-not (assq 'permissions (disco-state-channel "c1")))
        (should-not (disco-state-guild-channels-loaded-p "g1")))
    (disco-state-reset)))

(ert-deftest disco-gateway-compact-ready-cannot-leak-old-access-into-rest ()
  (disco-state-reset)
  (unwind-protect
      (let ((disco-gateway--session-generation 0))
        (disco-state-upsert-gateway-channel
         '((id . "c1") (guild_id . "g1") (type . 0)))
        (disco-state-put-channels
         "g1"
         '(((id . "c1") (guild_id . "g1") (type . 0)
            (permissions . "1024"))))
        (cl-letf (((symbol-function 'disco-gateway--subscribe-watched-guild-channels)
                   #'ignore)
                  ((symbol-function 'disco-gateway--reset-reconnect-backoff)
                   #'ignore)
                  ((symbol-function 'message) #'ignore))
          ;; Compact READY keeps the structural channel cache but contributes
          ;; no visibility evidence for c1 in the new session.
          (disco-gateway--dispatch-ready
           '((session_id . "new-session")
             (user . ((id . "self")))
             (guilds . [((id . "g1") (name . "Guild"))]))))
        (should-not (disco-state-channel-access "c1"))
        (should-not (assq 'permissions (disco-state-channel "c1")))

        (disco-state-put-channels
         "g1"
         '(((id . "c1") (guild_id . "g1") (type . 0)
            (permissions . "0"))))
        (should (eq 'hidden (disco-state-channel-access "c1")))
        (should-not (disco-state-channel-viewable-p
                     (disco-state-channel "c1") t)))
    (disco-state-reset)))

(ert-deftest disco-gateway-ready-channel-snapshot-seeds-unresolved-guild ()
  (disco-state-reset)
  (disco-gateway--ingest-ready-guilds
   '(((id . "g1")
      (channels . (((id . "c1") (guild_id . "g1") (type . 0)))))))
  (should-not (disco-state-guild-channels-loaded-p "g1"))
  (should (equal "c1"
                 (alist-get 'id (car (disco-state-guild-channels "g1"))))))

(ert-deftest disco-gateway-guild-create-seeds-explicit-channels-and-threads ()
  (disco-state-reset)
  (cl-letf (((symbol-function 'disco-gateway--emit-guild-event)
             (lambda (&rest _args) nil)))
    (disco-gateway--dispatch-guild-create
     '((id . "g1")
       (channels . [((id . "c1") (guild_id . "g1") (type . 0))])
       (threads . [((id . "t1")
                    (parent_id . "c1")
                    (type . 11))]))))
  ;; Gateway snapshots establish native visibility without claiming the
  ;; REST-computed action permissions tracked by the directory cache.
  (should-not (disco-state-guild-channels-loaded-p "g1"))
  (should (equal "c1" (alist-get 'id (disco-state-channel "c1"))))
  (should (equal "t1" (alist-get 'id (disco-state-channel "t1"))))
  (should (equal "g1" (alist-get 'guild_id (disco-state-channel "t1"))))
  (should (equal '("t1")
                 (mapcar (lambda (thread) (alist-get 'id thread))
                         (disco-state-parent-threads "c1")))))

(ert-deftest disco-gateway-compact-guild-create-preserves-associated-snapshots ()
  (disco-state-reset)
  (cl-letf (((symbol-function 'disco-gateway--emit-guild-event)
             (lambda (&rest _args) nil)))
    (disco-gateway--dispatch-guild-create
     '((id . "g1")
       (channels . [((id . "c1") (guild_id . "g1") (type . 0))])
       (threads . [((id . "t1")
                    (guild_id . "g1")
                    (parent_id . "c1")
                    (type . 11))])
       (emojis . [((id . "e1") (name . "wave"))])
       (roles . [((id . "r1") (name . "Admin"))])
       (members . [((nick . "Alice")
                    (user (id . "u1") (username . "alice")))])
       (presences . [((user (id . "u1")) (status . "online"))])))
    ;; A later compact GUILD_CREATE carries no replacement snapshots.
    (disco-gateway--dispatch-guild-create
     '((id . "g1") (name . "Renamed"))))
  (should (disco-state-channel "c1"))
  (should (disco-state-channel "t1"))
  (should (equal "wave"
                 (alist-get 'name (car (disco-state-guild-emojis "g1")))))
  (should (equal "Admin"
                 (alist-get 'name (car (disco-state-guild-roles "g1")))))
  (should (equal "Alice"
                 (alist-get 'nick
                            (disco-state-guild-member "g1" "u1"))))
  (should (equal "online"
                 (alist-get 'status (disco-state-presence "u1" "g1")))))

(ert-deftest disco-gateway-guild-emoji-snapshots-only-change-when-explicit ()
  (disco-state-reset)
  (disco-gateway--ingest-ready-guilds
   '(((id . "g1")
      (emojis . [((id . "e1") (name . "wave"))]))))
  (should (equal "wave"
                 (alist-get 'name (car (disco-state-guild-emojis "g1")))))
  ;; Compact guild payloads omit `emojis'; omission is not an empty snapshot.
  (disco-gateway--dispatch-guild-update
   '((id . "g1") (name . "Compact")))
  (should (equal "wave"
                 (alist-get 'name (car (disco-state-guild-emojis "g1")))))
  (disco-gateway--dispatch-guild-update
   '((id . "g1") (emojis . [])))
  (should (disco-state-guild-emojis-loaded-p "g1"))
  (should-not (disco-state-guild-emojis "g1")))

(ert-deftest disco-gateway-guild-role-snapshots-only-change-when-explicit ()
  (disco-state-reset)
  (disco-gateway--ingest-ready-guilds
   '(((id . "g1")
      (roles . [((id . "r1") (name . "Admin"))]))))
  (disco-gateway--dispatch-guild-update
   '((id . "g1") (name . "Compact")))
  (should (equal "Admin"
                 (alist-get 'name (car (disco-state-guild-roles "g1")))))
  (disco-gateway--dispatch-guild-update
   '((id . "g1") (roles . [])))
  (should (disco-state-guild-roles-loaded-p "g1"))
  (should-not (disco-state-guild-roles "g1")))

(ert-deftest disco-gateway-ready-and-create-seed-explicit-guild-members ()
  (disco-state-reset)
  (disco-gateway--ingest-ready-guilds
   '(((id . "g1")
      (members . [((nick . "Ready")
                    (user (id . "u1") (username . "alice")))])
      (presences . [((user (id . "u1")) (status . "online"))]))))
  (should (equal "Ready"
                 (alist-get 'nick
                            (disco-state-guild-member "g1" "u1"))))
  (should (equal "online"
                 (alist-get 'status (disco-state-presence "u1" "g1"))))
  (cl-letf (((symbol-function 'disco-gateway--emit-guild-event)
             (lambda (&rest _args) nil)))
    (disco-gateway--dispatch-guild-create
     '((id . "g2")
       (members . [((nick . "Create")
                     (user (id . "u2") (username . "bob")))]))))
  (should (equal "Create"
                 (alist-get 'nick
                            (disco-state-guild-member "g2" "u2")))))

(ert-deftest disco-gateway-dispatch-user-guild-settings-update-applies-and-emits ()
  (let (applied emitted)
    (cl-letf (((symbol-function 'disco-state-apply-user-guild-setting)
               (lambda (setting) (setq applied setting)))
              ((symbol-function 'disco-gateway--emit)
               (lambda (event) (setq emitted event))))
      (let ((setting '((guild_id . "g") (muted . t))))
        (disco-gateway--dispatch-user-guild-settings-update setting)
        (should (equal setting applied))
        (should (equal `(:type user-guild-settings-update
                         :guild-id "g" :setting ,setting)
                       emitted))))))

(ert-deftest disco-gateway-event-guild-ids-infers-indexed-and-deleted-channels ()
  (disco-state-reset)
  (disco-state-upsert-channel
   '((id . "c1") (guild_id . "g1") (type . 0)))
  (should
   (equal '("g1")
          (disco-gateway-event-guild-ids
           '(:type message-create :channel-id "c1"))))
  (disco-state-delete-channel "c1")
  (should
   (equal '("g1")
          (disco-gateway-event-guild-ids
           '(:type channel-delete
             :channel-id "c1"
             :channel ((id . "c1") (guild_id . "g1")))))))

(ert-deftest disco-gateway-old-websocket-callbacks-cannot-touch-successor ()
  (let ((disco-gateway--connection-generation 0)
        (disco-gateway--connection-owner nil)
        (disco-gateway--reset-in-progress nil)
        (disco-gateway--ws nil)
        (disco-gateway--connecting nil)
        (disco-gateway--stopping nil)
        (disco-gateway--heartbeat-timer nil)
        (disco-gateway--heartbeat-timer-owner nil)
        (disco-gateway--reconnect-timer nil)
        (disco-gateway--reconnect-timer-owner nil)
        (disco-gateway--send-queue-timer nil)
        (disco-gateway--send-queue-timer-owner nil)
        (disco-gateway--send-opcode-cooldowns
         (make-hash-table :test #'equal))
        callbacks
        closed
        (payload-count 0)
        (reconnect-count 0)
        (reset-count 0))
    (cl-letf (((symbol-function 'disco-gateway--ensure-token) #'ignore)
              ((symbol-function 'disco-gateway--connect-url)
               (lambda () "wss://gateway.invalid"))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest arguments)
                 (let ((websocket (if callbacks 'new-websocket
                                    'old-websocket)))
                   (push (cons websocket arguments) callbacks)
                   websocket)))
              ((symbol-function 'websocket-openp) (lambda (_websocket) t))
              ((symbol-function 'websocket-close)
               (lambda (websocket) (push websocket closed)))
              ((symbol-function 'disco-gateway--frame-json-text)
               (lambda (_frame) "{}"))
              ((symbol-function 'disco-gateway--json-decode)
               (lambda (_text) 'decoded))
              ((symbol-function 'disco-gateway--handle-payload)
               (lambda (_payload) (cl-incf payload-count)))
              ((symbol-function 'disco-gateway--schedule-reconnect)
               (lambda (&optional _delay) (cl-incf reconnect-count)))
              ((symbol-function 'disco-gateway--zlib-reset-state)
               (lambda () (cl-incf reset-count)))
              ((symbol-function 'disco-gateway--reset-send-rate-state)
               #'ignore)
              ((symbol-function 'message) #'ignore))
      (should (eq 'old-websocket (disco-gateway--connect)))
      (let* ((old-entry (car callbacks))
             (old-arguments (cdr old-entry))
             (old-open (plist-get old-arguments :on-open))
             (old-message (plist-get old-arguments :on-message))
             (old-close (plist-get old-arguments :on-close))
             (old-error (plist-get old-arguments :on-error)))
        (disco-gateway-stop)
        (setq disco-gateway--stopping nil)
        (should (eq 'new-websocket (disco-gateway--connect)))
        (let ((new-owner disco-gateway--connection-owner)
              (new-generation disco-gateway--connection-generation))
          (setq disco-gateway--connecting t
                payload-count 0
                reconnect-count 0
                reset-count 0)
          (funcall old-open 'old-websocket)
          (funcall old-message 'old-websocket 'late-frame)
          (funcall old-error 'old-websocket 'old-error '(error "late"))
          (funcall old-close 'old-websocket)
          (should (= 0 payload-count))
          (should (= 0 reconnect-count))
          (should (= 0 reset-count))
          (should (eq 'new-websocket disco-gateway--ws))
          (should (eq new-owner disco-gateway--connection-owner))
          (should (= new-generation disco-gateway--connection-generation))
          (should disco-gateway--connecting))))))

(ert-deftest disco-gateway-reset-before-websocket-return-closes-returned-socket ()
  (let ((disco-gateway--connection-generation 4)
        (disco-gateway--connection-owner nil)
        (disco-gateway--reset-in-progress nil)
        (disco-gateway--ws nil)
        (disco-gateway--connecting nil)
        (disco-gateway--stopping nil)
        (disco-gateway--heartbeat-timer nil)
        (disco-gateway--heartbeat-timer-owner nil)
        (disco-gateway--reconnect-timer nil)
        (disco-gateway--reconnect-timer-owner nil)
        (disco-gateway--send-queue-timer nil)
        (disco-gateway--send-queue-timer-owner nil)
        (disco-gateway--send-opcode-cooldowns
         (make-hash-table :test #'equal))
        closed)
    (cl-letf (((symbol-function 'disco-gateway--ensure-token) #'ignore)
              ((symbol-function 'disco-gateway--connect-url)
               (lambda () "wss://gateway.invalid"))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest _arguments)
                 (disco-gateway-stop)
                 'returned-after-reset))
              ((symbol-function 'websocket-close)
               (lambda (websocket) (push websocket closed)))
              ((symbol-function 'message) #'ignore))
      (should-not (disco-gateway--connect))
      (should (equal '(returned-after-reset) closed))
      (should-not disco-gateway--connection-owner)
      (should-not disco-gateway--ws)
      (should-not disco-gateway--connecting)
      (should disco-gateway--stopping)
      (should (= 6 disco-gateway--connection-generation)))))

(ert-deftest disco-gateway-connection-timers-ignore-cancelled-callbacks ()
  (let* ((connection-owner
          (list :generation 10 :websocket 'current-websocket))
         (disco-gateway--connection-generation 10)
         (disco-gateway--connection-owner connection-owner)
         (disco-gateway--reset-in-progress nil)
         (disco-gateway--ws 'current-websocket)
         (disco-gateway--stopping nil)
         (disco-gateway--heartbeat-timer nil)
         (disco-gateway--heartbeat-timer-owner nil)
         (disco-gateway--reconnect-timer nil)
         (disco-gateway--reconnect-timer-owner nil)
         (disco-gateway--reconnect-attempt 0)
         (disco-gateway--send-queue-timer nil)
         (disco-gateway--send-queue-timer-owner nil)
         (next-timer 0)
         callbacks
         canceled
         (heartbeat-ticks 0)
         (send-flushes 0)
         (connects 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest _arguments)
                 (let ((timer (intern (format "timer-%d" (cl-incf next-timer)))))
                   (push (cons timer function) callbacks)
                   timer)))
              ((symbol-function 'timerp)
               (lambda (object)
                 (and (symbolp object)
                      (string-prefix-p "timer-" (symbol-name object)))))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer canceled)))
              ((symbol-function 'disco-gateway--send-heartbeat) #'ignore)
              ((symbol-function 'disco-gateway--heartbeat-tick)
               (lambda () (cl-incf heartbeat-ticks)))
              ((symbol-function 'disco-gateway--flush-send-queue)
               (lambda () (cl-incf send-flushes)))
              ((symbol-function 'disco-gateway--connect)
               (lambda () (cl-incf connects)))
              ((symbol-function 'message) #'ignore))
      (let* ((timer (disco-gateway--start-heartbeat 1000))
             (callback (cdr (assq timer callbacks))))
        (disco-gateway--cancel-heartbeat-timer)
        (funcall callback)
        (should (= 0 heartbeat-ticks)))
      (let* ((timer (disco-gateway--schedule-send-queue-drain 1))
             (callback (cdr (assq timer callbacks))))
        (disco-gateway--cancel-send-queue-timer)
        (funcall callback)
        (should (= 0 send-flushes)))
      (disco-gateway--schedule-reconnect 1)
      (let* ((old-timer disco-gateway--reconnect-timer)
             (old-callback (cdr (assq old-timer callbacks)))
             (replacement-owner
              (list :generation 10 :connection connection-owner
                    :timer 'replacement-timer)))
        (disco-gateway--cancel-reconnect-timer)
        (setq disco-gateway--reconnect-timer 'replacement-timer
              disco-gateway--reconnect-timer-owner replacement-owner)
        (funcall old-callback)
        (should (= 0 connects))
        (should (eq 'replacement-timer disco-gateway--reconnect-timer))
        (should (eq replacement-owner
                    disco-gateway--reconnect-timer-owner)))
      (should (= 3 (length canceled))))))

(ert-deftest disco-gateway-reset-barrier-rejects-new-watchers-and-purely-clears-old ()
  (let ((disco-gateway--reset-in-progress t)
        (disco-gateway--watch-counts (make-hash-table :test #'equal))
        (disco-gateway--global-watch-count 3)
        (disco-gateway--lazy-subscribed-channels
         (make-hash-table :test #'equal))
        (disco-gateway--send-opcode-cooldowns
         (make-hash-table :test #'equal))
        (disco-gateway--connection-generation 7)
        starts
        subscriptions)
    (puthash "old-channel" 2 disco-gateway--watch-counts)
    (cl-letf (((symbol-function 'disco-gateway-start)
               (lambda () (cl-incf starts)))
              ((symbol-function 'disco-gateway--maybe-subscribe-watched-channel)
               (lambda (_channel-id) (cl-incf subscriptions))))
      (should-not (disco-gateway-watch-channel "new-channel"))
      (should-not (disco-gateway-watch-global)))
    (should (= 2 (gethash "old-channel" disco-gateway--watch-counts)))
    (should-not (gethash "new-channel" disco-gateway--watch-counts))
    (should (= 3 disco-gateway--global-watch-count))
    (should-not starts)
    (should-not subscriptions)
    (disco-gateway--clear-session-data)
    (should (= 0 (hash-table-count disco-gateway--watch-counts)))
    (should (= 0 disco-gateway--global-watch-count))
    (should (= 8 disco-gateway--connection-generation))))

(provide 'disco-gateway-test)

;;; disco-gateway-test.el ends here
