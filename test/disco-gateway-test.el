;;; disco-gateway-test.el --- Tests for disco-gateway read-state flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-gateway)

(ert-deftest disco-gateway-identify-payload-includes-passive-v2-capability-by-default ()
  (let ((disco-gateway-identify-capabilities nil)
        (disco-gateway-enable-passive-guild-update-v2 t)
        (disco-gateway-identify-intents nil)
        (disco-gateway-identify-presence nil))
    (cl-letf (((symbol-function 'disco-current-token)
               (lambda () "tok")))
      (let* ((payload (disco-gateway--identify-payload))
             (capabilities (alist-get 'capabilities payload)))
        (should (= capabilities (ash 1 14)))))))

(ert-deftest disco-gateway-identify-payload-merges-custom-and-passive-capabilities ()
  (let ((disco-gateway-identify-capabilities (ash 1 2))
        (disco-gateway-enable-passive-guild-update-v2 t)
        (disco-gateway-identify-intents nil)
        (disco-gateway-identify-presence nil))
    (cl-letf (((symbol-function 'disco-current-token)
               (lambda () "tok")))
      (let* ((payload (disco-gateway--identify-payload))
             (capabilities (alist-get 'capabilities payload)))
        (should (= capabilities (logior (ash 1 2) (ash 1 14))))))))

(ert-deftest disco-gateway-identify-payload-omits-capabilities-when-passive-v2-disabled ()
  (let ((disco-gateway-identify-capabilities nil)
        (disco-gateway-enable-passive-guild-update-v2 nil)
        (disco-gateway-identify-intents nil)
        (disco-gateway-identify-presence nil))
    (cl-letf (((symbol-function 'disco-current-token)
               (lambda () "tok")))
      (let ((payload (disco-gateway--identify-payload)))
        (should-not (assq 'capabilities payload))))))

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
    (cl-letf (((symbol-function 'disco-gateway--ingest-ready-read-states)
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
      (should (equal '(read-state settings guilds private-channels subscribe backoff emit)
                     (nreverse order))))))

(ert-deftest disco-gateway-ready-channel-snapshot-marks-guild-loaded ()
  (disco-state-reset)
  (disco-gateway--ingest-ready-guilds
   '(((id . "g1")
      (channels . (((id . "c1") (guild_id . "g1") (type . 0)))))))
  (should (disco-state-guild-channels-loaded-p "g1"))
  (should (equal "c1"
                 (alist-get 'id (car (disco-state-guild-channels "g1"))))))

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

(provide 'disco-gateway-test)

;;; disco-gateway-test.el ends here
