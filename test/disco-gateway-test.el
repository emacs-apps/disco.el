;;; disco-gateway-test.el --- Tests for disco-gateway read-state flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-gateway)

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

(ert-deftest disco-gateway-dispatch-passive-update-v1-applies-state-and-emits ()
  (let (captured-updates emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-unread-updates)
               (lambda (updates)
                 (setq captured-updates updates)
                 1))
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
                       :members nil)
                     emitted)))))

(ert-deftest disco-gateway-dispatch-passive-update-v2-applies-state-and-emits ()
  (let (captured-updates emitted)
    (cl-letf (((symbol-function 'disco-state-apply-channel-unread-updates)
               (lambda (updates)
                 (setq captured-updates updates)
                 2))
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
                       :updated-members nil)
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

(provide 'disco-gateway-test)

;;; disco-gateway-test.el ends here
