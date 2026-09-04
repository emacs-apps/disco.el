;;; disco-room-poll-test.el --- Tests for room poll interaction -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-room)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

(defun disco-room-poll-test-message (&optional channel-id)
  "Return a two-answer open poll fixture for CHANNEL-ID."
  `((id . "p1")
    (channel_id . ,(or channel-id "poll-race"))
    (content . "")
    (poll . ((question . ((text . "Question")))
             (allow_multiselect . t)
             (answers . (((answer_id . 1)
                          (poll_media . ((text . "one"))))
                         ((answer_id . 2)
                          (poll_media . ((text . "two"))))))
             (results . ((answer_counts . (((id . 1)
                                            (count . 0)
                                            (me_voted . :false))
                                           ((id . 2)
                                            (count . 0)
                                            (me_voted . :false))))))))))

(ert-deftest disco-room-poll-menu-hides-nonactionable-polls ()
  (disco-room-test-with-runtime
    "The message menu exposes polls only when a poll action can run."
    (let ((message '((id . "p1")
                     (poll . ((question . ((text . "Question"))))))))
      (cl-letf (((symbol-function 'disco-room-menu--message-at-point)
                 (lambda () message))
                ((symbol-function 'disco-room--poll-vote-unavailable-reason)
                 (lambda (&optional _message) nil))
                ((symbol-function 'disco-room--poll-expire-unavailable-reason)
                 (lambda (&optional _message) "only poll author can end this poll")))
        (should (disco-room-poll-actionable-at-point-p)))
      (cl-letf (((symbol-function 'disco-room-menu--message-at-point)
                 (lambda () message))
                ((symbol-function 'disco-room--poll-vote-unavailable-reason)
                 (lambda (&optional _message) "poll is closed"))
                ((symbol-function 'disco-room--poll-expire-unavailable-reason)
                 (lambda (&optional _message) "poll is already closed")))
        (should-not (disco-room-poll-actionable-at-point-p))))))

(ert-deftest disco-room-poll-rest-success-and-self-echo-are-idempotent ()
  (disco-room-test-with-runtime
    (let ((disco-runtime--app nil)
          success-callback)
      (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
        (unwind-protect
            (disco-room-test-with-surface "poll-echo"
              (disco-room-test-setup-channel "poll-echo")
              (disco-state-put-messages
               "poll-echo" (list (disco-room-poll-test-message "poll-echo")))
              (disco-room--ensure-surface)
              (disco-room--poll-set-draft-selection "p1" '(1))
              (cl-letf (((symbol-function 'disco-api-create-poll-vote-async)
                         (lambda (_channel-id _message-id _answer-ids &rest args)
                           (setq success-callback
                                 (plist-get args :on-success))))
                        ((symbol-function 'disco-gateway-current-user-id)
                         (lambda () "self"))
                        ((symbol-function 'message) #'ignore))
                (disco-room-submit-poll-vote "p1")
                (funcall success-callback nil)
              ;; This is a newer unsent draft and must survive the old echo.
                (disco-room--poll-set-draft-selection "p1" '(2))
                (disco-room--apply-live-poll-vote-event
                 '(:type message-poll-vote-add
                   :message-id "p1"
                   :answer-id 1
                   :user-id "self")))
              (let ((poll (disco-msg-poll (disco-room--message-by-id "p1"))))
                (should (= 1 (disco-msg-poll-answer-count poll 1)))
                (should (equal '(1) (disco-msg-poll-voted-answer-ids poll)))
                (should (equal '(2)
                               (disco-room--poll-draft-selection "p1")))))
          (disco-runtime-stop))))))

(ert-deftest disco-room-poll-self-echo-before-rest-completion-is-authoritative ()
  (disco-room-test-with-runtime
    (let ((disco-runtime--app nil)
          success-callback
          )
      (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
        (unwind-protect
            (disco-room-test-with-surface "poll-echo-first"
              (disco-room-test-setup-channel "poll-echo-first")
              (disco-state-put-messages
               "poll-echo-first"
               (list (disco-room-poll-test-message "poll-echo-first")))
              (disco-room--ensure-surface)
              (disco-room--poll-set-draft-selection "p1" '(1))
              (cl-letf (((symbol-function 'disco-api-create-poll-vote-async)
                         (lambda (_channel-id _message-id _answer-ids &rest args)
                           (setq success-callback
                                 (plist-get args :on-success))))
                        ((symbol-function 'disco-gateway-current-user-id)
                         (lambda () "self"))

                        ((symbol-function 'message) #'ignore))
                (disco-room-submit-poll-vote "p1")
                (disco-room--apply-live-poll-vote-event
                 '(:type message-poll-vote-add
                   :message-id "p1"
                   :answer-id 1
                   :user-id "self"))
                (funcall success-callback nil))
              (let ((poll (disco-msg-poll (disco-room--message-by-id "p1"))))
                (should (= 1 (disco-msg-poll-answer-count poll 1)))
                (should (equal '(1) (disco-msg-poll-voted-answer-ids poll)))
                (should-not (disco-room--poll-draft-selection-present-p "p1"))
                ))
          (disco-runtime-stop))))))

(ert-deftest disco-room-queued-poll-echo-keeps-frozen-self-identity ()
  (disco-room-test-with-runtime
    (let ((disco-runtime--app nil)
          (disco-gateway--current-user-id "self")
          emitted)
      (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
        (unwind-protect
            (disco-room-test-with-surface "poll-queued-self"
              (disco-room-test-setup-channel "poll-queued-self")
            ;; This is the state after the matching REST success.
              (disco-state-put-messages
               "poll-queued-self"
               (list
                (disco-room--message-with-poll-vote-selection
                 (disco-room-poll-test-message "poll-queued-self")
                 '(1))))
              (disco-room--poll-set-draft-selection "p1" '(1))
              (let ((view (disco-room--ensure-surface))
                    (op-token (disco-room--poll-vote-op-begin "p1" '(1))))
                (cl-letf (((symbol-function 'disco-gateway--emit)
                           (lambda (event) (setq emitted event))))
                  (disco-gateway--dispatch-message-poll-vote-add
                   '((channel_id . "poll-queued-self")
                     (message_id . "p1")
                     (user_id . "self")
                     (answer_id . 1))))
                (should (eq t (plist-get emitted :self-p)))
                (disco-room--queue-update view (list 'gateway-event emitted))
                (setq disco-gateway--current-user-id nil)
                (disco-room-test-drain view)
                (should-not
                 (disco-room--poll-vote-op-current-p "p1" op-token))
                (should-not
                 (disco-room--poll-draft-selection-present-p "p1")))
              (let ((poll (disco-msg-poll (disco-room--message-by-id "p1"))))
                (should (= 1 (disco-msg-poll-answer-count poll 1)))
                (should (equal '(1)
                               (disco-msg-poll-voted-answer-ids poll)))))
          (disco-runtime-stop))))))

(ert-deftest disco-room-stale-poll-rest-success-cannot-overwrite-newer-op ()
  (disco-room-test-with-runtime
    (let ((disco-runtime--app nil)
          callbacks
          )
      (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
        (unwind-protect
            (disco-room-test-with-surface "poll-generation"
              (disco-room-test-setup-channel "poll-generation")
              (disco-state-put-messages
               "poll-generation"
               (list (disco-room-poll-test-message "poll-generation")))
              (disco-room--ensure-surface)
              (cl-letf (((symbol-function 'disco-api-create-poll-vote-async)
                         (lambda (_channel-id _message-id _answer-ids &rest args)
                           (setq callbacks
                                 (append callbacks
                                         (list (plist-get args :on-success))))))

                        ((symbol-function 'message) #'ignore))
                (disco-room--poll-set-draft-selection "p1" '(1))
                (disco-room-submit-poll-vote "p1")
                (disco-room--poll-set-draft-selection "p1" '(2))
                (disco-room-submit-poll-vote "p1")
              ;; New completion wins, then a newly staged draft must survive
              ;; the old completion as well.
                (funcall (nth 1 callbacks) nil)
                (disco-room--poll-set-draft-selection "p1" '(1 2))
                (funcall (nth 0 callbacks) nil))
              (let ((poll (disco-msg-poll (disco-room--message-by-id "p1"))))
                (should (equal '(2) (disco-msg-poll-voted-answer-ids poll)))
                (should (= 0 (disco-msg-poll-answer-count poll 1)))
                (should (= 1 (disco-msg-poll-answer-count poll 2)))
                (should (equal '(1 2)
                               (disco-room--poll-draft-selection "p1")))
                ))
          (disco-runtime-stop))))))

(ert-deftest disco-room-send-poll-errors-while-replying ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "chat"
      (setq-local disco-room--channel-id "chat")
      (disco-room--set-composer-aux-state nil "m1")
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "562949953423360")))
      (should-error (disco-room-send-poll "Q" '("a" "b") 24 nil nil)
                    :type 'user-error))))

(ert-deftest disco-room-send-poll-rejects-overlong-content-before-send-state ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "chan"
      (setq-local disco-room--channel-id "chan")
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "562949953423360")))
      (should-error
       (disco-room-send-poll "Question" '("one" "two") 24 nil
                             (make-string (1+ disco-api--message-content-limit) ?a))
       :type 'error)
      (should-not disco-room--send-in-flight))))

(ert-deftest disco-room-deleted-poll-response-does-not-advance-live-frontier ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "chat"
      (disco-room-test-setup-channel "chat")
      (setq disco-room--remote-latest-message-id "100")
      (let (success)
        (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                   #'ignore)
                  ((symbol-function 'disco-permission-ensure-channel)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'disco-room--channel-object)
                   (lambda () '((id . "chat"))))

                  ((symbol-function 'disco-room--channel-buffer-p)
                   (lambda (&rest _arguments) t))

                  ((symbol-function 'disco-room--request-render) #'ignore)
                  ((symbol-function 'disco-api-create-message-async)
                   (lambda (_channel-id &rest options)
                     (setq success (plist-get options :on-success))))
                  ((symbol-function 'message) #'ignore))
          (disco-room-send-poll "Question" '("one" "two"))
          (disco-state-delete-message "chat" "200")
          (funcall success
                   '((id . "200") (channel_id . "chat")
                     (content . "") (poll . ((question . ((text . "Question"))))))))
        (should (equal "100" disco-room--remote-latest-message-id))
        (should-not (disco-room--channel-message-by-id "chat" "200"))))))

(ert-deftest disco-room-poll-expire-captures-revision-after-confirmation ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "chat"
      (disco-room-test-setup-channel "chat")
      (disco-state-put-messages
       "chat" (list (disco-room-poll-test-message "chat")))
      (let ((disco-room-poll-confirm-expire t))
        (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                   #'ignore)
                  ((symbol-function 'disco-permission-ensure-channel)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'disco-room--channel-object)
                   (lambda () '((id . "chat"))))

                  ((symbol-function 'disco-room--channel-buffer-p)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _arguments)
                     (disco-state-upsert-message
                      "chat"
                      '((id . "p1") (channel_id . "chat")
                        (content . "updated during confirmation")))
                     t))
                  ((symbol-function 'disco-api-expire-poll-async)
                   (lambda (_channel-id _message-id &rest options)
                     (funcall
                      (plist-get options :on-success)
                      '((id . "p1") (channel_id . "chat")
                        (content . "expired response"))))))
          (disco-room-expire-poll "p1")))
      (should
       (equal "expired response"
              (alist-get 'content
                         (disco-room--channel-message-by-id "chat" "p1")))))))

;;; disco-room-poll-test.el ends here
