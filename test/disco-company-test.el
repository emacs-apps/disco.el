;;; disco-company-test.el --- Tests for Disco composer completion -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(let ((load-prefer-newer t))
  (require 'disco-company))

(defun disco-company-test--complete
    (typed expected-label expected-input)
  "Complete TYPED as EXPECTED-LABEL and assert EXPECTED-INPUT."
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (setq-local disco-room--guild-id "g1")
    (setq-local disco-room--channel-id "room")
    (insert typed)
    (let ((draft-sync-count 0))
      (cl-letf (((symbol-function 'disco-room--sync-draft-from-buffer)
                 (lambda () (cl-incf draft-sync-count))))
        (let* ((capf (disco-room-complete-at-point))
               (table (nth 2 capf))
               (exit-function
                (plist-get (nthcdr 3 capf) :exit-function)))
          (should capf)
          (should (member expected-label
                          (all-completions typed table)))
          ;; Model the completion UI replacing the token with its visible
          ;; label before calling the CAPF exit function.
          (delete-region (nth 0 capf) (point))
          (insert expected-label)
          (funcall exit-function expected-label 'finished)
          (should (equal expected-input
                         (appkit-chatbuf-input-string)))
          (should (equal expected-input
                         (appkit-chatbuf-input-state)))
          (should (= 1 draft-sync-count)))))))

(ert-deftest disco-company-token-bounds-support-unicode ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "你好 @徐天")
    (should
     (equal (list :start (- (point) 3)
                  :end (point)
                  :trigger ?@
                  :raw "@徐天"
                  :query "徐天")
            (disco-company--completion-token-bounds)))))

(ert-deftest disco-company-token-bounds-support-closed-custom-emoji ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "你好 :跳舞:")
    (should
     (equal (list :start (- (point) 4)
                  :end (point)
                  :trigger ?:
                  :raw ":跳舞:"
                  :query "跳舞")
            (disco-company--completion-token-bounds)))))

(ert-deftest disco-company-guild-member-alias-inserts-user-mention ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-apply-guild-members-chunk
         "g1"
         '(((nick . "徐天天")
            (user (id . "1356835185")
                  (username . "GreenKite")
                  (global_name . "Green Kite")))))
        ;; The visible nick does not contain this query; the match comes from
        ;; the cached member's username search alias.
        (disco-company-test--complete
         "@green" "@徐天天" "<@1356835185> ")
        (disco-company-test--complete
         "@kite" "@徐天天" "<@1356835185> "))
    (disco-state-reset)))

(ert-deftest disco-company-member-search-response-updates-model-without-presentation ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (setq-local disco-room--guild-id "g1")
    (setq-local disco-room--channel-id "room")
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal))
    (insert "@alice")
    (let (requests)
      (cl-letf (((symbol-function 'disco-gateway-running-p)
                 (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (guild-id &rest args)
                   (push (cons guild-id args) requests)
                   t))
                ((symbol-function 'appkit-chat-completion-complete)
                 (lambda (&rest _args)
                   (ert-fail "gateway callback reopened completion UI")))
                ((symbol-function 'minibuffer-completion-help)
                 (lambda (&rest _args)
                   (ert-fail "gateway callback refreshed minibuffer UI"))))
        (disco-company--maybe-request-guild-members "alice" :explicit t)
        (disco-company--maybe-request-guild-members "alice" :explicit t)
        (should (= 1 (length requests)))
        (let* ((request (car requests))
               (nonce (plist-get (cdr request) :nonce)))
          (should (equal "g1" (car request)))
          (should (equal "alice" (plist-get (cdr request) :query)))
          (should (stringp nonce))
          (disco-company--handle-gateway-event
           (list :type 'guild-members-chunk
                 :guild-id "wrong-guild"
                 :nonce nonce
                 :members nil))
          (should disco-company--pending-member-search)
          (disco-company--handle-gateway-event
           (list :type 'guild-members-chunk
                 :guild-id "g1"
                 :nonce nonce
                 :members '(((user (id . "u1"))))))
          (should-not disco-company--pending-member-search)
          (should (eq 'done
                      (plist-get
                       (gethash '("g1" . "alice")
                                disco-company--member-search-requests)
                       :status)))
          ;; A completed request is cached for the retry window.
          (disco-company--maybe-request-guild-members "alice" :explicit t)
          (should (= 1 (length requests))))))))

(ert-deftest disco-company-gateway-and-timers-are-exact-view-owned ()
  (appkit-register-app-kind 'disco-company-test nil)
  (let ((app (appkit-start-app 'disco-company-test :id 'completion-owner)))
    (unwind-protect
        (with-temp-buffer
          (let* ((view
                  (appkit-attach-view
                   :app app :id '(room completion-owner)
                   :mode major-mode :parts nil))
                 old-handler
                 old-token)
            (disco-company-setup-room-buffer)
            (setq old-handler disco-company--gateway-handler
                  old-token disco-company--owner-token)
            (should (functionp old-handler))
            (should (appkit-handle-alive-p disco-company--gateway-handle))
            (should (eq view
                        (appkit-handle-owner disco-company--gateway-handle)))
            (appkit-kill-view view)
            (should-not (memq old-handler disco-gateway-event-hook))
            (should-not disco-company--gateway-handler)
            (should-not disco-company--gateway-handle)
            (let ((replacement
                   (appkit-attach-view
                    :app app :id '(room completion-owner)
                    :mode major-mode :parts nil))
                  (key '("g1" . "alice")))
              (disco-company-setup-room-buffer)
              (setq-local disco-company--member-search-requests
                          (make-hash-table :test #'equal))
              (puthash key '(:nonce "new" :status in-flight)
                       disco-company--member-search-requests)
              (setq-local disco-company--pending-member-search
                          '(:nonce "new" :status in-flight))
              ;; A canceled predecessor timer and handler cannot clear the
              ;; replacement view's completion request model.
              (disco-company--member-search-timeout
               (current-buffer) view old-token key "new")
              (funcall old-handler '(:type ready))
              (should (gethash key disco-company--member-search-requests))
              (should disco-company--pending-member-search)
              (should (eq replacement (appkit-current-view))))))
      (appkit-stop-app app))))

(ert-deftest disco-company-automatic-member-search-debounces-prefixes ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (setq-local disco-room--guild-id "g1")
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal))
    (insert "@alice")
    (let (scheduled requests)
      (cl-letf (((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (_guild-id &rest args)
                   (push args requests)
                   t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (when (eq function
                             #'disco-company--run-debounced-member-search)
                     (push (cons function args) scheduled))
                   'fake-timer)))
        (disco-company--schedule-member-search "a")
        (disco-company--schedule-member-search "al")
        (disco-company--schedule-member-search "alice")
        (dolist (call (nreverse scheduled))
          (apply (car call) (cdr call)))
        (should (= 1 (length requests)))
        (should (equal "alice" (plist-get (car requests) :query)))))))

(ert-deftest disco-company-debounced-member-search-drops-stale-token ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (setq-local disco-room--guild-id "g1")
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal))
    (insert "@alice")
    (let (scheduled (requests 0))
      (cl-letf (((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (&rest _args) (cl-incf requests) t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (setq scheduled (cons function args))
                   'fake-timer)))
        (disco-company--schedule-member-search "alice")
        (delete-region (appkit-chatbuf-input-start-position) (point))
        (apply (car scheduled) (cdr scheduled))
        (should (= 0 requests))
        (should-not disco-company--member-search-debounce-timer)
        (should-not disco-company--member-search-debounce-query)))))

(ert-deftest disco-company-member-search-timeout-allows-immediate-retry ()
  (with-temp-buffer
    (setq-local disco-room--guild-id "g1")
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal))
    (let ((requests 0) timeout-call)
      (cl-letf (((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (&rest _args) (cl-incf requests) t))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (when (eq function #'disco-company--member-search-timeout)
                     (setq timeout-call (cons function args)))
                   'fake-timer))
                ((symbol-function 'message) (lambda (&rest _args) nil)))
        (disco-company--maybe-request-guild-members "alice" :explicit t)
        (should disco-company--pending-member-search)
        (apply (car timeout-call) (cdr timeout-call))
        (should-not disco-company--pending-member-search)
        (should-not (gethash '("g1" . "alice")
                             disco-company--member-search-requests))
        (disco-company--maybe-request-guild-members "alice" :explicit t)
        (should (= 2 requests))))))

(ert-deftest disco-company-member-search-uses-user-ids-for-snowflake-query ()
  (with-temp-buffer
    (setq-local disco-room--guild-id "g1")
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal))
    (let (captured)
      (cl-letf (((symbol-function 'disco-gateway-running-p)
                 (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (_guild-id &rest args)
                   (setq captured args)
                   t)))
        (disco-company--maybe-request-guild-members "135683518512345678")
        (should (equal '("135683518512345678")
                       (plist-get captured :user-ids)))
        (should-not (plist-member captured :query))))))

(ert-deftest disco-company-role-capf-inserts-role-mention ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-set-guild-roles
         "g1"
         [((id . "g1") (name . "@everyone"))
          ((id . "role-1") (name . "Admin"))])
        ;; Compact directory refreshes must not erase Gateway role snapshots.
        (disco-state-set-guilds '(((id . "g1") (name . "Compact"))))
        (disco-company-test--complete
         "@adm" "@Admin" "<@&role-1> "))
    (disco-state-reset)))

(ert-deftest disco-company-colliding-user-labels-are-stable-and-all-suffixed ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-apply-guild-members-chunk
         "g1"
         '(((nick . "Same") (user (id . "123450002") (username . "two")))
           ((nick . "Same") (user (id . "123450001") (username . "one")))))
        (let ((disco-room--guild-id "g1"))
          (should
           (equal '("@Same (0001)" "@Same (0002)")
                  (mapcar (lambda (candidate)
                            (plist-get candidate :label))
                          (disco-company--completion-user-candidates))))))
    (disco-state-reset)))

(ert-deftest disco-company-channel-capf-offers-only-viewable-channel-mentions ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-upsert-gateway-channel
         '((id . "channel-visible")
           (guild_id . "g1")
           (type . 0)
           (name . "general")
           (flags . 0)))
        (disco-state-upsert-gateway-channel
         `((id . "channel-obfuscated")
           (guild_id . "g1")
           (type . 0)
           (name . "secret")
           (flags . ,disco-channel-flag-obfuscated)))
        (let ((disco-room--guild-id "g1"))
          (should
           (equal '("#general")
                  (mapcar (lambda (candidate)
                            (plist-get candidate :label))
                          (disco-company--completion-channel-candidates)))))
        (disco-company-test--complete
         "#gen" "#general" "<#channel-visible> "))
    (disco-state-reset)))

(ert-deftest disco-company-custom-emoji-capf-inserts-discord-syntax ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-set-guild-emojis
         "g1"
         '(((id . "101") (name . "dance") (animated . :false))
           ((id . "102") (name . "party") (animated . t))))
        (disco-company-test--complete
         ":dan" ":dance:" "<:dance:101> ")
        (disco-company-test--complete
         ":party:" ":party:" "<a:party:102> "))
    (disco-state-reset)))

(ert-deftest disco-company-custom-emoji-excludes-unavailable-items ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-set-guild-emojis
         "g1"
         '(((id . "101") (name . "usable") (available . t))
           ((id . "102") (name . "gone") (available . :false))))
        (let ((disco-room--guild-id "g1"))
          (should (equal '(":usable:")
                         (mapcar (lambda (candidate)
                                   (plist-get candidate :label))
                                 (disco-company--completion-emoji-candidates))))))
    (disco-state-reset)))

(ert-deftest disco-company-unicode-emoji-capf-inserts-plain-glyph ()
  (let ((candidate
         (appkit-chat-completion-candidate-create
          :label ":rocket:"
          :insert "🚀"
          :prefix "🚀 "
          :value '(:kind unicode-emoji :name "rocket" :emoji "🚀"))))
    (cl-letf (((symbol-function 'appkit-chat-emoji-candidates)
               (lambda (&optional _force) (list candidate))))
      (disco-company-test--complete
       ":rock" ":rocket:" "🚀 "))))

(ert-deftest disco-company-appkit-wrapper-keeps-value-and-lazy-annotation ()
  (let* ((raw '(:label "@Alice"
                :insert "<@1>"
                :kind user
                :user-id "1"
                :username "alice"
                :display-name "Alice"))
         (annotation-count 0)
         shared)
    (cl-letf (((symbol-function
                'disco-company--completion-capf-annotation)
               (lambda (_candidate)
                 (cl-incf annotation-count)
                 " annotation")))
      (setq shared (disco-company--completion-appkit-candidate raw))
      (should (eq raw
                  (appkit-chat-completion-candidate-value shared)))
      (should (member "alice"
                      (appkit-chat-completion-candidate-search-terms shared)))
      (should (= 0 annotation-count))
      (should (equal
               " annotation"
               (funcall
                (appkit-chat-completion-candidate-annotation shared)
                shared)))
      (should (= 1 annotation-count)))))

(ert-deftest disco-company-setup-uses-company-first-shared-order ()
  (with-temp-buffer
    (setq-local disco-room-enable-company-backend nil)
    (setq-local completion-at-point-functions '(ignore))
    (setq-local appkit-chat-completion-functions '(beginning-of-line))
    (disco-company-setup-room-buffer)
    (should (equal '(disco-room-complete-at-point ignore)
                   completion-at-point-functions))
    (should
     (equal '(disco-company--complete-with-company
              appkit-chat-completion-at-point
              beginning-of-line)
            appkit-chat-completion-functions))
    ;; Room mode setup may be rerun; shared hooks stay unique and ordered.
    (disco-company-setup-room-buffer)
    (should (equal '(disco-room-complete-at-point ignore)
                   completion-at-point-functions))
    (should
     (equal '(disco-company--complete-with-company
              appkit-chat-completion-at-point
              beginning-of-line)
            appkit-chat-completion-functions))))

(ert-deftest disco-company-user-avatar-uses-shared-user-api-directly ()
  (let ((candidate
         '(:user-id "123" :username "alice" :global-name "Alice"
           :avatar-hash "avatar-hash" :discriminator "4321"))
        received-user
        received-size)
    (cl-letf (((symbol-function 'disco-avatar-rounded-image)
               (lambda (user pixel-size)
                 (setq received-user user
                       received-size pixel-size)
                 :rounded-avatar)))
      (should (eq :rounded-avatar
                  (disco-company--completion-user-avatar-image candidate 19)))
      (should (= 19 received-size))
      (should (equal "123" (alist-get 'id received-user)))
      (should (equal "avatar-hash" (alist-get 'avatar received-user)))
      (should (equal "4321" (alist-get 'discriminator received-user)))
      (should (equal "alice" (alist-get 'username received-user)))
      (should (equal "Alice" (alist-get 'global_name received-user))))))

(ert-deftest disco-company-user-candidates-preserve-default-avatar-discriminator ()
  (unwind-protect
      (progn
        (disco-state-reset)
        (disco-state-apply-guild-members-chunk
         "g1"
         '(((nick . "Member")
            (user . ((id . "100") (username . "member")
                     (discriminator . "1234"))))))
        (disco-state-put-messages
         "c1"
         '(((id . "m1")
            (author . ((id . "200") (username . "author")
                       (discriminator . "4321"))))))
        (let* ((disco-room--guild-id "g1")
               (disco-room--channel-id "c1")
               (candidates (disco-company--completion-user-candidates))
               (member (seq-find (lambda (candidate)
                                   (equal "100"
                                          (plist-get candidate :user-id)))
                                 candidates))
               (author (seq-find (lambda (candidate)
                                   (equal "200"
                                          (plist-get candidate :user-id)))
                                 candidates)))
          (should (equal "1234" (plist-get member :discriminator)))
          (should (equal "4321" (plist-get author :discriminator)))
          (should (equal "1234"
                         (alist-get
                          'discriminator
                          (disco-company--completion-avatar-user member))))
          (should (equal "4321"
                         (alist-get
                          'discriminator
                          (disco-company--completion-avatar-user author))))))
    (disco-state-reset)))

(provide 'disco-company-test)

;;; disco-company-test.el ends here
