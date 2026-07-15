;;; disco-reset-test.el --- Session privacy reset tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco)

(defun disco-reset-test--make-mode-buffer (name mode secret)
  "Create NAME in MODE and insert SECRET as generated account data."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (funcall mode)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert secret)))
    buffer))

(defun disco-reset-test--attach-view (app buffer id mode)
  "Attach BUFFER to APP using view ID and MODE."
  (with-current-buffer buffer
    (appkit-attach-view
     :app app :id id :mode mode :parts '(content))))

(ert-deftest disco-reset-kills-renamed-legacy-and-fixed-account-projections ()
  (let* ((app (appkit-start-app 'disco :id (make-symbol "privacy-reset")))
         (foreign-app
          (appkit-start-app 'disco :id (make-symbol "foreign-session")))
         (disco-runtime--app app)
         (disco-state-reset-hook
          (list (lambda ()
                  ;; Reentrant session creation must not survive logout even
                  ;; when the same hook subsequently fails.
                  (disco-runtime-app)
                  (error "broken reset hook"))))
         (disco-notifications--seen (make-hash-table :test #'equal))
         (disco-notifications--seen-order '("old-message"))
         (disco-notifications--history (make-ring 4))
         (disco-notifications--history-buffer nil)
         (disco-notifications--last-id 'old-notification)
         (disco-notifications--last-display-op 'old-display)
         (disco-notifications--generation 3)
         (disco-notifications--display-sequence 0)
         (disco-notifications--active-display-op nil)
         (disco-notifications--reset-in-progress nil)
         (disco-notifications--delay-owners nil)
         (disco-notifications--timeout-owners nil)
         (disco-markdown--fontification-buffers
          (make-hash-table :test #'eq))
         (disco-root-debug-log-buffer-name "*disco-root-debug-reset-test*")
         (disco-root--debug-log-buffer nil)
         (disco-root--debug-log-configured-name nil)
         (root-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-reset-root*" 'disco-root-mode "OLD_ACCOUNT_SECRET root"))
         (room-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-reset-room*" 'disco-room-mode "OLD_ACCOUNT_SECRET room"))
         (directory-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-reset-directory*" 'disco-channel-directory-mode
           "OLD_ACCOUNT_SECRET directory"))
         (archived-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-reset-archived*" 'disco-root-archived-threads-mode
           "OLD_ACCOUNT_SECRET archived"))
         (message-inspect-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-reset-message-inspect*" 'disco-msg-inspect-mode
           "OLD_ACCOUNT_SECRET message"))
         (channel-inspect-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-reset-channel-inspect*" 'disco-root-channel-inspect-mode
           "OLD_ACCOUNT_SECRET channel"))
         (tracked-history-buffer
          (disco-reset-test--make-mode-buffer
           " *renamed-disco-history*" 'special-mode
           "OLD_ACCOUNT_SECRET tracked history"))
         (fixed-history-buffer
          (get-buffer-create disco-notifications--history-buffer-name))
         (preview-buffer (get-buffer-create "*disco-room-preview*"))
         (rate-limit-buffer (get-buffer-create "*disco-rate-limit*"))
         (debug-buffer (get-buffer-create disco-root-debug-log-buffer-name))
         (markdown-collision-buffer
          (get-buffer-create
           " *disco-markdown-code-fontification:fundamental-mode*"))
         (markdown-buffer
          (disco-markdown--fontification-buffer 'fundamental-mode))
         (unrelated-buffer
          (disco-reset-test--make-mode-buffer
           " *disco-looking-but-unrelated*" 'special-mode
           "UNRELATED_SENTINEL"))
         (foreign-buffer
          (disco-reset-test--make-mode-buffer
           " *foreign-appkit-view*" 'disco-root-mode
           "FOREIGN_SENTINEL"))
         (account-buffers
          (list root-buffer room-buffer directory-buffer archived-buffer
                message-inspect-buffer channel-inspect-buffer
                tracked-history-buffer fixed-history-buffer
                preview-buffer rate-limit-buffer debug-buffer
                markdown-buffer))
         cleanup-after-failure
         spawned-app
         spawned-buffer
         account-clone
         foreign-clone
         kill-observations
         closed)
    (unwind-protect
        (progn
          (disco-reset-test--attach-view
           app root-buffer '(root main) 'disco-root-mode)
          (disco-reset-test--attach-view
           app room-buffer '(room "old-channel") 'disco-room-mode)
          (setq account-clone
                (with-current-buffer room-buffer
                  (clone-indirect-buffer
                   (generate-new-buffer-name " *current-app-clone*") nil)))
          (push account-clone account-buffers)
          (disco-reset-test--attach-view
           app directory-buffer '(guild "old-guild")
           'disco-channel-directory-mode)
          (disco-reset-test--attach-view
           app archived-buffer '(root archived "old-parent")
           'disco-root-archived-threads-mode)
          (let ((foreign-view
                 (disco-reset-test--attach-view
                  foreign-app foreign-buffer 'foreign-view
                  'disco-root-mode)))
            (setq foreign-clone
                  (with-current-buffer foreign-buffer
                    (clone-indirect-buffer
                     (generate-new-buffer-name " *foreign-app-clone*") nil)))
            ;; A corrupt foreign alias in this app's registry must not broaden
            ;; destructive reset ownership to another Appkit session.
            (puthash 'foreign-alias foreign-view
                     (appkit-app-view-registry app))
            ;; A detached foreign fingerprint remains foreign even though its
            ;; live/raw view pointer can no longer prove ownership.  The
            ;; indirect clone inherits the same persistent identity.
            (appkit-stop-app foreign-app)
            (should-not (appkit-app-live-p foreign-app))
            (should
             (with-current-buffer foreign-buffer appkit--view-fingerprint))
            (should
             (with-current-buffer foreign-clone appkit--view-fingerprint)))
          (with-current-buffer root-buffer
            (rename-buffer
             (generate-new-buffer-name " *renamed-disco-root*"))
            ;; An arbitrary nonlocal exit from a normal kill hook must neither
            ;; retain this projection nor skip later account buffers.  A new
            ;; app/view created by that hook must be drained as well.
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (unless spawned-buffer
                          (setq spawned-app
                                (appkit-start-app
                                 'disco :id (make-symbol "kill-hook-app"))
                                disco-runtime--app spawned-app
                                spawned-buffer
                                (disco-reset-test--make-mode-buffer
                                 " *kill-hook-created-view*"
                                 'disco-root-mode
                                 "OLD_ACCOUNT_SECRET spawned"))
                          (disco-reset-test--attach-view
                           spawned-app spawned-buffer 'spawned-root
                           'disco-root-mode))
                        (throw 'disco-reset-test-escape 'hook-escape))
                      nil t))
          (with-current-buffer room-buffer
            (rename-buffer
             (generate-new-buffer-name " *renamed-disco-room*")))
          (with-current-buffer directory-buffer
            (rename-buffer
             (generate-new-buffer-name " *renamed-disco-directory*"))
            (setq-local kill-buffer-query-functions (list (lambda () nil))))
          (with-current-buffer message-inspect-buffer
            (add-hook
             'kill-buffer-hook
             (lambda ()
               (push (list (null disco-runtime--app)
                           (null disco-state--guilds))
                     kill-observations))
             nil t))
          (with-current-buffer fixed-history-buffer
            (special-mode)
            (setq-local disco-notifications--history-owner-p t)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET fixed history")))
          (with-current-buffer preview-buffer
            (special-mode)
            (setq-local disco-room--preview-buffer-owner-p t)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET preview")))
          (with-current-buffer rate-limit-buffer
            (special-mode)
            (setq-local disco-api--rate-limit-buffer-owner-p t)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET rate limit")))
          (with-current-buffer debug-buffer
            (special-mode)
            (setq-local disco-root--debug-log-owner-p t)
            (let ((inhibit-read-only t))
              (insert "OLD_ACCOUNT_SECRET debug")))
          (with-current-buffer markdown-buffer
            (fundamental-mode)
            (insert "OLD_ACCOUNT_SECRET code block"))
          (with-current-buffer markdown-collision-buffer
            (special-mode)
            (let ((inhibit-read-only t))
              (insert "UNRELATED_MARKDOWN_PREFIX_SENTINEL")))
          (setq disco-notifications--history-buffer tracked-history-buffer
                disco-root--debug-log-buffer debug-buffer
                disco-state--guilds
                '(((id . "old-guild") (name . "OLD_ACCOUNT_SECRET"))))
          (puthash "old-message" t disco-notifications--seen)
          (ring-insert
           disco-notifications--history
           '((id . "old-message") (content . "OLD_ACCOUNT_SECRET")))
          ;; Appkit shutdown must continue after one lifecycle cancellation
          ;; fails, and the outer reset must continue after Appkit reports it.
          (appkit-register-handle
           app 'function (lambda () (setq cleanup-after-failure t)))
          (appkit-register-handle
           app 'function (lambda () (error "broken lifecycle cancellation")))
          (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore)
                    ((symbol-function 'notifications-close-notification)
                     (lambda (id) (push id closed))))
            (should
             (eq (catch 'disco-reset-test-escape
                   (disco-reset-session-state)
                   'completed)
                 'hook-escape))
            ;; The destructive reset is deliberately idempotent.
            (disco-reset-session-state))
          (should cleanup-after-failure)
          (should-not disco-runtime--app)
          (should-not (and spawned-app (appkit-app-live-p spawned-app)))
          (should-not (and spawned-buffer (buffer-live-p spawned-buffer)))
          (should-not disco-state--guilds)
          (should-not disco-notifications--history)
          (should-not disco-notifications--seen-order)
          (should (= 0 (hash-table-count disco-notifications--seen)))
          (should (equal closed '(old-notification)))
          (should (equal kill-observations '((t t))))
          (dolist (buffer account-buffers)
            (ert-info ((format "retained account buffer: %S" buffer))
              (should-not (buffer-live-p buffer))))
          (should (buffer-live-p unrelated-buffer))
          (with-current-buffer unrelated-buffer
            (should (equal (buffer-string) "UNRELATED_SENTINEL")))
          (should (buffer-live-p markdown-collision-buffer))
          (with-current-buffer markdown-collision-buffer
            (should (equal (buffer-string)
                           "UNRELATED_MARKDOWN_PREFIX_SENTINEL")))
          (should (buffer-live-p foreign-buffer))
          (with-current-buffer foreign-buffer
            (should (equal (buffer-string) "FOREIGN_SENTINEL")))
          (should (buffer-live-p foreign-clone))
          (with-current-buffer foreign-clone
            (should (equal (buffer-string) "FOREIGN_SENTINEL"))))
      (dolist (buffer (append (list unrelated-buffer markdown-collision-buffer
                                    foreign-clone
                                    foreign-buffer)
                              (when spawned-buffer (list spawned-buffer))
                              account-buffers))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((kill-buffer-hook nil)
                  (kill-buffer-query-functions nil))
              (kill-buffer buffer)))))
      (when (appkit-app-live-p app)
        (appkit-stop-app app))
      (when (appkit-app-live-p foreign-app)
        (appkit-stop-app foreign-app))
      (when (appkit-app-live-p spawned-app)
        (appkit-stop-app spawned-app)))))

(ert-deftest disco-reset-does-not-own-configurable-debug-name-collision ()
  (let* ((disco-runtime--app nil)
         (disco-state-reset-hook nil)
         (disco-notifications--seen (make-hash-table :test #'equal))
         (disco-notifications--seen-order nil)
         (disco-notifications--history nil)
         (disco-notifications--history-buffer nil)
         (disco-notifications--last-id nil)
         (disco-notifications--last-display-op nil)
         (disco-notifications--generation 0)
         (disco-notifications--display-sequence 0)
         (disco-notifications--active-display-op nil)
         (disco-notifications--reset-in-progress nil)
         (disco-notifications--delay-owners nil)
         (disco-notifications--timeout-owners nil)
         (disco-root--debug-log-buffer nil)
         (disco-root--debug-log-configured-name nil)
         (disco-root-debug-log-enabled t)
         (disco-root-debug-log-buffer-name
          "*scratch-style-unowned-debug-name*")
         (ordinary-buffer
          (get-buffer-create disco-root-debug-log-buffer-name))
         owned-buffer)
    (unwind-protect
        (progn
          (with-current-buffer ordinary-buffer
            (special-mode)
            (let ((inhibit-read-only t))
              (insert "UNRELATED_SCRATCH_SENTINEL")))
          (disco-root--debug-log "OLD_ACCOUNT_SECRET owned debug")
          (setq owned-buffer disco-root--debug-log-buffer)
          (should (buffer-live-p owned-buffer))
          (should-not (eq owned-buffer ordinary-buffer))
          (should
           (buffer-local-value 'disco-root--debug-log-owner-p owned-buffer))
          ;; Ownership is lifecycle state, not presentation-mode state.  It
          ;; must survive a user changing the auxiliary buffer's major mode.
          (with-current-buffer owned-buffer
            (fundamental-mode))
          (should
           (buffer-local-value 'disco-root--debug-log-owner-p owned-buffer))
          (disco-root--debug-log "OLD_ACCOUNT_SECRET second debug")
          (should (eq disco-root--debug-log-buffer owned-buffer))
          (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
            (disco-reset-session-state))
          (should-not (buffer-live-p owned-buffer))
          (should (buffer-live-p ordinary-buffer))
          (with-current-buffer ordinary-buffer
            (should (equal (buffer-string)
                           "UNRELATED_SCRATCH_SENTINEL"))))
      (when (buffer-live-p ordinary-buffer)
        (kill-buffer ordinary-buffer))
      (when (buffer-live-p owned-buffer)
        (kill-buffer owned-buffer)))))

(ert-deftest disco-root-debug-owner-follows-option-not-user-rename ()
  (let* ((first-name (generate-new-buffer-name " *disco-debug-option-a*"))
         (second-name (generate-new-buffer-name " *disco-debug-option-b*"))
         (disco-root-debug-log-enabled t)
         (disco-root-debug-log-buffer-name first-name)
         (disco-root--debug-log-buffer nil)
         (disco-root--debug-log-configured-name nil)
         first-buffer
         second-buffer)
    (unwind-protect
        (progn
          (disco-root--debug-log "first")
          (setq first-buffer disco-root--debug-log-buffer)
          (with-current-buffer first-buffer
            (rename-buffer " *user-renamed-disco-debug*"))
          (disco-root--debug-log "after user rename")
          (should (eq disco-root--debug-log-buffer first-buffer))
          (setq disco-root-debug-log-buffer-name second-name)
          (disco-root--debug-log "after option change")
          (setq second-buffer disco-root--debug-log-buffer)
          (should-not (eq first-buffer second-buffer))
          (should (equal disco-root--debug-log-configured-name second-name))
          (should
           (buffer-local-value 'disco-root--debug-log-owner-p first-buffer))
          (should
           (buffer-local-value 'disco-root--debug-log-owner-p second-buffer))
          (disco-root-reset-debug-log-owner)
          (should-not disco-root--debug-log-buffer)
          (should-not disco-root--debug-log-configured-name))
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest disco-auxiliary-helpers-preserve-fixed-name-collisions ()
  (let* ((disco-notifications--history-buffer-name
          (generate-new-buffer-name " *ordinary-notification-name*"))
         (disco-notifications--history-buffer nil)
         (disco-notifications--history nil)
         (disco-api--rate-limit-buffer-name
          (generate-new-buffer-name " *ordinary-rate-limit-name*"))
         (disco-api--rate-limit-buffer nil)
         (history-collision
          (get-buffer-create disco-notifications--history-buffer-name))
         (rate-limit-collision
          (get-buffer-create disco-api--rate-limit-buffer-name))
         history-owned
         rate-limit-owned)
    (unwind-protect
        (progn
          (with-current-buffer history-collision
            (insert "UNRELATED_HISTORY_SENTINEL"))
          (with-current-buffer rate-limit-collision
            (insert "UNRELATED_RATE_LIMIT_SENTINEL"))
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (disco-notifications-history)
            (disco-api-describe-rate-limits))
          (setq history-owned disco-notifications--history-buffer
                rate-limit-owned disco-api--rate-limit-buffer)
          (should-not (eq history-owned history-collision))
          (should-not (eq rate-limit-owned rate-limit-collision))
          (should
           (buffer-local-value
            'disco-notifications--history-owner-p history-owned))
          (should
           (buffer-local-value
            'disco-api--rate-limit-buffer-owner-p rate-limit-owned))
          (with-current-buffer history-owned
            (rename-buffer " *renamed-owned-notification-history*"))
          (with-current-buffer rate-limit-owned
            (rename-buffer " *renamed-owned-rate-limit*"))
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (disco-notifications-history)
            (disco-api-describe-rate-limits))
          (should (eq history-owned disco-notifications--history-buffer))
          (should (eq rate-limit-owned disco-api--rate-limit-buffer))
          (with-current-buffer history-collision
            (should (equal (buffer-string) "UNRELATED_HISTORY_SENTINEL")))
          (with-current-buffer rate-limit-collision
            (should (equal (buffer-string)
                           "UNRELATED_RATE_LIMIT_SENTINEL"))))
      (dolist (buffer (list history-owned rate-limit-owned
                            history-collision rate-limit-collision))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest disco-reset-owns-stopped-canonical-default-view-and-clone ()
  (let ((disco-runtime--app nil)
        app
        buffer
        clone)
    (unwind-protect
        (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
          (setq app (disco-runtime-app)
                buffer
                (disco-reset-test--make-mode-buffer
                 " *stopped-default-disco-view*" 'disco-root-mode
                 "OLD_ACCOUNT_SECRET stopped default"))
          (disco-reset-test--attach-view
           app buffer '(root stopped-default) 'disco-root-mode)
          (setq clone
                (with-current-buffer buffer
                  (clone-indirect-buffer
                   (generate-new-buffer-name " *stopped-default-clone*") nil)))
          (disco-runtime-stop)
          (should-not disco-runtime--app)
          (should
           (equal (seq-take
                   (buffer-local-value 'appkit--view-fingerprint buffer) 2)
                  '(disco default)))
          (disco-reset-session-state)
          (should-not (buffer-live-p buffer))
          (should-not (buffer-live-p clone)))
      (dolist (candidate (list clone buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (let ((kill-buffer-hook nil))
              (kill-buffer candidate)))))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest disco-reset-force-drains-successor-created-before-throw ()
  (let ((disco-runtime--app nil)
        (hook-count 0)
        buffers)
    (cl-labels
        ((make-successor
          (index)
          (let ((buffer
                 (disco-reset-test--make-mode-buffer
                  (format " *disco-throw-chain-%d*" index)
                  'disco-root-mode
                  "OLD_ACCOUNT_SECRET successor")))
            (push buffer buffers)
            (with-current-buffer buffer
              (add-hook
               'kill-buffer-hook
               (lambda ()
                 (cl-incf hook-count)
                 (when (< hook-count 4)
                   (make-successor hook-count))
                 (throw 'disco-reset-test-chain hook-count))
               nil t))
            buffer)))
      (unwind-protect
          (progn
            (make-successor 0)
            (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
              (should
               (numberp
                (catch 'disco-reset-test-chain
                  (disco-reset-session-state)
                  nil))))
            ;; Three normal projection drain actions each encounter a throw;
            ;; the fresh terminal snapshot must close the fourth buffer with
            ;; hooks disabled instead of starting another successor.
            (should (= 3 hook-count))
            (dolist (buffer buffers)
              (should-not (buffer-live-p buffer))))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((kill-buffer-hook nil))
                (kill-buffer buffer)))))))))

(ert-deftest disco-reset-clears-global-account-caches-after-kill-hooks ()
  (let* ((secret "OLD_ACCOUNT_SECRET")
         (old-url "https://OLD_ACCOUNT_SECRET.invalid/account-resource")
         (disco-runtime--app nil)
         (disco-state-reset-hook nil)
         (disco-notifications--seen (make-hash-table :test #'equal))
         (disco-notifications--seen-order nil)
         (disco-notifications--history nil)
         (disco-notifications--history-buffer nil)
         (disco-notifications--last-id nil)
         (disco-notifications--last-display-op nil)
         (disco-notifications--generation 0)
         (disco-notifications--display-sequence 0)
         (disco-notifications--active-display-op nil)
         (disco-notifications--reset-in-progress nil)
         (disco-notifications--delay-owners nil)
         (disco-notifications--timeout-owners nil)
         (disco-state--guilds nil)
         (disco-markdown--cache (make-hash-table :test #'equal))
         (disco-markdown--fontification-buffers (make-hash-table :test #'eq))
         (disco-preview--pending-by-guild (make-hash-table :test #'equal))
         (disco-preview--requested-message-id-by-channel
          (make-hash-table :test #'equal))
         (disco-preview--in-flight-by-guild (make-hash-table :test #'equal))
         (disco-preview--blocked-until-by-guild
          (make-hash-table :test #'equal))
         (disco-preview--timer nil)
         (disco-preview--timer-deadline nil)
         (disco-preview--pending-private nil)
         (disco-preview--in-flight-private nil)
         (disco-preview--flushing-private nil)
         (disco-preview--generation 5)
         (disco-directory--index-generation 6)
         (disco-directory--guild-generation
          (make-hash-table :test #'equal))
         (disco-directory--guild-status (make-hash-table :test #'equal))
         (disco-directory--parent-thread-state
          (make-hash-table :test #'equal))
         (disco-api--global-rate-limit-until 0.0)
         (disco-api--route-rate-limit-until
          (make-hash-table :test #'equal))
         (disco-api--route-bucket-map (make-hash-table :test #'equal))
         (disco-api--bucket-rate-limit-until
          (make-hash-table :test #'equal))
         (disco-api--generation 6)
         (disco-api--reset-in-progress nil)
         (disco-api--retry-owners nil)
         (disco-api--rate-limit-buffer nil)
         (disco-http--plz-queue nil)
         (disco-http--plz-queue-limit nil)
         (disco-http--generation 7)
         (disco-http--reset-in-progress nil)
         (disco-http--direct-request-owners nil)
         (disco-media--attachment-preview-image-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-preview-fetching
          (make-hash-table :test #'equal))
         (disco-media--attachment-preview-owner-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-download-state-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-download-owner-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-audio-state-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-waveform-image-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-placeholder-image-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-decorated-preview-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-preview-fetch-budget 4)
         (disco-media--attachment-audio-current-process nil)
         (disco-media--attachment-audio-current-owner nil)
         (disco-media--generation 10)
         (disco-media--reset-in-progress nil)
         (disco-room--avatar-image-cache (make-hash-table :test #'equal))
         (disco-room--avatar-fetching (make-hash-table :test #'equal))
         (disco-room--avatar-failures (make-hash-table :test #'equal))
         (disco-room--avatar-pending-invalidations
          (make-hash-table :test #'equal))
         (disco-room--avatar-round-image-cache
          (make-hash-table :test #'equal))
         (disco-room--forward-guild-icon-image-cache
          (make-hash-table :test #'equal))
         (disco-room--forward-guild-icon-fetching
          (make-hash-table :test #'equal))
         (disco-room--avatar-fetch-generation 20)
         (disco-room--forward-guild-icon-fetch-generation 30)
         (disco-room--session-cache-reset-in-progress nil)
         (disco-room--avatar-retry-timer nil)
         (disco-room--avatar-invalidation-timer nil)
         (disco-room--avatar-plz-queue nil)
         (disco-room--avatar-plz-queue-limit nil)
         (disco-room--avatar-fetch-budget 2)
         (disco-room-draft-history-search-history (list secret))
         (disco-room-search-inplace-history (list old-url))
         (disco-root--guild-icon-image-cache
          (make-hash-table :test #'equal))
         (disco-root--guild-icon-fetching
          (make-hash-table :test #'equal))
         (disco-root--extra-info-provider-error-cache
          (make-hash-table :test #'eq))
         (disco-root--guild-icon-fetch-generation 40)
         (disco-root--session-cache-reset-in-progress nil)
         (disco-root-search-history (list secret old-url))
         (disco-root--debug-log-buffer nil)
         (disco-root--debug-log-configured-name nil)
         (disco-company--rounded-avatar-cache
          (make-hash-table :test #'equal))
         (redraw-count 0)
         (buffer (generate-new-buffer " *disco-cache-reset-reentry*"))
         late-buffer
         late-app
         late-timer
         cleared-queue
         (tables
          (list disco-notifications--seen
                disco-markdown--cache
                disco-preview--pending-by-guild
                disco-preview--requested-message-id-by-channel
                disco-preview--in-flight-by-guild
                disco-preview--blocked-until-by-guild
                disco-directory--guild-generation
                disco-directory--guild-status
                disco-directory--parent-thread-state
                disco-api--route-rate-limit-until
                disco-api--route-bucket-map
                disco-api--bucket-rate-limit-until
                disco-media--attachment-preview-image-cache
                disco-media--attachment-preview-fetching
                disco-media--attachment-preview-owner-table
                disco-media--attachment-download-state-table
                disco-media--attachment-download-owner-table
                disco-media--attachment-audio-state-table
                disco-media--attachment-waveform-image-cache
                disco-media--attachment-placeholder-image-cache
                disco-media--attachment-decorated-preview-cache
                disco-room--avatar-image-cache
                disco-room--avatar-fetching
                disco-room--avatar-failures
                disco-room--avatar-pending-invalidations
                disco-room--avatar-round-image-cache
                disco-room--forward-guild-icon-image-cache
                disco-room--forward-guild-icon-fetching
                disco-root--guild-icon-image-cache
                disco-root--guild-icon-fetching
                disco-root--extra-info-provider-error-cache
                disco-company--rounded-avatar-cache)))
    (unwind-protect
        (progn
          (puthash old-url secret disco-markdown--cache)
          (puthash old-url secret
                   disco-media--attachment-preview-image-cache)
          (puthash old-url t disco-media--attachment-preview-fetching)
          (puthash old-url (list :generation 10)
                   disco-media--attachment-preview-owner-table)
          (let ((owner (list :generation 10 :key old-url)))
            (puthash old-url owner
                     disco-media--attachment-download-owner-table)
            (puthash old-url (list :owner owner :path old-url :error secret)
                     disco-media--attachment-download-state-table))
          (puthash old-url (list :source old-url :error secret)
                   disco-media--attachment-audio-state-table)
          (dolist (table (list disco-media--attachment-waveform-image-cache
                               disco-media--attachment-placeholder-image-cache
                               disco-media--attachment-decorated-preview-cache))
            (puthash old-url secret table))
          (puthash old-url secret disco-room--avatar-image-cache)
          (puthash old-url t disco-room--avatar-fetching)
          (puthash old-url (list :url old-url :reason secret)
                   disco-room--avatar-failures)
          (puthash old-url t disco-room--avatar-pending-invalidations)
          (puthash old-url secret disco-room--avatar-round-image-cache)
          (puthash old-url secret
                   disco-room--forward-guild-icon-image-cache)
          (puthash old-url (list :generation 30 :process nil)
                   disco-room--forward-guild-icon-fetching)
          (puthash old-url secret disco-root--guild-icon-image-cache)
          (puthash old-url (list :generation 40 :process nil)
                   disco-root--guild-icon-fetching)
          (puthash 'old-provider secret
                   disco-root--extra-info-provider-error-cache)
          (puthash old-url secret disco-company--rounded-avatar-cache)
          (with-current-buffer buffer
            (special-mode)
            (setq-local disco-room--preview-buffer-owner-p t)
            (let ((inhibit-read-only t))
              (insert secret))
            ;; These writes happen after the early cache sweep.  The final
            ;; sweep must remove them without invoking any redraw path.
            (add-hook
             'kill-buffer-hook
             (lambda ()
               ;; This queue is born after the early HTTP reset.  Its
               ;; synchronous terminal cancellation below is the final
               ;; callback boundary before hooks-disabled projection cleanup.
               (setq disco-http--plz-queue 'late-http-queue
                     disco-http--plz-queue-limit 4)
               (puthash old-url secret disco-markdown--cache)
               (puthash old-url (list :error secret)
                        disco-media--attachment-download-state-table)
               (puthash old-url (list :url old-url :reason secret)
                        disco-room--avatar-failures)
               (puthash old-url secret disco-root--guild-icon-image-cache)
               (puthash old-url secret disco-company--rounded-avatar-cache))
             nil t))
          (let ((disco-media-rerender-function
                 (lambda (&rest _) (cl-incf redraw-count)))
                (disco-root-view-queue-live-update-function
                 (lambda (&rest _) (cl-incf redraw-count))))
            (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore)
                      ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
                      ((symbol-function 'appkit-media-cancel-video-preview)
                       #'ignore)
                      ((symbol-function
                        'appkit-media-clear-video-decoration-cache)
                       #'ignore)
                      ((symbol-function
                        'disco-room--sync-resource-changes-in-open-rooms)
                       (lambda (&rest _) (cl-incf redraw-count)))
                      ((symbol-function 'disco-room--refresh-open-rooms)
                       (lambda (&rest _) (cl-incf redraw-count)))
                      ((symbol-function 'disco-root--rerender-open-root-buffers)
                       (lambda (&rest _) (cl-incf redraw-count)))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (&rest _) (cl-incf redraw-count)))
                      ((symbol-function 'plz-clear)
                       (lambda (queue)
                         (setq cleared-queue queue
                               late-buffer
                               (generate-new-buffer
                                " *disco-terminal-callback-view*"))
                         (with-current-buffer late-buffer
                           (disco-root-mode)
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (insert secret)))
                         (setq late-app
                               (appkit-start-app
                                'disco :id (make-symbol "late-http-app"))
                               disco-runtime--app late-app)
                         (appkit-register-handle
                          late-app 'function
                          (lambda ()
                            (setq late-timer
                                  (run-at-time 3600 nil #'ignore)
                                  disco-preview--timer late-timer
                                  disco-preview--timer-deadline
                                  (+ (float-time) 3600))
                            (puthash old-url secret
                                     disco-preview--pending-by-guild)))
                         (disco-reset-test--attach-view
                          late-app late-buffer 'late-root 'disco-root-mode)
                         ;; Repopulate every adapter family after its hookful
                         ;; cache cancellation has run.  Only the pure terminal
                         ;; sweep is allowed to remove these writes.
                         (setq disco-state--guilds
                               `(((id . ,secret) (name . ,secret)))
                               disco-notifications--seen-order (list secret)
                               disco-notifications--history (list secret)
                               disco-api--global-rate-limit-until secret
                               disco-http--plz-queue 'resurrected-queue
                               disco-http--plz-queue-limit secret)
                         (dolist (table tables)
                           (puthash old-url secret table))
                         queue)))
              (disco-reset-session-state)))
          (should-not (buffer-live-p buffer))
          (should-not (buffer-live-p late-buffer))
          (should-not (appkit-app-live-p late-app))
          (should-not disco-runtime--app)
          (should (eq cleared-queue 'late-http-queue))
          (dolist (table tables)
            (should (= 0 (hash-table-count table))))
          (should-not disco-state--guilds)
          (should-not disco-notifications--seen-order)
          (should-not disco-notifications--history)
          (should (equal disco-api--global-rate-limit-until 0.0))
          (should-not disco-http--plz-queue)
          (should-not disco-http--plz-queue-limit)
          (should-not disco-http--direct-request-owners)
          (should-not disco-preview--timer)
          (should-not disco-preview--timer-deadline)
          (should-not disco-media--attachment-preview-fetch-budget)
          (should-not disco-media--attachment-audio-current-process)
          (should-not disco-media--attachment-audio-current-owner)
          (should-not disco-room--avatar-fetch-budget)
          (should-not disco-room-draft-history-search-history)
          (should-not disco-room-search-inplace-history)
          (should-not disco-root-search-history)
          (should (= 0 redraw-count))
          (should-not
           (string-match-p
            secret
            (prin1-to-string
             (list tables
                   disco-room-draft-history-search-history
                   disco-room-search-inplace-history
                   disco-root-search-history
                   disco-state--guilds
                   disco-notifications--seen-order
                   disco-notifications--history
                   disco-api--global-rate-limit-until
                   disco-http--plz-queue
                   disco-http--plz-queue-limit)))))
      (dolist (candidate (list late-buffer buffer))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (let ((kill-buffer-hook nil))
              (kill-buffer candidate)))))
      (when (appkit-app-live-p late-app)
        (appkit-stop-app late-app))
      (when (timerp late-timer)
        (cancel-timer late-timer)))))

(provide 'disco-reset-test)

;;; disco-reset-test.el ends here
