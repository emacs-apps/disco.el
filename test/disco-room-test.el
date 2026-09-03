;;; disco-room-test.el --- Tests for disco-room pin ack flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-room)
(require 'disco-state)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest disco-room-pinned-messages-frame-only-skips-render ()
  (let ((invalidations (appkit-invalidations-create)))
    (setf (appkit-invalidations-parts invalidations) '(frame))
    (cl-letf (((symbol-function 'disco-room-pinned-messages--list-spec)
               (lambda ()
                 (ert-fail "frame-only sync rendered pinned messages"))))
      (disco-room-pinned-messages--sync-invalidations
       'unused invalidations nil))))

(ert-deftest disco-room-mode-is-not-special-mode ()
  (with-temp-buffer
    (disco-room-mode)
    (should-not (derived-mode-p 'special-mode))))

(ert-deftest disco-room-mode-delegates-wrap-state-to-appkit ()
  (with-temp-buffer
    (disco-room-mode)
    (should appkit-chatbuf-owns-wrap-prefix-p)
    (should appkit-chatbuf-wrap-long-lines)
    (should visual-line-mode)
    (should word-wrap)
    (should-not truncate-lines)
    (cl-letf (((symbol-function 'message) #'ignore))
      (disco-room-toggle-breakline)
      (should-not appkit-chatbuf-wrap-long-lines)
      (should-not visual-line-mode)
      (should-not word-wrap)
      (should truncate-lines)
      (disco-room-toggle-breakline)
      (should appkit-chatbuf-wrap-long-lines)
      (should visual-line-mode)
      (should word-wrap)
      (should-not truncate-lines))))

(ert-deftest disco-room-fill-width-uses-standard-visual-fill-setting ()
  (with-temp-buffer
    (setq-local visual-fill-column-mode t)
    (setq-local visual-fill-column-width 72)
    (setq-local fill-column 66)
    (should (= 72 (disco-room--line-fill-column)))
    (setq-local visual-fill-column-width nil)
    (should (= 66 (disco-room--line-fill-column)))))

(ert-deftest disco-room-open-resets-replacement-view-state-but-reuses-live-state ()
  (let ((disco-runtime--app nil)
        (channel-id "room-replacement")
        (channel-name "replacement")
        buffer
        old-view
        history-owner
        (refreshes 0))
    (disco-state-reset)
    (disco-state-upsert-channel
     `((id . ,channel-id)
       (name . ,channel-name)
       (type . 0)
       (guild_id . "g-replacement")
       (permissions . "2048")))
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buf &rest _args) (setq buffer buf)))
              ((symbol-function 'disco-room--attach-live-updates) #'ignore)
              ((symbol-function 'disco-room-refresh)
               (lambda () (cl-incf refreshes)))
              ((symbol-function 'appkit-view-refresh-responsive-geometry)
               #'ignore)
              ((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (progn
            (disco-room-open channel-id channel-name)
            (should (buffer-live-p buffer))
            (should (= 1 refreshes))
            (with-current-buffer buffer
              (setq old-view (appkit-current-view))
              (appkit-chatbuf-input-state-set "surviving draft")
              (appkit-chatbuf-input-history-push "old history")
              (appkit-chat-history-window-set "100" "200")
              (setq history-owner
                    (appkit-chat-history-request-start old-view 'older))
              (setq-local disco-room--pending-reply-to "reply-old"
                          disco-room--pending-edit '(:type edit :message-id "edit-old")
                          disco-room--pending-jump-message-id "jump-old"
                          disco-room--send-in-flight t
                          disco-room--remote-latest-message-id "remote-old"
                          disco-room--filter-generation 7
                          disco-room--filter-in-flight t
                          disco-room--msg-filter '(:active t :query "old")
                          disco-room--inplace-search-generation 9
                          disco-room--inplace-search-filter '(:query "old")
                          disco-room--pending-attachments '((:path "old"))
                          disco-room--optimistic-read-ack-seq 4
                          disco-room--pending-optimistic-read-ack '(:seq 4)
                          disco-room--poll-vote-op-seq 5
                          disco-room--reaction-op-seq 6
                          disco-room--pins-ack-seq 7)
              (puthash "poll-old" '(1)
                       disco-room--poll-selection-drafts)
              (puthash "poll-old" '(:token 5 :target (1))
                       disco-room--poll-vote-ops)
              (puthash '("message-old" (name . "wave"))
                       '(:token 6 :addp t)
                       disco-room--reaction-ops))
            ;; SETUP is not run for a still-live view, so reopening preserves
            ;; controller, composer, and history ownership.
            (disco-room-open channel-id channel-name)
            (should (= 1 refreshes))
            (with-current-buffer buffer
              (should (eq old-view (appkit-current-view)))
              (should (eq history-owner
                          (appkit-chat-history-request-owner)))
              (should (equal "surviving draft" (disco-room--current-draft)))
              (should (= 7 disco-room--filter-generation))
              (should disco-room--send-in-flight))
            (appkit-kill-view old-view)
            (should-not (appkit-view-live-p old-view))
            ;; The same major-mode buffer survives, but the new Appkit view gets
            ;; fresh ownership instead of inheriting the dead predecessor.
            (disco-room-open channel-id channel-name)
            (should (= 2 refreshes))
            (with-current-buffer buffer
              (let ((replacement (appkit-current-view)))
                (should (appkit-view-live-p replacement))
                (should-not (eq old-view replacement))
                (should (equal channel-id disco-room--channel-id))
                (should (equal "g-replacement" disco-room--guild-id))
                (should (equal "" (disco-room--current-draft)))
                (should-not (appkit-chat-history-window-known-p))
                (should-not (appkit-chat-history-loading-p))
                (should-not (appkit-chat-history-request-owner))
                (should-not disco-room--pending-reply-to)
                (should-not disco-room--pending-edit)
                (should-not disco-room--pending-jump-message-id)
                (should-not disco-room--send-in-flight)
                (should-not disco-room--remote-latest-message-id)
                (should (= 0 disco-room--filter-generation))
                (should-not disco-room--filter-in-flight)
                (should-not disco-room--msg-filter)
                (should (= 0 disco-room--inplace-search-generation))
                (should-not disco-room--inplace-search-filter)
                (should-not disco-room--pending-attachments)
                (should (= 0 (hash-table-count
                              disco-room--poll-selection-drafts)))
                (should (= 0 disco-room--poll-vote-op-seq))
                (should (= 0 (hash-table-count disco-room--poll-vote-ops)))
                (should (= 0 disco-room--reaction-op-seq))
                (should (= 0 (hash-table-count disco-room--reaction-ops)))
                (should (= 0 disco-room--optimistic-read-ack-seq))
                (should-not disco-room--pending-optimistic-read-ack)
                (should (= 0 disco-room--pins-ack-seq))
                (should (= 0 (hash-table-count
                              (appkit-view-request-table replacement)))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (disco-runtime-stop)))))

(ert-deftest disco-room-cross-channel-jump-uses-renamed-view-buffer ()
  (let ((disco-runtime--app nil)
        target-buffer
        queued
        synced
        (renamed-name (generate-new-buffer-name "*disco-renamed-target*")))
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args) buffer))
              ((symbol-function 'disco-room--attach-live-updates) #'ignore)
              ((symbol-function 'disco-room-refresh) #'ignore)
              ((symbol-function 'appkit-view-refresh-responsive-geometry)
               #'ignore)
              ((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (progn
            (disco-state-reset)
            (disco-state-upsert-channel
             '((id . "jump-source") (type . 1) (name . "source")))
            (disco-state-upsert-channel
             '((id . "jump-target") (type . 1) (name . "target")))
            (setq target-buffer (disco-room-open "jump-target" "target"))
            (should (buffer-live-p target-buffer))
            (with-current-buffer target-buffer
              (rename-buffer renamed-name t))
            ;; Reopening the Appkit identity returns the actual reused buffer,
            ;; independent of its display name.
            (should (eq target-buffer
                        (disco-room-open "jump-target" "target")))
            (with-temp-buffer
              (disco-room-mode)
              (setq-local disco-room--channel-id "jump-source")
              (cl-letf (((symbol-function 'disco-room--queue-jump)
                         (lambda (message-id view)
                           (setq queued
                                 (list (current-buffer) message-id view))))
                        ((symbol-function 'appkit-sync-invalidations)
                         (lambda (view) (setq synced view))))
                (disco-room-jump-to-message "message-42" "jump-target")))
            (should (eq target-buffer (nth 0 queued)))
            (should (equal "message-42" (nth 1 queued)))
            (should (eq (nth 2 queued) synced))
            (should (equal renamed-name (buffer-name target-buffer))))
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer))
        (disco-runtime-stop)))))

(ert-deftest disco-room-history-callback-cannot-land-in-replacement-view ()
  (let ((disco-runtime--app nil)
        success-callback)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "callback-view")
            (let ((old-view (disco-room--ensure-view)))
              (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                         (lambda (_channel-id &rest args)
                           (setq success-callback
                                 (plist-get args :on-success))))
                        ((symbol-function 'disco-room--update-frame) #'ignore)
                        ((symbol-function 'message) #'ignore))
                (disco-room-refresh))
              (should (functionp success-callback))
              (appkit-kill-view old-view)
              (let ((replacement (disco-room--ensure-view)))
                (should-not (eq old-view replacement))
                ;; Buffer, channel, generation, and history owner still look
                ;; compatible; originating view identity is the decisive guard.
                (funcall success-callback
                         '(((id . "200")
                            (channel_id . "callback-view")
                            (content . "stale"))))
                (should-not (disco-state-messages "callback-view")))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-gateway-hook-and-watch-are-view-owned-and-idempotent ()
  (let ((disco-runtime--app nil)
        (watch-count 0)
        (unwatch-count 0)
        (requests 0))
    (cl-letf (((symbol-function 'disco-gateway-watch-channel)
               (lambda (_channel-id) (cl-incf watch-count)))
              ((symbol-function 'disco-gateway-unwatch-channel)
               (lambda (_channel-id) (cl-incf unwatch-count)))
              ((symbol-function 'disco-gateway-stop) #'ignore)
              ((symbol-function 'appkit-request-sync)
               (lambda (&rest _args) (cl-incf requests)))
              ((symbol-function 'appkit-invalidate)
               (lambda (&rest _args)
                 (ert-fail "gateway callback invalidated and scheduled separately")))
              ((symbol-function 'appkit-schedule-sync)
               (lambda (&rest _args)
                 (ert-fail "gateway callback scheduled separately"))))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "gateway-owner")
            (let* ((view (disco-room--attach-live-updates))
                   (handler disco-room--gateway-handler)
                   (handle disco-room--live-update-handle))
              (should (= 1 watch-count))
              (should (memq handler disco-gateway-event-hook))
              (should (appkit-handle-alive-p handle))
              (should (eq view (appkit-handle-owner handle)))
              (should (eq view (disco-room--attach-live-updates)))
              (should (= 1 watch-count))
              (should (eq handle disco-room--live-update-handle))
              (funcall handler '(:type channel-update :channel-id "gateway-owner"))
              (should (= 1 requests))
              (appkit-kill-view view)
              (should-not (memq handler disco-gateway-event-hook))
              (should-not (appkit-handle-alive-p handle))
              (should-not disco-room--gateway-handler)
              (should-not disco-room--live-update-handle)
              (should (= 1 unwatch-count))
              (disco-room--detach-live-updates)
              (disco-room--detach-live-updates)
              (should (= 1 unwatch-count))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-async-refresh-callback-only-requests-appkit-sync ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "refresh-boundary")
            (disco-room--ensure-view)
            (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                       (lambda (_channel-id &rest args)
                         (setq callback (plist-get args :on-success))))
                      ((symbol-function 'disco-room--update-frame) #'ignore)
                      ((symbol-function 'disco-room--mark-read) #'ignore)
                      ((symbol-function 'disco-room-render)
                       (lambda ()
                         (ert-fail "async callback rendered directly")))
                      ((symbol-function 'appkit-sync-invalidations)
                       (lambda (&rest _args)
                         (ert-fail "async callback synced directly")))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (view &rest args)
                         (push (cons view args) requests)))
                      ((symbol-function 'message) #'ignore))
              (disco-room-refresh)
              ;; Ignore the explicit request-start loading invalidation; this
              ;; assertion is about the transport callback boundary itself.
              (setq requests nil)
              (funcall callback
                       '(((id . "300")
                          (channel_id . "refresh-boundary")
                          (content . "fresh"))))
              (should (= 1 (length requests)))
              (should (plist-get (cdar requests) :structure))
              (should (equal '(frame timeline composer)
                             (plist-get (cdar requests) :parts)))
              (should (equal "300"
                             (alist-get 'id
                                        (car (disco-state-messages
                                              "refresh-boundary")))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-typing-callbacks-never-project-directly ()
  (let ((disco-runtime--app nil)
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "typing-boundary")
            (let ((view (disco-room--ensure-view)))
              (puthash "expired"
                       (list :user-id "expired"
                             :display-name "Expired"
                             :expires-at (- (float-time) 1)
                             :updated-at (- (float-time) 2))
                       disco-room--typing-users)
              (cl-letf (((symbol-function 'disco-room--update-frame)
                         (lambda (&rest _args)
                           (ert-fail "typing callback updated frame directly")))
                        ((symbol-function 'disco-room-render)
                         (lambda ()
                           (ert-fail "typing callback rendered directly")))
                        ((symbol-function 'disco-room--sync-timeline)
                         (lambda (&rest _args)
                           (ert-fail "typing callback projected timeline directly")))
                        ((symbol-function 'appkit-sync-invalidations)
                         (lambda (&rest _args)
                           (ert-fail "typing callback synced directly")))
                        ((symbol-function 'disco-room--typing-reschedule-expire-timer)
                         #'ignore)
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest args)
                           (push (cons owner args) requests))))
                (disco-room--typing-expire-timer-callback
                 (current-buffer) view)
                (should-not (gethash "expired" disco-room--typing-users))
                (should (= 1 (length requests)))
                (should (eq view (caar requests)))
                (should (eq 'frame (plist-get (cdar requests) :part)))
                ;; Track/stop run while a gateway event is already being
                ;; consumed by Appkit sync, so they remain controller-only.
                (should (disco-room--typing-track-user
                         "active" nil (float-time)))
                (should (gethash "active" disco-room--typing-users))
                (should (disco-room--typing-stop-user "active"))
                (should-not (gethash "active" disco-room--typing-users)))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-post-command-spoiler-hide-only-requests-entry-sync ()
  (let ((disco-runtime--app nil)
        request)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "post-command-boundary")
            (let ((view (disco-room--ensure-view)))
              (setq-local disco-room--revealed-spoiler-message-id "m1")
              (cl-letf (((symbol-function 'disco-room--maybe-auto-load-newer)
                         #'ignore)
                        ((symbol-function 'disco-room--maybe-auto-load-older)
                         #'ignore)
                        ((symbol-function 'disco-room--invalidate-message-node)
                         (lambda (&rest _args)
                           (ert-fail "post-command hook invalidated a row directly")))
                        ((symbol-function 'disco-room--update-frame)
                         (lambda (&rest _args)
                           (ert-fail "post-command hook updated frame directly")))
                        ((symbol-function 'disco-room-render)
                         (lambda ()
                           (ert-fail "post-command hook rendered directly")))
                        ((symbol-function 'appkit-sync-invalidations)
                         (lambda (&rest _args)
                           (ert-fail "post-command hook synced directly")))
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest args)
                           (setq request (cons owner args)))))
                (disco-room--post-command)
                (should-not disco-room--revealed-spoiler-message-id)
                (should (eq view (car request)))
                (should (equal "m1" (plist-get (cdr request) :entry))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-reaction-reader-uses-shared-visual-catalog ()
  (let* ((rocket
          (appkit-chat-completion-candidate-create
           :label ":rocket:"
           :insert "🚀"
           :prefix "🚀 "
           :search-terms '("rocket")
           :group "Unicode"
           :value '(:kind unicode-emoji)))
         (dance
          (appkit-chat-completion-candidate-create
           :label ":dance:"
           :insert "<:dance:101>"
           :search-terms '("dance" "101")
           :group "This Server · Home"
           :value '(:kind emoji)))
         (candidates (list rocket dance))
         (reads 0))
    (cl-letf (((symbol-function 'disco-company-reaction-candidates)
               (lambda (&optional _message _own-only) candidates))
              ((symbol-function 'completing-read)
               (lambda (_prompt table _predicate _require _initial _history
				default)
                 (cl-incf reads)
                 (let ((group-function
                        (completion-metadata-get
                         (completion-metadata "" table nil)
                         'group-function)))
                   (if (= reads 1)
                       (progn
                         (should default)
                         (should
                          (string-prefix-p
                           ":rocket:" (substring-no-properties default)))
                         (should (equal "Unicode"
                                        (funcall group-function default nil)))
                         default)
                     (let* ((matches
                             (completion-all-completions
                              "dance" table nil 5))
                            (match (car matches)))
                       (should (equal (cdr matches) 0))
                       (should (equal "This Server · Home"
                                      (funcall group-function match nil)))
                       match))))))
      (should
       (equal "🚀"
              (disco-room--read-reaction-emoji
               "Add reaction" "🚀")))
      (should
       (equal "<:dance:101>"
              (disco-room--read-reaction-emoji "Add reaction")))
      (should (= reads 2)))))

(ert-deftest disco-room-reaction-default-normalizes-search-terms ()
  (let ((grouped
         (appkit-chat-completion-candidate-create
          :label ":alien:"
          :insert "👽"
          :group "Unicode · Smileys"))
        (alias
         (appkit-chat-completion-candidate-create
          :label ":thumbs_up:"
          :insert "👍🏻"
          :search-terms "👍"
          :group "Unicode · Body")))
    (should
     (eq alias
         (disco-room--reaction-default-candidate
          (list grouped alias) "👍")))))

(ert-deftest disco-room-reaction-reader-keeps-text-fallback-without-catalog ()
  (cl-letf (((symbol-function 'disco-company-reaction-candidates) #'ignore)
            ((symbol-function 'read-string)
             (lambda (&rest _) "")))
    (should
     (equal "👍"
            (disco-room--read-reaction-emoji "Add reaction" "👍")))))

(ert-deftest disco-room-remove-reaction-picker-offers-only-own-identities ()
  (let* ((msg
          '((id . "m1")
            (reactions
             . (((emoji . ((id . nil) (name . "🔥"))) (me . :false))
                ((emoji . ((id . "42") (name . "mine"))) (me . t))))))
         seen-message
         seen-own-only
         removed)
    (cl-letf (((symbol-function 'disco-room--reaction-unavailable-reason)
               (lambda (&optional _msg) nil))
              ((symbol-function 'disco-room--read-reaction-emoji)
               (lambda (_prompt _default message own-only)
                 (setq seen-message message
                       seen-own-only own-only)
                 "<:mine:42>"))
              ((symbol-function 'disco-room-remove-reaction)
               (lambda (emoji message-id)
                 (setq removed (list emoji message-id)))))
      (disco-room--remove-reaction-from-msg msg)
      (should seen-own-only)
      (should (eq msg seen-message))
      (should (equal '("<:mine:42>" "m1") removed)))))

(ert-deftest disco-room-remove-reaction-picker-errors-without-own-reaction ()
  (cl-letf (((symbol-function 'disco-room--reaction-unavailable-reason)
             (lambda (&optional _msg) nil)))
    (should-error
     (disco-room--remove-reaction-from-msg
      '((id . "m1")
        (reactions
         . (((emoji . ((id . nil) (name . "🔥"))) (me . :false))))))
     :type 'user-error)))

(ert-deftest disco-room-reaction-callback-only-requests-entry-sync ()
  (let ((disco-runtime--app nil)
        callback
        request)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "reaction-boundary")
            (disco-state-put-messages
             "reaction-boundary"
             '(((id . "m1")
                (channel_id . "reaction-boundary")
                (content . "hello"))))
            (let ((view (disco-room--ensure-view)))
              (cl-letf (((symbol-function 'disco-room--reaction-unavailable-reason)
                         (lambda (&optional _msg) nil))
                        ((symbol-function 'disco-api-add-reaction-async)
                         (lambda (_channel-id _message-id _emoji &rest args)
                           (setq callback (plist-get args :on-success)))))
                (disco-room-add-reaction "wave" "m1"))
              (should (functionp callback))
              (cl-letf (((symbol-function 'disco-room--update-frame)
                         (lambda (&rest _args)
                           (ert-fail "reaction callback updated frame directly")))
                        ((symbol-function 'disco-room-render)
                         (lambda ()
                           (ert-fail "reaction callback rendered directly")))
                        ((symbol-function 'disco-room--sync-timeline)
                         (lambda (&rest _args)
                           (ert-fail "reaction callback projected timeline directly")))
                        ((symbol-function 'appkit-sync-invalidations)
                         (lambda (&rest _args)
                           (ert-fail "reaction callback synced directly")))
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest args)
                           (setq request (cons owner args))))
                        ((symbol-function 'message) #'ignore))
                (funcall callback nil)
                (should (eq view (car request)))
                (should (equal "m1" (plist-get (cdr request) :entry)))
                (should (equal "wave"
                               (disco-msg-reaction-emoji
                                (car (disco-msg-reactions
                                      (car (disco-state-messages
                                            "reaction-boundary"))))))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-reaction-rest-success-and-self-echo-are-idempotent ()
  (let ((disco-runtime--app nil)
        success-callback)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "reaction-echo")
            (disco-state-put-messages
             "reaction-echo"
             '(((id . "m1")
                (channel_id . "reaction-echo")
                (content . "hello"))))
            (disco-room--ensure-view)
            (cl-letf (((symbol-function 'disco-room--reaction-unavailable-reason)
                       (lambda (&optional _msg) nil))
                      ((symbol-function 'disco-api-add-reaction-async)
                       (lambda (_channel-id _message-id _emoji &rest args)
                         (setq success-callback
                               (plist-get args :on-success))))
                      ((symbol-function 'disco-gateway-current-user-id)
                       (lambda () "self"))
                      ((symbol-function 'message) #'ignore))
              (disco-room-add-reaction "oldname:42" "m1")
              (funcall success-callback nil)
              ;; Gateway may report a renamed custom emoji.  Its id owns the
              ;; operation and the self echo must not increment count twice.
              (disco-room--apply-live-reaction-event
               '(:type message-reaction-add
                       :message-id "m1"
                       :user-id "self"
                       :emoji ((id . "42") (name . "renamed")))))
            (let* ((message (disco-room--message-by-id "m1"))
                   (reaction (car (disco-msg-reactions message))))
              (should (= 1 (disco-msg-reaction-count reaction)))
              (should (disco-msg-reaction-selected-p reaction))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-reaction-self-echo-before-rest-completion-is-authoritative ()
  (let ((disco-runtime--app nil)
        success-callback
        (requests 0))
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "reaction-echo-first")
            (disco-state-put-messages
             "reaction-echo-first"
             '(((id . "m1")
                (channel_id . "reaction-echo-first")
                (content . "hello"))))
            (disco-room--ensure-view)
            (cl-letf (((symbol-function 'disco-room--reaction-unavailable-reason)
                       (lambda (&optional _msg) nil))
                      ((symbol-function 'disco-api-add-reaction-async)
                       (lambda (_channel-id _message-id _emoji &rest args)
                         (setq success-callback
                               (plist-get args :on-success))))
                      ((symbol-function 'disco-gateway-current-user-id)
                       (lambda () "self"))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (&rest _args) (cl-incf requests)))
                      ((symbol-function 'message) #'ignore))
              (disco-room-add-reaction "wave" "m1")
              (disco-room--apply-live-reaction-event
               '(:type message-reaction-add
                       :message-id "m1"
                       :user-id "self"
                       :emoji ((name . "wave"))))
              ;; The echo retired the request owner.  Its later REST success
              ;; cannot mutate or schedule presentation again.
              (funcall success-callback nil))
            (let* ((message (disco-room--message-by-id "m1"))
                   (reaction (car (disco-msg-reactions message))))
              (should (= 1 (disco-msg-reaction-count reaction)))
              (should (disco-msg-reaction-selected-p reaction))
              (should (= 0 requests))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-queued-reaction-echo-keeps-frozen-self-identity ()
  (let ((disco-runtime--app nil)
        (disco-gateway--current-user-id "self")
        emitted)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "reaction-queued-self")
            ;; This is the state after the matching REST success.
            (disco-state-put-messages
             "reaction-queued-self"
             '(((id . "m1")
                (channel_id . "reaction-queued-self")
                (reactions . (((count . 1)
                               (me . t)
                               (emoji . ((name . "wave")
                                         (id . nil)))))))))
            (let ((view (disco-room--ensure-view))
                  (op-token
                   (disco-room--reaction-op-begin "m1" "wave" t)))
              (cl-letf (((symbol-function 'disco-gateway--emit)
                         (lambda (event) (setq emitted event))))
                (disco-gateway--dispatch-message-reaction-add
                 '((channel_id . "reaction-queued-self")
                   (message_id . "m1")
                   (user_id . "self")
                   (emoji . ((name . "wave"))))))
              (should (eq t (plist-get emitted :self-p)))
              (appkit-view-enqueue-event view emitted)
              (appkit-request-sync view :part 'timeline)
              ;; Disconnect clears the session identity before Appkit consumes
              ;; the already queued echo.
              (setq disco-gateway--current-user-id nil)
              (cl-letf (((symbol-function 'disco-room-render) #'ignore))
                (appkit-sync-invalidations view))
              (should-not
               (disco-room--reaction-op-current-p
                "m1" "wave" op-token)))
            (let ((reaction
                   (car (disco-msg-reactions
                         (disco-room--message-by-id "m1")))))
              (should (= 1 (disco-msg-reaction-count reaction)))
              (should (disco-msg-reaction-selected-p reaction))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-other-user-reaction-delta-preserves-own-selection ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "reaction-other")
    (disco-state-put-messages
     "reaction-other"
     '(((id . "m1")
        (channel_id . "reaction-other")
        (reactions . (((count . 1)
                       (me . t)
                       (emoji . ((name . "wave") (id . nil)))))))))
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "self")))
      (disco-room--apply-live-reaction-event
       '(:type message-reaction-add
               :message-id "m1"
               :user-id "other"
               :emoji ((name . "wave"))))
      (let ((reaction
             (car (disco-msg-reactions (disco-room--message-by-id "m1")))))
        (should (= 2 (disco-msg-reaction-count reaction)))
        (should (disco-msg-reaction-selected-p reaction)))
      (disco-room--apply-live-reaction-event
       '(:type message-reaction-remove
               :message-id "m1"
               :user-id "other"
               :emoji ((name . "wave"))))
      (let ((reaction
             (car (disco-msg-reactions (disco-room--message-by-id "m1")))))
        (should (= 1 (disco-msg-reaction-count reaction)))
        (should (disco-msg-reaction-selected-p reaction)))
      ;; Even an out-of-order/duplicate other-user remove cannot erase the
      ;; aggregate vote implied by our own selected state.
      (disco-room--apply-live-reaction-event
       '(:type message-reaction-remove
               :message-id "m1"
               :user-id "other"
               :emoji ((name . "wave"))))
      (let ((reaction
             (car (disco-msg-reactions (disco-room--message-by-id "m1")))))
        (should (= 1 (disco-msg-reaction-count reaction)))
        (should (disco-msg-reaction-selected-p reaction))))))

(ert-deftest disco-room-stale-reaction-rest-success-cannot-overwrite-newer-op ()
  (let ((disco-runtime--app nil)
        add-success
        remove-success
        (requests 0))
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "reaction-generation")
            (disco-state-put-messages
             "reaction-generation"
             '(((id . "m1")
                (channel_id . "reaction-generation")
                (content . "hello"))))
            (disco-room--ensure-view)
            (cl-letf (((symbol-function 'disco-room--reaction-unavailable-reason)
                       (lambda (&optional _msg) nil))
                      ((symbol-function 'disco-api-add-reaction-async)
                       (lambda (_channel-id _message-id _emoji &rest args)
                         (setq add-success (plist-get args :on-success))))
                      ((symbol-function 'disco-api-remove-own-reaction-async)
                       (lambda (_channel-id _message-id _emoji &rest args)
                         (setq remove-success (plist-get args :on-success))))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (&rest _args) (cl-incf requests)))
                      ((symbol-function 'message) #'ignore))
              (disco-room-add-reaction "wave" "m1")
              (disco-room-remove-reaction "wave" "m1")
              (funcall remove-success nil)
              (funcall add-success nil))
            (should-not
             (disco-msg-reactions (disco-room--message-by-id "m1")))
            (should (= 1 requests)))
        (disco-runtime-stop)))))

(ert-deftest disco-room-contextual-bindings-follow-point-location ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (should (derived-mode-p 'appkit-chatbuf-mode))
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
    (appkit-chatbuf-update-context-mode)
    (should-not disco-room-timeline-mode)
    (should (eq (key-binding (kbd "q") t)
                'self-insert-command))
    (should (eq (key-binding (kbd "DEL") t)
                'appkit-chatbuf-input-backward-delete))
    (should (eq (key-binding (kbd "C-d") t)
                'appkit-chatbuf-input-forward-delete))
    (should (eq (key-binding (kbd "M-RET") t) 'disco-room-input-preview))
    (should (eq (key-binding (kbd "M-r") t) 'disco-room-draft-history-search))
    (should (eq (key-binding (kbd "C-c /") t) 'disco-room-filter-search))
    (should (eq (key-binding (kbd "C-c C-r") t)
                'disco-room-inplace-search-query))
    (should (eq (key-binding (kbd "C-c C-s") t)
                'disco-room-inplace-search-query-forward))
    (should (eq (key-binding (kbd "C-c C-c") t) 'disco-room-filter-cancel))
    (should (eq (key-binding (kbd "C-c M-/") t) 'disco-room-search-channel))
    (should (eq (key-binding (kbd "C-c RET") t) 'disco-room-send-message))
    (should (eq (key-binding (kbd "C-c M-p") t)
                'disco-room-list-pinned-messages))
    (should-not (lookup-key disco-room-mode-map (kbd "C-c s")))
    (should-not (lookup-key disco-room-mode-map (kbd "C-c n")))
    (should-not (lookup-key disco-room-mode-map (kbd "C-c p")))
    (should (eq (key-binding (kbd "C-c C-f") t) 'disco-room-attach-file))
    (should (eq (key-binding (kbd "ESC ESC") t) 'disco-room-cancel-reply))
    (should (eq (key-binding (kbd "C-M-c") t) 'disco-room-cancel-reply))
    (should-not (lookup-key disco-room-mode-map (kbd "C-c C-/")))
    (should-not (lookup-key disco-room-mode-map (kbd "C-c C-e")))
    (should (eq (key-binding (kbd "C-c C-o") t) 'disco-room-input-options-transient))
    (should-not (lookup-key disco-room-mode-map (kbd "C-c C-v")))
    (should (eq (key-binding (kbd "C-c M-v") t) 'disco-avatar-refetch))
    (should-not (lookup-key disco-room-mode-map (kbd "M-<")))
    (should-not (lookup-key disco-room-mode-map (kbd "M->")))
    (should (eq (key-binding (kbd "M-<") t) 'beginning-of-buffer))
    (should (eq (key-binding (kbd "M->") t) 'end-of-buffer))
    (goto-char (point-min))
    (appkit-chatbuf-update-context-mode)
    (should disco-room-timeline-mode)
    (should (eq (key-binding (kbd "q") t) 'quit-window))
    (should (eq (key-binding (kbd "c") t) 'disco-msg-copy-dwim))
    (should (eq (key-binding (kbd "l") t) 'disco-msg-copy-link))
    (should (eq (key-binding (kbd "n") t) 'disco-msg-next))
    (should (eq (key-binding (kbd "p") t) 'disco-msg-previous))
    (should (eq (key-binding (kbd "o") t) 'disco-msg-operate))
    (should (eq (key-binding (kbd "r") t) 'disco-msg-reply))
    (should (eq (key-binding (kbd "f") t) 'disco-msg-forward))
    (should (eq (key-binding (kbd "e") t) 'disco-msg-edit))
    (should (eq (key-binding (kbd "d") t) 'disco-msg-delete))
    (should (eq (key-binding (kbd "P") t) 'disco-msg-toggle-pin))
    (should (eq (key-binding (kbd "i") t) 'disco-msg-describe-message))
    (should (eq (key-binding (kbd "L") t) 'disco-msg-redisplay))
    (should (eq (key-binding (kbd "!") t) 'disco-msg-add-reaction))
    (should (eq (key-binding (kbd "+") t) 'disco-msg-toggle-reaction))
    (should (eq (key-binding (kbd "-") t) 'disco-msg-remove-reaction))
    (should (eq (key-binding (kbd "T") t) 'disco-msg-open-thread))
    (should (eq (key-binding (kbd "?") t) 'disco-room-transient))
    (should (eq (key-binding (kbd "C-c m c") t) 'disco-msg-copy-dwim))
    (should (eq (key-binding (kbd "C-c m l") t) 'disco-msg-copy-link))
    (should (eq (key-binding (kbd "C-c m n") t) 'disco-msg-next))
    (should (eq (key-binding (kbd "C-c m p") t) 'disco-msg-previous))
    (should (eq (key-binding (kbd "C-c m o") t) 'disco-msg-operate))
    (should (eq (key-binding (kbd "C-c m t") t) 'disco-msg-copy-text))
    (should (eq (key-binding (kbd "C-c m r") t) 'disco-msg-reply))
    (should (eq (key-binding (kbd "C-c m f") t) 'disco-msg-forward))
    (should (eq (key-binding (kbd "C-c m e") t) 'disco-msg-edit))
    (should (eq (key-binding (kbd "C-c m d") t) 'disco-msg-delete))
    (should (eq (key-binding (kbd "C-c m P") t) 'disco-msg-toggle-pin))
    (should (eq (key-binding (kbd "C-c m i") t) 'disco-msg-describe-message))
    (should (eq (key-binding (kbd "C-c m L") t) 'disco-msg-redisplay))))

(ert-deftest disco-room-msg-layer-adapters-and-message-properties-are-installed-without-row-keymap ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (setq-local disco-room--guild-id "g1")
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (content . "hello world"))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (should (eq disco-msg-resolve-function #'disco-room--resolve-message))
    (should (eq disco-msg-content-text-function #'disco-room--message-copy-text))
    (should (eq disco-msg-reply-function #'disco-room--reply-to-msg))
    (should (eq disco-msg-forward-function #'disco-room--forward-msg))
    (should (eq disco-msg-operate-function #'disco-room--operate-msg))
    (should (eq disco-msg-edit-function #'disco-room--edit-msg))
    (should (eq disco-msg-delete-function #'disco-room--delete-msg))
    (should (eq disco-msg-toggle-pin-function #'disco-room--toggle-pin-on-msg))
    (should (eq disco-msg-open-thread-function #'disco-room-thread-open-from-message))
    (should (eq disco-msg-toggle-reaction-function #'disco-room--toggle-reaction-on-msg))
    (should (eq disco-msg-add-reaction-function #'disco-room--add-reaction-to-msg))
    (should (eq disco-msg-remove-reaction-function #'disco-room--remove-reaction-from-msg))
    (should (eq disco-msg-redisplay-function #'disco-room--redisplay-msg))
    (goto-char (point-min))
    (search-forward "hello")
    (backward-char 2)
    (should-not (get-text-property (point) 'keymap))
    (should (equal "m1" (get-text-property (point) 'disco-message-id)))
    (should (equal "chat" (get-text-property (point) 'disco-message-channel-id)))
    (should (equal "g1" (get-text-property (point) 'disco-message-guild-id)))
    (should (equal "m1" (alist-get 'id (disco-msg-at (point)))))))

(ert-deftest disco-room-message-links-retain-their-keymaps ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (content . "see [link](https://example.com) now"))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (goto-char (point-min))
    (search-forward "link")
    (backward-char 2)
    (should (equal "https://example.com"
                   (get-text-property (point) 'disco-markdown-url)))
    (should (keymapp (get-text-property (point) 'keymap)))))

(ert-deftest disco-room-message-navigation-uses-msg-next-and-previous ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m2")
        (channel_id . "chat")
        (content . "second"))
       ((id . "m1")
        (channel_id . "chat")
        (content . "first"))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    ;; Room headers may precede the first message; navigation starts from the
    ;; first actual message property span, not blindly from `point-min'.
    (let* ((starts (disco-msg--message-start-positions))
           (first-id (get-text-property (nth 0 starts) 'disco-message-id))
           (second-id (get-text-property (nth 1 starts) 'disco-message-id)))
      (should (= (length starts) 2))
      (should-not (equal first-id second-id))
      (goto-char (car starts))
      (should (equal first-id (get-text-property (point) 'disco-message-id)))
      (disco-msg-next)
      (should (equal second-id (get-text-property (point) 'disco-message-id)))
      (disco-msg-previous)
      (should (equal first-id (get-text-property (point) 'disco-message-id))))))

(ert-deftest disco-room-deleted-tail-does-not-return-after-frame-refresh ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (goto-char (point-max))
    (insert "abc")
    (delete-backward-char 1)
    (should (equal "ab"
                   (appkit-chatbuf-string-plain-text
                    (disco-room--current-draft))))
    (disco-room--update-frame)
    (should (equal "ab" (appkit-chatbuf-input-string)))
    (should (equal "ab"
                   (appkit-chatbuf-string-plain-text
                    (disco-room--current-draft))))))

(ert-deftest disco-room-ack-channel-pins-applies-state-on-success ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
    (let ((disco-room--channel-id "chan")
          called-channel-id)
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (channel-id &rest args)
                   (setq called-channel-id channel-id)
                   (funcall (plist-get args :on-success) nil)))
                ((symbol-function 'disco-room--callback-active-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-ack-channel-pins)
        (should (equal "chan" called-channel-id))
        (should (equal "2026-03-04T01:00:00.000000+00:00"
                       (disco-state-channel-last-read-pin-timestamp "chan")))))))

(ert-deftest disco-room-ack-channel-pins-skips-when-already-acked ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
    (disco-state-apply-channel-pins-ack
     "chan"
     "2026-03-04T01:00:00.000000+00:00")
    (let ((disco-room--channel-id "chan")
          (api-called nil))
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (&rest _args)
                   (setq api-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-ack-channel-pins)
        (should-not api-called)))))

(ert-deftest disco-room-stale-pins-ack-success-cannot-regress-newer-cursor ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
    (setq-local disco-room--channel-id "chan")
    (let (success-callbacks)
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (_channel-id &rest args)
                   (setq success-callbacks
                         (append success-callbacks
                                 (list (plist-get args :on-success))))))
                ((symbol-function 'disco-room--callback-active-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'message) #'ignore))
        (disco-room-ack-channel-pins)
        (disco-state-apply-channel-pins-update
         "chan" "2026-03-04T02:00:00.000000+00:00")
        (disco-room-ack-channel-pins)
        (funcall (nth 1 success-callbacks) nil)
        (funcall (nth 0 success-callbacks) nil))
      (should
       (equal "2026-03-04T02:00:00.000000+00:00"
              (disco-state-channel-last-read-pin-timestamp "chan"))))))

(ert-deftest disco-room-pins-ack-success-never-overwrites-newer-gateway-ack ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:00:00.000000+00:00")))
    (setq-local disco-room--channel-id "chan")
    (let (success-callback)
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (_channel-id &rest args)
                   (setq success-callback (plist-get args :on-success))))
                ((symbol-function 'disco-room--callback-active-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'message) #'ignore))
        (disco-room-ack-channel-pins)
        (disco-state-apply-channel-pins-ack
         "chan" "2026-03-04T02:00:00.000000+00:00")
        (funcall success-callback nil))
      (should
       (equal "2026-03-04T02:00:00.000000+00:00"
              (disco-state-channel-last-read-pin-timestamp "chan"))))))

(ert-deftest disco-room-pins-ack-success-uses-timezone-aware-state-merge ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (last_pin_timestamp . "2026-03-04T01:30:00Z")))
    ;; 02:00 +01:00 is 01:00Z, so the channel pin at 01:30Z is newer even
    ;; though its timestamp is lexically smaller.
    (disco-state-apply-channel-pins-ack
     "chan" "2026-03-04T02:00:00+01:00")
    (setq-local disco-room--channel-id "chan")
    (let (success-callback)
      (cl-letf (((symbol-function 'disco-api-ack-channel-pins-async)
                 (lambda (_channel-id &rest args)
                   (setq success-callback (plist-get args :on-success))))
                ((symbol-function 'disco-room--callback-active-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'message) #'ignore))
        (disco-room-ack-channel-pins)
        (should (functionp success-callback))
        (funcall success-callback nil))
      (should
       (equal "2026-03-04T01:30:00Z"
              (disco-state-channel-last-read-pin-timestamp "chan"))))))

(ert-deftest disco-room-handle-gateway-pin-events-refresh-current-frame ()
  (with-temp-buffer
    (let ((disco-room--channel-id "chan")
          (disco-room--channel-name "old")
          (frame-called nil))
      (cl-letf (((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chan") (name . "new"))))
                ((symbol-function 'disco-room--update-frame)
                 (lambda () (setq frame-called t))))
        (disco-room--apply-gateway-event
         '(:type channel-pins-update
		 :channel-id "chan"))
        (should frame-called)
        (should (equal "new" disco-room--channel-name))))))

(ert-deftest disco-room-handle-gateway-message-create-patches-persistent-ewoc ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (setq-local disco-room-group-messages t)
    (setq-local disco-room-group-messages-timespan 3600)
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "first")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m1 (appkit-chat-timeline-node "m1"))
          render-called)
      (disco-state-put-messages
       "chat"
       '(((id . "m2")
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:05:00.000000+00:00")
          (content . "second")
          (author . ((id . "u1") (username . "alice"))))
         ((id . "m1")
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:00:00.000000+00:00")
          (content . "first")
          (author . ((id . "u1") (username . "alice"))))))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t)))
                ((symbol-function 'disco-room--mark-read)
                 (lambda (&rest _args) nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         '(:type message-create
		 :channel-id "chat"
		 :message ((id . "m2")
			   (channel_id . "chat")
			   (timestamp . "2026-03-08T00:05:00.000000+00:00")
			   (content . "second")
			   (author . ((id . "u1") (username . "alice")))))))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m1 (appkit-chat-timeline-node "m1")))
      (should (appkit-chat-timeline-node "m2"))
      (should (equal '("m1" "m2") (appkit-chat-timeline-keys)))
      (should (plist-get (appkit-chat-timeline-context "m2")
                         :compact))
      (should (string-match-p "second" (buffer-string))))))

(ert-deftest disco-room-handle-gateway-message-delete-recomputes-next-context ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (setq-local disco-room-group-messages t)
    (setq-local disco-room-group-messages-timespan 3600)
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m2")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:05:00.000000+00:00")
        (content . "second")
        (author . ((id . "u1") (username . "alice"))))
       ((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "first")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (should (plist-get (appkit-chat-timeline-context "m2")
                       :compact))
    (disco-room--poll-set-draft-selection "m1" '(1))
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m2 (appkit-chat-timeline-node "m2"))
          (poll-token (disco-room--poll-vote-op-begin "m1" '(1)))
          (reaction-token
           (disco-room--reaction-op-begin "m1" "wave" t))
          render-called)
      (disco-state-put-messages
       "chat"
       '(((id . "m2")
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:05:00.000000+00:00")
          (content . "second")
          (author . ((id . "u1") (username . "alice"))))))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         '(:type message-delete
		 :channel-id "chat"
		 :message-id "m1")))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m2 (appkit-chat-timeline-node "m2")))
      (should-not (appkit-chat-timeline-node "m1"))
      (should (equal '("m2") (appkit-chat-timeline-keys)))
      (should-not (plist-get (appkit-chat-timeline-context "m2")
                             :compact))
      (should-not (disco-room--poll-draft-selection-present-p "m1"))
      (should-not (disco-room--poll-vote-op-current-p "m1" poll-token))
      (should-not
       (disco-room--reaction-op-current-p "m1" "wave" reaction-token))
      (should (string-match-p "alice" (buffer-string))))))

(ert-deftest disco-room-handle-gateway-message-update-refreshes-dependent-reply-preview ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m3")
        (type . 19)
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:10:00.000000+00:00")
        (content . "reply body")
        (message_reference . ((message_id . "m1")
                              (channel_id . "chat")))
        (author . ((id . "u2") (username . "bob"))))
       ((id . "m2")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:05:00.000000+00:00")
        (content . "middle")
        (author . ((id . "u3") (username . "carol"))))
       ((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "source one")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m3 (appkit-chat-timeline-node "m3"))
          render-called)
      (disco-state-put-messages
       "chat"
       '(((id . "m3")
          (type . 19)
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:10:00.000000+00:00")
          (content . "reply body")
          (message_reference . ((message_id . "m1")
                                (channel_id . "chat")))
          (author . ((id . "u2") (username . "bob"))))
         ((id . "m2")
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:05:00.000000+00:00")
          (content . "middle")
          (author . ((id . "u3") (username . "carol"))))
         ((id . "m1")
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:00:00.000000+00:00")
          (content . "source edited")
          (author . ((id . "u1") (username . "alice"))))))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         '(:type message-update
		 :channel-id "chat"
		 :message ((id . "m1")
			   (channel_id . "chat")
			   (timestamp . "2026-03-08T00:00:00.000000+00:00")
			   (content . "source edited")
			   (author . ((id . "u1") (username . "alice")))))))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m3 (appkit-chat-timeline-node "m3")))
      (should (string-match-p "↪ alice: source edited" (buffer-string)))
      (should-not (string-match-p (regexp-quote "[Jump]") (buffer-string)))
      (goto-char (point-min))
      (search-forward "↪ alice: source edited")
      (let ((position (match-beginning 0)))
        (should (keymapp (get-text-property position 'keymap)))
        (should (equal "Open replied-to message"
                       (get-text-property position 'help-echo)))))))

(ert-deftest disco-room-handle-gateway-message-delete-refreshes-thread-starter-preview ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m2")
        (type . 21)
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:10:00.000000+00:00")
        (message_reference . ((message_id . "m1")
                              (channel_id . "chat")))
        (author . ((id . "u2") (username . "bob"))))
       ((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "thread source")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m2 (appkit-chat-timeline-node "m2"))
          render-called)
      (disco-state-put-messages
       "chat"
       '(((id . "m2")
          (type . 21)
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:10:00.000000+00:00")
          (message_reference . ((message_id . "m1")
                                (channel_id . "chat")))
          (author . ((id . "u2") (username . "bob"))))))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         '(:type message-delete
		 :channel-id "chat"
		 :message-id "m1")))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m2 (appkit-chat-timeline-node "m2")))
      (should (string-match-p "Sorry, we couldn't load the first message in this thread."
                              (buffer-string))))))

(ert-deftest disco-room-thread-starter-spoiler-targets-container-message ()
  (with-temp-buffer
    (disco-room-mode)
    (let* ((msg '((id . "container")
                  (type . 21)
                  (referenced_message
                   . ((id . "source") (content . "||secret||")))))
           (hidden (disco-room--thread-starter-reference-content msg))
           (hidden-pos (string-match "secret" hidden)))
      (should hidden-pos)
      (should (equal "container"
                     (get-text-property
                      hidden-pos 'disco-markdown-spoiler-message-id hidden)))
      (should (equal "█" (get-text-property hidden-pos 'display hidden)))
      (setq-local disco-room--revealed-spoiler-message-id "container")
      (let* ((revealed (disco-room--thread-starter-reference-content msg))
             (revealed-pos (string-match "secret" revealed)))
        (should revealed-pos)
        (should-not (get-text-property revealed-pos 'display revealed))))))

(ert-deftest disco-room-handle-channel-update-refreshes-forward-source-label ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (disco-state-upsert-channel '((id . "src") (type . 0) (guild_id . "g1") (name . "old-src")))
    (disco-state-upsert-guild '((id . "g1") (name . "Guild")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (message_reference . ((type . 1)
                              (message_id . "s1")
                              (channel_id . "src")
                              (guild_id . "g1")))
        (message_snapshots . [((content . "snap body")
                               (timestamp . "2026-03-08T00:00:00.000000+00:00"))])
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m1 (appkit-chat-timeline-node "m1"))
          render-called)
      (disco-state-upsert-channel '((id . "src") (type . 0) (guild_id . "g1") (name . "new-src")))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         '(:type channel-update
		 :channel-id "src")))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m1 (appkit-chat-timeline-node "m1")))
      (should (string-match-p "Guild / #new-src" (buffer-string))))))

(ert-deftest disco-room-handle-message-update-refreshes-composer-reply-context ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-room--set-composer-aux-state nil "m1")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "source one")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          render-called)
      (disco-state-put-messages
       "chat"
       '(((id . "m1")
          (channel_id . "chat")
          (timestamp . "2026-03-08T00:00:00.000000+00:00")
          (content . "source edited")
          (author . ((id . "u1") (username . "alice"))))))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         '(:type message-update
		 :channel-id "chat"
		 :message ((id . "m1")
			   (channel_id . "chat")
			   (timestamp . "2026-03-08T00:00:00.000000+00:00")
			   (content . "source edited")
			   (author . ((id . "u1") (username . "alice")))))))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (string-match-p "  ▏ source edited" (buffer-string))))))

(ert-deftest disco-room-handle-message-ack-moves-unread-divider-in-place ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m3")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:10:00.000000+00:00")
        (content . "third")
        (author . ((id . "u2") (username . "bob"))))
       ((id . "m2")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:05:00.000000+00:00")
        (content . "second")
        (author . ((id . "u1") (username . "alice"))))
       ((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "first")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-state-apply-message-ack "chat" "m1" 1)
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (should (eq (plist-get (appkit-chat-timeline-context "m2")
                           :insert-unread)
                t))
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m2 (appkit-chat-timeline-node "m2"))
          (node-m3 (appkit-chat-timeline-node "m3"))
          render-called)
      (disco-state-apply-message-ack "chat" "m2" 0)
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda () (setq render-called t))))
        (disco-room--apply-gateway-event
         '(:type message-ack
		 :channel-id "chat"
		 :message-id "m2")))
      (should-not render-called)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m2 (appkit-chat-timeline-node "m2")))
      (should (eq node-m3 (appkit-chat-timeline-node "m3")))
      (should-not (plist-get (appkit-chat-timeline-context "m2")
                             :insert-unread))
      (should (eq (plist-get (appkit-chat-timeline-context "m3")
                             :insert-unread)
                  t)))))

(ert-deftest disco-room-mark-read-applies-optimistic-unread-patch ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m3")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:10:00.000000+00:00")
        (content . "third")
        (author . ((id . "u2") (username . "bob"))))
       ((id . "m2")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:05:00.000000+00:00")
        (content . "second")
        (author . ((id . "u1") (username . "alice"))))
       ((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "first")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-state-apply-message-ack "chat" "m1" 1)
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (should (eq (plist-get (appkit-chat-timeline-context "m2")
                           :insert-unread)
                t))
    (cl-letf (((symbol-function 'disco-api-ack-message-async)
               (lambda (&rest _args) nil))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room--mark-read "m3"))
    (should (equal "m3" (disco-state-channel-last-read-message-id "chat")))
    (should disco-room--pending-optimistic-read-ack)
    (should-not (plist-get (appkit-chat-timeline-context "m2")
                           :insert-unread))
    (should-not (plist-get (appkit-chat-timeline-context "m3")
                           :insert-unread))))

(ert-deftest disco-room-mark-read-empty-window-does-not-ack-stale-channel-id ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-upsert-channel
     '((id . "chan")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")
       (last_message_id . "300")))
    (disco-state-set-channel-unread "chan" 2)
    (appkit-chat-history-window-establish-empty)
    (let (acked-id)
      (cl-letf (((symbol-function 'disco-api-ack-message-async)
                 (lambda (_channel-id message-id &rest _args)
                   (setq acked-id message-id))))
        (disco-room--mark-read))
      (should-not acked-id))
    (should (= 0 (disco-state-channel-unread-count "chan")))
    (should-not (disco-state-channel-last-read-message-id "chan"))))

(ert-deftest disco-room-mark-read-rolls-back-optimistic-unread-patch-on-error ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m3")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:10:00.000000+00:00")
        (content . "third")
        (author . ((id . "u2") (username . "bob"))))
       ((id . "m2")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:05:00.000000+00:00")
        (content . "second")
        (author . ((id . "u1") (username . "alice"))))
       ((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (content . "first")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-state-apply-message-ack "chat" "m1" 1)
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (let (error-callback
          request)
      (cl-letf (((symbol-function 'disco-api-ack-message-async)
                 (lambda (&rest args)
                   (setq error-callback (plist-get args :on-error))))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--mark-read "m3"))
      (should-not (plist-get (appkit-chat-timeline-context "m2")
                             :insert-unread))
      (cl-letf (((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (ert-fail "read ACK callback updated frame directly")))
                ((symbol-function 'disco-room-render)
                 (lambda ()
                   (ert-fail "read ACK callback rendered directly")))
                ((symbol-function 'disco-room--sync-timeline)
                 (lambda (&rest _args)
                   (ert-fail "read ACK callback projected timeline directly")))
                ((symbol-function 'appkit-sync-invalidations)
                 (lambda (&rest _args)
                   (ert-fail "read ACK callback synced directly")))
                ((symbol-function 'appkit-request-sync)
                 (lambda (owner &rest args)
                   (setq request (cons owner args))))
                ((symbol-function 'message) #'ignore))
        (funcall error-callback '(:message "boom"))
        (should (eq (appkit-current-view) (car request)))
        (should (eq 'timeline (plist-get (cdr request) :part)))
        ;; Controller state is rolled back immediately, while the old
        ;; optimistic projection remains until Appkit consumes the request.
        (should-not disco-room--pending-optimistic-read-ack)
        (should (equal "m1"
                       (disco-state-channel-last-read-message-id "chat")))
        (should-not (plist-get (appkit-chat-timeline-context "m2")
                               :insert-unread)))
      (appkit-request-sync (appkit-current-view) :part 'timeline)
      (appkit-sync-invalidations (appkit-current-view)))
    (should-not disco-room--pending-optimistic-read-ack)
    (should (equal "m1" (disco-state-channel-last-read-message-id "chat")))
    (should (eq (plist-get (appkit-chat-timeline-context "m2")
                           :insert-unread)
                t))
    (should-not (plist-get (appkit-chat-timeline-context "m3")
                           :insert-unread))))

(ert-deftest disco-room-resolve-pending-jump-fetches-around-once ()
  (with-temp-buffer
    (let ((disco-room--pending-jump-message-id "m1")
          fetched)
      (cl-letf (((symbol-function 'disco-room--jump-to-visible-message)
                 (lambda (_message-id) nil))
                ((symbol-function 'disco-room--fetch-around-pending-jump)
                 (lambda ()
                   (setq fetched t))))
        (disco-room--resolve-pending-jump)
        (should fetched)))))

(ert-deftest disco-room-fetch-around-pending-jump-merges-cache-and-jumps ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan")
          (disco-room--pending-jump-message-id "20")
          jumped
          rendered)
      (disco-state-reset)
      (disco-state-put-messages
       "chan"
       '(((id . "90") (channel_id . "chan") (content . "newer"))))
      (setq disco-room--remote-latest-message-id "90")
      (appkit-chat-history-window-set "90" nil)
      (cl-letf (((symbol-function 'disco-api-channel-messages-around-async)
                 (lambda (_channel-id _message-id &rest args)
                   (funcall (plist-get args :on-success)
                            '(((id . "30") (channel_id . "chan") (content . "older"))
                              ((id . "20") (channel_id . "chan") (content . "target"))
                              ((id . "10") (channel_id . "chan") (content . "oldest"))))))
                ((symbol-function 'disco-room--callback-active-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-room-render)
                 (lambda ()
                   (setq rendered t)))
                ((symbol-function 'disco-room--jump-to-visible-message)
                 (lambda (message-id)
                   (setq jumped message-id)
                   t))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--fetch-around-pending-jump)
        ;; The transport callback updates state and only queues presentation.
        (should-not rendered)
        (should-not jumped)
        (should (equal "20" disco-room--pending-jump-message-id))
        (appkit-sync-invalidations (appkit-current-view))
        (should rendered)
        (should (equal "20" jumped))
        (should-not disco-room--pending-jump-message-id)
        (should (equal '("90" "30" "20" "10")
                       (mapcar (lambda (msg) (alist-get 'id msg))
                               (disco-state-messages "chan"))))
        (should (equal "10" (appkit-chat-history-window-first-key)))
        (should (equal "30" (appkit-chat-history-window-last-key)))
        (should
         (equal '("30" "20" "10")
                (mapcar #'disco-room--message-id
                        (disco-room--display-messages))))))))

(ert-deftest disco-room-refresh-preserves-gateway-mutations-during-request ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan"))
      (disco-state-reset)
      (disco-state-put-messages
       "chan"
       '(((id . "20") (channel_id . "chan") (content . "baseline"))
         ((id . "10") (channel_id . "chan") (content . "deleted soon"))))
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (disco-state-put-messages
                    "chan"
                    '(((id . "30") (channel_id . "chan") (content . "gateway create"))
                      ((id . "20") (channel_id . "chan") (content . "gateway update"))))
                   (funcall (plist-get args :on-success)
                            '(((id . "20") (channel_id . "chan")
                               (content . "stale REST value"))
                              ((id . "10") (channel_id . "chan")
                               (content . "stale deleted value"))
                              ((id . "5") (channel_id . "chan")
                               (content . "REST history"))))))
                ((symbol-function 'disco-room--update-frame)
                 #'ignore)
                ((symbol-function 'disco-room--mark-read) #'ignore)
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--resolve-pending-jump) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-refresh)
        (let ((messages (disco-state-messages "chan")))
          (should (equal '("30" "20" "5")
                         (mapcar (lambda (message) (alist-get 'id message))
                                 messages)))
          (should (equal "gateway update"
                         (alist-get 'content (cadr messages)))))))))

(ert-deftest disco-room-fetch-around-pending-jump-errors-when-target-missing ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan")
          (disco-room--pending-jump-message-id "m2")
          rendered)
      (disco-state-reset)
      (cl-letf (((symbol-function 'disco-api-channel-messages-around-async)
                 (lambda (_channel-id _message-id &rest args)
                   (funcall (plist-get args :on-success)
                            '(((id . "m3") (channel_id . "chan") (content . "older"))))))
                ((symbol-function 'disco-room--callback-active-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-room-render)
                 (lambda ()
                   (setq rendered t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--fetch-around-pending-jump)
        (should-not rendered)
        (should-not disco-room--pending-jump-message-id)
        (should (equal '("m3")
                       (mapcar #'disco-room--message-id
                               (disco-state-messages "chan"))))))))

(ert-deftest disco-room-around-rejects-target-deleted-after-request ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300"
          disco-room--pending-jump-message-id "250")
    (appkit-chat-history-window-set "100" "300")
    (let (callback rendered)
      (cl-letf (((symbol-function 'disco-api-channel-messages-around-async)
                 (lambda (_channel-id _message-id &rest args)
                   (setq callback (plist-get args :on-success))))
                ((symbol-function 'disco-room-render)
                 (lambda () (setq rendered t)))
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room--fetch-around-pending-jump)
        (disco-state-delete-message "chan" "250")
        (funcall callback
                 '(((id . "300") (channel_id . "chan"))
                   ((id . "250") (channel_id . "chan"))
                   ((id . "200") (channel_id . "chan")))))
      (should-not rendered))
    (should-not disco-room--pending-jump-message-id)
    (should (equal "100" (appkit-chat-history-window-first-key)))
    (should (equal "300" (appkit-chat-history-window-last-key)))))

(ert-deftest disco-room-history-window-strictly-hides-cache-islands ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "900") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "200") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))
       ((id . "10") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "900")
    (appkit-chat-history-window-set "100" "300")
    (should
     (equal '("300" "200" "100")
            (mapcar #'disco-room--message-id
                    (disco-room--display-messages))))
    (disco-room-render)
    (should (equal '("100" "200" "300")
                   (appkit-chat-timeline-keys)))))

(ert-deftest disco-room-refresh-replaces-around-window-with-latest-slice ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "900") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "200") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "900")
    (appkit-chat-history-window-set "100" "300")
    (cl-letf (((symbol-function 'disco-api-channel-messages-async)
               (lambda (_channel-id &rest args)
                 ;; Deliberately oldest-first: room normalization owns order.
                 (funcall (plist-get args :on-success)
                          '(((id . "1000") (channel_id . "chan"))
                            ((id . "1100") (channel_id . "chan"))))))
              ((symbol-function 'disco-room--mark-read) #'ignore)
              ((symbol-function 'message) #'ignore))
      (disco-room-refresh))
    (should (equal "1100" disco-room--remote-latest-message-id))
    (should (equal "1000" (appkit-chat-history-window-first-key)))
    (should-not (appkit-chat-history-window-last-key))
    (should
     (equal '("1100" "1000")
            (mapcar #'disco-room--message-id
                    (disco-room--display-messages))))))

(ert-deftest disco-room-empty-latest-hides-stale-cache-islands ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan" '(((id . "900") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "900")
    (appkit-chat-history-window-set "900" nil)
    (cl-letf (((symbol-function 'disco-api-channel-messages-async)
               (lambda (_channel-id &rest args)
                 (funcall (plist-get args :on-success) nil)))
              ((symbol-function 'disco-room--mark-read) #'ignore)
              ((symbol-function 'message) #'ignore))
      (disco-room-refresh))
    (should (appkit-chat-history-window-empty-p))
    (should (appkit-chat-history-older-loaded-p))
    (should-not disco-room--remote-latest-message-id)
    (should-not (disco-room--display-messages))
    (should (equal '("900")
                   (mapcar #'disco-room--message-id
                           (disco-state-messages "chan"))))))

(ert-deftest disco-room-latest-uses-only-revision-retained-response-edges ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan" '(((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "100")
    (appkit-chat-history-window-set "100" nil)
    (let ((disco-message-fetch-limit 3)
          callback)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq callback (plist-get args :on-success))))
                ((symbol-function 'disco-room--mark-read) #'ignore)
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-refresh)
        (disco-state-delete-message "chan" "200")
        (funcall callback
                 '(((id . "400") (channel_id . "chan"))
                   ((id . "300") (channel_id . "chan"))
                   ((id . "200") (channel_id . "chan"))))))
    (should (equal "300" (appkit-chat-history-window-first-key)))
    (should-not (appkit-chat-history-window-last-key))
    ;; Raw transport count was full even though revision filtering retained
    ;; only two rows, so it does not prove the beginning of history.
    (should-not (appkit-chat-history-older-loaded-p))
    (should (equal "400" disco-room--remote-latest-message-id))))

(ert-deftest disco-room-latest-full-page-with-no-retained-edge-stays-unknown ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan" '(((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "100")
    (appkit-chat-history-window-set "100" nil)
    (let ((disco-message-fetch-limit 2)
          callback
          marked-read)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq callback (plist-get args :on-success))))
                ((symbol-function 'disco-room--mark-read)
                 (lambda (&rest _args) (setq marked-read t)))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-refresh)
        (disco-state-delete-message "chan" "300")
        (disco-state-delete-message "chan" "200")
        (funcall callback
                 '(((id . "300") (channel_id . "chan"))
                   ((id . "200") (channel_id . "chan"))))
        (should-not marked-read)))
    (should-not (appkit-chat-history-window-known-p))
    (should (equal "100" disco-room--remote-latest-message-id))))

(ert-deftest disco-room-latest-conflict-keeps-concurrent-live-frontier ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan" '(((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "100")
    (appkit-chat-history-window-set "100" nil)
    (let ((disco-message-fetch-limit 2)
          callback)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq callback (plist-get args :on-success))))
                ((symbol-function 'disco-room--mark-read) #'ignore)
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-refresh)
        (disco-state-upsert-message
         "chan" '((id . "300") (channel_id . "chan")))
        (disco-room--observe-live-create "300")
        (disco-state-delete-message "chan" "500")
        (disco-state-delete-message "chan" "400")
        (funcall callback
                 '(((id . "500") (channel_id . "chan"))
                   ((id . "400") (channel_id . "chan"))))))
    (should (equal "300" disco-room--remote-latest-message-id))
    (should (equal "300" (appkit-chat-history-window-first-key)))
    (should-not (appkit-chat-history-window-last-key))
    (should-not (appkit-chat-history-older-loaded-p))))

(ert-deftest disco-room-load-older-uses-window-first-not-cache-minimum ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "900") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "200") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "900")
    (appkit-chat-history-window-set "200" "300")
    (let (captured-before)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq captured-before (plist-get args :before))
                   (funcall (plist-get args :on-success)
                            '(((id . "150") (channel_id . "chan"))
                              ((id . "180") (channel_id . "chan"))))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-load-older-messages t))
      (should (equal "200" captured-before)))
    (should (equal "150" (appkit-chat-history-window-first-key)))
    (should (equal "300" (appkit-chat-history-window-last-key)))
    (should
     (equal '("300" "200" "180" "150")
            (mapcar #'disco-room--message-id
                    (disco-room--display-messages))))))

(ert-deftest disco-room-older-full-page-does-not-confuse-retained-count-with-eof ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300")
    (appkit-chat-history-window-set "100" nil)
    (let ((disco-message-fetch-limit 2)
          callback)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq callback (plist-get args :on-success))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-load-older-messages t)
        (disco-state-delete-message "chan" "80")
        (funcall callback
                 '(((id . "90") (channel_id . "chan"))
                   ((id . "80") (channel_id . "chan"))))))
    (should (equal "90" (appkit-chat-history-window-first-key)))
    (should-not (appkit-chat-history-older-loaded-p))))

(ert-deftest disco-room-newer-pages-normalize-order-and-attach-at-frontier ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "500") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "200") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "500")
    (appkit-chat-history-window-set "100" "300")
    (let (after-cursors)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (let ((after (plist-get args :after)))
                     (push after after-cursors)
                     (funcall
                      (plist-get args :on-success)
                      (if (equal after "300")
                          '(((id . "350") (channel_id . "chan"))
                            ((id . "400") (channel_id . "chan")))
                        '(((id . "450") (channel_id . "chan"))
                          ((id . "500") (channel_id . "chan"))))))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-load-newer-messages t)
        (should (equal "400" (appkit-chat-history-window-last-key)))
        (should
         (equal '("400" "350" "300" "200" "100")
                (mapcar #'disco-room--message-id
                        (disco-room--display-messages))))
        (disco-room-load-newer-messages t))
      (should (equal '("400" "300") after-cursors)))
    (should-not (appkit-chat-history-window-last-key))
    (should (equal "500" disco-room--remote-latest-message-id))
    (should
     (equal '("500" "450" "400" "350" "300" "200" "100")
            (mapcar #'disco-room--message-id
                    (disco-room--display-messages))))))

(ert-deftest disco-room-newer-no-progress-stalls-only-current-edge ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "500") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "500")
    (appkit-chat-history-window-set "100" "300")
    (cl-letf (((symbol-function 'disco-api-channel-messages-async)
               (lambda (_channel-id &rest args)
                 (funcall (plist-get args :on-success) nil)))
              ((symbol-function 'disco-room-render) #'ignore)
              ((symbol-function 'disco-room--update-frame) #'ignore)
              ((symbol-function 'message) #'ignore))
      (disco-room-load-newer-messages t))
    (should (equal "300" (appkit-chat-history-window-last-key)))
    (should (appkit-chat-history-newer-stalled-p))))

(ert-deftest disco-room-newer-uses-only-revision-retained-response-edge ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "500") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "500")
    (appkit-chat-history-window-set "100" "300")
    (disco-room-render)
    (let ((disco-message-fetch-limit 2)
          callback)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq callback (plist-get args :on-success))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-load-newer-messages t)
        (disco-state-delete-message "chan" "500")
        (disco-room--apply-gateway-event
         '(:type message-delete :channel-id "chan" :message-id "500"))
        (funcall callback
                 '(((id . "500") (channel_id . "chan"))
                   ((id . "400") (channel_id . "chan"))))))
    (should (equal "400" (appkit-chat-history-window-last-key)))
    (should-not (member "500"
                        (mapcar #'disco-room--message-id
                                (disco-room--display-messages))))))

(ert-deftest disco-room-stale-older-owner-cannot-overwrite-latest-refresh ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "300") (channel_id . "chan"))
       ((id . "200") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300")
    (appkit-chat-history-window-set "200" nil)
    (let (older-callback latest-callback canceled)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (let* ((owner (plist-get args :owner))
                          (callback (plist-get args :on-success))
                          (handle
                           (appkit-register-handle
                            owner 'test-history owner
                            (lambda (object) (push object canceled))))
                          (settle
                           (lambda (messages)
                             (appkit-retire-handle handle)
                             (funcall callback messages))))
                     (if (plist-get args :before)
                         (setq older-callback settle)
                       (setq latest-callback settle)))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'disco-room--mark-read) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-load-older-messages t)
        (let ((older-owner (appkit-chat-history-request-owner)))
          (should (appkit-view-operation-p older-owner))
          (disco-room-refresh)
          (should-not
           (appkit-chat-history-request-current-p older-owner))
          (should (equal canceled (list older-owner))))
        (funcall latest-callback
                 '(((id . "500") (channel_id . "chan"))
                   ((id . "400") (channel_id . "chan"))))
        (should-not (appkit-chat-history-loading-p))
        (funcall older-callback
                 '(((id . "150") (channel_id . "chan"))))))
    (should (equal "400" (appkit-chat-history-window-first-key)))
    (should-not (appkit-chat-history-window-last-key))
    (should-not
     (seq-find (lambda (message)
                 (equal "150" (disco-room--message-id message)))
               (disco-state-messages "chan")))))

(ert-deftest disco-room-partial-live-create-stays-hidden-and-unread ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "900") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "200") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "900")
    (appkit-chat-history-window-set "100" "300")
    (disco-room-render)
    (disco-state-apply-message-create
     "chan"
     '((id . "1000")
       (type . 0)
       (author . ((id . "u2")))
       (mentions . [((id . "u1"))])
       (mention_roles . [])
       (mention_everyone . :false)
       (member . ((roles . []))))
     "u1" t)
    (disco-state-upsert-message
     "chan"
     '((id . "1000")
       (channel_id . "chan")
       (type . 0)
       (author . ((id . "u2")))
       (mentions . [((id . "u1"))])))
    (let (read-id)
      (cl-letf (((symbol-function 'disco-room--mark-read)
                 (lambda (&optional id) (setq read-id id))))
        (disco-room--apply-gateway-event
         '(:type message-create :channel-id "chan"
		 :message ((id . "1000") (channel_id . "chan")))))
      (should-not read-id))
    (should (equal "1000" disco-room--remote-latest-message-id))
    (should (= 1 (disco-state-channel-unread-count "chan")))
    (should (= 1 (disco-state-channel-unread-mention-count "chan")))
    (should (equal '("100" "200" "300")
                   (appkit-chat-timeline-keys)))
    (should-not (member "1000"
                        (mapcar #'disco-room--message-id
                                (disco-room--display-messages))))))

(ert-deftest disco-room-visible-live-create-optimistically-clears-unread ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan" '(((id . "300") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300")
    (appkit-chat-history-window-set "300" nil)
    (disco-room-render)
    (let ((message
           '((id . "400")
             (channel_id . "chan")
             (type . 0)
             (author . ((id . "u2")))
             (mentions . [((id . "u1"))])
             (mention_roles . [])
             (mention_everyone . :false)
             (member . ((roles . []))))))
      (disco-state-apply-message-create "chan" message "u1" t)
      (disco-state-upsert-message "chan" message)
      (cl-letf (((symbol-function 'disco-api-ack-message-async)
                 (lambda (&rest _args) nil)))
        (disco-room--apply-gateway-event
         (list :type 'message-create :channel-id "chan" :message message))))
    (should (= 0 (disco-state-channel-unread-count "chan")))
    (should (equal "400"
                   (disco-state-channel-last-read-message-id "chan")))
    (should (equal '("300" "400") (appkit-chat-timeline-keys)))))

(ert-deftest disco-room-live-create-frontier-is-monotonic-and-clears-stall ()
  (with-temp-buffer
    (disco-room-mode)
    (setq disco-room--remote-latest-message-id "500")
    (appkit-chat-history-window-set "100" "300")
    (appkit-chat-history-newer-stalled-set "300")
    (disco-room--observe-live-create "400")
    (should (equal "500" disco-room--remote-latest-message-id))
    (should (appkit-chat-history-newer-stalled-p))
    (disco-room--observe-live-create "600")
    (should (equal "600" disco-room--remote-latest-message-id))
    (should-not (appkit-chat-history-newer-stalled-p))))

(ert-deftest disco-room-empty-window-shows-pending-then-seeds-canonical-create ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-establish-empty)
    (disco-state-insert-pending-message "chan" "250" "pending" "u1")
    (should (equal '("250")
                   (mapcar #'disco-room--message-id
                           (disco-room--display-messages))))
    (disco-state-upsert-message
     "chan"
     '((id . "300") (nonce . "250") (channel_id . "chan")))
    (disco-room--observe-live-create "300")
    (should-not (appkit-chat-history-window-empty-p))
    (should (equal "300" (appkit-chat-history-window-first-key)))
    (should-not (appkit-chat-history-window-last-key))
    (should (equal '("300")
                   (mapcar #'disco-room--message-id
                           (disco-room--display-messages))))))

(ert-deftest disco-room-pending-upsert-preserves-large-exact-window-edge ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (let ((messages
           (cl-loop for id from 100 downto 51
                    collect `((id . ,(number-to-string id))
                              (channel_id . "chan")))))
      (disco-state-put-messages "chan" messages)
      (setq disco-room--remote-latest-message-id "100")
      (appkit-chat-history-window-set "51" nil)
      (disco-state-insert-pending-message "chan" "local-1" "draft" "u1")
      (let ((display (disco-room--display-messages)))
        (should (= 51 (length (disco-state-messages "chan"))))
        (should (member "51" (mapcar #'disco-room--message-id display)))
        (should (member "local-1"
                        (mapcar #'disco-room--message-id display)))))))

(ert-deftest disco-room-footer-history-delimiter-is-passive ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (setq fill-column 24)
    (appkit-chat-history-window-set "100" "300")
    (let ((footer (disco-room--footer-text)))
      (should (string-match-p "····" footer))
      (with-temp-buffer
        (insert footer)
        (should-not (next-button (point-min)))))))

(ert-deftest disco-room-installs-view-owned-scroll-observer ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-set "100" "300")
    (let ((disco-room-history-auto-load-threshold 100)
          install-owner
          install-args
          calls)
      (cl-letf (((symbol-function 'appkit-scroll-observer-install)
                 (lambda (owner &rest args)
                   (setq install-owner owner
                         install-args args)
                   'observer))
                ((symbol-function 'appkit-chatbuf-point-in-input-p)
                 (lambda (&optional _position) nil))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'disco-room-load-older-messages)
                 (lambda (&optional quiet) (push (list 'older quiet) calls)))
                ((symbol-function 'disco-room-load-newer-messages)
                 (lambda (&optional quiet) (push (list 'newer quiet) calls))))
        (disco-room--install-scroll-observer 'view)
        (should (eq 'view install-owner))
        (should
         (eq #'appkit-chat-timeline-footer-start-position
             (plist-get install-args :end-boundary-function)))
        (funcall (plist-get install-args :start-function) 'window 50 1)
        (funcall (plist-get install-args :end-function) 'window 950 1000)
        (should (equal '((newer t) (older t)) calls))))))

(ert-deftest disco-room-scroll-observer-callbacks-respect-client-gates ()
  (let ((app
         (appkit-start-app
          'disco :id (make-symbol "scroll-gates") :shutdown #'ignore)))
    (unwind-protect
        (with-temp-buffer
          (disco-room-mode)
          (disco-room-test-setup-channel)
          (appkit-chat-history-window-set "100" "300")
          (let ((disco-room-history-auto-load-threshold 100)
                (composer-idle-p t)
                (view
                 (appkit-attach-view
                  :app app :id '(room "chan") :state "chan"
                  :mode major-mode))
                calls)
            (cl-letf (((symbol-function 'appkit-chatbuf-composer-idle-p)
                       (lambda () composer-idle-p))
                      ((symbol-function
                        'appkit-chat-timeline-footer-start-position)
                       (lambda () 1000))
                      ((symbol-function 'disco-room-load-newer-messages)
                       (lambda (&optional quiet) (push quiet calls))))
              ;; The viewport is still far from the footer.
              (disco-room--maybe-auto-load-newer 800)
              (should-not calls)
              ;; An active filter suppresses history paging.
              (setq disco-room--msg-filter '(:active t :query "needle"))
              (disco-room--maybe-auto-load-newer 950)
              (should-not calls)
              ;; An active composer interaction also suppresses paging.
              (setq disco-room--msg-filter nil
                    composer-idle-p nil)
              (disco-room--maybe-auto-load-newer 950)
              (should-not calls)
              ;; Appkit's shared loading gate suppresses overlapping requests.
              (setq composer-idle-p t)
              (let ((owner
                     (appkit-chat-history-request-start view 'newer)))
                (unwind-protect
                    (progn
                      (disco-room--maybe-auto-load-newer 950)
                      (should-not calls))
                  (appkit-chat-history-request-end owner))))))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest disco-room-sync-rechecks-scroll-observer-after-projection ()
  (let ((invalidations (appkit-invalidations-create))
        (observer (appkit-scroll-observer--create))
        checks)
    (with-temp-buffer
      (setq-local disco-room--scroll-observer observer)
      (cl-letf (((symbol-function 'appkit-view-live-p) (lambda (_view) t))
                ((symbol-function 'appkit-scroll-observer-check)
                 (lambda (candidate &optional _window)
                   (should (eq candidate observer))
                   (setq checks (1+ (or checks 0))))))
        (disco-room--sync-invalidations 'view invalidations nil)
        (should (= 1 checks))))))

(ert-deftest disco-room-delete-message-errors-without-manage-messages ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (let ((msg '((id . "m2")
                 (channel_id . "chat")
                 (content . "body")
                 (author . ((id . "u2") (username . "bob"))))))
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg))
                ((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1")))
        (should-error (disco-room-delete-message) :type 'user-error)))))

(ert-deftest disco-room-toggle-message-spoilers-switches-active-message ()
  (with-temp-buffer
    (let ((disco-room--revealed-spoiler-message-id nil)
          invalidated)
      (cl-letf (((symbol-function 'disco-room--invalidate-message-node)
                 (lambda (message-id)
                   (push message-id invalidated)
                   t)))
        (disco-room-toggle-message-spoilers "m1")
        (should (equal "m1" disco-room--revealed-spoiler-message-id))
        (should (equal '("m1") invalidated))
        (setq invalidated nil)
        (disco-room-toggle-message-spoilers "m2")
        (should (equal "m2" disco-room--revealed-spoiler-message-id))
        (should (equal '("m2" "m1") invalidated))
        (setq invalidated nil)
        (disco-room-toggle-message-spoilers "m2")
        (should-not disco-room--revealed-spoiler-message-id)
        (should (equal '("m2") invalidated))))))

(ert-deftest disco-room-avatar-is-a-projected-message-dependency ()
  (let ((message
         '((id . "message")
           (author . ((id . "user") (avatar . "avatar-hash"))))))
    (cl-letf (((symbol-function 'disco-avatar-resource-key)
               (lambda (user)
                 (should (equal "user" (alist-get 'id user)))
                 '(:avatar "avatar-key"))))
      (should (member '(:avatar "avatar-key")
                      (disco-room--message-dependency-keys message))))))

(ert-deftest disco-room-attachment-previews-are-projected-message-dependencies ()
  (let ((attachment '((id . "attachment") (filename . "image.png"))))
    (cl-letf (((symbol-function 'disco-media-attachment-download-key)
               (lambda (_attachment) "download-key"))
              ((symbol-function 'disco-media-attachment-preview-cache-key)
               (lambda (_attachment) "preview-key"))
              ((symbol-function 'disco-embed-message-preview-cache-keys)
               (lambda (_message) '("embed-preview-key"))))
      (let ((dependencies
             (disco-room--message-dependency-keys
              `((id . "message") (attachments . (,attachment))))))
        (should (member '(:attachment "download-key") dependencies))
        (should (member '(:preview "preview-key") dependencies))
        (should (member '(:preview "embed-preview-key") dependencies))))))

(ert-deftest disco-room-pin-message-requires-pin-messages-permission ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "pin-permission")
    (let ((msg '((id . "m1") (pinned . :false))))
      (should (equal "missing PIN_MESSAGES"
                     (disco-room--pin-message-unavailable-reason msg)))
      (should-error (disco-room--toggle-pin-on-msg msg) :type 'user-error))))

(ert-deftest disco-room-pin-message-commits-current-success-only ()
  (let ((disco-runtime--app nil)
        callback
        request)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "pin-success")
            (disco-state-upsert-channel
             '((id . "pin-success") (type . 0)
               (permissions . "2251799813685248")))
            (disco-state-put-messages
             "pin-success"
             '(((id . "m1") (channel_id . "pin-success")
                (content . "hello") (pinned . :false))))
            (let ((view (disco-room--ensure-view)))
              (cl-letf (((symbol-function 'disco-api-pin-message-async)
                         (lambda (_channel-id _message-id &rest options)
                           (setq callback (plist-get options :on-success))))
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest options)
                           (setq request (cons owner options))))
                        ((symbol-function 'message) #'ignore))
                (disco-room-toggle-pin "m1")
                (funcall callback nil))
              (should (eq view (car request)))
              (should (equal "m1" (plist-get (cdr request) :entry)))
              (should (eq t (alist-get 'pinned
                                       (disco-room--message-by-id "m1"))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-pin-message-supersedes-stale-toggle-callback ()
  (let ((disco-runtime--app nil)
        pin-callback
        unpin-callback
        (requests 0))
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "pin-race")
            (disco-state-upsert-channel
             '((id . "pin-race") (type . 0)
               (permissions . "2251799813685248")))
            (disco-state-put-messages
             "pin-race"
             '(((id . "m1") (channel_id . "pin-race")
                (content . "hello") (pinned . :false))))
            (disco-room--ensure-view)
            (cl-letf (((symbol-function 'disco-api-pin-message-async)
                       (lambda (_channel-id _message-id &rest options)
                         (setq pin-callback (plist-get options :on-success))))
                      ((symbol-function 'disco-api-unpin-message-async)
                       (lambda (_channel-id _message-id &rest options)
                         (setq unpin-callback (plist-get options :on-success))))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (&rest _options) (cl-incf requests)))
                      ((symbol-function 'message) #'ignore))
              (disco-room-toggle-pin "m1")
              (disco-room-toggle-pin "m1")
              (should (functionp pin-callback))
              (should (functionp unpin-callback))
              (funcall pin-callback nil)
              (should (= 0 requests))
              (should (eq :false (alist-get 'pinned
                                             (disco-room--message-by-id "m1"))))
              (funcall unpin-callback nil)
              (should (= 1 requests))
              (should (eq :false (alist-get 'pinned
                                             (disco-room--message-by-id "m1"))))))
        (disco-runtime-stop)))))
(provide 'disco-room-test)

;;; disco-room-test.el ends here
