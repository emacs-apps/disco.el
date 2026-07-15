;;; disco-room-test.el --- Tests for disco-room pin ack flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-room)
(require 'disco-state)

(defun disco-room-test-establish-latest-window (&optional channel-id)
  "Establish a known latest window for CHANNEL-ID's canonical fixture rows."
  (let* ((channel-id (or channel-id disco-room--channel-id))
         (messages
          (disco-room--normalize-history-page
           (disco-state-messages channel-id)))
         (newest (disco-room--message-id (car messages)))
         (oldest (disco-room--message-id (car (last messages)))))
    (setq disco-room--remote-latest-message-id newest)
    (if newest
        (appkit-chat-history-window-set oldest nil)
      (appkit-chat-history-window-establish-empty))))

(defun disco-room-test-setup-channel (&optional channel-id)
  "Reset state and bind the current room buffer to CHANNEL-ID."
  (let ((channel-id (or channel-id "chan")))
    (disco-state-reset)
    (setq-local disco-room--channel-id channel-id)
    (setq-local disco-room--channel-name channel-id)
    (disco-state-upsert-channel
     `((id . ,channel-id)
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    channel-id))

(defun disco-room-test-poll-message (&optional channel-id)
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

(ert-deftest disco-room-mode-is-not-special-mode ()
  (with-temp-buffer
    (disco-room-mode)
    (should-not (derived-mode-p 'special-mode))))

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
              ((symbol-function 'disco-room--on-window-size-change) #'ignore)
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
                    (appkit-chat-history-request-begin 'older '(old-owner)))
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

(ert-deftest disco-room-video-action-cannot-rebind-to-replacement-app ()
  (let ((disco-runtime--app nil)
        (disco-room-use-rich-attachment-cards t)
        (disco-media-show-previews nil)
        old-app
        replacement-app
        play-action
        played-owner)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore)
              ((symbol-function 'appkit-media-play-video-url)
               (lambda (_url _label &rest options)
                 (setq played-owner (plist-get options :owner))
                 :player)))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "video-owner")
            (disco-state-put-messages
             "video-owner"
             '(((id . "m-video")
                (channel_id . "video-owner")
                (content . "")
                (attachments
                 . (((id . "a-video")
                     (filename . "clip.mp4")
                     (content_type . "video/mp4")
                     (url . "https://example.invalid/clip.mp4")))))))
            (disco-room-test-establish-latest-window "video-owner")
            (disco-room-render)
            (let ((old-view (appkit-current-view)))
              (setq old-app (appkit-view-app old-view))
              (goto-char (point-min))
              (search-forward "clip.mp4")
              (setq play-action
                    (plist-get
                     (get-text-property
                      (match-beginning 0) appkit-media-card-context-property)
                     :open-action))
              (should (functionp play-action))
              (appkit-stop-app old-app)
              (should-not (appkit-app-live-p old-app))
              (setq replacement-app (disco-runtime-app))
              (should (appkit-app-live-p replacement-app))
              (should-not (eq old-app replacement-app))
              (should (equal (appkit-app-id old-app)
                             (appkit-app-id replacement-app)))
              (funcall play-action)
              (should (eq old-app played-owner))
              (should-not (eq replacement-app played-owner))))
        (when (appkit-app-live-p replacement-app)
          (appkit-stop-app replacement-app))))))

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
              ((symbol-function 'disco-room--on-window-size-change) #'ignore)
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

(ert-deftest disco-room-filter-callback-only-requests-appkit-sync ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "filter-boundary")
            (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                       (lambda (&rest args)
                         (setq callback (plist-get args :on-success))))
                      ((symbol-function 'disco-room-render)
                       (lambda ()
                         (ert-fail "filter path rendered directly")))
                      ((symbol-function 'appkit-sync-invalidations)
                       (lambda (&rest _args)
                         (ert-fail "filter callback synced directly")))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (view &rest args)
                         (push (cons view args) requests)))
                      ((symbol-function 'message) #'ignore))
              (disco-room-search--run-filter '(:query "needle"))
              (should (= 1 (length requests)))
              (setq requests nil)
              (funcall callback
                       '((total_results . 1)
                         (messages (((id . "result")
                                     (channel_id . "filter-boundary"))))))
              (should (= 1 (length requests)))
              (should (equal "result"
                             (alist-get
                              'id
                              (car (plist-get disco-room--msg-filter :items)))))))
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
              (cl-letf (((symbol-function 'appkit-chatbuf-post-command-clamp-point)
                         #'ignore)
                        ((symbol-function 'appkit-chatbuf-point-in-input-p)
                         (lambda () nil))
                        ((symbol-function 'disco-room--update-context-mode) #'ignore)
                        ((symbol-function 'disco-room--maybe-auto-load-newer) #'ignore)
                        ((symbol-function 'disco-room--maybe-auto-load-older) #'ignore)
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

(ert-deftest disco-room-media-visual-callback-only-requests-geometry-sync ()
  (let ((disco-runtime--app nil)
        request)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "media-visual-boundary")
            (let ((view (disco-room--ensure-view)))
              (cl-letf (((symbol-function 'buffer-list)
                         (lambda () (list (current-buffer))))
                        ((symbol-function 'disco-room-render)
                         (lambda ()
                           (ert-fail "media callback rendered directly")))
                        ((symbol-function 'disco-room--update-frame)
                         (lambda (&rest _args)
                           (ert-fail "media callback updated frame directly")))
                        ((symbol-function 'disco-room--refresh-timeline-layout)
                         (lambda ()
                           (ert-fail "media callback refreshed layout directly")))
                        ((symbol-function 'appkit-sync-invalidations)
                         (lambda (&rest _args)
                           (ert-fail "media callback synced directly")))
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest args)
                           (setq request (cons owner args)))))
                (disco-room--handle-media-rerender 'visual nil)
                (should (eq view (car request)))
                (should (eq 'geometry (plist-get (cdr request) :part))))))
        (disco-runtime-stop)))))

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

(ert-deftest disco-room-poll-rest-success-and-self-echo-are-idempotent ()
  (let ((disco-runtime--app nil)
        success-callback)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "poll-echo")
            (disco-state-put-messages
             "poll-echo" (list (disco-room-test-poll-message "poll-echo")))
            (disco-room--ensure-view)
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
        (disco-runtime-stop)))))

(ert-deftest disco-room-poll-self-echo-before-rest-completion-is-authoritative ()
  (let ((disco-runtime--app nil)
        success-callback
        (requests 0))
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "poll-echo-first")
            (disco-state-put-messages
             "poll-echo-first"
             (list (disco-room-test-poll-message "poll-echo-first")))
            (disco-room--ensure-view)
            (disco-room--poll-set-draft-selection "p1" '(1))
            (cl-letf (((symbol-function 'disco-api-create-poll-vote-async)
                       (lambda (_channel-id _message-id _answer-ids &rest args)
                         (setq success-callback
                               (plist-get args :on-success))))
                      ((symbol-function 'disco-gateway-current-user-id)
                       (lambda () "self"))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (&rest _args) (cl-incf requests)))
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
              (should (= 0 requests))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-queued-poll-echo-keeps-frozen-self-identity ()
  (let ((disco-runtime--app nil)
        (disco-gateway--current-user-id "self")
        emitted)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "poll-queued-self")
            ;; This is the state after the matching REST success.
            (disco-state-put-messages
             "poll-queued-self"
             (list
              (disco-room--message-with-poll-vote-selection
               (disco-room-test-poll-message "poll-queued-self")
               '(1))))
            (disco-room--poll-set-draft-selection "p1" '(1))
            (let ((view (disco-room--ensure-view))
                  (op-token (disco-room--poll-vote-op-begin "p1" '(1))))
              (cl-letf (((symbol-function 'disco-gateway--emit)
                         (lambda (event) (setq emitted event))))
                (disco-gateway--dispatch-message-poll-vote-add
                 '((channel_id . "poll-queued-self")
                   (message_id . "p1")
                   (user_id . "self")
                   (answer_id . 1))))
              (should (eq t (plist-get emitted :self-p)))
              (appkit-view-enqueue-event view emitted)
              (appkit-request-sync view :part 'timeline)
              (setq disco-gateway--current-user-id nil)
              (cl-letf (((symbol-function 'disco-room-render) #'ignore))
                (appkit-sync-invalidations view))
              (should-not
               (disco-room--poll-vote-op-current-p "p1" op-token))
              (should-not
               (disco-room--poll-draft-selection-present-p "p1")))
            (let ((poll (disco-msg-poll (disco-room--message-by-id "p1"))))
              (should (= 1 (disco-msg-poll-answer-count poll 1)))
              (should (equal '(1)
                             (disco-msg-poll-voted-answer-ids poll)))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-stale-poll-rest-success-cannot-overwrite-newer-op ()
  (let ((disco-runtime--app nil)
        callbacks
        (requests 0))
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "poll-generation")
            (disco-state-put-messages
             "poll-generation"
             (list (disco-room-test-poll-message "poll-generation")))
            (disco-room--ensure-view)
            (cl-letf (((symbol-function 'disco-api-create-poll-vote-async)
                       (lambda (_channel-id _message-id _answer-ids &rest args)
                         (setq callbacks
                               (append callbacks
                                       (list (plist-get args :on-success))))))
                      ((symbol-function 'appkit-request-sync)
                       (lambda (&rest _args) (cl-incf requests)))
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
              (should (= 1 requests))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-inplace-search-callback-only-queues-appkit-jump ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "search-boundary")
            (setq-local disco-room--newest-message-id "m9")
            (let ((view (disco-room--ensure-view)))
              (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                         (lambda (&rest args)
                           (setq callback (plist-get args :on-success))))
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest args)
                           (push (cons owner args) requests)))
                        ((symbol-function 'message) #'ignore))
                (disco-room--inplace-search-dispatch
                 '(:query "needle") nil "m9"))
              (should (functionp callback))
              (setq requests nil)
              (cl-letf (((symbol-function 'disco-room-jump-to-message)
                         (lambda (&rest _args)
                           (ert-fail "search callback jumped directly")))
                        ((symbol-function 'disco-room--update-frame)
                         (lambda (&rest _args)
                           (ert-fail "search callback updated frame directly")))
                        ((symbol-function 'disco-room-render)
                         (lambda ()
                           (ert-fail "search callback rendered directly")))
                        ((symbol-function 'disco-room--sync-timeline)
                         (lambda (&rest _args)
                           (ert-fail "search callback projected timeline directly")))
                        ((symbol-function 'appkit-sync-invalidations)
                         (lambda (&rest _args)
                           (ert-fail "search callback synced directly")))
                        ((symbol-function 'appkit-request-sync)
                         (lambda (owner &rest args)
                           (push (cons owner args) requests)))
                        ((symbol-function 'message) #'ignore))
                (funcall callback
                         '((messages (((id . "m5")
                                       (channel_id . "search-boundary")
                                       (content . "needle"))))))
                (should (equal "m5" disco-room--pending-jump-message-id))
                (should (= 1 (length requests)))
                (should (eq view (caar requests)))
                (should (eq t (plist-get (cdar requests) :position)))
                (should (eq 'timeline (plist-get (cdar requests) :part))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-attachment-send-error-callback-restores-controller-only ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "attachment-send-boundary")
            (disco-state-upsert-channel
             `((id . "attachment-send-boundary")
               (type . 0)
               (guild_id . "g1")
               (permissions . ,(number-to-string
                                 (logior 2048 (ash 1 15))))))
            (let ((path (make-temp-file "disco-room-callback-attach")))
              (unwind-protect
                  (let* ((view (disco-room--ensure-view))
                         (draft
                          (concat
                           "hello "
                           (disco-room--attachment-input-object-string
                            (disco-room--make-attachment-input-object path)))))
                    (disco-room--set-draft draft)
                    (cl-letf (((symbol-function
                                'disco-api-send-message-with-attachments-async)
                               (lambda (_channel-id &rest args)
                                 (setq callback (plist-get args :on-error))))
                              ((symbol-function 'message) #'ignore))
                      (disco-room-send-message))
                    (should (functionp callback))
                    (should disco-room--send-in-flight)
                    (setq requests nil)
                    (cl-letf (((symbol-function 'disco-room--update-frame)
                               (lambda (&rest _args)
                                 (ert-fail "send callback updated frame directly")))
                              ((symbol-function 'disco-room-render)
                               (lambda ()
                                 (ert-fail "send callback rendered directly")))
                              ((symbol-function 'disco-room--sync-timeline)
                               (lambda (&rest _args)
                                 (ert-fail "send callback projected timeline directly")))
                              ((symbol-function 'appkit-chatbuf-input-replace)
                               (lambda (&rest _args)
                                 (ert-fail "send callback replaced live input directly")))
                              ((symbol-function 'appkit-sync-invalidations)
                               (lambda (&rest _args)
                                 (ert-fail "send callback synced directly")))
                              ((symbol-function 'appkit-request-sync)
                               (lambda (owner &rest args)
                                 (push (cons owner args) requests)))
                              ((symbol-function 'message) #'ignore))
                      (funcall callback '(:message "upload failed"))
                      (should-not disco-room--send-in-flight)
                      (should (= 1 (length requests)))
                      (should (eq view (caar requests)))
                      (should (plist-get (cdar requests) :structure))
                      (should (equal "hello "
                                     (disco-room--draft-without-attachment-tokens)))
                      (should (equal path
                                     (plist-get
                                      (car (disco-room--attachments-from-draft))
                                      :path)))))
                (delete-file path))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-edit-success-callback-restores-controller-only ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "edit-callback-boundary")
            (let ((message
                   '((id . "m1")
                     (channel_id . "edit-callback-boundary")
                     (content . "old body")
                     (author . ((id . "self") (username . "Me"))))))
              (disco-state-put-messages "edit-callback-boundary" (list message))
              (let ((view (disco-room--ensure-view)))
                (disco-room--set-draft "saved draft")
                (disco-room--composer-enter-edit message)
                (disco-room--set-draft "updated body")
                (cl-letf (((symbol-function 'disco-room--edit-permission-reason)
                           (lambda (&optional _msg) nil))
                          ((symbol-function 'disco-api-edit-message-async)
                           (lambda (_channel-id _message-id _content &rest args)
                             (setq callback (plist-get args :on-success))))
                          ((symbol-function 'message) #'ignore))
                  (disco-room-send-message))
                (should (functionp callback))
                (should disco-room--send-in-flight)
                (setq requests nil)
                (cl-letf (((symbol-function 'disco-room--update-frame)
                           (lambda (&rest _args)
                             (ert-fail "edit callback updated frame directly")))
                          ((symbol-function 'disco-room-render)
                           (lambda ()
                             (ert-fail "edit callback rendered directly")))
                          ((symbol-function 'disco-room--sync-timeline)
                           (lambda (&rest _args)
                             (ert-fail "edit callback projected timeline directly")))
                          ((symbol-function 'appkit-chatbuf-input-replace)
                           (lambda (&rest _args)
                             (ert-fail "edit callback replaced live input directly")))
                          ((symbol-function 'appkit-sync-invalidations)
                           (lambda (&rest _args)
                             (ert-fail "edit callback synced directly")))
                          ((symbol-function 'appkit-request-sync)
                           (lambda (owner &rest args)
                             (push (cons owner args) requests)))
                          ((symbol-function 'message) #'ignore))
                  (funcall callback
                           '((id . "m1")
                             (channel_id . "edit-callback-boundary")
                             (content . "updated body")
                             (author . ((id . "self") (username . "Me")))))
                  (should-not disco-room--send-in-flight)
                  (should-not (disco-room--composer-edit-active-p))
                  (should (equal "saved draft"
                                 (appkit-chatbuf-string-plain-text
                                  (disco-room--current-draft))))
                  (should (equal "updated body"
                                 (alist-get
                                  'content
                                  (car (disco-state-messages
                                        "edit-callback-boundary")))))
                  (should (= 1 (length requests)))
                  (should (eq view (caar requests)))
                  (should (plist-get (cdar requests) :structure))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-contextual-bindings-follow-point-location ()
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
    (disco-room-render)
    (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
    (disco-room--update-context-mode)
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
    (should-not (lookup-key disco-room-mode-map (kbd "C-c C-/")))
    (should (eq (key-binding (kbd "C-c C-e") t) 'disco-room-input-formatting-set))
    (should (eq (key-binding (kbd "C-c C-o") t) 'disco-room-input-options-transient))
    (should (eq (key-binding (kbd "C-c C-v") t) 'disco-room-attach-clipboard))
    (should (eq (key-binding (kbd "C-c M-v") t) 'disco-room-refetch-avatars))
    (should-not (lookup-key disco-room-mode-map (kbd "M-<")))
    (should-not (lookup-key disco-room-mode-map (kbd "M->")))
    (should (eq (key-binding (kbd "M-<") t) 'beginning-of-buffer))
    (should (eq (key-binding (kbd "M->") t) 'end-of-buffer))
    (goto-char (point-min))
    (disco-room--update-context-mode)
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
    (should (eq (key-binding (kbd "i") t) 'disco-msg-describe-message))
    (should (eq (key-binding (kbd "L") t) 'disco-msg-redisplay))
    (should (eq (key-binding (kbd "!") t) 'disco-msg-add-reaction))
    (should (eq (key-binding (kbd "+") t) 'disco-msg-toggle-reaction))
    (should (eq (key-binding (kbd "-") t) 'disco-msg-remove-reaction))
    (should (eq (key-binding (kbd "T") t) 'disco-msg-open-thread))
    (should (eq (key-binding (kbd "C-c C-a") t) 'disco-room-attach-transient))
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
    (should (eq (key-binding (kbd "C-c m i") t) 'disco-msg-describe-message))
    (should (eq (key-binding (kbd "C-c m L") t) 'disco-msg-redisplay))))

(ert-deftest disco-room-msg-layer-adapters-and-message-properties-are-installed ()
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
    (should (eq disco-msg-open-thread-function #'disco-room--open-thread-from-message))
    (should (eq disco-msg-toggle-reaction-function #'disco-room--toggle-reaction-on-msg))
    (should (eq disco-msg-add-reaction-function #'disco-room--add-reaction-to-msg))
    (should (eq disco-msg-remove-reaction-function #'disco-room--remove-reaction-from-msg))
    (should (eq disco-msg-redisplay-function #'disco-room--redisplay-msg))
    (goto-char (point-min))
    (search-forward "hello")
    (backward-char 2)
    (should (eq disco-msg-command-map (get-text-property (point) 'keymap)))
    (should (equal "m1" (get-text-property (point) 'disco-message-id)))
    (should (equal "chat" (get-text-property (point) 'disco-message-channel-id)))
    (should (equal "g1" (get-text-property (point) 'disco-message-guild-id)))
    (should (equal "m1" (alist-get 'id (disco-msg-at (point)))))))

(ert-deftest disco-room-message-command-map-does-not-clobber-link-keymaps ()
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
    (should (keymapp (get-text-property (point) 'keymap)))
    (should-not (eq disco-msg-command-map
                    (get-text-property (point) 'keymap)))))

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

(ert-deftest disco-room-draft-history-search-loads-match ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-chatbuf-input-history-push "deploy status")
    (appkit-chatbuf-input-history-push "hello world")
    (appkit-chatbuf-input-history-push "alpha beta")
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room-draft-history-search "hello"))
    (should (equal "hello world"
                   (appkit-chatbuf-string-plain-text
                    (disco-room--current-draft))))))

(ert-deftest disco-room-current-draft-tracks-live-input-through-state-sync ()
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
    (appkit-chatbuf-input-state-set "cached")
    (disco-room-render)
    (appkit-chatbuf-input-set-text "live")
    (should (equal "live"
                   (appkit-chatbuf-string-plain-text
                    (disco-room--current-draft))))))

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

(ert-deftest disco-room-return-completes-token-without-sending ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-chatbuf-install-prompt "> ")
    (insert "@ali")
    (let (completed sent)
      (cl-letf (((symbol-function 'disco-company-completion-token-at-point)
                 (lambda () '(:trigger ?@ :raw "@ali")))
                ((symbol-function 'disco-room-complete-mention)
                 (lambda () (setq completed t)))
                ((symbol-function 'disco-room-send-message)
                 (lambda () (setq sent t))))
        (disco-room-return-dwim))
      (should completed)
      (should-not sent)
      (should (equal "@ali" (appkit-chatbuf-input-string))))))

(ert-deftest disco-room-return-outside-composer-never-sends-draft ()
  (with-temp-buffer
    (disco-room-mode)
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt "> ")
    (insert "unsent draft")
    (goto-char (point-min))
    (let (sent)
      (cl-letf (((symbol-function 'disco-room-send-message)
                 (lambda () (setq sent t))))
        (disco-room-return-dwim))
      (should-not sent)
      (should (appkit-chatbuf-point-in-input-p))
      (should (= (point) (point-max)))
      (should (equal "unsent draft" (appkit-chatbuf-input-string))))))

(ert-deftest disco-room-image-attachment-uses-canonical-preview-object ()
  (let ((path (make-temp-file "disco-composer-preview" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "123456"))
          (cl-letf (((symbol-function
                      'appkit-media-one-line-preview-image-from-file)
                     (lambda (file &optional _max-width)
                       (should (equal file path))
                       '(:composer-preview)))
                    ((symbol-function 'appkit-media-image-display-string)
                     (lambda (image fallback)
                       (propertize fallback 'display image))))
            (let* ((object
                    (disco-room--make-attachment-input-object
                     path :filename "preview.png" :content-type "image/png"))
                   (text (disco-room--attachment-input-object-string object)))
              (should (string-match-p "\\[image\\]" text))
              (should (string-match-p "preview.png" text))
              (should (string-match-p "(6)" text))
              (should (equal object
                             (get-text-property
                              0 appkit-chatbuf-input-object-property text)))
              (should (equal
                       (substring-no-properties text 0 (1- (length text)))
                       (get-text-property
                        0 appkit-chatbuf-input-object-text-property text)))
              (should (get-text-property
                       (1- (length text))
                       appkit-chatbuf-input-object-end-property text)))))
      (ignore-errors (delete-file path)))))

(ert-deftest disco-room-equal-adjacent-objects-parse-as-two-attachments ()
  (let* ((object
          (disco-room--make-attachment-input-object
           "/tmp/reused.png" :filename "reused.png"))
         (label "[image] reused.png")
         (draft
          (concat (appkit-chatbuf-input-object-string label object)
                  (appkit-chatbuf-input-object-string label object)))
         (parsed (disco-room--parse-draft-input draft)))
    (should (= 2 (length (plist-get parsed :objects))))
    (should (= 2 (length (plist-get parsed :attachments))))
    (should (equal "" (plist-get parsed :content)))))

(ert-deftest disco-room-attachment-insertion-inside-object-keeps-both-atomic ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt "> ")
    (let ((first (disco-room--make-attachment-input-object
                  "/tmp/first.png" :filename "first.png"))
          (second (disco-room--make-attachment-input-object
                   "/tmp/second.png" :filename "second.png")))
      (disco-room--insert-attachment-input-object first)
      (goto-char (1+ (appkit-chatbuf-input-start-position)))
      (disco-room--insert-attachment-input-object second)
      (appkit-chatbuf-input-prune-broken-objects)
      (let ((parsed (disco-room--parse-draft-input
                     (appkit-chatbuf-input-string))))
        (should (= 2 (length (plist-get parsed :attachments))))
        (should (equal '("first.png" "second.png")
                       (mapcar (lambda (attachment)
                                 (plist-get attachment :filename))
                               (plist-get parsed :attachments))))))))

(ert-deftest disco-room-draft-history-prev-next-restores-structured-pending-draft ()
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
    (let ((path (make-temp-file "disco-room-history"))
          (render-calls 0))
      (unwind-protect
          (progn
            (appkit-chatbuf-input-history-push "older draft")
            (disco-room--set-draft
             (concat "pending "
                     (disco-room--attachment-input-object-string
                      (disco-room--make-attachment-input-object path :filename "a.txt"))))
            (let ((orig-update (symbol-function 'disco-room--update-frame)))
              (cl-letf (((symbol-function 'disco-room--update-frame)
                         (lambda (&rest args)
                           (setq render-calls (1+ render-calls))
                           (apply orig-update args))))
                (disco-room-draft-prev)
                (should (equal "older draft"
                               (appkit-chatbuf-string-plain-text
                                (disco-room--current-draft))))
                (should-not (appkit-chatbuf-string-has-objects-p
                             (disco-room--current-draft)))
                (disco-room-draft-next)
                (should (appkit-chatbuf-string-has-objects-p
                         (disco-room--current-draft)))
                (should (= 1 (length disco-room--pending-attachments)))
                (should (equal "a.txt"
                               (plist-get (car disco-room--pending-attachments)
                                          :filename)))
                (should (= 2 render-calls)))))
        (delete-file path)))))

(ert-deftest disco-room-reply-and-cancel-sync-shared-aux-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (cl-letf (((symbol-function 'disco-room--ensure-action-available)
               (lambda (&rest _args) nil))
              ((symbol-function 'disco-room--update-frame)
               (lambda (&rest _args) nil))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room-reply-to-message "m42")
      (should (equal "m42" disco-room--pending-reply-to))
      (should (eq 'reply (appkit-chatbuf-aux-type)))
      (should (equal "m42" (appkit-chatbuf-aux-message-id)))
      (disco-room-cancel-reply)
      (should-not disco-room--pending-reply-to)
      (should-not (appkit-chatbuf-aux-active-p)))))

(ert-deftest disco-room-input-options-use-shared-chatbuf-state-only ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-chatbuf-input-options-set
     '(:send-on-return t
       :long-message-action file
       :allowed-mentions none
       :reply-mention-replied-user t))
    (setq-local disco-room-send-on-return nil)
    (setq-local disco-room-long-message-action 'split)
    (setq-local disco-room-allowed-mentions 'all)
    (setq-local disco-room-reply-mention-replied-user nil)
    (should (eq t (plist-get (disco-room--input-options-state) :send-on-return)))
    (should (disco-room--input-option-send-on-return))
    (should (eq 'file (disco-room--input-option-long-message-action)))
    (should (eq 'none (disco-room--input-option-allowed-mentions)))
    (should (disco-room--input-option-reply-mention-replied-user))))

(ert-deftest disco-room-input-options-write-path-syncs-shared-state ()
  (with-temp-buffer
    (disco-room-mode)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room-toggle-send-on-return)
      (should-not disco-room-send-on-return)
      (should-not (disco-room--input-option-send-on-return))
      (disco-room-cycle-long-message-action)
      (should (eq 'file disco-room-long-message-action))
      (should (eq 'file (disco-room--input-option-long-message-action)))
      (disco-room-cycle-allowed-mentions)
      (should (eq 'none disco-room-allowed-mentions))
      (should (eq 'none (disco-room--input-option-allowed-mentions)))
      (disco-room-toggle-reply-mention-replied-user)
      (should disco-room-reply-mention-replied-user)
      (should (disco-room--input-option-reply-mention-replied-user))
      (disco-room-reset-input-options)
      (should (equal (disco-room--current-input-options-state)
                     (disco-room--input-options-state))))))

(ert-deftest disco-room-composer-aux-state-uses-shared-chatbuf-state-only ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room--set-composer-aux-state nil "m1")
    (appkit-chatbuf-aux-set
     '(:aux-type reply :message-id "m1" :aux-msg ((id . "m1") (content . "shared"))))
    (let ((aux (appkit-chatbuf-aux-state)))
      (should (eq 'reply (plist-get aux :aux-type)))
      (should (equal "m1" (plist-get aux :message-id)))
      (should (equal "shared"
                     (alist-get 'content (plist-get aux :aux-msg)))))
    (setq-local disco-room--pending-reply-to "m2")
    (let ((aux (appkit-chatbuf-aux-state)))
      (should (equal "m1" (plist-get aux :message-id)))
      (should (equal "shared"
                     (alist-get 'content (plist-get aux :aux-msg)))))
    (appkit-chatbuf-aux-reset)
    (should-not (appkit-chatbuf-aux-state))))

(ert-deftest disco-room-input-preview-renders-parsed-attachments ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (let ((path (make-temp-file "disco-room-preview"))
          (preview-buf nil))
      (unwind-protect
          (progn
            (disco-room--set-draft
             (concat "hello "
                     (disco-room--attachment-input-object-string
                      (disco-room--make-attachment-input-object path :description "preview"))))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buffer-or-name &rest _args)
                         (setq preview-buf (get-buffer buffer-or-name))
                         preview-buf))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-input-preview))
            (should (buffer-live-p preview-buf))
            (with-current-buffer preview-buf
              (should (string-match-p "Composer mode: message" (buffer-string)))
              (should (string-match-p "hello" (buffer-string)))
              (should (string-match-p "preview" (buffer-string)))))
        (when (buffer-live-p preview-buf)
          (kill-buffer preview-buf))
        (delete-file path)))))

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
      (should (string-match-p "> source edited" (buffer-string))))))

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
    (should (disco-util-json-true-p
             (plist-get (appkit-chat-timeline-context "m2")
                        :insert-unread)))
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
      (should (disco-util-json-true-p
               (plist-get (appkit-chat-timeline-context "m3")
                          :insert-unread))))))

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
    (should (disco-util-json-true-p
             (plist-get (appkit-chat-timeline-context "m2")
                        :insert-unread)))
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
    (should (disco-util-json-true-p
             (plist-get (appkit-chat-timeline-context "m2")
                        :insert-unread)))
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

(ert-deftest disco-room-search-channel-opens-root-search-transient ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "c1")
          passed-channel)
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "c1") (type . 1) (name . "dm")))
      (cl-letf (((symbol-function 'disco-root-search-channel-transient)
                 (lambda (channel)
                   (setq passed-channel channel))))
        (disco-room-search-channel)
        (should (equal "c1" (alist-get 'id passed-channel)))))))

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
    (let (older-callback latest-callback)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (if (plist-get args :before)
                       (setq older-callback (plist-get args :on-success))
                     (setq latest-callback (plist-get args :on-success)))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'disco-room--mark-read) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-load-older-messages t)
        (let ((older-owner (appkit-chat-history-request-owner)))
          (disco-room-refresh)
          (should-not
           (appkit-chat-history-request-current-p older-owner)))
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

(ert-deftest disco-room-filter-live-create-advances-hidden-frontier ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300"
          disco-room--msg-filter
          '(:active t
            :query "needle"
            :items (((id . "200") (channel_id . "chan")))))
    (appkit-chat-history-window-set "100" "300")
    (disco-room-render)
    (disco-state-upsert-message
     "chan" '((id . "400") (channel_id . "chan")))
    (let (read-id)
      (cl-letf (((symbol-function 'disco-room--mark-read)
                 (lambda (&optional id) (setq read-id id))))
        (disco-room--apply-gateway-event
         '(:type message-create :channel-id "chan"
           :message ((id . "400") (channel_id . "chan")))))
      (should-not read-id))
    (should (equal "400" disco-room--remote-latest-message-id))
    (should (equal "100" (appkit-chat-history-window-first-key)))
    (should (equal "300" (appkit-chat-history-window-last-key)))
    (should (equal '("200")
                   (mapcar #'disco-room--message-id
                           (disco-room--display-messages))))))

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

(ert-deftest disco-room-filter-live-delete-invalidates-hidden-edge ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    ;; Gateway state mutation precedes room event delivery, so the deleted
    ;; frontier is already absent from the canonical cache here.
    (disco-state-put-messages
     "chan"
     '(((id . "200") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300"
          disco-room--msg-filter
          '(:active t
            :query "needle"
            :items (((id . "200") (channel_id . "chan")))))
    (appkit-chat-history-window-set "100" "300")
    (let ((owner (appkit-chat-history-request-begin 'latest)))
      (disco-room--apply-gateway-event
       '(:type message-delete :channel-id "chan" :message-id "300"))
      (should-not (appkit-chat-history-request-current-p owner)))
    (should (equal "200" disco-room--remote-latest-message-id))
    (should-not (appkit-chat-history-loading-p))
    (should-not (appkit-chat-history-window-known-p))))

(ert-deftest disco-room-filter-delete-removes-result-and-rejects-load-more ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (setq disco-room--msg-filter
          '(:active t
            :query "needle"
            :items (((id . "200") (channel_id . "chan")))
            :total-count 2
            :has-more t))
    (appkit-chat-history-window-establish-empty)
    (let (success-callback)
      (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                 (lambda (&rest args)
                   (setq success-callback (plist-get args :on-success))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-filter-load-more)
        (should disco-room--filter-in-flight)
        (disco-room--apply-gateway-event
         '(:type message-delete :channel-id "chan" :message-id "200"))
        (should-not disco-room--filter-in-flight)
        (should-not (plist-get disco-room--msg-filter :items))
        (should (= 1 (plist-get disco-room--msg-filter :total-count)))
        (funcall success-callback
                 '((total_results . 2)
                   (messages (((id . "300") (channel_id . "chan"))))))
        (should-not (plist-get disco-room--msg-filter :items))))))

(ert-deftest disco-room-filter-cancel-refreshes-invalidated-history-window ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (setq disco-room--msg-filter
          '(:active t
            :query "needle"
            :items (((id . "200") (channel_id . "chan")))))
    (appkit-chat-history-window-clear)
    (let (refreshed rendered)
      (cl-letf (((symbol-function 'disco-room-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'disco-room-render)
                 (lambda () (setq rendered t)))
                ((symbol-function 'message) #'ignore))
        (disco-room-filter-cancel))
      (should refreshed)
      (should rendered)
      (should-not disco-room--msg-filter))))

(ert-deftest disco-room-filter-cancel-jumps-to-cached-item-outside-window ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "900") (channel_id . "chan"))
       ((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--msg-filter
          '(:active t
            :query "needle"
            :items (((id . "900") (channel_id . "chan")))))
    (appkit-chat-history-window-set "100" "300")
    (let (jumped rendered)
      (cl-letf (((symbol-function 'disco-room--message-id-at-point)
                 (lambda () "900"))
                ((symbol-function 'disco-room-render)
                 (lambda () (setq rendered t)))
                ((symbol-function 'disco-room-jump-to-message)
                 (lambda (message-id &optional _channel-id)
                   (setq jumped message-id)))
                ((symbol-function 'message) #'ignore))
        (disco-room-filter-cancel))
      (should rendered)
      (should (equal "900" jumped)))))

(ert-deftest disco-room-filter-cancel-rejects-late-success ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-establish-empty)
    (let (success-callback)
      (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                 (lambda (&rest args)
                   (setq success-callback (plist-get args :on-success))))
                ((symbol-function 'disco-room-render) #'ignore)
                ((symbol-function 'message) #'ignore))
        (disco-room-search--run-filter '(:query "needle"))
        (should disco-room--filter-in-flight)
        (disco-room-filter-cancel)
        (should-not disco-room--msg-filter)
        (funcall success-callback
                 '((total_results . 1)
                   (messages (((id . "200") (channel_id . "chan"))))))
        (should-not disco-room--msg-filter)
        (should-not disco-room--filter-in-flight)))))

(ert-deftest disco-room-filter-edge-delete-rejects-inflight-history ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (disco-state-put-messages
     "chan"
     '(((id . "300") (channel_id . "chan"))
       ((id . "100") (channel_id . "chan"))))
    (setq disco-room--remote-latest-message-id "300")
    (appkit-chat-history-window-set "100" "300")
    (let (old-success refreshed)
      (cl-letf (((symbol-function 'disco-api-channel-messages-async)
                 (lambda (_channel-id &rest args)
                   (setq old-success (plist-get args :on-success))))
                ((symbol-function 'disco-room--update-frame) #'ignore))
        (disco-room-refresh))
      (setq disco-room--msg-filter
            '(:active t
              :query "needle"
              :items (((id . "200") (channel_id . "chan")))))
      ;; Mirror Gateway ordering: canonical deletion happens before delivery.
      (disco-state-put-messages
       "chan"
       '(((id . "200") (channel_id . "chan"))
         ((id . "100") (channel_id . "chan"))))
      (disco-room--apply-gateway-event
       '(:type message-delete :channel-id "chan" :message-id "300"))
      (cl-letf (((symbol-function 'disco-room-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'message) #'ignore))
        (disco-room-filter-cancel))
      (funcall old-success
               '(((id . "300") (channel_id . "chan"))
                 ((id . "100") (channel_id . "chan"))))
      (should refreshed)
      (should-not (appkit-chat-history-window-known-p))
      (should (equal "200" disco-room--remote-latest-message-id)))))

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
    (setq disco-room--chat-fill-column 24)
    (appkit-chat-history-window-set "100" "300")
    (let ((footer (disco-room--footer-text)))
      (should (string-match-p "····" footer))
      (with-temp-buffer
        (insert footer)
        (should-not (next-button (point-min)))))))

(ert-deftest disco-room-history-autoload-respects-filter-and-both-edges ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-set "100" "300")
    (let ((disco-room-history-auto-load-threshold 100)
          calls)
      (cl-letf (((symbol-function 'appkit-chatbuf-point-in-input-p)
                 (lambda (&optional _position) nil))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'disco-room-load-older-messages)
                 (lambda (&optional quiet) (push (list 'older quiet) calls)))
                ((symbol-function 'disco-room-load-newer-messages)
                 (lambda (&optional quiet) (push (list 'newer quiet) calls))))
        (goto-char (point-min))
        (disco-room--maybe-auto-load-older)
        (disco-room--maybe-auto-load-newer 950)
        (should (equal '((newer t) (older t)) calls))
        (setq calls nil
              disco-room--msg-filter '(:active t :query "needle"))
        (disco-room--maybe-auto-load-older)
        (disco-room--maybe-auto-load-newer 950)
        (should-not calls)))))

(ert-deftest disco-room-window-scroll-autoloads-from-selected-viewport-end ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-set "100" "300")
    (let ((disco-room-history-auto-load-threshold 100)
          (window (selected-window))
          calls)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer)))
                ((symbol-function
                  'appkit-chat-timeline-window-visible-end-position)
                 (lambda (candidate)
                   (should (eq candidate window))
                   950))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'disco-room-load-newer-messages)
                 (lambda (&optional quiet) (push quiet calls))))
        (disco-room--window-scroll window 1)
        (should (equal '(t) calls))))))

(ert-deftest disco-room-window-scroll-autoloads-from-inactive-viewport-end ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-set "100" "300")
    (let ((disco-room-history-auto-load-threshold 100)
          (window 'inactive-room-window)
          calls)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer)))
                ((symbol-function
                  'appkit-chat-timeline-window-visible-end-position)
                 (lambda (_window) 950))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'disco-room-load-newer-messages)
                 (lambda (&optional quiet) (push quiet calls))))
        (disco-room--window-scroll window 1)
        (should (equal '(t) calls))))))

(ert-deftest disco-room-window-scroll-newer-respects-viewport-and-client-gates ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel)
    (appkit-chat-history-window-set "100" "300")
    (let ((disco-room-history-auto-load-threshold 100)
          (window 'room-window)
          (visible-end 800)
          (composer-idle-p t)
          calls)
      (cl-letf (((symbol-function 'window-live-p) (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) (current-buffer)))
                ((symbol-function
                  'appkit-chat-timeline-window-visible-end-position)
                 (lambda (_window) visible-end))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () composer-idle-p))
                ((symbol-function 'disco-room-load-newer-messages)
                 (lambda (&optional quiet) (push quiet calls))))
        ;; The viewport is still far from the footer.
        (disco-room--window-scroll window 1)
        (should-not calls)
        ;; An active filter suppresses history paging.
        (setq visible-end 950
              disco-room--msg-filter '(:active t :query "needle"))
        (disco-room--window-scroll window 1)
        (should-not calls)
        ;; An active composer interaction also suppresses paging.
        (setq disco-room--msg-filter nil
              composer-idle-p nil)
        (disco-room--window-scroll window 1)
        (should-not calls)
        ;; AppKit's shared loading gate suppresses overlapping requests.
        (setq composer-idle-p t)
        (let ((owner (appkit-chat-history-request-begin 'newer)))
          (unwind-protect
              (progn
                (disco-room--window-scroll window 1)
                (should-not calls))
            (appkit-chat-history-request-end owner)))))))

(ert-deftest disco-room-filter-search-activates-msg-filter ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan")
          requested)
      (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-success)
                            '((total_results . 1)
                              (messages (((id . "m1")
                                          (channel_id . "chan")
                                          (content . "hello"))))))))
                ((symbol-function 'disco-room-render)
                 (lambda ()
                   (ert-fail "filter command rendered outside Appkit sync")))
                ((symbol-function 'appkit-request-sync)
                 (lambda (&rest _args) (setq requested t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-filter-search "hello")
        (should requested)
        (should (equal "hello" (plist-get disco-room--msg-filter :query)))
        (should (equal '("m1")
                       (mapcar (lambda (msg) (alist-get 'id msg))
                               (plist-get disco-room--msg-filter :items))))))))

(ert-deftest disco-room-filter-status-is-state-only-not-a-key-cheat-sheet ()
  (let ((disco-room--msg-filter
         '(:active t :query "hello" :items (((id . "m1"))) :total-count 3))
        (disco-room--filter-in-flight nil))
    (let ((status (disco-room--msg-filter-status-line)))
      (should (string-match-p "1/3" status))
      (should (string-match-p "More results available" status))
      (should-not (string-match-p "M-<" status))
      (should-not (string-match-p "C-c" status)))))

(ert-deftest disco-room-filter-search-rejects-unsupported-channel-types ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "voice")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "voice") (type . 2) (name . "Voice")))
    (should-error (disco-room-filter-search "hello") :type 'error)))

(ert-deftest disco-room-filter-search-supports-ephemeral-dm ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "ephemeral")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "ephemeral") (type . 18)))
    (should (disco-room--searchable-channel-type-p))
    (should (equal "ephemeral-dm"
                   (disco-room--searchable-channel-type-name)))))

(ert-deftest disco-room-search-current-channel-auto-includes-age-restricted-thread ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (setq-local disco-room--guild-id "g1")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "parent")
       (type . 0)
       (guild_id . "g1")
       (nsfw . t)))
    (disco-state-upsert-channel
     '((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (parent_id . "parent")))
    (let (captured)
      (cl-letf (((symbol-function 'disco-api-guild-search-messages-async)
                 (lambda (guild-id &rest args)
                   (setq captured (cons guild-id args)))))
        (disco-room--search-current-channel-async :query "hello")
        (should (equal "g1" (car captured)))
        (should (eq t (plist-get (cdr captured) :include-nsfw)))))))

(ert-deftest disco-room-composer-visible-p-hides-read-only-guild-channel ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "readonly")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "readonly")
       (type . 0)
       (guild_id . "g1")
       (permissions . "0")))
    (should-not (disco-room--composer-visible-p))
    (should (equal '(send-messages)
                   (disco-room--composer-missing-permissions)))))

(ert-deftest disco-room-composer-visible-p-uses-thread-send-permission ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (permissions . "2048")))
    (should-not (disco-room--composer-visible-p))
    (should (equal '(send-messages-in-threads)
                   (disco-room--composer-missing-permissions)))))

(ert-deftest disco-room-composer-visible-p-hides-system-user-dm ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "sysdm")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "sysdm")
       (type . 1)
       (recipients . (((id . "643945264868098049")
                       (username . "Discord")
                       (system . t))))))
    (should-not (disco-room--composer-visible-p))
    (should (string-match-p "official Discord system DMs are read-only"
                            (disco-room--composer-hidden-status-line)))))

(ert-deftest disco-room-render-hides-composer-when-send-permission-missing ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "readonly")
    (setq-local disco-room--channel-name "readonly")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "readonly")
       (type . 0)
       (guild_id . "g1")
       (permissions . "0")))
    (disco-room-render)
    (should-not (text-property-any (point-min) (point-max) 'disco-room-input t))
    (should-not (appkit-chatbuf-input-start-position))
    (should (string-match-p "composer hidden: missing SEND_MESSAGES"
                            (buffer-string)))
    (should-not (string-match-p "type at >>>" (buffer-string)))))

(ert-deftest disco-room-render-shows-age-restricted-header-tag ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "adult")
    (setq-local disco-room--channel-name "adult")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "adult")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")
       (nsfw . t)))
    (disco-room-render)
    (should (string-match-p (regexp-quote "Channel: adult [18+]")
                            (buffer-string)))))

(ert-deftest disco-room-header-omits-static-keybinding-cheat-sheet ()
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
    (should (string-match-p "Channel: chat" (buffer-string)))
    (should-not (string-match-p "M-<: older/more" (buffer-string)))
    (should-not (string-match-p "timeline c/l/n/p" (buffer-string)))
    (should-not (string-match-p "type at >>>" (buffer-string)))))

(ert-deftest disco-room-render-shows-reply-and-attachments-near-composer ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (appkit-chatbuf-input-state-set "hello")
    (disco-room--set-composer-aux-state nil "m42")
    (setq-local disco-room--pending-attachments
                '((:token-id 1 :path "/tmp/a.txt")
                  (:token-id 2 :path "/tmp/b.png" :description "preview")))
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m42")
        (channel_id . "chat")
        (content . "hello reply")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-render)
    (should (text-property-any (point-min) (point-max) 'disco-room-input t))
    (should (string-match-p "Replying to alice \\[m42\\]"
                            (buffer-string)))
    (should-not (string-match-p "C-c C-k" (buffer-string)))
    (should (string-match-p "> hello reply"
                            (buffer-string)))
    (should (string-match-p "Queued attachments: \\\[file:1\\\] a.txt, \\\[file:2\\\] b.png - preview"
                            (buffer-string)))))

(ert-deftest disco-room-render-keeps-reply-context-when-composer-hidden ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "readonly")
    (setq-local disco-room--channel-name "readonly")
    (disco-room--set-composer-aux-state nil "m42")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "readonly")
       (type . 0)
       (guild_id . "g1")
       (permissions . "0")))
    (disco-state-put-messages
     "readonly"
     '(((id . "m42")
        (channel_id . "readonly")
        (content . "hello reply")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-render)
    (should-not (text-property-any (point-min) (point-max) 'disco-room-input t))
    (should (string-match-p "Replying to alice \\[m42\\]"
                            (buffer-string)))))

(ert-deftest disco-room-edit-message-enters-composer-edit-mode ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (appkit-chatbuf-input-state-set "saved draft")
    (let ((msg '((id . "m1")
                 (channel_id . "chat")
                 (content . "old body")
                 (author . ((id . "u1") (username . "alice"))))))
      (disco-state-reset)
      (disco-state-upsert-channel
       '((id . "chat")
         (type . 0)
         (guild_id . "g1")
         (permissions . "2048")))
      (disco-state-put-messages "chat" (list msg))
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg))
                ((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1"))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-edit-message))
      (should (disco-room--composer-edit-active-p))
      (should (equal "m1" (disco-room--composer-edit-message-id)))
      (should (eq 'edit (appkit-chatbuf-aux-type)))
      (should (equal "m1" (appkit-chatbuf-aux-message-id)))
      (should (equal "old body"
                     (appkit-chatbuf-string-plain-text
                      (disco-room--current-draft))))
      (should (string-match-p "Editing alice \\[m1\\]"
                              (buffer-string)))
      (should (string-match-p "> old body"
                              (buffer-string)))
      (should (text-property-any
               (point-min) (point-max) 'disco-room-input t)))))

(ert-deftest disco-room-send-message-commits-composer-edit-and-restores-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (let ((msg '((id . "m1")
                 (channel_id . "chat")
                 (content . "old body")
                 (author . ((id . "u1") (username . "alice"))))))
      (disco-state-reset)
      (disco-state-upsert-channel
       '((id . "chat")
         (type . 0)
         (guild_id . "g1")
         (permissions . "2048")))
      (disco-state-put-messages "chat" (list msg))
      (appkit-chatbuf-input-state-set "saved draft [file:1]")
      (puthash "1" '(:token-id "1" :path "/tmp/a.txt") disco-room--attachment-token-table)
      (setq-local disco-room--attachment-token-seq 1)
      (disco-room--sync-pending-attachments-from-draft)
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg))
                ((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1"))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-edit-message))
      (should (eq 'edit (appkit-chatbuf-aux-type)))
      (should (equal "m1" (appkit-chatbuf-aux-message-id)))
      (disco-room--set-draft "updated body")
      (let (edit-call send-called)
        (cl-letf (((symbol-function 'disco-api-edit-message-async)
                   (lambda (channel-id message-id content &rest args)
                     (setq edit-call (list channel-id message-id content))
                     (funcall (plist-get args :on-success)
                              `((id . ,message-id) (channel_id . ,channel-id)
                                (content . ,content) (author (id . "u1"))))))
                  ((symbol-function 'disco-gateway-current-user-id)
                   (lambda () "u1"))
                  ((symbol-function 'disco-api-send-message-async)
                   (lambda (&rest _args)
                     (setq send-called t)))
                  ((symbol-function 'disco-room--channel-buffer-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'message)
                   (lambda (&rest _args) nil)))
          (disco-room-send-message))
        (should (equal '("chat" "m1" "updated body") edit-call))
        (should-not send-called)
        (should-not (disco-room--composer-edit-active-p))
        (should-not (appkit-chatbuf-aux-active-p))
        (let ((restored-draft (appkit-chatbuf-input-state)))
          (should (equal "saved draft [file:1]"
                         (appkit-chatbuf-string-plain-text restored-draft)))
          (should-not disco-room--pending-reply-to)
          (should (equal '("1")
                         (disco-room--attachment-token-ids-in-text restored-draft))))
        (should (= 1 (hash-table-count disco-room--attachment-token-table)))))))

(ert-deftest disco-room-render-shows-composer-when-send-permission-present ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (should (text-property-any (point-min) (point-max) 'disco-room-input t))
    (should (integerp (appkit-chatbuf-input-start-position)))
    (should (string-match-p (regexp-quote ">>> hello")
                            (buffer-string)))))

(ert-deftest disco-room-set-draft-preserves-ewoc-and-composer-anchor ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (input-start (appkit-chatbuf-input-start-position))
          (prompt-start (appkit-chatbuf-prompt-start-position))
          frame-update-called)
      (cl-letf (((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (setq frame-update-called t))))
        (disco-room--set-draft "updated body"))
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (= input-start (appkit-chatbuf-input-start-position)))
      (should (= prompt-start (appkit-chatbuf-prompt-start-position)))
      (should-not frame-update-called)
      (should (string-match-p "> updated body" (buffer-string))))))

(ert-deftest disco-room-set-draft-rerenders-when-attachment-footer-state-changes ()
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
    (let ((frame-update-called nil))
      (cl-letf (((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (setq frame-update-called t))))
        (disco-room--set-draft
         (disco-room--attachment-input-object-string
          (disco-room--make-attachment-input-object "/tmp/a.txt"))))
      (should frame-update-called)
      (should (= 1 (length disco-room--pending-attachments))))))

(ert-deftest disco-room-sync-draft-from-buffer-preserves-attachment-input-objects ()
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
    (appkit-chatbuf-input-insert "hello ")
    (disco-room--insert-attachment-input-object
     (disco-room--make-attachment-input-object
      "/tmp/a.txt"
      :description "preview"))
    (disco-room--sync-draft-from-buffer)
    (should (appkit-chatbuf-string-has-objects-p
             (disco-room--current-draft)))
    (should (= 1 (length (disco-room--draft-input-objects))))
    (should (equal "hello "
                   (disco-room--draft-without-attachment-tokens)))
    (should (equal '((:path "/tmp/a.txt"
                      :filename "a.txt"
                      :description "preview"))
                   (disco-room--attachments-from-draft)))))

(ert-deftest disco-room-send-message-parses-attachment-input-objects ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     `((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . ,(number-to-string (logior 2048 (ash 1 15))))))
    (disco-room-render)
    (let ((path (make-temp-file "disco-room-object-attach"))
          sent-content
          sent-attachments)
      (unwind-protect
          (progn
            (goto-char (point-max))
            (appkit-chatbuf-input-insert "hello ")
            (disco-room--insert-attachment-input-object
             (disco-room--make-attachment-input-object
              path
              :description "preview"))
            (cl-letf (((symbol-function 'disco-api-send-message-with-attachments-async)
                       (lambda (_channel-id &rest args)
                         (setq sent-content (plist-get args :content)
                               sent-attachments (plist-get args :attachments))
                         (funcall (plist-get args :on-success)
                                  `((id . "server-1")
                                    (nonce . ,(plist-get args :nonce))
                                    (channel_id . "chat")))))
                      ((symbol-function 'disco-room--channel-buffer-p)
                       (lambda (&rest _args) t))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-send-message))
            (should (equal "hello" sent-content))
            (should (equal `((:path ,path
                              :filename ,(file-name-nondirectory path)
                              :description "preview"))
                           sent-attachments))
            (should (equal ""
                           (appkit-chatbuf-string-plain-text
                            (disco-room--current-draft)))))
        (delete-file path)))))

(ert-deftest disco-room-render-reuses-existing-message-nodes ()
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
     '(((id . "m2") (channel_id . "chat") (content . "two"))
       ((id . "m1") (channel_id . "chat") (content . "one"))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (node-m1 (appkit-chat-timeline-node "m1"))
          (node-m2 (appkit-chat-timeline-node "m2")))
      (disco-state-put-messages
       "chat"
       '(((id . "m3") (channel_id . "chat") (content . "three"))
         ((id . "m2") (channel_id . "chat") (content . "two updated"))
         ((id . "m1") (channel_id . "chat") (content . "one"))))
      (disco-room-render)
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (eq node-m1 (appkit-chat-timeline-node "m1")))
      (should (eq node-m2 (appkit-chat-timeline-node "m2")))
      (should (appkit-chat-timeline-node "m3"))
      (should (equal '("m1" "m2" "m3")
                     (appkit-chat-timeline-keys)))
      (should (string-match-p "two updated" (buffer-string))))))

(ert-deftest disco-room-render-shows-plain-attachment-lines-when-rich-cards-disabled ()
  (with-temp-buffer
    (let ((disco-room-use-rich-attachment-cards nil)
          (disco-room-show-attachment-urls t))
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
       '(((id . "m1")
          (channel_id . "chat")
          (content . "see file")
          (attachments . (((id . "a1")
                           (filename . "doc.txt")
                           (url . "https://example.invalid/doc.txt")))))))
      (disco-room-test-establish-latest-window)
      (disco-room-render)
      (should (string-match-p (regexp-quote "[file] doc.txt") (buffer-string)))
      (should (string-match-p (regexp-quote "https://example.invalid/doc.txt")
                              (buffer-string))))))

(ert-deftest disco-room-insert-message-attachments-dispatches-rich-attachments-by-kind ()
  (with-temp-buffer
    (let ((disco-room-use-rich-attachment-cards t)
          (seen nil))
      (cl-letf (((symbol-function 'disco-ins-insert-attachment-photo)
                 (lambda (_attachment &rest _args)
                   (push 'photo seen)
                   (insert "[photo-block]
")))
                ((symbol-function 'disco-ins-insert-attachment-video)
                 (lambda (_attachment &rest _args)
                   (push 'video seen)
                   (insert "[video-block]
")))
                ((symbol-function 'disco-ins-insert-attachment-audio)
                 (lambda (_attachment &rest _args)
                   (push 'audio seen)
                   (insert "[audio-block]
")))
                ((symbol-function 'disco-ins-insert-attachment-document)
                 (lambda (_attachment &rest _args)
                   (push 'document seen)
                   (insert "[document-block]
"))))
        (disco-room--insert-message-attachments
         '((attachments . (((filename . "cat.png"))
                           ((filename . "clip.mp4"))
                           ((filename . "voice-message.ogg")
                            (content_type . "audio/ogg")
                            (duration_secs . 12.0)
                            (waveform . "AAAA"))
                           ((filename . "doc.txt"))))))
        (should (equal '(photo video audio document) (nreverse seen)))
        (should (string-match-p (regexp-quote "[photo-block]") (buffer-string)))
        (should (string-match-p (regexp-quote "[video-block]") (buffer-string)))
        (should (string-match-p (regexp-quote "[audio-block]") (buffer-string)))
        (should (string-match-p (regexp-quote "[document-block]") (buffer-string)))))))

(ert-deftest disco-room-insert-message-attachments-surfaces-render-error ()
  (with-temp-buffer
    (let ((disco-room-use-rich-attachment-cards t))
      (cl-letf (((symbol-function 'disco-ins-insert-attachment-photo)
                 (lambda (&rest _args)
                   (error "boom"))))
        (should-error
         (disco-room--insert-message-attachments
          '((attachments . (((filename . "cat.png")
                             (url . "https://example.invalid/cat.png")))))))))))

(ert-deftest disco-room-handle-media-rerender-syncs-affected-audio-resource ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat") (type . 0) (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m2")
        (channel_id . "chat")
        (attachments . (((id . "a2") (filename . "two.ogg")))))
       ((id . "m1")
        (channel_id . "chat")
        (attachments . (((id . "a1") (filename . "one.ogg")))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (should (equal '("m2")
                   (appkit-chat-timeline-dependent-keys
                    '((:attachment "a2")))))
    (let (changed-resources refreshed frame-updated)
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda () (list (current-buffer))))
                ((symbol-function 'disco-room--sync-timeline)
                 (lambda (&rest arguments)
                   (setq changed-resources
                         (plist-get arguments :changed-resources))))
                ((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args) (setq frame-updated t)))
                ((symbol-function 'disco-room--refresh-open-rooms)
                 (lambda () (setq refreshed t))))
        (disco-room--handle-media-rerender 'audio "a2")
        (appkit-sync-invalidations (appkit-current-view))
        (should (equal '((:attachment "a2")) changed-resources))
        (should-not frame-updated)
        (should-not refreshed)))))

(ert-deftest disco-room-preview-completion-syncs-dependent-message-resource ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat") (type . 0) (permissions . "2048")))
    (let* ((attachment
            '((id . "image-2")
              (filename . "two.png")
              (content_type . "image/png")
              (url . "https://cdn.invalid/two.png")
              (proxy_url . "https://media.invalid/two.png")))
           (preview-key
            (disco-media-attachment-preview-cache-key attachment)))
      (disco-state-put-messages
       "chat"
       `(((id . "m2")
          (channel_id . "chat")
          (attachments . (,attachment)))
         ((id . "m1") (channel_id . "chat"))))
      (disco-room-test-establish-latest-window)
      (disco-room-render)
      (should (equal '("m2")
                     (appkit-chat-timeline-dependent-keys
                      (list (list :preview preview-key)))))
      (let (changed-resources)
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda () (list (current-buffer))))
                  ((symbol-function 'disco-room--sync-timeline)
                   (lambda (&rest arguments)
                     (setq changed-resources
                           (plist-get arguments :changed-resources)))))
          (disco-room--handle-media-rerender 'preview preview-key)
          (appkit-sync-invalidations (appkit-current-view))
          (should (equal (list (list :preview preview-key))
                         changed-resources)))))))

(ert-deftest disco-room-insert-message-attachments-hides-spoiler-media-until-revealed ()
  (with-temp-buffer
    (let ((disco-room-use-rich-attachment-cards t)
          (disco-room--revealed-spoiler-message-id nil)
          spoiler-hidden
          toggled-id)
      (cl-letf (((symbol-function 'disco-ins-insert-attachment-photo)
                 (lambda (_attachment &rest args)
                   (setq spoiler-hidden (plist-get args :spoiler-hidden))
                   (insert "[photo-card]\n")
                   (insert-text-button
                    "[Reveal spoiler]"
                    'action (lambda (_button)
                              (disco-room-toggle-message-spoilers "m1")))
                   (insert "\n")))
                ((symbol-function 'disco-room-toggle-message-spoilers)
                 (lambda (message-id)
                   (setq toggled-id message-id))))
        (disco-room--insert-message-attachments
         '((id . "m1")
           (attachments . (((filename . "SPOILER_cat.png")
                            (flags . 8))))))
        (should spoiler-hidden)
        (should (string-match-p (regexp-quote "[photo-card]") (buffer-string)))
        (goto-char (point-min))
        (search-forward "[Reveal spoiler]")
        (button-activate (button-at (match-beginning 0)))
        (should (equal "m1" toggled-id))))))

(ert-deftest disco-room-toggle-message-spoilers-reveals-spoiler-attachment-on-rerender ()
  (with-temp-buffer
    (let ((disco-room-use-rich-attachment-cards nil)
          (disco-room-show-attachment-urls nil))
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
       '(((id . "m1")
          (channel_id . "chat")
          (content . "")
          (attachments . (((id . "a1")
                           (filename . "SPOILER_cat.png")
                           (flags . 8)
                           (width . 640)
                           (height . 480)
                           (url . "https://example.invalid/cat.png")))))))
      (disco-room-test-establish-latest-window)
      (disco-room-render)
      (should (string-match-p (regexp-quote "[spoiler image hidden]")
                              (buffer-string)))
      (should-not (string-match-p (regexp-quote "cat.png") (buffer-string)))
      (disco-room-toggle-message-spoilers "m1")
      (should (string-match-p (regexp-quote "cat.png") (buffer-string)))
      (should-not (string-match-p (regexp-quote "[spoiler image hidden]")
                                  (buffer-string))))))

(ert-deftest disco-room-composer-visible-p-hides-archived-thread ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (permissions . "274877906944")
       (thread_metadata . ((archived . t)))))
    (should-not (disco-room--composer-visible-p))
    (should (string-match-p "current thread is archived"
                            (disco-room--composer-hidden-status-line)))))

(ert-deftest disco-room-attach-file-errors-while-editing ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (disco-room--set-composer-aux-state '(:type edit :message-id "m1" :saved-state nil) nil)
    (let ((path (make-temp-file "disco-room-attach")))
      (unwind-protect
          (should-error (disco-room-attach-file path) :type 'user-error)
        (delete-file path)))))

(ert-deftest disco-room-attach-file-inserts-attachment-input-object ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "34816")))
    (disco-room-render)
    (let ((path (make-temp-file "disco-room-attach")))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-attach-file path "preview"))
            (should (appkit-chatbuf-string-has-objects-p
                     (disco-room--current-draft)))
            (should (equal `((:path ,path
                              :filename ,(file-name-nondirectory path)
                              :description "preview"))
                           (disco-room--attachments-from-draft)))
            (should (string-match-p (regexp-quote (format "[file] %s"
                                                          (file-name-nondirectory path)))
                                    (buffer-string))))
        (delete-file path)))))

(ert-deftest disco-room-remove-attachment-token-removes-attachment-input-object ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "34816")))
    (disco-room-render)
    (let ((path (make-temp-file "disco-room-attach")))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (appkit-chatbuf-input-insert "hello ")
            (disco-room--insert-attachment-input-object
             (disco-room--make-attachment-input-object path :description "preview"))
            (goto-char (or (text-property-not-all (appkit-chatbuf-input-start-position)
                                                  (point-max)
                                                  appkit-chatbuf-input-object-property
                                                  nil)
                           (point-max)))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-remove-attachment-token-at-point))
            (should-not (appkit-chatbuf-string-has-objects-p
                         (disco-room--current-draft)))
            (should (equal '() (disco-room--attachments-from-draft)))
            (should (equal "hello "
                           (appkit-chatbuf-string-plain-text
                            (disco-room--current-draft)))))
        (delete-file path)))))

(ert-deftest disco-room-remove-attachment-token-errors-while-editing ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room--set-composer-aux-state '(:type edit :message-id "m1" :saved-state nil) nil)
    (appkit-chatbuf-input-state-set "[file:1]")
    (should-error (disco-room-remove-attachment-token-at-point) :type 'user-error)))

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

(ert-deftest disco-room-create-thread-from-message-errors-without-create-public-threads ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (should-error (disco-room-create-thread-from-message "topic" "m1") :type 'user-error)))

(ert-deftest disco-room-create-thread-errors-without-create-private-threads ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     `((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . ,(number-to-string (ash 1 35)))))
    (should-error (disco-room-create-thread "topic" 12 nil nil nil) :type 'user-error)))

(ert-deftest disco-room-add-reaction-errors-without-add-reactions ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (should-error (disco-room-add-reaction "👍" "m1") :type 'user-error)))

(ert-deftest disco-room-vote-poll-answer-errors-in-archived-thread ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (let* ((poll-msg
            '((id . "m1")
              (channel_id . "thread")
              (poll . ((answers . (((answer_id . 1)
                                    (poll_media . ((text . "one"))))
                                   ((answer_id . 2)
                                    (poll_media . ((text . "two")))))))))))
      (disco-state-reset)
      (disco-state-upsert-channel
       `((id . "thread")
         (type . 11)
         (guild_id . "g1")
         (permissions . ,(number-to-string (ash 1 38)))
         (thread_metadata . ((archived . t)))))
      (disco-state-put-messages "thread" (list poll-msg))
      (should-error (disco-room-vote-poll-answer 1 "m1") :type 'user-error))))

(ert-deftest disco-room-rename-thread-errors-when-archived ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel
     `((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (permissions . ,(number-to-string (ash 1 34)))
       (thread_metadata . ((archived . t)))))
    (should-error (disco-room-rename-thread "new-name") :type 'user-error)))

(ert-deftest disco-room-toggle-thread-archived-errors-without-manage-threads ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel
     `((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (permissions . ,(number-to-string (ash 1 38)))
       (thread_metadata . ((archived . :false) (locked . :false)))))
    (should-error (disco-room-toggle-thread-archived) :type 'user-error)))

(ert-deftest disco-room-join-thread-errors-when-already-joined ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "thread") (type . 11) (guild_id . "g1")
                                  (thread_metadata . ((archived . :false)))))
    (disco-state-upsert-thread-member "thread" "u1")
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "u1")))
      (should-error (disco-room-join-thread) :type 'user-error))))

(ert-deftest disco-room-leave-thread-errors-when-not-joined ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "thread") (type . 11) (guild_id . "g1")
                                  (thread_metadata . ((archived . :false)))))
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "u1")))
      (should-error (disco-room-leave-thread) :type 'user-error))))

(ert-deftest disco-room-set-thread-muted-errors-when-not-joined ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "thread") (type . 11) (guild_id . "g1")
                                  (thread_metadata . ((archived . :false)))))
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "u1")))
      (should-error (disco-room-set-thread-muted t) :type 'user-error))))

(ert-deftest disco-room-send-poll-errors-while-replying ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-composer-aux-state nil "m1")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "562949953423360")))
    (should-error (disco-room-send-poll "Q" '("a" "b") 24 nil nil)
                  :type 'user-error)))

(ert-deftest disco-room-forward-message-errors-while-replying ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-composer-aux-state nil "m1")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (should-error (disco-room-forward-message "m2" "src" nil nil)
                  :type 'user-error)))

(ert-deftest disco-room-send-message-errors-for-system-user-dm ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "sysdm")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "sysdm")
       (type . 1)
       (recipients . (((id . "643945264868098049")
                       (username . "Discord")
                       (system . t))))))
    (should-error (disco-room-send-message) :type 'user-error)))

(ert-deftest disco-room-edit-message-errors-while-replying ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-composer-aux-state nil "m1")
    (let ((msg '((id . "m2")
                 (channel_id . "chat")
                 (content . "body")
                 (author . ((id . "u1") (username . "alice"))))))
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg)))
        (should-error (disco-room-edit-message) :type 'user-error)))))

(ert-deftest disco-room-refresh-reruns-active-filter ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--msg-filter '(:active t :query "hello"))
          refreshed)
      (cl-letf (((symbol-function 'disco-room-filter-refresh)
                 (lambda () (setq refreshed t))))
        (disco-room-refresh)
        (should refreshed)))))

(ert-deftest disco-room-load-older-messages-loads-more-filter-results ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--msg-filter '(:active t :query "hello"))
          loaded-more)
      (cl-letf (((symbol-function 'disco-room-filter-load-more)
                 (lambda () (setq loaded-more t))))
        (disco-room-load-older-messages)
        (should loaded-more)))))

(ert-deftest disco-room-highlight-search-query-adds-face ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--inplace-search-filter '(:query "beta"))
    (let* ((text (disco-room--highlight-search-query "alpha beta gamma"))
           (start (string-match "beta" text)))
      (should (integerp start))
      (should (eq 'disco-room-search-highlight
                  (get-text-property start 'face text))))))

(ert-deftest disco-room-inplace-search-dispatch-local-hit-skips-api ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan")
          api-called)
      (disco-state-reset)
      (disco-state-put-messages "chan"
                                '(((id . "m2") (channel_id . "chan") (content . "beta"))
                                  ((id . "m1") (channel_id . "chan") (content . "alpha"))))
      (let ((inhibit-read-only t))
        (insert "alpha\n")
        (add-text-properties (line-beginning-position 0) (line-end-position 0)
                             '(disco-message-id "m1"))
        (insert "beta\n")
        (add-text-properties (line-beginning-position 0) (line-end-position 0)
                             '(disco-message-id "m2")))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                 (lambda (&rest _args)
                   (setq api-called t)))
                ((symbol-function 'disco-room-render)
                 (lambda () nil)))
        (disco-room--inplace-search-dispatch '(:query "beta") t)
        (should-not api-called)
        (should (equal "m2" (get-text-property (line-beginning-position) 'disco-message-id)))))))

(ert-deftest disco-room-inplace-search-dispatch-rerenders-when-highlight-query-changes ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan")
          requested)
      (disco-state-reset)
      (disco-state-put-messages "chan"
                                '(((id . "m1") (channel_id . "chan") (content . "alpha"))))
      (let ((inhibit-read-only t))
        (insert "alpha\n")
        (add-text-properties (point-min) (point-max) '(disco-message-id "m1")))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'disco-room-render)
                 (lambda ()
                   (ert-fail "inplace command rendered outside Appkit sync")))
                ((symbol-function 'appkit-request-sync)
                 (lambda (&rest _args) (setq requested t))))
        (disco-room--inplace-search-dispatch '(:query "alpha") t)
        (should requested)))))

(ert-deftest disco-room-inplace-search-dispatch-server-hit-jumps-to-message ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "chan")
          queued)
      (disco-state-reset)
      (setq-local disco-room--newest-message-id "m9")
      (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                 (lambda (&rest args)
                   (funcall (plist-get args :on-success)
                            '((messages (((id . "m5")
                                          (channel_id . "chan")
                                          (content . "match"))))))))
                ((symbol-function 'disco-room--queue-jump)
                 (lambda (message-id view)
                   (setq queued (list message-id view))))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room--inplace-search-dispatch '(:query "match") nil "m9")
        (should (equal "m5" (car queued)))
        (should (eq (cadr queued) (appkit-current-view)))))))

(ert-deftest disco-room-inplace-search-unsupported-channel-skips-remote-search ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "voice")
    (setq-local disco-room--newest-message-id "m9")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "voice") (type . 2) (name . "Voice")))
    (let (api-called)
      (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                 (lambda (&rest _args)
                   (setq api-called t)))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (should-not (disco-room--inplace-search-dispatch '(:query "match") nil "m9"))
        (should-not api-called)))))

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

(ert-deftest disco-room-send-message-splits-overlong-content-by-default ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (appkit-chatbuf-input-state-set "first line\nsecond line\nthird line")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "2048")))
    (let ((disco-api--message-content-limit 12)
          sent)
      (cl-letf (((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chan"))))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-room-render)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'disco-api-send-message-async)
                 (lambda (_channel-id content &rest args)
                   (push content sent)
                   (funcall (plist-get args :on-success)
                            `((id . ,(format "server-%d" (length sent)))
                              (nonce . ,(plist-get args :nonce))
                              (channel_id . "chan"))))))
        (disco-room-send-message)
        (should (equal '("first line" "second line" "third line")
                       (nreverse sent)))
        (should-not disco-room--send-in-flight)
        (should (equal ""
                       (appkit-chatbuf-string-plain-text
                        (disco-room--current-draft))))))))

(ert-deftest disco-room-send-message-sends-overlong-content-as-file-when-configured ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (let* ((long-text (make-string 32 ?x))
           (disco-api--message-content-limit 10)
           (disco-room-long-message-action 'file)
           sent-content
           sent-attachments
           captured-file-body)
      (appkit-chatbuf-input-state-set long-text)
      (disco-room--sync-shared-input-options-state)
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "2048")))
      (cl-letf (((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chan"))))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-room-render)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'disco-api-send-message-with-attachments-async)
                 (lambda (_channel-id &rest args)
                   (setq sent-content (plist-get args :content)
                         sent-attachments (plist-get args :attachments))
                   (with-temp-buffer
                     (insert-file-contents (plist-get (car sent-attachments) :path))
                     (setq captured-file-body (buffer-string)))
                   (funcall (plist-get args :on-success)
                            `((id . "server-file")
                              (nonce . ,(plist-get args :nonce))
                              (channel_id . "chan"))))))
        (disco-room-send-message)
        (should-not sent-content)
        (should (= 1 (length sent-attachments)))
        (should (equal disco-room-long-message-file-name
                       (plist-get (car sent-attachments) :filename)))
        (should (equal long-text captured-file-body))
        (should-not disco-room--send-in-flight)
        (should (equal ""
                       (appkit-chatbuf-string-plain-text
                        (disco-room--current-draft))))))))

(ert-deftest disco-room-send-message-rejects-overlong-content-before-send-state ()
  (let ((disco-room-long-message-action 'file))
    (with-temp-buffer
      (disco-room-mode)
      (setq-local disco-room--channel-id "chan")
      (appkit-chatbuf-input-state-set
       (make-string (1+ disco-api--message-content-limit) ?a))
      (disco-room--sync-shared-input-options-state)
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "0")))
      (should-error (disco-room-send-message) :type 'error)
      (should-not disco-room--send-in-flight)
      (should (= (1+ disco-api--message-content-limit)
                 (length (disco-room--current-draft)))))))

(ert-deftest disco-room-send-poll-rejects-overlong-content-before-send-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "562949953423360")))
    (should-error
     (disco-room-send-poll "Question" '("one" "two") 24 nil
                           (make-string (1+ disco-api--message-content-limit) ?a))
     :type 'error)
    (should-not disco-room--send-in-flight)))

(ert-deftest disco-room-forward-message-rejects-overlong-comment-before-send-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "2048")))
    (cl-letf (((symbol-function 'disco-room--ensure-jump-permissions)
               (lambda (&rest _args) t)))
      (should-error
       (disco-room-forward-message
        "m1" "chan"
        (make-string (1+ disco-api--message-content-limit) ?a)
        nil)
       :type 'error)
      (should-not disco-room--send-in-flight))))

(ert-deftest disco-room-forward-message-upserts-response-without-refresh ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "target")
          requested
          refreshed)
      (disco-state-reset)
      (cl-letf (((symbol-function 'disco-room--forward-unavailable-reason)
                 (lambda () nil))
                ((symbol-function 'disco-room--resolve-target-channel)
                 (lambda (_channel-id) '((id . "source") (type . 0))))
                ((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "target") (type . 0))))
                ((symbol-function 'disco-permission-ensure-channel) #'ignore)
                ((symbol-function 'disco-room--ensure-jump-permissions) #'ignore)
                ((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (ert-fail "forward callback updated frame directly")))
                ((symbol-function 'disco-room-render)
                 (lambda ()
                   (ert-fail "forward callback rendered directly")))
                ((symbol-function 'disco-room--render-send-state-change)
                 (lambda ()
                   (ert-fail "forward callback rendered directly")))
                ((symbol-function 'appkit-sync-invalidations)
                 (lambda (&rest _args)
                   (ert-fail "forward callback synced directly")))
                ((symbol-function 'appkit-request-sync)
                 (lambda (&rest _args) (setq requested t)))
                ((symbol-function 'disco-room-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'disco-api-forward-message-async)
                 (lambda (_target _message _source &rest args)
                   (funcall (plist-get args :on-success)
                            '((id . "100") (channel_id . "target")
                              (content . "forwarded")))))
                ((symbol-function 'message) #'ignore))
        (disco-room-forward-message "50" "source" nil nil)
        (should requested)
        (should-not refreshed)
        (should-not disco-room--send-in-flight)
        (should (equal "100"
                       (alist-get 'id (car (disco-state-messages "target")))))))))

(ert-deftest disco-room-forward-snapshot-content-uses-internal-markdown-renderer ()
  (let* ((msg '((id . "m1")
                (message_snapshots
                 . (((message
                      . ((content . "[link](https://example.com)\n> quote"))))))))
         (rendered (disco-room--forward-snapshot-content msg))
         (plain (substring-no-properties rendered))
         (link-pos (string-match "link" plain))
         (quote-pos (string-match "quote" plain)))
    (should (equal "link\nquote" plain))
    (should (equal "https://example.com"
                   (get-text-property link-pos 'disco-markdown-url rendered)))
    (should (equal "| "
                   (substring-no-properties
                    (get-text-property quote-pos 'line-prefix rendered))))))

(ert-deftest disco-room-thread-entry-is-a-navigable-reference-not-a-button ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-upsert-channel
     '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-03-08T00:00:00.000000+00:00")
        (flags . 32)
        (content . "starter")
        (author . ((id . "u1") (username . "alice"))))))
    (disco-room-test-establish-latest-window)
    (disco-room-render)
    (should (string-match-p (regexp-quote "↪ Thread: thread:m1")
                            (buffer-string)))
    (should-not (string-match-p (regexp-quote "[Open thread]")
                                (buffer-string)))
    (goto-char (point-min))
    (search-forward "↪ Thread: thread:m1")
    (should (keymapp (get-text-property (match-beginning 0) 'keymap)))))

(ert-deftest disco-room-avatar-http-retry-policy-is-explicit ()
  (dolist (status '(nil 0 408 425 429 500 502 503))
    (should (disco-room--avatar-transient-http-status-p status)))
  (dolist (status '(400 401 403 404))
    (should-not (disco-room--avatar-transient-http-status-p status))))

(ert-deftest disco-room-avatar-fetch-failure-enters-backoff-not-image-cache ()
  (let ((disco-room--avatar-image-cache (make-hash-table :test #'equal))
        (disco-room--avatar-fetching (make-hash-table :test #'equal))
        (disco-room--avatar-failures (make-hash-table :test #'equal))
        (disco-room--avatar-retry-timer nil)
        (disco-room-avatar-retry-delays '(2))
        scheduled-delay)
    (puthash "avatar" t disco-room--avatar-fetching)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest args)
                 (setq scheduled-delay delay)
                 (list function args)))
              ((symbol-function 'message) #'ignore))
      (let ((failure
             (disco-room--avatar-record-failure
              "avatar" "https://cdn.invalid/avatar.png" "/tmp/avatar"
              "temporary network error" t nil 503)))
        (should (= 1 (plist-get failure :attempts)))
        (should-not (plist-get failure :permanent))
        (should (= 503 (plist-get failure :status)))
        (should (> (plist-get failure :retry-at) (float-time)))
        (should-not (gethash "avatar" disco-room--avatar-image-cache))
        (should-not (gethash "avatar" disco-room--avatar-fetching))
        (should (> scheduled-delay 1.5))))))

(ert-deftest disco-room-avatar-fetch-http-error-preserves-retry-reason ()
  (let ((disco-room--avatar-image-cache (make-hash-table :test #'equal))
        (disco-room--avatar-fetching (make-hash-table :test #'equal))
        (disco-room--avatar-failures (make-hash-table :test #'equal))
        (disco-room--avatar-retry-timer nil)
        (disco-room--avatar-fetch-generation 7)
        else-callback)
    (cl-letf (((symbol-function 'disco-room--avatar-ensure-queue)
               (lambda () 'avatar-queue))
              ((symbol-function 'plz-run) #'ignore)
              ((symbol-function 'plz-queue)
               (lambda (&rest arguments)
                 (setq else-callback
                       (plist-get (nthcdr 3 arguments) :else))))
              ((symbol-function 'run-at-time)
               (lambda (&rest arguments) arguments))
              ((symbol-function 'message) #'ignore))
      (disco-room--start-avatar-fetch
       "avatar" "https://cdn.invalid/avatar.png" "/tmp/avatar")
      (should (gethash "avatar" disco-room--avatar-fetching))
      (funcall else-callback '(:status 429 :message "rate limited"))
      (let ((failure (gethash "avatar" disco-room--avatar-failures)))
        (should-not (gethash "avatar" disco-room--avatar-fetching))
        (should (= 429 (plist-get failure :status)))
        (should (equal "rate limited" (plist-get failure :reason)))
        (should-not (plist-get failure :permanent))))))

(ert-deftest disco-room-avatar-image-respects-backoff-and-retries-when-due ()
  (let ((disco-room--avatar-image-cache (make-hash-table :test #'equal))
        (disco-room--avatar-fetching (make-hash-table :test #'equal))
        (disco-room--avatar-failures (make-hash-table :test #'equal))
        started)
    (puthash "avatar"
             (list :retry-at (+ (float-time) 60) :permanent nil)
             disco-room--avatar-failures)
    (cl-letf (((symbol-function 'disco-room--image-rendering-available-p)
               (lambda () t))
              ((symbol-function 'disco-room--avatar-cache-key)
               (lambda (_message) "avatar"))
              ((symbol-function 'disco-room--avatar-url)
               (lambda (_message) "https://cdn.invalid/avatar.png"))
              ((symbol-function 'disco-room--avatar-cache-file-base)
               (lambda (_key) "/tmp/avatar"))
              ((symbol-function 'disco-room--avatar-cache-existing-file)
               (lambda (_key) nil))
              ((symbol-function 'disco-room--start-avatar-fetch)
               (lambda (&rest arguments) (setq started arguments))))
      (should-not (disco-room--avatar-image 'message))
      (should-not started)
      (puthash "avatar" '(:retry-at 0 :permanent nil)
               disco-room--avatar-failures)
      (should-not (disco-room--avatar-image 'message))
      (should (equal started
                     '("avatar" "https://cdn.invalid/avatar.png"
                       "/tmp/avatar"))))))

(ert-deftest disco-room-avatar-is-a-projected-message-dependency ()
  (cl-letf (((symbol-function 'disco-room--avatar-cache-key)
             (lambda (_message) "avatar-key")))
    (should (member '(:avatar "avatar-key")
                    (disco-room--message-dependency-keys
                     '((id . "message")))))))

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

(ert-deftest disco-room-avatar-invalidations-target-dependent-rows ()
  (with-temp-buffer
    (setq major-mode 'disco-room-mode)
    (let ((disco-room--avatar-pending-invalidations
           (make-hash-table :test #'equal))
          (disco-room--avatar-invalidation-timer 'pending)
          changed-resources)
      (puthash "avatar-a" t disco-room--avatar-pending-invalidations)
      (puthash "avatar-b" t disco-room--avatar-pending-invalidations)
      (cl-letf (((symbol-function 'disco-room--sync-resource-changes-in-open-rooms)
                 (lambda (resources)
                   (setq changed-resources resources))))
        (disco-room--avatar-flush-invalidations)
        (should-not disco-room--avatar-invalidation-timer)
        (should (= 0 (hash-table-count
                      disco-room--avatar-pending-invalidations)))
        (should (equal (sort changed-resources
                             (lambda (left right)
                               (string< (cadr left) (cadr right))))
                       '((:avatar "avatar-a") (:avatar "avatar-b"))))))))

(ert-deftest disco-room-avatar-success-clears-failure-and-invalidates-dependents ()
  (let ((disco-room--avatar-image-cache (make-hash-table :test #'equal))
        (disco-room--avatar-fetching (make-hash-table :test #'equal))
        (disco-room--avatar-failures (make-hash-table :test #'equal))
        retry-scheduled
        invalidated-key)
    (puthash "avatar" t disco-room--avatar-fetching)
    (puthash "avatar" '(:attempts 2) disco-room--avatar-failures)
    (cl-letf (((symbol-function 'disco-room--avatar-schedule-next-retry)
              (lambda () (setq retry-scheduled t)))
              ((symbol-function 'disco-room--avatar-schedule-invalidation)
               (lambda (cache-key) (setq invalidated-key cache-key))))
      (disco-room--avatar-complete-fetch "avatar" :image)
      (should (eq :image (gethash "avatar" disco-room--avatar-image-cache)))
      (should-not (gethash "avatar" disco-room--avatar-fetching))
      (should-not (gethash "avatar" disco-room--avatar-failures))
      (should retry-scheduled)
      (should (equal "avatar" invalidated-key)))))

(ert-deftest disco-room-avatar-svg-cache-key-tracks-derived-factor-geometry ()
  (let ((disco-room-avatar-image-size 28)
        (disco-room-avatar-round-size-factor 1.0)
        (disco-room-avatar-round-inset-ratio 0.0)
        (disco-room-avatar-extra-bottom-line t)
        (disco-room-avatar-factors-alist '((2 . (0.8 . 0.1)))))
    (cl-letf (((symbol-function 'appkit-chat-avatar-line-pixel-height)
               (lambda () 21))
              ((symbol-function 'appkit-chat-avatar-column-pixel-width)
               (lambda () 9)))
      (let* ((before (disco-room--avatar-svg-geometry 2))
             (before-key
              (disco-room--avatar-svg-cache-key "avatar.png" 'mtime 2 before)))
        (setq disco-room-avatar-factors-alist '((2 . (0.82 . 0.08))))
        (let* ((after (disco-room--avatar-svg-geometry 2))
               (after-key
                (disco-room--avatar-svg-cache-key "avatar.png" 'mtime 2 after)))
          (should (= (plist-get before :char-columns)
                     (plist-get after :char-columns)))
          (should-not (= (plist-get before :circle-height)
                         (plist-get after :circle-height)))
          (should-not (equal before-key after-key)))))))

(ert-deftest disco-room-text-scale-reprints-existing-avatar-slices ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-upsert-channel
     '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (disco-state-put-messages
     "chat"
     '(((id . "m1")
        (channel_id . "chat")
        (timestamp . "2026-07-11T12:00:00.000000+00:00")
        (content . "hello")
        (author . ((id . "u1") (username . "Alice"))))))
    (disco-room-test-establish-latest-window)
    (let ((line-height 21)
          (disco-room-avatar-round-images nil))
      (cl-labels
          ((avatar-slice-height
            ()
            (goto-char (point-min))
            (search-forward "Alice")
            (let* ((prefix
                    (get-text-property (line-beginning-position) 'line-prefix))
                   (display (and (stringp prefix)
                                 (get-text-property 0 'display prefix))))
              (nth 4 (car display)))))
        (cl-letf (((symbol-function 'disco-room--avatar-image)
                   (lambda (_message)
                     '(image :type png :data "avatar" :width 16 :height 16)))
                  ((symbol-function 'appkit-media-image-object-valid-p)
                   (lambda (image) (and (consp image) (eq (car image) 'image))))
                  ((symbol-function 'image-size)
                   (lambda (image &rest _args)
                     (cons (or (plist-get (cdr image) :width) 16)
                           (or (plist-get (cdr image) :height) 16))))
                  ((symbol-function 'appkit-chat-avatar-line-pixel-height)
                   (lambda () line-height))
                  ((symbol-function 'appkit-chat-avatar-column-pixel-width)
                   (lambda () 9))
                  ((symbol-function 'appkit-chat-avatar--graphical-display-p)
                   (lambda () t))
                  ((symbol-function 'disco-media-clear-preview-memory-cache)
                   #'ignore))
          (disco-room-render)
          (let ((node (appkit-chat-timeline-node "m1")))
            (should (= 21 (avatar-slice-height)))
            (setq line-height 35)
            (disco-room--on-text-scale-change)
            ;; The hook only invalidates; the Appkit transaction owns redraw.
            (should (= 21 (avatar-slice-height)))
            (appkit-sync-invalidations (appkit-current-view))
            (should (eq node (appkit-chat-timeline-node "m1")))
            (should (= 35 (avatar-slice-height)))))))))

(ert-deftest disco-room-window-resize-refreshes-presentation-geometry ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "geometry")
    (let ((view (disco-room--ensure-view)))
      (setq-local disco-room--chat-fill-column 70)
      (let (request)
        (cl-letf (((symbol-function 'disco-room--update-chat-fill-column)
                   (lambda (&optional _window)
                     (setq disco-room--chat-fill-column 90)))
                  ((symbol-function 'disco-room-render)
                   (lambda () (ert-fail "window hook rendered directly")))
                  ((symbol-function 'disco-room--refresh-timeline-layout)
                   (lambda () (ert-fail "window hook refreshed directly")))
                  ((symbol-function 'appkit-sync-invalidations)
                   (lambda (&rest _args) (ert-fail "window hook synced directly")))
                  ((symbol-function 'appkit-request-sync)
                   (lambda (owner &rest args)
                     (setq request (cons owner args)))))
          (disco-room--on-window-size-change)
          (should (eq view (car request)))
          (should (eq 'geometry (plist-get (cdr request) :part))))))))

(ert-deftest disco-room-session-cache-reset-revokes-icon-callbacks-without-sync ()
  (let ((disco-room--session-cache-reset-in-progress nil)
        (disco-room--avatar-fetch-generation 2)
        (disco-room--forward-guild-icon-fetch-generation 4)
        (disco-room--avatar-image-cache (make-hash-table :test #'equal))
        (disco-room--avatar-round-image-cache (make-hash-table :test #'equal))
        (disco-room--avatar-fetching (make-hash-table :test #'equal))
        (disco-room--avatar-failures (make-hash-table :test #'equal))
        (disco-room--avatar-pending-invalidations
         (make-hash-table :test #'equal))
        (disco-room--forward-guild-icon-image-cache
         (make-hash-table :test #'equal))
        (disco-room--forward-guild-icon-fetching
         (make-hash-table :test #'equal))
        (disco-room--avatar-plz-queue nil)
        (disco-room--avatar-plz-queue-limit nil)
        (disco-room--avatar-retry-timer nil)
        (disco-room--avatar-invalidation-timer nil)
        (disco-room-draft-history-search-history
         '("OLD_ACCOUNT_SECRET-draft"))
        (disco-room-search-inplace-history
         '("OLD_ACCOUNT_SECRET-search"))
        then-callback
        else-callback
        canceled
        (plz-calls 0)
        (sync-count 0))
    (puthash "avatar-secret" "OLD_ACCOUNT_SECRET-image"
             disco-room--avatar-image-cache)
    (puthash "round-secret" "OLD_ACCOUNT_SECRET-round"
             disco-room--avatar-round-image-cache)
    (puthash "failure-secret"
             '(:url "https://OLD_ACCOUNT_SECRET.invalid/avatar.png")
             disco-room--avatar-failures)
    (puthash "pending-secret" t
             disco-room--avatar-pending-invalidations)
    (puthash "old-icon" "https://OLD_ACCOUNT_SECRET.invalid/icon.png"
             disco-room--forward-guild-icon-image-cache)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (cl-incf plz-calls)
                 (setq then-callback (plist-get args :then)
                       else-callback (plist-get args :else))
                 'old-icon-process))
              ((symbol-function 'process-live-p)
               (lambda (process) (eq process 'old-icon-process)))
              ((symbol-function 'delete-process)
               (lambda (process)
                 (setq canceled process)
                 ;; Cancellation can run sentinels synchronously.  Both the
                 ;; old callback and an attempted successor must stay inert.
                 (funcall then-callback "OLD_ACCOUNT_SECRET-bytes")
                 (disco-room--start-forward-guild-icon-fetch
                  "reentrant-icon" "new-guild"
                  "https://OLD_ACCOUNT_SECRET.invalid/reentrant.png")))
              ((symbol-function 'create-image)
               (lambda (&rest _args) :image))
              ((symbol-function 'disco-room--forward-guild-icon-image-valid-p)
               (lambda (image) (eq image :image)))
              ((symbol-function 'disco-room--sync-resource-changes-in-open-rooms)
               (lambda (&rest _args) (cl-incf sync-count)))
              ((symbol-function 'disco-room--refresh-open-rooms)
               (lambda () (ert-fail "session reset requested a redraw"))))
      (disco-room--start-forward-guild-icon-fetch
       "live-icon" "old-guild"
       "https://OLD_ACCOUNT_SECRET.invalid/live.png")
      (should (= 1 plz-calls))
      (should (eq 'old-icon-process
                  (plist-get
                   (gethash "live-icon"
                            disco-room--forward-guild-icon-fetching)
                   :process)))
      (disco-room-reset-session-cache-state)
      (should (eq 'old-icon-process canceled))
      (should (= 1 plz-calls))
      (should (= 0 sync-count))
      (dolist (table (list disco-room--avatar-image-cache
                           disco-room--avatar-round-image-cache
                           disco-room--avatar-fetching
                           disco-room--avatar-failures
                           disco-room--avatar-pending-invalidations
                           disco-room--forward-guild-icon-image-cache
                           disco-room--forward-guild-icon-fetching))
        (should (= 0 (hash-table-count table))))
      (should-not disco-room-draft-history-search-history)
      (should-not disco-room-search-inplace-history)
      ;; A response already queued by plz remains harmless after reset too.
      (funcall then-callback "OLD_ACCOUNT_SECRET-late-bytes")
      (funcall else-callback '(:message "OLD_ACCOUNT_SECRET-late-error"))
      (should (= 0 sync-count))
      (should (= 0 (hash-table-count
                    disco-room--forward-guild-icon-image-cache))))))

(ert-deftest disco-room-session-cache-reset-clears-after-cancel-failures ()
  (dolist (failure '(error quit throw))
    (let ((disco-room--forward-guild-icon-fetch-generation 1)
          (disco-room--forward-guild-icon-image-cache
           (make-hash-table :test #'equal))
          (disco-room--forward-guild-icon-fetching
           (make-hash-table :test #'equal))
          (disco-room--avatar-image-cache (make-hash-table :test #'equal))
          (disco-room--avatar-round-image-cache (make-hash-table :test #'equal))
          (disco-room--avatar-fetching (make-hash-table :test #'equal))
          (disco-room--avatar-failures (make-hash-table :test #'equal))
          (disco-room--avatar-pending-invalidations
           (make-hash-table :test #'equal))
          (disco-room--avatar-plz-queue nil)
          (disco-room--avatar-retry-timer nil)
          (disco-room--avatar-invalidation-timer nil))
      (puthash "secret" "OLD_ACCOUNT_SECRET"
               disco-room--forward-guild-icon-image-cache)
      (puthash "secret" (list :generation 1 :process 'process)
               disco-room--forward-guild-icon-fetching)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
                ((symbol-function 'delete-process)
                 (lambda (_process)
                   (pcase failure
                     ('error (error "cancel failed"))
                     ('quit (signal 'quit nil))
                     ('throw (throw 'cancel-escape :escaped))))))
        (let ((result
               (catch 'cancel-escape
                 (disco-room-reset-session-cache-state)
                 :returned)))
          (if (eq failure 'throw)
              (should (eq result :escaped))
            (should (eq result :returned))))
        (should (= 0 (hash-table-count
                      disco-room--forward-guild-icon-image-cache)))
        (should (= 0 (hash-table-count
                      disco-room--forward-guild-icon-fetching)))))))

(ert-deftest disco-room-icon-process-cancel-drain-is-stack-safe ()
  (let ((max-lisp-eval-depth 800)
        (canceled 0))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (_process) (cl-incf canceled))))
      (disco-room--cancel-icon-processes (number-sequence 1 2000)))
    (should (= 2000 canceled)))
  (let (canceled)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (process)
                 (push process canceled)
                 (throw 'cancel-escape process))))
      (should (eq 'third
                  (catch 'cancel-escape
                    (disco-room--cancel-icon-processes
                     '(escape second third))
                    :returned))))
    (should (equal '(third second escape) canceled))))

(ert-deftest disco-room-preview-name-collision-preserves-ordinary-buffer ()
  (let* ((disco-room--preview-buffer nil)
         (disco-room--preview-buffer-name
          (generate-new-buffer-name "*disco-room-preview-collision*"))
         (ordinary (get-buffer-create disco-room--preview-buffer-name))
         owned)
    (unwind-protect
        (progn
          (with-current-buffer ordinary
            (insert "UNRELATED_PREVIEW_SENTINEL"))
          (setq owned (disco-room--owned-preview-buffer))
          (should (buffer-live-p owned))
          (should-not (eq ordinary owned))
          (should (buffer-local-value
                   'disco-room--preview-buffer-owner-p owned))
          (with-current-buffer owned
            (special-mode)
            (rename-buffer "*renamed-owned-room-preview*" t))
          (should (eq owned (disco-room--owned-preview-buffer)))
          (with-current-buffer ordinary
            (should (equal "UNRELATED_PREVIEW_SENTINEL" (buffer-string)))))
      (when (buffer-live-p owned)
        (kill-buffer owned))
      (when (buffer-live-p ordinary)
        (kill-buffer ordinary)))))

(provide 'disco-room-test)

;;; disco-room-test.el ends here
