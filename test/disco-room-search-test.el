;;; disco-room-search-test.el --- Tests for room search and filters -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-room)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

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

;;; disco-room-search-test.el ends here
