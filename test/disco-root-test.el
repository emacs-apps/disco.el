;;; disco-root-test.el --- Tests for disco-root live patching -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mouse)
(require 'seq)

(require 'disco-root)

(cl-defun disco-root-test--invalidations (&key entries structure parts)
  "Return Appkit invalidations with ENTRIES, STRUCTURE, and PARTS."
  (let ((invalidations (appkit-invalidations-create)))
    (setf (appkit-invalidations-entry-keys invalidations) entries
          (appkit-invalidations-structure-p invalidations) structure
          (appkit-invalidations-parts invalidations) parts)
    invalidations))

(defun disco-root-test--current-live-view ()
  "Attach and return a live Appkit view for the current test buffer."
  (when (and (eq major-mode 'disco-root-archived-threads-mode)
             (null disco-root--archived-parent-channel))
    (setq-local disco-root--archived-parent-channel '((id . "parent"))))
  (disco-root--ensure-view))

(defun disco-root-test--primary-click (window position)
  "Return a real primary-click event pair in WINDOW at POSITION."
  (let ((posn (list window position '(0 . 0) 0 nil position)))
    (vector (list 'down-mouse-1 posn)
            (list 'mouse-1 posn))))

(ert-deftest disco-root-displayable-channel-requires-authoritative-access ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (should
         (disco-root--displayable-channel-p
          '((id . "dm") (type . 1))))
        (should-not
         (disco-root--displayable-channel-p
          '((id . "unknown") (guild_id . "g1") (type . 0))))
        (should-not
         (disco-root--displayable-channel-p
          '((id . "hidden") (guild_id . "g1") (type . 0)
            (permissions . "0"))))
        (disco-state-put-channels
         "g1"
         '(((id . "hidden") (guild_id . "g1") (type . 0)
            (permissions . "0"))
           ((id . "visible") (guild_id . "g1") (type . 0)
            (permissions . "1024"))))
        (should
         (disco-root--displayable-channel-p
          (disco-state-channel "visible")))
        (disco-state-upsert-gateway-channel
         '((id . "gateway-visible") (guild_id . "g1")
           (type . 0) (flags . 0)))
        (disco-state-upsert-gateway-channel
         `((id . "gateway-hidden") (guild_id . "g1")
           (type . 0) (flags . ,disco-channel-flag-obfuscated)))
        (should
         (disco-root--displayable-channel-p
          (disco-state-channel "gateway-visible")))
        (should-not
         (disco-root--displayable-channel-p
          (disco-state-channel "gateway-hidden"))))
    (disco-state-reset)))

(ert-deftest disco-root-unread-does-not-depend-on-expanding-guild-directory ()
  (disco-state-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-root-mode)
        (let ((surface (disco-root--ensure-tree-directory-surface)))
          (disco-state-set-guilds '(((id . "g1") (name . "Guild"))))
          (disco-state-apply-ready-read-state-entry
           '((id . "gateway-unread") (last_message_id . "100")))
          (disco-state-apply-ready-read-state-entry
           '((id . "gateway-hidden-unread") (last_message_id . "100")))
          (disco-state-seed-guild-channels
           "g1"
           `(((id . "gateway-unread") (type . 0) (flags . 0)
              (name . "updates") (last_message_id . "101"))
             ((id . "gateway-hidden-unread") (type . 0)
              (flags . ,disco-channel-flag-obfuscated)
              (name . "secret") (last_message_id . "102"))))
          (cl-letf (((symbol-function 'disco-directory-load-guild-async)
                     (lambda (&rest _args)
                       (ert-fail "Unread projection loaded a collapsed guild"))))
            (should-not (disco-root--tree-guild-expanded-p surface "g1"))
            (should
             (equal '("gateway-unread")
                    (mapcar
                     (lambda (channel) (alist-get 'id channel))
                     (disco-root--collect-visible-unread-channels))))
            (should
             (seq-find
              (lambda (entry)
                (equal '(root unread channel "gateway-unread")
                       (appkit-directory-entry-key entry)))
              (disco-root--tree-layout-entries surface)))
            (should-not (disco-root--tree-guild-expanded-p surface "g1")))))
    (disco-state-reset)))

(ert-deftest disco-root-ready-event-projects-unread-before-guild-expansion ()
  (let ((disco-gateway--session-generation 0)
        (disco-gateway-event-hook nil))
    (disco-state-reset)
    (unwind-protect
        (with-temp-buffer
          (disco-root-mode)
          (let ((surface (disco-root--ensure-tree-directory-surface))
                queued-events)
            (setq disco-root--refresh-in-flight t)
            (add-hook 'disco-gateway-event-hook
                      (lambda (event)
                        (push (plist-get event :type) queued-events)
                        (disco-root--handle-gateway-event event)))
            (cl-letf (((symbol-function 'disco-root--queue-live-update)
                       (lambda (_channel-ids &optional structural-p _header-p)
                         (when structural-p
                           (push 'structural queued-events))))
                      ((symbol-function
                        'disco-gateway--subscribe-watched-guild-channels)
                       #'ignore)
                      ((symbol-function 'disco-gateway--reset-reconnect-backoff)
                       #'ignore)
                      ((symbol-function 'disco-directory-load-guild-async)
                       (lambda (&rest _args)
                         (ert-fail "READY unread projection hydrated a guild")))
                      ((symbol-function 'message) #'ignore))
              (disco-gateway--dispatch-ready
               `((session_id . "session")
                 (user . ((id . "self")))
                 (read_state
                  . [((id . "visible") (last_message_id . "100"))
                     ((id . "hidden") (last_message_id . "100"))])
                 (guilds
                  . [((id . "g1") (name . "Guild")
                      (channels
                       . [((id . "visible") (type . 0) (flags . 0)
                           (name . "updates") (last_message_id . "101"))
                          ((id . "hidden") (type . 0)
                           (flags . ,disco-channel-flag-obfuscated)
                           (name . "secret") (last_message_id . "102"))]))])))
              (should (memq 'guild-sync queued-events))
              (should (memq 'structural queued-events))
              (should-not disco-root--refresh-in-flight)
              (should-not (disco-root--tree-guild-expanded-p surface "g1"))
              (let ((entries (disco-root--tree-layout-entries surface)))
                (should
                 (seq-find
                  (lambda (entry)
                    (equal '(root unread channel "visible")
                           (appkit-directory-entry-key entry)))
                  entries))
                (should-not
                 (seq-find
                  (lambda (entry)
                    (equal '(root unread channel "hidden")
                           (appkit-directory-entry-key entry)))
                  entries))))))
      (disco-state-reset))))

(ert-deftest disco-root-activity-candidates-exclude-unresolved-and-hidden-channels ()
  (disco-state-reset)
  (unwind-protect
      (let ((disco-root-activity-include-threads nil))
        (disco-state-set-guilds '(((id . "g1"))))
        (disco-state-put-channels
         "g1"
         '(((id . "unknown") (guild_id . "g1") (type . 0))
           ((id . "hidden") (guild_id . "g1") (type . 0)
            (permissions . "0"))
           ((id . "visible") (guild_id . "g1") (type . 0)
            (permissions . "1024"))))
        (should
         (equal '("visible")
                (mapcar (lambda (channel) (alist-get 'id channel))
                        (disco-root--collect-activity-candidates)))))
    (disco-state-reset)))

(ert-deftest disco-root-thread-parent-candidates-honor-latest-gateway-access ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-set-guilds '(((id . "g1") (name . "Guild"))))
        (disco-state-put-channels
         "g1"
         '(((id . "forum") (guild_id . "g1") (name . "Forum")
            (type . 15) (permissions . "1024"))))
        (should (= 1 (length (disco-root--thread-parent-candidates))))
        (disco-state-upsert-gateway-channel
         `((id . "forum") (guild_id . "g1") (name . "Forum")
           (type . 15) (flags . ,disco-channel-flag-obfuscated)))
        (should-not (disco-root--thread-parent-candidates)))
    (disco-state-reset)))

(ert-deftest disco-root-visible-guild-unread-total-excludes-inaccessible-channels ()
  (disco-state-reset)
  (unwind-protect
      (let ((disco-root--view-mode 'all)
            counted)
        (disco-state-put-channels
         "g1"
         '(((id . "unknown") (guild_id . "g1") (type . 0))
           ((id . "hidden") (guild_id . "g1") (type . 0)
            (permissions . "0"))
           ((id . "visible") (guild_id . "g1") (type . 0)
            (permissions . "1024"))))
        (cl-letf (((symbol-function 'disco-state-guild-threads) #'ignore)
                  ((symbol-function 'disco-state-channels-unread-total)
                   (lambda (channels)
                     (setq counted
                           (mapcar (lambda (channel) (alist-get 'id channel))
                                   channels))
                     7)))
          (should (= 7 (disco-root--guild-unread-total "g1" t)))
          (should (equal '("visible") counted))))
    (disco-state-reset)))

(ert-deftest disco-root-mode-uses-persistent-header-without-key-cheat-sheet ()
  (with-temp-buffer
    (disco-root-mode)
    (should (equal '(:eval (disco-root--header-line)) header-line-format))
    (let ((disco-root--layout 'tree)
          (disco-root--view-mode 'all)
          (disco-root--sort-mode 'activity))
      (cl-letf (((symbol-function 'disco-root--gateway-status-label)
                 (lambda () "Ready"))
                ((symbol-function 'disco-root--sessions-summary-label) #'ignore)
                ((symbol-function 'disco-root--voice-summary-label) #'ignore)
                ((symbol-function 'disco-root--feature-badge-summary) #'ignore)
                ((symbol-function 'disco-root--activity-metrics-by-view)
                 (lambda ()
                   '((all :count 12 :unread 7)
                     (unread :count 3 :unread 7)
                     (dms :count 4 :unread 2)))))
        (let ((header (substring-no-properties (disco-root--header-line))))
          (should (string-match-p "Disco" header))
          (should (string-match-p "Ready" header))
          (should (string-match-p "Main 12" header))
          (should (string-match-p "Home · Recent" header))
          (should-not (string-match-p "keys\\[" header))
          (should-not (string-match-p "Status:" header)))))))

(ert-deftest disco-root-header-redisplay-reuses-expensive-state-cache ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'tree)
          (disco-root--view-mode 'all)
          (disco-root--sort-mode 'activity)
          (disco-root--header-state-cache nil)
          (status-reads 0)
          (metric-scans 0))
      (cl-letf (((symbol-function 'disco-root--gateway-status-label)
                 (lambda ()
                   (cl-incf status-reads)
                   "Ready"))
                ((symbol-function 'disco-root--sessions-summary-label) #'ignore)
                ((symbol-function 'disco-root--voice-summary-label) #'ignore)
                ((symbol-function 'disco-root--feature-badge-summary) #'ignore)
                ((symbol-function 'disco-root--activity-metrics-by-view)
                 (lambda ()
                   (cl-incf metric-scans)
                   '((all :count 1 :unread 0)
                     (unread :count 0 :unread 0)
                     (dms :count 0 :unread 0))))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (disco-root--header-line)
        (disco-root--header-line)
        (should (= 2 status-reads))
        (should (= 1 metric-scans))
        (disco-root--refresh-header-line)
        (disco-root--header-line)
        (should (= 3 status-reads))
        (should (= 2 metric-scans))))))

(ert-deftest disco-root-state-reset-invalidates-appkit-view ()
  (with-temp-buffer
    (disco-root-mode)
    (let (queued)
      (cl-letf (((symbol-function 'appkit-current-view) (lambda () 'view))
                ((symbol-function 'appkit-view-live-p) (lambda (_view) t))
                ((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids &optional structural-p header-p)
                   (setq queued (list channel-ids structural-p header-p)))))
      (disco-root--handle-state-reset))
      (should (equal '(nil t t) queued)))))

(ert-deftest disco-root-attach-live-updates-does-not-own-render-timer ()
  (let ((disco-runtime--app nil)
        timer-created)
    (unwind-protect
        (with-temp-buffer
          (disco-root-mode)
          (cl-letf (((symbol-function 'run-with-timer)
                     (lambda (&rest _args) (setq timer-created t)))
                    ((symbol-function 'disco-gateway-watch-global) #'ignore)
                    ((symbol-function 'disco-gateway-unwatch-global) #'ignore))
            (disco-root--attach-live-updates)
            (should-not timer-created)))
      (disco-runtime-stop))))

(ert-deftest disco-root-live-subscriptions-are-view-owned ()
  (let ((disco-runtime--app nil)
        (disco-gateway-event-hook nil)
        (disco-directory-event-hook nil)
        (disco-preview-update-hook nil)
        (watch-count 0)
        (unwatch-count 0))
    (unwind-protect
        (with-temp-buffer
          (disco-root-mode)
          (cl-letf (((symbol-function 'disco-gateway-watch-global)
                     (lambda () (cl-incf watch-count)))
                    ((symbol-function 'disco-gateway-unwatch-global)
                     (lambda () (cl-incf unwatch-count))))
            (let* ((view (disco-root--attach-live-updates))
                   (gateway disco-root--gateway-handler)
                   (directory disco-root--directory-handler)
                   (preview disco-root--preview-handler)
                   (handle disco-root--live-updates-handle))
              (should (= 1 watch-count))
              (should (appkit-handle-alive-p handle))
              (should (memq gateway disco-gateway-event-hook))
              (should (memq directory disco-directory-event-hook))
              (should (memq preview disco-preview-update-hook))
              (appkit-kill-view view)
              (should (= 1 unwatch-count))
              (should-not (appkit-handle-alive-p handle))
              (should-not disco-root--live-updates-handle)
              (should-not (memq gateway disco-gateway-event-hook))
              (should-not (memq directory disco-directory-event-hook))
              (should-not (memq preview disco-preview-update-hook)))))
      (disco-runtime-stop))))

(ert-deftest disco-root-queue-live-update-coalesces-in-appkit ()
  (let ((disco-runtime--app nil)
        (scheduled-count 0)
        snapshots)
    (unwind-protect
        (with-temp-buffer
          (disco-root-mode)
          (let ((view (disco-root--ensure-view)))
            (setf (appkit-view-sync-function view)
                  (lambda (_view invalidations)
                    (push invalidations snapshots)))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_time _repeat _function &rest _args)
                         (cl-incf scheduled-count)
                         'fake-root-sync-timer)))
              (disco-root--queue-live-update '("c1") nil nil)
              (disco-root--queue-live-update '("c2" "c1") t nil)
              (disco-root--queue-live-update nil nil t)
              (let* ((pending (appkit-view-invalidations view))
                     (handle
                      (appkit-invalidations-scheduled-handle pending)))
                (should (= 1 scheduled-count))
                (should (appkit-handle-alive-p handle))
                (should (appkit-invalidations-structure-p pending))
                (should (equal '("c1" "c2")
                               (sort
                                (copy-sequence
                                 (appkit-invalidations-entry-keys pending))
                                #'string-lessp)))
                (should (equal '(header)
                               (appkit-invalidations-parts pending))))
              (appkit-sync-invalidations view)
              (should (= 1 (length snapshots)))
              (let ((snapshot (car snapshots)))
                (should (appkit-invalidations-structure-p snapshot))
                (should (memq 'header
                              (appkit-invalidations-parts snapshot)))))))
      (disco-runtime-stop))))

(ert-deftest disco-root-queue-live-update-keeps-exact-invalidation-domains ()
  (let ((disco-runtime--app nil))
    (unwind-protect
        (with-temp-buffer
          (disco-root-mode)
          (let ((view (disco-root--ensure-view)))
            (cl-letf (((symbol-function 'appkit-schedule-sync) #'ignore))
              (disco-root--queue-live-update '("c1" "c1") nil nil)
              (let ((invalidations (appkit-view-invalidations view)))
                (should (equal '("c1")
                               (appkit-invalidations-entry-keys invalidations)))
                (should-not
                 (appkit-invalidations-structure-p invalidations))
                (should-not (appkit-invalidations-parts invalidations)))
              (disco-root--queue-live-update nil nil t)
              (let ((invalidations (appkit-view-invalidations view)))
                (should (equal '(header)
                               (appkit-invalidations-parts invalidations)))
                (should-not
                 (appkit-invalidations-structure-p invalidations))))))
      (disco-runtime-stop))))

(ert-deftest disco-root-killed-view-cancels-sync-and-stales-callback ()
  (let ((disco-runtime--app nil)
        callback-called)
    (cl-letf (((symbol-function 'disco-gateway-watch-global) #'ignore)
              ((symbol-function 'disco-gateway-unwatch-global) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat _function &rest _args)
                 'fake-root-sync-timer)))
      (unwind-protect
          (with-temp-buffer
            (disco-root-mode)
            (let* ((view (disco-root--attach-live-updates))
                   (callback disco-root--gateway-handler))
              (disco-root--queue-live-update '("c1") nil nil)
              (let ((handle
                     (appkit-invalidations-scheduled-handle
                      (appkit-view-invalidations view))))
                (should (appkit-handle-alive-p handle))
                (appkit-kill-view view)
                (should-not (appkit-view-live-p view))
                (should-not (appkit-handle-alive-p handle))
                (cl-letf (((symbol-function 'disco-root--handle-gateway-event)
                           (lambda (_event) (setq callback-called t))))
                  (funcall callback '(:type message-create
                                      :channel-id "c1")))
                (should-not callback-called))))
        (disco-runtime-stop)))))

(ert-deftest disco-root-async-callbacks-only-queue-invalidations ()
  (with-temp-buffer
    (disco-root-mode)
    (let (queued)
      (cl-letf (((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids &optional structural-p header-p)
                   (push (list channel-ids structural-p header-p) queued)))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda () (ert-fail "async callback rendered directly")))
                ((symbol-function 'appkit-sync-invalidations)
                 (lambda (_view) (ert-fail "async callback synced directly"))))
        (disco-root--handle-gateway-event
         '(:type message-create :channel-id "c1"))
        (disco-root--handle-directory-event '(:type index-loading))
        (disco-root--handle-directory-event '(:type index-loaded))
        (disco-root--handle-preview-update "c2")
        (should (member '(("c1") nil nil) queued))
        (should (member '(nil nil t) queued))
        (should (member '(nil t t) queued))
        (should (member '(("c2") nil nil) queued))))))

(ert-deftest disco-root-directory-parent-thread-lifecycle-reconciles-state ()
  (with-temp-buffer
    (disco-root-mode)
    (let (queued)
      (cl-letf (((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids &optional structural-p header-p)
                   (push (list channel-ids structural-p header-p) queued)))
                ((symbol-function 'disco-directory-load-parent-threads-async)
                 (lambda (&rest _args)
                   (ert-fail "directory callback retried a parent load"))))
        (dolist (type '(parent-threads-loading
                        parent-threads-loaded
                        parent-threads-error))
          (disco-root--handle-directory-event
           (list :type type :guild-id "g1" :parent-id "forum"
                 :error '(:message "failed"))))
        (should (= 3 (length queued)))
        (dolist (update queued)
          (should (equal '(("forum") t nil) update)))))))

(ert-deftest disco-root-directory-guild-error-reconciles-without-retry ()
  (with-temp-buffer
    (disco-root-mode)
    (let (queued messages)
      (cl-letf (((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids &optional structural-p header-p)
                   (push (list channel-ids structural-p header-p) queued)))
                ((symbol-function 'disco-directory-load-guild-async)
                 (lambda (&rest _args)
                   (ert-fail "directory callback retried a guild load")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (disco-root--handle-directory-event
         '(:type guild-error :guild-id "g1" :error (:message "denied")))
        (should (equal '((nil t nil)) queued))
        (should (= 1 (length messages)))
        (should (string-match-p "g1.*denied" (car messages)))))))

(ert-deftest disco-root-tree-default-expands-sections-and-collapses-guilds ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((surface (disco-root--ensure-tree-directory-surface)))
      (dolist (section '(unread private guilds))
        (should (disco-root--tree-section-expanded-p surface section)))
      (should-not (disco-root--tree-guild-expanded-p surface "g1")))))

(ert-deftest disco-gateway-event-channel-ids-aggregates-and-dedupes ()
  (should
   (equal
    '("c0" "t0" "c1" "c2" "c3" "c4" "p4" "c5")
    (disco-gateway-event-channel-ids
     '(:channel-id "c0"
       :thread-id "t0"
       :channel-unread-updates (((id . "c1"))
                                ((channel_id . "c2"))
                                ((id . "c1")))
       :channels (((id . "c2"))
                  ((id . "c3")))
       :updated-channels (((channel_id . "c3"))
                          ((id . "c4")))
       :threads (((id . "c4") (parent_id . "p4"))
                 ((id . "c2") (parent_id . "p4")))
       :channel-ids ("c5" "c0"))))))

(ert-deftest disco-gateway-event-channel-ids-includes-voice-move-and-message-payloads ()
  (should
   (equal
    '("c2" "c1" "c3")
    (disco-gateway-event-channel-ids
     '(:channel-id "c2"
       :previous-channel-id "c1"
       :messages (((channel_id . "c3"))
                  ((channel_id . "c2"))))))))

(ert-deftest disco-root-append-extra-info-merges-provider-output ()
  (let ((disco-root-extra-info-functions
         (list (lambda (_kind _object _context) "one")
               (lambda (_kind _object _context) '("two" nil ""))
               (lambda (_kind _object _context) 3))))
    (let ((disco-root--extra-info-provider-error-cache (make-hash-table :test #'eq)))
      (should
       (equal "base one two 3"
              (disco-root--append-extra-info
               "base"
               'channel
               '((id . "chan"))
               '(:scope root)))))))

(ert-deftest disco-root-append-extra-info-provider-error-reported-once ()
  (let* ((provider (lambda (_kind _object _context)
                     (error "boom")))
         (disco-root-extra-info-functions (list provider))
         (disco-root--extra-info-provider-error-cache (make-hash-table :test #'eq))
         (message-count 0))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args)
                 (setq message-count (1+ message-count)))))
      (disco-root--append-extra-info "base" 'channel nil nil)
      (disco-root--append-extra-info "base" 'channel nil nil)
      (should (= 1 message-count)))))

(ert-deftest disco-root-sync-invalidations-renders-in-unread-view ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--view-mode 'unread)
          rendered)
      (cl-letf (((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should rendered)))))

(ert-deftest disco-root-sync-invalidations-does-not-poll-during-refresh ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--refresh-in-flight t)
          patched)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _args)
                   (ert-fail "root sync created a polling timer")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "root sync created a polling timer")))
                ((symbol-function 'disco-root--refresh-channel-node)
                 (lambda (_channel-id)
                   (setq patched t)
                   'updated))
                ((symbol-function 'disco-root--activity-reorder-visible-nodes)
                 (lambda (&optional _channel-ids) nil))
                ((symbol-function 'disco-root--refresh-active-layout-headings)
                 #'ignore)
                ((symbol-function 'disco-root--refresh-header-line) #'ignore))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should patched)))))

(ert-deftest disco-root-sync-invalidations-rerenders-archived-thread-buffer ()
  (with-temp-buffer
    (disco-root-archived-threads-mode)
    (let (rendered)
      (cl-letf (((symbol-function 'disco-root--archived-threads-list-spec)
                 (lambda () 'spec))
                ((symbol-function 'appkit-view-render-list-spec-preserving-position)
                 (lambda (_spec &rest _args)
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("t1")))
        (should rendered)))))

(ert-deftest disco-root-sync-invalidations-patches-in-all-view ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--view-mode 'all)
          patched
          heading-ids
          rendered)
      (cl-letf (((symbol-function 'disco-root--buffer-visible-p)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'disco-root--refresh-channel-node)
                 (lambda (channel-id)
                   (push channel-id patched)
                   'updated))
                ((symbol-function 'disco-root--refresh-active-layout-headings)
                 (lambda (channel-ids)
                   (setq heading-ids channel-ids)))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1" "c2")))
        (should-not rendered)
        (should (equal '("c1" "c2")
                       (sort (copy-sequence patched) #'string-lessp)))
        (should (equal '("c1" "c2")
                       (sort (copy-sequence heading-ids) #'string-lessp)))))))

(ert-deftest disco-root-sync-invalidations-renders-for-full-layout ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root-custom-layouts
           '((stress-full
              :label "Stress Full"
              :build disco-root--build-activity-layout-view-spec
              :update-mode full)))
          (disco-root--layout 'stress-full)
          rendered)
      (cl-letf (((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should rendered)))))

(ert-deftest disco-root-sync-invalidations-activity-reorders-incrementally ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--view-mode 'all)
          reordered
          rendered)
      (cl-letf (((symbol-function 'disco-root--buffer-visible-p)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'disco-root--selected-window-for-buffer)
                 (lambda (&optional _buffer) 'selected-root-win))
                ((symbol-function 'disco-root--refresh-channel-node)
                 (lambda (_channel-id) 'updated))
                ((symbol-function 'disco-root--activity-reorder-visible-nodes)
                 (lambda (&optional _channel-ids)
                   (setq reordered t)
                   nil))
                ((symbol-function 'disco-root--refresh-header-line) #'ignore)
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should reordered)
        (should-not rendered)))))

(ert-deftest disco-root-sync-invalidations-hidden-buffer-keeps-incremental-path ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--view-mode 'all)
          rendered
          patched)
      (cl-letf (((symbol-function 'disco-root--buffer-visible-p)
                 (lambda (&optional _buffer) nil))
                ((symbol-function 'disco-root--refresh-channel-node)
                 (lambda (_channel-id)
                   (setq patched t)
                   'updated))
                ((symbol-function 'disco-root--activity-reorder-visible-nodes)
                 (lambda (&optional _channel-ids) nil))
                ((symbol-function 'disco-root--refresh-active-layout-headings)
                 (lambda (_channel-ids) nil))
                ((symbol-function 'disco-root--refresh-header-line) #'ignore)
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should patched)
        (should-not rendered)))))

(ert-deftest disco-root-sync-invalidations-unfocused-activity-keeps-incremental-path ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--view-mode 'all)
          rendered
          patched
          restored)
      (cl-letf (((symbol-function 'disco-root--buffer-visible-p)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'disco-root--selected-window-for-buffer)
                 (lambda (&optional _buffer) nil))
                ((symbol-function 'appkit-position-capture)
                 (lambda (&rest _args) 'snapshot))
                ((symbol-function 'appkit-position-restore)
                 (lambda (_snapshot)
                   (setq restored t)))
                ((symbol-function 'disco-root--refresh-channel-node)
                 (lambda (_channel-id)
                   (setq patched t)
                   'updated))
                ((symbol-function 'disco-root--activity-reorder-visible-nodes)
                 (lambda (&optional _channel-ids) nil))
                ((symbol-function 'disco-root--refresh-active-layout-headings)
                 (lambda (_channel-ids) nil))
                ((symbol-function 'disco-root--refresh-header-line) #'ignore)
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should patched)
        (should restored)
        (should-not rendered)))))

(ert-deftest disco-root-sync-invalidations-refreshes-header-in-same-sync ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          called)
      (cl-letf (((symbol-function 'disco-root--selected-window-for-buffer)
                 (lambda (&optional _buffer) 'selected-root-win))
                ((symbol-function 'disco-root--refresh-channel-node)
                 (lambda (_channel-id) 'updated))
                ((symbol-function 'disco-root--activity-reorder-visible-nodes)
                 (lambda (&optional _channel-ids)
                   nil))
                ((symbol-function 'disco-root--refresh-active-layout-headings)
                 (lambda (_channel-ids) nil))
                ((symbol-function 'disco-root--refresh-header-line)
                 (lambda ()
                   (setq called t)))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda () nil)))
        (disco-root--sync-invalidations
         (disco-root-test--current-live-view)
         (disco-root-test--invalidations :entries '("c1")))
        (should called)))))

(ert-deftest disco-root-activity-reorder-visible-nodes-dirty-path-skips-full-collect ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--ewoc t)
          full-collect-called
          reordered-ids)
      (cl-letf (((symbol-function 'disco-root--activity-reorder-channel-node)
                 (lambda (channel-id)
                   (push channel-id reordered-ids)
                   'moved))
                ((symbol-function 'disco-root--collect-activity-channels)
                 (lambda ()
                   (setq full-collect-called t)
                   nil)))
        (disco-root--activity-reorder-visible-nodes '("c1" "c1" "c2"))
        (should-not full-collect-called)
        (should (equal '("c1" "c2")
                       (sort (copy-sequence reordered-ids) #'string-lessp)))))))

(ert-deftest disco-root-activity-reorder-visible-nodes-signals-structural-reconcile ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--ewoc t))
      (cl-letf (((symbol-function 'disco-root--activity-reorder-channel-node)
                 (lambda (_channel-id)
                   'missing-visible)))
        (should (disco-root--activity-reorder-visible-nodes '("c1")))))))

(ert-deftest disco-root-rerender-open-root-buffers-uses-live-update-queue ()
  (let (queued)
    (with-temp-buffer
      (disco-root-mode)
      (cl-letf (((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids &optional structural-p header-p)
                   (push (list (current-buffer) channel-ids structural-p header-p)
                         queued))))
        (disco-root--rerender-open-root-buffers)
        (should (= 1 (length queued)))
        (pcase-let ((`(,buffer ,channel-ids ,structural-p ,header-p)
                     (car queued)))
          (should (buffer-live-p buffer))
          (should-not channel-ids)
          (should structural-p)
          (should-not header-p))))))

(ert-deftest disco-root-render-fill-column-hidden-buffer-reuses-last-width ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 88))
      (cl-letf (((symbol-function 'disco-root--display-window)
                 (lambda (&optional _buffer) nil))
                ((symbol-function 'disco-root--compute-fill-column)
                 (lambda (&optional _buffer _window) 42)))
        (should (= 88 (disco-root--render-fill-column)))))))

(ert-deftest disco-root-render-fill-column-background-visible-buffer-reuses-last-width ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 88)
          (calls 0))
      (cl-letf (((symbol-function 'disco-root--selected-window-for-buffer)
                 (lambda (&optional _buffer) nil))
                ((symbol-function 'disco-root--display-window)
                 (lambda (&optional _buffer) 'background-win))
                ((symbol-function 'window-live-p)
                 (lambda (win)
                   (eq win 'background-win)))
                ((symbol-function 'disco-root--compute-fill-column)
                 (lambda (&optional _buffer _window)
                   (setq calls (1+ calls))
                   42)))
        (should (= 88 (disco-root--render-fill-column)))
        (should (= 0 calls))))))

(ert-deftest disco-root-render-fill-column-background-visible-buffer-computes-first-width ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column nil)
          (calls 0))
      (cl-letf (((symbol-function 'disco-root--selected-window-for-buffer)
                 (lambda (&optional _buffer) nil))
                ((symbol-function 'disco-root--display-window)
                 (lambda (&optional _buffer) 'background-win))
                ((symbol-function 'window-live-p)
                 (lambda (win)
                   (eq win 'background-win)))
                ((symbol-function 'disco-root--compute-fill-column)
                 (lambda (&optional _buffer _window)
                   (setq calls (1+ calls))
                   42)))
        (should (= 42 (disco-root--render-fill-column)))
        (should (= 1 calls))))))

(ert-deftest disco-root-render-coalesces-reentrant-update ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((render-count 0)
          queued)
      (cl-letf (((symbol-function 'disco-root--render-fill-column)
                 (lambda (&optional _buffer) 80))
                ((symbol-function 'disco-root-layout-render)
                 (lambda (_layout)
                   (setq render-count (1+ render-count))
                   (disco-root-render)
                   t))
                ((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids &optional structural-p header-p)
                   (setq queued (list channel-ids structural-p header-p)))))
        (disco-root-render)
        (should (= render-count 1))
        (should (equal queued '(nil t nil)))
        (should-not disco-root--rendering)
        (should-not disco-root--render-pending)
        (should (string-empty-p (buffer-string)))))))

(ert-deftest disco-root-channel-row-keeps-help-without-blanket-hover ()
  (with-temp-buffer
    (let ((channel '((id . "c1") (type . 0) (name . "general"))))
      (cl-letf (((symbol-function 'disco-root--channel-label)
                 (lambda (&rest _args) "# general"))
                ((symbol-function 'disco-state-channel-effective-unread-count)
                 (lambda (_channel) 0))
                ((symbol-function 'disco-root--channel-has-unread-p)
                 (lambda (_channel) nil))
                ((symbol-function 'disco-root--openable-channel-p)
                 (lambda (_channel) t))
                ((symbol-function 'disco-root--channel-open-help-echo)
                 (lambda (_channel) "Open #general")))
        (disco-root--insert-channel-line channel 0)
        (should (equal (get-text-property (point-min) 'help-echo)
                       "Open #general"))
        (should-not (text-property-not-all
                     (point-min) (point-max) 'mouse-face nil))))))

(ert-deftest disco-root-hack-window-points-updates-prev-buffer-marker ()
  (with-temp-buffer
    (disco-root-mode)
    (let* ((marker (copy-marker (point-min)))
           (entry (list (current-buffer) nil marker))
           (prev-buffers (list entry)))
      (goto-char (point-max))
      (cl-letf (((symbol-function 'get-buffer-window-list)
                 (lambda (&rest _args) '(fake-win)))
                ((symbol-function 'window-live-p)
                 (lambda (_win) t))
                ((symbol-function 'window-prev-buffers)
                 (lambda (_win) prev-buffers)))
        (disco-root--hack-window-points)
        (should (= (point-max) (marker-position (nth 2 entry))))))))

(ert-deftest disco-root-render-preserving-position-syncs-window-points ()
  (with-temp-buffer
    (disco-root-mode)
    (let (rendered preserved anchor updated)
      (cl-letf (((symbol-function 'disco-root-render)
                 (lambda ()
                   (setq rendered t)))
                ((symbol-function 'appkit-position-render-preserving)
                 (lambda (fn &rest args)
                   (setq preserved (plist-get args :preserve-window-start))
                   (setq anchor (plist-get args :anchor-property))
                   (funcall fn)))
                ((symbol-function 'disco-root--update-window-points)
                 (lambda (&optional _point)
                   (setq updated t))))
        (disco-root--render-preserving-position)
        (should rendered)
        (should preserved)
        (should (eq appkit-directory-key-property anchor))
        (should updated)))))

(ert-deftest disco-root-reflow-preserving-position-syncs-window-points ()
  (with-temp-buffer
    (disco-root-mode)
    (let (reflowed preserved anchor updated)
      (cl-letf (((symbol-function 'disco-root--reflow-layout)
                 (lambda ()
                   (setq reflowed t)))
                ((symbol-function 'appkit-position-render-preserving)
                 (lambda (fn &rest args)
                   (setq preserved (plist-get args :preserve-window-start))
                   (setq anchor (plist-get args :anchor-property))
                   (funcall fn)))
                ((symbol-function 'disco-root--update-window-points)
                 (lambda (&optional _point)
                   (setq updated t))))
        (disco-root--reflow-preserving-position)
        (should reflowed)
        (should preserved)
        (should (eq appkit-directory-key-property anchor))
        (should updated)))))

(ert-deftest disco-root-update-window-points-keeps-live-windows-independent ()
  (with-temp-buffer
    (disco-root-mode)
    (let (passive-point)
      (cl-letf (((symbol-function 'disco-root--hack-window-points)
                 (lambda (&optional point) (setq passive-point point)))
                ((symbol-function 'set-window-point)
                 (lambda (&rest _arguments)
                   (ert-fail "Appkit owns per-window point restoration"))))
        (disco-root--update-window-points 17)
        (should (= 17 passive-point))))))

(ert-deftest disco-root-toggle-unread-lens-tree-toggles-section ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'tree)
          requested)
      (disco-root--ensure-tree-directory-surface)
      (let ((view (disco-root-test--current-live-view)))
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (candidate &rest arguments)
                     (setq requested (cons candidate arguments))))
                  ((symbol-function 'appkit-sync-invalidations) #'ignore)
                  ((symbol-function 'message) (lambda (&rest _args) nil)))
          (should (disco-root--section-expanded-p 'unread))
          (disco-root-toggle-unread-lens)
          (should (eq view (car requested)))
          (should (plist-get (cdr requested) :structure))
          (should-not (disco-root--section-expanded-p 'unread)))))))

(ert-deftest disco-root-toggle-unread-lens-activity-toggles-filter ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--view-mode 'all)
          (disco-root--pre-unread-view-mode 'all))
      (cl-letf (((symbol-function 'disco-root--render-preserving-position)
                 (lambda () nil))
                ((symbol-function 'message) (lambda (&rest _args) nil)))
        (disco-root-toggle-unread-lens)
        (should (eq disco-root--view-mode 'unread))
        (disco-root-toggle-unread-lens)
        (should (eq disco-root--view-mode 'all))))))

(ert-deftest disco-root-tree-collapsed-guild-does-not-project-or-load-children ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((surface (disco-root--ensure-tree-directory-surface))
          projected
          loaded)
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 #'ignore)
                ((symbol-function 'disco-root--visible-private-channels)
                 #'ignore)
                ((symbol-function 'disco-state-guilds)
                 (lambda () '(((id . "g1") (name . "Guild")))))
                ((symbol-function 'disco-root--guild-unread-total)
                 (lambda (&rest _args) 0))
                ((symbol-function 'disco-guild-directory-project)
                 (lambda (_context) (setq projected t) nil))
                ((symbol-function 'disco-directory-load-guild-async)
                 (lambda (&rest _args) (setq loaded t))))
        (let ((entries (disco-root--tree-layout-entries surface)))
          (should (seq-find
                   (lambda (entry)
                     (equal '(root guild "g1")
                            (appkit-directory-entry-key entry)))
                   entries))
          (should-not projected)
          (should-not loaded))))))

(ert-deftest disco-root-tree-projection-keeps-fixed-composite-order-and-keys ()
  (with-temp-buffer
    (disco-root-mode)
    (let* ((surface (disco-root--ensure-tree-directory-surface))
           (unread '((id . "u1") (guild_id . "g1") (type . 0)
                     (name . "updates")))
           (dm '((id . "d1") (type . 1) (name . "Alice")))
           (guild '((id . "g1") (name . "Guild"))))
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 (lambda () (list unread)))
                ((symbol-function 'disco-root--visible-private-channels)
                 (lambda () (list dm)))
                ((symbol-function 'disco-state-guilds)
                 (lambda () (list guild)))
                ((symbol-function 'disco-root--guild-unread-total)
                 (lambda (&rest _args) 0))
                ((symbol-function 'disco-directory-guild-status)
                 (lambda (_guild-id) 'loaded)))
        (let ((entries (disco-root--tree-layout-entries surface)))
          (should
           (equal
            '((root section unread)
              (root unread channel "u1")
              (root section dm)
              (root dm channel "d1")
              (root section guilds)
              (root guild "g1"))
            (mapcar #'appkit-directory-entry-key entries)))
          (should
           (seq-every-p
            #'appkit-directory-entry-expanded-p
            (seq-filter
             (lambda (entry)
               (eq (appkit-directory-entry-role entry) 'section))
             entries)))
          (should-not
           (appkit-directory-entry-expanded-p (car (last entries)))))))))

(ert-deftest disco-root-tree-expands-only-the-exact-guild ()
  (with-temp-buffer
    (disco-root-mode)
    (let* ((surface (disco-root--ensure-tree-directory-surface))
           (guilds '(((id . "g1") (name . "One"))
                     ((id . "g2") (name . "Two"))))
           loaded
           projected)
      (appkit-directory-set-fold-expanded surface '(root guild "g1") t)
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 #'ignore)
                ((symbol-function 'disco-root--visible-private-channels)
                 #'ignore)
                ((symbol-function 'disco-state-guilds)
                 (lambda () guilds))
                ((symbol-function 'disco-root--guild-unread-total)
                 (lambda (&rest _args) 0))
                ((symbol-function 'disco-directory-guild-status)
                 (lambda (_guild-id) 'unloaded))
                ((symbol-function 'disco-directory-load-guild-async)
                 (lambda (guild-id &rest _args)
                   (push guild-id loaded)))
                ((symbol-function 'disco-guild-directory-project)
                 (lambda (context)
                   (push (disco-guild-directory-context-guild-id context)
                         projected)
                   (should
                    (equal '(root guild "g1")
                           (disco-guild-directory-context-section-key
                            context)))
                   nil)))
        (disco-root--tree-layout-entries surface)
        (should (equal '("g1") loaded))
        (should (equal '("g1") projected))))))

(ert-deftest disco-root-tree-expanded-guild-projects-loading-lifecycle ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((surface (disco-root--ensure-tree-directory-surface))
          (guild '((id . "g1") (name . "Guild"))))
      (appkit-directory-set-fold-expanded surface '(root guild "g1") t)
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 #'ignore)
                ((symbol-function 'disco-root--visible-private-channels)
                 #'ignore)
                ((symbol-function 'disco-state-guilds)
                 (lambda () (list guild)))
                ((symbol-function 'disco-root--guild-unread-total)
                 (lambda (&rest _args) 0))
                ((symbol-function 'disco-directory-guild-status)
                 (lambda (_guild-id) 'loading))
                ((symbol-function 'disco-state-guild-channels-loaded-p)
                 (lambda (_guild-id) nil))
                ((symbol-function 'disco-directory-load-guild-async)
                 (lambda (&rest _args)
                   (ert-fail "loading guild was requested again"))))
        (let ((entries (disco-root--tree-layout-entries surface)))
          (should
           (seq-find
            (lambda (entry)
              (and (equal
                    '(disco-guild-directory (root guild "g1") "g1"
                                            note loading)
                    (appkit-directory-entry-key entry))
                   (equal "Loading channels…"
                          (appkit-directory-entry-label entry))))
            entries)))))))

(ert-deftest disco-root-tree-expanded-guild-projects-loaded-channel-tree ()
  (with-temp-buffer
    (disco-root-mode)
    (let* ((surface (disco-root--ensure-tree-directory-surface))
           (guild '((id . "g1") (name . "Guild")))
           (category '((id . "cat") (guild_id . "g1")
                       (type . 4) (name . "General") (position . 0)))
           (channel '((id . "c1") (guild_id . "g1")
                      (parent_id . "cat") (type . 0)
                      (name . "chat") (position . 1))))
      (appkit-directory-set-fold-expanded surface '(root guild "g1") t)
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 #'ignore)
                ((symbol-function 'disco-root--visible-private-channels)
                 #'ignore)
                ((symbol-function 'disco-state-guilds)
                 (lambda () (list guild)))
                ((symbol-function 'disco-root--guild-unread-total)
                 (lambda (&rest _args) 0))
                ((symbol-function 'disco-directory-guild-status)
                 (lambda (_guild-id) 'loaded))
                ((symbol-function 'disco-state-guild-channels-loaded-p)
                 (lambda (_guild-id) t))
                ((symbol-function 'disco-state-guild-channels)
                 (lambda (_guild-id) (list category channel)))
                ((symbol-function 'disco-state-channel)
                 (lambda (channel-id)
                   (and (equal channel-id "cat") category)))
                ((symbol-function 'disco-state-channel-viewable-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-state-channel-has-unread-p)
                 (lambda (_channel) nil))
                ((symbol-function 'disco-state-channel-effective-unread-count)
                 (lambda (_channel) 0))
                ((symbol-function 'disco-msg-channel-last-cached-message)
                 (lambda (_channel) nil))
                ((symbol-function 'disco-directory-load-guild-async)
                 (lambda (&rest _args)
                   (ert-fail "loaded guild was requested again"))))
        (let ((entries (disco-root--tree-layout-entries surface)))
          (should
           (seq-find
            (lambda (entry)
              (equal
               '(disco-guild-directory (root guild "g1") "g1"
                                       group "cat")
               (appkit-directory-entry-key entry)))
            entries))
          (should
           (seq-find
            (lambda (entry)
              (equal
               '(disco-guild-directory (root guild "g1") "g1"
                                       channel "c1")
               (appkit-directory-entry-key entry)))
            entries)))))))

(ert-deftest disco-root-tree-fold-dispatches-category-and-forum-exactly ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((surface (disco-root--ensure-tree-directory-surface))
          loaded-parents
          (rendered 0))
      (cl-labels
          ((entry (kind &optional parent-id)
             (appkit-directory-entry-create
              :key (list 'shared kind (or parent-id "x"))
              :role (if (eq kind 'group) 'group 'item)
              :section-key '(root guild "g1")
              :item-p (eq kind 'thread-parent)
              :properties
              (list disco-guild-directory-row-kind-property kind
                    disco-guild-directory-thread-parent-id-property
                    parent-id))))
        (cl-letf (((symbol-function 'disco-directory-load-guild-async)
                   (lambda (&rest _args)
                     (ert-fail "category/forum fold loaded a guild")))
                  ((symbol-function 'disco-directory-load-parent-threads-async)
                   (lambda (parent-id &rest _args)
                     (push parent-id loaded-parents)))
                  ((symbol-function 'disco-root-render)
                   (lambda () (cl-incf rendered))))
          (disco-root--tree-fold-changed surface (entry 'group) nil)
          (should-not loaded-parents)
          (disco-root--tree-fold-changed
           surface (entry 'thread-parent "forum-1") t)
          (should (equal '("forum-1") loaded-parents))
          (disco-root--tree-fold-changed
           surface (entry 'thread-parent "forum-2") nil)
          (should (equal '("forum-1") loaded-parents))
          (should (= 3 rendered)))))))

(ert-deftest disco-root-tree-parent-page-actions-dispatch-exactly ()
  (let ((parent '((id . "forum") (guild_id . "g1") (type . 15)))
        calls)
    (cl-letf (((symbol-function 'disco-directory-load-parent-threads-async)
               (lambda (parent-id &rest _args)
                 (push (list 'load parent-id) calls)))
              ((symbol-function
                'disco-directory-load-more-parent-threads-async)
               (lambda (parent-id) (push (list 'more parent-id) calls)))
              ((symbol-function
                'disco-directory-retry-parent-threads-async)
               (lambda (parent-id) (push (list 'retry parent-id) calls))))
      (dolist (kind '(parent-threads-load
                      parent-threads-load-more
                      parent-threads-retry))
        (disco-root--tree-activate-item
         nil
         (appkit-directory-entry-create
          :key kind :role 'item :section-key '(root guild "g1")
          :item-p t :payload parent
          :properties
          (list disco-guild-directory-row-kind-property kind
                disco-guild-directory-thread-parent-id-property "forum"))))
      (should (equal '((retry "forum") (more "forum") (load "forum"))
                     calls)))))

(ert-deftest disco-root-tree-dirty-channel-refreshes-every-visible-occurrence ()
  (with-temp-buffer
    (disco-root-mode)
    (let* ((surface (disco-root--ensure-tree-directory-surface))
           (channel '((id . "c1") (guild_id . "g1")
                      (type . 0) (name . "chat")))
           (guild '((id . "g1") (name . "Guild")))
           (unread-p t))
      (appkit-directory-set-fold-expanded surface '(root guild "g1") t)
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 (lambda () (and unread-p (list channel))))
                ((symbol-function 'disco-root--visible-private-channels)
                 #'ignore)
                ((symbol-function 'disco-state-guilds)
                 (lambda () (list guild)))
                ((symbol-function 'disco-root--guild-unread-total)
                 (lambda (&rest _args) (if unread-p 1 0)))
                ((symbol-function 'disco-directory-guild-status)
                 (lambda (_guild-id) 'loaded))
                ((symbol-function 'disco-guild-directory-project)
                 (lambda (context)
                   (list
                    (appkit-directory-entry-create
                     :key (disco-guild-directory-channel-key context "c1")
                     :role 'item
                     :section-key '(root guild "g1")
                     :item-p t
                     :payload channel)))))
        (let* ((before (disco-root--tree-layout-entries surface))
               (before-occurrences
                (disco-root--tree-channel-occurrence-keys before "c1")))
          (should
           (equal
            '((root unread channel "c1")
              (disco-guild-directory (root guild "g1") "g1"
                                      channel "c1"))
            before-occurrences))
          (let ((disco-root--tree-force-channel-ids '("c1")))
            (should (equal before-occurrences
                           (disco-root--tree-force-keys before))))
          (setq unread-p nil)
          (let* ((after (disco-root--tree-layout-entries surface))
                 (after-occurrences
                  (disco-root--tree-channel-occurrence-keys after "c1")))
            (should
             (equal
              '((disco-guild-directory (root guild "g1") "g1"
                                        channel "c1"))
              after-occurrences))
            (should-not
             (seq-find
              (lambda (entry)
                (equal '(root unread channel "c1")
                       (appkit-directory-entry-key entry)))
              after))))))))

(ert-deftest disco-root-tree-items-use-rich-renderer-with-scoped-context ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((channel '((id . "c1") (type . 1)))
          calls
          (inhibit-read-only t))
      (cl-letf (((symbol-function 'disco-root--insert-activity-channel-line)
                 (lambda (candidate indent &optional scope width)
                   (push (list (alist-get 'id candidate) indent scope width)
                         calls)
                   (insert "rich\n"))))
        (dolist (entry
                 (list
                  (appkit-directory-entry-create
                   :key '(root unread channel "c1")
                   :role 'item :section-key '(root section unread)
                   :payload channel
                   :properties
                   (list disco-root-directory-row-kind-property
                         'unread-channel))
                  (appkit-directory-entry-create
                   :key '(root dm channel "c1")
                   :role 'item :section-key '(root section dm)
                   :payload channel
                   :properties
                   (list disco-root-directory-row-kind-property 'dm-channel))
                  (appkit-directory-entry-create
                   :key '(shared channel "c1")
                   :role 'item :section-key '(root guild "g1")
                   :payload channel
                   :properties
                   (list disco-guild-directory-row-kind-property 'channel))))
          (disco-root--tree-item-inserter nil entry))
        (should
         (equal '(("c1" 0 unread nil)
                  ("c1" 0 dm nil)
                  ("c1" 0 directory nil))
                (nreverse calls)))))))

(ert-deftest disco-root-tree-renders-without-buttons-or-whole-row-hover ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--view-mode 'all)
          (dm '((id . "d1") (type . 1) (name . "Alice"))))
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 (lambda () (list dm)))
                ((symbol-function 'disco-root--visible-private-channels)
                 (lambda () (list dm)))
                ((symbol-function 'disco-state-guilds) #'ignore)
                ((symbol-function 'disco-root--insert-activity-channel-line)
                 (lambda (&rest _args) (insert "rich\n"))))
        (disco-root-layout-render-view-spec
         (disco-root--build-tree-layout-view-spec))
        (should-not (next-button (point-min) t))
        (should-not
         (text-property-not-all (point-min) (point-max) 'mouse-face nil))
        (should-not
         (appkit-directory-surface-action-rows-p
          (appkit-directory-current-surface)))))))

(ert-deftest disco-root-tree-layout-switch-rebuilds-surface-and-keeps-folds ()
  (with-temp-buffer
    (disco-root-mode)
    (let* ((first (disco-root--ensure-tree-directory-surface))
           (fold-key '(root guild "g1")))
      (appkit-directory-set-fold-expanded first fold-key t)
      (cl-letf (((symbol-function 'disco-root--refresh-header-line) #'ignore)
                ((symbol-function 'disco-root--render-fill-column)
                 (lambda (&optional _buffer) 80))
                ((symbol-function 'disco-root--collect-activity-channels)
                 #'ignore)
                ((symbol-function 'disco-root--collect-visible-unread-channels)
                 #'ignore)
                ((symbol-function 'disco-root--visible-private-channels)
                 #'ignore)
                ((symbol-function 'disco-state-guilds) #'ignore))
        (setq-local disco-root--layout 'activity)
        (disco-root-render)
        (should-not (appkit-directory-current-surface))
        (setq-local disco-root--layout 'tree)
        (disco-root-render)
        (let ((second (appkit-directory-current-surface)))
          (should (appkit-directory-surface-p second))
          (should-not (eq first second))
          (should (appkit-directory-fold-expanded-p
                   second fold-key nil)))))))

(ert-deftest disco-root-normal-lifecycle-never-sweeps-unexpanded-guilds ()
  (with-temp-buffer
    (disco-root-mode)
    (let (queued)
      (cl-letf (((symbol-function 'disco-directory-load-guild-async)
                 (lambda (&rest _args)
                   (ert-fail "ordinary root lifecycle swept a guild")))
                ((symbol-function 'disco-root--queue-live-update)
                 (lambda (&rest args) (push args queued))))
        (disco-root--handle-gateway-event '(:type guild-create))
        (disco-root--handle-directory-event '(:type index-loaded))
        (disco-root--handle-directory-event
         '(:type guild-loaded :guild-id "g1"))
        (should (= 3 (length queued)))
        (should-not (fboundp 'disco-root--ensure-guild-channel-permissions))))))

(ert-deftest disco-root-default-layout-remains-composite-tree ()
  (should (eq disco-root-default-layout 'tree))
  (with-temp-buffer
    (disco-root-mode)
    (should (eq disco-root--layout 'tree))))

(ert-deftest disco-root-unread-guild-channel-keeps-guild-icon-scope ()
  (with-temp-buffer
    (let ((channel '((id . "c1") (guild_id . "g1") (type . 0)))
          (guild '((id . "g1") (name . "Guild")))
          guild-icons)
      (cl-letf (((symbol-function 'disco-root--guild-by-id)
                 (lambda (_guild-id) guild))
                ((symbol-function 'disco-root--insert-guild-icon)
                 (lambda (candidate)
                   (push (alist-get 'id candidate) guild-icons)
                   (insert "G"))))
        (disco-root--insert-activity-icon channel 'unread)
        (should (equal '("g1") guild-icons))
        (erase-buffer)
        (disco-root--insert-activity-icon channel 'directory)
        (should (equal "#" (buffer-string)))
        (should (equal '("g1") guild-icons))))))

(ert-deftest disco-root-dm-icon-uses-shared-rounded-avatar-api ()
  (with-temp-buffer
    (let* ((self '((id . "self") (avatar . "self-hash")))
           (other '((id . "alice") (avatar . "alice-hash")))
           (channel `((id . "dm1") (type . 1)
                      (recipients . (,self ,other))))
           requested-user
           requested-size
           inserted-image)
      (cl-letf (((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "self"))
                ((symbol-function 'disco-avatar-rounded-image)
                 (lambda (user size)
                   (setq requested-user user
                         requested-size size)
                   'avatar-image))
                ((symbol-function 'disco-root--scaled-image) #'identity)
                ((symbol-function 'insert-image)
                 (lambda (image &rest _args)
                   (setq inserted-image image)
                   (insert "A"))))
        (disco-root--insert-activity-icon channel 'dm)
        (should (equal other requested-user))
        (should (= disco-root-guild-icon-size requested-size))
        (should (eq 'avatar-image inserted-image))
        (should (equal "A" (buffer-string)))))))

(ert-deftest disco-root-avatar-resource-maps-to-all-private-channel-ids ()
  (let* ((self '((id . "self") (avatar . "self-hash")))
         (alice '((id . "alice") (avatar . "hash-a")))
         (alice-other-avatar '((id . "alice") (avatar . "hash-b")))
         (channels
          `(((id . "dm1") (type . 1) (recipients . (,self ,alice)))
            ((id . "dm2") (type . 1) (recipients . (,alice)))
            ((id . "dm3") (type . 1)
             (recipients . (,alice-other-avatar)))
            ((id . "group") (type . 3) (recipients . (,alice)))))
         queued)
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "self"))
              ((symbol-function 'disco-state-private-channels)
               (lambda () channels))
              ((symbol-function 'disco-root--queue-live-update)
               (lambda (ids &rest _args) (setq queued ids))))
      (let ((resource (disco-avatar-resource-key alice)))
        (should (equal '("dm1" "dm2")
                       (disco-root--avatar-resource-channel-ids
                        (list resource))))
        (disco-root--handle-avatar-resources-updated (list resource))
        (should (equal '("dm1" "dm2") queued))))))

(ert-deftest disco-root-refresh-index-is-lazy-unless-prefix-is-given ()
  (with-temp-buffer
    (disco-root-mode)
    (let (index-refresh full-refresh)
      (cl-letf (((symbol-function 'disco-directory-refresh-index-async)
                 (lambda (&rest _args) (setq index-refresh t)))
                ((symbol-function 'disco-directory-refresh-all-async)
                 (lambda () (setq full-refresh t)))
                ((symbol-function 'message) #'ignore))
        (disco-root-refresh)
        (should index-refresh)
        (should-not full-refresh)
        (setq index-refresh nil)
        (disco-root-refresh t)
        (should full-refresh)
        (should-not index-refresh)))))

(ert-deftest disco-root-layout-specs-merge-custom-layout-overrides ()
  (let ((disco-root-custom-layouts
         '((activity :label "Recent" :update-mode full)
           (custom-demo :label "Custom Demo" :update-mode incremental))))
    (should (equal "Recent" (disco-root-layout-label 'activity)))
    (should (eq 'full (disco-root-layout-update-mode 'activity)))
    (should (equal "Custom Demo" (disco-root-layout-label 'custom-demo)))))

(ert-deftest disco-root-layout-activity-default-update-mode-is-incremental ()
  (let ((disco-root-custom-layouts nil))
    (should (eq 'incremental (disco-root-layout-update-mode 'activity)))))

(ert-deftest disco-root-layout-render-uses-builder-view-spec ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root-custom-layouts
           '((demo
              :label "Demo"
              :build disco-root-test--build-demo))))
      (cl-letf (((symbol-function 'disco-root-test--build-demo)
                 (lambda ()
                   (disco-root-layout-list-spec-view-spec-create
                    (appkit-view-list-spec-create
                     :title "Builder Demo"
                     :empty-text "(empty)")))))
        (should (disco-root-layout-render 'demo))
        (should (string-match-p "Builder Demo" (buffer-string)))))))

(ert-deftest disco-root-layout-list-spec-view-spec-create-wraps-list-spec ()
  (let* ((list-spec (appkit-view-list-spec-create :title "List" :empty-text "(empty)"))
         (view-spec (disco-root-layout-list-spec-view-spec-create list-spec)))
    (should (disco-root-layout-view-spec-p view-spec))
    (should (eq 'list-spec (disco-root-layout-view-spec-kind view-spec)))
    (should (eq list-spec (disco-root-layout-view-spec-list-spec view-spec)))))

(ert-deftest disco-root-layout-ewoc-entry-view-spec-create-defaults-to-root-hooks ()
  (let* ((entries (list (disco-root-layout-entry-create :type 'text :text "hello")))
         (view-spec (disco-root-layout-ewoc-entry-view-spec-create entries)))
    (should (disco-root-layout-view-spec-p view-spec))
    (should (eq 'entries (disco-root-layout-view-spec-kind view-spec)))
    (should (eq 'disco-root--prepare-ewoc-state
                (disco-root-layout-view-spec-before-render view-spec)))
    (should (eq 'disco-root--ewoc-insert-entry
                (disco-root-layout-view-spec-entry-inserter view-spec)))
    (should (equal entries (disco-root-layout-view-spec-entries view-spec)))))

(ert-deftest disco-root-build-activity-layout-view-spec-returns-ewoc-entry-view-spec ()
  (with-temp-buffer
    (disco-root-mode)
    (cl-letf (((symbol-function 'disco-root--collect-activity-channels)
               (lambda () '(((id . "c1") (type . 0) (name . "general"))))))
      (let* ((view-spec (disco-root--build-activity-layout-view-spec))
             (entries (disco-root-layout-view-spec-entries view-spec))
             (first-entry (car entries)))
        (should (disco-root-layout-view-spec-p view-spec))
        (should (eq 'entries (disco-root-layout-view-spec-kind view-spec)))
        (should (eq 'disco-root--ewoc-insert-entry
                    (disco-root-layout-view-spec-entry-inserter view-spec)))
        (should (eq 'channel (disco-root-layout-entry-type first-entry)))
        (should (equal "c1"
                       (alist-get 'id (disco-root-layout-entry-channel first-entry))))))))

(ert-deftest disco-root-build-tree-layout-view-spec-returns-directory-view-spec ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--view-mode 'all))
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 (lambda () '(((id . "u1") (type . 0) (name . "updates")))))
                ((symbol-function 'disco-root--tree-unread-section-channels)
                 (lambda (channels) channels))
                ((symbol-function 'disco-root--visible-private-channels)
                 (lambda () nil))
                ((symbol-function 'disco-state-guilds)
                 (lambda () nil)))
        (let* ((view-spec (disco-root--build-tree-layout-view-spec))
               (entries (disco-root-layout-view-spec-entries view-spec))
               (first-entry (car entries))
               (channel-entry
                (seq-find
                 (lambda (entry)
                   (equal '(root unread channel "u1")
                          (appkit-directory-entry-key entry)))
                 entries)))
          (should (disco-root-layout-view-spec-p view-spec))
          (should (eq 'directory
                      (disco-root-layout-view-spec-kind view-spec)))
          (should (eq (appkit-directory-current-surface)
                      (disco-root-layout-view-spec-directory-surface view-spec)))
          (should (eq 'section (appkit-directory-entry-role first-entry)))
          (should (equal '(root section unread)
                         (appkit-directory-entry-key first-entry)))
          (should channel-entry)
          (should (eq 'item (appkit-directory-entry-role channel-entry)))
          (should-not (appkit-directory-entry-group-key channel-entry)))))))

(ert-deftest disco-root-layout-render-view-spec-renders-ewoc-entries ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((view-spec
           (disco-root-layout-ewoc-entry-view-spec-create
            (list (disco-root-layout-entry-create :type 'text :text "hello")))))
      (disco-root-layout-render-view-spec view-spec)
      (should (string-match-p "hello" (buffer-string))))))

(ert-deftest disco-root-archived-threads-list-spec-uses-layout-entry-inserter ()
  (with-temp-buffer
    (disco-root-archived-threads-mode)
    (let ((disco-root--archived-parent-channel '((id . "p1") (type . 15) (name . "Forum")))
          (disco-root--archived-threads-cache '(((id . "t1") (type . 11) (name . "Thread"))))
          (disco-root--archived-last-errors nil))
      (cl-letf (((symbol-function 'disco-root--channel-label)
                 (lambda (&rest _args) "Forum"))
                ((symbol-function 'disco-root--archived-source-status-string)
                 (lambda () "public:1"))
                ((symbol-function 'disco-root--archived-any-source-has-more-p)
                 (lambda () t)))
        (let* ((spec (disco-root--archived-threads-list-spec))
               (entries (appkit-view-list-spec-items spec))
               (first-entry (car entries)))
          (should (eq 'disco-root--insert-layout-entry
                      (appkit-view-list-spec-item-inserter spec)))
          (should (eq 'channel (disco-root-layout-entry-type first-entry)))
          (should (eq 'archived-thread (disco-root-layout-entry-scope first-entry))))))))

(ert-deftest disco-root-archived-thread-view-is-stable-and-refreshes-once ()
  (let ((disco-runtime--app nil)
        (parent '((id . "forum") (name . "Forum") (type . 15)))
        (buffers-before (buffer-list))
        (refreshes 0)
        (attachments 0))
    (unwind-protect
        (cl-letf (((symbol-function 'disco-state-channel)
                   (lambda (channel-id)
                     (and (equal channel-id "forum") parent)))
                  ((symbol-function 'disco-root-archived-threads-refresh)
                   (lambda () (cl-incf refreshes)))
                  ((symbol-function 'disco-root-view--attach-live-updates)
                   (lambda () (cl-incf attachments))))
          (let* ((first-buffer (disco-root-list-archived-threads "forum"))
                 (first-view (with-current-buffer first-buffer
                               (appkit-current-view)))
                 (second-buffer (disco-root-list-archived-threads "forum"))
                 (second-view (with-current-buffer second-buffer
                                (appkit-current-view))))
            (should (eq first-buffer second-buffer))
            (should (eq first-view second-view))
            (should (= 1 refreshes))
            (should (= 1 attachments))))
      (disco-runtime-stop)
      (dolist (buffer (buffer-list))
        (when (and (not (memq buffer buffers-before))
                   (buffer-live-p buffer))
          (kill-buffer buffer))))))

(ert-deftest disco-root-mode-disables-undo-history ()
  (with-temp-buffer
    (disco-root-mode)
    (should (eq buffer-undo-list t))
    (should-not switch-to-buffer-preserve-window-point)
    (should (eq 'tree disco-root--layout))))

(ert-deftest disco-root-open-attaches-view-before-initial-sync ()
  (let ((disco-root-buffer-name " *disco-root-open-test*")
        order
        displayed)
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (push 'pop order)
                 (setq displayed buffer)))
              ((symbol-function 'disco-root--attach-live-updates)
               (lambda () (push 'attach order)))
              ((symbol-function 'disco-root-render)
               (lambda () (push 'render order))))
      (unwind-protect
          (let ((result (disco-root-open)))
            (should (eq displayed result))
            (should (equal '(pop attach render) (nreverse order)))
            (with-current-buffer result
              (let ((view (appkit-current-view)))
                (should (appkit-view-live-p view))
                (should (equal '(root main) (appkit-view-id view)))
                (should (eq #'disco-root--sync-invalidations
                            (appkit-view-sync-function view))))))
        (when (buffer-live-p displayed)
          (kill-buffer displayed))))))

(ert-deftest disco-root-open-reuses-live-view-without-full-resync ()
  (let ((disco-runtime--app nil)
        (disco-root-buffer-name " *disco-root-reuse-test*")
        (sync-count 0)
        first
        second)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'disco-root--attach-live-updates) #'ignore)
              ((symbol-function 'disco-root--render-preserving-position)
               (lambda () (cl-incf sync-count))))
      (unwind-protect
          (progn
            (setq first (disco-root-open))
            (setq second (disco-root-open))
            (should (eq first second))
            (should (= 1 sync-count)))
        (when (buffer-live-p first)
          (kill-buffer first))
        (disco-runtime-stop)))))

(ert-deftest disco-root-fresh-view-preserves-presentation-and-resets-session-state ()
  (let ((disco-runtime--app nil)
        (disco-root-buffer-name " *disco-root-session-reuse-test*")
        (sync-count 0)
        first
        second
        first-view)
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'disco-root--attach-live-updates) #'ignore)
              ((symbol-function 'disco-root--render-preserving-position)
               (lambda () (cl-incf sync-count))))
      (unwind-protect
          (progn
            (setq first (disco-root-open))
            (with-current-buffer first
              (setq first-view (appkit-current-view))
              (setq-local disco-root--layout 'tree)
              (setq-local disco-root--sort-mode 'name)
              (setq-local disco-root--view-mode 'dms)
              (puthash '(root section unread) nil
                       disco-root--tree-fold-state)
              (puthash '(root section dm) t
                       disco-root--tree-fold-state)
              (puthash '(root section guilds) t
                       disco-root--tree-fold-state)
              (puthash '(root guild "g1") t
                       disco-root--tree-fold-state)
              (setq-local disco-root--search-query "old")
              (setq-local disco-root--search-tabs '((messages :loading t))))
            (disco-runtime-stop)
            (should-not (appkit-view-live-p first-view))
            (setq second (disco-root-open))
            (should (eq first second))
            (with-current-buffer second
              (should-not (eq first-view (appkit-current-view)))
              (should (eq 'tree disco-root--layout))
              (should (eq 'name disco-root--sort-mode))
              (should (eq 'dms disco-root--view-mode))
              (should-not
               (gethash '(root section unread)
                        disco-root--tree-fold-state))
              (should
               (gethash '(root section dm) disco-root--tree-fold-state))
              (should
               (gethash '(root section guilds)
                        disco-root--tree-fold-state))
              (should
               (gethash '(root guild "g1") disco-root--tree-fold-state))
              (should-not disco-root--search-query)
              (should-not disco-root--search-tabs))
            (should (= 2 sync-count)))
        (when (buffer-live-p first)
          (kill-buffer first))
        (disco-runtime-stop)))))

(ert-deftest disco-root-fresh-view-leaves-session-bound-search-layout ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--layout 'search)
    (setq-local disco-root--search-prev-layout 'activity)
    (setq-local disco-root--search-query "old")
    (disco-root--reset-session-controller-state)
    (should (eq 'activity disco-root--layout))
    (should-not disco-root--search-query)
    (should-not disco-root--search-prev-layout)))

(ert-deftest disco-root-open-at-point-jumps-to-search-message ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--layout 'search)
    (let ((inhibit-read-only t)
          (disco-root--fill-column 80))
      (disco-root--insert-search-message-line
       '((id . "m1") (channel_id . "c1") (content . "hit"))
       2 'messages))
    (goto-char (point-min))
    (let (jumped)
      (cl-letf (((symbol-function 'disco-room-jump-to-message)
                 (lambda (message-id channel-id)
                   (setq jumped (list message-id channel-id)))))
        (disco-root-open-at-point)
        (should (equal '("m1" "c1") jumped))))))

(ert-deftest disco-root-search-result-button-dispatches-exact-primary-click ()
  (save-window-excursion
    (with-temp-buffer
      (disco-root-mode)
      (setq-local disco-root--layout 'search)
      (let* ((inhibit-read-only t)
             (disco-root--fill-column 80)
             (first '((id . "m1") (channel_id . "c1") (content . "first")))
             (second '((id . "900719925474099312345")
                       (channel_id . "c2")
                       (content . "second")))
             first-button
             second-button
             first-newline
             blank-position
             jumped)
        (disco-root--insert-search-message-line first 2 'messages)
        (setq first-button (button-at (point-min))
              first-newline (1- (point)))
        (let ((start (point)))
          (disco-root--insert-search-message-line second 2 'messages)
          (setq second-button (button-at start)))
        (setq blank-position (point))
        (insert "\n")
        (should (eq (button-type first-button) 'appkit-ui-action-row-button))
        (should (eq (button-get first-button 'appkit-ui-action-row-object) first))
        (should (eq (button-get second-button 'appkit-ui-action-row-object) second))
        (should-not (button-at first-newline))
        (should-not (button-at blank-position))
        (switch-to-buffer (current-buffer))
        (goto-char first-button)
        (cl-letf (((symbol-function 'disco-room-jump-to-message)
                   (lambda (message-id channel-id)
                     (push (list message-id channel-id) jumped))))
          (let ((mouse-1-click-follows-link 450))
            (execute-kbd-macro
             (disco-root-test--primary-click
              (selected-window) (marker-position second-button)))
            (should (= (point) (marker-position first-button)))
            (execute-kbd-macro
             (disco-root-test--primary-click
              (selected-window) first-newline))
            (execute-kbd-macro
             (disco-root-test--primary-click
              (selected-window) blank-position))))
        (should (equal '(("900719925474099312345" "c2")) jumped))))))

(ert-deftest disco-root-search-show-more-dispatches-exact-entry-tab ()
  (save-window-excursion
    (with-temp-buffer
      (disco-root-mode)
      (setq-local disco-root--layout 'search)
      (let* ((inhibit-read-only t)
             (first (disco-root--entry-search-action
                     "Show more messages" 'load-more 'messages))
             (second (disco-root--entry-search-action
                      "Show more files" 'load-more 'files))
             first-button
             second-button
             loaded-tabs)
        (disco-root--insert-layout-entry first)
        (setq first-button (button-at (point-min)))
        (let ((start (point)))
          (disco-root--insert-layout-entry second)
          (setq second-button (button-at start)))
        (should (eq (button-get first-button 'appkit-ui-action-row-object) first))
        (should (eq (button-get second-button 'appkit-ui-action-row-object) second))
        (switch-to-buffer (current-buffer))
        (goto-char first-button)
        (let ((disco-root-view-load-more-function
               (lambda (tab) (push tab loaded-tabs)))
              (mouse-1-click-follows-link 450))
          (execute-kbd-macro
           (disco-root-test--primary-click
            (selected-window) (marker-position second-button))))
        (should (equal '(files) loaded-tabs))))))

(ert-deftest disco-root-search-open-does-not-fall-forward-from-blank-row ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--layout 'search)
    (let ((inhibit-read-only t)
          (disco-root--fill-column 80)
          jumped)
      (insert "\n")
      (disco-root--insert-search-message-line
       '((id . "m1") (channel_id . "c1") (content . "hit"))
       2 'messages)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'disco-room-jump-to-message)
                 (lambda (&rest args) (setq jumped args))))
        (should-error (disco-root-open-at-point) :type 'user-error)
        (should-not jumped)))))

(ert-deftest disco-root-tree-open-on-blank-never-opens-next-item ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--layout 'tree)
    (let* ((surface (disco-root--ensure-tree-directory-surface))
           (channel '((id . "c1") (type . 1) (name . "DM")))
           opened)
      (appkit-directory-configure
       surface
       :item-inserter (lambda (_surface _entry) (insert "channel\n")))
      (appkit-directory-reconcile
       surface
       (list
        (appkit-directory-entry-create
         :key '(root spacer) :role 'spacer)
        (appkit-directory-entry-create
         :key '(root dm channel "c1")
         :role 'item
         :section-key '(root section dm)
         :item-p t
         :payload channel)))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'disco-root--open-channel)
                 (lambda (channel-id) (setq opened channel-id))))
        (disco-root-open-at-point)
        (should-not opened)
        (should (equal '(root dm channel "c1")
                       (appkit-directory-key-at-point)))))))

(ert-deftest disco-root-search-parse-query-supports-discord-style-filters ()
  (with-temp-buffer
    (disco-root-mode)
    (cl-letf (((symbol-function 'disco-root--search-user-candidates)
               (lambda (_domain)
                 '(("alice" . "u1")
                   ("bob" . "u2"))))
              ((symbol-function 'disco-root--search-channel-candidates)
               (lambda (_domain)
                 '(("general" . "c1")))))
      (let ((parsed (disco-root--search-parse-query
                     "hello world from:alice author-type:user,bot mentions:bob has:link,file in:general pinned:true sort:relevance order:asc slop:3 before:123 after:456"
                     '(:kind guild :id "g1" :label "Guild"))))
        (should (equal "hello world" (plist-get parsed :content)))
        (should (equal '("u1") (plist-get parsed :author-ids)))
        (should (equal '("user" "bot") (plist-get parsed :author-types)))
        (should (equal '("u2") (plist-get parsed :mentions)))
        (should (equal '("link" "file") (plist-get parsed :has)))
        (should (equal '("c1") (plist-get parsed :channel-ids)))
        (should (eq t (plist-get parsed :pinned)))
        (should (= 3 (plist-get parsed :slop)))
        (should (eq 'relevance (plist-get parsed :sort-by)))
        (should (eq 'asc (plist-get parsed :sort-order)))
        (should (equal "123" (plist-get parsed :max-id)))
        (should (equal "456" (plist-get parsed :min-id)))))))

(ert-deftest disco-root-search-current-domain-at-point-prefers-channel ()
  (disco-state-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-root-mode)
        (disco-state-upsert-channel '((id . "c1")
                                      (guild_id . "g1")
                                      (type . 0)
                                      (name . "general")))
        (let ((inhibit-read-only t))
          (insert "hit\n")
          (add-text-properties (point-min) (point-max)
                               '(disco-channel-id "c1")))
        (goto-char (point-min))
        (let ((domain (disco-root--search-current-domain-at-point)))
          (should (eq 'channel (plist-get domain :kind)))
          (should (equal "c1" (plist-get domain :id)))
          (should (equal "g1" (plist-get domain :guild-id)))))
    (disco-state-reset)))

(ert-deftest disco-root-search-parse-query-rejects-in-filter-for-channel-domain ()
  (with-temp-buffer
    (disco-root-mode)
    (should-error
     (disco-root--search-parse-query
      "in:general"
      '(:kind channel :id "c1" :guild-id "g1" :label "general"))
     :type 'error)))

(ert-deftest disco-root-search-transient-boundary-use-org-read-date ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--search-query-spec nil)
    (let (prompts)
      (cl-letf (((symbol-function 'org-read-date)
                 (lambda (&optional _with-time to-time _from-string prompt &rest _args)
                   (push prompt prompts)
                   (should to-time)
                   (pcase prompt
                     ("Before (message id or time): " (encode-time 0 0 0 7 3 2026))
                     ("After (message id or time): " (encode-time 0 0 0 8 3 2026)))))
                ((symbol-function 'disco-root--search-transient-buffer)
                 (lambda () (current-buffer))))
        (let ((before (disco-root--search-transient-before-value "Before (message id or time): " nil nil))
              (after (disco-root--search-transient-after-value "After (message id or time): " nil nil)))
          (should (stringp before))
          (should (stringp after))
          (should (equal '("After (message id or time): "
                           "Before (message id or time): ")
                         prompts)))))))

(ert-deftest disco-root-search-query-capf-completes-filter-values ()
  (with-temp-buffer
    (insert "has:vi")
    (goto-char (point-max))
    (setq-local disco-root--search-completion-domain
                '(:kind guild :id "g1" :label "Guild"))
    (cl-letf (((symbol-function 'minibuffer-prompt-end)
               (lambda () (point-min))))
      (pcase-let ((`(,start ,end ,table . ,_props)
                   (disco-root--search-query-complete-at-point)))
        (should (= 5 start))
        (should (= (point-max) end))
        (should (member "video"
                        (all-completions
                         (buffer-substring-no-properties start end)
                         table)))))))

(ert-deftest disco-root-search-user-candidates-include-guild-presences ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-apply-presence-update
         '((guild_id . "g1")
           (user (id . "u1")
                 (username . "alice")
                 (global_name . "Alice"))))
        (let ((candidates (disco-root--search-user-candidates
                           '(:kind guild :id "g1" :label "Guild"))))
          (should (equal "u1"
                         (cdr (assoc "alice" candidates))))
          (should (equal "u1"
                         (cdr (assoc "Alice" candidates))))))
    (disco-state-reset)))

(ert-deftest disco-root-search-user-candidates-include-guild-members ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-apply-guild-members-chunk
         "g1"
         '(((nick . "Ali")
            (user (id . "u1")
                  (username . "alice")
                  (global_name . "Alice")))))
        (let ((candidates (disco-root--search-user-candidates
                           '(:kind guild :id "g1" :label "Guild"))))
          (should (equal "u1" (cdr (assoc "Ali" candidates))))
          (should (equal "u1" (cdr (assoc "alice" candidates))))))
    (disco-state-reset)))

(ert-deftest disco-root-search-member-completion-requests-guild-members ()
  (let (requested)
    (with-temp-buffer
      (setq-local disco-root--search-completion-requested-prefixes nil)
      (cl-letf (((symbol-function 'disco-gateway-running-p)
                 (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (guild-id &rest args)
                   (setq requested (cons guild-id args)))))
        (disco-root--search-maybe-request-member-completion
         "from"
         "ali"
         '(:kind guild :id "g1" :label "Guild"))
        (should (equal '("g1" :query "ali" :limit 50) requested))))))

(ert-deftest disco-root-search-member-completion-requests-guild-members-for-channel-domain ()
  (let (requested)
    (with-temp-buffer
      (setq-local disco-root--search-completion-requested-prefixes nil)
      (cl-letf (((symbol-function 'disco-gateway-running-p)
                 (lambda () t))
                ((symbol-function 'disco-gateway-request-guild-members)
                 (lambda (guild-id &rest args)
                   (setq requested (cons guild-id args)))))
        (disco-root--search-maybe-request-member-completion
         "mentions"
         "ali"
         '(:kind channel :id "c1" :guild-id "g1" :label "general"))
        (should (equal '("g1" :query "ali" :limit 50) requested))))))

(ert-deftest disco-root-search-format-user-and-channel-ids-show_labels ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-apply-guild-members-chunk
         "g1"
         '(((nick . "Ali")
            (user (id . "u1")
                  (username . "alice")))))
        (disco-state-upsert-channel '((id . "c1") (guild_id . "g1") (type . 0) (name . "general")))
        (with-temp-buffer
          (disco-root-mode)
          (setq-local disco-root--search-domain '(:kind guild :id "g1" :label "Guild"))
          (should (equal "Ali"
                         (disco-root--search-format-user-ids '("u1") disco-root--search-domain)))
          (should (equal "general"
                         (disco-root--search-format-channel-ids '("c1"))))))
    (disco-state-reset)))

(ert-deftest disco-root-member-chunk-callback-never-presents-completion-ui ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'disco-root--live-event-p)
               (lambda (_event-type) nil))
              ((symbol-function 'minibuffer-completion-help)
               (lambda ()
                 (ert-fail "gateway callback refreshed minibuffer UI")))
              ((symbol-function 'appkit-chat-completion-complete)
               (lambda (&rest _args)
                 (ert-fail "gateway callback reopened completion UI"))))
      (disco-root--handle-gateway-event
       '(:type guild-members-chunk :guild-id "g1" :members nil)))))

(ert-deftest disco-root-search-transient-format-channel-ids-shows-fixed-by-domain ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--search-domain '(:kind channel :id "c1" :guild-id "g1" :label "general"))
    (should (equal "fixed by domain"
                   (disco-root--search-transient-format-channel-ids nil)))))

(ert-deftest disco-root-search-layout-entries-preserve-section-metadata ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--search-tabs
                '((messages :items (((id . "m1")))
                   :loading nil
                   :error nil
                   :cursor nil
                   :total-results 1)))
    (let ((first-entry (car (disco-root--search-layout-entries))))
      (should (eq 'search-section (disco-root-layout-entry-type first-entry)))
      (should (equal "Messages" (disco-root-layout-entry-title first-entry)))
      (should (= 1 (disco-root-layout-entry-loaded-count first-entry)))
      (should (= 1 (disco-root-layout-entry-total-count first-entry)))
      (should-not (disco-root-layout-entry-loading first-entry)))))

(ert-deftest disco-root-layout-entry-anchors-distinguish-duplicate-domain-rows ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((channel '((id . "c1") (type . 1)))
          (inhibit-read-only t)
          keys)
      (cl-letf (((symbol-function 'disco-root--insert-channel-line)
                 (lambda (&rest _arguments) (insert "channel\n"))))
        (dolist (entry (list (disco-root--entry-channel channel 2 'unread)
                             (disco-root--entry-channel channel 2 'private)))
          (let ((start (point)))
            (disco-root--insert-layout-entry entry)
            (push (get-text-property start 'disco-root-entry-key) keys))))
      (should (equal '((channel unread "c1") (channel private "c1"))
                     (nreverse keys)))
      (should-not
       (equal (disco-root-layout-entry-key
               (disco-root--entry-search-message '((id . "m1")) 2 'messages))
              (disco-root-layout-entry-key
               (disco-root--entry-search-message '((id . "m1")) 2 'links)))))))

(ert-deftest disco-root-build-search-layout-view-spec-renders-sections ()
  (with-temp-buffer
    (disco-root-mode)
    (setq-local disco-root--layout 'search)
    (setq-local disco-root--search-domain '(:kind dms :id nil :label "DMs"))
    (setq-local disco-root--search-query-spec '(:content "foo" :sort-by timestamp :sort-order desc))
    (setq-local disco-root--search-tabs
                '((messages :items (((id . "m1")
                                     (channel_id . "c1")
                                     (content . "hello")))
                   :loading nil
                   :error nil
                   :cursor ((type . "timestamp")
                            (timestamp . "1"))
                   :total-results 1)
                  (links :items nil
                         :loading nil
                         :error nil
                         :cursor nil
                         :total-results 0)
                  (media :items nil :loading t :error nil :cursor nil :total-results nil)
                  (files :items nil :loading nil :error "boom" :cursor nil :total-results nil)
                  (pins :items nil :loading nil :error nil :cursor nil :total-results 0)))
    (setq-local disco-root--search-channel-table (make-hash-table :test #'equal))
    (puthash "c1" '((id . "c1") (type . 1) (name . "dm"))
             disco-root--search-channel-table)
    (let ((disco-root--fill-column 80))
      (cl-letf (((symbol-function 'disco-root--insert-search-message-line)
                 (lambda (_message _indent _tab)
                   (insert "  result-row\n"))))
        (let ((view-spec (disco-root--build-search-layout-view-spec)))
          (should (disco-root-layout-view-spec-p view-spec))
          (should (eq 'list-spec (disco-root-layout-view-spec-kind view-spec)))
          (disco-root-layout-render-view-spec view-spec))
        (should (string-match-p "Search results in DMs" (buffer-string)))
        (should (string-match-p "Messages (1/1)" (buffer-string)))
        (should (string-match-p "Show more" (buffer-string)))
        (should (string-match-p "(loading...)" (buffer-string)))
        (should (string-match-p "(boom)" (buffer-string)))))))

(ert-deftest disco-root-search-dispatch-channel-domain-uses-guild-tabs-for-guild-channel ()
  (with-temp-buffer
    (disco-root-mode)
    (let (captured)
      (setq-local disco-root--search-domain '(:kind channel :id "c1" :guild-id "g1" :label "general"))
      (setq-local disco-root--search-query-spec '(:content "foo" :sort-by timestamp :sort-order desc))
      (disco-root--search-reset-tab-states)
      (cl-letf (((symbol-function 'disco-root--search-render-if-visible)
                 (lambda () nil))
                ((symbol-function 'disco-api-guild-search-messages-tabs-async)
                 (lambda (guild-id &rest args)
                   (setq captured (cons guild-id args))))
                ((symbol-function 'disco-api-channel-search-messages-tabs-async)
                 (lambda (&rest _args)
                   (ert-fail "channel endpoint should not be used for guild channel"))))
        (disco-root--search-dispatch 1 (disco-root--search-request-tabs nil))
        (should (equal "g1" (car captured)))
        (should (equal '("c1") (plist-get (cdr captured) :channel-ids)))))))

(ert-deftest disco-root-search-result-projection-is-scheduled-not-rendered ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'search)
          queued)
      (cl-letf (((symbol-function 'disco-root--queue-live-update)
                 (lambda (&rest arguments) (setq queued arguments)))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda () (ert-fail "search callback rendered directly"))))
        (disco-root--search-render-if-visible)
        (should (equal '(nil t nil) queued))))))

(ert-deftest disco-root-search-response-cannot-land-in-replacement-view ()
  (let ((disco-runtime--app nil)
        error-callback)
    (unwind-protect
        (with-temp-buffer
          (disco-root-mode)
          (setq-local disco-root--layout 'search)
          (setq-local disco-root--search-domain
                      '(:kind dms :id nil :label "DMs"))
          (setq-local disco-root--search-query-spec
                      '(:content "old" :sort-by timestamp :sort-order desc))
          (setq-local disco-root--search-generation 1)
          (disco-root--search-reset-tab-states)
          (let ((old-view (disco-root--ensure-view)))
            (cl-letf (((symbol-function 'disco-root--search-render-if-visible)
                       #'ignore)
                      ((symbol-function 'disco-api-user-search-messages-tabs-async)
                       (lambda (&rest arguments)
                         (setq error-callback
                               (plist-get arguments :on-error)))))
              (disco-root--search-dispatch
               1 (disco-root--search-request-tabs nil)))
            (should (functionp error-callback))
            (appkit-kill-view old-view)
            (let ((replacement (disco-root--ensure-view))
                  (replacement-tabs
                   '((messages :items (((id . "new")))
                               :loading nil :error "new state" :cursor nil))))
              (should-not (eq old-view replacement))
              (setq-local disco-root--search-generation 1)
              (setq-local disco-root--search-tabs replacement-tabs)
              (funcall error-callback '(:message "old response"))
              (should (equal replacement-tabs disco-root--search-tabs)))))
      (disco-runtime-stop))))

(ert-deftest disco-root-search-dispatch-channel-domain-uses-channel-tabs-for-private-channel ()
  (with-temp-buffer
    (disco-root-mode)
    (let (captured)
      (setq-local disco-root--search-domain '(:kind channel :id "c1" :guild-id nil :label "dm"))
      (setq-local disco-root--search-query-spec '(:content "foo" :sort-by timestamp :sort-order desc))
      (setq-local disco-root--search-channel-table (make-hash-table :test #'equal))
      (puthash "c1" '((id . "c1") (type . 1) (name . "dm")) disco-root--search-channel-table)
      (disco-root--search-reset-tab-states)
      (cl-letf (((symbol-function 'disco-root--search-render-if-visible)
                 (lambda () nil))
                ((symbol-function 'disco-api-channel-search-messages-tabs-async)
                 (lambda (channel-id &rest args)
                   (setq captured (cons channel-id args))))
                ((symbol-function 'disco-api-guild-search-messages-tabs-async)
                 (lambda (&rest _args)
                   (ert-fail "guild endpoint should not be used for private channel"))))
        (disco-root--search-dispatch 1 (disco-root--search-request-tabs nil))
        (should (equal "c1" (car captured)))))))

(ert-deftest disco-root-search-dispatch-channel-domain-auto-includes-age-restricted-thread ()
  (with-temp-buffer
    (disco-root-mode)
    (disco-state-reset)
    (unwind-protect
        (let (captured)
          (disco-state-upsert-channel
           '((id . "parent")
             (type . 0)
             (guild_id . "g1")
             (name . "adult")
             (nsfw . t)))
          (disco-state-upsert-channel
           '((id . "thread")
             (type . 11)
             (guild_id . "g1")
             (name . "topic")
             (parent_id . "parent")))
          (setq-local disco-root--search-domain '(:kind channel :id "thread" :guild-id "g1" :label "topic"))
          (setq-local disco-root--search-query-spec '(:content "foo" :sort-by timestamp :sort-order desc))
          (disco-root--search-reset-tab-states)
          (cl-letf (((symbol-function 'disco-root--search-render-if-visible)
                     (lambda () nil))
                    ((symbol-function 'disco-api-guild-search-messages-tabs-async)
                     (lambda (guild-id &rest args)
                       (setq captured (cons guild-id args))))
                    ((symbol-function 'disco-api-channel-search-messages-tabs-async)
                     (lambda (&rest _args)
                       (ert-fail "channel endpoint should not be used for guild thread"))))
            (disco-root--search-dispatch 1 (disco-root--search-request-tabs nil))
            (should (equal "g1" (car captured)))
            (should (equal '("thread") (plist-get (cdr captured) :channel-ids)))
            (should (eq t (plist-get (cdr captured) :include-nsfw)))))
      (disco-state-reset))))

(ert-deftest disco-root-toggle-section-at-point-requires-section-row ()
  (with-temp-buffer
    (disco-root-mode)
    (should-error (disco-root-toggle-section-at-point)
                  :type 'user-error)))

(ert-deftest disco-root-activity-primary-label-uses-guild-category-channel-order ()
  (disco-state-reset)
  (let* ((guild-id "g1")
         (category '((id . "cat1")
                     (guild_id . "g1")
                     (type . 4)
                     (name . "General")))
         (channel '((id . "chan1")
                    (guild_id . "g1")
                    (type . 0)
                    (name . "emacs")
                    (parent_id . "cat1"))))
    (unwind-protect
        (progn
          (disco-state-set-guilds (list `((id . ,guild-id)
                                          (name . "Emacs CN"))))
          (disco-state-put-channels guild-id (list category channel))
          (let* ((label (disco-root--activity-primary-label channel))
                 (separator-position (string-match-p "" label)))
            (should (equal "Emacs CN  General  emacs"
                           (substring-no-properties label)))
            (should separator-position)
            (should (eq 'disco-root-context-separator
                        (get-text-property separator-position 'face label)))))
      (disco-state-reset))))

(ert-deftest disco-root-thread-browser-context-label-includes-applied-tags ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-channel
         '((id . "forum1")
           (guild_id . "g1")
           (type . 15)
           (name . "Forum")
           (available_tags . (((id . "tag1") (name . "bug"))
                              ((id . "tag2") (emoji_name . "🔥") (name . "hot"))))))
        (should (equal "Thread title | bug | 🔥 hot"
                       (disco-root--thread-browser-context-label
                        '((id . "th1")
                          (type . 11)
                          (parent_id . "forum1")
                          (name . "Thread title")
                          (applied_tags . ("tag1" "tag2")))))))
    (disco-state-reset)))

(ert-deftest disco-root-insert-channel-line-parent-thread-uses-activity-preview-layout ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 90)
          (inhibit-read-only t))
      (disco-state-reset)
      (unwind-protect
          (progn
            (disco-state-upsert-channel
             '((id . "forum1")
               (guild_id . "g1")
               (type . 15)
               (name . "Forum")
               (available_tags . (((id . "tag1") (name . "bug"))))))
            (disco-state-upsert-channel
             '((id . "th1")
               (guild_id . "g1")
               (type . 11)
               (parent_id . "forum1")
               (name . "Thread title")
               (applied_tags . ("tag1"))
               (last_message_id . "m1")))
            (disco-state-put-messages
             "th1"
             '(((id . "th1")
                (channel_id . "th1")
                (content . "hello world")
                (author . ((username . "alice"))))))
            (disco-root--insert-channel-line
             (disco-state-channel "th1") 2 'parent-thread)
            (should (string-match-p "\\[Thread title | bug *\\]" (buffer-string)))
            (should (string-match-p "alice> hello world" (buffer-string))))
        (disco-state-reset)))))

(ert-deftest disco-root-private-channel-display-name-prefers-non-self-recipient ()
  (cl-letf (((symbol-function 'disco-gateway-current-user-id)
             (lambda () "self")))
    (should (equal "Friend"
                   (disco-root--private-channel-display-name
                    '((type . 18)
                      (recipients . (((id . "self") (username . "me"))
                                     ((id . "u2") (global_name . "Friend"))))))))))

(ert-deftest disco-root-channel-visible-in-dms-includes-ephemeral-dm ()
  (should (disco-root--channel-visible-in-mode-p '((type . 18)) 'dms))
  (should-not (disco-root--channel-visible-in-mode-p '((type . 2)) 'dms)))

(ert-deftest disco-root-open-channel-opens-voice-timeline ()
  (disco-state-reset)
  (unwind-protect
      (let (opened)
        (disco-state-upsert-gateway-channel
         '((id . "voice1") (guild_id . "g1") (type . 2) (name . "Voice")))
        (cl-letf (((symbol-function 'disco-room-open)
                   (lambda (channel-id channel-name)
                     (setq opened (list channel-id channel-name))))
                  ((symbol-function
                    'disco-channel-directory-open-thread-parent)
                   (lambda (&rest _args)
                     (ert-fail "voice channels should not open directories"))))
          (disco-root--open-channel "voice1")
          (should (equal '("voice1" "Voice") opened))))
    (disco-state-reset)))

(ert-deftest disco-root-open-channel-opens-directory-inspect-buffer ()
  (disco-state-reset)
  (let (opened-buffer)
    (unwind-protect
        (progn
          (disco-state-upsert-gateway-channel
           '((id . "dir1")
             (type . 14)
             (guild_id . "g1")
             (name . "Directory")))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _args)
                       (setq opened-buffer buffer)
                       buffer))
                    ((symbol-function 'disco-room-open)
                     (lambda (&rest _args)
                       (ert-fail "directory channels should not open room timelines")))
                    ((symbol-function
                      'disco-channel-directory-open-thread-parent)
                     (lambda (&rest _args)
                       (ert-fail "directory channels should not open guild directories"))))
            (disco-root--open-channel "dir1")
            (should (buffer-live-p opened-buffer))
            (with-current-buffer opened-buffer
              (should (eq major-mode 'disco-root-channel-inspect-mode))
              (should (string-match-p "Directory channel browsing is not implemented yet"
                                      (buffer-string))))))
      (when (buffer-live-p opened-buffer)
        (kill-buffer opened-buffer))
      (disco-state-reset))))

(ert-deftest disco-root-open-channel-opens-forum-in-guild-directory ()
  (disco-state-reset)
  (unwind-protect
      (let (opened)
        (disco-state-upsert-gateway-channel
         '((id . "forum1") (guild_id . "g1") (type . 15) (name . "Ideas")))
        (cl-letf (((symbol-function
                    'disco-channel-directory-open-thread-parent)
                   (lambda (channel-id) (setq opened channel-id)))
                  ((symbol-function 'disco-room-open)
                   (lambda (&rest _args)
                     (ert-fail "forum channels cannot open message timelines"))))
          (disco-root--open-channel "forum1")
          (should (equal "forum1" opened))))
    (disco-state-reset)))

(ert-deftest disco-root-text-thread-count-keeps-canonical-state-owner ()
  (disco-state-reset)
  (disco-directory-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-gateway-channel
         '((id . "text") (guild_id . "g1") (type . 0)))
        (disco-state-upsert-gateway-channel
         '((id . "active") (guild_id . "g1") (parent_id . "text")
           (type . 11) (thread_metadata . ((archived . :false)))))
        (disco-state-upsert-gateway-channel
         '((id . "archived") (guild_id . "g1") (parent_id . "text")
           (type . 11) (thread_metadata . ((archived . t)))))
        (disco-state-upsert-gateway-channel
         `((id . "hidden") (guild_id . "g1") (parent_id . "text")
           (type . 11) (flags . ,disco-channel-flag-obfuscated)
           (thread_metadata . ((archived . :false)))))
        ;; Forum directory snapshots do not own ordinary text/news threads.
        (puthash "text"
                 '(:status loaded :thread-ids nil :total 0)
                 disco-directory--parent-thread-state)
        (should (= 1
                   (disco-root--thread-count-under-parent
                    (disco-state-channel "text")))))
    (disco-directory-reset)
    (disco-state-reset)))

(ert-deftest disco-root-forum-label-does-not-present-page-size-as-total ()
  (disco-state-reset)
  (disco-directory-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-gateway-channel
         '((id . "forum") (guild_id . "g1") (type . 15)
           (name . "support")))
        (disco-state-upsert-channel
         '((id . "loaded-post") (guild_id . "g1")
           (parent_id . "forum") (type . 11)))
        (puthash "forum"
                 '(:status loaded :thread-ids ("loaded-post")
                   :next-cursor "older" :total 545)
                 disco-directory--parent-thread-state)
        (let ((label
               (substring-no-properties
                (disco-root--channel-label
                 (disco-state-channel "forum") 'directory))))
          (should (string-match-p "support" label))
          (should-not (string-match-p "threads" label))))
    (disco-directory-reset)
    (disco-state-reset)))

(ert-deftest disco-root-search-channel-candidates-only-offer-viewable-searchable-channels ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-gateway-channel
         '((id . "text1") (guild_id . "g1") (type . 0)
           (name . "chat") (flags . 0)))
        (disco-state-upsert-gateway-channel
         `((id . "hidden1") (guild_id . "g1") (type . 0)
           (name . "secret") (flags . ,disco-channel-flag-obfuscated)))
        (disco-state-upsert-gateway-channel
         '((id . "dir1") (guild_id . "g1") (type . 14)
           (name . "Directory") (flags . 0)))
        (let ((candidates (disco-root--search-channel-candidates '(:kind guild :id "g1"))))
          (should (member '("chat" . "text1") candidates))
          (should-not (member '("secret" . "hidden1") candidates))
          (should-not (member '("Directory" . "dir1") candidates))))
    (disco-state-reset)))

(ert-deftest disco-root-search-domain-candidates-hide-obfuscated-current-channel ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-gateway-channel
         '((id . "visible") (guild_id . "g1") (type . 0)
           (name . "general") (flags . 0)))
        (disco-state-upsert-gateway-channel
         `((id . "obfuscated") (guild_id . "g1") (type . 0)
           (name . "secret") (flags . ,disco-channel-flag-obfuscated)))
        (cl-letf (((symbol-function 'disco-root--search-current-channel-domain)
                   (lambda () '(:kind channel :id "visible" :guild-id "g1"
                                         :label "general"))))
          (should
           (assoc "Channel: general" (disco-root--search-domain-candidates))))
        (cl-letf (((symbol-function 'disco-root--search-current-channel-domain)
                   (lambda () '(:kind channel :id "obfuscated" :guild-id "g1"
                                         :label "secret"))))
          (should-not
           (assoc "Channel: secret" (disco-root--search-domain-candidates)))))
    (disco-state-reset)))

(ert-deftest disco-root-activity-secondary-label-uses-directory-placeholder ()
  (let ((channel '((id . "dir1") (type . 14) (name . "Directory")
                   (last_message_id . "42"))))
    (should (equal "(directory view)"
                   (disco-root--activity-secondary-label channel)))))

(ert-deftest disco-root-activity-secondary-label-keeps-missing-preview-blank ()
  (disco-state-reset)
  (let ((channel '((id . "c1")
                   (type . 0)
                   (last_message_id . "42")
                   (last_pin_timestamp . "2026-03-05T01:00:00.000000+00:00"))))
    (unwind-protect
        (progn
          (disco-state-upsert-channel channel)
          (let ((label (disco-root--activity-secondary-label channel)))
            (should (equal "" label))
            (should-not (string-match-p "pins" label))
            (should-not (string-match-p "unread" label))))
      (disco-state-reset))))

(ert-deftest disco-root-activity-secondary-label-prefers-conversation-summary ()
  (disco-state-reset)
  (let ((channel '((id . "c2")
                   (type . 0)
                   (last_message_id . "43"))))
    (unwind-protect
        (progn
          (disco-state-upsert-channel channel)
          (disco-state-apply-conversation-summary-update
           "c2"
           '(((id . "99")
              (summ_short . "summary-preview"))))
          (should (equal "summary-preview"
                         (disco-root--activity-secondary-label channel))))
      (disco-state-reset))))

(ert-deftest disco-root-activity-preview-line-queues-preview-fetch ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((channel '((id . "c3")
                     (guild_id . "g1")
                     (type . 0)
                     (last_message_id . "44")))
          queued)
      (cl-letf (((symbol-function 'disco-preview-request-channel)
                 (lambda (_channel)
                   (setq queued t))))
        (should (equal ""
                       (disco-root--activity-preview-line channel nil 'activity)))
        (should queued)))))

(ert-deftest disco-root-preview-update-queues-channel-row-refresh ()
  (with-temp-buffer
    (disco-root-mode)
    (let (queued)
      (cl-letf (((symbol-function 'disco-root--queue-live-update)
                 (lambda (channel-ids structural header)
                   (setq queued (list channel-ids structural header)))))
        (disco-root--handle-preview-update "dm1")
        (should (equal '(("dm1") nil nil) queued))))))

(ert-deftest disco-root-directory-preview-queues-fetch-without-placeholder ()
  (let ((channel '((id . "c1") (type . 0) (name . "general")
                   (last_message_id . "100")))
        queued)
    (cl-letf (((symbol-function 'disco-msg-channel-last-cached-message)
               (lambda (_channel) nil))
              ((symbol-function 'disco-state-channel-conversation-summary-preview)
               (lambda (_channel-id) nil))
              ((symbol-function 'disco-preview-request-channel)
               (lambda (_channel)
                 (setq queued t))))
      (should
       (equal ""
              (disco-root--activity-preview-line
               channel nil 'directory)))
      (should queued))))

(ert-deftest disco-root-forum-preview-keeps-message-oriented-preview ()
  (let ((forum '((id . "forum") (guild_id . "g1")
                 (type . 15) (name . "Ideas"))))
    (cl-letf (((symbol-function 'disco-msg-channel-preview-line)
               (lambda (_channel) "latest forum activity"))
              ((symbol-function 'disco-preview-request-channel)
               (lambda (_channel)
                 (ert-fail "available forum preview requested hydration"))))
      (should (equal "latest forum activity"
                     (disco-root--activity-preview-line
                      forum nil 'directory))))))

(ert-deftest disco-root-parent-thread-preview-shows-unavailable-state ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((thread '((id . "th1")
                    (guild_id . "g1")
                    (type . 11)
                    (parent_id . "forum1")
                    (last_message_id . "44")
                    (message_count . 8)))
          queued)
      (cl-letf (((symbol-function 'disco-preview-request-channel)
                 (lambda (_channel)
                   (setq queued t))))
        (let ((preview
               (disco-root--activity-preview-line
                thread nil 'parent-thread)))
          (should (equal "Original post unavailable"
                         (substring-no-properties preview)))
          (should (eq 'shadow (get-text-property 0 'face preview))))
        (should-not queued)))))

(ert-deftest disco-root-parent-thread-preview-prefers-cached-starter-message ()
  (disco-state-reset)
  (unwind-protect
      (let ((thread '((id . "th1")
                      (guild_id . "g1")
                      (type . 11)
                      (parent_id . "forum1")
                      (last_message_id . "latest")))
            queued)
        (disco-state-upsert-channel thread)
        (disco-state-upsert-message
         "th1"
         '((id . "th1")
           (channel_id . "th1")
           (content . "starter preview")
           (author . ((username . "alice")))))
        (disco-state-upsert-message
         "th1"
         '((id . "latest")
           (channel_id . "th1")
           (content . "latest preview")
           (author . ((username . "bob")))))
        (cl-letf (((symbol-function 'disco-preview-request-channel)
                   (lambda (_channel) (setq queued t))))
          (should
           (equal "alice> starter preview"
                  (appkit-view-one-line-row-preview
                   (disco-root--channel-one-line-row thread 'parent-thread))))
          (should-not queued)))
    (disco-state-reset)))

(ert-deftest disco-root-parent-thread-row-uses-thread-icon-not-guild-icon ()
  (disco-state-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-state-set-guilds '(((id . "g1") (name . "Guild"))))
        (disco-root--insert-activity-icon
         '((id . "th1") (guild_id . "g1") (type . 11))
         'parent-thread)
        (should (equal "↳" (substring-no-properties (buffer-string)))))
    (disco-state-reset)))

(ert-deftest disco-root-collect-activity-channels-default-excludes-threads ()
  (disco-state-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-root-mode)
        (let ((disco-root-activity-include-threads nil)
              (guild-id "g1"))
          (disco-state-set-guilds (list `((id . ,guild-id)
                                          (name . "Guild One"))))
          (disco-state-put-channels
           guild-id
           (list '((id . "c1")
                   (guild_id . "g1")
                   (type . 0)
                   (permissions . "1024")
                   (name . "general")
                   (last_message_id . "10"))
                 '((id . "t1")
                   (guild_id . "g1")
                   (type . 11)
                   (permissions . "1024")
                   (parent_id . "c1")
                   (name . "hot-thread")
                   (last_message_id . "11"))))
          (let ((ids (mapcar (lambda (ch) (alist-get 'id ch))
                             (disco-root--collect-activity-channels))))
            (should (member "c1" ids))
            (should-not (member "t1" ids)))))
    (disco-state-reset)))

(ert-deftest disco-root-collect-activity-channels-includes-threads-when-enabled ()
  (disco-state-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-root-mode)
        (let ((disco-root-activity-include-threads t)
              (guild-id "g1"))
          (disco-state-set-guilds (list `((id . ,guild-id)
                                          (name . "Guild One"))))
          (disco-state-put-channels
           guild-id
           (list '((id . "c1")
                   (guild_id . "g1")
                   (type . 0)
                   (permissions . "1024")
                   (name . "general")
                   (last_message_id . "10"))
                 '((id . "t1")
                   (guild_id . "g1")
                   (type . 11)
                   (permissions . "1024")
                   (parent_id . "c1")
                   (name . "hot-thread")
                   (last_message_id . "11"))))
          (let ((ids (mapcar (lambda (ch) (alist-get 'id ch))
                             (disco-root--collect-activity-channels))))
            (should (member "c1" ids))
            (should (member "t1" ids)))))
    (disco-state-reset)))

(ert-deftest disco-root-auto-fill-to-width-requests-geometry-sync-on-change ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 80)
          requested)
      (let ((view (disco-root-test--current-live-view)))
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (candidate &rest arguments)
                     (setq requested (cons candidate arguments)))))
          (should (disco-root--auto-fill-to-width 100))
          (should (= 100 disco-root--fill-column))
          (should (eq view (car requested)))
          (should (eq 'geometry (plist-get (cdr requested) :part)))
          (should (plist-get (cdr requested) :position)))))))

(ert-deftest disco-root-auto-fill-to-width-noop-when-unchanged ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 80)
          requested)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (&rest arguments)
                   (setq requested arguments))))
        (should-not (disco-root--auto-fill-to-width 80))
        (should-not requested)))))

(ert-deftest disco-root-reflow-layout-refreshes-existing-ewoc ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--ewoc 'dummy-ewoc)
          ewoc-refreshed
          full-rendered)
      (cl-letf (((symbol-function 'ewoc-refresh)
                 (lambda (_ewoc)
                   (setq ewoc-refreshed t)))
                ((symbol-function 'disco-root-render)
                 (lambda ()
                   (setq full-rendered t))))
        (disco-root--reflow-layout)
        (should ewoc-refreshed)
        (should-not full-rendered)))))

(ert-deftest disco-root-chars-xwidth-avoids-window-font-width-side-effects ()
  (cl-letf (((symbol-function 'disco-root--display-window)
             (lambda (&optional _buffer)
               'fake-window))
            ((symbol-function 'window-live-p)
             (lambda (_window)
               t))
            ((symbol-function 'window-frame)
             (lambda (_window)
               'fake-frame))
            ((symbol-function 'frame-live-p)
             (lambda (_frame)
               t))
            ((symbol-function 'face-font)
             (lambda (_face _frame)
               'fake-font))
            ((symbol-function 'font-info)
             (lambda (_font _frame)
               (let ((info (make-vector 12 0)))
                 (aset info 11 15)
                 info)))
            ((symbol-function 'window-font-width)
             (lambda (&rest _args)
               (ert-fail "window-font-width should not be used"))))
    (should (= 150 (disco-root--chars-xwidth 10)))))

(ert-deftest disco-root-compute-fill-column-uses-remap-margins-and-line-number-width ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root-auto-fill-margin-columns 1))
      (cl-letf (((symbol-function 'disco-root--display-window)
                 (lambda (&optional _buffer)
                   (selected-window)))
                ((symbol-function 'disco-root--window-width-remap)
                 (lambda (_window) 100))
                ((symbol-function 'window-margins)
                 (lambda (&optional _window)
                   '(1 . 2)))
                ((symbol-function 'line-number-display-width)
                 (lambda (&rest _args)
                   16))
                ((symbol-function 'disco-root--chars-in-width)
                 (lambda (&rest _args)
                   2)))
        (should (= 100 (disco-root--compute-fill-column)))))))

(ert-deftest disco-root-buffer-auto-fill-respects-auto-fill-toggle ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root-auto-fill-on-window-size-change nil)
          called)
      (cl-letf (((symbol-function 'disco-root--compute-fill-column)
                 (lambda (&optional _buffer _window)
                   90))
                ((symbol-function 'disco-root--auto-fill-to-width)
                 (lambda (width &optional force)
                   (setq called (list width force))
                   t)))
        (disco-root-buffer-auto-fill)
        (should-not called)
        (disco-root-buffer-auto-fill t)
        (should (equal '(90 t) called))))))

(ert-deftest disco-root-scaled-image-applies-text-scale-factor ()
  (with-temp-buffer
    (setq-local text-scale-mode-amount 2)
    (let* ((text-scale-mode-step 1.2)
           (image '(image :type png :data "x"))
           (scaled (disco-root--scaled-image image (current-buffer)))
           (scale (plist-get (cdr scaled) :scale)))
      (should (numberp scale))
      (should (< (abs (- scale 1.44)) 0.001))
      (should-not (plist-member (cdr image) :scale)))))

(ert-deftest disco-root-scaled-image-noop-at-default-text-scale ()
  (with-temp-buffer
    (setq-local text-scale-mode-amount 0)
    (let ((image '(image :type png :data "x")))
      (should (eq image (disco-root--scaled-image image (current-buffer)))))))

(ert-deftest disco-root-activity-time-status-symbol-checkmarks-own-message ()
  (let ((channel '((id . "c1")
                   (last_message_id . "99")))
        (message '((id . "99")
                   (author . ((id . "u1"))))))
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "u1"))
              ((symbol-function 'disco-root--channel-read-p)
               (lambda (_channel) t)))
      (should (equal "✔"
                     (disco-root--activity-time-status-symbol channel message))))
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "u1"))
              ((symbol-function 'disco-root--channel-read-p)
               (lambda (_channel) nil)))
      (should (equal "✓"
                     (disco-root--activity-time-status-symbol channel message))))))

(ert-deftest disco-root-activity-time-status-symbol-uses-unread-dot ()
  (let ((channel '((id . "c2")))
        (message '((id . "100")
                   (author . ((id . "u9"))))))
    (cl-letf (((symbol-function 'disco-gateway-current-user-id)
               (lambda () "u1"))
              ((symbol-function 'disco-root--channel-has-unread-p)
               (lambda (_channel) t)))
      (should (equal "•"
                     (disco-root--activity-time-status-symbol channel message))))))

(ert-deftest disco-root-channel-last-activity-time-label-appends-status ()
  (let ((channel '((id . "c3"))))
    (cl-letf (((symbol-function 'disco-root--channel-last-activity-seconds)
               (lambda (&rest _args) 123456.0))
              ((symbol-function 'disco-root--activity-time-string)
               (lambda (&rest _args) "Wed"))
              ((symbol-function 'disco-root--activity-time-status-symbol)
               (lambda (&rest _args) "•")))
      (should (equal "Wed•"
                     (disco-root--channel-last-activity-time-label channel nil))))))

(ert-deftest disco-root-channel-label-uses-mention-badge ()
  (let ((channel '((id . "c4")
                   (type . 0)
                   (name . "general"))))
    (cl-letf (((symbol-function 'disco-state-channel-effective-unread-count)
               (lambda (_channel) 3))
              ((symbol-function 'disco-root--channel-has-unread-p)
               (lambda (_channel) t)))
      (let ((label (disco-root--channel-label channel)))
        (should (string-match-p "@3" label))
        (should-not (string-match-p "•" label))
        (should-not (string-match-p "\\[read\\]" label))))))

(ert-deftest disco-root-channel-label-shows-unread-when-no-mention-badge ()
  (let ((channel '((id . "c5")
                   (type . 0)
                   (name . "general"))))
    (cl-letf (((symbol-function 'disco-state-channel-effective-unread-count)
               (lambda (_channel) 0))
              ((symbol-function 'disco-root--channel-has-unread-p)
               (lambda (_channel) t)))
      (let ((label (disco-root--channel-label channel)))
        (should (string-match-p "•" label))
        (should-not (string-match-p "@[0-9]+" label))))))

(ert-deftest disco-root-channel-label-shows-age-restricted-tag ()
  (let ((channel '((id . "c6")
                   (type . 0)
                   (name . "adult")
                   (nsfw . t))))
    (cl-letf (((symbol-function 'disco-state-channel-effective-unread-count)
               (lambda (_channel) 0))
              ((symbol-function 'disco-root--channel-has-unread-p)
               (lambda (_channel) nil)))
      (let ((label (disco-root--channel-label channel)))
        (should (string-match-p (regexp-quote "[18+]") label))))))

(ert-deftest disco-root-line-has-unread-p-uses-state-flag-or-count ()
  (with-temp-buffer
    (insert "row\n")
    (add-text-properties 1 4 '(disco-unread-count 0))
    (should-not (disco-root--line-has-unread-p 1))
    (add-text-properties 1 4 '(disco-unread-count 2))
    (should (disco-root--line-has-unread-p 1))
    (add-text-properties 1 4 '(disco-has-unread t disco-unread-count 0))
    (should (disco-root--line-has-unread-p 1))))

(ert-deftest disco-root-session-cache-reset-revokes-old-icon-callbacks ()
  (let ((disco-root--session-cache-reset-in-progress nil)
        (disco-root--guild-icon-fetch-generation 8)
        (disco-root--guild-icon-image-cache
         (make-hash-table :test #'equal))
        (disco-root--guild-icon-fetching
         (make-hash-table :test #'equal))
        (disco-root--extra-info-provider-error-cache
         (make-hash-table :test #'eq))
        (disco-root-search-history '("OLD_ACCOUNT_SECRET-query"))
        then-callback
        else-callback
        canceled
        (plz-calls 0)
        (rerender-count 0))
    (puthash "old-cache" "https://OLD_ACCOUNT_SECRET.invalid/icon.png"
             disco-root--guild-icon-image-cache)
    (puthash 'old-provider t disco-root--extra-info-provider-error-cache)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (cl-incf plz-calls)
                 (setq then-callback (plist-get args :then)
                       else-callback (plist-get args :else))
                 'old-root-icon-process))
              ((symbol-function 'process-live-p)
               (lambda (process) (eq process 'old-root-icon-process)))
              ((symbol-function 'delete-process)
               (lambda (process)
                 (setq canceled process)
                 (funcall then-callback "OLD_ACCOUNT_SECRET-bytes")
                 (disco-root--start-guild-icon-fetch
                  "reentrant"
                  "https://OLD_ACCOUNT_SECRET.invalid/reentrant.png")))
              ((symbol-function 'create-image)
               (lambda (&rest _args) :image))
              ((symbol-function 'disco-root--guild-icon-image-valid-p)
               (lambda (image) (eq image :image)))
              ((symbol-function 'disco-root--rerender-open-root-buffers)
               (lambda () (cl-incf rerender-count))))
      (disco-root--start-guild-icon-fetch
       "live-icon" "https://OLD_ACCOUNT_SECRET.invalid/live.png")
      (should (= 1 plz-calls))
      (should (eq 'old-root-icon-process
                  (plist-get
                   (gethash "live-icon" disco-root--guild-icon-fetching)
                   :process)))
      (disco-root-reset-session-cache-state)
      (should (eq 'old-root-icon-process canceled))
      (should (= 1 plz-calls))
      (should (= 0 rerender-count))
      (should-not disco-root-search-history)
      (should (= 0 (hash-table-count disco-root--guild-icon-image-cache)))
      (should (= 0 (hash-table-count disco-root--guild-icon-fetching)))
      (should (= 0 (hash-table-count
                    disco-root--extra-info-provider-error-cache)))
      (funcall then-callback "OLD_ACCOUNT_SECRET-late")
      (funcall else-callback '(:message "OLD_ACCOUNT_SECRET-late"))
      (should (= 0 rerender-count))
      (should (= 0 (hash-table-count disco-root--guild-icon-image-cache))))))

(ert-deftest disco-root-session-cache-reset-clears-after-cancel-throw ()
  (let ((disco-root--guild-icon-fetch-generation 1)
        (disco-root--guild-icon-image-cache
         (make-hash-table :test #'equal))
        (disco-root--guild-icon-fetching
         (make-hash-table :test #'equal))
        (disco-root--extra-info-provider-error-cache
         (make-hash-table :test #'eq)))
    (puthash "secret" "OLD_ACCOUNT_SECRET"
             disco-root--guild-icon-image-cache)
    (puthash "secret" (list :generation 1 :process 'process)
             disco-root--guild-icon-fetching)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (_process) (throw 'cancel-escape :escaped))))
      (should (eq :escaped
                  (catch 'cancel-escape
                    (disco-root-reset-session-cache-state)
                    :returned)))
      (should (= 0 (hash-table-count disco-root--guild-icon-image-cache)))
      (should (= 0 (hash-table-count disco-root--guild-icon-fetching))))))

(ert-deftest disco-root-icon-process-cancel-drain-is-stack-safe ()
  (let ((max-lisp-eval-depth 800)
        (canceled 0))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (_process) (cl-incf canceled))))
      (disco-root-view--cancel-guild-icon-processes
       (number-sequence 1 2000)))
    (should (= 2000 canceled)))
  (let (canceled)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (process)
                 (push process canceled)
                 (throw 'cancel-escape process))))
      (should (eq 'third
                  (catch 'cancel-escape
                    (disco-root-view--cancel-guild-icon-processes
                     '(escape second third))
                    :returned))))
    (should (equal '(third second escape) canceled))))

(provide 'disco-root-test)

;;; disco-root-test.el ends here
