;;; disco-root-test.el --- Tests for disco-root live patching -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)

(require 'disco-root)

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

(ert-deftest disco-root-tree-default-collapses-server-directory ()
  (let ((disco-root-tree-default-expanded-sections '(unread private))
        (disco-root--section-expanded nil))
    (disco-root--ensure-section-state '(unread private guilds))
    (should (disco-root--section-expanded-p 'unread))
    (should (disco-root--section-expanded-p 'private))
    (should-not (disco-root--section-expanded-p 'guilds))))

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

(ert-deftest disco-root-flush-live-updates-renders-in-unread-view ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--dirty-channel-ids '("c1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
          (disco-root--view-mode 'unread)
          rendered)
      (cl-letf (((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--flush-live-updates (current-buffer))
        (should rendered)))))

(ert-deftest disco-root-flush-live-updates-rerenders-archived-thread-buffer ()
  (with-temp-buffer
    (disco-root-archived-threads-mode)
    (let ((disco-root--dirty-channel-ids '("t1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          rendered)
      (cl-letf (((symbol-function 'disco-root--archived-threads-list-spec)
                 (lambda () 'spec))
                ((symbol-function 'disco-view-render-list-spec-preserving-position)
                 (lambda (_spec &rest _args)
                   (setq rendered t))))
        (disco-root--flush-live-updates (current-buffer))
        (should rendered)))))

(ert-deftest disco-root-flush-live-updates-patches-in-all-view ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--dirty-channel-ids '("c1" "c2"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
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
        (disco-root--flush-live-updates (current-buffer))
        (should-not rendered)
        (should (equal '("c1" "c2")
                       (sort (copy-sequence patched) #'string-lessp)))
        (should (equal '("c1" "c2")
                       (sort (copy-sequence heading-ids) #'string-lessp)))))))

(ert-deftest disco-root-flush-live-updates-renders-for-full-layout ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root-custom-layouts
           '((stress-full
              :label "Stress Full"
              :build disco-root--build-activity-layout-view-spec
              :update-mode full)))
          (disco-root--layout 'stress-full)
          (disco-root--dirty-channel-ids '("c1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
          rendered)
      (cl-letf (((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--flush-live-updates (current-buffer))
        (should rendered)))))

(ert-deftest disco-root-flush-live-updates-activity-reorders-incrementally ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--dirty-channel-ids '("c1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
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
                ((symbol-function 'disco-root--maybe-refresh-activity-header-line)
                 (lambda () t))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--flush-live-updates (current-buffer))
        (should reordered)
        (should-not rendered)))))

(ert-deftest disco-root-flush-live-updates-hidden-buffer-keeps-incremental-path ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--dirty-channel-ids '("c1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
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
                ((symbol-function 'disco-root--maybe-refresh-activity-header-line)
                 (lambda () t))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--flush-live-updates (current-buffer))
        (should patched)
        (should-not rendered)))))

(ert-deftest disco-root-flush-live-updates-unfocused-activity-keeps-incremental-path ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--dirty-channel-ids '("c1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
          (disco-root--view-mode 'all)
          rendered
          patched
          restored)
      (cl-letf (((symbol-function 'disco-root--buffer-visible-p)
                 (lambda (&optional _buffer) t))
                ((symbol-function 'disco-root--selected-window-for-buffer)
                 (lambda (&optional _buffer) nil))
                ((symbol-function 'disco-view-capture-position)
                 (lambda (&rest _args) 'snapshot))
                ((symbol-function 'disco-view-restore-position)
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
                ((symbol-function 'disco-root--maybe-refresh-activity-header-line)
                 (lambda () t))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (disco-root--flush-live-updates (current-buffer))
        (should patched)
        (should restored)
        (should-not rendered)))))

(ert-deftest disco-root-flush-live-updates-activity-uses-throttled-header-refresh ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          (disco-root--dirty-channel-ids '("c1"))
          (disco-root--dirty-structure-p nil)
          (disco-root--dirty-header-p nil)
          (disco-root--refresh-in-flight nil)
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
                ((symbol-function 'disco-root--maybe-refresh-activity-header-line)
                 (lambda ()
                   (setq called t)
                   t))
                ((symbol-function 'disco-root--render-preserving-position)
                 (lambda () nil)))
        (disco-root--flush-live-updates (current-buffer))
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

(ert-deftest disco-root-maybe-refresh-activity-header-line-respects-interval ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root-activity-header-refresh-interval 1.0)
          (disco-root--last-header-refresh-at 10.0)
          refreshed)
      (cl-letf (((symbol-function 'float-time) (lambda () 10.5))
                ((symbol-function 'disco-root--refresh-header-line)
                 (lambda ()
                   (setq refreshed t))))
        (should-not (disco-root--maybe-refresh-activity-header-line))
        (should-not refreshed))
      (setq refreshed nil)
      (cl-letf (((symbol-function 'float-time) (lambda () 11.1))
                ((symbol-function 'disco-root--refresh-header-line)
                 (lambda ()
                   (setq refreshed t))))
        (should (disco-root--maybe-refresh-activity-header-line))
        (should refreshed)))))

(ert-deftest disco-root-rerender-open-root-buffers-debounces-via-live-update-queue ()
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
    (let (rendered preserved updated)
      (cl-letf (((symbol-function 'disco-root-render)
                 (lambda ()
                   (setq rendered t)))
                ((symbol-function 'disco-view-render-preserving-position)
                 (lambda (fn &rest args)
                   (setq preserved (plist-get args :preserve-window-start))
                   (funcall fn)))
                ((symbol-function 'disco-root--update-window-points)
                 (lambda (&optional _point)
                   (setq updated t))))
        (disco-root--render-preserving-position)
        (should rendered)
        (should preserved)
        (should updated)))))

(ert-deftest disco-root-reflow-preserving-position-syncs-window-points ()
  (with-temp-buffer
    (disco-root-mode)
    (let (reflowed preserved updated)
      (cl-letf (((symbol-function 'disco-root--reflow-layout)
                 (lambda ()
                   (setq reflowed t)))
                ((symbol-function 'disco-view-render-preserving-position)
                 (lambda (fn &rest args)
                   (setq preserved (plist-get args :preserve-window-start))
                   (funcall fn)))
                ((symbol-function 'disco-root--update-window-points)
                 (lambda (&optional _point)
                   (setq updated t))))
        (disco-root--reflow-preserving-position)
        (should reflowed)
        (should preserved)
        (should updated)))))

(ert-deftest disco-root-toggle-unread-lens-tree-toggles-section ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'tree)
          (disco-root--tree-show-unread-section t)
          rendered)
      (cl-letf (((symbol-function 'disco-root--render-preserving-position)
                 (lambda ()
                   (setq rendered t)))
                ((symbol-function 'message) (lambda (&rest _args) nil)))
        (disco-root-toggle-unread-lens)
        (should rendered)
        (should-not disco-root--tree-show-unread-section)))))

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

(ert-deftest disco-root-guild-row-opens-separate-channel-directory ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((inhibit-read-only t)
          opened)
      (disco-view-insert-label-row
       (disco-root--guild-label-row
        '((id . "g1") (name . "Guild")) 0))
      (should-not
       (text-property-not-all (point-min) (point-max) 'mouse-face nil))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'disco-channel-directory-open)
                 (lambda (guild-id)
                   (setq opened guild-id))))
        (disco-root-open-at-point)
        (should (equal "g1" opened))))))

(ert-deftest disco-root-tree-projects-guild-navigation-without-channel-children ()
  (with-temp-buffer
    (disco-root-mode)
    (disco-state-reset)
    (disco-state-set-guilds '(((id . "g1") (name . "Guild"))))
    (disco-state-put-channels
     "g1"
     '(((id . "cat") (guild_id . "g1") (name . "Category") (type . 4))
       ((id . "c1") (guild_id . "g1") (parent_id . "cat")
        (name . "general") (type . 0))))
    (disco-root--set-section-expanded 'guilds t)
    (let* ((disco-root--tree-show-unread-section nil)
           (entries (disco-root--tree-layout-entries))
           (guild-entries
            (seq-filter
             (lambda (entry)
               (eq (disco-root-layout-entry-type entry) 'guild))
             entries))
           (channel-entries
            (seq-filter
             (lambda (entry)
               (eq (disco-root-layout-entry-type entry) 'channel))
             entries)))
      (should (= 1 (length guild-entries)))
      (should-not channel-entries))))

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
                    (disco-view-list-spec-create
                     :title "Builder Demo"
                     :empty-text "(empty)")))))
        (should (disco-root-layout-render 'demo))
        (should (string-match-p "Builder Demo" (buffer-string)))))))

(ert-deftest disco-root-layout-list-spec-view-spec-create-wraps-list-spec ()
  (let* ((list-spec (disco-view-list-spec-create :title "List" :empty-text "(empty)"))
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

(ert-deftest disco-root-build-tree-layout-view-spec-returns-ewoc-entry-view-spec ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--view-mode 'all)
          (disco-root--tree-show-unread-section t))
      (cl-letf (((symbol-function 'disco-root--collect-visible-unread-channels)
                 (lambda () '(((id . "u1") (type . 0) (name . "updates")))))
                ((symbol-function 'disco-root--tree-unread-section-channels)
                 (lambda (channels) channels))
                ((symbol-function 'disco-root--visible-private-channels)
                 (lambda () nil))
                ((symbol-function 'disco-state-guilds)
                 (lambda () nil))
                ((symbol-function 'disco-root--ensure-section-state)
                 (lambda (_sections) nil))
                ((symbol-function 'disco-root--section-expanded-p)
                 (lambda (_section) t)))
        (let* ((view-spec (disco-root--build-tree-layout-view-spec))
               (entries (disco-root-layout-view-spec-entries view-spec)))
          (should (disco-root-layout-view-spec-p view-spec))
          (should (eq 'entries (disco-root-layout-view-spec-kind view-spec)))
          (should (eq 'section (disco-root-layout-entry-type (car entries))))
          (should (equal 'unread (disco-root-layout-entry-section (car entries))))
          (let ((channel-entry
                 (seq-find (lambda (entry)
                             (eq 'channel
                                 (disco-root-layout-entry-type entry)))
                           entries)))
            (should channel-entry)
            (should (eq 'activity
                        (disco-root-layout-entry-scope channel-entry)))))))))

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
               (entries (disco-view-list-spec-items spec))
               (first-entry (car entries)))
          (should (eq 'disco-root--insert-layout-entry
                      (disco-view-list-spec-item-inserter spec)))
          (should (eq 'channel (disco-root-layout-entry-type first-entry)))
          (should (eq 'archived-thread (disco-root-layout-entry-scope first-entry))))))))

(ert-deftest disco-root-mode-disables-undo-history ()
  (with-temp-buffer
    (disco-root-mode)
    (should (eq buffer-undo-list t))
    (should-not switch-to-buffer-preserve-window-point)))

(ert-deftest disco-root-open-displays-buffer-before-rendering ()
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
            (should (equal '(pop attach render) (nreverse order))))
        (when (buffer-live-p displayed)
          (kill-buffer displayed))))))

(ert-deftest disco-root-open-at-point-jumps-to-search-message ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((inhibit-read-only t))
      (insert "hit\n")
      (add-text-properties (point-min) (point-max)
                           '(disco-root-search-message-id "m1"
                             disco-channel-id "c1")))
    (goto-char (point-min))
    (let (jumped)
      (cl-letf (((symbol-function 'disco-room-jump-to-message)
                 (lambda (message-id channel-id)
                   (setq jumped (list message-id channel-id)))))
        (disco-root-open-at-point)
        (should (equal '("m1" "c1") jumped))))))

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

(ert-deftest disco-root-search-refresh-active-completions-refreshes_minibuffer_help ()
  (let ((mini-buffer (generate-new-buffer " *mini*"))
        refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'active-minibuffer-window)
                   (lambda () 'miniwin))
                  ((symbol-function 'get-buffer-window)
                   (lambda (buffer &optional _all-frames)
                     (and (equal buffer "*Completions*") 'compwin)))
                  ((symbol-function 'window-buffer)
                   (lambda (_win) mini-buffer))
                  ((symbol-function 'minibuffer-completion-help)
                   (lambda ()
                     (setq refreshed t))))
          (with-current-buffer mini-buffer
            (setq-local disco-root--search-completion-domain
                        '(:kind guild :id "g1" :label "Guild"))
            (disco-root--search-refresh-active-completions "g1")
            (should refreshed)))
      (kill-buffer mini-buffer))))

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
        (disco-state-upsert-channel '((id . "voice1") (type . 2) (name . "Voice")))
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
          (disco-state-upsert-channel
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
        (disco-state-upsert-channel
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

(ert-deftest disco-root-search-channel-candidates-skip-unsearchable-types ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-channel '((id . "text1") (guild_id . "g1") (type . 0) (name . "chat")))
        (disco-state-upsert-channel '((id . "dir1") (guild_id . "g1") (type . 14) (name . "Directory")))
        (let ((candidates (disco-root--search-channel-candidates '(:kind guild :id "g1"))))
          (should (member '("chat" . "text1") candidates))
          (should-not (member '("Directory" . "dir1") candidates))))
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

(ert-deftest disco-root-forum-preview-is-active-post-count-without-message-fetch ()
  (disco-state-reset)
  (unwind-protect
      (let ((forum '((id . "forum") (guild_id . "g1")
                     (type . 15) (name . "Ideas")))
            queued)
        (disco-state-upsert-channel forum)
        (disco-state-upsert-channel
         '((id . "active") (guild_id . "g1") (parent_id . "forum")
           (type . 11) (thread_metadata . ((archived . :false)))))
        (disco-state-upsert-channel
         '((id . "archived") (guild_id . "g1") (parent_id . "forum")
           (type . 11) (thread_metadata . ((archived . t)))))
        (cl-letf (((symbol-function 'disco-preview-request-channel)
                   (lambda (_channel) (setq queued t))))
          (should (equal "1 active post"
                         (disco-root--activity-preview-line
                          forum nil 'directory)))
          (should-not queued)))
    (disco-state-reset)))

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
                  (disco-view-one-line-row-preview
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
                   (name . "general")
                   (last_message_id . "10"))
                 '((id . "t1")
                   (guild_id . "g1")
                   (type . 11)
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
                   (name . "general")
                   (last_message_id . "10"))
                 '((id . "t1")
                   (guild_id . "g1")
                   (type . 11)
                   (parent_id . "c1")
                   (name . "hot-thread")
                   (last_message_id . "11"))))
          (let ((ids (mapcar (lambda (ch) (alist-get 'id ch))
                             (disco-root--collect-activity-channels))))
            (should (member "c1" ids))
            (should (member "t1" ids)))))
    (disco-state-reset)))

(ert-deftest disco-root-auto-fill-to-width-rerenders-on-change ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 80)
          rendered)
      (cl-letf (((symbol-function 'disco-root--reflow-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (should (disco-root--auto-fill-to-width 100))
        (should (= 100 disco-root--fill-column))
        (should rendered)))))

(ert-deftest disco-root-auto-fill-to-width-noop-when-unchanged ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--fill-column 80)
          rendered)
      (cl-letf (((symbol-function 'disco-root--reflow-preserving-position)
                 (lambda ()
                   (setq rendered t))))
        (should-not (disco-root--auto-fill-to-width 80))
        (should-not rendered)))))

(ert-deftest disco-root-reflow-layout-refreshes-existing-ewoc ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--ewoc 'dummy-ewoc)
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

(provide 'disco-root-test)

;;; disco-root-test.el ends here
