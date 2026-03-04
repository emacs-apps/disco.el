;;; disco-root-test.el --- Tests for disco-root live patching -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-root)

(ert-deftest disco-root-event-channel-ids-aggregates-and-dedupes ()
  (should
   (equal
    '("c0" "t0" "c1" "c2" "c3" "c4" "p4" "c5")
    (disco-root--event-channel-ids
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
      (cl-letf (((symbol-function 'disco-root--refresh-channel-node)
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
    (let ((disco-root--layout 'activity)
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

(ert-deftest disco-root-layout-specs-merge-custom-layout-overrides ()
  (let ((disco-root-custom-layouts
         '((activity :label "Recent" :update-mode full)
           (custom-demo :label "Custom Demo" :update-mode incremental))))
    (should (equal "Recent" (disco-root-layout-label 'activity)))
    (should (eq 'full (disco-root-layout-update-mode 'activity)))
    (should (equal "Custom Demo" (disco-root-layout-label 'custom-demo)))))

(ert-deftest disco-root-mode-disables-undo-history ()
  (with-temp-buffer
    (disco-root-mode)
    (should (eq buffer-undo-list t))))

(ert-deftest disco-root-toggle-section-at-point-activity-falls-forward ()
  (with-temp-buffer
    (disco-root-mode)
    (let ((disco-root--layout 'activity)
          moved)
      (cl-letf (((symbol-function 'disco-root-button-forward)
                 (lambda (&optional _n)
                   (setq moved t))))
        (disco-root-toggle-section-at-point)
        (should moved)))))

(ert-deftest disco-root-activity-primary-label-includes-channel-category-guild ()
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
          (should (equal "emacs | General | Emacs CN"
                         (disco-root--activity-primary-label channel))))
      (disco-state-reset))))

(ert-deftest disco-root-canonicalize-number-supports-ratio-and-bounds ()
  (should (= 42 (disco-root--canonicalize-number 42 100)))
  (should (= 35 (disco-root--canonicalize-number 0.35 100)))
  (should (= 20 (disco-root--canonicalize-number '(0.1 20 60) 100)))
  (should (= 60 (disco-root--canonicalize-number '(0.8 20 60) 100)))
  (should (= 90 (disco-root--canonicalize-number '(0.9 20) 100))))

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
          divider-refreshed
          ewoc-refreshed
          full-rendered)
      (cl-letf (((symbol-function 'disco-root--refresh-mode-divider-line)
                 (lambda ()
                   (setq divider-refreshed t)))
                ((symbol-function 'ewoc-refresh)
                 (lambda (_ewoc)
                   (setq ewoc-refreshed t)))
                ((symbol-function 'disco-root-render)
                 (lambda ()
                   (setq full-rendered t))))
        (disco-root--reflow-layout)
        (should divider-refreshed)
        (should ewoc-refreshed)
        (should-not full-rendered)))))

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

(ert-deftest disco-root-move-to-column-always-inserts-align-spacer ()
  (with-temp-buffer
    (insert "abc")
    (let ((insert-pos (point)))
      (disco-root--move-to-column 3)
      (should (= (point) (1+ insert-pos)))
      (let ((display-prop (get-text-property insert-pos 'display)))
        (should (consp display-prop))
        (should (eq (car display-prop) 'space))
        (should (plist-member (cdr display-prop) :align-to))))))

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
              ((symbol-function 'disco-state-channel-effective-unread-count)
               (lambda (_channel) 3)))
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

(provide 'disco-root-test)

;;; disco-root-test.el ends here
