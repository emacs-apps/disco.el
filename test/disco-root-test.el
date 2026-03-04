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

(provide 'disco-root-test)

;;; disco-root-test.el ends here
