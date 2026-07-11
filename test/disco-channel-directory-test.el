;;; disco-channel-directory-test.el --- Guild directory tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'disco-channel-directory)

(defun disco-channel-directory-test--entry-types (entries)
  "Return type symbols from directory ENTRIES."
  (mapcar #'disco-channel-directory-entry-type entries))

(defun disco-channel-directory-test--entry-id (entry)
  "Return semantic ID represented by directory ENTRY."
  (pcase (disco-channel-directory-entry-type entry)
    ('group (disco-channel-directory-entry-group-id entry))
    ('channel (alist-get 'id
                         (disco-channel-directory-entry-channel entry)))
    (_ nil)))

(defmacro disco-channel-directory-test--with-guild (&rest body)
  "Evaluate BODY in a populated guild-directory buffer."
  (declare (indent 0) (debug t))
  `(progn
     (disco-state-reset)
     (disco-directory-reset)
     (disco-state-set-guilds '(((id . "g1") (name . "Guild One"))))
     (disco-state-put-channels
      "g1"
      '(((id . "cat") (guild_id . "g1") (name . "Work")
         (type . 4) (position . 1))
        ((id . "c2") (guild_id . "g1") (parent_id . "cat")
         (name . "projects") (type . 0) (position . 2))
        ((id . "c1") (guild_id . "g1") (name . "general")
         (type . 0) (position . 0))))
     (disco-state-upsert-channel
      '((id . "t1") (guild_id . "g1") (parent_id . "c2")
        (name . "release") (type . 11)))
     (with-temp-buffer
       (disco-channel-directory-mode)
       (setq disco-channel-directory--guild-id "g1")
       (cl-letf (((symbol-function 'disco-permission-channel-viewable-p)
                  (lambda (&rest _args) t)))
         ,@body))))

(ert-deftest disco-channel-directory-projects-categories-and-active-threads ()
  (disco-channel-directory-test--with-guild
    (let* ((entries (disco-channel-directory--project-loaded-entries))
           (ids (mapcar #'disco-channel-directory-test--entry-id entries)))
      (should
       (equal '(group channel group channel channel)
              (disco-channel-directory-test--entry-types entries)))
      (should
       (equal (list disco-channel-directory--uncategorized-group
                    "c1" "cat" "c2" "t1")
              ids)))))

(ert-deftest disco-channel-directory-keyed-reconcile-preserves-unrelated-nodes ()
  (disco-channel-directory-test--with-guild
    (disco-channel-directory--reconcile)
    (let* ((c1-key (disco-channel-directory--entry-key-for-channel "c1"))
           (cat-key (disco-channel-directory--entry-key-for-group "cat"))
           (c1-node (gethash c1-key disco-channel-directory--node-table)))
      (should c1-node)
      (should (gethash cat-key disco-channel-directory--node-table))
      (puthash "cat" t disco-channel-directory--collapsed-groups)
      (disco-channel-directory--reconcile)
      (should (eq c1-node
                  (gethash c1-key disco-channel-directory--node-table)))
      (should-not
       (gethash (disco-channel-directory--entry-key-for-channel "c2")
                disco-channel-directory--node-table))
      (should-not
       (gethash (disco-channel-directory--entry-key-for-channel "t1")
                disco-channel-directory--node-table))
      (should-not
       (gethash
        (disco-channel-directory--entry-key-for-group
         disco-channel-directory--orphan-thread-group)
        disco-channel-directory--node-table)))))

(ert-deftest disco-channel-directory-live-row-update-retains-node-identity ()
  (disco-channel-directory-test--with-guild
    (disco-channel-directory--reconcile)
    (let* ((key (disco-channel-directory--entry-key-for-channel "c1"))
           (node (gethash key disco-channel-directory--node-table)))
      (disco-state-set-channel-unread "c1" 3)
      (disco-channel-directory--reconcile '("c1"))
      (should (eq node (gethash key disco-channel-directory--node-table)))
      (should (= 3 (car (disco-channel-directory-entry-stamp
                         (ewoc-data node))))))))

(ert-deftest disco-channel-directory-filter-reveals-matching-channel-path ()
  (disco-channel-directory-test--with-guild
    (puthash "cat" t disco-channel-directory--collapsed-groups)
    (setq disco-channel-directory--filter "projects")
    (let ((ids
           (mapcar
            #'disco-channel-directory-test--entry-id
            (disco-channel-directory--project-loaded-entries))))
      (should (equal '("cat" "c2") ids)))))

(ert-deftest disco-channel-directory-category-filter-reveals-category-children ()
  (disco-channel-directory-test--with-guild
    (setq disco-channel-directory--filter "work")
    (let ((ids
           (mapcar
            #'disco-channel-directory-test--entry-id
            (disco-channel-directory--project-loaded-entries))))
      (should (equal '("cat" "c2") ids)))))

(ert-deftest disco-channel-directory-open-hydrates-only-selected-guild ()
  (disco-state-reset)
  (disco-directory-reset)
  (disco-state-set-guilds '(((id . "g1") (name . "Guild One"))))
  (let (loaded displayed order)
    (cl-letf (((symbol-function 'disco-channel-directory--attach-live-updates)
               (lambda () (push 'attach order)))
              ((symbol-function 'disco-channel-directory--reflow-to-width)
               (lambda (_width) (push 'reflow order)))
              ((symbol-function 'disco-channel-directory--reconcile)
               (lambda (&rest _args) (push 'reconcile order)))
              ((symbol-function 'disco-directory-load-guild-async)
               (lambda (guild-id &rest _args)
                 (push 'load order)
                 (setq loaded guild-id)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (push 'pop order)
                 (setq displayed buffer))))
      (unwind-protect
          (progn
            (disco-channel-directory-open "g1")
            (should (equal "g1" loaded))
            (should (buffer-live-p displayed))
            (should (equal '(pop attach reflow reconcile load)
                           (nreverse order)))
            (with-current-buffer displayed
              (should (equal "g1" disco-channel-directory--guild-id))))
        (when (buffer-live-p displayed)
          (kill-buffer displayed))))))

(provide 'disco-channel-directory-test)

;;; disco-channel-directory-test.el ends here
