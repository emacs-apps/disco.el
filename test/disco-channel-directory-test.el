;;; disco-channel-directory-test.el --- Guild directory tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'disco-channel-directory)
(require 'disco-root)

(defun disco-channel-directory-test--entry-types (entries)
  "Return type symbols from directory ENTRIES."
  (mapcar #'disco-channel-directory-entry-type entries))

(defun disco-channel-directory-test--entry-id (entry)
  "Return semantic ID represented by directory ENTRY."
  (pcase (disco-channel-directory-entry-type entry)
    ('group (disco-channel-directory-entry-group-id entry))
    ((or 'channel 'thread-parent)
     (alist-get 'id
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

(ert-deftest disco-channel-directory-excludes-ordinary-channel-threads ()
  (disco-channel-directory-test--with-guild
    (let* ((entries (disco-channel-directory--project-loaded-entries))
           (ids (mapcar #'disco-channel-directory-test--entry-id entries)))
      (should
       (equal '(group channel group channel)
              (disco-channel-directory-test--entry-types entries)))
      (should
       (equal (list disco-channel-directory--uncategorized-group
                    "c1" "cat" "c2")
              ids)))))

(ert-deftest disco-channel-directory-ordinary-thread-upsert-keeps-projection ()
  (disco-channel-directory-test--with-guild
    (let ((before
           (mapcar #'disco-channel-directory-entry-key
                   (disco-channel-directory--project-loaded-entries))))
      (disco-state-upsert-channel
       '((id . "t2") (guild_id . "g1") (parent_id . "c1")
         (name . "newly opened thread") (type . 11)
         (message_count . 119) (member_count . 4)))
      (should
       (equal before
              (mapcar #'disco-channel-directory-entry-key
                      (disco-channel-directory--project-loaded-entries)))))))

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

(ert-deftest disco-channel-directory-header-is-cached-between-reconciles ()
  (disco-channel-directory-test--with-guild
    (should (eq 'disco-channel-directory--header-line-cache
                header-line-format))
    (let ((computations 0))
      (cl-letf (((symbol-function 'disco-channel-directory--header-line)
                 (lambda ()
                   (cl-incf computations)
                   " cached header"))
                ((symbol-function 'force-mode-line-update) #'ignore))
        (disco-channel-directory--refresh-header-line)
        (should (= 1 computations))
        (should (equal " cached header"
                       disco-channel-directory--header-line-cache))
        (format-mode-line header-line-format)
         (format-mode-line header-line-format)
         (should (= 1 computations))))))

(ert-deftest disco-channel-directory-state-reset-refreshes-cached-header ()
  (disco-channel-directory-test--with-guild
    (let ((refreshes 0))
      (cl-letf (((symbol-function 'disco-channel-directory--refresh-header-line)
                 (lambda () (cl-incf refreshes))))
        (disco-channel-directory--handle-state-reset)
        (should (= 1 refreshes))))))

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

(ert-deftest disco-channel-directory-forum-is-collapsed-and-excludes-archived-posts ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "ideas") (type . 15) (position . 3)))
    (disco-state-upsert-channel
     '((id . "active") (guild_id . "g1") (parent_id . "forum")
       (name . "active post") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (disco-state-upsert-message
     "active"
     '((id . "active") (channel_id . "active")
       (content . "starter")))
    (disco-state-upsert-channel
     '((id . "archived") (guild_id . "g1") (parent_id . "forum")
       (name . "archived post") (type . 11)
       (thread_metadata . ((archived . t)))))
    (let* ((collapsed (disco-channel-directory--project-loaded-entries))
           (forum-entry
            (seq-find
             (lambda (entry)
               (equal "forum"
                      (disco-channel-directory-test--entry-id entry)))
             collapsed)))
      (should (eq 'thread-parent
                  (disco-channel-directory-entry-type forum-entry)))
      (should-not (disco-channel-directory-entry-expanded forum-entry))
      (should-not
       (member "active"
               (mapcar #'disco-channel-directory-test--entry-id collapsed)))
      (puthash "forum" t disco-channel-directory--expanded-thread-parents)
      (let ((expanded-ids
             (mapcar #'disco-channel-directory-test--entry-id
                     (disco-channel-directory--project-loaded-entries))))
        (should (member "active" expanded-ids))
        (should-not (member "archived" expanded-ids))))))

(ert-deftest disco-channel-directory-forum-hides-unhydrated-posts ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "ideas") (type . 15)))
    (disco-state-upsert-channel
     '((id . "post") (guild_id . "g1") (parent_id . "forum")
       (name . "proposal") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (puthash "forum" t disco-channel-directory--expanded-thread-parents)
    (let ((ids
           (mapcar #'disco-channel-directory-test--entry-id
                   (disco-channel-directory--project-loaded-entries))))
      (should-not (member "post" ids)))
    (disco-state-upsert-message
     "post" '((id . "post") (channel_id . "post") (content . "starter")))
    (let ((ids
           (mapcar #'disco-channel-directory-test--entry-id
                   (disco-channel-directory--project-loaded-entries))))
      (should (member "post" ids)))))

(ert-deftest disco-channel-directory-forum-shows-confirmed-unavailable-starter ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "ideas") (type . 15)))
    (disco-state-upsert-channel
     '((id . "post") (guild_id . "g1") (parent_id . "forum")
       (name . "orphaned original") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (puthash "forum"
             '(:status loaded :starter-unavailable-ids ("post"))
             disco-directory--parent-thread-state)
    (puthash "forum" t disco-channel-directory--expanded-thread-parents)
    (let ((ids
           (mapcar #'disco-channel-directory-test--entry-id
                   (disco-channel-directory--project-loaded-entries))))
      (should (member "post" ids)))))

(ert-deftest disco-channel-directory-excludes-orphaned-threads ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "active") (guild_id . "g1") (parent_id . "missing")
       (name . "active thread") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (disco-state-upsert-channel
     '((id . "archived") (guild_id . "g1") (parent_id . "missing")
       (name . "archived post") (type . 11)
       (thread_metadata . ((archived . t)))))
    (let ((ids
           (mapcar #'disco-channel-directory-test--entry-id
                   (disco-channel-directory--project-loaded-entries))))
      (should-not (member "active" ids))
      (should-not (member "archived" ids))
      (should-not (memq :orphan-threads ids)))))

(ert-deftest disco-channel-directory-toggle-forum-loads-on-first-expansion ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (name . "ideas") (type . 15)))
    (let (loaded)
      (cl-letf (((symbol-function
                  'disco-directory-load-parent-threads-async)
                 (lambda (parent-id &rest _args)
                   (setq loaded parent-id)))
                ((symbol-function 'disco-channel-directory--line-property)
                 (lambda (property &optional _position)
                   (and (eq property
                            'disco-channel-directory-thread-parent-id)
                        "forum"))))
        (disco-channel-directory-toggle-thread-parent)
        (should (equal "forum" loaded))
        (should (gethash "forum"
                         disco-channel-directory--expanded-thread-parents))
        (disco-channel-directory-toggle-thread-parent)
        (should-not (gethash "forum"
                             disco-channel-directory--expanded-thread-parents))))))

(ert-deftest disco-channel-directory-refresh-is-parent-scoped-on-forum-row ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (name . "ideas") (type . 15)))
    (let (request)
      (cl-letf (((symbol-function
                  'disco-directory-load-parent-threads-async)
                 (lambda (parent-id &rest args)
                   (setq request (cons parent-id args))))
                ((symbol-function 'disco-channel-directory--line-property)
                 (lambda (property &optional _position)
                   (and (eq property
                            'disco-channel-directory-thread-parent-id)
                        "forum")))
                ((symbol-function 'message) #'ignore))
        (disco-channel-directory-refresh)
        (should (equal '("forum" :force t) request))))))

(ert-deftest disco-channel-directory-waits-for-resolved-channel-permissions ()
  (disco-state-reset)
  (disco-directory-reset)
  (disco-state-set-guilds '(((id . "g1") (name . "Guild One"))))
  (disco-state-seed-guild-channels
   "g1"
   '(((id . "visible") (guild_id . "g1") (name . "visible") (type . 0))
     ((id . "hidden") (guild_id . "g1") (name . "hidden") (type . 0))))
  (unwind-protect
      (with-temp-buffer
        (disco-channel-directory-mode)
        (setq disco-channel-directory--guild-id "g1")
        (should
         (equal '(note)
                (disco-channel-directory-test--entry-types
                 (disco-channel-directory--project-entries))))
        (let ((resolved
               '(((id . "visible") (guild_id . "g1") (name . "visible")
                  (type . 0) (permissions . "1024"))
                 ((id . "hidden") (guild_id . "g1") (name . "hidden")
                  (type . 0) (permissions . "0")))))
          (disco-state-put-channels "g1" resolved)
          (let ((before
                 (mapcar #'disco-channel-directory-test--entry-id
                         (disco-channel-directory--project-loaded-entries))))
            (should
             (equal (list disco-channel-directory--uncategorized-group
                          "visible")
                    before))
            (disco-state-put-channels "g1" (copy-tree resolved))
            (should
             (equal before
                    (mapcar
                     #'disco-channel-directory-test--entry-id
                     (disco-channel-directory--project-loaded-entries)))))))
    (disco-state-reset)
    (disco-directory-reset)))

(ert-deftest disco-channel-directory-rehydrates-provisional-gateway-sync ()
  (disco-channel-directory-test--with-guild
    (disco-state-seed-guild-channels
     "g1" (disco-state-guild-channels "g1"))
    (let (requested reconciled)
      (cl-letf (((symbol-function 'disco-directory-load-guild-async)
                 (lambda (guild-id &rest args)
                   (setq requested (cons guild-id args))))
                ((symbol-function 'disco-channel-directory--request-reconcile)
                 (lambda (&optional channel-ids)
                   (setq reconciled channel-ids))))
        (disco-channel-directory--handle-gateway-event
         '(:type guild-sync :guild-ids ("g1")))
        (should (equal '("g1") requested))
        (should-not reconciled)))))

(ert-deftest disco-channel-directory-hidden-updates-defer-until-displayed ()
  (disco-channel-directory-test--with-guild
    (let ((displayed nil)
          reconciled)
      (cl-letf (((symbol-function 'disco-channel-directory--displayed-p)
                 (lambda () displayed))
                ((symbol-function 'disco-channel-directory--reconcile)
                 (lambda (&optional channel-ids)
                   (push channel-ids reconciled))))
        (disco-channel-directory--request-reconcile '("c1" "c2"))
        (disco-channel-directory--request-reconcile '("c2"))
        (should disco-channel-directory--deferred-reconcile-p)
        (should-not reconciled)
        (setq displayed t)
        (disco-channel-directory--request-reconcile)
        (should-not disco-channel-directory--deferred-reconcile-p)
        (should-not disco-channel-directory--deferred-channel-ids)
        (should (equal '(("c1" "c2")) reconciled))))))

(ert-deftest disco-channel-directory-thread-rows-use-parent-thread-layout ()
  (with-temp-buffer
    (let ((entry
           (disco-channel-directory-entry-create
            :key "channel:t1"
            :type 'channel
            :channel '((id . "t1") (type . 11) (name . "post"))
            :indent 4))
          captured)
      (cl-letf (((symbol-function 'disco-root--insert-activity-channel-line)
                 (lambda (channel indent scope width)
                   (setq captured (list channel indent scope width)))))
        (disco-channel-directory--insert-channel entry)
        (should (equal 'parent-thread (nth 2 captured)))))))

(ert-deftest disco-channel-directory-finds-equal-nonidentical-channel-id ()
  (with-temp-buffer
    (let* ((property-id (copy-sequence "123456789012345678"))
           (lookup-id (copy-sequence "123456789012345678")))
      (insert "  channel\n")
      (add-text-properties
       (point-min) (point-max) (list 'disco-channel-id property-id))
      (should-not (eq property-id lookup-id))
      (should (= (point-min)
                 (disco-channel-directory--find-channel-position lookup-id))))))

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
