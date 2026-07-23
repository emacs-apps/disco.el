;;; disco-channel-directory-test.el --- Guild directory tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'disco-channel-directory)
(require 'disco-root)

(defun disco-channel-directory-test--context ()
  "Return the shared guild projector context for the current test buffer."
  (disco-channel-directory--projection-context))

(defun disco-channel-directory-test--project-loaded ()
  "Return the shared loaded projection for the current test buffer."
  (disco-guild-directory-project-loaded
   (disco-channel-directory-test--context)))

(defun disco-channel-directory-test--channel-key (channel-id)
  "Return the current test context's shared key for CHANNEL-ID."
  (disco-guild-directory-channel-key
   (disco-channel-directory-test--context) channel-id))

(defun disco-channel-directory-test--key< (left right)
  "Return non-nil when opaque directory key LEFT sorts before RIGHT."
  (string-lessp (prin1-to-string left) (prin1-to-string right)))

(defun disco-channel-directory-test--entry-types (entries)
  "Return type symbols from directory ENTRIES."
  (mapcar
   (lambda (entry)
     (pcase (appkit-directory-entry-role entry)
       ('group 'group)
       ('note 'note)
       ('item (if (appkit-directory-entry-foldable-p entry)
                  'thread-parent
                'channel))
       (role role)))
   entries))

(defun disco-channel-directory-test--entry-id (entry)
  "Return semantic ID represented by directory ENTRY."
  (pcase (appkit-directory-entry-role entry)
    ('group
     (disco-guild-directory-entry-group-id entry))
    ('item
     (alist-get 'id (appkit-directory-entry-payload entry)))
    (_ nil)))

(defun disco-channel-directory-test--node-table ()
  "Return the current Appkit directory node table."
  (appkit-directory-surface-node-table (appkit-directory-surface)))

(defun disco-channel-directory-test--entry-position (key)
  "Return the rendered row position for directory entry KEY."
  (when-let* ((node (gethash key
                            (disco-channel-directory-test--node-table))))
    (ewoc-location node)))

(defun disco-channel-directory-test--line-string-at (position)
  "Return the physical row containing POSITION, without its newline."
  (save-excursion
    (goto-char position)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun disco-channel-directory-test--set-group-expanded (group-id expanded-p)
  "Set GROUP-ID expansion to EXPANDED-P in the current directory."
  (appkit-directory-set-fold-expanded
   (appkit-directory-surface)
   (disco-guild-directory-group-key
    (disco-channel-directory-test--context) group-id)
   expanded-p))

(defun disco-channel-directory-test--set-parent-expanded (parent-id expanded-p)
  "Set PARENT-ID expansion to EXPANDED-P in the current directory."
  (appkit-directory-set-fold-expanded
   (appkit-directory-surface)
   (disco-guild-directory-thread-parent-key
    (disco-channel-directory-test--context) parent-id)
   expanded-p))

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
       (cl-letf (((symbol-function 'disco-state-channel-viewable-p)
                  (lambda (_channel _unknown-value) t)))
         ,@body))))

(defmacro disco-channel-directory-test--with-appkit-guild (&rest body)
  "Evaluate BODY with `view' bound to a live guild-directory Appkit view."
  (declare (indent 0) (debug t))
  `(let ((disco-runtime--app nil))
     (unwind-protect
         (disco-channel-directory-test--with-guild
           (let ((view (disco-channel-directory--ensure-view)))
             ,@body))
       (when (appkit-app-p disco-runtime--app)
         (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
           (appkit-stop-app disco-runtime--app))))))

(ert-deftest disco-channel-directory-projects-ordinary-channel-threads ()
  (disco-channel-directory-test--with-guild
    (let* ((entries (disco-channel-directory-test--project-loaded))
           (ids (mapcar #'disco-channel-directory-test--entry-id entries))
           (parent
            (seq-find
             (lambda (entry)
               (equal "c2"
                      (disco-channel-directory-test--entry-id entry)))
             entries)))
      (should
       (equal '(group thread-parent group thread-parent)
              (disco-channel-directory-test--entry-types entries)))
      (should
       (equal (list disco-guild-directory-uncategorized-group
                    "c1" "cat" "c2")
              ids))
      (should (eq 'item (appkit-directory-entry-primary-action parent)))
      (should-not (member "t1" ids)))
    (puthash "c2"
             '(:status loaded :thread-ids ("t1") :total 1)
             disco-directory--parent-thread-state)
    (disco-channel-directory-test--set-parent-expanded "c2" t)
    (let ((expanded
           (disco-channel-directory-test--project-loaded)))
      (should
       (equal (list disco-guild-directory-uncategorized-group
                    "c1" "cat" "c2" "t1")
              (mapcar #'disco-channel-directory-test--entry-id expanded)))
      (should
       (equal '(group thread-parent group thread-parent channel)
              (disco-channel-directory-test--entry-types expanded))))))

(ert-deftest disco-guild-directory-preserves-position-order-after-grouping ()
  (disco-channel-directory-test--with-guild
    (dolist (channel
             '(((id . "u1") (guild_id . "g1") (name . "one")
                (type . 0) (position . 1))
               ((id . "u2") (guild_id . "g1") (name . "two")
                (type . 0) (position . 2))
               ((id . "cc0") (guild_id . "g1") (parent_id . "cat")
                (name . "zero") (type . 0) (position . 0))
               ((id . "cc1") (guild_id . "g1") (parent_id . "cat")
                (name . "one") (type . 0) (position . 1))))
      (disco-state-upsert-channel channel))
    (should
     (equal
      (list disco-guild-directory-uncategorized-group
            "c1" "u1" "u2" "cat" "cc0" "cc1" "c2")
      (mapcar #'disco-channel-directory-test--entry-id
              (disco-channel-directory-test--project-loaded))))))

(ert-deftest disco-guild-directory-keys-and-folds-are-occurrence-scoped ()
  (disco-channel-directory-test--with-guild
    (let* ((surface (appkit-directory-surface))
           (standalone (disco-channel-directory-test--context))
           (root
            (disco-guild-directory-context-create
             :guild-id "g1"
             :surface surface
             :namespace '(root guild g1)
             :section-key '(root-guild g1)
             :group-indent 4 :channel-indent 6 :thread-indent 8))
           (standalone-key
            (disco-guild-directory-group-key standalone "cat"))
           (root-key (disco-guild-directory-group-key root "cat")))
      (should-not (equal standalone-key root-key))
      (should (equal '(guild "g1") (nth 1 standalone-key)))
      (should (equal '(root guild g1) (nth 1 root-key)))
      (should (equal "g1" (nth 2 standalone-key)))
      (should (equal "g1" (nth 2 root-key)))
      (appkit-directory-set-fold-expanded surface standalone-key nil)
      (let ((standalone-group
             (seq-find
              (lambda (entry)
                (equal "cat"
                       (disco-guild-directory-entry-group-id entry)))
              (disco-guild-directory-project-loaded standalone)))
            (root-group
             (seq-find
              (lambda (entry)
                (equal "cat"
                       (disco-guild-directory-entry-group-id entry)))
              (disco-guild-directory-project-loaded root))))
        (should-not (appkit-directory-entry-expanded-p standalone-group))
        (should (appkit-directory-entry-expanded-p root-group))))))

(ert-deftest disco-guild-directory-projects-public-row-semantics ()
  (disco-channel-directory-test--with-guild
    (should (equal "Work"
                   (disco-guild-directory-channel-name
                    (disco-state-channel "cat"))))
    (should (disco-guild-directory-category-p
             (disco-state-channel "cat")))
    (should-not (disco-guild-directory-category-p
                 (disco-state-channel "c2")))
    (should (disco-guild-directory-displayable-channel-p
             (disco-state-channel "c2")))
    (let* ((context
            (disco-guild-directory-context-create
             :guild-id "g1"
             :surface (appkit-directory-surface)
             :namespace '(root guild g1)
             :section-key '(root-guild g1)
             :group-indent 4 :channel-indent 6 :thread-indent 8))
           (entries (disco-guild-directory-project-loaded context))
           (category
            (seq-find
             (lambda (entry)
               (equal "cat"
                      (disco-guild-directory-entry-group-id entry)))
             entries))
           (channel
            (seq-find
             (lambda (entry)
               (equal "c2" (alist-get 'id
                                      (appkit-directory-entry-payload entry))))
             entries)))
      (dolist (entry entries)
        (should (equal "g1"
                       (disco-guild-directory-entry-guild-id entry)))
        (should (memq (disco-guild-directory-entry-row-kind entry)
                      '(group channel thread-parent note)))
        (let ((key (appkit-directory-entry-key entry)))
          (should (eq 'disco-guild-directory (car key)))
          (should (equal '(root guild g1) (nth 1 key)))
          (should (equal "g1" (nth 2 key)))))
      (should (equal '(root-guild g1)
                     (appkit-directory-entry-section-key category)))
      (should (= 4 (appkit-directory-entry-indent category)))
      (should (= 6 (appkit-directory-entry-indent channel)))
      (should (eq (disco-state-channel "c2")
                  (appkit-directory-entry-payload channel)))
      (should
       (equal
        "RET opens channel; TAB/t toggles active threads; g refreshes; A opens archived threads"
        (appkit-directory-entry-help-echo channel)))
      (should (appkit-directory-entry-foldable-p channel))
      (should (eq 'item (appkit-directory-entry-primary-action channel)))
      (should (equal "cat"
                     (disco-guild-directory-entry-group-id channel))))))

(ert-deftest disco-channel-directory-shell-builds-shared-context ()
  (disco-channel-directory-test--with-guild
    (let (captured)
      (cl-letf (((symbol-function 'disco-guild-directory-project)
                 (lambda (context)
                   (setq captured context)
                   nil)))
        (should-not (disco-channel-directory--project-entries))
        (should (equal "g1"
                       (disco-guild-directory-context-guild-id captured)))
        (should (eq (appkit-directory-surface)
                    (disco-guild-directory-context-surface captured)))
        (should (equal '(guild "g1")
                       (disco-guild-directory-context-namespace captured)))
        (should (= 1
                   (disco-guild-directory-context-group-indent captured)))
        (should (= 2
                   (disco-guild-directory-context-channel-indent captured)))
        (should (= 4
                   (disco-guild-directory-context-thread-indent captured)))))))

(ert-deftest disco-channel-directory-preserves-native-category-prefixes ()
  (disco-channel-directory-test--with-guild
    (let ((surface (appkit-directory-surface)))
      (appkit-directory-reconcile
       surface
       (list
        (disco-guild-directory--group-entry
         (disco-channel-directory-test--context) "cat" "Work" 0 t)))
      (should
       (equal " ▾ Work"
              (disco-channel-directory-test--line-string-at (point-min))))
      (appkit-directory-reconcile
       surface
       (list
        (disco-guild-directory--group-entry
         (disco-channel-directory-test--context) "cat" "Work" 0 nil))
       :force-keys
       (list
        (disco-guild-directory-group-key
         (disco-channel-directory-test--context) "cat")))
      (should
       (equal " ▸ Work"
              (disco-channel-directory-test--line-string-at (point-min)))))))

(ert-deftest disco-channel-directory-preserves-native-note-indentation ()
  (disco-channel-directory-test--with-guild
    (appkit-directory-reconcile
     (appkit-directory-surface)
     (list
      (disco-guild-directory--note-entry
       (disco-channel-directory-test--context)
       'status "Loading channels…")))
    (should
     (equal "  Loading channels…"
            (disco-channel-directory-test--line-string-at (point-min))))))

(ert-deftest disco-channel-directory-rows-are-not-text-buttons ()
  (disco-channel-directory-test--with-guild
    (appkit-directory-reconcile
     (appkit-directory-surface)
     (disco-channel-directory-test--project-loaded))
    (let ((position (point-min)))
      (while (< position (point-max))
        (should-not (button-at position))
        (should-not (eq (get-text-property position 'mouse-face) 'highlight))
        (setq position (1+ position))))))

(ert-deftest disco-channel-directory-mode-map-preserves-native-commands ()
  (dolist (binding
           '(("RET" . disco-channel-directory-open-at-point)
             ("TAB" . disco-channel-directory-tab-dwim)
             ("n" . disco-channel-directory-next-channel)
             ("p" . disco-channel-directory-previous-channel)
             ("u" . disco-channel-directory-next-unread)))
    (should (eq (lookup-key disco-channel-directory-mode-map
                            (kbd (car binding)))
                (cdr binding))))
  (should (eq (lookup-key disco-channel-directory-mode-map [mouse-1])
              #'disco-channel-directory-mouse-open-at-point)))

(ert-deftest disco-channel-directory-ordinary-parent-ret-opens-tab-expands ()
  (disco-channel-directory-test--with-guild
    (let (opened loaded)
      (cl-letf (((symbol-function 'disco-root--open-channel)
                 (lambda (channel-id)
                   (setq opened channel-id)))
                ((symbol-function
                  'disco-directory-load-parent-threads-async)
                 (lambda (parent-id &rest _arguments)
                   (setq loaded parent-id))))
        (disco-channel-directory--reconcile)
        (goto-char
         (disco-channel-directory-test--entry-position
          (disco-channel-directory--entry-key-for-channel "c2")))
        (disco-channel-directory-open-at-point)
        (should (equal "c2" opened))
        (should-not loaded)
        (should-not
         (appkit-directory-fold-expanded-p
          (appkit-directory-surface)
          (disco-channel-directory--entry-key-for-thread-parent "c2")
          nil))
        (disco-channel-directory-tab-dwim)
        (should (equal "c2" loaded))
        (should
         (appkit-directory-fold-expanded-p
          (appkit-directory-surface)
          (disco-channel-directory--entry-key-for-thread-parent "c2")
          nil))))))

(ert-deftest disco-channel-directory-ret-advances-from-passive-rows ()
  (disco-channel-directory-test--with-guild
    (let* ((note-key
            (disco-guild-directory-note-key
             (disco-channel-directory-test--context) 'status))
           (spacer-key "test:spacer")
           (channel-key
            (disco-channel-directory--entry-key-for-channel "c1")))
      (appkit-directory-reconcile
       (appkit-directory-surface)
       (list
        (disco-guild-directory--note-entry
         (disco-channel-directory-test--context)
         'status "Loading channels…")
        (appkit-directory-entry-create :key spacer-key :role 'spacer)
        (disco-guild-directory--channel-entry
         (disco-channel-directory-test--context)
         (disco-state-channel "c1") 2)))
      (dolist (passive-key (list note-key spacer-key))
        (goto-char
         (or (disco-channel-directory-test--entry-position passive-key)
             (ert-fail (format "Missing passive row %S" passive-key))))
        (disco-channel-directory-open-at-point)
        (should
         (= (point)
            (disco-channel-directory-test--entry-position channel-key)))))))

(ert-deftest disco-channel-directory-ordinary-thread-upsert-keeps-projection ()
  (disco-channel-directory-test--with-guild
    (let ((before
           (mapcar #'appkit-directory-entry-key
                   (disco-channel-directory-test--project-loaded))))
      (disco-state-upsert-channel
       '((id . "t2") (guild_id . "g1") (parent_id . "c1")
         (name . "newly opened thread") (type . 11)
         (message_count . 119) (member_count . 4)))
      (should
       (equal before
              (mapcar #'appkit-directory-entry-key
                      (disco-channel-directory-test--project-loaded)))))))

(ert-deftest disco-channel-directory-keyed-reconcile-preserves-unrelated-nodes ()
  (disco-channel-directory-test--with-guild
    (disco-channel-directory--reconcile)
    (let* ((c1-key (disco-channel-directory--entry-key-for-channel "c1"))
           (cat-key
            (disco-guild-directory-group-key
             (disco-channel-directory-test--context) "cat"))
           (c1-node (gethash c1-key
                             (disco-channel-directory-test--node-table))))
      (should c1-node)
      (should (gethash cat-key
                       (disco-channel-directory-test--node-table)))
      (disco-channel-directory-test--set-group-expanded "cat" nil)
      (disco-channel-directory--reconcile)
      (should (eq c1-node
                  (gethash c1-key
                           (disco-channel-directory-test--node-table))))
      (should-not
       (gethash (disco-channel-directory--entry-key-for-channel "c2")
                (disco-channel-directory-test--node-table)))
      (should-not
       (gethash (disco-channel-directory--entry-key-for-channel "t1")
                (disco-channel-directory-test--node-table))))))

(ert-deftest disco-channel-directory-live-row-update-retains-node-identity ()
  (disco-channel-directory-test--with-guild
    (disco-channel-directory--reconcile)
    (let* ((key (disco-channel-directory--entry-key-for-channel "c1"))
           (node (gethash key
                          (disco-channel-directory-test--node-table))))
      (disco-state-set-channel-unread "c1" 3)
      (disco-channel-directory--reconcile '("c1"))
      (should (eq node
                  (gethash key
                           (disco-channel-directory-test--node-table))))
      (should (= 3 (caar (appkit-directory-entry-stamp
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

(ert-deftest disco-channel-directory-state-reset-queues-structural-sync ()
  (disco-channel-directory-test--with-appkit-guild
    (let (updates)
      (cl-letf (((symbol-function
                  'disco-channel-directory--queue-view-update)
                 (lambda (target &rest args)
                   (push (cons target args) updates))))
        (disco-channel-directory--handle-state-reset)
        (should (= 1 (length updates)))
        (should (eq view (caar updates)))
        (should (plist-get (cdar updates) :structure))))))

(ert-deftest disco-channel-directory-filter-reveals-matching-channel-path ()
  (disco-channel-directory-test--with-guild
    (disco-channel-directory-test--set-group-expanded "cat" nil)
    (setq disco-channel-directory--filter "projects")
    (let ((ids
           (mapcar
            #'disco-channel-directory-test--entry-id
            (disco-channel-directory-test--project-loaded))))
      (should (equal '("cat" "c2") ids)))))

(ert-deftest disco-channel-directory-category-filter-reveals-category-children ()
  (disco-channel-directory-test--with-guild
    (setq disco-channel-directory--filter "work")
    (let ((ids
           (mapcar
            #'disco-channel-directory-test--entry-id
            (disco-channel-directory-test--project-loaded))))
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
    (puthash "forum"
             '(:status loaded :thread-ids ("active" "archived") :total 2)
             disco-directory--parent-thread-state)
    (let* ((collapsed (disco-channel-directory-test--project-loaded))
           (forum-entry
            (seq-find
             (lambda (entry)
               (equal "forum"
                      (disco-channel-directory-test--entry-id entry)))
             collapsed)))
      (should (eq 'thread-parent
                  (car (disco-channel-directory-test--entry-types
                        (list forum-entry)))))
      (should-not (appkit-directory-entry-expanded-p forum-entry))
      (should-not
       (member "active"
               (mapcar #'disco-channel-directory-test--entry-id collapsed)))
      (disco-channel-directory-test--set-parent-expanded "forum" t)
      (let ((expanded-ids
             (mapcar #'disco-channel-directory-test--entry-id
                     (disco-channel-directory-test--project-loaded))))
        (should (member "active" expanded-ids))
        (should-not (member "archived" expanded-ids))))))

(ert-deftest disco-channel-directory-forum-shows-posts-without-starters ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "ideas") (type . 15)))
    (disco-state-upsert-channel
     '((id . "post") (guild_id . "g1") (parent_id . "forum")
       (name . "proposal") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (puthash "forum"
             '(:status loaded :thread-ids ("post") :total 1)
             disco-directory--parent-thread-state)
    (disco-channel-directory-test--set-parent-expanded "forum" t)
    (let ((ids
           (mapcar #'disco-channel-directory-test--entry-id
                   (disco-channel-directory-test--project-loaded))))
      (should (member "post" ids)))
    (disco-state-upsert-message
     "post" '((id . "post") (channel_id . "post") (content . "starter")))
    (let ((ids
           (mapcar #'disco-channel-directory-test--entry-id
                   (disco-channel-directory-test--project-loaded))))
      (should (member "post" ids)))))

(ert-deftest disco-channel-directory-forum-summary-and-children-share-snapshot ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "support") (type . 15)))
    ;; A stale global thread must not leak into either the parent count or its
    ;; expanded children after the endpoint confirmed an empty snapshot.
    (disco-state-upsert-channel
     '((id . "stale") (guild_id . "g1") (parent_id . "forum")
       (name . "stale post") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (puthash "forum"
             '(:status loaded :thread-ids nil :total 0)
             disco-directory--parent-thread-state)
    (disco-channel-directory-test--set-parent-expanded "forum" t)
    (let* ((entries (disco-channel-directory-test--project-loaded))
           (note
            (seq-find
             (lambda (entry)
               (eq 'note (appkit-directory-entry-role entry)))
             entries)))
      (should (equal "No active posts."
                     (appkit-directory-entry-label note)))
      (should-not
       (seq-find
        (lambda (entry)
          (equal "stale" (disco-channel-directory-test--entry-id entry)))
        entries)))))

(ert-deftest disco-channel-directory-forum-partial-page-offers-load-more-row ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "support") (type . 15)))
    (disco-state-upsert-channel
     '((id . "post") (guild_id . "g1") (parent_id . "forum")
       (name . "question") (type . 11)
       (thread_metadata . ((archived . :false)))))
    (disco-state-upsert-message
     "post" '((id . "post") (channel_id . "post") (content . "starter")))
    (puthash "forum"
             '(:status loaded :thread-ids ("post")
               :total 545 :next-cursor "cursor")
             disco-directory--parent-thread-state)
    (disco-channel-directory-test--set-parent-expanded "forum" t)
    (let* ((entries (disco-channel-directory-test--project-loaded))
           (parent
            (seq-find
             (lambda (entry)
               (eq 'thread-parent
                   (disco-guild-directory-entry-row-kind entry)))
             entries))
           (action
            (seq-find
             (lambda (entry)
               (eq 'parent-threads-load-more
                   (disco-guild-directory-entry-row-kind entry)))
             entries)))
      (should action)
      (should (equal "Load more active posts (1/545)"
                     (appkit-directory-entry-label action)))
      (should
       (< (cl-position-if
           (lambda (entry)
             (equal "post" (disco-channel-directory-test--entry-id entry)))
           entries)
          (cl-position action entries :test #'eq))))))

(ert-deftest disco-channel-directory-unloaded-forum-offers-load-action ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "support") (type . 15)))
    (disco-channel-directory-test--set-parent-expanded "forum" t)
    (let* ((entries (disco-channel-directory-test--project-loaded))
           (action
            (seq-find
             (lambda (entry)
               (eq 'parent-threads-load
                   (disco-guild-directory-entry-row-kind entry)))
             entries)))
      (should action)
      (should (equal "Load active posts"
                     (appkit-directory-entry-label action)))
      ;; Control rows carry only their parent property, never a channel
      ;; payload that generic occurrence/open code could misinterpret.
      (should-not (appkit-directory-entry-payload action))
      (should-not
       (seq-find
        (lambda (entry)
          (and (eq 'note (appkit-directory-entry-role entry))
               (equal "Active posts are not loaded."
                      (appkit-directory-entry-label entry))))
        entries)))))

(ert-deftest disco-guild-directory-parent-action-key-is-stable ()
  (disco-channel-directory-test--with-guild
    (let* ((context (disco-channel-directory-test--context))
           (parent '((id . "forum") (guild_id . "g1") (type . 15)))
           (keys
            (mapcar
             (lambda (action)
               (appkit-directory-entry-key
                (disco-guild-directory--parent-action-entry
                 context parent action (symbol-name action))))
             '(parent-threads-load
               parent-threads-load-more
               parent-threads-retry))))
      (should (equal (car keys) (cadr keys)))
      (should (equal (cadr keys) (caddr keys))))))

(ert-deftest disco-channel-directory-incomplete-forum-respects-lenses ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "support") (type . 15)))
    (dolist (lens '((:filter "does-not-match") (:unread t)))
      (setq disco-channel-directory--filter (plist-get lens :filter)
            disco-channel-directory--unread-only (plist-get lens :unread))
      (let ((entries (disco-channel-directory-test--project-loaded)))
        (should-not
         (seq-find
          (lambda (entry)
            (eq 'thread-parent
                (disco-guild-directory-entry-row-kind entry)))
          entries))
        (should-not
         (seq-find
          (lambda (entry)
            (eq 'parent-threads-load
                (disco-guild-directory-entry-row-kind entry)))
          entries))))))

(ert-deftest disco-channel-directory-unread-forum-does-not-require-page-load ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-gateway-channel
     '((id . "forum") (guild_id . "g1") (parent_id . "cat")
       (name . "support") (type . 15)))
    (disco-state-upsert-gateway-channel
     '((id . "post") (guild_id . "g1") (parent_id . "forum")
       (name . "question") (type . 11)))
    (disco-state-set-channel-unread "post" 3)
    (setq disco-channel-directory--unread-only t)
    (let ((entries (disco-channel-directory-test--project-loaded)))
      (should
       (seq-find
        (lambda (entry)
          (and (eq 'thread-parent
                   (disco-guild-directory-entry-row-kind entry))
               (equal "forum"
                      (disco-channel-directory-test--entry-id entry))))
        entries)))))

(ert-deftest disco-channel-directory-parent-page-actions-dispatch-exactly ()
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
        (disco-channel-directory--activate-item
         nil
         (appkit-directory-entry-create
          :key kind :role 'item :section-key 'section :item-p t
          :payload parent
          :properties
          (list disco-guild-directory-row-kind-property kind
                disco-guild-directory-thread-parent-id-property "forum"))))
      (should (equal '((retry "forum") (more "forum") (load "forum"))
                     calls)))))

(ert-deftest disco-channel-directory-opens-snapshot-thread-with-scoped-proof ()
  (let* ((thread
          '((id . "post") (guild_id . "g1") (parent_id . "forum")
            (type . 11)))
         (entry
          (appkit-directory-entry-create
           :key 'post :role 'item :section-key 'section :item-p t
           :payload thread
           :properties
           (list disco-guild-directory-row-kind-property 'channel
                 disco-guild-directory-thread-parent-id-property "forum")))
         opened)
    (cl-letf (((symbol-function
                'disco-directory-parent-thread-viewable-p)
               (lambda (parent-id channel)
                 (and (equal parent-id "forum") (eq channel thread))))
              ((symbol-function 'disco-root--open-channel)
               (lambda (channel-id) (setq opened channel-id))))
      (disco-channel-directory--activate-item nil entry)
      (should (equal "post" opened)))))

(ert-deftest disco-channel-directory-refuses-inaccessible-forum-open ()
  (disco-state-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-channel
         '((id . "forum") (guild_id . "g1") (type . 15)))
        (cl-letf (((symbol-function 'disco-channel-directory-open)
                   (lambda (&rest _args)
                     (ert-fail "inaccessible forum opened a directory"))))
          (should-error
           (disco-channel-directory-open-thread-parent "forum")
           :type 'user-error)))
    (disco-state-reset)))

(ert-deftest disco-guild-directory-active-snapshot-excludes-denied-threads ()
  (disco-state-reset)
  (disco-directory-reset)
  (unwind-protect
      (progn
        (disco-state-upsert-gateway-channel
         '((id . "forum") (guild_id . "g1") (type . 15)))
        (disco-state-upsert-channel
         '((id . "visible") (guild_id . "g1") (parent_id . "forum")
           (type . 11)))
        (disco-state-upsert-channel
         '((id . "hidden") (guild_id . "g1") (parent_id . "forum")
           (type . 11)))
        (disco-state-upsert-gateway-channel
         `((id . "hidden") (guild_id . "g1") (parent_id . "forum")
           (type . 11) (flags . ,disco-channel-flag-obfuscated)))
        (puthash "forum"
                 '(:status loaded :thread-ids ("visible" "hidden")
                   :total 2)
                 disco-directory--parent-thread-state)
        (should
         (equal '("visible")
                (mapcar
                 (lambda (thread) (alist-get 'id thread))
                (disco-guild-directory--active-threads "forum")))))
    (disco-directory-reset)
    (disco-state-reset)))

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
                   (disco-channel-directory-test--project-loaded))))
      (should-not (member "active" ids))
      (should-not (member "archived" ids))
      (should-not (memq :orphan-threads ids)))))

(ert-deftest disco-channel-directory-toggle-forum-loads-on-first-expansion ()
  (disco-channel-directory-test--with-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (name . "ideas") (type . 15)))
    (let (loaded (loads 0))
      (cl-letf (((symbol-function
                  'disco-directory-load-parent-threads-async)
                 (lambda (parent-id &rest _args)
                   (cl-incf loads)
                   (setq loaded parent-id))))
        (appkit-directory-reconcile
         (appkit-directory-surface)
         (list
          (disco-guild-directory--thread-parent-entry
           (disco-channel-directory-test--context)
           (disco-state-channel "forum") 2)))
        (disco-channel-directory-toggle-thread-parent "forum")
        (should (equal "forum" loaded))
        (should (= 1 loads))
        (should
         (appkit-directory-fold-expanded-p
          (appkit-directory-surface)
          (disco-guild-directory-thread-parent-key
           (disco-channel-directory-test--context) "forum") nil))
        (appkit-directory-reconcile
         (appkit-directory-surface)
         (list
          (disco-guild-directory--thread-parent-entry
           (disco-channel-directory-test--context)
           (disco-state-channel "forum") 2)))
        (disco-channel-directory-toggle-thread-parent "forum")
        (should (= 1 loads))
        (should-not
         (appkit-directory-fold-expanded-p
          (appkit-directory-surface)
          (disco-guild-directory-thread-parent-key
           (disco-channel-directory-test--context) "forum") nil))))))

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
                            disco-guild-directory-thread-parent-id-property)
                        "forum")))
                ((symbol-function 'message) #'ignore))
        (disco-channel-directory-refresh)
        (should (equal '("forum" :force t) request))))))

(ert-deftest disco-channel-directory-event-does-not-start-parent-request ()
  (disco-channel-directory-test--with-appkit-guild
    (disco-state-upsert-channel
     '((id . "forum") (guild_id . "g1") (name . "support") (type . 15)))
    (disco-channel-directory-test--set-parent-expanded "forum" t)
    (let ((loads 0))
      (cl-letf (((symbol-function
                  'disco-directory-load-parent-threads-async)
                 (lambda (parent-id &rest _args)
                   (ignore parent-id)
                   (cl-incf loads))))
        (disco-channel-directory--handle-directory-event
         '(:type parent-threads-loaded
           :guild-id "g1" :parent-id "forum")
         view)
        (should (= 0 loads))))))

(ert-deftest disco-channel-directory-waits-for-resolved-channel-permissions ()
  (disco-state-reset)
  (disco-directory-reset)
  (disco-state-set-guilds '(((id . "g1") (name . "Guild One"))))
  (disco-state-seed-guild-channels
   "g1"
   `(((id . "visible") (guild_id . "g1") (name . "visible")
      (type . 0) (flags . 0))
     ((id . "hidden") (guild_id . "g1") (name . "hidden") (type . 0)
      (flags . ,disco-channel-flag-obfuscated))))
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
                         (disco-channel-directory-test--project-loaded))))
            (should
             (equal (list disco-guild-directory-uncategorized-group
                          "visible")
                    before))
            (disco-state-put-channels "g1" (copy-tree resolved))
            (should
             (equal before
                    (mapcar
                     #'disco-channel-directory-test--entry-id
                     (disco-channel-directory-test--project-loaded)))))))
    (disco-state-reset)
    (disco-directory-reset)))

(ert-deftest disco-channel-directory-gateway-hidden-overrides-rest-visibility ()
  (disco-state-reset)
  (disco-directory-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-channel-directory-mode)
        (setq disco-channel-directory--guild-id "g1")
        (disco-state-set-guilds '(((id . "g1") (name . "Guild One"))))
        (disco-state-put-channels
         "g1"
         '(((id . "c1") (guild_id . "g1") (name . "general")
            (type . 0) (permissions . "1024"))))
        (should
         (member "c1"
                 (mapcar #'disco-channel-directory-test--entry-id
                         (disco-channel-directory-test--project-loaded))))
        ;; A full Gateway channel update is newer than the cached REST
        ;; permission snapshot.  The carried `permissions' field must not make
        ;; this obfuscated channel visible again.
        (disco-state-upsert-gateway-channel
         `((id . "c1") (guild_id . "g1") (name . "general")
           (type . 0) (flags . ,disco-channel-flag-obfuscated)))
        (should-not
         (member "c1"
                 (mapcar #'disco-channel-directory-test--entry-id
                         (disco-channel-directory-test--project-loaded)))))
    (disco-state-reset)
    (disco-directory-reset)))

(ert-deftest disco-channel-directory-rehydrates-provisional-gateway-sync ()
  (disco-channel-directory-test--with-appkit-guild
    (disco-state-seed-guild-channels
     "g1" (disco-state-guild-channels "g1"))
    (let ((requests 0)
          requested)
      (cl-letf (((symbol-function 'disco-directory-load-guild-async)
                 (lambda (guild-id &rest args)
                   (cl-incf requests)
                   (setq requested (cons guild-id args))))
                ((symbol-function 'disco-channel-directory--displayed-p)
                 (lambda () nil)))
        (disco-channel-directory--handle-gateway-event
         '(:type guild-sync :guild-ids ("g1")) view)
        (should-not requested)
        (appkit-sync-invalidations view)
        (should (equal '("g1") requested))
        (should (= 1 requests))
        (should disco-channel-directory--deferred-reconcile-p)
        (disco-channel-directory--handle-directory-event
         '(:type guild-error :guild-id "g1") view)
        (appkit-sync-invalidations view)
        (should (= 1 requests))))))

(ert-deftest disco-channel-directory-gateway-events-coalesce-one-sync ()
  (disco-channel-directory-test--with-appkit-guild
    (let ((reconciles 0)
          forced
          first-handle)
      (cl-letf (((symbol-function 'disco-channel-directory--displayed-p)
                 (lambda () t))
                ((symbol-function 'disco-channel-directory--reconcile)
                 (lambda (_channel-ids entry-keys)
                   (cl-incf reconciles)
                   (setq forced entry-keys))))
        (disco-channel-directory--handle-gateway-event
         '(:type message-create :guild-id "g1" :channel-id "c1") view)
        (setq first-handle
              (appkit-invalidations-scheduled-handle
               (appkit-view-invalidations view)))
        (disco-channel-directory--handle-gateway-event
         '(:type message-update :guild-id "g1" :channel-id "c2") view)
        (should (eq first-handle
                    (appkit-invalidations-scheduled-handle
                     (appkit-view-invalidations view))))
        (should (zerop reconciles))
        (appkit-sync-invalidations view)
        (should (= 1 reconciles))
        (should
         (equal (mapcar #'disco-channel-directory-test--channel-key
                        '("c1" "c2"))
                (sort (copy-sequence forced)
                      #'disco-channel-directory-test--key<)))))))

(ert-deftest disco-channel-directory-last-messages-refreshes-returned-thread-row ()
  (disco-channel-directory-test--with-appkit-guild
    (disco-channel-directory--handle-gateway-event
     '(:type last-messages :guild-id "g1" :channel-ids ("t1"))
     view)
    (let ((pending
           (appkit-invalidations-take
            (appkit-view-invalidations view))))
      (should-not (appkit-invalidations-structure-p pending))
      (should
       (equal (list (disco-channel-directory-test--channel-key "t1"))
              (appkit-invalidations-entry-keys pending))))))

(ert-deftest disco-channel-directory-callbacks-target-or-structure-only ()
  (disco-channel-directory-test--with-appkit-guild
    (let (requests
          (mutations 0))
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (candidate &rest options)
                   (push (cons candidate options) requests)
                   (apply #'appkit-invalidate candidate options)))
                ((symbol-function 'appkit-schedule-sync)
                 (lambda (&rest _args)
                   (ert-fail "directory callback used bare scheduling")))
                ((symbol-function 'disco-channel-directory--reconcile)
                 (lambda (&rest _args) (cl-incf mutations)))
                ((symbol-function 'force-window-update)
                 (lambda (&rest _args) (cl-incf mutations))))
        (disco-channel-directory--handle-gateway-event
         '(:type message-update :guild-id "g1" :channel-id "c1") view)
        (let ((pending (appkit-invalidations-take
                        (appkit-view-invalidations view))))
          (should-not (appkit-invalidations-structure-p pending))
          (should (equal (list
                          (disco-channel-directory-test--channel-key "c1"))
                         (appkit-invalidations-entry-keys pending))))
        (disco-channel-directory--handle-gateway-event
         '(:type channel-create :guild-id "g1" :channel-id "c2") view)
        (let ((pending (appkit-invalidations-take
                        (appkit-view-invalidations view))))
          (should (appkit-invalidations-structure-p pending))
          (should (equal (list
                          (disco-channel-directory-test--channel-key "c2"))
                         (appkit-invalidations-entry-keys pending)))
          (should
           (equal '((guild-channel-snapshot "g1"))
                  (appkit-invalidations-resource-keys pending))))
        (disco-channel-directory--handle-gateway-event
         '(:type channel-sync :guild-id "g1"
           :channels (((id . "c3") (guild_id . "g1"))))
         view)
        (let ((pending (appkit-invalidations-take
                        (appkit-view-invalidations view))))
          (should (appkit-invalidations-structure-p pending))
          (should
           (equal (list
                   (disco-channel-directory-test--channel-key "c3"))
                  (appkit-invalidations-entry-keys pending)))
          (should
           (equal '((guild-channel-snapshot "g1"))
                  (appkit-invalidations-resource-keys pending))))
        (disco-channel-directory--handle-directory-event
         '(:type parent-threads-loaded :guild-id "g1"
           :parent-id "c2" :channel-id "t1")
         view)
        (let ((pending (appkit-invalidations-take
                        (appkit-view-invalidations view))))
          (should (appkit-invalidations-structure-p pending))
          (should
           (equal (mapcar #'disco-channel-directory-test--channel-key
                          '("c2" "t1"))
                  (sort (copy-sequence
                         (appkit-invalidations-entry-keys pending))
                        #'disco-channel-directory-test--key<))))
        (should (= 4 (length requests)))
        (should (seq-every-p (lambda (request) (eq view (car request)))
                             requests))
        (should (zerop mutations))))))

(ert-deftest disco-channel-directory-dead-view-callbacks-are-inert ()
  (disco-channel-directory-test--with-appkit-guild
    (appkit-kill-view view)
    (let ((updates 0))
      (cl-letf (((symbol-function
                  'disco-channel-directory--queue-view-update)
                 (lambda (&rest _args) (cl-incf updates))))
        (disco-channel-directory--handle-gateway-event
         '(:type channel-create :guild-id "g1" :channel-id "c1") view)
        (disco-channel-directory--handle-directory-event
         '(:type guild-loaded :guild-id "g1") view)
        (should (zerop updates))))))

(ert-deftest disco-channel-directory-reconcile-never-implicitly-reattaches-view ()
  (with-temp-buffer
    (disco-channel-directory-mode)
    (cl-letf (((symbol-function 'disco-channel-directory--ensure-view)
               (lambda () (ert-fail "reconcile must not attach a view"))))
      (should-not (disco-channel-directory--request-reconcile nil t t))
      (setq-local disco-channel-directory--fill-column nil)
      (disco-channel-directory--reflow-to-width 80)
      (should (= 80 disco-channel-directory--fill-column)))))

(ert-deftest disco-channel-directory-hidden-sync-defers-until-visible ()
  (disco-channel-directory-test--with-appkit-guild
    (let ((displayed nil)
          reconciled)
      (cl-letf (((symbol-function 'disco-channel-directory--displayed-p)
                 (lambda () displayed))
                ((symbol-function 'disco-channel-directory--reconcile)
                 (lambda (_channel-ids entry-keys)
                   (push entry-keys reconciled))))
        (disco-channel-directory--queue-view-update
         view :channel-ids '("c1" "c2"))
        (appkit-sync-invalidations view)
        (should disco-channel-directory--deferred-reconcile-p)
        (should-not reconciled)
        (setq displayed t)
        (disco-channel-directory--schedule-deferred-sync view)
        (appkit-sync-invalidations view)
        (should-not disco-channel-directory--deferred-reconcile-p)
        (should-not disco-channel-directory--deferred-entry-keys)
        (should
         (equal (list
                 (mapcar #'disco-channel-directory-test--channel-key
                         '("c1" "c2")))
                reconciled))))))

(ert-deftest disco-channel-directory-visible-sync-does-not-force-redisplay ()
  (disco-channel-directory-test--with-appkit-guild
    (let ((forced 0))
      (cl-letf (((symbol-function 'disco-channel-directory--displayed-p)
                 (lambda () t))
                ((symbol-function 'force-window-update)
                 (lambda (&rest _args) (cl-incf forced))))
        (disco-channel-directory--queue-view-update view :structure t)
        (appkit-sync-invalidations view)
        (should (zerop forced))))))

(ert-deftest disco-channel-directory-thread-rows-use-parent-capability-scope ()
  (disco-state-reset)
  (unwind-protect
      (with-temp-buffer
        (disco-state-upsert-channel
         '((id . "text") (guild_id . "g1") (type . 0)))
        (disco-state-upsert-channel
         '((id . "forum") (guild_id . "g1") (type . 15)))
        (let ((entries
               (list
                (appkit-directory-entry-create
                 :key "channel:text-thread"
                 :role 'item
                 :payload '((id . "text-thread") (type . 11)
                            (parent_id . "text") (name . "discussion"))
                 :indent 4)
                (appkit-directory-entry-create
                 :key "channel:forum-post"
                 :role 'item
                 :payload '((id . "forum-post") (type . 11)
                            (parent_id . "forum") (name . "post"))
                 :indent 4)))
              captured)
          (cl-letf (((symbol-function 'disco-root--insert-activity-channel-line)
                     (lambda (channel indent scope width)
                       (push (list (alist-get 'id channel)
                                   indent scope width)
                             captured))))
            (dolist (entry entries)
              (disco-channel-directory--insert-item nil entry))
            (should
             (equal '(("text-thread" 0 timeline-thread nil)
                      ("forum-post" 0 thread-post nil))
                    (nreverse captured))))))
    (disco-state-reset)))

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
  (let ((disco-runtime--app nil)
        loaded displayed order)
    (cl-letf (((symbol-function 'disco-channel-directory--attach-live-updates)
               (lambda () (push 'attach order)))
              ((symbol-function 'disco-channel-directory--reflow-to-width)
               (lambda (_width) (push 'reflow order)))
              ((symbol-function 'disco-channel-directory--request-reconcile)
               (lambda (&rest _args) (push 'request order)))
              ((symbol-function 'disco-directory-load-guild-async)
               (lambda (guild-id &rest _args)
                 (push 'load order)
                 (setq loaded guild-id)))
              ((symbol-function 'appkit-sync-invalidations)
               (lambda (_view) (push 'sync order)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer &rest _args)
                 (push 'pop order)
                 (setq displayed buffer)))
              ((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (progn
            (disco-channel-directory-open "g1")
            (should (equal "g1" loaded))
            (should (buffer-live-p displayed))
            (should (equal '(pop attach reflow request load sync)
                           (nreverse order)))
            (with-current-buffer displayed
              (let ((view (appkit-current-view)))
                (should (appkit-view-live-p view))
                (should (equal '(channel-directory "g1")
                               (appkit-view-id view)))
                (should (equal "g1" (appkit-view-state view)))
                (should (equal "g1"
                               disco-channel-directory--guild-id)))))
        (when (buffer-live-p displayed)
          (kill-buffer displayed))
        (when (appkit-app-p disco-runtime--app)
          (appkit-stop-app disco-runtime--app))))))

(provide 'disco-channel-directory-test)

;;; disco-channel-directory-test.el ends here
