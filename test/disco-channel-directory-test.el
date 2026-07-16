;;; disco-channel-directory-test.el --- Guild directory tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'disco-channel-directory)
(require 'disco-root)

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
     (plist-get (appkit-directory-entry-properties entry)
                'disco-channel-directory-group-id))
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
   (disco-channel-directory--entry-key-for-group group-id)
   expanded-p))

(defun disco-channel-directory-test--set-parent-expanded (parent-id expanded-p)
  "Set PARENT-ID expansion to EXPANDED-P in the current directory."
  (appkit-directory-set-fold-expanded
   (appkit-directory-surface) (list 'thread-parent parent-id) expanded-p))

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

(ert-deftest disco-channel-directory-preserves-native-category-prefixes ()
  (disco-channel-directory-test--with-guild
    (let ((surface (appkit-directory-surface)))
      (appkit-directory-reconcile
       surface
       (list (disco-channel-directory--group-entry "cat" "Work" 0 t)))
      (should
       (equal " ▾ Work"
              (disco-channel-directory-test--line-string-at (point-min))))
      (appkit-directory-reconcile
       surface
       (list (disco-channel-directory--group-entry "cat" "Work" 0 nil))
       :force-keys
       (list (disco-channel-directory--entry-key-for-group "cat")))
      (should
       (equal " ▸ Work"
              (disco-channel-directory-test--line-string-at (point-min)))))))

(ert-deftest disco-channel-directory-preserves-native-note-indentation ()
  (disco-channel-directory-test--with-guild
    (appkit-directory-reconcile
     (appkit-directory-surface)
     (list (disco-channel-directory--note-entry 'status "Loading channels…")))
    (should
     (equal "  Loading channels…"
            (disco-channel-directory-test--line-string-at (point-min))))))

(ert-deftest disco-channel-directory-rows-are-not-text-buttons ()
  (disco-channel-directory-test--with-guild
    (appkit-directory-reconcile
     (appkit-directory-surface)
     (disco-channel-directory--project-loaded-entries))
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

(ert-deftest disco-channel-directory-ret-advances-from-passive-rows ()
  (disco-channel-directory-test--with-guild
    (let* ((note-key (disco-channel-directory--entry-key-for-note 'status))
           (spacer-key "test:spacer")
           (channel-key
            (disco-channel-directory--entry-key-for-channel "c1")))
      (appkit-directory-reconcile
       (appkit-directory-surface)
       (list
        (disco-channel-directory--note-entry 'status "Loading channels…")
        (appkit-directory-entry-create :key spacer-key :role 'spacer)
        (disco-channel-directory--channel-entry
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
                   (disco-channel-directory--project-loaded-entries))))
      (disco-state-upsert-channel
       '((id . "t2") (guild_id . "g1") (parent_id . "c1")
         (name . "newly opened thread") (type . 11)
         (message_count . 119) (member_count . 4)))
      (should
       (equal before
              (mapcar #'appkit-directory-entry-key
                      (disco-channel-directory--project-loaded-entries)))))))

(ert-deftest disco-channel-directory-keyed-reconcile-preserves-unrelated-nodes ()
  (disco-channel-directory-test--with-guild
    (disco-channel-directory--reconcile)
    (let* ((c1-key (disco-channel-directory--entry-key-for-channel "c1"))
           (cat-key (disco-channel-directory--entry-key-for-group "cat"))
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
      (should (= 3 (car (appkit-directory-entry-stamp
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
                  (car (disco-channel-directory-test--entry-types
                        (list forum-entry)))))
      (should-not (appkit-directory-entry-expanded-p forum-entry))
      (should-not
       (member "active"
               (mapcar #'disco-channel-directory-test--entry-id collapsed)))
      (disco-channel-directory-test--set-parent-expanded "forum" t)
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
    (disco-channel-directory-test--set-parent-expanded "forum" t)
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
    (disco-channel-directory-test--set-parent-expanded "forum" t)
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
                   (setq loaded parent-id))))
        (appkit-directory-reconcile
         (appkit-directory-surface)
         (list
          (disco-channel-directory--thread-parent-entry
           (disco-state-channel "forum") 2)))
        (disco-channel-directory-toggle-thread-parent "forum")
        (should (equal "forum" loaded))
        (should
         (appkit-directory-fold-expanded-p
          (appkit-directory-surface) '(thread-parent "forum") nil))
        (appkit-directory-reconcile
         (appkit-directory-surface)
         (list
          (disco-channel-directory--thread-parent-entry
           (disco-state-channel "forum") 2)))
        (disco-channel-directory-toggle-thread-parent "forum")
        (should-not
         (appkit-directory-fold-expanded-p
          (appkit-directory-surface) '(thread-parent "forum") nil))))))

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
         (equal '("channel:c1" "channel:c2")
                (sort (copy-sequence forced) #'string-lessp)))))))

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
          (should (equal '("channel:c1")
                         (appkit-invalidations-entry-keys pending))))
        (disco-channel-directory--handle-gateway-event
         '(:type channel-create :guild-id "g1" :channel-id "c2") view)
        (let ((pending (appkit-invalidations-take
                        (appkit-view-invalidations view))))
          (should (appkit-invalidations-structure-p pending))
          (should (equal '("channel:c2")
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
           (equal '("channel:c2" "channel:t1")
                  (sort (copy-sequence
                         (appkit-invalidations-entry-keys pending))
                        #'string-lessp))))
        (should (= 3 (length requests)))
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
         (equal '(("channel:c1" "channel:c2")) reconciled))))))

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

(ert-deftest disco-channel-directory-thread-rows-use-parent-thread-layout ()
  (with-temp-buffer
    (let ((entry
           (appkit-directory-entry-create
            :key "channel:t1"
            :role 'item
            :payload '((id . "t1") (type . 11) (name . "post"))
            :indent 4))
          captured)
      (cl-letf (((symbol-function 'disco-root--insert-activity-channel-line)
                 (lambda (channel indent scope width)
                   (setq captured (list channel indent scope width)))))
        (disco-channel-directory--insert-channel nil entry)
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
