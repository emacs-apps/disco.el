;;; disco-channel-directory.el --- Per-guild channel directory -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; A persistent, EWOC-backed channel directory for one Discord guild.  The
;; global root remains a compact account navigator; opening a guild creates a
;; dedicated buffer whose categories can be expanded independently.  Guild
;; channel snapshots are hydrated lazily by `disco-directory'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-invalidation)
(require 'appkit-transaction)
(require 'appkit-view)
(require 'disco-channel-type)
(require 'disco-customize)
(require 'disco-directory)
(require 'disco-gateway)
(require 'disco-guild-directory)
(require 'disco-msg)
(require 'disco-permission)
(require 'disco-preview)
(require 'disco-root-view)
(require 'disco-runtime)
(require 'disco-state)
(require 'disco-thread)

(autoload 'disco-root-open "disco-root" nil t)

(defface disco-channel-directory-filter
  '((t :inherit font-lock-keyword-face :weight semi-bold))
  "Face for an active guild-directory filter."
  :group 'disco)

(defcustom disco-channel-directory-auto-fill-on-window-size-change t
  "When non-nil, reflow visible guild directories after window resizing."
  :type 'boolean
  :group 'disco)

(defcustom disco-channel-directory-margin-columns 1
  "Columns reserved at the right edge of guild-directory rows."
  :type 'integer
  :group 'disco)

(defvar disco-channel-directory--window-size-hook-installed nil
  "Non-nil once the guild-directory resize hook has been installed.")

(defvar-local disco-channel-directory--guild-id nil
  "Guild ID owned by the current channel-directory buffer.")

(defvar-local disco-channel-directory--pending-focus-channel-id nil
  "Channel ID to focus after its guild snapshot becomes renderable.")

(defvar-local disco-channel-directory--filter nil
  "Case-folded channel-name filter, or nil when no filter is active.")

(defvar-local disco-channel-directory--unread-only nil
  "Non-nil when the current directory shows unread channels only.")

(defvar-local disco-channel-directory--fill-column nil
  "Effective width used to render directory rows.")

(defvar-local disco-channel-directory--header-line-cache ""
  "Cached guild-directory header line refreshed during reconciliation.")

(defvar-local disco-channel-directory--gateway-handler nil
  "Buffer-local gateway event handler closure.")

(defvar-local disco-channel-directory--directory-handler nil
  "Buffer-local directory lifecycle event handler closure.")

(defvar-local disco-channel-directory--preview-handler nil
  "Buffer-local preview update handler closure.")

(defvar-local disco-channel-directory--live-update-handle nil
  "Appkit lifecycle handle owning this directory's shared event hooks.")

(defvar-local disco-channel-directory--rendering nil
  "Non-nil while an EWOC reconciliation is active.")

(defvar-local disco-channel-directory--render-pending nil
  "Non-nil when another reconciliation was requested while rendering.")

(defvar-local disco-channel-directory--deferred-reconcile-p nil
  "Non-nil when hidden-buffer updates await a display window.")

(defvar-local disco-channel-directory--deferred-entry-keys nil
  "Stable entry keys accumulated while this directory has no display window.")

(defvar-local disco-channel-directory--deferred-structure-p nil
  "Non-nil when hidden updates require structural reconciliation.")

(defvar-local disco-channel-directory--deferred-position-p nil
  "Non-nil when hidden updates require geometry-sensitive row reflow.")

(defun disco-channel-directory--normalize-id (value)
  "Return VALUE as an ID string, or nil."
  (and value (format "%s" value)))

(defun disco-channel-directory--guild ()
  "Return the guild object owned by the current directory buffer."
  (seq-find
   (lambda (guild)
     (equal (disco-channel-directory--normalize-id (alist-get 'id guild))
            disco-channel-directory--guild-id))
   (disco-state-guilds)))

(defun disco-channel-directory--guild-name ()
  "Return the display name for the current directory guild."
  (or (alist-get 'name (disco-channel-directory--guild))
      disco-channel-directory--guild-id
      "Unknown guild"))

(defun disco-channel-directory--buffer-name (guild-id)
  "Return the stable channel-directory buffer name for GUILD-ID."
  (format "*disco:guild:%s*" guild-id))

(defun disco-channel-directory--view-id (guild-id)
  "Return the Appkit view identity for GUILD-ID."
  (list 'channel-directory
        (or (disco-channel-directory--normalize-id guild-id)
            (error "Disco: channel-directory view requires a guild id"))))

(defun disco-channel-directory--guild-snapshot-resource-key ()
  "Return the Appkit resource key for the current guild channel snapshot."
  (list 'guild-channel-snapshot disco-channel-directory--guild-id))

(defun disco-channel-directory--ensure-view ()
  "Return the live Appkit view owning the current directory buffer."
  (let* ((app (disco-runtime-app))
         (id (disco-channel-directory--view-id
              disco-channel-directory--guild-id))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal id (appkit-view-id current)))
      (setf (appkit-view-state current) disco-channel-directory--guild-id
            (appkit-view-sync-function current)
            #'disco-channel-directory--sync-invalidations
            (appkit-view-parts current) '(frame entries))
      current)
     ((appkit-view-live-p current)
      (error "Disco: channel-directory buffer belongs to another Appkit view"))
     (t
      (appkit-attach-view
       :app app
       :id id
       :state disco-channel-directory--guild-id
       :mode 'disco-channel-directory-mode
       :sync-function #'disco-channel-directory--sync-invalidations
       :parts '(frame entries))))))

(defun disco-channel-directory--projection-context ()
  "Return the shared projector context for the current standalone directory."
  (let ((guild-id
         (or disco-channel-directory--guild-id
             (error "Disco: channel directory has no guild id"))))
    (disco-guild-directory-context-create
     :guild-id guild-id
     :surface (appkit-directory-surface)
     :namespace (list 'guild guild-id)
     :section-key (list 'guild guild-id)
     :group-indent 1
     :channel-indent 2
     :thread-indent 4
     :filter disco-channel-directory--filter
     :unread-only disco-channel-directory--unread-only)))

(defun disco-channel-directory--entry-key-for-channel (channel-id)
  "Return the standalone projector row key for CHANNEL-ID."
  (disco-guild-directory-channel-key
   (disco-channel-directory--projection-context) channel-id))

(defun disco-channel-directory--entry-key-for-thread-parent (parent-id)
  "Return the standalone projector fold key for thread parent PARENT-ID."
  (disco-guild-directory-thread-parent-key
   (disco-channel-directory--projection-context) parent-id))

(defun disco-channel-directory--project-entries ()
  "Project lifecycle state through the shared guild projector."
  (disco-guild-directory-project
   (disco-channel-directory--projection-context)))


(defun disco-channel-directory--usable-width ()
  "Return current usable directory width in columns."
  (or (when-let* ((widths
                   (delq nil
                         (mapcar
                          (lambda (window)
                            (appkit-view-window-fill-column
                             window
                             disco-channel-directory-margin-columns))
                          (get-buffer-window-list
                           (current-buffer) nil t)))))
        (apply #'max widths))
      disco-channel-directory--fill-column
      (max 40 (- (window-width) disco-channel-directory-margin-columns))))

(defun disco-channel-directory--insert-item (_surface entry)
  "Insert one responsive Appkit directory item ENTRY."
  (pcase (disco-guild-directory-entry-row-kind entry)
    ((or 'parent-threads-load
         'parent-threads-load-more
         'parent-threads-retry)
     (insert (or (appkit-directory-entry-label entry) "") "\n"))
    (_
     (let* ((channel (appkit-directory-entry-payload entry))
            (scope (if (disco-state-channel-thread-p channel)
                       (disco-root--thread-directory-scope channel)
                     'directory)))
       (disco-root--insert-activity-channel-line
        channel 0 scope disco-channel-directory--fill-column)))))

(defun disco-channel-directory--activate-item (_surface entry)
  "Activate the channel or pagination action carried by ENTRY."
  (let ((parent-id
         (disco-guild-directory-entry-thread-parent-id entry)))
    (pcase (disco-guild-directory-entry-row-kind entry)
      ('parent-threads-load
       (disco-directory-load-parent-threads-async parent-id))
      ('parent-threads-load-more
       (disco-directory-load-more-parent-threads-async parent-id))
      ('parent-threads-retry
       (disco-directory-retry-parent-threads-async parent-id))
      (_
       (let* ((channel (appkit-directory-entry-payload entry))
              (channel-id (alist-get 'id channel))
              (scoped-thread-p
               (and (disco-state-channel-thread-p channel)
                    parent-id
                    (disco-directory-parent-thread-viewable-p
                     parent-id channel))))
         (when (and (disco-state-channel-thread-p channel)
                    parent-id
                    (not scoped-thread-p))
           (user-error "Disco: thread %s is not viewable below %s"
                       channel-id parent-id))
         (disco-root--open-channel channel-id))))))

(defun disco-channel-directory--fold-changed (_surface entry expanded-p)
  "Apply an Appkit fold change for ENTRY with EXPANDED-P."
  (pcase (disco-guild-directory-entry-row-kind entry)
    ('thread-parent
     (let ((parent-id
            (disco-guild-directory-entry-thread-parent-id entry)))
       (when expanded-p
         (disco-directory-load-parent-threads-async parent-id))
       (disco-channel-directory--invalidate-and-sync (list parent-id) t)))
    ('group
     (disco-channel-directory--invalidate-and-sync nil t))
    (_
     (error "Disco: unsupported guild-directory fold row %S"
            (disco-guild-directory-entry-row-kind entry)))))

(defun disco-channel-directory--apply-entries (entries force-entry-keys)
  "Reconcile Appkit directory ENTRIES, redrawing FORCE-ENTRY-KEYS."
  (appkit-directory-reconcile
   (appkit-directory-surface) entries :force-keys force-entry-keys))

(defun disco-channel-directory--header-line ()
  "Compute header-line text for the current guild directory."
  (let* ((guild-name (disco-channel-directory--guild-name))
         (loaded-p
          (disco-state-guild-channels-loaded-p
           disco-channel-directory--guild-id))
         (channels
          (and loaded-p
               (seq-filter
                (lambda (channel)
                  (and (not (disco-guild-directory-category-p channel))
                       (not (disco-state-channel-thread-p channel))
                       (disco-guild-directory-displayable-channel-p channel)))
                (disco-state-guild-channels
                 disco-channel-directory--guild-id))))
         (unread
          (if channels
              (cl-count-if #'disco-state-channel-has-unread-p channels)
            0))
         (lens
          (string-join
           (delq nil
                 (list
                  (and disco-channel-directory--filter
                       (propertize
                        (format "filter:%s" disco-channel-directory--filter)
                        'face 'disco-channel-directory-filter))
                  (and disco-channel-directory--unread-only
                       (propertize "unread" 'face
                                   'disco-channel-directory-filter))))
           " · ")))
    (concat
     " " (propertize guild-name 'face 'mode-line-emphasis)
     (if loaded-p
         (format "  %d channels · %d unread" (length channels) unread)
       (format "  %s" (disco-directory-guild-status
                        disco-channel-directory--guild-id)))
     (if (string-empty-p lens) "" (concat "  [" lens "]")))))

(defun disco-channel-directory--refresh-header-line ()
  "Refresh the cached guild-directory header line."
  (setq disco-channel-directory--header-line-cache
        (disco-channel-directory--header-line))
  (force-mode-line-update))

(defun disco-channel-directory--reconcile
    (&optional force-channel-ids force-entry-keys)
  "Reconcile the directory, forcing channel IDs and stable entry keys.

FORCE-CHANNEL-IDS is retained for direct interactive callers.
FORCE-ENTRY-KEYS is the native Appkit invalidation representation."
  (if disco-channel-directory--rendering
      (setq disco-channel-directory--render-pending t)
    (let ((disco-channel-directory--rendering t)
          (forced
           (delete-dups
            (append
             (mapcar #'disco-channel-directory--entry-key-for-channel
                     force-channel-ids)
             (copy-sequence force-entry-keys)))))
      (unwind-protect
          (progn
            (setq disco-channel-directory--fill-column
                  (disco-channel-directory--usable-width))
            (disco-channel-directory--apply-entries
             (disco-channel-directory--project-entries)
             forced)
            (when disco-channel-directory--pending-focus-channel-id
              (when-let* ((position
                           (disco-channel-directory--find-channel-position
                            disco-channel-directory--pending-focus-channel-id)))
                (goto-char position)
                (beginning-of-line)
                (setq disco-channel-directory--pending-focus-channel-id nil)))
            (disco-channel-directory--refresh-header-line))
        (setq disco-channel-directory--rendering nil))
      (when disco-channel-directory--render-pending
        (setq disco-channel-directory--render-pending nil)
        (disco-channel-directory--reconcile)))))

(defun disco-channel-directory--displayed-p ()
  "Return non-nil when the current directory has a live display window."
  (window-live-p (get-buffer-window (current-buffer) t)))

(defun disco-channel-directory--all-entry-keys ()
  "Return all stable keys currently represented by this Appkit directory."
  (let (keys)
    (let ((node-table
           (appkit-directory-surface-node-table
            (appkit-directory-surface))))
      (maphash (lambda (key _node) (push key keys))
               node-table))
    keys))

(defun disco-channel-directory--defer-invalidations (invalidations)
  "Retain Appkit INVALIDATIONS until this directory has a display window."
  (setq disco-channel-directory--deferred-reconcile-p t
        disco-channel-directory--deferred-structure-p
        (or disco-channel-directory--deferred-structure-p
            (appkit-invalidations-structure-p invalidations))
        disco-channel-directory--deferred-position-p
        (or disco-channel-directory--deferred-position-p
            (appkit-invalidations-position-p invalidations))
        disco-channel-directory--deferred-entry-keys
        (delete-dups
         (append (appkit-invalidations-entry-keys invalidations)
                 disco-channel-directory--deferred-entry-keys))))

(defun disco-channel-directory--clear-deferred-invalidations ()
  "Forget hidden-buffer invalidations after a successful visible sync."
  (setq disco-channel-directory--deferred-reconcile-p nil
        disco-channel-directory--deferred-entry-keys nil
        disco-channel-directory--deferred-structure-p nil
        disco-channel-directory--deferred-position-p nil))

(defun disco-channel-directory--sync-invalidations (view invalidations)
  "Synchronize VIEW from coalesced Appkit INVALIDATIONS."
  (when (appkit-view-live-p view)
    (let* ((parts (appkit-invalidations-parts invalidations))
           (resources (appkit-invalidations-resource-keys invalidations))
           (structure-p
            (or disco-channel-directory--deferred-structure-p
                (appkit-invalidations-structure-p invalidations)))
           (position-p
            (or disco-channel-directory--deferred-position-p
                (appkit-invalidations-position-p invalidations)))
           (entry-keys
            (delete-dups
             (append (appkit-invalidations-entry-keys invalidations)
                     disco-channel-directory--deferred-entry-keys)))
           (entries-p
            (or disco-channel-directory--deferred-reconcile-p
                structure-p position-p entry-keys (memq 'entries parts)))
           (frame-p (or entries-p (memq 'frame parts))))
      (when (and (member
                  (disco-channel-directory--guild-snapshot-resource-key)
                  resources)
                 (not (disco-state-guild-channels-loaded-p
                       disco-channel-directory--guild-id)))
        (disco-directory-load-guild-async
         disco-channel-directory--guild-id))
      (if (not (disco-channel-directory--displayed-p))
          (when (or entries-p frame-p)
            (disco-channel-directory--defer-invalidations invalidations))
        (when position-p
          (setq entry-keys
                (delete-dups
                 (append (disco-channel-directory--all-entry-keys)
                         entry-keys))))
        (appkit-with-content-update view
          (cond
           (entries-p
            (disco-channel-directory--reconcile nil entry-keys))
           (frame-p
            (disco-channel-directory--refresh-header-line))))
        (disco-channel-directory--clear-deferred-invalidations)))))

(cl-defun disco-channel-directory--queue-view-update
    (view &key channel-ids structure position hydrate)
  "Queue one Appkit update for VIEW.

CHANNEL-IDS identify exact rows.  STRUCTURE marks membership or ordering
changes.  POSITION requests a geometry-sensitive reflow.  HYDRATE marks the
guild channel snapshot as a resource that sync may need to resolve."
  (when (appkit-view-live-p view)
    (appkit-with-live-view view
      (let ((entry-keys
             (delete-dups
              (delq nil
                    (mapcar
                     (lambda (channel-id)
                       (when-let* ((id
                                    (disco-channel-directory--normalize-id
                                     channel-id)))
                         (disco-channel-directory--entry-key-for-channel id)))
                     channel-ids)))))
        (appkit-request-sync
         view
         :structure structure
         :parts '(frame entries)
         :entries entry-keys
         :resource
         (and hydrate
              (disco-channel-directory--guild-snapshot-resource-key))
         :position position)
        t))))

(defun disco-channel-directory--request-reconcile
    (&optional force-channel-ids structure-p position-p view)
  "Queue an Appkit reconciliation for FORCE-CHANNEL-IDS.

STRUCTURE-P marks membership or ordering changes.  POSITION-P marks geometry
changes.  VIEW defaults to the Appkit view attached to the current buffer."
  (when-let* ((view (or view (appkit-current-view))))
    (when (appkit-view-live-p view)
      (disco-channel-directory--queue-view-update
       view
       :channel-ids force-channel-ids
       :structure structure-p
       :position position-p))))

(defun disco-channel-directory--invalidate-and-sync
    (&optional force-channel-ids structure-p position-p)
  "Immediately sync explicit directory changes through the current Appkit view.

FORCE-CHANNEL-IDS, STRUCTURE-P, and POSITION-P describe the invalidation."
  (when (disco-channel-directory--request-reconcile
         force-channel-ids structure-p position-p)
    (appkit-sync-invalidations (appkit-current-view))))

(defun disco-channel-directory--schedule-deferred-sync (view)
  "Schedule VIEW to consume invalidations deferred while it was hidden."
  (when (and disco-channel-directory--deferred-reconcile-p
             (appkit-view-live-p view))
    (appkit-request-sync
     view
     :structure disco-channel-directory--deferred-structure-p
     :parts '(frame entries)
     :entries disco-channel-directory--deferred-entry-keys
     :position disco-channel-directory--deferred-position-p)
    t))

(defun disco-channel-directory--window-buffer-change (window)
  "Flush deferred directory updates when WINDOW displays the current buffer."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (disco-channel-directory--reflow-to-width
     (disco-channel-directory--usable-width))
    (when-let* ((view (appkit-current-view)))
      (disco-channel-directory--schedule-deferred-sync view))))

(defun disco-channel-directory--line-property (property &optional position)
  "Return PROPERTY from row at POSITION or point."
  (let ((position (or position (point))))
    (or (get-text-property position property)
        (get-text-property (line-beginning-position) property))))

(defun disco-channel-directory--find-channel-position (channel-id)
  "Return the first row position whose channel ID equals CHANNEL-ID."
  (let ((position (point-min))
        found)
    (while (and (< position (point-max)) (not found))
      (when (equal (get-text-property position 'disco-channel-id)
                   channel-id)
        (setq found position))
      (unless found
        (setq position
              (next-single-property-change
               position 'disco-channel-id nil (point-max)))))
    found))

(defun disco-channel-directory--move-channel (step)
  "Move STEP channel rows from the current line."
  (unless
      (appkit-directory-move
       (appkit-directory-surface)
       #'appkit-directory-entry-item-p
       (if (> step 0) 1 -1))
    (message "Disco: no %s channel"
             (if (> step 0) "next" "previous"))))

(defun disco-channel-directory-next-channel ()
  "Move to the next channel row."
  (interactive)
  (disco-channel-directory--move-channel 1))

(defun disco-channel-directory-previous-channel ()
  "Move to the previous channel row."
  (interactive)
  (disco-channel-directory--move-channel -1))

(defun disco-channel-directory-next-unread ()
  "Move to the next unread channel row, wrapping once."
  (interactive)
  (unless
      (appkit-directory-move
       (appkit-directory-surface)
       (lambda (entry)
         (and (appkit-directory-entry-item-p entry)
              (appkit-directory-entry-unread-p entry)))
       1 t)
    (message "Disco: no unread channels in this guild")))

(defun disco-channel-directory-toggle-group ()
  "Toggle the category/group row at point."
  (interactive)
  (let ((entry (appkit-directory-entry-at-point)))
    (unless (and entry (eq (appkit-directory-entry-role entry) 'group))
      (user-error "Disco: point is not on a category"))
    (appkit-directory-toggle-entry-fold
     (appkit-directory-surface) entry)))

(defun disco-channel-directory-toggle-thread-parent (&optional parent-id)
  "Toggle inline active threads under PARENT-ID or the row at point."
  (interactive)
  (setq parent-id
        (disco-channel-directory--normalize-id
         (or parent-id
             (disco-channel-directory--line-property
              disco-guild-directory-thread-parent-id-property))))
  (let ((parent (and parent-id (disco-state-channel parent-id))))
    (unless (and parent (disco-channel-thread-parent-p parent))
      (user-error "Disco: point is not on a thread parent channel"))
    (let ((entry
           (appkit-directory-entry-for-key
            (appkit-directory-surface)
            (disco-channel-directory--entry-key-for-channel parent-id))))
      (unless (and entry (appkit-directory-entry-foldable-p entry))
        (user-error "Disco: thread parent channel is not visible"))
      (appkit-directory-toggle-entry-fold
       (appkit-directory-surface) entry))))

(defun disco-channel-directory-toggle-at-point ()
  "Toggle the category or thread parent at point."
  (interactive)
  (let ((entry (appkit-directory-entry-at-point)))
    (unless (and entry (appkit-directory-entry-foldable-p entry))
      (user-error "Disco: point is not on a foldable row"))
    (appkit-directory-toggle-entry-fold
     (appkit-directory-surface) entry)))

(defun disco-channel-directory-open-at-point ()
  "Run the row's primary action or advance from a passive row."
  (interactive)
  (let ((entry (appkit-directory-entry-at-point)))
    (if (and entry
             (or (appkit-directory-entry-foldable-p entry)
                 (appkit-directory-entry-item-p entry)))
        (appkit-directory-activate-entry
         (appkit-directory-surface) entry)
      (disco-channel-directory-next-channel))))

(defun disco-channel-directory-tab-dwim ()
  "Toggle a foldable row at point, otherwise move to the next channel."
  (interactive)
  (let ((entry (appkit-directory-entry-at-point)))
    (if (and entry (appkit-directory-entry-foldable-p entry))
        (appkit-directory-toggle-entry-fold
         (appkit-directory-surface) entry)
      (disco-channel-directory-next-channel))))

(defun disco-channel-directory-mouse-open-at-point (event)
  "Open the directory row selected by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (disco-channel-directory-open-at-point))

(defun disco-channel-directory-set-filter (filter)
  "Set the current directory text FILTER."
  (interactive
   (list (read-string "Channel filter: " disco-channel-directory--filter)))
  (setq disco-channel-directory--filter
        (when-let* ((value (string-trim (or filter ""))))
          (unless (string-empty-p value)
            (downcase value))))
  (disco-channel-directory--invalidate-and-sync nil t))

(defun disco-channel-directory-clear-filter ()
  "Clear all active directory lenses."
  (interactive)
  (setq disco-channel-directory--filter nil
        disco-channel-directory--unread-only nil)
  (disco-channel-directory--invalidate-and-sync nil t))

(defun disco-channel-directory-toggle-unread-only ()
  "Toggle the current directory's unread-only lens."
  (interactive)
  (setq disco-channel-directory--unread-only
        (not disco-channel-directory--unread-only))
  (disco-channel-directory--invalidate-and-sync nil t)
  (message "Disco: guild unread lens %s"
           (if disco-channel-directory--unread-only "enabled" "disabled")))

(defun disco-channel-directory-refresh ()
  "Refresh the thread parent at point, or the guild channel snapshot."
  (interactive)
  (if-let* ((parent-id
             (disco-channel-directory--line-property
              disco-guild-directory-thread-parent-id-property)))
      (progn
        (appkit-directory-set-fold-expanded
         (appkit-directory-surface)
         (disco-channel-directory--entry-key-for-thread-parent parent-id) t)
        (disco-directory-load-parent-threads-async parent-id :force t)
        (let* ((parent (disco-state-channel parent-id))
               (children
                (disco-guild-directory--thread-child-noun parent t)))
          (message "Disco: refreshing active %s in %s…"
                   children
                   (disco-guild-directory-channel-name parent))))
    (unless (disco-channel-directory--guild)
      (user-error "Disco: this guild is no longer available"))
    (disco-directory-load-guild-async
     disco-channel-directory--guild-id :force t)
    (message "Disco: refreshing %s channels…"
             (disco-channel-directory--guild-name))))

(defun disco-channel-directory--archived-parent-at-point ()
  "Return a thread parent channel resolved from the current row."
  (let* ((parent-id
          (or (disco-channel-directory--line-property
               disco-guild-directory-thread-parent-id-property)
              (when-let* ((channel-id
                           (disco-channel-directory--line-property
                            'disco-channel-id))
                          (channel (disco-state-channel channel-id)))
                (and (disco-state-channel-thread-p channel)
                     (alist-get 'parent_id channel)))))
         (parent (and parent-id (disco-state-channel parent-id))))
    (and parent (disco-channel-thread-parent-p parent) parent)))

(defun disco-channel-directory-open-archived-at-point ()
  "Open paginated archived threads for the parent represented at point."
  (interactive)
  (let ((parent (disco-channel-directory--archived-parent-at-point)))
    (unless parent
      (user-error "Disco: point has no thread parent context"))
    (disco-root-list-archived-threads (alist-get 'id parent))))

(defun disco-channel-directory-open-root ()
  "Return to the global disco root buffer."
  (interactive)
  (disco-root-open))

(defun disco-channel-directory--event-relevant-p (event)
  "Return non-nil when gateway EVENT affects the current guild."
  (member disco-channel-directory--guild-id
          (disco-gateway-event-guild-ids event)))

(defconst disco-channel-directory--structural-gateway-events
  '(guild-create guild-update guild-delete guild-sync
    channel-create channel-update channel-delete channel-sync
    user-guild-settings-update
    thread-create thread-update thread-delete thread-list-sync)
  "Gateway event types that may change directory membership or ordering.")

(defun disco-channel-directory--handle-gateway-event (event &optional view)
  "Queue precise Appkit invalidations for a relevant gateway EVENT.

VIEW defaults to the current buffer's Appkit view."
  (let ((view (or view (appkit-current-view))))
    (when (appkit-view-live-p view)
      (appkit-with-live-view view
        (when (disco-channel-directory--event-relevant-p event)
          (let* ((type (plist-get event :type))
                 (channel-ids (disco-gateway-event-channel-ids event))
                 (structure-p
                  (or (memq type
                            disco-channel-directory--structural-gateway-events)
                      (null channel-ids))))
            (disco-channel-directory--queue-view-update
             view
             :channel-ids channel-ids
             :structure structure-p
             :hydrate (memq type '(guild-sync guild-create guild-update
                                   channel-create channel-update channel-sync)))))))))

(defun disco-channel-directory--handle-directory-event (event &optional view)
  "Queue Appkit invalidations for one relevant directory lifecycle EVENT.

VIEW defaults to the current buffer's Appkit view."
  (let ((view (or view (appkit-current-view))))
    (when (appkit-view-live-p view)
      (appkit-with-live-view view
        (let ((type (plist-get event :type))
              (guild-id
               (disco-channel-directory--normalize-id
                (plist-get event :guild-id))))
          (when (or (eq type 'index-loaded)
                    (and guild-id
                         (equal guild-id disco-channel-directory--guild-id)))
            (disco-channel-directory--queue-view-update
             view
             :channel-ids
             (delq nil
                   (list (plist-get event :parent-id)
                         (plist-get event :channel-id)))
             :structure t)))))))

(defun disco-channel-directory--handle-preview-update (channel-id &optional view)
  "Queue an exact row invalidation for preview CHANNEL-ID in this guild.

VIEW defaults to the current buffer's Appkit view."
  (let ((view (or view (appkit-current-view))))
    (when (appkit-view-live-p view)
      (appkit-with-live-view view
        (when-let* ((channel-id
                     (disco-channel-directory--normalize-id channel-id))
                    (channel (disco-state-channel channel-id))
                    (guild-id
                     (disco-channel-directory--normalize-id
                      (alist-get 'guild_id channel))))
          (when (equal guild-id disco-channel-directory--guild-id)
            (disco-channel-directory--queue-view-update
             view :channel-ids (list channel-id))))))))

(defun disco-channel-directory--remove-live-updates ()
  "Remove this buffer's shared event hooks without touching Appkit ownership."
  (let ((watched-p (functionp disco-channel-directory--gateway-handler)))
    (when disco-channel-directory--gateway-handler
      (remove-hook 'disco-gateway-event-hook
                   disco-channel-directory--gateway-handler))
    (when disco-channel-directory--directory-handler
      (remove-hook 'disco-directory-event-hook
                   disco-channel-directory--directory-handler))
    (when disco-channel-directory--preview-handler
      (remove-hook 'disco-preview-update-hook
                   disco-channel-directory--preview-handler))
    (setq disco-channel-directory--gateway-handler nil
          disco-channel-directory--directory-handler nil
          disco-channel-directory--preview-handler nil
          disco-channel-directory--live-update-handle nil)
    (when watched-p
      (disco-gateway-unwatch-global))))

(defun disco-channel-directory--detach-live-updates ()
  "Detach the current directory from shared update streams."
  (let ((handle disco-channel-directory--live-update-handle))
    (setq disco-channel-directory--live-update-handle nil)
    (if (and (appkit-handle-p handle)
             (appkit-handle-alive-p handle))
        (appkit-cancel-handle handle)
      (disco-channel-directory--remove-live-updates))))

(defun disco-channel-directory--handle-state-reset ()
  "Queue live directory views after canonical state is reset."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq major-mode 'disco-channel-directory-mode)
          (when-let* ((view (appkit-current-view)))
            (disco-channel-directory--queue-view-update
             view :structure t)))))))

(add-hook 'disco-state-reset-hook
          #'disco-channel-directory--handle-state-reset)

(defun disco-channel-directory--attach-live-updates ()
  "Attach the current Appkit view to gateway, directory, and preview events."
  (let ((view (disco-channel-directory--ensure-view)))
    (disco-channel-directory--detach-live-updates)
    (let ((buffer (current-buffer)))
      (setq disco-channel-directory--gateway-handler
            (lambda (event)
              (disco-channel-directory--handle-gateway-event event view)))
      (setq disco-channel-directory--directory-handler
            (lambda (event)
              (disco-channel-directory--handle-directory-event event view)))
      (setq disco-channel-directory--preview-handler
            (lambda (channel-id)
              (disco-channel-directory--handle-preview-update
               channel-id view)))
      (condition-case err
          (progn
            (add-hook 'disco-gateway-event-hook
                      disco-channel-directory--gateway-handler)
            (add-hook 'disco-directory-event-hook
                      disco-channel-directory--directory-handler)
            (add-hook 'disco-preview-update-hook
                      disco-channel-directory--preview-handler)
            (disco-gateway-watch-global)
            (setq disco-channel-directory--live-update-handle
                  (appkit-register-handle
                   view 'function
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (disco-channel-directory--remove-live-updates)))))))
        (error
         (disco-channel-directory--remove-live-updates)
         (signal (car err) (cdr err)))))
    view))

(defun disco-channel-directory--reflow-to-width (width)
  "Queue a position-preserving directory reflow when WIDTH changed."
  (when (and (integerp width)
             (> width 0)
             (/= width (or disco-channel-directory--fill-column 0)))
    (setq disco-channel-directory--fill-column width)
    (disco-channel-directory--request-reconcile nil nil t)))

(defun disco-channel-directory--window-size-change (frame)
  "Reflow guild-directory buffers visible on FRAME."
  (when disco-channel-directory-auto-fill-on-window-size-change
    (let ((widths (make-hash-table :test #'eq)))
      (walk-windows
       (lambda (window)
         (let ((buffer (window-buffer window)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (eq major-mode 'disco-channel-directory-mode)
                 (let ((width
                        (appkit-view-window-fill-column
                         window disco-channel-directory-margin-columns)))
                   (when width
                     (puthash buffer
                              (max width (or (gethash buffer widths) 0))
                              widths))))))))
       nil frame)
      (maphash
       (lambda (buffer width)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (disco-channel-directory--reflow-to-width width))))
       widths))))

(defun disco-channel-directory--ensure-window-size-hook ()
  "Install the global guild-directory window-size hook once."
  (unless disco-channel-directory--window-size-hook-installed
    (add-hook 'window-size-change-functions
              #'disco-channel-directory--window-size-change)
    (setq disco-channel-directory--window-size-hook-installed t)))

(defvar disco-channel-directory-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'disco-channel-directory-refresh)
    (define-key map (kbd "/") #'disco-channel-directory-set-filter)
    (define-key map (kbd "C-c C-k") #'disco-channel-directory-clear-filter)
    (define-key map (kbd "U") #'disco-channel-directory-toggle-unread-only)
    (define-key map (kbd "RET") #'disco-channel-directory-open-at-point)
    (define-key map (kbd "TAB") #'disco-channel-directory-tab-dwim)
    (define-key map (kbd "<backtab>")
      #'disco-channel-directory-previous-channel)
    (define-key map (kbd "t") #'disco-channel-directory-toggle-at-point)
    (define-key map (kbd "A")
      #'disco-channel-directory-open-archived-at-point)
    (define-key map (kbd "n") #'disco-channel-directory-next-channel)
    (define-key map (kbd "p") #'disco-channel-directory-previous-channel)
    (define-key map (kbd "u") #'disco-channel-directory-next-unread)
    (define-key map (kbd "b") #'disco-channel-directory-open-root)
    (define-key map [mouse-1]
      #'disco-channel-directory-mouse-open-at-point)
    map)
  "Keymap for `disco-channel-directory-mode'.")

(define-derived-mode disco-channel-directory-mode special-mode
  "Disco-Directory"
  "Major mode for one guild's channel directory."
  (setq buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local disco-channel-directory--pending-focus-channel-id nil)
  (setq-local disco-channel-directory--filter nil)
  (setq-local disco-channel-directory--unread-only nil)
  (setq-local disco-channel-directory--fill-column nil)
  (setq-local disco-channel-directory--header-line-cache "")
  (setq-local disco-channel-directory--rendering nil)
  (setq-local disco-channel-directory--render-pending nil)
  (setq-local disco-channel-directory--live-update-handle nil)
  (setq-local disco-channel-directory--deferred-reconcile-p nil)
  (setq-local disco-channel-directory--deferred-entry-keys nil)
  (setq-local disco-channel-directory--deferred-structure-p nil)
  (setq-local disco-channel-directory--deferred-position-p nil)
  (setq-local header-line-format
              'disco-channel-directory--header-line-cache)
  (setq-local revert-buffer-function
              (lambda (&rest _ignored)
                (disco-channel-directory-refresh)))
  (appkit-directory-initialize)
  (appkit-directory-configure
   (appkit-directory-surface)
   :item-inserter #'disco-channel-directory--insert-item
   :activate-function #'disco-channel-directory--activate-item
   :fold-function #'disco-channel-directory--fold-changed)
  (disco-channel-directory--ensure-window-size-hook)
  (add-hook 'window-buffer-change-functions
            #'disco-channel-directory--window-buffer-change nil t)
  (add-hook 'kill-buffer-hook
            #'disco-channel-directory--detach-live-updates nil t))

;;;###autoload
(defun disco-channel-directory-open (guild-id)
  "Open the lazy channel directory for GUILD-ID."
  (interactive
   (let* ((guilds (disco-state-guilds))
          (choices
           (mapcar
            (lambda (guild)
              (cons (format "%s (%s)"
                            (or (alist-get 'name guild) "Unnamed guild")
                            (alist-get 'id guild))
                    (disco-channel-directory--normalize-id
                     (alist-get 'id guild))))
            guilds))
          (choice (completing-read "Guild: " choices nil t)))
     (list (cdr (assoc choice choices)))))
  (setq guild-id (disco-channel-directory--normalize-id guild-id))
  (unless (and guild-id
               (seq-some
                (lambda (guild)
                  (equal guild-id
                         (disco-channel-directory--normalize-id
                          (alist-get 'id guild))))
                (disco-state-guilds)))
    (user-error "Disco: unknown guild %s" guild-id))
  (let* ((app (disco-runtime-app))
         (view-id (disco-channel-directory--view-id guild-id))
         (fresh-p (null (appkit-view-for-id app view-id)))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode 'disco-channel-directory-mode
           :buffer-name (disco-channel-directory--buffer-name guild-id)
           :state guild-id
           :sync-function #'disco-channel-directory--sync-invalidations
           :parts '(frame entries)
           :setup
           (lambda (_view)
             (setq-local disco-channel-directory--guild-id guild-id))
           :select t))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq disco-channel-directory--guild-id guild-id)
      (disco-channel-directory--attach-live-updates)
      (disco-channel-directory--reflow-to-width
       (disco-channel-directory--usable-width))
      (disco-channel-directory--request-reconcile nil t nil view)
      (disco-directory-load-guild-async guild-id)
      (when fresh-p
        (appkit-sync-invalidations view)))
    buffer))

;;;###autoload
(defun disco-channel-directory-open-thread-parent (parent-channel-id)
  "Open PARENT-CHANNEL-ID inline in its guild channel directory."
  (let* ((parent-id
          (disco-channel-directory--normalize-id parent-channel-id))
         (parent (and parent-id (disco-state-channel parent-id)))
         (guild-id (and parent (alist-get 'guild_id parent))))
    (unless (and parent (disco-channel-thread-parent-p parent))
      (user-error "Disco: channel %s is not a thread parent"
                  parent-channel-id))
    (unless (disco-state-channel-viewable-p parent nil)
      (user-error "Disco: channel %s is not viewable" parent-id))
    (unless guild-id
      (user-error "Disco: channel %s has no guild context" parent-id))
    (let ((buffer (disco-channel-directory-open guild-id)))
      (with-current-buffer buffer
        (appkit-directory-set-fold-expanded
         (appkit-directory-surface)
         (disco-channel-directory--entry-key-for-thread-parent parent-id) t)
        (setq disco-channel-directory--pending-focus-channel-id parent-id)
        (disco-channel-directory--invalidate-and-sync (list parent-id) t)
        (disco-directory-load-parent-threads-async parent-id))
      buffer)))

(provide 'disco-channel-directory)

;;; disco-channel-directory.el ends here
