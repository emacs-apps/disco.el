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
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-presentation)
(require 'disco-api)
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

(defvar-local disco-channel-directory--guild-profile nil
  "Extended Guild Profile owned by this directory view.")

(defvar-local disco-channel-directory--profile-loading nil
  "Non-nil while this directory is loading its Guild Profile.")

(defvar-local disco-channel-directory--profile-error nil
  "Guild Profile error text shown without hiding cached Guild data.")

(defvar-local disco-channel-directory--profile-owner nil
  "Opaque owner of the active Guild Profile request.")

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

(defvar-local disco-channel-directory--rendering nil
  "Non-nil while an EWOC reconciliation is active.")

(defvar-local disco-channel-directory--render-pending nil
  "Non-nil when another reconciliation was requested while rendering.")

(defvar-local disco-channel-directory--deferred-change nil
  "Projection change retained until this directory has a display window.")

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
  "Return the display name for the current directory Guild."
  (or (and (listp disco-channel-directory--guild-profile)
           (alist-get 'name disco-channel-directory--guild-profile))
      (alist-get 'name (disco-channel-directory--guild))
      disco-channel-directory--guild-id
      "Unknown guild"))

(defun disco-channel-directory--buffer-name (guild-id)
  "Return the stable channel-directory buffer name for GUILD-ID."
  (format "*disco:guild:%s*" guild-id))

(defun disco-channel-directory--surface-id (guild-id)
  "Return the Generated Surface identity for GUILD-ID."
  (list 'channel-directory
        (or (disco-channel-directory--normalize-id guild-id)
            (error "Disco: channel-directory Surface requires a guild id"))))

(defun disco-channel-directory--surface-current-p (surface guild-id)
  "Return non-nil when SURFACE still owns GUILD-ID's directory."
  (and (appkit-surface-live-p surface)
       (with-current-buffer (appkit-surface-buffer surface)
         (and (derived-mode-p 'disco-channel-directory-mode)
              (eq surface (appkit-current-surface))
              (equal guild-id disco-channel-directory--guild-id)
              (equal (appkit-surface-identity surface)
                     (disco-channel-directory--surface-id guild-id))))))

(defun disco-channel-directory--profile-current-p
    (surface buffer guild-id owner)
  "Return non-nil when OWNER still loads GUILD-ID for SURFACE."
  (and (buffer-live-p buffer)
       (eq buffer (appkit-surface-buffer surface))
       (disco-channel-directory--surface-current-p surface guild-id)
       (with-current-buffer buffer
         (eq owner disco-channel-directory--profile-owner))))

(defun disco-channel-directory--guild-snapshot-resource-key ()
  "Return the Appkit resource key for the current guild channel snapshot."
  (list 'guild-channel-snapshot disco-channel-directory--guild-id))

(defun disco-channel-directory--ensure-surface ()
  "Return the live Generated Surface owning the current directory."
  (let* ((app (disco-runtime-app))
         (identity (disco-channel-directory--surface-id
                    disco-channel-directory--guild-id))
         (current (appkit-current-surface)))
    (cond
     ((and (appkit-surface-live-p current)
           (eq app (appkit-surface-app current))
           (equal identity (appkit-surface-identity current)))
      current)
     ((appkit-surface-p current)
      (error "Disco: channel-directory buffer belongs to another Surface"))
     (t
      (appkit-open-generated-surface
       disco-channel-directory--surface-type
       :app app
       :identity identity
       :input disco-channel-directory--guild-id
       :buffer (current-buffer))))))

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

(defun disco-channel-directory--overview-value (field)
  "Return Guild Profile or cached Guild value for FIELD."
  (or (and (listp disco-channel-directory--guild-profile)
           (alist-get field disco-channel-directory--guild-profile))
      (alist-get field (disco-channel-directory--guild))))

(defun disco-channel-directory--overview-label (label value)
  "Return one compact overview row for LABEL and VALUE."
  (concat (propertize (format "%-10s" (concat label ":")) 'face 'bold)
          value))

(defun disco-channel-directory--overview-metrics ()
  "Return a compact server metrics string, or nil."
  (let* ((members
          (disco-channel-directory--overview-value 'member_count))
         (online
          (disco-channel-directory--overview-value 'online_count))
         (tier
          (disco-channel-directory--overview-value 'premium_tier))
         (boosts
          (disco-channel-directory--overview-value
           'premium_subscription_count))
         (joined (alist-get 'joined_at (disco-channel-directory--guild)))
         (parts
          (delq nil
                (list
                 (and (integerp members)
                      (format "%d members" members))
                 (and (integerp online)
                      (format "%d online" online))
                 (and (integerp tier)
                      (format "Boost level %d" tier))
                 (and (integerp boosts)
                      (format "%d boosts" boosts))
                 (and (stringp joined)
                      (format "Joined %s"
                              (substring joined 0 (min 10 (length joined)))))))))
    (and parts (string-join parts " · "))))

(defun disco-channel-directory--overview-traits ()
  "Return the Guild Profile trait labels, or nil."
  (when-let* ((traits
               (and (listp disco-channel-directory--guild-profile)
                    (alist-get 'traits
                               disco-channel-directory--guild-profile)))
              ((listp traits)))
    (let ((labels
           (delq nil
                 (mapcar
                  (lambda (trait)
                    (and (listp trait) (alist-get 'label trait)))
                  traits))))
      (and labels (string-join labels " · ")))))

(defun disco-channel-directory--overview-entry
    (suffix label &optional face help-echo)
  "Return one stable overview entry identified by SUFFIX and showing LABEL."
  (appkit-directory-entry-create
   :key (list 'guild-overview disco-channel-directory--guild-id suffix)
   :role 'note
   :section-key (list 'guild-overview disco-channel-directory--guild-id)
   :label label
   :face face
   :stamp label
   :help-echo help-echo))

(defun disco-channel-directory--overview-entries ()
  "Return compact Guild context rows prepended to the channel directory."
  (let* ((description
          (disco-channel-directory--overview-value 'description))
         (metrics (disco-channel-directory--overview-metrics))
         (traits (disco-channel-directory--overview-traits))
         (width (max 20 (or disco-channel-directory--fill-column 80)))
         entries)
    (when (and (stringp description) (not (string-empty-p description)))
      (let ((short
             (truncate-string-to-width
              description (max 10 (- width 10)) nil nil "…")))
        (push
         (disco-channel-directory--overview-entry
          'description
          (disco-channel-directory--overview-label "About" short)
          nil description)
         entries)))
    (when metrics
      (push
       (disco-channel-directory--overview-entry
        'metrics
        (disco-channel-directory--overview-label "Server" metrics)
        'shadow)
       entries))
    (when traits
      (push
       (disco-channel-directory--overview-entry
        'traits
        (disco-channel-directory--overview-label "Traits" traits))
       entries))
    (when disco-channel-directory--profile-loading
      (push
       (disco-channel-directory--overview-entry
        'loading "Loading server details…" 'shadow)
       entries))
    (when disco-channel-directory--profile-error
      (push
       (disco-channel-directory--overview-entry
        'error disco-channel-directory--profile-error 'error)
       entries))
    (when entries
      (setq entries (nreverse entries))
      (append
       entries
       (list
        (appkit-directory-entry-create
         :key (list 'guild-overview
                    disco-channel-directory--guild-id 'spacer)
         :role 'spacer
         :stamp 'guild-overview-spacer))))))

(defun disco-channel-directory--project-entries ()
  "Project Guild overview and lifecycle state into one canonical surface."
  (append
   (disco-channel-directory--overview-entries)
   (disco-guild-directory-project
    (disco-channel-directory--projection-context))))

(defun disco-channel-directory--usable-width ()
  "Return current usable directory width in columns."
  (or (when-let* ((widths
                   (delq nil
                         (mapcar
                          (lambda (window)
                            (appkit-geometry-window-width
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
       (disco-channel-directory--refresh-and-sync (list parent-id) t)))
    ('group
     (disco-channel-directory--refresh-and-sync nil t))
    (_
     (error "Disco: unsupported guild-directory fold row %S"
            (disco-guild-directory-entry-row-kind entry)))))

(defun disco-channel-directory--apply-entries (entries force-entry-keys)
  "Reconcile Appkit directory ENTRIES, redrawing FORCE-ENTRY-KEYS."
  (appkit-directory-reconcile
   (appkit-directory-surface) entries
   :force-keys force-entry-keys))

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
FORCE-ENTRY-KEYS contains stable projection keys to redraw."
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

(defun disco-channel-directory--surface-init (_context guild-id)
  "Initialize a channel directory Surface for GUILD-ID."
  (setq-local disco-channel-directory--guild-id guild-id)
  (appkit-next
   :model guild-id
   :render (appkit-projection-change-create :full-p t :frame-p t)))

(defun disco-channel-directory--surface-update (_context model message)
  "Translate directory client MESSAGE into a projection change."
  (pcase message
    ('refresh
     (appkit-next
      :model model
      :render
      (appkit-projection-change-create :full-p t :frame-p t)))
    ('geometry
     (appkit-next
      :model model
      :render
      (appkit-projection-change-create
       :geometry-p t
       :position 'preserve)))
    ('frame
     (appkit-next
      :model model
      :render
      (appkit-projection-change-create :frame-p t)))
    ('display
     (appkit-next
      :model model
      :render
      (appkit-projection-change-create)))
    ('guild-snapshot
     (appkit-next
      :model model
      :render
      (appkit-projection-change-create
       :full-p t
       :frame-p t
       :resources
       (list (disco-channel-directory--guild-snapshot-resource-key)))))
    (`(channels-changed ,channel-ids)
     (appkit-next
      :model model
      :render
      (appkit-projection-change-create
       :keys (delete-dups
              (mapcar #'disco-channel-directory--entry-key-for-channel
                      channel-ids))
       :frame-p t)))
    (_ (appkit-next-reject 'unknown-channel-directory-message))))

(defun disco-channel-directory--surface-renderer (_surface)
  "Create one Generated Renderer for a channel directory."
  (appkit-generated-renderer-create
   :mount #'disco-runtime-retain-surface-owner
   :merge #'appkit-projection-change-merge
   :render (lambda (surface _app-read-view _model request)
             (disco-channel-directory--render-change
              surface request)
             nil)
   :recover nil
   :unmount
   (lambda (_surface)
     (disco-channel-directory--remove-live-updates))))

(defun disco-channel-directory--render-change (_surface change)
  "Render CHANGE, retaining hidden presentation work until displayed."
  (let* ((change (if disco-channel-directory--deferred-change
                     (appkit-projection-change-merge
                      disco-channel-directory--deferred-change change)
                   change))
         (entry-keys (appkit-projection-change-keys change))
         (all-rows-p (or (appkit-projection-change-full-p change)
                         (appkit-projection-change-geometry-p change)))
         (entries-p (or all-rows-p entry-keys
                        (appkit-projection-change-resources change)))
         (frame-p (or entries-p (appkit-projection-change-frame-p change)))
         (old-modified-p (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t))
    (if (not (disco-channel-directory--displayed-p))
        (when (or entries-p frame-p)
          (setq disco-channel-directory--deferred-change change))
      (when all-rows-p
        (setq entry-keys (disco-channel-directory--all-entry-keys)))
      (unwind-protect
          (cond
           (entries-p
            (disco-channel-directory--reconcile nil entry-keys))
           (frame-p
            (disco-channel-directory--refresh-header-line)))
        (set-buffer-modified-p old-modified-p))
      (setq disco-channel-directory--deferred-change nil))))

(defun disco-channel-directory--queue-surface-update (surface message)
  "Queue directory client MESSAGE on live SURFACE."
  (when (appkit-surface-live-p surface)
    (appkit-surface-post surface message)
    t))

(defun disco-channel-directory--request-reconcile
    (&optional force-channel-ids structure-p position-p surface)
  "Queue directory reconciliation on SURFACE or the current Surface."
  (when-let* ((surface (or surface (appkit-current-surface)))
              ((appkit-surface-live-p surface)))
    (when position-p
      (disco-channel-directory--queue-surface-update surface 'geometry))
    (disco-channel-directory--queue-surface-update
     surface (if (or structure-p (null force-channel-ids))
                 'refresh
               (list 'channels-changed force-channel-ids)))))

(defun disco-channel-directory--refresh-and-sync
    (&optional force-channel-ids structure-p position-p)
  "Synchronously commit a directory refresh."
  (when (disco-channel-directory--request-reconcile
         force-channel-ids structure-p position-p)
    (when-let* ((surface (appkit-current-surface))
                ((appkit-surface-live-p surface)))
      (appkit-surface-send
       surface 'display))))

(defun disco-channel-directory--request-profile (surface &optional force)
  "Load SURFACE's Guild Profile, retrying when FORCE is non-nil."
  (when (and (disco-channel-directory--surface-current-p
              surface disco-channel-directory--guild-id)
             (or force
                 (and (null disco-channel-directory--guild-profile)
                      (not disco-channel-directory--profile-loading))))
    (let* ((buffer (current-buffer))
           (guild-id disco-channel-directory--guild-id)
           (owner (gensym "disco-guild-profile-")))
      (setq disco-channel-directory--profile-owner owner
            disco-channel-directory--profile-loading t
            disco-channel-directory--profile-error nil)
      (disco-channel-directory--queue-surface-update surface 'refresh)
      (condition-case request-error
          (disco-api-guild-profile-async
           guild-id
           :on-success
           (lambda (profile)
             (when (disco-channel-directory--profile-current-p
                    surface buffer guild-id owner)
               (with-current-buffer buffer
                 (setq disco-channel-directory--guild-profile
                       (and (consp profile) profile)
                       disco-channel-directory--profile-loading nil
                       disco-channel-directory--profile-error
                       (unless (consp profile)
                         "Server details returned an invalid response")
                       disco-channel-directory--profile-owner nil)
                 (disco-channel-directory--queue-surface-update
                  surface 'refresh))))
           :on-error
           (lambda (error-data)
             (when (disco-channel-directory--profile-current-p
                    surface buffer guild-id owner)
               (with-current-buffer buffer
                 (setq disco-channel-directory--profile-loading nil
                       disco-channel-directory--profile-error
                       (format "Unable to load server details: %s"
                               (error-message-string error-data))
                       disco-channel-directory--profile-owner nil)
                 (disco-channel-directory--queue-surface-update
                  surface 'refresh)))))
        (error
         (when (disco-channel-directory--profile-current-p
                surface buffer guild-id owner)
           (setq disco-channel-directory--profile-loading nil
                 disco-channel-directory--profile-error
                 (format "Unable to load server details: %s"
                         (error-message-string request-error))
                 disco-channel-directory--profile-owner nil)
           (disco-channel-directory--queue-surface-update
            surface 'refresh))))
      owner)))

(defun disco-channel-directory--schedule-deferred-sync (surface)
  "Queue display of deferred presentation work on live SURFACE."
  (when disco-channel-directory--deferred-change
    (disco-channel-directory--queue-surface-update surface 'display)))

(defun disco-channel-directory--window-buffer-change (window)
  "Flush deferred directory updates when WINDOW displays this host."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (disco-channel-directory--reflow-to-width
     (disco-channel-directory--usable-width))
    (when-let* ((surface (appkit-current-surface)))
      (disco-channel-directory--schedule-deferred-sync surface))))

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
  (disco-channel-directory--refresh-and-sync nil t))

(defun disco-channel-directory-clear-filter ()
  "Clear all active directory lenses."
  (interactive)
  (setq disco-channel-directory--filter nil
        disco-channel-directory--unread-only nil)
  (disco-channel-directory--refresh-and-sync nil t))

(defun disco-channel-directory-toggle-unread-only ()
  "Toggle the current directory's unread-only lens."
  (interactive)
  (setq disco-channel-directory--unread-only
        (not disco-channel-directory--unread-only))
  (disco-channel-directory--refresh-and-sync nil t)
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
     disco-channel-directory--guild-id
     :force t)
    (when-let* ((surface (appkit-current-surface)))
      (disco-channel-directory--request-profile surface t))
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

(defun disco-channel-directory--handle-gateway-event
    (event &optional surface)
  "Queue channel changes and request missing snapshots for gateway EVENT."
  (let ((surface (or surface (appkit-current-surface))))
    (when (appkit-surface-live-p surface)
      (with-current-buffer (appkit-surface-buffer surface)
        (when (disco-channel-directory--event-relevant-p event)
          (let* ((type (plist-get event :type))
                 (channel-ids (disco-gateway-event-channel-ids event))
                 (structure-p
                  (or (memq type
                            disco-channel-directory--structural-gateway-events)
                      (null channel-ids)))
                 (snapshot-p
                  (memq type '(guild-sync guild-create guild-update
                               channel-create channel-update channel-sync))))
            (disco-channel-directory--queue-surface-update
             surface (cond (snapshot-p 'guild-snapshot)
                           (structure-p 'refresh)
                           (t (list 'channels-changed channel-ids))))
            (when (and snapshot-p
                       (not (disco-state-guild-channels-loaded-p
                             disco-channel-directory--guild-id)))
              (disco-directory-load-guild-async
               disco-channel-directory--guild-id))))))))

(defun disco-channel-directory--handle-directory-event
    (event &optional surface)
  "Queue a snapshot refresh for a relevant directory lifecycle EVENT."
  (let ((surface (or surface (appkit-current-surface))))
    (when (appkit-surface-live-p surface)
      (with-current-buffer (appkit-surface-buffer surface)
        (let ((type (plist-get event :type))
              (guild-id
               (disco-channel-directory--normalize-id
                (plist-get event :guild-id))))
          (when (or (eq type 'index-loaded)
                    (and guild-id
                         (equal guild-id disco-channel-directory--guild-id)))
            (disco-channel-directory--queue-surface-update
             surface 'guild-snapshot)))))))

(defun disco-channel-directory--handle-preview-update
    (channel-id &optional surface)
  "Queue the exact preview CHANNEL-ID row on live SURFACE."
  (let ((surface (or surface (appkit-current-surface))))
    (when (appkit-surface-live-p surface)
      (with-current-buffer (appkit-surface-buffer surface)
        (when-let* ((channel-id
                     (disco-channel-directory--normalize-id channel-id))
                    (channel (disco-state-channel channel-id))
                    (guild-id
                     (disco-channel-directory--normalize-id
                      (alist-get 'guild_id channel))))
          (when (equal guild-id disco-channel-directory--guild-id)
            (disco-channel-directory--queue-surface-update
             surface (list 'channels-changed (list channel-id)))))))))

(defun disco-channel-directory--remove-live-updates ()
  "Remove this buffer's shared event hooks."
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
          disco-channel-directory--preview-handler nil)
    (when watched-p (disco-gateway-unwatch-global))))

(defun disco-channel-directory--detach-live-updates ()
  "Detach the current directory from shared update streams."
  (disco-channel-directory--remove-live-updates))

(defun disco-channel-directory--handle-state-reset ()
  "Queue live directory Surfaces after canonical state reset."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (eq major-mode 'disco-channel-directory-mode)
                   (appkit-surface-live-p (appkit-current-surface)))
          (disco-channel-directory--queue-surface-update
           (appkit-current-surface) 'refresh))))))

(add-hook 'disco-state-reset-hook
          #'disco-channel-directory--handle-state-reset)

(defun disco-channel-directory--attach-live-updates ()
  "Attach the current directory Surface to shared event streams."
  (let ((surface (disco-channel-directory--ensure-surface)))
    (disco-channel-directory--remove-live-updates)
    (setq disco-channel-directory--gateway-handler
          (lambda (event)
            (disco-channel-directory--handle-gateway-event event surface))
          disco-channel-directory--directory-handler
          (lambda (event)
            (disco-channel-directory--handle-directory-event event surface))
          disco-channel-directory--preview-handler
          (lambda (channel-id)
            (disco-channel-directory--handle-preview-update
             channel-id surface)))
    (condition-case condition
        (progn
          (add-hook 'disco-gateway-event-hook
                    disco-channel-directory--gateway-handler)
          (add-hook 'disco-directory-event-hook
                    disco-channel-directory--directory-handler)
          (add-hook 'disco-preview-update-hook
                    disco-channel-directory--preview-handler)
          (disco-gateway-watch-global)
          surface)
      (error
       (disco-channel-directory--remove-live-updates)
       (signal (car condition) (cdr condition))))))

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
                        (appkit-geometry-window-width
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

(defvar-keymap disco-channel-directory-mode-map
  :doc "Keymap for `disco-channel-directory-mode'."
  :parent special-mode-map
  "g" #'disco-channel-directory-refresh
  "/" #'disco-channel-directory-set-filter
  "C-c C-k" #'disco-channel-directory-clear-filter
  "U" #'disco-channel-directory-toggle-unread-only
  "RET" #'disco-channel-directory-open-at-point
  "<return>" #'disco-channel-directory-open-at-point
  "TAB" #'disco-channel-directory-tab-dwim
  "<backtab>" #'disco-channel-directory-previous-channel
  "t" #'disco-channel-directory-toggle-at-point
  "A" #'disco-channel-directory-open-archived-at-point
  "n" #'disco-channel-directory-next-channel
  "p" #'disco-channel-directory-previous-channel
  "u" #'disco-channel-directory-next-unread
  "b" #'disco-channel-directory-open-root
  "<mouse-1>" #'disco-channel-directory-mouse-open-at-point)

(define-derived-mode disco-channel-directory-mode special-mode
  "Disco-Directory"
  "Major mode for one guild's channel directory."
  (setq buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local disco-channel-directory--pending-focus-channel-id nil)
  (setq-local disco-channel-directory--guild-profile nil)
  (setq-local disco-channel-directory--profile-loading nil)
  (setq-local disco-channel-directory--profile-error nil)
  (setq-local disco-channel-directory--profile-owner nil)
  (setq-local disco-channel-directory--filter nil)
  (setq-local disco-channel-directory--unread-only nil)
  (setq-local disco-channel-directory--fill-column nil)
  (setq-local disco-channel-directory--header-line-cache "")
  (setq-local disco-channel-directory--rendering nil)
  (setq-local disco-channel-directory--render-pending nil)
  (setq-local disco-channel-directory--deferred-change nil)
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
            #'disco-channel-directory--window-buffer-change nil t))

(defconst disco-channel-directory--surface-type
  (appkit-surface-type-create
   :name 'disco-channel-directory
   :mode #'disco-channel-directory-mode
   :init #'disco-channel-directory--surface-init
   :update #'disco-channel-directory--surface-update
   :renderer-factory #'disco-channel-directory--surface-renderer)
  "Generated Surface type for per-guild channel directories.")

;;;###autoload
(defun disco-channel-directory-open (guild-id)
  "Open the Generated channel directory Surface for GUILD-ID."
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
         (identity (disco-channel-directory--surface-id guild-id))
         (existing (appkit-app-surface app identity))
         (surface
          (cond
           ((appkit-surface-live-p existing)
            (pop-to-buffer (appkit-surface-buffer existing))
            existing)
           (existing
            (error "Disco: channel directory is unavailable: %S"
                   (appkit-surface-status existing)))
           (t
            (appkit-open-generated-surface
             disco-channel-directory--surface-type
             :app app
             :identity identity
             :input guild-id
             :buffer-name (disco-channel-directory--buffer-name guild-id)
             :select t))))
         (buffer (appkit-surface-buffer surface)))
    (with-current-buffer buffer
      (unless existing
        (disco-channel-directory--attach-live-updates)
        (disco-channel-directory--reflow-to-width
         (disco-channel-directory--usable-width))
        (disco-channel-directory--request-profile surface)
        (disco-channel-directory--request-reconcile nil t nil surface)
        (disco-directory-load-guild-async guild-id)
        (appkit-surface-send
         surface 'display)))
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
        (disco-channel-directory--refresh-and-sync (list parent-id) t)
        (disco-directory-load-parent-threads-async parent-id))
      buffer)))

(provide 'disco-channel-directory)

;;; disco-channel-directory.el ends here
