;;; disco-channel-directory.el --- Per-guild channel directory -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; A persistent, EWOC-backed channel directory for one Discord guild.  The
;; global root remains a compact account navigator; opening a guild creates a
;; dedicated buffer whose categories can be expanded independently.  Guild
;; channel snapshots are hydrated lazily by `disco-directory'.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'disco-channel-type)
(require 'disco-customize)
(require 'disco-directory)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-permission)
(require 'disco-root-view)
(require 'disco-state)
(require 'disco-thread)
(require 'appkit-view)
(require 'appkit-ewoc)
(require 'appkit-position)

(autoload 'disco-root-open "disco-root" nil t)

(defface disco-channel-directory-category
  '((t :inherit header-line :weight semi-bold :extend t))
  "Face for category dividers in guild channel directories."
  :group 'disco)

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

(cl-defstruct (disco-channel-directory-entry
               (:constructor disco-channel-directory-entry-create))
  key
  type
  group-id
  title
  unread-count
  expanded
  channel
  indent
  stamp
  text
  face)

(defvar disco-channel-directory--window-size-hook-installed nil
  "Non-nil once the guild-directory resize hook has been installed.")

(defvar-local disco-channel-directory--guild-id nil
  "Guild ID owned by the current channel-directory buffer.")

(defvar-local disco-channel-directory--ewoc nil
  "Persistent EWOC for the current guild channel directory.")

(defvar-local disco-channel-directory--node-table nil
  "Hash table mapping stable entry keys to EWOC nodes.")

(defvar-local disco-channel-directory--key-cache nil
  "Hash table interning stable entry keys within this directory buffer.")

(defvar-local disco-channel-directory--collapsed-groups nil
  "Hash table containing category/group IDs explicitly collapsed by the user.")

(defvar-local disco-channel-directory--expanded-thread-parents nil
  "Hash table containing forum/media parent IDs expanded by the user.")

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

(defvar-local disco-channel-directory--rendering nil
  "Non-nil while an EWOC reconciliation is active.")

(defvar-local disco-channel-directory--render-pending nil
  "Non-nil when another reconciliation was requested while rendering.")

(defvar-local disco-channel-directory--deferred-reconcile-p nil
  "Non-nil when hidden-buffer updates await a display window.")

(defvar-local disco-channel-directory--deferred-channel-ids nil
  "Channel IDs accumulated while this directory has no display window.")

(defconst disco-channel-directory--uncategorized-group :uncategorized
  "Synthetic group ID used for channels outside a category.")

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

(defun disco-channel-directory--canonical-key (kind id)
  "Return a buffer-local canonical entry key for KIND and ID."
  (let* ((token (format "%s:%s" kind id))
         (cached (gethash token disco-channel-directory--key-cache)))
    (or cached
        (puthash token token disco-channel-directory--key-cache))))

(defun disco-channel-directory--entry-key-for-channel (channel-id)
  "Return canonical EWOC key for CHANNEL-ID."
  (disco-channel-directory--canonical-key 'channel channel-id))

(defun disco-channel-directory--entry-key-for-group (group-id)
  "Return canonical EWOC key for GROUP-ID."
  (disco-channel-directory--canonical-key 'group group-id))

(defun disco-channel-directory--entry-key-for-note (note-id)
  "Return canonical EWOC key for NOTE-ID."
  (disco-channel-directory--canonical-key 'note note-id))

(defun disco-channel-directory--channel-name (channel)
  "Return CHANNEL's directory display name."
  (or (alist-get 'name channel) "(unnamed channel)"))

(defun disco-channel-directory--channel-position (channel)
  "Return numeric Discord ordering position for CHANNEL."
  (let ((position (alist-get 'position channel)))
    (cond
     ((integerp position) position)
     ((and (stringp position)
           (string-match-p "\\`[0-9]+\\'" position))
      (string-to-number position))
     (t most-positive-fixnum))))

(defun disco-channel-directory--channel-before-p (left right)
  "Return non-nil when LEFT belongs before RIGHT in a guild directory."
  (let ((left-position (disco-channel-directory--channel-position left))
        (right-position (disco-channel-directory--channel-position right)))
    (if (= left-position right-position)
        (let ((left-name (downcase (disco-channel-directory--channel-name left)))
              (right-name (downcase (disco-channel-directory--channel-name right))))
          (if (equal left-name right-name)
              (string-lessp
               (or (disco-channel-directory--normalize-id (alist-get 'id left)) "")
               (or (disco-channel-directory--normalize-id (alist-get 'id right)) ""))
            (string-lessp left-name right-name)))
      (< left-position right-position))))

(defun disco-channel-directory--sort-channels (channels)
  "Return a Discord-position-sorted copy of CHANNELS."
  (sort (copy-sequence (or channels '()))
        #'disco-channel-directory--channel-before-p))

(defun disco-channel-directory--category-p (channel)
  "Return non-nil when CHANNEL is a category container."
  (equal (alist-get 'type channel) 4))

(defun disco-channel-directory--displayable-channel-p (channel)
  "Return non-nil when CHANNEL may be shown in the directory."
  (and (alist-get 'id channel)
       (disco-channel-root-visible-p channel)
       (disco-permission-channel-viewable-p channel t)))

(defun disco-channel-directory--filter-active-p ()
  "Return non-nil when a filter lens is active."
  (or disco-channel-directory--filter
      disco-channel-directory--unread-only))

(defun disco-channel-directory--name-matches-p (channel)
  "Return non-nil when CHANNEL matches the active text filter."
  (or (null disco-channel-directory--filter)
      (string-match-p
       (regexp-quote disco-channel-directory--filter)
       (downcase (disco-channel-directory--channel-name channel)))))

(defun disco-channel-directory--channel-matches-lens-p
    (channel &optional ignore-name-filter)
  "Return non-nil when CHANNEL matches current directory lenses.

When IGNORE-NAME-FILTER is non-nil, retain permission and unread filtering
but do not require the channel name to match."
  (and (disco-channel-directory--displayable-channel-p channel)
       (or ignore-name-filter
           (disco-channel-directory--name-matches-p channel))
       (or (not disco-channel-directory--unread-only)
           (disco-state-channel-has-unread-p channel))))

(defun disco-channel-directory--group-expanded-p (group-id)
  "Return non-nil when GROUP-ID should expose its children."
  (or (disco-channel-directory--filter-active-p)
      (not (gethash group-id disco-channel-directory--collapsed-groups))))

(defun disco-channel-directory--thread-before-p (left right)
  "Return non-nil when thread LEFT should appear before RIGHT."
  (let ((left-id (disco-channel-directory--normalize-id
                  (or (alist-get 'last_message_id left)
                      (alist-get 'id left))))
        (right-id (disco-channel-directory--normalize-id
                   (or (alist-get 'last_message_id right)
                       (alist-get 'id right)))))
    (cond
     ((and left-id right-id (not (equal left-id right-id)))
      (disco-state-snowflake< right-id left-id))
     (t
      (string-lessp (downcase (disco-channel-directory--channel-name left))
                    (downcase (disco-channel-directory--channel-name right)))))))

(defun disco-channel-directory--active-forum-posts (parent-id)
  "Return sorted active forum/media posts beneath PARENT-ID."
  (sort
   (seq-filter
    (lambda (thread)
      (and (not (disco-thread-archived-p thread))
           (disco-channel-directory--displayable-channel-p thread)))
    (disco-state-parent-threads parent-id))
   #'disco-channel-directory--thread-before-p))

(defun disco-channel-directory--visible-forum-posts
    (channel &optional ignore-name-filter)
  "Return sorted visible active posts beneath forum/media CHANNEL.

When IGNORE-NAME-FILTER is non-nil, retain unread and permission lenses while
showing every child name."
  (let ((ignore-name-filter
         (or ignore-name-filter
             (and disco-channel-directory--filter
                  (disco-channel-directory--name-matches-p channel)))))
    (sort
     (seq-filter
      (lambda (thread)
        (and (not (disco-thread-archived-p thread))
             (or (disco-thread-starter-message thread)
                 (disco-directory-parent-thread-starter-unavailable-p
                  (alist-get 'id channel)
                  (alist-get 'id thread)))
             (disco-channel-directory--channel-matches-lens-p
              thread ignore-name-filter)))
      (disco-state-parent-threads (alist-get 'id channel)))
     #'disco-channel-directory--thread-before-p)))

(defun disco-channel-directory--channel-visible-p
    (channel &optional ignore-name-filter)
  "Return non-nil when CHANNEL belongs in the current directory lens.

IGNORE-NAME-FILTER has the same meaning as in
`disco-channel-directory--channel-matches-lens-p'."
  (if (disco-channel-forum-or-media-p channel)
      (or (and (disco-channel-directory--displayable-channel-p channel)
               (or ignore-name-filter
                   (disco-channel-directory--name-matches-p channel))
               (not disco-channel-directory--unread-only))
          (disco-channel-directory--visible-forum-posts
           channel ignore-name-filter))
    (disco-channel-directory--channel-matches-lens-p
     channel ignore-name-filter)))

(defun disco-channel-directory--thread-parent-expanded-p (parent-id)
  "Return non-nil when PARENT-ID should expose inline active threads."
  (or (disco-channel-directory--filter-active-p)
      (gethash (disco-channel-directory--normalize-id parent-id)
               disco-channel-directory--expanded-thread-parents)))

(defun disco-channel-directory--channel-stamp (channel)
  "Return render-relevant state stamp for CHANNEL."
  (let ((latest-message (disco-msg-channel-last-cached-message channel))
        (starter-message
         (and (disco-state-channel-thread-p channel)
              (disco-thread-starter-message channel))))
    (list (disco-state-channel-effective-unread-count channel)
          (and (disco-state-channel-has-unread-p channel) t)
          (alist-get 'last_message_id channel)
          (and latest-message (sxhash-equal latest-message))
          (and starter-message (sxhash-equal starter-message)))))

(defun disco-channel-directory--channel-entry (channel indent)
  "Return one directory entry for CHANNEL at INDENT."
  (let ((channel-id
         (disco-channel-directory--normalize-id (alist-get 'id channel))))
    (disco-channel-directory-entry-create
     :key (disco-channel-directory--entry-key-for-channel channel-id)
     :type 'channel
     :channel channel
     :indent indent
     :stamp (disco-channel-directory--channel-stamp channel))))

(defun disco-channel-directory--thread-parent-entry (channel indent)
  "Return one expandable forum/media CHANNEL entry at INDENT."
  (let* ((parent-id
          (disco-channel-directory--normalize-id (alist-get 'id channel)))
         (threads (disco-channel-directory--active-forum-posts parent-id))
         (state (disco-directory-parent-threads-state parent-id)))
    (disco-channel-directory-entry-create
     :key (disco-channel-directory--entry-key-for-channel parent-id)
     :type 'thread-parent
     :channel channel
     :indent indent
     :unread-count (disco-channel-directory--children-unread-count threads)
     :expanded (disco-channel-directory--thread-parent-expanded-p parent-id)
     :stamp (list (disco-channel-directory--channel-stamp channel)
                  (plist-get state :status)
                  (plist-get state :phase)
                  (plist-get state :loaded-count)
                  (plist-get state :total)
                  (length threads)))))

(defun disco-channel-directory--group-entry (group-id title unread-count expanded)
  "Return category GROUP-ID entry with TITLE, UNREAD-COUNT, and EXPANDED."
  (disco-channel-directory-entry-create
   :key (disco-channel-directory--entry-key-for-group group-id)
   :type 'group
   :group-id group-id
   :title title
   :unread-count unread-count
   :expanded expanded))

(defun disco-channel-directory--note-entry (note-id text &optional face indent)
  "Return one status NOTE-ID entry displaying TEXT with FACE at INDENT."
  (disco-channel-directory-entry-create
   :key (disco-channel-directory--entry-key-for-note note-id)
   :type 'note
   :text text
   :indent indent
   :face (or face 'shadow)))

(defun disco-channel-directory--children-unread-count (channels)
  "Return unread mention total represented by parent CHANNELS."
  (let ((total 0))
    (dolist (channel channels total)
      (setq total
            (+ total
               (if (disco-channel-forum-or-media-p channel)
                   (+ (disco-state-channel-own-unread-count channel)
                      (disco-channel-directory--children-unread-count
                       (disco-channel-directory--active-forum-posts
                        (alist-get 'id channel))))
                 (disco-state-channel-effective-unread-count channel)))))))

(defun disco-channel-directory--prepend-channel-entries
    (entries channel channel-indent post-indent &optional ignore-name-filter)
  "Prepend CHANNEL and any visible forum posts to reverse ENTRIES.

CHANNEL-INDENT and POST-INDENT control row indentation.  IGNORE-NAME-FILTER
is forwarded when a matching category reveals its children.  Return the
extended reverse accumulator."
  (if (disco-channel-forum-or-media-p channel)
      (let* ((parent-id
              (disco-channel-directory--normalize-id (alist-get 'id channel)))
             (expanded
              (disco-channel-directory--thread-parent-expanded-p parent-id))
             (threads
              (disco-channel-directory--visible-forum-posts
               channel ignore-name-filter))
             (state (disco-directory-parent-threads-state parent-id))
             (status (plist-get state :status)))
        (push (disco-channel-directory--thread-parent-entry
               channel channel-indent)
              entries)
        (when expanded
          (pcase status
            ('unloaded
             (push
              (disco-channel-directory--note-entry
               (list 'parent-threads parent-id)
               "Active posts are not loaded."
               'shadow post-indent)
              entries))
            ('loading
             (let ((loaded (or (plist-get state :loaded-count) 0))
                   (total (plist-get state :total))
                   (phase (plist-get state :phase)))
               (push
                (disco-channel-directory--note-entry
                 (list 'parent-threads parent-id)
                 (cond
                  ((eq phase 'indexing)
                   "Discord is indexing active posts…")
                  ((and (numberp total) (> total 0))
                   (format "Loading active posts… %d/%d" loaded total))
                  ((> loaded 0)
                   (format "Loading active posts… %d" loaded))
                  (t "Loading active posts…"))
                 'shadow post-indent)
                entries)))
            ('error
             (push
              (disco-channel-directory--note-entry
               (list 'parent-threads parent-id)
               (format "Active post loading failed: %s"
                       (disco-root--async-error-message
                        (plist-get state :error)))
               'error post-indent)
              entries))
            ('loaded
             (unless threads
               (push
                (disco-channel-directory--note-entry
                 (list 'parent-threads parent-id)
                 "No active posts."
                 'shadow post-indent)
                entries))))
          (dolist (thread threads)
            (push (disco-channel-directory--channel-entry thread post-indent)
                  entries))))
    (push (disco-channel-directory--channel-entry channel channel-indent)
          entries))
  entries)

(defun disco-channel-directory--project-loaded-entries ()
  "Project current guild state into ordered directory entries."
  (let* ((channels
          (or (disco-state-guild-channels disco-channel-directory--guild-id) '()))
         (categories
          (disco-channel-directory--sort-channels
           (seq-filter (lambda (channel)
                         (and (alist-get 'id channel)
                              (disco-channel-directory--category-p channel)
                              (disco-permission-channel-viewable-p channel t)))
                       channels)))
         (parents
          (disco-channel-directory--sort-channels
           (seq-filter
            (lambda (channel)
              (and (not (disco-channel-directory--category-p channel))
                   (not (disco-state-channel-thread-p channel))
                   (disco-channel-directory--displayable-channel-p channel)))
            channels)))
         (category-table (make-hash-table :test #'equal))
         (children-by-category (make-hash-table :test #'equal))
         uncategorized
         entries)
    (dolist (category categories)
      (when-let* ((category-id
                   (disco-channel-directory--normalize-id
                    (alist-get 'id category))))
        (puthash category-id category category-table)))
    (dolist (channel parents)
      (let ((parent-id
             (disco-channel-directory--normalize-id
              (alist-get 'parent_id channel))))
        (if (and parent-id (gethash parent-id category-table))
            (puthash parent-id
                     (append (gethash parent-id children-by-category)
                             (list channel))
                     children-by-category)
          (setq uncategorized (append uncategorized (list channel))))))
    (let ((visible-uncategorized
           (seq-filter
            (lambda (channel)
              (disco-channel-directory--channel-visible-p channel))
            uncategorized)))
      (when visible-uncategorized
        (let* ((group-id disco-channel-directory--uncategorized-group)
               (expanded (disco-channel-directory--group-expanded-p group-id)))
          (push
           (disco-channel-directory--group-entry
            group-id
            "Channels"
            (disco-channel-directory--children-unread-count
             visible-uncategorized)
            expanded)
           entries)
          (when expanded
            (dolist (channel visible-uncategorized)
              (when (disco-channel-directory--channel-visible-p channel)
                (setq entries
                      (disco-channel-directory--prepend-channel-entries
                       entries channel 2 4))))))))
    (dolist (category categories)
      (let* ((category-id
              (disco-channel-directory--normalize-id (alist-get 'id category)))
             (children (gethash category-id children-by-category))
             (category-name-matches
              (and disco-channel-directory--filter
                   (disco-channel-directory--name-matches-p category)))
             (visible-children
             (seq-filter
               (lambda (channel)
                 (disco-channel-directory--channel-visible-p
                  channel category-name-matches))
               children))
             (category-matches
              (disco-channel-directory--name-matches-p category))
             (visible-p
              (or (not (disco-channel-directory--filter-active-p))
                  category-matches
                  visible-children)))
        (when visible-p
          (let ((expanded
                 (disco-channel-directory--group-expanded-p category-id)))
            (push
             (disco-channel-directory--group-entry
              category-id
              (disco-channel-directory--channel-name category)
              (disco-channel-directory--children-unread-count
               visible-children)
              expanded)
             entries)
            (when expanded
              (dolist (channel visible-children)
                (setq entries
                      (disco-channel-directory--prepend-channel-entries
                       entries channel 2 4 category-name-matches))))))))
    (or (nreverse entries)
        (list
         (disco-channel-directory--note-entry
          'empty
          (if (disco-channel-directory--filter-active-p)
              "No channels match the active directory lens."
            "This guild has no visible channels."))))))

(defun disco-channel-directory--project-entries ()
  "Return the current directory projection, including lifecycle state."
  (let ((guild (disco-channel-directory--guild))
        (status
         (disco-directory-guild-status disco-channel-directory--guild-id)))
    (cond
     ((null guild)
      (list (disco-channel-directory--note-entry
             'missing-guild "This guild is no longer available." 'warning)))
     ((and (eq status 'loading)
           (not (disco-state-guild-channels-loaded-p
                 disco-channel-directory--guild-id)))
      (list (disco-channel-directory--note-entry
             'loading "Loading channels…")))
     ((eq status 'error)
      (list (disco-channel-directory--note-entry
             'load-error "Channel loading failed." 'error)))
     ((not (disco-state-guild-channels-loaded-p
            disco-channel-directory--guild-id))
      (list (disco-channel-directory--note-entry
             'unloaded "Channels have not been loaded.")))
     (t
      (disco-channel-directory--project-loaded-entries)))))

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

(defun disco-channel-directory--insert-group (entry)
  "Insert category/group ENTRY."
  (let* ((start (point))
         (expanded (disco-channel-directory-entry-expanded entry))
         (indicator (if expanded "▾" "▸"))
         (title (or (disco-channel-directory-entry-title entry) "Category"))
         (unread (or (disco-channel-directory-entry-unread-count entry) 0)))
    (insert " " indicator " " title)
    (when (> unread 0)
      (insert (format "  @%d" unread)))
    (insert "\n")
    (add-text-properties
     start
     (point)
     (list 'face 'disco-channel-directory-category
           'disco-channel-directory-row-type 'group
           'disco-channel-directory-group-id
           (disco-channel-directory-entry-group-id entry)
           'disco-channel-directory-key
           (disco-channel-directory-entry-key entry)
           'help-echo "RET/TAB toggles this category"))))

(defun disco-channel-directory--insert-channel (entry)
  "Insert channel ENTRY as a responsive one-line row."
  (let* ((start (point))
         (channel (disco-channel-directory-entry-channel entry))
         (scope (if (disco-state-channel-thread-p channel)
                    'parent-thread
                  'directory)))
    (disco-root--insert-activity-channel-line
     channel
     (or (disco-channel-directory-entry-indent entry) 0)
     scope
     disco-channel-directory--fill-column)
    (add-text-properties
     start
     (point)
     (list 'disco-channel-directory-row-type 'channel
           'disco-channel-directory-key
           (disco-channel-directory-entry-key entry)))))

(defun disco-channel-directory--insert-thread-parent (entry)
  "Insert expandable forum/media parent ENTRY."
  (let* ((start (point))
         (channel (disco-channel-directory-entry-channel entry))
         (parent-id
          (disco-channel-directory--normalize-id (alist-get 'id channel)))
         (expanded (disco-channel-directory-entry-expanded entry))
         (unread (or (disco-channel-directory-entry-unread-count entry) 0))
         (indent (or (disco-channel-directory-entry-indent entry) 0)))
    (insert (make-string indent ?\s) (if expanded "▾ " "▸ "))
    (disco-root--insert-activity-channel-line
     channel 0 'directory disco-channel-directory--fill-column)
    (add-text-properties
     start
     (point)
     (list 'disco-channel-directory-row-type 'thread-parent
           'disco-channel-directory-thread-parent-id parent-id
           'disco-channel-directory-key
           (disco-channel-directory-entry-key entry)
           'disco-unread-count unread
           'disco-has-unread (> unread 0)
           'help-echo
           "RET/TAB toggles active posts; g refreshes; A opens archived posts"))))

(defun disco-channel-directory--insert-note (entry)
  "Insert lifecycle/status note ENTRY."
  (let ((start (point)))
    (insert (make-string (or (disco-channel-directory-entry-indent entry) 2)
                         ?\s)
            (or (disco-channel-directory-entry-text entry) "") "\n")
    (add-text-properties
     start
     (point)
     (list 'face (or (disco-channel-directory-entry-face entry) 'shadow)
           'disco-channel-directory-row-type 'note
           'disco-channel-directory-key
           (disco-channel-directory-entry-key entry)))))

(defun disco-channel-directory--ewoc-printer (entry)
  "Insert one guild-directory EWOC ENTRY."
  (pcase (disco-channel-directory-entry-type entry)
    ('group (disco-channel-directory--insert-group entry))
    ('channel (disco-channel-directory--insert-channel entry))
    ('thread-parent (disco-channel-directory--insert-thread-parent entry))
    ('note (disco-channel-directory--insert-note entry))
    (type (error "Disco: unknown channel-directory entry type %S" type))))

(defun disco-channel-directory--apply-entries (entries force-channel-ids)
  "Reconcile EWOC against ENTRIES and FORCE-CHANNEL-IDS.

Existing keyed nodes are updated or moved in place.  Only disappeared nodes
are deleted and only new nodes are inserted."
  (setq disco-channel-directory--node-table
        (appkit-ewoc-reconcile
         disco-channel-directory--ewoc
         entries
         #'disco-channel-directory-entry-key
         :force-keys
         (mapcar #'disco-channel-directory--entry-key-for-channel
                 force-channel-ids))))

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
                  (and (not (disco-channel-directory--category-p channel))
                       (not (disco-state-channel-thread-p channel))
                       (disco-channel-directory--displayable-channel-p channel)))
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

(defun disco-channel-directory--reconcile (&optional force-channel-ids)
  "Reconcile the current EWOC, forcing rows in FORCE-CHANNEL-IDS."
  (if disco-channel-directory--rendering
      (setq disco-channel-directory--render-pending t)
    (let ((disco-channel-directory--rendering t)
          (snapshot
           (appkit-position-capture
            :anchor-property 'disco-channel-directory-key
            :preserve-window-start t)))
      (unwind-protect
          (progn
            (setq disco-channel-directory--fill-column
                  (disco-channel-directory--usable-width))
            (let ((inhibit-read-only t)
                  (buffer-undo-list t))
              (with-silent-modifications
                (disco-channel-directory--apply-entries
                 (disco-channel-directory--project-entries)
                 force-channel-ids)))
            (when snapshot
              (appkit-position-restore snapshot))
            (when disco-channel-directory--pending-focus-channel-id
              (when-let* ((position
                           (disco-channel-directory--find-channel-position
                            disco-channel-directory--pending-focus-channel-id)))
                (goto-char position)
                (beginning-of-line)
                (setq disco-channel-directory--pending-focus-channel-id nil)))
            (disco-channel-directory--refresh-header-line)
            (force-window-update (current-buffer)))
        (setq disco-channel-directory--rendering nil))
      (when disco-channel-directory--render-pending
        (setq disco-channel-directory--render-pending nil)
        (disco-channel-directory--reconcile)))))

(defun disco-channel-directory--displayed-p ()
  "Return non-nil when the current directory has a live display window."
  (window-live-p (get-buffer-window (current-buffer) t)))

(defun disco-channel-directory--request-reconcile (&optional force-channel-ids)
  "Reconcile now when displayed, otherwise defer FORCE-CHANNEL-IDS.

Directory rows use graphical window metrics for pixel alignment.  Mutating an
EWOC while its buffer is hidden would mix character-aligned and pixel-aligned
rows, so passive updates are coalesced until a real window is available."
  (dolist (channel-id force-channel-ids)
    (when channel-id
      (cl-pushnew channel-id
                  disco-channel-directory--deferred-channel-ids
                  :test #'equal)))
  (if (disco-channel-directory--displayed-p)
      (let ((channel-ids
             (prog1 (nreverse disco-channel-directory--deferred-channel-ids)
               (setq disco-channel-directory--deferred-channel-ids nil
                     disco-channel-directory--deferred-reconcile-p nil))))
        (disco-channel-directory--reconcile channel-ids))
    (setq disco-channel-directory--deferred-reconcile-p t)))

(defun disco-channel-directory--window-buffer-change (window)
  "Flush deferred directory updates when WINDOW displays the current buffer."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (disco-channel-directory--reflow-to-width
     (disco-channel-directory--usable-width))
    (when disco-channel-directory--deferred-reconcile-p
      (disco-channel-directory--request-reconcile))))

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

(defun disco-channel-directory--channel-line-positions (&optional unread-only)
  "Return channel-row positions, restricted to unread rows when UNREAD-ONLY."
  (let ((position (point-min))
        positions)
    (while (< position (point-max))
      (when (and (get-text-property position 'disco-channel-id)
                 (or (not unread-only)
                     (get-text-property position 'disco-has-unread)))
        (push position positions))
      (setq position (next-single-property-change
                      position 'disco-channel-id nil (point-max))))
    (nreverse (seq-uniq positions #'=))))

(defun disco-channel-directory--move-channel (step)
  "Move STEP channel rows from the current line."
  (let* ((positions (disco-channel-directory--channel-line-positions))
         (origin (line-beginning-position))
         (index (cl-position-if (lambda (position) (> position origin)) positions))
         (target-index
          (if (> step 0)
              (or index (length positions))
            (1- (or (cl-position-if
                     (lambda (position) (>= position origin)) positions)
                    (length positions)))))
         (target (nth target-index positions)))
    (if target
        (goto-char target)
      (message "Disco: no %s channel"
               (if (> step 0) "next" "previous")))))

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
  (let* ((positions (disco-channel-directory--channel-line-positions t))
         (origin (line-beginning-position))
         (target (or (seq-find (lambda (position) (> position origin)) positions)
                     (car positions))))
    (if target
        (goto-char target)
      (message "Disco: no unread channels in this guild"))))

(defun disco-channel-directory-toggle-group ()
  "Toggle the category/group row at point."
  (interactive)
  (when (disco-channel-directory--filter-active-p)
    (user-error "Disco: clear directory lenses before collapsing categories"))
  (let ((group-id
         (disco-channel-directory--line-property
          'disco-channel-directory-group-id)))
    (unless group-id
      (user-error "Disco: point is not on a category"))
    (if (gethash group-id disco-channel-directory--collapsed-groups)
        (remhash group-id disco-channel-directory--collapsed-groups)
      (puthash group-id t disco-channel-directory--collapsed-groups))
    (disco-channel-directory--reconcile)))

(defun disco-channel-directory-toggle-thread-parent (&optional parent-id)
  "Toggle inline active posts under forum/media PARENT-ID or the row at point."
  (interactive)
  (when (disco-channel-directory--filter-active-p)
    (user-error "Disco: clear directory lenses before folding active posts"))
  (setq parent-id
        (disco-channel-directory--normalize-id
         (or parent-id
             (disco-channel-directory--line-property
              'disco-channel-directory-thread-parent-id))))
  (let ((parent (and parent-id (disco-state-channel parent-id))))
    (unless (and parent (disco-channel-forum-or-media-p parent))
      (user-error "Disco: point is not on a forum or media channel"))
    (if (gethash parent-id disco-channel-directory--expanded-thread-parents)
        (remhash parent-id disco-channel-directory--expanded-thread-parents)
      (puthash parent-id t disco-channel-directory--expanded-thread-parents)
      (disco-directory-load-parent-threads-async parent-id))
    (disco-channel-directory--reconcile)))

(defun disco-channel-directory-toggle-at-point ()
  "Toggle the category or forum/media parent at point."
  (interactive)
  (cond
   ((disco-channel-directory--line-property
     'disco-channel-directory-group-id)
    (disco-channel-directory-toggle-group))
   ((disco-channel-directory--line-property
     'disco-channel-directory-thread-parent-id)
    (disco-channel-directory-toggle-thread-parent))
   (t
    (user-error "Disco: point is not on a foldable row"))))

(defun disco-channel-directory-open-at-point ()
  "Toggle the category or open the channel at point."
  (interactive)
  (cond
   ((disco-channel-directory--line-property
     'disco-channel-directory-group-id)
    (disco-channel-directory-toggle-group))
   ((disco-channel-directory--line-property
     'disco-channel-directory-thread-parent-id)
    (disco-channel-directory-toggle-thread-parent))
   ((disco-channel-directory--line-property 'disco-channel-id)
    (disco-root--open-channel
     (disco-channel-directory--line-property 'disco-channel-id)))
   (t
    (disco-channel-directory-next-channel))))

(defun disco-channel-directory-tab-dwim ()
  "Toggle a foldable row at point, otherwise move to the next channel."
  (interactive)
  (if (or (disco-channel-directory--line-property
           'disco-channel-directory-group-id)
          (disco-channel-directory--line-property
           'disco-channel-directory-thread-parent-id))
      (disco-channel-directory-toggle-at-point)
    (disco-channel-directory-next-channel)))

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
  (disco-channel-directory--reconcile))

(defun disco-channel-directory-clear-filter ()
  "Clear all active directory lenses."
  (interactive)
  (setq disco-channel-directory--filter nil
        disco-channel-directory--unread-only nil)
  (disco-channel-directory--reconcile))

(defun disco-channel-directory-toggle-unread-only ()
  "Toggle the current directory's unread-only lens."
  (interactive)
  (setq disco-channel-directory--unread-only
        (not disco-channel-directory--unread-only))
  (disco-channel-directory--reconcile)
  (message "Disco: guild unread lens %s"
           (if disco-channel-directory--unread-only "enabled" "disabled")))

(defun disco-channel-directory-refresh ()
  "Refresh the forum at point, or the current guild channel snapshot."
  (interactive)
  (if-let* ((parent-id
             (disco-channel-directory--line-property
              'disco-channel-directory-thread-parent-id)))
      (progn
        (puthash parent-id t disco-channel-directory--expanded-thread-parents)
        (disco-directory-load-parent-threads-async parent-id :force t)
        (message "Disco: refreshing active posts in %s…"
                 (disco-channel-directory--channel-name
                  (disco-state-channel parent-id))))
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
               'disco-channel-directory-thread-parent-id)
              (when-let* ((channel-id
                           (disco-channel-directory--line-property
                            'disco-channel-id))
                          (channel (disco-state-channel channel-id)))
                (and (disco-state-channel-thread-p channel)
                     (alist-get 'parent_id channel)))))
         (parent (and parent-id (disco-state-channel parent-id))))
    (and parent (disco-channel-thread-parent-p parent) parent)))

(defun disco-channel-directory-open-archived-at-point ()
  "Open paginated archived posts for the parent represented at point."
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

(defun disco-channel-directory--handle-gateway-event (event)
  "Reconcile current directory after a relevant gateway EVENT."
  (when (disco-channel-directory--event-relevant-p event)
    (when (and (memq (plist-get event :type)
                     '(guild-sync channel-create channel-update))
               (not (disco-state-guild-channels-loaded-p
                     disco-channel-directory--guild-id)))
      (disco-directory-load-guild-async
       disco-channel-directory--guild-id))
    (disco-channel-directory--request-reconcile
     (disco-gateway-event-channel-ids event))))

(defun disco-channel-directory--handle-directory-event (event)
  "Reconcile current directory after one lifecycle EVENT."
  (let ((type (plist-get event :type))
        (guild-id
         (disco-channel-directory--normalize-id
          (plist-get event :guild-id))))
    (when (or (memq type '(index-loaded))
              (and guild-id
                   (equal guild-id disco-channel-directory--guild-id)))
      (disco-channel-directory--request-reconcile))))

(defun disco-channel-directory--detach-live-updates ()
  "Detach the current directory from shared update streams."
  (when disco-channel-directory--gateway-handler
    (remove-hook 'disco-gateway-event-hook
                 disco-channel-directory--gateway-handler)
    (setq disco-channel-directory--gateway-handler nil)
    (disco-gateway-unwatch-global))
  (when disco-channel-directory--directory-handler
    (remove-hook 'disco-directory-event-hook
                 disco-channel-directory--directory-handler)
    (setq disco-channel-directory--directory-handler nil)))

(defun disco-channel-directory--handle-state-reset ()
  "Refresh cached directory headers after canonical state is reset."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq major-mode 'disco-channel-directory-mode)
          (disco-channel-directory--refresh-header-line))))))

(add-hook 'disco-state-reset-hook
          #'disco-channel-directory--handle-state-reset)

(defun disco-channel-directory--attach-live-updates ()
  "Attach the current directory to gateway and directory events."
  (disco-channel-directory--detach-live-updates)
  (let ((buffer (current-buffer)))
    (setq disco-channel-directory--gateway-handler
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (disco-channel-directory--handle-gateway-event event)))))
    (setq disco-channel-directory--directory-handler
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (disco-channel-directory--handle-directory-event event))))))
  (add-hook 'disco-gateway-event-hook
            disco-channel-directory--gateway-handler)
  (add-hook 'disco-directory-event-hook
            disco-channel-directory--directory-handler)
  (disco-gateway-watch-global))

(defun disco-channel-directory--reflow-to-width (width)
  "Reflow the current directory to WIDTH when it changed."
  (when (and (integerp width)
             (> width 0)
             (/= width (or disco-channel-directory--fill-column 0)))
    (let ((snapshot
           (appkit-position-capture
            :anchor-property 'disco-channel-directory-key
            :preserve-window-start t))
          (inhibit-read-only t)
          (buffer-undo-list t))
      (setq disco-channel-directory--fill-column width)
      (with-silent-modifications
        (ewoc-refresh disco-channel-directory--ewoc))
      (when snapshot
        (appkit-position-restore snapshot)))))

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
    (define-key map (kbd "q") #'quit-window)
    (define-key map [mouse-1]
      #'disco-channel-directory-mouse-open-at-point)
    map)
  "Keymap for `disco-channel-directory-mode'.")

(define-derived-mode disco-channel-directory-mode special-mode "Disco-Directory"
  "Major mode for one guild's channel directory."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local disco-channel-directory--ewoc nil)
  (setq-local disco-channel-directory--node-table
              (make-hash-table :test #'equal))
  (setq-local disco-channel-directory--key-cache
              (make-hash-table :test #'equal))
  (setq-local disco-channel-directory--collapsed-groups
              (make-hash-table :test #'equal))
  (setq-local disco-channel-directory--expanded-thread-parents
              (make-hash-table :test #'equal))
  (setq-local disco-channel-directory--pending-focus-channel-id nil)
  (setq-local disco-channel-directory--filter nil)
  (setq-local disco-channel-directory--unread-only nil)
  (setq-local disco-channel-directory--fill-column nil)
  (setq-local disco-channel-directory--header-line-cache "")
  (setq-local disco-channel-directory--rendering nil)
  (setq-local disco-channel-directory--render-pending nil)
  (setq-local disco-channel-directory--deferred-reconcile-p nil)
  (setq-local disco-channel-directory--deferred-channel-ids nil)
  (setq-local header-line-format
              'disco-channel-directory--header-line-cache)
  (setq-local revert-buffer-function
              (lambda (&rest _ignored)
                (disco-channel-directory-refresh)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq disco-channel-directory--ewoc
          (ewoc-create #'disco-channel-directory--ewoc-printer nil nil t)))
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
  (let ((buffer
         (get-buffer-create
          (disco-channel-directory--buffer-name guild-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'disco-channel-directory-mode)
        (disco-channel-directory-mode))
      (setq disco-channel-directory--guild-id guild-id))
    ;; Render only after the buffer has a window: graphical alignment and
    ;; emoji elision depend on that window's actual pixel metrics.
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (disco-channel-directory--attach-live-updates)
      (disco-channel-directory--reflow-to-width
       (disco-channel-directory--usable-width))
      (disco-channel-directory--reconcile)
      (disco-directory-load-guild-async guild-id))
    buffer))

;;;###autoload
(defun disco-channel-directory-open-thread-parent (parent-channel-id)
  "Open PARENT-CHANNEL-ID inline in its guild channel directory."
  (let* ((parent-id
          (disco-channel-directory--normalize-id parent-channel-id))
         (parent (and parent-id (disco-state-channel parent-id)))
         (guild-id (and parent (alist-get 'guild_id parent))))
    (unless (and parent (disco-channel-forum-or-media-p parent))
      (user-error "Disco: channel %s is not a forum or media channel"
                  parent-channel-id))
    (unless guild-id
      (user-error "Disco: channel %s has no guild context" parent-id))
    (let ((buffer (disco-channel-directory-open guild-id)))
      (with-current-buffer buffer
        (puthash parent-id t disco-channel-directory--expanded-thread-parents)
        (setq disco-channel-directory--pending-focus-channel-id parent-id)
        (disco-channel-directory--reconcile)
        (disco-directory-load-parent-threads-async parent-id))
      buffer)))

(provide 'disco-channel-directory)

;;; disco-channel-directory.el ends here
