;;; disco-guild-directory.el --- Shared guild directory projection -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Purely context-parameterized projection of one Discord guild's channel
;; hierarchy into a flat stream of `appkit-directory-entry' values.  Both the
;; global root and the standalone channel directory use this module, while
;; keeping their own controller and rendering adapters.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-directory)
(require 'disco-channel-type)
(require 'disco-directory)
(require 'disco-msg)
(require 'disco-state)
(require 'disco-thread)

(defface disco-guild-directory-category
  '((t :inherit header-line :weight semi-bold :extend t))
  "Face for category dividers in shared guild directory projections."
  :group 'disco)

(defconst disco-guild-directory-row-kind-property
  'disco-guild-directory-row-kind
  "Text property identifying the semantic kind of a projected guild row.")

(defconst disco-guild-directory-guild-id-property
  'disco-guild-directory-guild-id
  "Text property carrying a projected guild row's guild ID.")

(defconst disco-guild-directory-group-id-property
  'disco-guild-directory-group-id
  "Text property carrying a projected guild row's raw category ID.")

(defconst disco-guild-directory-thread-parent-id-property
  'disco-guild-directory-thread-parent-id
  "Text property carrying a projected row's forum/media parent ID.")

(defconst disco-guild-directory-uncategorized-group :uncategorized
  "Synthetic group ID used for channels outside a category.")

(cl-defstruct (disco-guild-directory-context
               (:constructor disco-guild-directory-context--create))
  guild-id
  surface
  namespace
  section-key
  group-indent
  channel-indent
  thread-indent
  filter
  unread-only)

(defun disco-guild-directory--normalize-id (value)
  "Return VALUE as an ID string, or nil."
  (and value (format "%s" value)))

(defun disco-guild-directory--validate-indent (name value)
  "Return directory indent VALUE after validating it for NAME."
  (unless (and (integerp value) (>= value 0))
    (error "Disco guild directory %s must be a non-negative integer" name))
  value)

(cl-defun disco-guild-directory-context-create
    (&key guild-id surface namespace section-key
          (group-indent 1) (channel-indent 2) (thread-indent 4)
          filter unread-only)
  "Create a validated guild projection context.

GUILD-ID identifies the canonical guild snapshot.  SURFACE owns category and
forum fold state.  NAMESPACE separates keys belonging to this occurrence from
other projections of the same guild, while SECTION-KEY is the semantic owner
assigned to every returned entry.  GROUP-INDENT, CHANNEL-INDENT, and
THREAD-INDENT control the three visible hierarchy levels.  FILTER and
UNREAD-ONLY are optional projection lenses."
  (setq guild-id (disco-guild-directory--normalize-id guild-id))
  (unless guild-id
    (error "Disco guild directory context requires a guild id"))
  (unless (appkit-directory-surface-p surface)
    (error "Disco guild directory context requires an Appkit surface"))
  (unless namespace
    (error "Disco guild directory context requires a namespace"))
  (unless section-key
    (error "Disco guild directory context requires a section key"))
  (setq filter
        (when-let* ((value (string-trim (or filter ""))))
          (unless (string-empty-p value)
            (downcase value))))
  (disco-guild-directory-context--create
   :guild-id guild-id
   :surface surface
   :namespace namespace
   :section-key section-key
   :group-indent
   (disco-guild-directory--validate-indent "group-indent" group-indent)
   :channel-indent
   (disco-guild-directory--validate-indent "channel-indent" channel-indent)
   :thread-indent
   (disco-guild-directory--validate-indent "thread-indent" thread-indent)
   :filter filter
   :unread-only (and unread-only t)))

(defun disco-guild-directory-key (context kind id)
  "Return a stable namespaced key for CONTEXT, KIND, and ID.

Every key explicitly contains both CONTEXT's namespace and guild ID so one
surface may safely host multiple occurrences or guilds."
  (unless (disco-guild-directory-context-p context)
    (error "Disco guild directory key requires a context"))
  (list 'disco-guild-directory
        (disco-guild-directory-context-namespace context)
        (disco-guild-directory-context-guild-id context)
        kind id))

(defun disco-guild-directory-channel-key (context channel-id)
  "Return CONTEXT's stable row key for CHANNEL-ID."
  (disco-guild-directory-key
   context 'channel (disco-guild-directory--normalize-id channel-id)))

(defun disco-guild-directory-group-key (context group-id)
  "Return CONTEXT's stable row and fold key for GROUP-ID."
  (disco-guild-directory-key context 'group group-id))

(defun disco-guild-directory-note-key (context note-id)
  "Return CONTEXT's stable row key for NOTE-ID."
  (disco-guild-directory-key context 'note note-id))

(defun disco-guild-directory-thread-parent-key (context parent-id)
  "Return CONTEXT's stable fold key for forum/media PARENT-ID."
  (disco-guild-directory-key
   context 'thread-parent
   (disco-guild-directory--normalize-id parent-id)))

(defun disco-guild-directory-entry-row-kind (entry)
  "Return the public semantic row kind carried by projected ENTRY."
  (plist-get (appkit-directory-entry-properties entry)
             disco-guild-directory-row-kind-property))

(defun disco-guild-directory-entry-guild-id (entry)
  "Return the guild ID carried by projected ENTRY."
  (plist-get (appkit-directory-entry-properties entry)
             disco-guild-directory-guild-id-property))

(defun disco-guild-directory-entry-group-id (entry)
  "Return the raw category/group ID carried by projected ENTRY."
  (plist-get (appkit-directory-entry-properties entry)
             disco-guild-directory-group-id-property))

(defun disco-guild-directory-entry-thread-parent-id (entry)
  "Return the forum/media parent ID carried by projected ENTRY."
  (plist-get (appkit-directory-entry-properties entry)
             disco-guild-directory-thread-parent-id-property))

(defun disco-guild-directory--properties
    (context row-kind &optional group-id thread-parent-id)
  "Return stable public properties for a CONTEXT ROW-KIND.

GROUP-ID and THREAD-PARENT-ID are raw Discord/synthetic identifiers."
  (list
   disco-guild-directory-row-kind-property row-kind
   disco-guild-directory-guild-id-property
   (disco-guild-directory-context-guild-id context)
   disco-guild-directory-group-id-property group-id
   disco-guild-directory-thread-parent-id-property thread-parent-id))

(defun disco-guild-directory--guild (context)
  "Return the guild object represented by CONTEXT."
  (let ((guild-id (disco-guild-directory-context-guild-id context)))
    (seq-find
     (lambda (guild)
       (equal (disco-guild-directory--normalize-id (alist-get 'id guild))
              guild-id))
     (disco-state-guilds))))

(defun disco-guild-directory-channel-name (channel)
  "Return CHANNEL's directory display name."
  (or (alist-get 'name channel) "(unnamed channel)"))

(defun disco-guild-directory--channel-position (channel)
  "Return numeric Discord ordering position for CHANNEL."
  (let ((position (alist-get 'position channel)))
    (cond
     ((integerp position) position)
     ((and (stringp position)
           (string-match-p "\\`[0-9]+\\'" position))
      (string-to-number position))
     (t most-positive-fixnum))))

(defun disco-guild-directory--channel-before-p (left right)
  "Return non-nil when LEFT belongs before RIGHT in a guild directory."
  (let ((left-position (disco-guild-directory--channel-position left))
        (right-position (disco-guild-directory--channel-position right)))
    (if (= left-position right-position)
        (let ((left-name
               (downcase (disco-guild-directory-channel-name left)))
              (right-name
               (downcase (disco-guild-directory-channel-name right))))
          (if (equal left-name right-name)
              (string-lessp
               (or (disco-guild-directory--normalize-id
                    (alist-get 'id left))
                   "")
               (or (disco-guild-directory--normalize-id
                    (alist-get 'id right))
                   ""))
            (string-lessp left-name right-name)))
      (< left-position right-position))))

(defun disco-guild-directory--sort-channels (channels)
  "Return a Discord-position-sorted copy of CHANNELS."
  (sort (copy-sequence (or channels '()))
        #'disco-guild-directory--channel-before-p))

(defun disco-guild-directory-category-p (channel)
  "Return non-nil when CHANNEL is a category container."
  (equal (alist-get 'type channel) 4))

(defun disco-guild-directory-displayable-channel-p (channel)
  "Return non-nil when CHANNEL may be shown in a guild directory."
  (and (alist-get 'id channel)
       (disco-channel-root-visible-p channel)
       (disco-state-channel-viewable-p channel nil)))

(defun disco-guild-directory--filter-active-p (context)
  "Return non-nil when CONTEXT has an active lens."
  (or (disco-guild-directory-context-filter context)
      (disco-guild-directory-context-unread-only context)))

(defun disco-guild-directory--name-matches-p (context channel)
  "Return non-nil when CHANNEL matches CONTEXT's text filter."
  (let ((filter (disco-guild-directory-context-filter context)))
    (or (null filter)
        (string-match-p
         (regexp-quote filter)
         (downcase (disco-guild-directory-channel-name channel))))))

(defun disco-guild-directory--channel-matches-lens-p
    (context channel &optional ignore-name-filter)
  "Return non-nil when CHANNEL matches CONTEXT's lenses.

When IGNORE-NAME-FILTER is non-nil, retain permission and unread filtering
but do not require the channel name to match."
  (and (disco-guild-directory-displayable-channel-p channel)
       (or ignore-name-filter
           (disco-guild-directory--name-matches-p context channel))
       (or (not (disco-guild-directory-context-unread-only context))
           (disco-state-channel-has-unread-p channel))))

(defun disco-guild-directory--group-expanded-p (context group-id)
  "Return non-nil when GROUP-ID should expose its children in CONTEXT."
  (appkit-directory-fold-expanded-p
   (disco-guild-directory-context-surface context)
   (disco-guild-directory-group-key context group-id)
   t
   (disco-guild-directory--filter-active-p context)))

(defun disco-guild-directory--thread-before-p (left right)
  "Return non-nil when thread LEFT should appear before RIGHT."
  (let ((left-id
         (disco-guild-directory--normalize-id
          (or (alist-get 'last_message_id left) (alist-get 'id left))))
        (right-id
         (disco-guild-directory--normalize-id
          (or (alist-get 'last_message_id right) (alist-get 'id right)))))
    (if (and left-id right-id (not (equal left-id right-id)))
        (disco-state-snowflake< right-id left-id)
      (string-lessp
       (downcase (disco-guild-directory-channel-name left))
       (downcase (disco-guild-directory-channel-name right))))))

(defun disco-guild-directory--active-forum-posts (parent-id)
  "Return sorted active forum/media posts beneath PARENT-ID."
  (sort
   (seq-filter
    (lambda (thread)
      (and (not (disco-thread-archived-p thread))
           (disco-guild-directory-displayable-channel-p thread)))
    (disco-state-parent-threads parent-id))
   #'disco-guild-directory--thread-before-p))

(defun disco-guild-directory--visible-forum-posts
    (context channel &optional ignore-name-filter)
  "Return CONTEXT's sorted visible active posts beneath forum/media CHANNEL.

IGNORE-NAME-FILTER retains unread and permission lenses while showing every
child name."
  (let ((ignore-name-filter
         (or ignore-name-filter
             (and (disco-guild-directory-context-filter context)
                  (disco-guild-directory--name-matches-p context channel)))))
    (sort
     (seq-filter
      (lambda (thread)
        (and (not (disco-thread-archived-p thread))
             (or (disco-thread-starter-message thread)
                 (disco-directory-parent-thread-starter-unavailable-p
                  (alist-get 'id channel) (alist-get 'id thread)))
             (disco-guild-directory--channel-matches-lens-p
              context thread ignore-name-filter)))
      (disco-state-parent-threads (alist-get 'id channel)))
     #'disco-guild-directory--thread-before-p)))

(defun disco-guild-directory--channel-visible-p
    (context channel &optional ignore-name-filter)
  "Return non-nil when CHANNEL belongs in CONTEXT's current lens."
  (if (disco-channel-forum-or-media-p channel)
      (or (and (disco-guild-directory-displayable-channel-p channel)
               (or ignore-name-filter
                   (disco-guild-directory--name-matches-p context channel))
               (not (disco-guild-directory-context-unread-only context)))
          (disco-guild-directory--visible-forum-posts
           context channel ignore-name-filter))
    (disco-guild-directory--channel-matches-lens-p
     context channel ignore-name-filter)))

(defun disco-guild-directory--thread-parent-expanded-p (context parent-id)
  "Return non-nil when PARENT-ID exposes inline active threads in CONTEXT."
  (appkit-directory-fold-expanded-p
   (disco-guild-directory-context-surface context)
   (disco-guild-directory-thread-parent-key context parent-id)
   nil
   (disco-guild-directory--filter-active-p context)))

(defun disco-guild-directory--channel-stamp (channel)
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

(defun disco-guild-directory--channel-group-id (channel)
  "Return the raw category/synthetic group ID owning CHANNEL."
  (let* ((parent-id
          (disco-guild-directory--normalize-id (alist-get 'parent_id channel)))
         (parent (and parent-id (disco-state-channel parent-id)))
         (category-id
          (if (and parent (disco-state-channel-thread-p channel))
              (disco-guild-directory--normalize-id
               (alist-get 'parent_id parent))
            parent-id)))
    (or category-id disco-guild-directory-uncategorized-group)))

(defun disco-guild-directory--channel-thread-parent-id (channel)
  "Return CHANNEL's forum/media parent ID, or nil."
  (cond
   ((disco-channel-forum-or-media-p channel)
    (disco-guild-directory--normalize-id (alist-get 'id channel)))
   ((disco-state-channel-thread-p channel)
    (let* ((parent-id
            (disco-guild-directory--normalize-id
             (alist-get 'parent_id channel)))
           (parent (and parent-id (disco-state-channel parent-id))))
      (and (disco-channel-forum-or-media-p parent) parent-id)))))

(defun disco-guild-directory--channel-help-echo (channel)
  "Return hover help describing CHANNEL's supported open action."
  (let ((channel-id (alist-get 'id channel))
        (mode (disco-channel-open-mode channel)))
    (cond
     ((eq mode 'thread-directory)
       (format "Expand active posts under channel %s in its guild directory"
               channel-id))
     ((eq mode 'inspect) (format "Inspect channel %s" channel-id))
     (mode (format "Open channel %s" channel-id)))))

(defun disco-guild-directory--channel-entry (context channel indent)
  "Return one directory entry for CHANNEL at INDENT in CONTEXT."
  (let* ((channel-id
          (disco-guild-directory--normalize-id (alist-get 'id channel)))
         (group-id (disco-guild-directory--channel-group-id channel))
         (thread-parent-id
          (disco-guild-directory--channel-thread-parent-id channel)))
    (appkit-directory-entry-create
     :key (disco-guild-directory-channel-key context channel-id)
     :role 'item
     :section-key (disco-guild-directory-context-section-key context)
     :group-key (disco-guild-directory-group-key context group-id)
     :item-p t
     :unread-p (disco-state-channel-has-unread-p channel)
     :payload channel
     :indent indent
     :stamp (disco-guild-directory--channel-stamp channel)
     :help-echo (disco-guild-directory--channel-help-echo channel)
     :properties
     (disco-guild-directory--properties
      context 'channel group-id thread-parent-id))))

(defun disco-guild-directory--children-unread-count (channels)
  "Return unread mention total represented by parent CHANNELS."
  (let ((total 0))
    (dolist (channel channels total)
      (setq total
            (+ total
               (if (disco-channel-forum-or-media-p channel)
                   (+ (disco-state-channel-own-unread-count channel)
                      (disco-guild-directory--children-unread-count
                       (disco-guild-directory--active-forum-posts
                        (alist-get 'id channel))))
                 (disco-state-channel-effective-unread-count channel)))))))

(defun disco-guild-directory--thread-parent-entry (context channel indent)
  "Return one expandable forum/media CHANNEL entry at INDENT in CONTEXT."
  (let* ((parent-id
          (disco-guild-directory--normalize-id (alist-get 'id channel)))
         (group-id (disco-guild-directory--channel-group-id channel))
         (threads (disco-guild-directory--active-forum-posts parent-id))
         (state (disco-directory-parent-threads-state parent-id))
         (unread-count
          (disco-guild-directory--children-unread-count threads)))
    (appkit-directory-entry-create
     :key (disco-guild-directory-channel-key context parent-id)
     :role 'item
     :section-key (disco-guild-directory-context-section-key context)
     :group-key (disco-guild-directory-group-key context group-id)
     :item-p t
     :unread-p (> unread-count 0)
     :payload channel
     :indent indent
     :foldable-p t
     :fold-key (disco-guild-directory-thread-parent-key context parent-id)
     :fold-default-expanded-p nil
     :expanded-p
     (disco-guild-directory--thread-parent-expanded-p context parent-id)
     :fold-locked-reason
     (and (disco-guild-directory--filter-active-p context)
          "Disco: clear directory lenses before folding active posts")
     :help-echo
     "RET/TAB toggles active posts; g refreshes; A opens archived posts"
     :stamp (list (disco-guild-directory--channel-stamp channel)
                  (plist-get state :status)
                  (plist-get state :phase)
                  (plist-get state :loaded-count)
                  (plist-get state :total)
                  (length threads))
     :properties
     (append
      (disco-guild-directory--properties
       context 'thread-parent group-id parent-id)
      (list 'disco-unread-count unread-count
            'disco-has-unread (> unread-count 0))))))

(defun disco-guild-directory--group-entry
    (context group-id title unread-count expanded)
  "Return CONTEXT category GROUP-ID with TITLE, UNREAD-COUNT, and EXPANDED."
  (let ((key (disco-guild-directory-group-key context group-id)))
    (appkit-directory-entry-create
     :key key
     :role 'group
     :section-key (disco-guild-directory-context-section-key context)
     :label title
     :indent (disco-guild-directory-context-group-indent context)
     :trailing (and (> unread-count 0) (format "  @%d" unread-count))
     :face 'disco-guild-directory-category
     :foldable-p t
     :fold-key key
     :fold-default-expanded-p t
     :expanded-p expanded
     :fold-locked-reason
     (and (disco-guild-directory--filter-active-p context)
          "Disco: clear directory lenses before collapsing categories")
     :help-echo "RET/TAB toggles this category"
     :properties
     (disco-guild-directory--properties context 'group group-id))))

(defun disco-guild-directory--note-entry
    (context note-id text &optional face indent thread-parent-id)
  "Return CONTEXT status NOTE-ID displaying TEXT with FACE at INDENT.

THREAD-PARENT-ID, when non-nil, records the forum/media context of the note."
  (appkit-directory-entry-create
   :key (disco-guild-directory-note-key context note-id)
   :role 'note
   :section-key (disco-guild-directory-context-section-key context)
   :label text
   :indent (or indent
               (disco-guild-directory-context-channel-indent context))
   :face (or face 'shadow)
   :properties
   (disco-guild-directory--properties
    context 'note nil thread-parent-id)))

(defun disco-guild-directory--async-error-message (err)
  "Return a user-facing message extracted from asynchronous ERR."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (format "%S" err)))

(defun disco-guild-directory--prepend-channel-entries
    (context entries channel &optional ignore-name-filter)
  "Prepend CHANNEL and visible forum children to reverse ENTRIES for CONTEXT."
  (let ((channel-indent
         (disco-guild-directory-context-channel-indent context))
        (thread-indent
         (disco-guild-directory-context-thread-indent context)))
    (if (disco-channel-forum-or-media-p channel)
        (let* ((parent-id
                (disco-guild-directory--normalize-id (alist-get 'id channel)))
               (expanded
                (disco-guild-directory--thread-parent-expanded-p
                 context parent-id))
               (threads
                (disco-guild-directory--visible-forum-posts
                 context channel ignore-name-filter))
               (state (disco-directory-parent-threads-state parent-id))
               (status (plist-get state :status)))
          (push (disco-guild-directory--thread-parent-entry
                 context channel channel-indent)
                entries)
          (when expanded
            (pcase status
              ('unloaded
               (push
                (disco-guild-directory--note-entry
                 context (list 'parent-threads parent-id)
                 "Active posts are not loaded." 'shadow thread-indent
                 parent-id)
                entries))
              ('loading
               (let ((loaded (or (plist-get state :loaded-count) 0))
                     (total (plist-get state :total))
                     (phase (plist-get state :phase)))
                 (push
                  (disco-guild-directory--note-entry
                   context (list 'parent-threads parent-id)
                   (cond
                    ((eq phase 'indexing)
                     "Discord is indexing active posts…")
                    ((and (numberp total) (> total 0))
                     (format "Loading active posts… %d/%d" loaded total))
                    ((> loaded 0) (format "Loading active posts… %d" loaded))
                    (t "Loading active posts…"))
                   'shadow thread-indent parent-id)
                  entries)))
              ('error
               (push
                (disco-guild-directory--note-entry
                 context (list 'parent-threads parent-id)
                 (format "Active post loading failed: %s"
                         (disco-guild-directory--async-error-message
                          (plist-get state :error)))
                 'error thread-indent parent-id)
                entries))
              ('loaded
               (unless threads
                 (push
                  (disco-guild-directory--note-entry
                   context (list 'parent-threads parent-id)
                   "No active posts." 'shadow thread-indent parent-id)
                  entries))))
            (dolist (thread threads)
              (push (disco-guild-directory--channel-entry
                     context thread thread-indent)
                    entries))))
      (push (disco-guild-directory--channel-entry
             context channel channel-indent)
            entries)))
  entries)

(defun disco-guild-directory-project-loaded (context)
  "Project CONTEXT's loaded guild state into ordered flat directory entries."
  (unless (disco-guild-directory-context-p context)
    (error "Disco guild loaded projection requires a context"))
  (let* ((guild-id (disco-guild-directory-context-guild-id context))
         (channels (or (disco-state-guild-channels guild-id) '()))
         (categories
          (disco-guild-directory--sort-channels
           (seq-filter
            (lambda (channel)
              (and (alist-get 'id channel)
                   (disco-guild-directory-category-p channel)
                   (disco-state-channel-viewable-p channel nil)))
            channels)))
         (parents
          (disco-guild-directory--sort-channels
           (seq-filter
            (lambda (channel)
              (and (not (disco-guild-directory-category-p channel))
                   (not (disco-state-channel-thread-p channel))
                   (disco-guild-directory-displayable-channel-p channel)))
            channels)))
         (category-table (make-hash-table :test #'equal))
         (children-by-category (make-hash-table :test #'equal))
         uncategorized
         entries)
    (dolist (category categories)
      (when-let* ((category-id
                   (disco-guild-directory--normalize-id
                    (alist-get 'id category))))
        (puthash category-id category category-table)))
    (dolist (channel parents)
      (let ((parent-id
             (disco-guild-directory--normalize-id
              (alist-get 'parent_id channel))))
        (if (and parent-id (gethash parent-id category-table))
            (puthash parent-id
                     (cons channel
                           (gethash parent-id children-by-category))
                     children-by-category)
          (push channel uncategorized))))
    (setq uncategorized (nreverse uncategorized))
    (let ((visible-uncategorized
           (seq-filter
            (lambda (channel)
              (disco-guild-directory--channel-visible-p context channel))
            uncategorized)))
      (when visible-uncategorized
        (let* ((group-id disco-guild-directory-uncategorized-group)
               (expanded
                (disco-guild-directory--group-expanded-p context group-id)))
          (push
           (disco-guild-directory--group-entry
            context group-id "Channels"
            (disco-guild-directory--children-unread-count
             visible-uncategorized)
            expanded)
           entries)
          (when expanded
            (dolist (channel visible-uncategorized)
              (setq entries
                    (disco-guild-directory--prepend-channel-entries
                     context entries channel)))))))
    (dolist (category categories)
      (let* ((category-id
              (disco-guild-directory--normalize-id (alist-get 'id category)))
             (children
              (nreverse (gethash category-id children-by-category)))
             (category-name-matches
              (and (disco-guild-directory-context-filter context)
                   (disco-guild-directory--name-matches-p context category)))
             (visible-children
              (seq-filter
               (lambda (channel)
                 (disco-guild-directory--channel-visible-p
                  context channel category-name-matches))
               children))
             (category-matches
              (disco-guild-directory--name-matches-p context category))
             (visible-p
              (or (not (disco-guild-directory--filter-active-p context))
                  category-matches visible-children)))
        (when visible-p
          (let ((expanded
                 (disco-guild-directory--group-expanded-p
                  context category-id)))
            (push
             (disco-guild-directory--group-entry
              context category-id
              (disco-guild-directory-channel-name category)
              (disco-guild-directory--children-unread-count visible-children)
              expanded)
             entries)
            (when expanded
              (dolist (channel visible-children)
                (setq entries
                      (disco-guild-directory--prepend-channel-entries
                       context entries channel category-name-matches))))))))
    (or (nreverse entries)
        (list
         (disco-guild-directory--note-entry
          context 'empty
          (if (disco-guild-directory--filter-active-p context)
              "No channels match the active directory lens."
            "This guild has no visible channels."))))))

(defun disco-guild-directory-project (context)
  "Project CONTEXT including guild channel-snapshot lifecycle state."
  (unless (disco-guild-directory-context-p context)
    (error "Disco guild projection requires a context"))
  (let* ((guild-id (disco-guild-directory-context-guild-id context))
         (guild (disco-guild-directory--guild context))
         (status (disco-directory-guild-status guild-id)))
    (cond
     ((null guild)
      (list (disco-guild-directory--note-entry
             context 'missing-guild
             "This guild is no longer available." 'warning)))
     ((and (eq status 'loading)
           (not (disco-state-guild-channels-loaded-p guild-id)))
      (list (disco-guild-directory--note-entry
             context 'loading "Loading channels…")))
     ((eq status 'error)
      (list (disco-guild-directory--note-entry
             context 'load-error "Channel loading failed." 'error)))
     ((not (disco-state-guild-channels-loaded-p guild-id))
      (list (disco-guild-directory--note-entry
             context 'unloaded "Channels have not been loaded.")))
     (t (disco-guild-directory-project-loaded context)))))

(provide 'disco-guild-directory)

;;; disco-guild-directory.el ends here
