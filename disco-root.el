;;; disco-root.el --- Root buffer for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root dashboard showing guilds and channels, inspired by telega/ement
;; list-driven navigation style.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'disco-ui)
(require 'disco-view)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-room)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-permission)
(require 'disco-transient)

(defconst disco-root-buffer-name "*disco*"
  "Main root buffer name.")

(defvar-local disco-root--archived-parent-channel nil
  "Parent channel object for archived thread list buffers.")

(defvar-local disco-root--parent-threads-parent-channel nil
  "Parent channel object for active-thread list buffers.")

(defvar-local disco-root--parent-threads-refresh-generation 0
  "Monotonic generation counter for parent-thread refresh callbacks.")

(defvar-local disco-root--parent-threads-refresh-in-flight nil
  "Non-nil while async active-thread fetch for parent-thread buffer runs.")

(defconst disco-root--archived-thread-sources
  '(("public" . disco-api-channel-archived-public-threads)
    ("private" . disco-api-channel-archived-private-threads)
    ("joined-private" . disco-api-channel-joined-private-archived-threads))
  "Archived thread source endpoints used for per-source pagination.")

(defvar-local disco-root--archived-before-cursors nil
  "Alist source-name -> before cursor for archived pagination.")

(defvar-local disco-root--archived-source-has-more nil
  "Alist source-name -> non-nil when source may have more archived pages.")

(defvar-local disco-root--archived-threads-cache nil
  "Accumulated archived thread list for current archived buffer.")

(defvar-local disco-root--archived-last-errors nil
  "Latest archived pagination errors for current archived buffer.")

(defvar-local disco-root--gateway-handler nil
  "Buffer-local gateway event handler closure.")

(defvar-local disco-root--refresh-generation 0
  "Monotonic generation counter for async refresh callbacks.")

(defvar-local disco-root--refresh-in-flight nil
  "Non-nil while an async root refresh is in progress.")

(defvar-local disco-root--sort-mode 'activity
  "Root channel sorting mode.

Supported values: `activity' and `name'.")

(defvar-local disco-root--view-mode 'all
  "Root visibility mode.

Supported values: `all', `unread', and `dms'.")

(defvar-local disco-root--ewoc nil
  "EWOC used to render the root tree list incrementally.")

(defvar-local disco-root--channel-node-table nil
  "Hash table mapping channel IDs to root EWOC node lists.")

(defconst disco-root--section-order '(unread private guilds)
  "Top-level section order for root buffer rendering.")

(defvar-local disco-root--section-expanded nil
  "Alist of section expansion state in root buffer.

Each entry is (SECTION-SYMBOL . BOOLEAN).")

(defvar-local disco-root--guild-expanded nil
  "Hash table guild-id -> expansion state for guild rows.")

(defvar-local disco-root--category-expanded nil
  "Hash table category-channel-id -> expansion state for category rows.")

(defcustom disco-root-show-guild-icons t
  "When non-nil, show guild icons in root guild rows when available."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-guild-icon-size 18
  "Pixel size used for inline guild icons in root rows."
  :type 'integer
  :group 'disco)

(defvar disco-root--guild-icon-image-cache (make-hash-table :test #'equal)
  "Global guild icon image cache keyed by guild icon cache key.

Values are image objects or the symbol `:missing'.")

(defvar disco-root--guild-icon-fetching (make-hash-table :test #'equal)
  "Global set of guild icon cache keys currently being fetched.")

(defun disco-root--live-event-p (event-type)
  "Return non-nil when EVENT-TYPE should trigger root rerender."
  (memq event-type
        '(message-create message-ack
          channel-create channel-update channel-delete channel-update-partial
          channel-unread-update passive-update-v1 passive-update-v2
          guild-create guild-update guild-delete guild-sync
          thread-create thread-update thread-delete thread-list-sync)))

(defun disco-root--render-preserving-position ()
  "Render root tree and keep point near previous line/column."
  (disco-view-render-preserving-position
   #'disco-root-render
   :preserve-window-start t))

(defun disco-root--ensure-section-state ()
  "Ensure `disco-root--section-expanded' has defaults for all sections."
  (dolist (section disco-root--section-order)
    (unless (assq section disco-root--section-expanded)
      (push (cons section t) disco-root--section-expanded)))
  (setq disco-root--section-expanded
        (nreverse (seq-uniq (nreverse disco-root--section-expanded)
                            (lambda (a b) (eq (car a) (car b)))))))

(defun disco-root--section-expanded-p (section)
  "Return non-nil when SECTION is currently expanded."
  (if-let* ((it (assq section disco-root--section-expanded)))
      (and (cdr it) t)
    t))

(defun disco-root--set-section-expanded (section expanded)
  "Set SECTION expansion state to EXPANDED and return EXPANDED."
  (if-let* ((it (assq section disco-root--section-expanded)))
      (setcdr it (and expanded t))
    (push (cons section (and expanded t)) disco-root--section-expanded))
  (and expanded t))

(defun disco-root--ensure-collapse-state-tables ()
  "Ensure guild/category expansion tables are initialized in current buffer."
  (unless (hash-table-p disco-root--guild-expanded)
    (setq disco-root--guild-expanded (make-hash-table :test #'equal)))
  (unless (hash-table-p disco-root--category-expanded)
    (setq disco-root--category-expanded (make-hash-table :test #'equal))))

(defun disco-root--node-expanded-p (table key)
  "Return expansion state from TABLE for KEY, defaulting to expanded."
  (disco-root--ensure-collapse-state-tables)
  (if (null key)
      t
    (let ((value (gethash key table '__disco-missing)))
      (if (eq value '__disco-missing)
          t
        (and value t)))))

(defun disco-root--set-node-expanded (table key expanded)
  "Store EXPANDED for KEY in TABLE and return normalized state."
  (disco-root--ensure-collapse-state-tables)
  (when key
    (puthash key (and expanded t) table))
  (and expanded t))

(defun disco-root--guild-expanded-p (guild-id)
  "Return non-nil when GUILD-ID row is expanded."
  (disco-root--node-expanded-p disco-root--guild-expanded guild-id))

(defun disco-root--set-guild-expanded (guild-id expanded)
  "Set GUILD-ID expansion to EXPANDED and return resulting state."
  (disco-root--set-node-expanded disco-root--guild-expanded guild-id expanded))

(defun disco-root--category-expanded-p (category-id)
  "Return non-nil when CATEGORY-ID row is expanded."
  (disco-root--node-expanded-p disco-root--category-expanded category-id))

(defun disco-root--set-category-expanded (category-id expanded)
  "Set CATEGORY-ID expansion to EXPANDED and return resulting state."
  (disco-root--set-node-expanded disco-root--category-expanded category-id expanded))

(defun disco-root--line-property (property &optional pos)
  "Return text PROPERTY on current rendered row at POS (or point)."
  (let ((p (or pos (point))))
    (or (get-text-property p property)
        (save-excursion
          (goto-char p)
          (get-text-property (line-beginning-position) property)))))

(defun disco-root--line-section (&optional pos)
  "Return section symbol for row at POS when row is a section header."
  (disco-root--line-property 'disco-root-section pos))

(defun disco-root--line-row-type (&optional pos)
  "Return row type symbol at POS (or point)."
  (disco-root--line-property 'disco-root-row-type pos))

(defun disco-root--line-guild-id (&optional pos)
  "Return guild id for row at POS when row is a guild header."
  (disco-root--line-property 'disco-root-guild-id pos))

(defun disco-root--line-category-id (&optional pos)
  "Return category channel id for row at POS when row is a category header."
  (disco-root--line-property 'disco-root-category-id pos))

(defun disco-root--line-channel-id (&optional pos)
  "Return channel id for row at POS when row is channel/thread row."
  (disco-root--line-property 'disco-channel-id pos))

(defun disco-root--line-unread-count (&optional pos)
  "Return unread count for row at POS, defaulting to 0."
  (or (disco-root--line-property 'disco-unread-count pos) 0))

(defun disco-root--channel-line-positions (&optional predicate)
  "Return ordered list of channel row positions.

When PREDICATE is non-nil, include row only when (PREDICATE POS) is non-nil."
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (and (disco-root--line-channel-id (point))
                   (or (null predicate)
                       (funcall predicate (point))))
          (push (line-beginning-position) positions))
        (forward-line 1)))
    (nreverse positions)))

(defun disco-root--next-position-after (positions cursor)
  "Return first position in POSITIONS strictly after CURSOR."
  (seq-find (lambda (pos) (> pos cursor)) positions))

(defun disco-root--previous-position-before (positions cursor)
  "Return last position in POSITIONS strictly before CURSOR."
  (let (found)
    (dolist (pos positions found)
      (when (< pos cursor)
        (setq found pos)))))

(defun disco-root--toggle-section (section)
  "Toggle expansion state of SECTION and rerender root buffer."
  (interactive)
  (disco-root--set-section-expanded section
                                    (not (disco-root--section-expanded-p section)))
  (disco-root--render-preserving-position)
  (message "disco: section %s -> %s"
           section
           (if (disco-root--section-expanded-p section) "expanded" "collapsed")))

(defun disco-root--toggle-guild (guild-id)
  "Toggle expansion state of GUILD-ID and rerender root buffer."
  (interactive)
  (unless guild-id
    (user-error "disco: missing guild id at point"))
  (disco-root--set-guild-expanded
   guild-id
   (not (disco-root--guild-expanded-p guild-id)))
  (disco-root--render-preserving-position)
  (message "disco: guild %s -> %s"
           guild-id
           (if (disco-root--guild-expanded-p guild-id) "expanded" "collapsed")))

(defun disco-root--toggle-category (category-id)
  "Toggle expansion state of CATEGORY-ID and rerender root buffer."
  (interactive)
  (unless category-id
    (user-error "disco: missing category id at point"))
  (disco-root--set-category-expanded
   category-id
   (not (disco-root--category-expanded-p category-id)))
  (disco-root--render-preserving-position)
  (message "disco: category %s -> %s"
           category-id
           (if (disco-root--category-expanded-p category-id) "expanded" "collapsed")))

(defun disco-root--toggle-node-at-point ()
  "Toggle collapsible node at point: section, guild, or category."
  (let ((section (disco-root--line-section))
        (guild-id (disco-root--line-guild-id))
        (category-id (disco-root--line-category-id)))
    (cond
     (section
      (disco-root--toggle-section section))
     (guild-id
      (disco-root--toggle-guild guild-id))
     (category-id
      (disco-root--toggle-category category-id))
     (t nil))))

(defun disco-root-toggle-section-at-point ()
  "Toggle collapsible node at point.

Supports top-level sections, guild rows, and category rows." 
  (interactive)
  (unless (disco-root--toggle-node-at-point)
    (user-error "disco: point is not on a collapsible row")))

(defun disco-root-tab-dwim ()
  "On collapsible row, toggle it; otherwise move to next channel row."
  (interactive)
  (unless (disco-root--toggle-node-at-point)
    (disco-root-button-forward 1)))

(defun disco-root--refresh-channel-node (channel-id)
  "Refresh one CHANNEL-ID row in EWOC, returning non-nil on success."
  (let ((nodes (and channel-id
                    disco-root--channel-node-table
                    (gethash channel-id disco-root--channel-node-table))))
    (when (and nodes disco-root--ewoc)
      (let ((channel (disco-state-channel channel-id))
            updated)
        (when (and channel (disco-root--displayable-channel-p channel))
          (let ((inhibit-read-only t))
            (dolist (node (if (listp nodes) nodes (list nodes)))
              (let ((entry (copy-sequence (ewoc-data node))))
                (setq entry (plist-put entry :channel channel))
                (ewoc-set-data node entry)
                (ewoc-invalidate disco-root--ewoc node)
                (setq updated t))))
          updated)))))

(defun disco-root--handle-gateway-event (event)
  "Apply one gateway EVENT to root buffer view."
  (when (disco-root--live-event-p (plist-get event :type))
    (let ((event-type (plist-get event :type))
          (channel-id (plist-get event :channel-id)))
      (if (memq event-type '(message-create message-ack))
          (unless (disco-root--refresh-channel-node channel-id)
            (disco-root--render-preserving-position))
        (disco-root--render-preserving-position)))))

(defun disco-root--attach-live-updates ()
  "Attach root buffer to global gateway update stream."
  (when disco-root--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-root--gateway-handler)
    (disco-gateway-unwatch-global))
  (let ((root-buffer (current-buffer)))
    (setq disco-root--gateway-handler
          (lambda (event)
            (when (buffer-live-p root-buffer)
              (with-current-buffer root-buffer
                (disco-root--handle-gateway-event event))))))
  (add-hook 'disco-gateway-event-hook disco-root--gateway-handler)
  (disco-gateway-watch-global)
  (add-hook 'kill-buffer-hook #'disco-root--detach-live-updates nil t))

(defun disco-root--detach-live-updates ()
  "Detach root buffer from global gateway update stream."
  (when disco-root--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-root--gateway-handler)
    (setq disco-root--gateway-handler nil))
  (disco-gateway-unwatch-global))

(defun disco-root--json-true-p (value)
  "Return non-nil when VALUE semantically represents JSON true."
  (or (eq value t)
      (eq value 'true)
      (equal value "true")))

(defun disco-root--parse-decimal-integer (value)
  "Parse decimal integer VALUE and return integer or nil."
  (cond
   ((integerp value)
    value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))
   (t nil)))

(defun disco-root--channel-viewable-p (channel)
  "Return non-nil when CHANNEL should be visible to current user.

For guild channels, computed `permissions' is used when available.
Channels lacking this field are treated as visible to avoid false negatives."
  (disco-permission-channel-viewable-p channel t))

(defun disco-root--archived-source-fetch-allowed-p (source-name parent-channel)
  "Return non-nil when archived SOURCE-NAME is expected to be fetchable.

This prevents noisy permission errors for sources that require elevated access."
  (cond
   ;; Discord private archived thread listing requires MANAGE_THREADS.
   ((equal source-name "private")
    (disco-permission-channel-has-p parent-channel 'manage-threads nil))
   (t t)))

(defun disco-root--archived-missing-access-error-p (err)
  "Return non-nil when ERR is a Discord missing-access response."
  (and (consp err)
       (eq (car err) 'disco-api-error)
       (let* ((data (cdr err))
              (status (nth 1 data))
              (body (nth 2 data))
              (code (and (listp body) (alist-get 'code body)))
              (message (and (listp body) (alist-get 'message body))))
         (and (equal status 403)
              (or (equal code 50001)
                  (equal message "Missing Access"))))))

(defun disco-root--displayable-channel-p (channel)
  "Return non-nil when CHANNEL should appear in root buffer."
  (and (memq (alist-get 'type channel) '(0 1 3 5 10 11 12 15 16))
       (disco-root--channel-viewable-p channel)))

(defun disco-root--openable-channel-p (channel)
  "Return non-nil when CHANNEL can be opened as a room timeline."
  (memq (alist-get 'type channel) '(0 1 3 5 10 11 12 15 16)))

(defun disco-root--thread-parent-channel-p (channel)
  "Return non-nil when CHANNEL can contain visible threads in UI."
  (disco-thread-parent-channel-p channel))

(defun disco-root--forum-or-media-channel-p (channel)
  "Return non-nil when CHANNEL is a forum/media parent channel."
  (disco-thread-forum-or-media-channel-p channel))

(defun disco-root--thread-metadata (channel)
  "Return thread metadata for CHANNEL."
  (disco-thread-metadata channel))

(defun disco-root--thread-status-tags (thread)
  "Return comma-joined status tags for THREAD."
  (disco-thread-status-string thread))

(defun disco-root--recipient-display-name (recipient)
  "Return best display name for one DM RECIPIENT user object."
  (or (alist-get 'global_name recipient)
      (alist-get 'username recipient)
      (alist-get 'id recipient)
      "unknown-user"))

(defun disco-root--private-channel-display-name (channel)
  "Return best display name for private CHANNEL."
  (let* ((channel-type (alist-get 'type channel))
         (explicit-name (and (stringp (alist-get 'name channel))
                             (not (string-empty-p (alist-get 'name channel)))
                             (alist-get 'name channel)))
         (recipients (or (alist-get 'recipients channel) '()))
         (recipient-names (delq nil
                                (mapcar (lambda (it)
                                          (when (listp it)
                                            (disco-root--recipient-display-name it)))
                                        recipients))))
    (pcase channel-type
      (1 (or (car recipient-names) explicit-name "direct-message"))
      (3 (or explicit-name
             (and recipient-names (mapconcat #'identity recipient-names ", "))
             "group-dm"))
      (_ (or explicit-name "(no-name)")))))

(defun disco-root--channel-display-name (channel)
  "Return display name for CHANNEL independent of badge suffixes."
  (if (memq (alist-get 'type channel) '(1 3))
      (disco-root--private-channel-display-name channel)
    (or (alist-get 'name channel) "(no-name)")))

(defun disco-root--guild-icon-hash (guild)
  "Return icon hash string from GUILD, or nil when unavailable."
  (let ((icon (alist-get 'icon guild)))
    (and (stringp icon)
         (not (string-empty-p icon))
         icon)))

(defun disco-root--guild-icon-url (guild)
  "Return Discord CDN guild icon URL for GUILD, or nil."
  (let ((guild-id (alist-get 'id guild))
        (icon-hash (disco-root--guild-icon-hash guild)))
    (when (and guild-id icon-hash)
      (format "https://cdn.discordapp.com/icons/%s/%s.png?size=64"
              guild-id icon-hash))))

(defun disco-root--guild-icon-cache-key (guild)
  "Build stable cache key for GUILD icon image."
  (let ((guild-id (alist-get 'id guild))
        (icon-hash (disco-root--guild-icon-hash guild)))
    (when (and guild-id icon-hash)
      (format "%s:%s:%s" guild-id icon-hash disco-root-guild-icon-size))))

(defun disco-root--guild-icon-fallback (guild)
  "Return fallback textual icon for GUILD when image is unavailable."
  (let* ((name (or (alist-get 'name guild) "?"))
         (initial (if (and (stringp name) (> (length name) 0))
                      (upcase (substring name 0 1))
                    "?")))
    (format "[%s]" initial)))

(defun disco-root--guild-icon-image-valid-p (image)
  "Return non-nil when IMAGE object appears renderable."
  (and image
       (ignore-errors (image-size image t) t)))

(defun disco-root--guild-icon-rendering-available-p ()
  "Return non-nil when inline guild icons can be rendered."
  (and disco-root-show-guild-icons
       (display-images-p)
       (image-type-available-p 'png)
       (fboundp 'plz)))

(defun disco-root--rerender-open-root-buffers ()
  "Rerender all live root buffers to refresh async guild icon updates."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq major-mode 'disco-root-mode)
          (disco-root--render-preserving-position))))))

(defun disco-root--start-guild-icon-fetch (cache-key url)
  "Start asynchronous guild icon fetch for CACHE-KEY from URL."
  (unless (or (gethash cache-key disco-root--guild-icon-fetching)
              (gethash cache-key disco-root--guild-icon-image-cache))
    (puthash cache-key t disco-root--guild-icon-fetching)
    (plz 'get url
         :as 'binary
         :headers '(("Accept" . "image/png,image/*;q=0.8,*/*;q=0.1"))
         :then (lambda (bytes)
                 (let ((image
                        (ignore-errors
                          (create-image bytes 'png t
                                        :width disco-root-guild-icon-size
                                        :height disco-root-guild-icon-size
                                        :ascent 'center))))
                   (puthash cache-key
                            (if (disco-root--guild-icon-image-valid-p image)
                                image
                              :missing)
                            disco-root--guild-icon-image-cache)
                   (remhash cache-key disco-root--guild-icon-fetching)
                   (disco-root--rerender-open-root-buffers)))
         :else (lambda (_err)
                 (puthash cache-key :missing disco-root--guild-icon-image-cache)
                 (remhash cache-key disco-root--guild-icon-fetching))))
  nil)

(defun disco-root--guild-icon-image (guild)
  "Return image object for GUILD icon when available, otherwise nil.

Starts asynchronous fetch when cache miss occurs."
  (when (disco-root--guild-icon-rendering-available-p)
    (let* ((cache-key (disco-root--guild-icon-cache-key guild))
           (cached (and cache-key
                        (gethash cache-key disco-root--guild-icon-image-cache))))
      (cond
       ((null cache-key)
        nil)
       ((eq cached :missing)
        nil)
       ((disco-root--guild-icon-image-valid-p cached)
        cached)
       (t
        (let ((url (disco-root--guild-icon-url guild)))
          (when url
            (disco-root--start-guild-icon-fetch cache-key url)))
        nil)))))

(defun disco-root--insert-guild-icon (guild)
  "Insert one guild icon for GUILD, falling back to text when needed."
  (let ((fallback (disco-root--guild-icon-fallback guild))
        (image (disco-root--guild-icon-image guild)))
    (if (disco-root--guild-icon-image-valid-p image)
        (insert-image image fallback)
      (insert fallback))))

(defun disco-root--channel-category-p (channel)
  "Return non-nil when CHANNEL is a guild category container."
  (= (alist-get 'type channel) 4))

(defun disco-root--channel-sort-position (channel)
  "Return sortable numeric position for CHANNEL.

Discord channel position can arrive as integer or numeric string."
  (let ((raw (alist-get 'position channel)))
    (or (disco-root--parse-decimal-integer raw)
        most-positive-fixnum)))

(defun disco-root--sort-categories (categories)
  "Sort category CHANNELS according to current root sort mode."
  (let ((copy (copy-sequence (or categories '()))))
    (pcase disco-root--sort-mode
      ('name
       (sort copy
             (lambda (a b)
               (string-lessp (disco-root--channel-display-name a)
                             (disco-root--channel-display-name b)))))
      (_
       (sort copy
             (lambda (a b)
               (let ((a-pos (disco-root--channel-sort-position a))
                     (b-pos (disco-root--channel-sort-position b)))
                 (if (= a-pos b-pos)
                     (string-lessp (disco-root--channel-display-name a)
                                   (disco-root--channel-display-name b))
                   (< a-pos b-pos)))))))))

(defun disco-root--thread-count-under-parent (channel)
  "Return number of indexed threads under CHANNEL."
  (length (disco-state-parent-threads (alist-get 'id channel))))

(defun disco-root--channel-label (channel)
  "Return display label for CHANNEL."
  (let ((name (disco-root--channel-display-name channel))
        (channel-type (alist-get 'type channel))
        (channel-id (alist-get 'id channel))
        (unread (disco-state-channel-effective-unread-count channel)))
    (let ((unread-suffix (if (> unread 0)
                             (format " [%d]" unread)
                           ""))
          (read-suffix
           (let ((last-read-id (disco-state-channel-last-read-message-id channel-id))
                 (last-message-id (alist-get 'last_message_id channel)))
             (if (and (= unread 0)
                      (disco-state-snowflake>= last-read-id last-message-id))
                 " [read]"
               ""))))
      (pcase channel-type
        (1 (format "[dm] %s%s" name (concat unread-suffix read-suffix)))
        (3 (format "[group] %s%s" name (concat unread-suffix read-suffix)))
        ((or 10 11 12)
         (let ((tags (disco-root--thread-status-tags channel)))
           (format "[thread] %s%s%s"
                   name
                   (if (string-empty-p tags)
                       ""
                     (format " (%s)" tags))
                   (concat unread-suffix read-suffix))))
        ((or 0 5 15 16)
         (let* ((thread-count (disco-root--thread-count-under-parent channel))
                (suffix (if (> thread-count 0)
                            (format " (%d threads)" thread-count)
                          "")))
           (pcase channel-type
             ((or 0 5) (format "#%s%s%s" name suffix (concat unread-suffix read-suffix)))
             (15 (format "[forum] %s%s%s" name suffix (concat unread-suffix read-suffix)))
             (16 (format "[media] %s%s%s" name suffix (concat unread-suffix read-suffix))))))
        (_ (format "[type-%s] %s%s" channel-type name (concat unread-suffix read-suffix)))))))

(defun disco-root--channel-has-unread-p (channel)
  "Return non-nil when CHANNEL has unread messages tracked locally."
  (disco-state-channel-has-unread-p channel))

(defun disco-root--channel-visible-in-view-p (channel)
  "Return non-nil when CHANNEL should appear under current view mode."
  (pcase disco-root--view-mode
    ('unread
     (disco-root--channel-has-unread-p channel))
    ('dms
     (memq (alist-get 'type channel) '(1 3)))
    (_ t)))

(defun disco-root--private-channels-sorted ()
  "Return private channels sorted by recency (newest first)."
  (sort (copy-sequence (disco-state-private-channels))
        (lambda (a b)
          (let ((a-last (alist-get 'last_message_id a))
                (b-last (alist-get 'last_message_id b)))
            (cond
             ((and (stringp a-last) (stringp b-last))
              (disco-state-snowflake< b-last a-last))
             ((stringp a-last) t)
             ((stringp b-last) nil)
             (t (string-lessp (or (alist-get 'id a) "")
                              (or (alist-get 'id b) ""))))))))

(defun disco-root--visible-private-channels ()
  "Return private channels that match current root view mode."
  (seq-filter #'disco-root--channel-visible-in-view-p
              (disco-root--private-channels-sorted)))

(defun disco-root--collect-visible-unread-channels ()
  "Return unique unread channels visible in current root view.

Includes private channels, guild channels and thread channels, sorted by
current sort mode."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (cl-labels
        ((push-unique (channel)
           (let ((channel-id (alist-get 'id channel)))
             (when (and channel-id
                        (not (gethash channel-id seen))
                        (disco-root--displayable-channel-p channel)
                        (disco-root--channel-visible-in-view-p channel)
                        (disco-root--channel-has-unread-p channel))
               (puthash channel-id t seen)
               (push channel result)))))
      (dolist (channel (disco-state-private-channels))
        (push-unique channel))
      (dolist (guild (or (disco-state-guilds) '()))
        (let ((guild-id (alist-get 'id guild)))
          (dolist (channel (or (disco-state-guild-channels guild-id) '()))
            (push-unique channel))
          (dolist (thread (or (disco-state-guild-threads guild-id) '()))
            (push-unique thread)))))
    (disco-root--sort-channels (nreverse result))))

(defun disco-root--guild-unread-total (guild-id &optional visible-only)
  "Return aggregated unread count for GUILD-ID.

When VISIBLE-ONLY is non-nil, only count channels visible in current view."
  (let ((channels
         (append (or (disco-state-guild-channels guild-id) '())
                 (or (disco-state-guild-threads guild-id) '()))))
    (when visible-only
      (setq channels (seq-filter #'disco-root--channel-visible-in-view-p channels)))
    (disco-state-channels-unread-total channels)))

(defun disco-root--channel-activity-score (channel)
  "Return sortable activity score for CHANNEL.

Higher score means channel should appear earlier in activity mode."
  (+ (* 1000 (disco-state-channel-unread-count (alist-get 'id channel)))
     (if (stringp (alist-get 'last_message_id channel))
         (string-to-number (alist-get 'last_message_id channel))
       0)))

(defun disco-root--sort-channels (channels)
  "Sort CHANNELS according to current root sort mode."
  (let ((copy (copy-sequence (or channels '()))))
    (pcase disco-root--sort-mode
      ('name
       (sort copy
             (lambda (a b)
               (string-lessp (disco-root--channel-display-name a)
                             (disco-root--channel-display-name b)))))
      (_
       (sort copy
             (lambda (a b)
               (let ((a-score (disco-root--channel-activity-score a))
                     (b-score (disco-root--channel-activity-score b)))
                 (if (= a-score b-score)
                     (string-lessp (disco-root--channel-display-name a)
                                   (disco-root--channel-display-name b))
                   (> a-score b-score)))))))))

(defun disco-root-toggle-sort-mode ()
  "Toggle root channel sort mode between activity and name."
  (interactive)
  (setq disco-root--sort-mode (if (eq disco-root--sort-mode 'activity) 'name 'activity))
  (disco-root-render)
  (message "disco: root sort mode -> %s" disco-root--sort-mode))

(defun disco-root-cycle-view-mode ()
  "Cycle root view mode across all, unread, and dms."
  (interactive)
  (setq disco-root--view-mode
        (pcase disco-root--view-mode
          ('all 'unread)
          ('unread 'dms)
          (_ 'all)))
  (disco-root-render)
  (message "disco: root view mode -> %s" disco-root--view-mode))

(defun disco-root--ewoc-printer (entry)
  "Pretty-printer for one root EWOC ENTRY."
  (pcase (plist-get entry :entry-type)
    ('section
     (let* ((section (plist-get entry :section))
            (title (or (plist-get entry :title) "Section"))
            (count (plist-get entry :count))
            (expanded (disco-root--section-expanded-p section))
            (indicator (if expanded "[-]" "[+]"))
            (suffix (if (numberp count) (format " (%d)" count) ""))
            (label (format "%s %s%s\n" indicator title suffix))
            (start (point)))
       (insert label)
       (add-text-properties
        start
        (point)
        (list 'face (if expanded 'font-lock-keyword-face 'shadow)
              'mouse-face 'highlight
              'help-echo "RET or TAB toggles this section"
              'disco-root-row-type 'section
              'disco-root-section section))))
    ('guild
     (let* ((guild (plist-get entry :guild))
            (guild-id (alist-get 'id guild))
            (guild-name (or (alist-get 'name guild) "(unnamed-guild)"))
            (unread (or (plist-get entry :unread-count) 0))
            (expanded (disco-root--guild-expanded-p guild-id))
            (indicator (if expanded "[-]" "[+]"))
            (suffix (if (> unread 0) (format " [%d]" unread) ""))
            (start (point)))
       (insert (format "  %s " indicator))
       (disco-root--insert-guild-icon guild)
       (insert (format " %s%s\n" guild-name suffix))
       (add-text-properties
        start
        (point)
        (list 'face 'font-lock-function-name-face
              'mouse-face 'highlight
              'help-echo "RET/TAB/t toggles this guild"
              'disco-root-row-type 'guild
              'disco-root-guild-id guild-id))))
    ('category
     (let* ((category (plist-get entry :category))
            (category-id (alist-get 'id category))
            (category-name (or (disco-root--channel-display-name category)
                               "(unnamed-category)"))
            (unread (or (plist-get entry :unread-count) 0))
            (expanded (disco-root--category-expanded-p category-id))
            (indicator (if expanded "[-]" "[+]"))
            (suffix (if (> unread 0) (format " [%d]" unread) ""))
            (start (point)))
       (insert (format "    %s [category] %s%s\n" indicator category-name suffix))
       (add-text-properties
        start
        (point)
        (list 'face 'font-lock-keyword-face
              'mouse-face 'highlight
              'help-echo "RET/TAB/t toggles this category"
              'disco-root-row-type 'category
              'disco-root-category-id category-id))))
    ('text
     (insert (or (plist-get entry :text) "") "\n"))
    ('blank
     (insert "\n"))
    ('channel
     (disco-root--insert-channel-line
      (plist-get entry :channel)
      (or (plist-get entry :indent) 0)))
    (_
     (insert "\n"))))

(defun disco-root--ewoc-insert-text (text)
  "Insert one plain TEXT row in root EWOC."
  (ewoc-enter-last disco-root--ewoc
                   (list :entry-type 'text :text text)))

(defun disco-root--ewoc-insert-section (section title &optional count)
  "Insert one clickable SECTION row with TITLE and optional COUNT."
  (ewoc-enter-last disco-root--ewoc
                   (list :entry-type 'section
                         :section section
                         :title title
                         :count count)))

(defun disco-root--ewoc-insert-guild (guild unread-count)
  "Insert one collapsible GUILD row with UNREAD-COUNT badge."
  (ewoc-enter-last disco-root--ewoc
                   (list :entry-type 'guild
                         :guild guild
                         :unread-count unread-count)))

(defun disco-root--ewoc-insert-category (category unread-count)
  "Insert one collapsible CATEGORY row with UNREAD-COUNT badge."
  (ewoc-enter-last disco-root--ewoc
                   (list :entry-type 'category
                         :category category
                         :unread-count unread-count)))

(defun disco-root--ewoc-insert-blank ()
  "Insert one blank row in root EWOC."
  (ewoc-enter-last disco-root--ewoc (list :entry-type 'blank)))

(defun disco-root--ewoc-insert-channel (channel indent)
  "Insert CHANNEL row at INDENT into root EWOC and index node by channel ID."
  (let* ((entry (list :entry-type 'channel :channel channel :indent indent))
         (node (ewoc-enter-last disco-root--ewoc entry))
         (channel-id (alist-get 'id channel)))
    (when channel-id
      (let ((existing (gethash channel-id disco-root--channel-node-table)))
        (puthash channel-id
                 (cons node (if (listp existing)
                                existing
                              (and existing (list existing))))
                 disco-root--channel-node-table)))
    node))

(defun disco-root--insert-private-channels ()
  "Insert visible private-channel (DM/group DM) rows into root buffer."
  (let ((channels (disco-root--visible-private-channels)))
    (if channels
        (dolist (channel channels)
          (disco-root--ewoc-insert-channel channel 2))
      (disco-root--ewoc-insert-text "  (no direct messages loaded)"))))

(defun disco-root--guild-name-by-id (guild-id)
  "Return guild display name for GUILD-ID."
  (let ((guild
         (seq-find (lambda (it)
                     (equal (alist-get 'id it) guild-id))
                   (disco-state-guilds))))
    (or (alist-get 'name guild) guild-id "unknown-guild")))

(defun disco-root--thread-parent-candidates ()
  "Return list of (DISPLAY . CHANNEL) suitable for archived thread lookup."
  (let (candidates)
    (dolist (guild (or (disco-state-guilds) '()))
      (let* ((guild-id (alist-get 'id guild))
             (guild-name (or (alist-get 'name guild) guild-id "unknown-guild")))
        (dolist (channel (disco-state-guild-channels guild-id))
          (when (and (disco-root--thread-parent-channel-p channel)
                     (disco-root--channel-viewable-p channel))
            (push (cons (format "%s / %s (%s)"
                                guild-name
                                (disco-root--channel-label channel)
                                (alist-get 'id channel))
                        channel)
                  candidates)))))
    (nreverse candidates)))

(defun disco-root--read-thread-parent-channel ()
  "Prompt user to select one parent channel and return its channel object."
  (let* ((candidates (disco-root--thread-parent-candidates))
         (choice (and candidates
                      (completing-read "Parent channel: " (mapcar #'car candidates) nil t))))
    (unless choice
      (user-error "disco: no parent channels available"))
    (or (cdr (assoc choice candidates))
        (user-error "disco: invalid channel selection"))))

(defun disco-root--thread-archive-sort-key (thread)
  "Return sort key for THREAD archive timestamp ordering."
  (or (alist-get 'archive_timestamp (disco-root--thread-metadata thread))
      ""))

(defun disco-root--dedupe-threads (threads)
  "Return THREADS deduped by channel ID."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (thread threads)
      (let ((thread-id (alist-get 'id thread)))
        (when (and thread-id (not (gethash thread-id seen)))
          (puthash thread-id t seen)
          (push thread result))))
    (nreverse result)))

(defun disco-root--sort-threads-by-archive-time (threads)
  "Return THREADS sorted descending by archive timestamp."
  (sort threads
        (lambda (a b)
          (string>
           (disco-root--thread-archive-sort-key a)
           (disco-root--thread-archive-sort-key b)))))

(defun disco-root--reset-archived-pagination-state ()
  "Reset archived-thread pagination state for current archived buffer."
  (setq disco-root--archived-before-cursors nil)
  (setq disco-root--archived-source-has-more nil)
  (setq disco-root--archived-threads-cache nil)
  (setq disco-root--archived-last-errors nil)
  (dolist (source disco-root--archived-thread-sources)
    (let ((name (car source)))
      (push (cons name nil) disco-root--archived-before-cursors)
      (push (cons name t) disco-root--archived-source-has-more)))
  (setq disco-root--archived-before-cursors
        (nreverse disco-root--archived-before-cursors))
  (setq disco-root--archived-source-has-more
        (nreverse disco-root--archived-source-has-more)))

(defun disco-root--archived-next-before-cursor (source-name threads)
  "Compute next BEFORE cursor for SOURCE-NAME from THREADS page."
  (let ((last-thread (car (last threads))))
    (when last-thread
      (if (equal source-name "joined-private")
          (alist-get 'id last-thread)
        (or (alist-get 'archive_timestamp (disco-root--thread-metadata last-thread))
            (alist-get 'archive_timestamp last-thread))))))

(defun disco-root--archived-any-source-has-more-p ()
  "Return non-nil when at least one archived source may return more pages."
  (seq-some #'cdr disco-root--archived-source-has-more))

(defun disco-root--archived-source-status-string ()
  "Return human-readable per-source pagination status string."
  (mapconcat
   (lambda (source)
     (let* ((name (car source))
            (has-more (alist-get name disco-root--archived-source-has-more nil nil #'equal)))
       (format "%s:%s" name (if has-more "more" "end"))))
   disco-root--archived-thread-sources
   "  "))

(defun disco-root--fetch-archived-source-page (source-name source-fn parent-channel-id before)
  "Fetch one archived page for SOURCE-NAME using SOURCE-FN.

Return plist with keys:
- `:threads' list
- `:has-more' boolean
- `:next-before' cursor (or nil)
- `:missing-access' when 403/50001
- `:error' human-readable error string."
  (condition-case err
      (let* ((resp (funcall source-fn parent-channel-id before disco-thread-archive-fetch-limit))
             (threads (or (alist-get 'threads resp) '()))
             (has-more (disco-root--json-true-p (alist-get 'has_more resp)))
             (next-before (disco-root--archived-next-before-cursor source-name threads)))
        (when (and has-more (null threads))
          ;; Prevent endless pagination loops when server returns
          ;; an empty page without advancing cursor semantics.
          (setq has-more nil))
        (list :threads threads
              :has-more has-more
              :next-before next-before))
    (error
     (if (disco-root--archived-missing-access-error-p err)
         (list :missing-access t)
       (list :error (error-message-string err))))))

(defun disco-root--fetch-archived-threads-page (parent-channel-id &optional reset)
  "Fetch one archived thread page for PARENT-CHANNEL-ID.

When RESET is non-nil, start pagination from first page for all sources.
Return plist with keys :threads and :errors for this page only."
  (when reset
    (disco-root--reset-archived-pagination-state))
  (let ((parent-channel (disco-state-channel parent-channel-id))
        page-threads
        errors)
    (dolist (source disco-root--archived-thread-sources)
      (let* ((source-name (car source))
             (source-fn (cdr source))
             (source-allowed
              (disco-root--archived-source-fetch-allowed-p source-name parent-channel))
             (should-fetch
              (or reset
                  (alist-get source-name
                             disco-root--archived-source-has-more
                             nil nil #'equal))))
        (cond
         ((not source-allowed)
          (setf (alist-get source-name disco-root--archived-source-has-more nil nil #'equal)
                nil))
         ((not should-fetch)
          nil)
         (t
          (let* ((before (alist-get source-name disco-root--archived-before-cursors nil nil #'equal))
                 (result (disco-root--fetch-archived-source-page
                          source-name source-fn parent-channel-id before))
                 (threads (or (plist-get result :threads) '()))
                 (has-more (plist-get result :has-more))
                 (next-before (plist-get result :next-before))
                 (missing-access (plist-get result :missing-access))
                 (error-text (plist-get result :error)))
            (cond
             (missing-access
              ;; Missing-access is expected for some sources on user accounts.
              (setf (alist-get source-name disco-root--archived-source-has-more nil nil #'equal)
                    nil))
             (error-text
              ;; Keep source marked as has-more so temporary failures can be retried.
              (setf (alist-get source-name disco-root--archived-source-has-more nil nil #'equal)
                    t)
              (push (format "%s: %s" source-name error-text) errors))
             (t
              (setf (alist-get source-name disco-root--archived-source-has-more nil nil #'equal)
                    has-more)
              (when next-before
                (setf (alist-get source-name disco-root--archived-before-cursors nil nil #'equal)
                      next-before))
              (setq page-threads (append page-threads threads)))))))))
    (list :threads (disco-root--sort-threads-by-archive-time
                    (disco-root--dedupe-threads page-threads))
          :errors (nreverse errors))))

(defun disco-root--render-archived-threads-buffer ()
  "Render archived-thread buffer from local pagination/cache state."
  (let* ((parent-channel disco-root--archived-parent-channel)
         (threads (or disco-root--archived-threads-cache '()))
         (errors (or disco-root--archived-last-errors '()))
         (inhibit-read-only t))
    (erase-buffer)
    (disco-ui-render-list-view
     :title (format "Archived Threads: %s"
                    (disco-root--channel-label parent-channel))
     :key-hints "g: refresh   n: next page   RET/mouse-1: open thread   q: quit"
     :summary (format "Loaded: %d   Sources: %s"
                      (length threads)
                      (disco-root--archived-source-status-string))
     :loading-note (unless (disco-root--archived-any-source-has-more-p)
                     "(no more archived pages)")
     :items threads
     :item-inserter (lambda (thread)
                      (disco-root--insert-channel-line thread 2))
     :empty-text "(no archived threads)"
     :footer-lines (when errors
                     (append (list "Errors:")
                             (mapcar (lambda (err)
                                       (format "  - %s" err))
                                     errors))))
    (goto-char (point-min))))

(defun disco-root--archived-buffer-name (parent-channel)
  "Return archived-thread buffer name for PARENT-CHANNEL."
  (format "*disco:archived:%s (%s)*"
          (or (alist-get 'name parent-channel) "(no-name)")
          (alist-get 'id parent-channel)))

(defun disco-root--parent-threads-buffer-name (parent-channel)
  "Return active-thread buffer name for PARENT-CHANNEL."
  (format "*disco:threads:%s (%s)*"
          (or (alist-get 'name parent-channel) "(no-name)")
          (alist-get 'id parent-channel)))

(defun disco-root--active-parent-threads (parent-channel)
  "Return active thread channels under PARENT-CHANNEL."
  (let ((parent-id (alist-get 'id parent-channel)))
    (disco-root--sort-channels
     (seq-filter #'disco-root--displayable-channel-p
                 (disco-state-parent-threads parent-id)))))

(defun disco-root--render-parent-threads-buffer ()
  "Render active-thread buffer from local parent-thread state."
  (let* ((parent-channel disco-root--parent-threads-parent-channel)
         (threads (and parent-channel
                       (disco-root--active-parent-threads parent-channel)))
         (inhibit-read-only t))
    (erase-buffer)
    (disco-ui-render-list-view
     :title (format "Threads: %s"
                    (if parent-channel
                        (disco-root--channel-label parent-channel)
                      "(no parent)"))
     :key-hints "g: refresh active   A: archived threads   RET/mouse-1: open thread   n/p/TAB: nav   q: quit"
     :summary (format "Active threads indexed: %d"
                      (length (or threads '())))
     :loading-note (when disco-root--parent-threads-refresh-in-flight
                     "[refreshing active threads...]")
     :items threads
     :item-inserter (lambda (thread)
                      (disco-root--insert-channel-line thread 2))
     :empty-text "(no active threads indexed)")
    (goto-char (point-min))))

(defun disco-root-parent-threads-refresh ()
  "Refresh active-thread list in current parent-thread buffer."
  (interactive)
  (let* ((thread-buffer (current-buffer))
         (parent-channel disco-root--parent-threads-parent-channel)
         (parent-id (and parent-channel (alist-get 'id parent-channel)))
         (guild-id (and parent-channel (alist-get 'guild_id parent-channel)))
         (generation (1+ disco-root--parent-threads-refresh-generation)))
    (unless parent-channel
      (user-error "disco: thread buffer has no parent context"))
    (unless guild-id
      (user-error "disco: parent channel has no guild context"))
    (setq disco-root--parent-threads-refresh-generation generation)
    (setq disco-root--parent-threads-refresh-in-flight t)
    (disco-root--render-parent-threads-buffer)
    (disco-api-channel-search-active-threads-async
     parent-id
     :limit 25
     :offset 0
     :on-success
     (lambda (result)
       (when (and (buffer-live-p thread-buffer)
                  (with-current-buffer thread-buffer
                    (and (eq major-mode 'disco-root-parent-threads-mode)
                         (= disco-root--parent-threads-refresh-generation generation))))
         (with-current-buffer thread-buffer
           (let ((threads
                  (or (alist-get 'threads result) '())))
             (disco-state-sync-threads guild-id (list parent-id) threads)
             (setq disco-root--parent-threads-refresh-in-flight nil)
             (disco-root--render-parent-threads-buffer)
             (message "disco: loaded %d active threads"
                      (length threads))))))
     :on-error
     (lambda (err)
       (when (and (buffer-live-p thread-buffer)
                  (with-current-buffer thread-buffer
                    (and (eq major-mode 'disco-root-parent-threads-mode)
                         (= disco-root--parent-threads-refresh-generation generation))))
         (with-current-buffer thread-buffer
           (setq disco-root--parent-threads-refresh-in-flight nil)
           (disco-root--render-parent-threads-buffer)
           (message "disco: active thread refresh failed: %s"
                    (disco-root--async-error-message err))))))))

(defun disco-root-parent-threads-open-archived ()
  "Open archived-thread browser for current parent-thread buffer context."
  (interactive)
  (unless disco-root--parent-threads-parent-channel
    (user-error "disco: thread buffer has no parent context"))
  (disco-root-list-archived-threads
   (alist-get 'id disco-root--parent-threads-parent-channel)))

(defvar disco-root-parent-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-parent-threads-refresh)
    (define-key map (kbd "A") #'disco-root-parent-threads-open-archived)
    (define-key map (kbd "RET") #'disco-root-open-at-point)
    (define-key map [mouse-1] #'disco-root-mouse-open-at-point)
    (define-key map (kbd "n") #'disco-root-button-forward)
    (define-key map (kbd "p") #'disco-root-button-backward)
    (define-key map (kbd "TAB") #'disco-root-button-forward)
    (define-key map (kbd "<backtab>") #'disco-root-button-backward)
    (define-key map (kbd "?") #'disco-root-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-root-parent-threads-mode'.")

(define-derived-mode disco-root-parent-threads-mode special-mode "Disco-Threads"
  "Major mode for active thread listing buffers."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun disco-root-open-parent-threads (&optional parent-channel-id)
  "Open active thread list for PARENT-CHANNEL-ID.

When PARENT-CHANNEL-ID is nil, prompt for one parent channel."
  (interactive)
  (let* ((parent-channel
          (or (and parent-channel-id (disco-state-channel parent-channel-id))
              (disco-root--read-thread-parent-channel))))
    (unless (and parent-channel
                 (disco-root--thread-parent-channel-p parent-channel))
      (user-error "disco: selected channel cannot contain threads"))
    (let ((buf (get-buffer-create
                (disco-root--parent-threads-buffer-name parent-channel))))
      (with-current-buffer buf
        (disco-root-parent-threads-mode)
        (setq disco-root--parent-threads-parent-channel parent-channel)
        (setq disco-root--parent-threads-refresh-generation 0)
        (setq disco-root--parent-threads-refresh-in-flight nil)
        (disco-root-parent-threads-refresh))
      (pop-to-buffer buf))))

(defun disco-root--open-channel (channel-id)
  "Open CHANNEL-ID according to channel semantics.

Forum/media parent channels open the active-thread listing buffer; other
channels open room timeline buffers."
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (unless channel
      (user-error "disco: channel %s is unavailable" channel-id))
    (if (disco-root--forum-or-media-channel-p channel)
        (disco-root-open-parent-threads channel-id)
      (disco-room-open channel-id (disco-root--channel-display-name channel)))))

(defun disco-root-archived-threads-refresh ()
  "Refresh archived thread list in current archived-thread buffer."
  (interactive)
  (let ((parent-channel disco-root--archived-parent-channel))
    (unless parent-channel
      (user-error "disco: archived-thread buffer has no parent context"))
    (let* ((parent-id (alist-get 'id parent-channel))
           (result (disco-root--fetch-archived-threads-page parent-id t))
           (threads (plist-get result :threads)))
      (setq disco-root--archived-last-errors (plist-get result :errors))
      (setq disco-root--archived-threads-cache threads)
      (dolist (thread threads)
        (disco-state-upsert-channel thread))
      (disco-root--render-archived-threads-buffer)
      (message "disco: loaded %d archived threads" (length threads)))))

(defun disco-root-archived-threads-load-more ()
  "Load next archived-thread page for current archived buffer."
  (interactive)
  (let ((parent-channel disco-root--archived-parent-channel))
    (unless parent-channel
      (user-error "disco: archived-thread buffer has no parent context"))
    (if (not (disco-root--archived-any-source-has-more-p))
        (message "disco: no more archived thread pages")
      (let* ((parent-id (alist-get 'id parent-channel))
             (result (disco-root--fetch-archived-threads-page parent-id nil))
             (page-threads (plist-get result :threads)))
        (setq disco-root--archived-last-errors (plist-get result :errors))
        (setq disco-root--archived-threads-cache
              (disco-root--sort-threads-by-archive-time
               (disco-root--dedupe-threads
                (append disco-root--archived-threads-cache page-threads))))
        (dolist (thread page-threads)
          (disco-state-upsert-channel thread))
        (disco-root--render-archived-threads-buffer)
        (message "disco: loaded %d more archived threads (total %d)"
                 (length page-threads)
                 (length disco-root--archived-threads-cache))))))

(defvar disco-root-archived-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-archived-threads-refresh)
    (define-key map (kbd "n") #'disco-root-archived-threads-load-more)
    (define-key map (kbd "RET") #'disco-root-open-at-point)
    (define-key map [mouse-1] #'disco-root-mouse-open-at-point)
    (define-key map (kbd "?") #'disco-root-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-root-archived-threads-mode'.")

(define-derived-mode disco-root-archived-threads-mode special-mode "Disco-Archived"
  "Major mode for archived thread listing buffers."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun disco-root-list-archived-threads (&optional parent-channel-id)
  "Open archived thread list for PARENT-CHANNEL-ID.

When PARENT-CHANNEL-ID is nil, prompt for a parent channel."
  (interactive)
  (let* ((parent-channel
          (or (and parent-channel-id (disco-state-channel parent-channel-id))
              (disco-root--read-thread-parent-channel)))
         (buf (get-buffer-create (disco-root--archived-buffer-name parent-channel))))
    (with-current-buffer buf
      (disco-root-archived-threads-mode)
      (setq disco-root--archived-parent-channel parent-channel)
      (disco-root-archived-threads-refresh))
    (pop-to-buffer buf)))

(defun disco-root--insert-channel-line (channel indent)
  "Insert one CHANNEL at INDENT spaces."
  (let ((channel-id (alist-get 'id channel))
        (channel-type (alist-get 'type channel))
        (label (disco-root--channel-label channel))
        (unread-count (disco-state-channel-effective-unread-count channel))
        (padding (make-string indent ?\s)))
    (let ((line-start (point)))
      (insert (format "%s%s\n" padding label))
      (when (disco-root--openable-channel-p channel)
        (add-text-properties
         line-start
         (point)
         (list 'mouse-face 'highlight
               'help-echo (if (memq channel-type '(15 16))
                              (format "Open threads under channel %s" channel-id)
                            (format "Open channel %s" channel-id))
               'disco-root-row-type 'channel
               'disco-channel-id channel-id
               'disco-unread-count unread-count))))))

(defun disco-root-button-forward (&optional n)
  "Move point to next channel row by N steps."
  (interactive "p")
  (let ((steps (max 1 (or n 1)))
        (positions (disco-root--channel-line-positions))
        (cursor (line-beginning-position))
        (ok t)
        found)
    (dotimes (_ steps)
      (when ok
        (setq found (disco-root--next-position-after positions cursor))
        (if found
            (setq cursor found)
          (setq ok nil))))
    (if (and ok found)
        (goto-char found)
      (message "disco: no next channel"))))

(defun disco-root-button-backward (&optional n)
  "Move point to previous channel row by N steps."
  (interactive "p")
  (let ((steps (max 1 (or n 1)))
        (positions (disco-root--channel-line-positions))
        (cursor (line-beginning-position))
        (ok t)
        found)
    (dotimes (_ steps)
      (when ok
        (setq found (disco-root--previous-position-before positions cursor))
        (if found
            (setq cursor found)
          (setq ok nil))))
    (if (and ok found)
        (goto-char found)
      (message "disco: no previous channel"))))

(defun disco-root-open-at-point ()
  "Open/toggle row at point.

If point is not on actionable row, jump to next channel row and open it."
  (interactive)
  (let ((channel-id (disco-root--line-channel-id)))
    (cond
     ((disco-root--toggle-node-at-point))
     (channel-id
      (disco-root--open-channel channel-id))
     (t
      (let* ((positions (disco-root--channel-line-positions))
             (next (disco-root--next-position-after
                    positions
                    (line-beginning-position))))
        (if next
            (progn
              (goto-char next)
              (disco-root--open-channel (disco-root--line-channel-id next)))
          (user-error "disco: no openable channel at point")))))))

(defun disco-root-next-unread ()
  "Jump to next channel row with unread count > 0."
  (interactive)
  (let* ((positions
          (disco-root--channel-line-positions
           (lambda (pos) (> (disco-root--line-unread-count pos) 0))))
         (origin (line-beginning-position))
         (found (or (disco-root--next-position-after positions origin)
                    (car positions))))
    (if (and found (integerp found))
        (goto-char found)
      (message "disco: no unread channels"))))

(defun disco-root-mouse-open-at-point (event)
  "Handle mouse EVENT by opening or toggling row at clicked point."
  (interactive "e")
  (mouse-set-point event)
  (disco-root-open-at-point))

(defun disco-root--insert-parent-threads (parent-channel rendered-thread-ids &optional indent)
  "Insert threads under PARENT-CHANNEL and mark IDs in RENDERED-THREAD-IDS.

INDENT controls child-thread row indentation and defaults to 8 spaces."
  (let ((parent-id (alist-get 'id parent-channel)))
    (dolist (thread (disco-state-parent-threads parent-id))
      (let ((thread-id (alist-get 'id thread)))
        (when (and thread-id
                   (disco-root--displayable-channel-p thread)
                   (disco-root--channel-visible-in-view-p thread))
          (puthash thread-id t rendered-thread-ids)
          (disco-root--ewoc-insert-channel thread (or indent 8)))))))

(defun disco-root--guild-visible-parent-channels (guild-id)
  "Return non-thread display channels for GUILD-ID."
  (let (parents)
    (dolist (channel (or (disco-state-guild-channels guild-id) '()))
      (when (and (disco-root--displayable-channel-p channel)
                 (disco-root--channel-visible-in-view-p channel)
                 (not (disco-state-channel-thread-p channel)))
        (push channel parents)))
    (disco-root--sort-channels (nreverse parents))))

(defun disco-root--guild-visible-categories (guild-id)
  "Return visible category channels for GUILD-ID."
  (let (categories)
    (dolist (channel (or (disco-state-guild-channels guild-id) '()))
      (when (and (disco-root--channel-category-p channel)
                 (disco-root--channel-viewable-p channel))
        (push channel categories)))
    (disco-root--sort-categories (nreverse categories))))

(defun disco-root--category-children-unread-total (children)
  "Return aggregated unread count for one category CHILDREN list."
  (let ((total 0))
    (dolist (channel children)
      (setq total (+ total (disco-state-channel-effective-unread-count channel))))
    total))

(defun disco-root--insert-guild (guild)
  "Insert one GUILD and its visible channels in root buffer.

Return non-nil when at least one visible row is inserted for GUILD."
  (let* ((guild-id (alist-get 'id guild))
         (guild-unread (disco-root--guild-unread-total guild-id t))
         (parents (disco-root--guild-visible-parent-channels guild-id))
         (categories (disco-root--guild-visible-categories guild-id))
         (category-id-set (make-hash-table :test #'equal))
         (category-children (make-hash-table :test #'equal))
         uncategorized-parents
         (rendered-thread-ids (make-hash-table :test #'equal))
         (has-visible-thread
          (seq-some (lambda (thread)
                      (and (disco-root--displayable-channel-p thread)
                           (disco-root--channel-visible-in-view-p thread)))
                    (or (disco-state-guild-threads guild-id) '()))))
    (dolist (category categories)
      (let ((category-id (alist-get 'id category)))
        (when category-id
          (puthash category-id t category-id-set))))
    (dolist (channel parents)
      (let ((parent-id (alist-get 'parent_id channel)))
        (if (and parent-id (gethash parent-id category-id-set))
            (puthash parent-id
                     (append (or (gethash parent-id category-children) '())
                             (list channel))
                     category-children)
          (push channel uncategorized-parents))))
    (setq uncategorized-parents (nreverse uncategorized-parents))
    (let ((has-categorized-parents
           (seq-some (lambda (category)
                       (let ((category-id (alist-get 'id category)))
                         (and category-id
                              (gethash category-id category-children))))
                     categories)))
      (when (or uncategorized-parents has-categorized-parents has-visible-thread)
        (disco-root--ewoc-insert-guild guild guild-unread)

        (when (disco-root--guild-expanded-p guild-id)
          (dolist (channel uncategorized-parents)
            (disco-root--ewoc-insert-channel channel 4)
            (when (disco-root--thread-parent-channel-p channel)
              (disco-root--insert-parent-threads channel rendered-thread-ids 8)))

          (dolist (category categories)
            (let* ((category-id (alist-get 'id category))
                   (children (and category-id
                                  (gethash category-id category-children)))
                   (category-unread (and children
                                         (disco-root--category-children-unread-total children))))
              (when children
                (disco-root--ewoc-insert-category category (or category-unread 0))
                (when (disco-root--category-expanded-p category-id)
                  (dolist (channel children)
                    (disco-root--ewoc-insert-channel channel 6)
                    (when (disco-root--thread-parent-channel-p channel)
                      (disco-root--insert-parent-threads channel rendered-thread-ids 10)))))))

          (let (orphan-threads)
            (dolist (thread (or (disco-state-guild-threads guild-id) '()))
              (let ((thread-id (alist-get 'id thread)))
                (when (and thread-id
                           (disco-root--displayable-channel-p thread)
                           (disco-root--channel-visible-in-view-p thread)
                           (not (gethash thread-id rendered-thread-ids)))
                  (push thread orphan-threads))))
            (setq orphan-threads (nreverse orphan-threads))
            (when orphan-threads
              (disco-root--ewoc-insert-text "    [threads]")
              (dolist (thread orphan-threads)
                (disco-root--ewoc-insert-channel thread 8)))))

        (disco-root--ewoc-insert-blank)
        t))))

(defun disco-root-render ()
  "Render root dashboard from in-memory state."
  (let ((inhibit-read-only t)
        (private-channels (disco-root--visible-private-channels))
        (unread-channels (disco-root--collect-visible-unread-channels))
        (guilds (or (disco-state-guilds) '())))
    (disco-root--ensure-section-state)
    (erase-buffer)
    (insert "disco.el\n")
    (insert (format "g: refresh   A: archived threads   \\: sort(%s)   v: view(%s)   RET: open/toggle   TAB/t: toggle section/guild/category or next channel   n/p: nav   u: next unread   q: quit"
                    disco-root--sort-mode
                    disco-root--view-mode))
    (when disco-root--refresh-in-flight
      (insert "   [refreshing...]"))
    (insert "\n\n")
    (setq disco-root--channel-node-table (make-hash-table :test #'equal))
    (setq disco-root--ewoc (ewoc-create #'disco-root--ewoc-printer nil nil t))

    (disco-root--ewoc-insert-section 'unread "Unread" (length unread-channels))
    (when (disco-root--section-expanded-p 'unread)
      (if unread-channels
          (dolist (channel unread-channels)
            (disco-root--ewoc-insert-channel channel 2))
        (disco-root--ewoc-insert-text "  (no unread channels)")))
    (disco-root--ewoc-insert-blank)

    (disco-root--ewoc-insert-section 'private "People" (length private-channels))
    (when (disco-root--section-expanded-p 'private)
      (if private-channels
          (dolist (channel private-channels)
            (disco-root--ewoc-insert-channel channel 2))
        (disco-root--ewoc-insert-text "  (no direct messages loaded)")))
    (disco-root--ewoc-insert-blank)

    (disco-root--ewoc-insert-section 'guilds "Guilds" (length guilds))
    (when (disco-root--section-expanded-p 'guilds)
      (if (eq disco-root--view-mode 'dms)
          (disco-root--ewoc-insert-text "  (guild sections hidden in dms view)")
        (let ((inserted 0))
          (if guilds
              (dolist (guild guilds)
                (when (disco-root--insert-guild guild)
                  (setq inserted (1+ inserted))))
            (setq inserted 0))
          (when (= inserted 0)
            (disco-root--ewoc-insert-text "  (no visible guild channels)")))))

    (goto-char (point-min))))

(defun disco-root--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (format "%S" err)))

(defun disco-root-refresh ()
  "Fetch guild/channel data asynchronously and redraw root buffer."
  (interactive)
  (let* ((root-buffer (current-buffer))
         (generation (1+ disco-root--refresh-generation))
         (guild-count 0)
         (channel-count 0)
         (pending 0)
         (last-render-at 0.0)
         (render-throttle-sec 0.08)
         errors)
    (setq disco-root--refresh-generation generation)
    (setq disco-root--refresh-in-flight t)
    (message "disco: refreshing...")
    (cl-labels
        ((callback-active-p ()
           (and (buffer-live-p root-buffer)
                (with-current-buffer root-buffer
                  (and (eq major-mode 'disco-root-mode)
                       (= disco-root--refresh-generation generation)))))
         (maybe-render-incremental (&optional force)
           (when (callback-active-p)
             (with-current-buffer root-buffer
               (let ((now (float-time)))
                 (when (or force
                           (>= (- now last-render-at) render-throttle-sec))
                   (setq last-render-at now)
                   (disco-root--render-preserving-position))))))
         (record-error (label err)
           (push (format "%s: %s" label (disco-root--async-error-message err))
                 errors))
         (finalize-when-done ()
           (when (and (<= pending 0) (callback-active-p))
             (with-current-buffer root-buffer
               (let ((thread-count 0))
                 (dolist (guild (or (disco-state-guilds) '()))
                   (let ((guild-id (alist-get 'id guild)))
                     (setq thread-count (+ thread-count
                                           (length (disco-state-guild-threads guild-id))))))
                 (setq disco-root--refresh-in-flight nil)
                 (disco-root-render)
                 (if errors
                     (message
                      "disco: loaded %d guilds, %d channels (%d threads), %d DMs (%d errors)"
                      guild-count
                      channel-count
                      thread-count
                      (length (disco-state-private-channels))
                      (length errors))
                   (message "disco: loaded %d guilds, %d channels (%d threads), %d DMs"
                            guild-count
                            channel-count
                            thread-count
                            (length (disco-state-private-channels))))))))
         (dec-pending ()
           (setq pending (1- pending))
           (finalize-when-done))
         (inc-pending ()
           (setq pending (1+ pending)))
         (fetch-guild-channels-and-threads (guild)
           (let ((guild-id (alist-get 'id guild)))
             (inc-pending)
             (disco-api-guild-channels-async
              guild-id
              :on-success
              (lambda (channels)
                (when (callback-active-p)
                  (setq channel-count (+ channel-count (length channels)))
                  (disco-state-put-channels guild-id channels)
                  (maybe-render-incremental))
                (dec-pending))
              :on-error
              (lambda (err)
                (when (callback-active-p)
                  (record-error (format "guild %s channels" guild-id) err))
                (dec-pending)))
             (when disco-fetch-guild-active-threads
               (inc-pending)
               (disco-api-guild-active-threads-async
                guild-id
                :on-success
                (lambda (active)
                  (when (callback-active-p)
                    (dolist (thread (or (alist-get 'threads active) '()))
                      (disco-state-upsert-channel thread))
                    (maybe-render-incremental))
                  (dec-pending))
                :on-error
                (lambda (err)
                  (when (callback-active-p)
                    (record-error (format "guild %s active threads" guild-id) err))
                  (dec-pending)))))))
      (inc-pending)
      (disco-api-user-guilds-async
       :on-success
       (lambda (guilds)
         (when (callback-active-p)
           (setq guild-count (length guilds))
           (disco-state-set-guilds guilds)
           (maybe-render-incremental)

           (inc-pending)
           (disco-api-user-private-channels-async
            :on-success
            (lambda (private-channels)
              (when (callback-active-p)
                (disco-state-set-private-channels private-channels)
                (maybe-render-incremental))
              (dec-pending))
            :on-error
            (lambda (err)
              (when (callback-active-p)
                (record-error "private channels" err))
              (dec-pending)))

           (dolist (guild guilds)
             (fetch-guild-channels-and-threads guild)))
         (dec-pending))
       :on-error
       (lambda (err)
         (when (callback-active-p)
           (record-error "guild list" err))
         (dec-pending)))
      (maybe-render-incremental t))))

(defvar disco-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-refresh)
    (define-key map (kbd "A") #'disco-root-list-archived-threads)
    (define-key map (kbd "\\") #'disco-root-toggle-sort-mode)
    (define-key map (kbd "v") #'disco-root-cycle-view-mode)
    (define-key map (kbd "t") #'disco-root-toggle-section-at-point)
    (define-key map (kbd "RET") #'disco-root-open-at-point)
    (define-key map [mouse-1] #'disco-root-mouse-open-at-point)
    (define-key map (kbd "n") #'disco-root-button-forward)
    (define-key map (kbd "p") #'disco-root-button-backward)
    (define-key map (kbd "TAB") #'disco-root-tab-dwim)
    (define-key map (kbd "<backtab>") #'disco-root-button-backward)
    (define-key map (kbd "u") #'disco-root-next-unread)
    (define-key map (kbd "?") #'disco-root-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-root-mode'.")

(define-derived-mode disco-root-mode special-mode "Disco-Root"
  "Major mode for disco.el root buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local disco-root--ewoc nil)
  (setq-local disco-root--channel-node-table (make-hash-table :test #'equal))
  (setq-local disco-root--guild-expanded (make-hash-table :test #'equal))
  (setq-local disco-root--category-expanded (make-hash-table :test #'equal)))

(defun disco-root-open ()
  "Open root buffer and render current state."
  (interactive)
  (let ((buf (get-buffer-create disco-root-buffer-name)))
    (with-current-buffer buf
      (disco-root-mode)
      (disco-root--attach-live-updates)
      (disco-root-render))
    (pop-to-buffer buf)))

(provide 'disco-root)

;;; disco-root.el ends here
