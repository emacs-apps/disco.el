;;; disco-root.el --- Root buffer for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root dashboard showing guilds and channels, inspired by telega/ement
;; list-driven navigation style.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'pp)
(autoload 'org-read-date "org" nil nil)
(require 'transient)
(require 'seq)
(require 'subr-x)
(require 'disco-ui)
(require 'disco-view)
(require 'disco-util)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-customize)
(require 'disco-directory)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-room)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-permission)
(require 'disco-root-layout)
(require 'disco-root-view)

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

(defvar-local disco-root--inspect-channel nil
  "Channel object rendered by the current inspect buffer.")

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

(defvar-local disco-root--directory-handler nil
  "Buffer-local directory lifecycle event handler closure.")

(defvar-local disco-root--refresh-in-flight nil
  "Non-nil while an async root refresh is in progress.")

(defvar-local disco-root--guild-load-status nil
  "Hash table guild-id -> directory request status for root projection.")

(defvar-local disco-root--sort-mode 'activity
  "Root channel sorting mode.

Supported values: `activity' and `name'.")

(defvar-local disco-root--view-mode 'all
  "Root visibility mode.

Supported values: `all', `unread', and `dms'.")

(defvar-local disco-root--pre-unread-view-mode 'all
  "Most recent non-unread `disco-root--view-mode' value.")

(defvar-local disco-root--layout 'tree
  "Current root layout symbol.")

(defvar-local disco-root--tree-show-unread-section t
  "When non-nil, tree layout renders quick unread section.")

(defvar-local disco-root--ewoc nil
  "EWOC used to render the root tree list incrementally.")

(defvar-local disco-root--channel-node-table nil
  "Hash table mapping channel IDs to root EWOC node lists.")

(defvar-local disco-root--section-node-table nil
  "Hash table mapping section symbols to EWOC nodes.")

(defvar-local disco-root--guild-node-table nil
  "Hash table mapping guild IDs to EWOC nodes.")

(defvar-local disco-root--category-node-table nil
  "Hash table mapping category IDs to EWOC nodes.")

(defvar-local disco-root--live-update-timer nil
  "Debounce timer used to flush aggregated gateway updates.")

(defvar-local disco-root--rendering nil
  "Non-nil while a root render transaction is rebuilding the buffer.")

(defvar-local disco-root--render-pending nil
  "Non-nil when an update arrives during a root render transaction.")

(defvar-local disco-root--missing-preview-fetch-timer nil
  "Debounce timer used to flush missing last-message fetch requests.")

(defvar-local disco-root--missing-preview-pending-by-guild nil
  "Hash table guild-id -> pending channel-id list for preview fetch.")

(defvar-local disco-root--missing-preview-requested-last-message-id-by-channel nil
  "Hash table channel-id -> last_message_id already requested via proactive op34.")

(defvar-local disco-root--dirty-channel-ids nil
  "List of channel IDs queued for incremental live patching.")

(defvar-local disco-root--dirty-structure-p nil
  "Non-nil when queued live updates require full root reconcile.")

(defvar-local disco-root--dirty-header-p nil
  "Non-nil when queued live updates require root header refresh.")

(defvar-local disco-root--last-header-refresh-at nil
  "Timestamp of the last root header refresh in current buffer.")

(defvar-local disco-root--fill-column nil
  "Effective root layout width used for the latest render pass.")

(defconst disco-root--section-order '(activity unread private guilds)
  "Known top-level section symbols for root buffer rendering.")

(defconst disco-root--activity-icon-slot-width 4
  "Reserved icon slot width (columns) in activity rows.")

(defvar-local disco-root--section-expanded nil
  "Alist of section expansion state in root buffer.

Each entry is (SECTION-SYMBOL . BOOLEAN).")

(defvar-local disco-root--guild-expanded nil
  "Hash table guild-id -> expansion state for guild rows.")

(defvar-local disco-root--category-expanded nil
  "Hash table category-channel-id -> expansion state for category rows.")

(defvar-local disco-root--header-marker nil
  "Marker pointing at the start of the root header block.")

(defvar-local disco-root--ewoc-marker nil
  "Marker pointing at the start of root EWOC content.")

(defvar-local disco-root--debug-log-enabled nil
  "Non-nil when root debug logging is enabled in this buffer.")

(defvar-local disco-root--debug-log-changes nil
  "Non-nil when root change logging is enabled in this buffer.")

(defvar-local disco-root--debug-log-verbose nil
  "Non-nil when verbose root debug logging is enabled in this buffer.")

(defvar disco-root-search-history nil
  "Minibuffer history for root search queries.")

(defconst disco-root--search-tab-order '(messages links media files pins)
  "Display order for root search result tabs.")

(defconst disco-root--search-tab-label-alist
  '((messages . "Messages")
    (links . "Links")
    (media . "Media")
    (files . "Files")
    (pins . "Pins"))
  "Display labels for root search result tabs.")

(defvar-local disco-root--search-query nil
  "Current root search query string, or nil when search view is inactive.")

(defvar-local disco-root--search-domain nil
  "Current root search domain plist.")

(defvar-local disco-root--search-query-spec nil
  "Parsed root search query plist for the active search session.")

(defvar-local disco-root--search-tabs nil
  "Alist of search tab symbol to tab-state plist for root search view.")

(defvar-local disco-root--search-generation 0
  "Monotonic generation counter for async root search callbacks.")

(defvar-local disco-root--search-in-flight nil
  "Non-nil while a root search request is in flight.")

(defvar-local disco-root--search-channel-table nil
  "Hash table channel-id -> channel object from the active search session.")

(defvar-local disco-root--search-thread-table nil
  "Hash table thread-id -> thread object from the active search session.")

(defvar-local disco-root--search-prev-layout nil
  "Most recent non-search layout before activating root search view.")

(defcustom disco-root-search-tab-limit 10
  "Default number of search results to request per root search tab."
  :type 'integer
  :group 'disco)

(defcustom disco-root-search-include-nsfw nil
  "When non-nil, include NSFW channels in root search requests when allowed.

Channel-scoped searches also enable this automatically for age-restricted
channels and threads."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-search-track-exact-total-hits nil
  "When non-nil, request exact total hit counts in root search results."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-search-member-completion-min-prefix 2
  "Minimum prefix length before root search requests remote guild members."
  :type 'integer
  :group 'disco)

(defcustom disco-root-search-member-completion-limit 50
  "Maximum number of guild members to request for root search completion."
  :type 'integer
  :group 'disco)

(defconst disco-root--search-filter-names
  '("from" "mentions" "author-type" "has" "in" "before" "after"
    "pinned" "sort" "order" "slop")
  "Supported Discord-style root search filter names.")

(defconst disco-root--search-author-type-values '("user" "bot" "webhook")
  "Supported values for Discord-style `author-type:' root search filter.")

(defconst disco-root--search-has-values
  '("image" "sound" "video" "file" "sticker" "embed" "link" "poll" "snapshot")
  "Supported values for Discord-style `has:' root search filter.")

(defconst disco-root--search-sort-values '("timestamp" "relevance")
  "Supported values for Discord-style `sort:' root search filter.")

(defconst disco-root--search-order-values '("asc" "desc")
  "Supported values for Discord-style `order:' root search filter.")

(defconst disco-root--search-bool-values '("true" "false")
  "Supported boolean values for Discord-style root search filters.")

(defconst disco-root--search-slop-values '("0" "1" "2" "3" "5" "10")
  "Suggested values for Discord-style `slop:' root search filter.")

(defvar-local disco-root--search-completion-domain nil
  "Minibuffer-local root search domain used by search query completion.")

(defvar-local disco-root--search-completion-filter nil
  "Minibuffer-local root search filter currently being completed.")

(defvar-local disco-root--search-completion-requested-prefixes nil
  "Minibuffer-local set of remote member completion prefixes already requested.")

(defcustom disco-root-show-guild-icons t
  "When non-nil, show guild icons in root guild rows when available."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-guild-icon-size 18
  "Pixel size used for inline guild icons in root rows."
  :type 'integer
  :group 'disco)

(defcustom disco-root-activity-context-width '(0.45 20)
  "Width of activity context block before preview text.

Same semantics as `telega-chat-button-width':
- Integer means fixed columns.
- Float means percentage of activity content width.
- List (VALUE MIN MAX) constrains computed width (MAX is optional)."
  :type '(choice number (list number))
  :group 'disco)

(defcustom disco-root-activity-include-threads nil
  "When non-nil, include thread channels in activity layout.

Thread-heavy guilds can create very large activity lists, so this is off
by default to keep root refresh and resize reflow responsive."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-activity-time-format-alist
  '((today . "%H:%M")
    (this-week . "%a")
    (old . "%d.%m.%y")
    (time . "%H:%M")
    (date . "%d.%m.%y")
    (date-time . "%d.%m.%y %a %H:%M"))
  "Activity timestamp formats, inspired by `telega-date-format-alist'."
  :type '(alist :key-type
          (choice (const :tag "If date is today" today)
                  (const :tag "If date is this week" this-week)
                  (const :tag "If date is older" old)
                  (const :tag "Time only" time)
                  (const :tag "Date only" date)
                  (const :tag "Date and time" date-time))
          :value-type string)
  :group 'disco)

(defcustom disco-root-week-start-day 1
  "Day of week considered as start of week.

0 means Sunday, 1 means Monday."
  :type 'integer
  :group 'disco)

(defcustom disco-root-auto-fill-on-window-size-change t
  "When non-nil, rerender visible root buffers after window width changes."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-auto-fill-margin-columns 1
  "Additional margin columns reserved when computing root fill width."
  :type '(choice (const :tag "No extra margin" nil)
          integer)
  :group 'disco)

(defcustom disco-root-live-update-debounce 0.06
  "Seconds to debounce aggregated gateway updates before UI flush."
  :type 'number
  :group 'disco)

(defcustom disco-root-activity-header-refresh-interval 0.2
  "Minimum seconds between activity header refreshes from live row updates.

This throttle applies only to implicit header refresh caused by dirty activity
rows. Explicit header-dirty events bypass the throttle."
  :type 'number
  :group 'disco)

(defcustom disco-root-debug-log-enabled nil
  "When non-nil, log root buffer operations to a debug buffer."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-debug-log-changes nil
  "When non-nil, log every buffer change applied to the root buffer."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-debug-log-verbose nil
  "When non-nil, include verbose per-channel operations in debug logs."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-debug-log-buffer-name "*disco-root-debug*"
  "Buffer name used to store root debug logs."
  :type 'string
  :group 'disco)

(defcustom disco-root-gateway-context-max-guilds 4
  "Maximum number of guilds to request extra Gateway context for per sync."
  :type 'integer
  :group 'disco)

(defcustom disco-root-gateway-context-last-messages-per-guild 30
  "Maximum channel IDs per guild for op34 last-message requests.

Discord limits one request to at most 100 channel IDs."
  :type 'integer
  :group 'disco)

(defcustom disco-root-gateway-context-request-channel-info t
  "When non-nil, request op43 channel metadata fields during root sync."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-missing-preview-fetch-enabled t
  "When non-nil, request missing activity-row last-message previews."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-missing-preview-fetch-debounce 0.6
  "Seconds to debounce batched proactive last-message requests."
  :type 'number
  :group 'disco)

(defcustom disco-root-missing-preview-fetch-max-per-guild 100
  "Maximum number of channel IDs per guild in one proactive op34 request.

Discord allows up to 100 channel IDs per op34 request."
  :type 'integer
  :group 'disco)

(defcustom disco-root-extra-info-functions nil
  "Functions that return additional display information for root rows.

Each function is called with arguments (KIND OBJECT CONTEXT) where:
- KIND is one of `channel', `guild', or `category'.
- OBJECT is the row object (channel/guild/category alist).
- CONTEXT is a plist with row metadata.

A function should return nil, a string, or a list of strings. Returned
fragments are joined and appended to the row label."
  :type '(repeat function)
  :group 'disco)

(defvar disco-root--extra-info-provider-error-cache (make-hash-table :test #'eq)
  "Provider symbols already reported for `disco-root-extra-info-functions'.")

(defvar disco-root--guild-icon-image-cache (make-hash-table :test #'equal)
  "Global guild icon image cache keyed by guild icon cache key.

Values are image objects or the symbol `:missing'.")

(defvar disco-root--guild-icon-fetching (make-hash-table :test #'equal)
  "Global set of guild icon cache keys currently being fetched.")

(defvar disco-root--window-size-hook-installed nil
  "Non-nil once root auto-fill window-size hook has been installed.")

(defun disco-root--live-event-p (event-type)
  "Return non-nil when EVENT-TYPE should trigger root updates."
  (memq event-type
        '(message-create message-ack
          channel-create channel-update channel-delete channel-update-partial
          channel-unread-update passive-update-v1 passive-update-v2
          channel-pins-update channel-pins-ack
          channel-statuses channel-info channel-member-count-update
          last-messages conversation-summary-update
          presence-update sessions-replace
          voice-state-update voice-channel-status-update voice-channel-start-time-update
          guild-create guild-update guild-delete guild-sync
          user-guild-settings-update
          guild-feature-ack user-non-channel-ack notification-center-items-ack
          thread-create thread-update thread-delete thread-list-sync)))

(defun disco-root--live-event-structural-p (event-type)
  "Return non-nil when EVENT-TYPE requires a full tree reconcile."
  (memq event-type
        '(channel-create channel-update channel-delete
          guild-create guild-update guild-delete guild-sync
          user-guild-settings-update
          thread-create thread-update thread-delete thread-list-sync)))

(defun disco-root--live-event-header-p (event-type)
  "Return non-nil when EVENT-TYPE affects root header state only."
  (memq event-type
        '(guild-feature-ack user-non-channel-ack notification-center-items-ack
          sessions-replace voice-state-update passive-update-v1 passive-update-v2)))

(defun disco-root--event-channel-ids (event)
  "Extract channel IDs from gateway EVENT."
  (let (ids)
    (dolist (value (list (plist-get event :channel-id)
                         (plist-get event :previous-channel-id)
                         (plist-get event :thread-id)))
      (when value
        (push value ids)))
    (dolist (item (or (plist-get event :channel-unread-updates) '()))
      (when-let* ((id (or (alist-get 'id item)
                          (alist-get 'channel_id item))))
        (push id ids)))
    (dolist (item (or (plist-get event :channels) '()))
      (when-let* ((id (or (alist-get 'id item)
                          (alist-get 'channel_id item))))
        (push id ids)))
    (dolist (item (or (plist-get event :updated-channels) '()))
      (when-let* ((id (or (alist-get 'id item)
                          (alist-get 'channel_id item))))
        (push id ids)))
    (dolist (item (or (plist-get event :messages) '()))
      (when-let* ((id (alist-get 'channel_id item)))
        (push id ids)))
    (dolist (item (or (plist-get event :threads) '()))
      (when-let* ((thread-id (alist-get 'id item)))
        (push thread-id ids))
      (when-let* ((parent-id (alist-get 'parent_id item)))
        (push parent-id ids)))
    (dolist (value (or (plist-get event :channel-ids) '()))
      (when value
        (push value ids)))
    (seq-uniq (nreverse ids) #'equal)))

(defun disco-root-set-layout (layout)
  "Set root LAYOUT and rerender the current root buffer."
  (interactive
   (list
    (intern
     (completing-read
      "Root layout: "
      (mapcar #'symbol-name (disco-root-layout-names))
      nil t nil nil (symbol-name (or disco-root--layout
                                     disco-root-default-layout))))))
  (unless (memq layout (disco-root-layout-names))
    (user-error "disco: unknown root layout: %s" layout))
  (setq disco-root--layout layout)
  (disco-root--render-preserving-position)
  (message "disco: root layout -> %s" (disco-root-layout-label layout)))

(defun disco-root-cycle-layout ()
  "Cycle active root layout across registered root layouts."
  (interactive)
  (let* ((layouts (disco-root-layout-names))
         (index (cl-position disco-root--layout layouts :test #'eq))
         (next-layout
          (cond
           ((null layouts)
            nil)
           ((null index)
            (car layouts))
           (t
            (nth (mod (1+ index) (length layouts)) layouts)))))
    (unless next-layout
      (user-error "disco: no root layouts registered"))
    (disco-root-set-layout next-layout)))

(defun disco-root--set-view-mode (mode)
  "Set root visibility MODE and keep unread-lens restore state."
  (setq disco-root--view-mode mode)
  (unless (eq mode 'unread)
    (setq disco-root--pre-unread-view-mode mode)))

(defun disco-root-toggle-unread-lens ()
  "Toggle unread lens for current layout.

Tree layout toggles unread quick section visibility. Other layouts toggle
between current view mode and unread-only filter."
  (interactive)
  (if (eq (disco-root--ensure-layout) 'tree)
      (progn
        (setq disco-root--tree-show-unread-section
              (not disco-root--tree-show-unread-section))
        (disco-root--render-preserving-position)
        (message "disco: tree unread section %s"
                 (if disco-root--tree-show-unread-section
                     "shown"
                   "hidden")))
    (if (eq disco-root--view-mode 'unread)
        (disco-root--set-view-mode disco-root--pre-unread-view-mode)
      (disco-root--set-view-mode 'unread))
    (disco-root--render-preserving-position)
    (message "disco: root view mode -> %s" disco-root--view-mode)))

(defun disco-root--render-preserving-position ()
  "Render root tree and keep point near previous line/column."
  (disco-view-render-preserving-position
   #'disco-root-render
   :preserve-window-start t)
  (disco-root--update-window-points))

(defun disco-root--buffer-visible-p (&optional buffer)
  "Return non-nil when BUFFER is currently displayed in any live window."
  (or noninteractive
      (window-live-p (get-buffer-window (or buffer (current-buffer)) t))))

(defun disco-root--hack-window-points (&optional point)
  "Update stored previous-buffer markers for current buffer to POINT.

This mirrors telega's workaround for Emacs not fully honoring buffer-local
nil `switch-to-buffer-preserve-window-point' in passive windows."
  (unless switch-to-buffer-preserve-window-point
    (let ((pos (or point (point))))
      (dolist (win (get-buffer-window-list (current-buffer) nil t))
        (when (window-live-p win)
          (when-let* ((entry (assq (current-buffer) (window-prev-buffers win))))
            (setf (nth 2 entry)
                  (copy-marker pos
                               (marker-insertion-type (nth 2 entry))))))))))

(defun disco-root--update-window-points (&optional point)
  "Update window points for current buffer to POINT.

Mirrors telega's root buffer behavior for keeping window points aligned
after passive EWOC and full-buffer updates."
  (let ((pos (or point (point))))
    (disco-root--hack-window-points pos)
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (when (window-live-p win)
        (set-window-point win pos)))))


(defun disco-root--search-domain-equal-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same search domain."
  (and (eq (disco-root--search-domain-kind left)
           (disco-root--search-domain-kind right))
       (equal (disco-root--search-domain-id left)
              (disco-root--search-domain-id right))))

(defun disco-root--search-domain-candidates ()
  "Return completion candidates for root search domains."
  (append
   (when-let* ((channel-domain (disco-root--search-current-channel-domain)))
     (list (cons (format "Channel: %s"
                         (disco-root--search-domain-label channel-domain))
                 channel-domain)))
   (list (cons "DMs"
               '(:kind dms :id nil :label "DMs")))
   (mapcar (lambda (guild)
             (let ((guild-id (alist-get 'id guild))
                   (guild-name (or (alist-get 'name guild) "Guild")))
               (cons (format "Guild: %s" guild-name)
                     (list :kind 'guild :id guild-id :label guild-name))))
           (or (disco-state-guilds) '()))))

(defun disco-root--search-current-domain-at-point ()
  "Infer the most relevant root search domain from point context."
  (or (when (and (eq (disco-root--ensure-layout) 'search)
                 disco-root--search-domain)
        disco-root--search-domain)
      (disco-root--search-current-channel-domain)
      (when-let* ((guild-id (disco-root--line-guild-id)))
        (list :kind 'guild
              :id guild-id
              :label (or (disco-root--guild-name-by-id guild-id) guild-id)))
      (when-let* ((category-id (disco-root--line-category-id))
                  (category (disco-state-channel category-id))
                  (guild-id (alist-get 'guild_id category)))
        (list :kind 'guild
              :id guild-id
              :label (or (disco-root--guild-name-by-id guild-id) guild-id)))
      (when (eq disco-root--view-mode 'dms)
        '(:kind dms :id nil :label "DMs"))
      (cdr (car (disco-root--search-domain-candidates)))))

(defun disco-root--read-search-domain ()
  "Prompt for root search domain and return its plist descriptor."
  (let* ((candidates (disco-root--search-domain-candidates))
         (default-domain (disco-root--search-current-domain-at-point))
         (default-choice
          (car (seq-find (lambda (cell)
                           (disco-root--search-domain-equal-p (cdr cell)
                                                              default-domain))
                         candidates)))
         (choice (completing-read "Search domain: "
                                  (mapcar #'car candidates)
                                  nil t nil nil default-choice)))
    (copy-tree (or (cdr (assoc choice candidates))
                   (cdr (car candidates))))))

(defun disco-root--search-split-query-tokens (query)
  "Split root search QUERY into shell-like tokens."
  (condition-case err
      (split-string-and-unquote (or query ""))
    (error
     (user-error "disco: invalid search query syntax: %s"
                 (error-message-string err)))))

(defun disco-root--search-split-filter-values (raw-value)
  "Split RAW-VALUE on commas, trimming whitespace and dropping empties."
  (let (result)
    (dolist (part (split-string (or raw-value "") "," t))
      (let ((trimmed (string-trim part)))
        (when (not (string-empty-p trimmed))
          (push trimmed result))))
    (nreverse result)))

(defun disco-root--search-quote-completion-candidate (value)
  "Return VALUE quoted for root search completion when needed."
  (let ((text (format "%s" value)))
    (if (string-match-p "[[:space:]]" text)
        (format "\"%s\"" (replace-regexp-in-string "[\\\"]" "\\\\&" text))
      text)))

(defun disco-root--search--register-candidate (label value seen result)
  "Helper to register LABEL -> VALUE in RESULT, deduped by SEEN hash table."
  (let ((text (and label (string-trim (format "%s" label)))))
    (when (and (stringp text)
               (not (string-empty-p text))
               (not (gethash text seen)))
      (puthash text t seen)
      (push (cons text value) result)))
  result)

(defun disco-root--search-domain-channel-objects (domain)
  "Return channel objects that belong to root search DOMAIN."
  (pcase (disco-root--search-domain-kind domain)
    ('guild
     (let ((guild-id (disco-root--search-domain-id domain)))
       (append (or (disco-state-guild-channels guild-id) '())
               (or (disco-state-guild-threads guild-id) '()))))
    ('channel
     (if-let* ((channel (disco-root--search-domain-channel-object domain)))
         (list channel)
       '()))
    (_
     (or (disco-state-private-channels) '()))))

(defun disco-root--search-channel-candidates (domain)
  "Return alist of display label to channel id candidates for DOMAIN."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (channel (disco-root--search-domain-channel-objects domain))
      (when (and (disco-channel-searchable-p channel)
                 (alist-get 'id channel))
        (let* ((channel-id (alist-get 'id channel))
               (name (disco-root--channel-display-name channel))
               (context (disco-root--search-context-label channel))
               (aliases (delq nil
                              (list context
                                    name
                                    (and name (format "#%s" name))
                                    channel-id))))
          (dolist (label aliases)
            (setq result
                  (disco-root--search--register-candidate label channel-id seen result))))))
    (nreverse result)))

(defun disco-root--search-user-candidates-from-message (message seen result)
  "Add search user candidates from MESSAGE to RESULT using SEEN hash table."
  (let* ((author (alist-get 'author message))
         (member (alist-get 'member message))
         (author-id (and (listp author) (alist-get 'id author)))
         (nick (and (listp member) (alist-get 'nick member)))
         (global-name (and (listp author) (alist-get 'global_name author)))
         (username (and (listp author) (alist-get 'username author))))
    (when author-id
      (dolist (label (delq nil (list nick global-name username
                                     (and username (format "@%s" username))
                                     author-id)))
        (setq result
              (disco-root--search--register-candidate label author-id seen result)))))
  result)

(defun disco-root--search-user-candidates-from-private-channel (channel seen result)
  "Add private-channel recipient user candidates from CHANNEL to RESULT."
  (dolist (recipient (or (alist-get 'recipients channel) '()))
    (when-let* ((user-id (alist-get 'id recipient)))
      (dolist (label (delq nil (list (alist-get 'global_name recipient)
                                     (alist-get 'username recipient)
                                     (and (alist-get 'username recipient)
                                          (format "@%s" (alist-get 'username recipient)))
                                     user-id)))
        (setq result
              (disco-root--search--register-candidate label user-id seen result)))))
  result)

(defun disco-root--search-user-candidates-from-guild-member (member seen result)
  "Add cached guild MEMBER user candidates to RESULT."
  (let* ((user (alist-get 'user member))
         (user-id (or (and (listp user) (alist-get 'id user))
                      (alist-get 'user_id member)))
         (nick (alist-get 'nick member)))
    (when user-id
      (dolist (label (delq nil (list nick
                                     (and (listp user) (alist-get 'global_name user))
                                     (and (listp user) (alist-get 'username user))
                                     (and (listp user)
                                          (alist-get 'username user)
                                          (format "@%s" (alist-get 'username user)))
                                     user-id)))
        (setq result
              (disco-root--search--register-candidate label user-id seen result)))))
  result)

(defun disco-root--search-user-candidates-from-presence (presence seen result)
  "Add presence-derived user candidates from PRESENCE to RESULT."
  (let* ((user (alist-get 'user presence))
         (user-id (and (listp user) (alist-get 'id user))))
    (when user-id
      (dolist (label (delq nil (list (alist-get 'global_name user)
                                     (alist-get 'username user)
                                     (and (alist-get 'username user)
                                          (format "@%s" (alist-get 'username user)))
                                     user-id)))
        (setq result
              (disco-root--search--register-candidate label user-id seen result)))))
  result)

(defun disco-root--search-user-candidates (domain)
  "Return alist of display label to user id candidates for DOMAIN."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (when-let* ((current-user-id (and (fboundp 'disco-gateway-current-user-id)
                                      (disco-gateway-current-user-id))))
      (dolist (label (list "me" current-user-id))
        (setq result
              (disco-root--search--register-candidate label current-user-id seen result))))
    (when-let* ((guild-id (disco-root--search-domain-guild-id domain)))
      (dolist (member (disco-state-guild-members guild-id))
        (setq result
              (disco-root--search-user-candidates-from-guild-member member seen result))))
    (dolist (presence (disco-state-presences
                       (disco-root--search-domain-guild-id domain)))
      (setq result
            (disco-root--search-user-candidates-from-presence presence seen result)))
    (dolist (channel (disco-root--search-domain-channel-objects domain))
      (when (disco-state-private-channel-p channel)
        (setq result
              (disco-root--search-user-candidates-from-private-channel channel seen result)))
      (dolist (message (or (disco-state-messages (alist-get 'id channel)) '()))
        (setq result
              (disco-root--search-user-candidates-from-message message seen result))))
    (nreverse result)))

(defun disco-root--search-find-candidate-id (needle candidates)
  "Resolve NEEDLE using CANDIDATES alist and return the matching id, or nil."
  (let ((downcased (downcase (string-trim (format "%s" needle)))))
    (cdr (seq-find (lambda (cell)
                     (equal (downcase (car cell)) downcased))
                   candidates))))

(defun disco-root--search-normalize-member-completion-prefix (value)
  "Normalize search completion VALUE into a guild member prefix query."
  (let* ((text (string-trim (or value "")))
         (unquoted (string-trim text "\""))
         (no-at (if (string-prefix-p "@" unquoted)
                    (substring unquoted 1)
                  unquoted)))
    (string-trim no-at)))

(defun disco-root--search-should-request-member-completion-p (domain filter prefix)
  "Return non-nil when member completion should request remote data."
  (and (disco-root--search-domain-guild-id domain)
       (member filter '("from" "mentions"))
       (>= (length prefix)
           (max 1 (or disco-root-search-member-completion-min-prefix 1)))
       (not (string-match-p "\\`[0-9]+\\'" prefix))
       (disco-gateway-running-p)))

(defun disco-root--search-maybe-request-member-completion (filter raw-value domain)
  "Request guild member completions for FILTER and RAW-VALUE in DOMAIN."
  (let* ((prefix (disco-root--search-normalize-member-completion-prefix raw-value))
         (request-key (cons (disco-root--search-domain-id domain) (downcase prefix))))
    (when (and (disco-root--search-should-request-member-completion-p domain filter prefix)
               (not (member request-key disco-root--search-completion-requested-prefixes)))
      (cl-pushnew request-key disco-root--search-completion-requested-prefixes :test #'equal)
      (disco-gateway-request-guild-members
       (disco-root--search-domain-guild-id domain)
       :query prefix
       :limit disco-root-search-member-completion-limit))))

(defun disco-root--search-parse-bool (raw-value field-name)
  "Parse RAW-VALUE as boolean for FIELD-NAME and return t or :false."
  (pcase (downcase (string-trim (or raw-value "")))
    ((or "true" "t" "yes" "y" "1") t)
    ((or "false" "nil" "no" "n" "0") :false)
    (_
     (user-error "disco: %s expects true/false" field-name))))

(defun disco-root--search-time-to-snowflake (time-value)
  "Return Discord snowflake boundary string for TIME-VALUE."
  (let* ((unix-ms (truncate (* 1000.0 (float-time time-value))))
         (discord-ms (- unix-ms (* 1000 disco-state-discord-epoch-seconds))))
    (when (< discord-ms 0)
      (user-error "disco: search timestamp precedes Discord epoch"))
    (format "%d" (ash discord-ms 22))))

(defun disco-root--search-parse-boundary-id (raw-value field-name)
  "Parse RAW-VALUE into a message-id boundary string for FIELD-NAME."
  (let ((trimmed (string-trim (or raw-value ""))))
    (cond
     ((string-empty-p trimmed)
      (user-error "disco: %s cannot be empty" field-name))
     ((string-match "<#[0-9]+>" trimmed)
      (replace-regexp-in-string "[^0-9]" "" trimmed))
     ((string-match "<[!@#]*\\([0-9]+\\)>" trimmed)
      (match-string 1 trimmed))
     ((string-match-p "\\`[0-9]+\\'" trimmed)
      trimmed)
     (t
      (condition-case _err
          (disco-root--search-time-to-snowflake (date-to-time trimmed))
        (error
         (user-error "disco: %s expects a message id or parseable date/time (%s)"
                     field-name
                     trimmed)))))))

(defun disco-root--search-resolve-channel (raw-value domain)
  "Resolve RAW-VALUE to a channel id within root search DOMAIN."
  (when (eq (disco-root--search-domain-kind domain) 'channel)
    (user-error "disco: in: is unavailable in channel search; switch domain instead"))
  (let* ((trimmed (string-trim (or raw-value "")))
         (normalized (replace-regexp-in-string "\\`#" "" trimmed)))
    (cond
     ((string-empty-p normalized)
      (user-error "disco: in: expects a channel name or id"))
     ((string-match "<#\\([0-9]+\\)>" normalized)
      (match-string 1 normalized))
     ((string-match-p "\\`[0-9]+\\'" normalized)
      normalized)
     ((disco-root--search-find-candidate-id normalized
                                            (disco-root--search-channel-candidates domain)))
     (t
      (user-error "disco: unknown channel in: %s" raw-value)))))

(defun disco-root--search-resolve-user (raw-value domain field-name)
  "Resolve RAW-VALUE to a user id within root search DOMAIN for FIELD-NAME."
  (let* ((trimmed (string-trim (or raw-value "")))
         (normalized (replace-regexp-in-string "\\`@" "" trimmed)))
    (cond
     ((string-empty-p normalized)
      (user-error "disco: %s expects a user or id" field-name))
     ((string-match "<@!?\\([0-9]+\\)>" normalized)
      (match-string 1 normalized))
     ((string-match-p "\\`[0-9]+\\'" normalized)
      normalized)
     ((equal (downcase normalized) "me")
      (or (and (fboundp 'disco-gateway-current-user-id)
               (disco-gateway-current-user-id))
          (user-error "disco: current user id is unavailable")))
     ((disco-root--search-find-candidate-id normalized
                                            (disco-root--search-user-candidates domain)))
     (t
      (user-error "disco: unknown user for %s: %s" field-name raw-value)))))

(defun disco-root--search-normalize-has-value (raw-value)
  "Normalize RAW-VALUE for the Discord-style `has:' search filter."
  (let* ((trimmed (string-trim (or raw-value "")))
         (negative (string-prefix-p "-" trimmed))
         (value (if negative (substring trimmed 1) trimmed))
         (downcased (downcase value)))
    (unless (member downcased disco-root--search-has-values)
      (user-error "disco: unsupported has: value %s" raw-value))
    (concat (if negative "-" "") downcased)))

(defun disco-root--search-normalize-author-type (raw-value)
  "Normalize RAW-VALUE for the Discord-style `author-type:' search filter."
  (let ((downcased (downcase (string-trim (or raw-value "")))))
    (unless (member downcased disco-root--search-author-type-values)
      (user-error "disco: unsupported author-type: value %s" raw-value))
    downcased))

(defun disco-root--search-parse-slop (raw-value)
  "Parse RAW-VALUE as integer slop for root search."
  (let ((trimmed (string-trim (or raw-value ""))))
    (unless (string-match-p "\\`[0-9]+\\'" trimmed)
      (user-error "disco: slop expects a number"))
    (max 0 (min 100 (string-to-number trimmed)))))

(defun disco-root--search-parse-filter-token (key raw-value domain spec)
  "Apply one Discord-style filter KEY with RAW-VALUE in DOMAIN to SPEC plist."
  (pcase key
    ("from"
     (plist-put spec :author-ids
                (append (plist-get spec :author-ids)
                        (mapcar (lambda (value)
                                  (disco-root--search-resolve-user value domain "from:"))
                                (disco-root--search-split-filter-values raw-value)))))
    ("author-type"
     (plist-put spec :author-types
                (append (plist-get spec :author-types)
                        (mapcar #'disco-root--search-normalize-author-type
                                (disco-root--search-split-filter-values raw-value)))))
    ("mentions"
     (let ((values (disco-root--search-split-filter-values raw-value))
           mentions)
       (setq mentions (plist-get spec :mentions))
       (dolist (value values)
         (if (member (downcase value) '("everyone" "@everyone"))
             (setq spec (plist-put spec :mention-everyone t))
           (push (disco-root--search-resolve-user value domain "mentions:")
                 mentions)))
       (plist-put spec :mentions (nreverse mentions))))
    ("has"
     (plist-put spec :has
                (append (plist-get spec :has)
                        (mapcar #'disco-root--search-normalize-has-value
                                (disco-root--search-split-filter-values raw-value)))))
    ("in"
     (plist-put spec :channel-ids
                (append (plist-get spec :channel-ids)
                        (mapcar (lambda (value)
                                  (disco-root--search-resolve-channel value domain))
                                (disco-root--search-split-filter-values raw-value)))))
    ("before"
     (plist-put spec :max-id
                (disco-root--search-parse-boundary-id raw-value "before:")))
    ("after"
     (plist-put spec :min-id
                (disco-root--search-parse-boundary-id raw-value "after:")))
    ("slop"
     (plist-put spec :slop (disco-root--search-parse-slop raw-value)))
    ("pinned"
     (plist-put spec :pinned
                (disco-root--search-parse-bool raw-value "pinned:")))
    ("sort"
     (let ((value (downcase (string-trim raw-value))))
       (unless (member value disco-root--search-sort-values)
         (user-error "disco: unsupported sort: value %s" raw-value))
       (plist-put spec :sort-by (intern value))))
    ("order"
     (let ((value (downcase (string-trim raw-value))))
       (unless (member value disco-root--search-order-values)
         (user-error "disco: unsupported order: value %s" raw-value))
       (plist-put spec :sort-order (intern value))))
    (_ spec)))

(defun disco-root--search-parse-query (query domain)
  "Parse Discord-style root search QUERY for DOMAIN into a plist spec."
  (let ((tokens (disco-root--search-split-query-tokens query))
        (content-parts nil)
        (spec (list :sort-by 'timestamp :sort-order 'desc)))
    (dolist (token tokens)
      (if (string-match "\\`\\([^:[:space:]]+\\):\\(.*\\)\\'" token)
          (let ((key (downcase (match-string 1 token)))
                (raw-value (match-string 2 token)))
            (if (member key disco-root--search-filter-names)
                (setq spec (disco-root--search-parse-filter-token key raw-value domain spec))
              (push token content-parts)))
        (push token content-parts)))
    (setq spec (plist-put spec :content
                          (when content-parts
                            (string-join (nreverse content-parts) " "))))
    (dolist (field '(:author-ids :author-types :mentions :has :channel-ids))
      (when-let* ((value (plist-get spec field)))
        (setq spec (plist-put spec field (seq-uniq value #'equal)))))
    spec))

(defun disco-root--search-capf-bounds ()
  "Return (START . END) bounds for the current root search minibuffer token."
  (let ((scan (minibuffer-prompt-end))
        (limit (point))
        (start (minibuffer-prompt-end))
        (quoted nil)
        (escaped nil))
    (while (< scan limit)
      (let ((char (char-after scan)))
        (cond
         (escaped
          (setq escaped nil))
         ((eq char ?\\)
          (setq escaped t))
         ((eq char ?\")
          (setq quoted (not quoted)))
         ((and (not quoted)
               (memq char '(9 10 13 32)))
          (setq start (1+ scan)))))
      (setq scan (1+ scan)))
    (cons start limit)))

(defun disco-root--search-capf-filter-candidates ()
  "Return completion candidates for root search filter names."
  (let ((filters (if (eq (disco-root--search-domain-kind disco-root--search-completion-domain)
                         'channel)
                     (delete "in" (copy-sequence disco-root--search-filter-names))
                   disco-root--search-filter-names)))
    (mapcar (lambda (name) (concat name ":")) filters)))

(defun disco-root--search-capf-value-candidates (filter domain)
  "Return completion candidates for FILTER values within DOMAIN."
  (pcase filter
    ((or "from" "mentions")
     (mapcar (lambda (cell)
               (disco-root--search-quote-completion-candidate (car cell)))
             (disco-root--search-user-candidates domain)))
    ("author-type"
     disco-root--search-author-type-values)
    ("in"
     (unless (eq (disco-root--search-domain-kind domain) 'channel)
       (mapcar (lambda (cell)
                 (disco-root--search-quote-completion-candidate (car cell)))
               (disco-root--search-channel-candidates domain))))
    ("has"
     disco-root--search-has-values)
    ("sort"
     disco-root--search-sort-values)
    ("order"
     disco-root--search-order-values)
    ("slop"
     disco-root--search-slop-values)
    ("pinned"
     disco-root--search-bool-values)
    (_ nil)))

(defun disco-root--search-query-complete-at-point ()
  "Completion-at-point function for Discord-style root search queries."
  (when-let* ((domain disco-root--search-completion-domain)
              (bounds (disco-root--search-capf-bounds)))
    (let* ((start (car bounds))
           (end (cdr bounds))
           (token (buffer-substring-no-properties start end)))
      (cond
       ((string-match "\\`\\([^:[:space:]]*\\)\\'" token)
        (let ((candidates (seq-filter (lambda (candidate)
                                        (string-prefix-p token candidate))
                                      (disco-root--search-capf-filter-candidates))))
          (when candidates
            (list start end candidates :exclusive 'no))))
       ((string-match "\\`\\([^:[:space:]]+\\):\\(.*\\)\\'" token)
        (let* ((filter (downcase (match-string 1 token)))
               (raw-value (match-string 2 token))
               (_request (disco-root--search-maybe-request-member-completion filter raw-value domain))
               (value-candidates (disco-root--search-capf-value-candidates filter domain)))
          (when value-candidates
            (let* ((comma-pos (or (cl-position ?, raw-value :from-end t) -1))
                   (value-start (+ start (length filter) 1
                                   (if (>= comma-pos 0)
                                       (1+ comma-pos)
                                     0)))
                   (prefix (buffer-substring-no-properties value-start end)))
              (list value-start
                    end
                    (seq-filter (lambda (candidate)
                                  (string-prefix-p prefix candidate))
                                value-candidates)
                    :exclusive 'no)))))))))

(defun disco-root--read-search-query (domain &optional initial-input)
  "Prompt for a Discord-style root search query within DOMAIN."
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "<tab>") #'completion-at-point)
    (define-key map (kbd "M-TAB") #'completion-at-point)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local disco-root--search-completion-domain domain)
          (setq-local disco-root--search-completion-filter 'query)
          (setq-local disco-root--search-completion-requested-prefixes nil)
          (setq-local completion-at-point-functions
                      '(disco-root--search-query-complete-at-point))
          (setq-local completion-ignore-case t))
      (read-from-minibuffer
       (format "Search %s: " (disco-root--search-domain-label domain))
       initial-input
       map nil 'disco-root-search-history))))

(defun disco-root--search-default-spec ()
  "Return default structured root search spec."
  (list :sort-by 'timestamp
        :sort-order 'desc))

(defun disco-root--search-normalize-spec (spec)
  "Return normalized copy of root search SPEC with default sort fields."
  (let ((copy (copy-tree (or spec '()))))
    (unless (plist-member copy :sort-by)
      (setq copy (plist-put copy :sort-by 'timestamp)))
    (unless (plist-member copy :sort-order)
      (setq copy (plist-put copy :sort-order 'desc)))
    copy))


(defun disco-root--search-ensure-draft ()
  "Ensure current root buffer has initialized draft search state."
  (unless disco-root--search-domain
    (setq-local disco-root--search-domain
                (copy-tree (or (disco-root--search-current-domain-at-point)
                               '(:kind dms :id nil :label "DMs")))))
  (setq-local disco-root--search-query-spec
              (disco-root--search-normalize-spec disco-root--search-query-spec))
  (unless (stringp disco-root--search-query)
    (setq-local disco-root--search-query "")))

(defun disco-root--search-value-summary (value)
  "Return concise string summary for VALUE."
  (cond
   ((null value) "none")
   ((and (listp value) (= (length value) 1))
    (format "%s" (car value)))
   ((listp value)
    (format "%d" (length value)))
   ((stringp value)
    (if (string-empty-p value) "none" value))
   ((eq value t) "yes")
   ((eq value :false) "no")
   (t (format "%s" value))))

(defun disco-root--search-shorten-list (items &optional max-items)
  "Return one concise string for ITEMS, truncated to MAX-ITEMS labels."
  (let* ((labels (delq nil (copy-sequence (or items '()))))
         (limit (max 1 (or max-items 3)))
         (total (length labels)))
    (cond
     ((zerop total)
      "none")
     ((<= total limit)
      (string-join labels ", "))
     (t
      (format "%s +%d"
              (string-join (seq-take labels limit) ", ")
              (- total limit))))))

(defun disco-root--search-user-label (user-id &optional domain)
  "Return best human-readable label for USER-ID in DOMAIN."
  (let* ((normalized (and user-id (format "%s" user-id)))
         (search-domain (or domain disco-root--search-domain))
         (guild-id (disco-root--search-domain-guild-id search-domain)))
    (cond
     ((null normalized)
      nil)
     ((equal normalized
             (and (fboundp 'disco-gateway-current-user-id)
                  (disco-gateway-current-user-id)))
      "me")
     ((and guild-id
           (when-let* ((member (disco-state-guild-member guild-id normalized)))
             (or (alist-get 'nick member)
                 (let ((user (alist-get 'user member)))
                   (or (alist-get 'global_name user)
                       (alist-get 'username user)))))))
     ((when-let* ((presence (disco-state-presence normalized guild-id))
                  (user (alist-get 'user presence)))
        (or (alist-get 'global_name user)
            (alist-get 'username user))))
     (t normalized))))

(defun disco-root--search-channel-label (channel-id)
  "Return best human-readable label for CHANNEL-ID."
  (let* ((normalized (and channel-id (format "%s" channel-id)))
         (channel (and normalized (disco-root--search-channel normalized))))
    (or (and channel
             (or (disco-root--channel-display-name channel)
                 (disco-root--search-context-label channel)))
        normalized)))

(defun disco-root--search-format-user-ids (ids &optional domain)
  "Return concise display string for user IDS in DOMAIN."
  (disco-root--search-shorten-list
   (mapcar (lambda (user-id)
             (disco-root--search-user-label user-id domain))
           (or ids '()))))

(defun disco-root--search-format-channel-ids (ids)
  "Return concise display string for channel IDS."
  (disco-root--search-shorten-list
   (mapcar #'disco-root--search-channel-label (or ids '()))))

(defun disco-root--search-summary-chips ()
  "Return list of summary chips for current root search spec."
  (let ((spec (disco-root--search-normalize-spec disco-root--search-query-spec))
        chips)
    (push (format "[in:%s]" (disco-root--search-domain-label disco-root--search-domain)) chips)
    (when-let* ((content (plist-get spec :content))
                ((not (string-empty-p content))))
      (push (format "[text:%s]" content) chips))
    (when-let* ((authors (plist-get spec :author-ids)))
      (push (format "[from:%s]"
                    (disco-root--search-format-user-ids authors disco-root--search-domain))
            chips))
    (when-let* ((author-types (plist-get spec :author-types)))
      (push (format "[author:%s]" (string-join author-types ",")) chips))
    (when-let* ((mentions (plist-get spec :mentions)))
      (push (format "[mentions:%s]"
                    (disco-root--search-format-user-ids mentions disco-root--search-domain))
            chips))
    (when (plist-get spec :mention-everyone)
      (push "[mentions:everyone]" chips))
    (when-let* ((channels (plist-get spec :channel-ids)))
      (push (format "[where:%s]"
                    (disco-root--search-format-channel-ids channels))
            chips))
    (when-let* ((has (plist-get spec :has)))
      (push (format "[has:%s]" (string-join has ",")) chips))
    (when (plist-member spec :pinned)
      (push (format "[pinned:%s]" (disco-root--search-value-summary (plist-get spec :pinned))) chips))
    (when-let* ((before (plist-get spec :max-id)))
      (push (format "[before:%s]" before) chips))
    (when-let* ((after (plist-get spec :min-id)))
      (push (format "[after:%s]" after) chips))
    (when-let* ((slop (plist-get spec :slop)))
      (push (format "[slop:%s]" slop) chips))
    (unless (eq (plist-get spec :sort-by) 'timestamp)
      (push (format "[sort:%s]" (plist-get spec :sort-by)) chips))
    (unless (eq (plist-get spec :sort-order) 'desc)
      (push (format "[order:%s]" (plist-get spec :sort-order)) chips))
    (nreverse chips)))

(defun disco-root--search-sync-query-display ()
  "Synchronize display query string from current structured search spec."
  (setq-local disco-root--search-query
              (string-join (disco-root--search-summary-chips) "  ")))

(defun disco-root--search-refresh-active-completions (&optional guild-id)
  "Refresh active root search completion UI when it matches GUILD-ID.

When GUILD-ID is nil, refresh any active root search completion session."
  (when-let* ((miniwin (active-minibuffer-window))
              (minibuf (window-buffer miniwin)))
    (when (get-buffer-window "*Completions*" t)
      (with-current-buffer minibuf
        (when (and disco-root--search-completion-domain
                   (or (null guild-id)
                       (equal (disco-root--search-domain-guild-id disco-root--search-completion-domain)
                              guild-id)))
          (ignore-errors
            (minibuffer-completion-help)))))))

(defun disco-root--search-user-completion-table (domain filter)
  "Return completion table for root search users in DOMAIN for FILTER."
  (completion-table-dynamic
   (lambda (input)
     (disco-root--search-maybe-request-member-completion filter input domain)
     (mapcar #'car (disco-root--search-user-candidates domain)))))

(defun disco-root--search-channel-completion-table (domain)
  "Return completion table for root search channels in DOMAIN."
  (completion-table-dynamic
   (lambda (_input)
     (mapcar #'car (disco-root--search-channel-candidates domain)))))

(defun disco-root--search-default-multi-input (ids candidates)
  "Return comma-separated default input for IDS using CANDIDATES."
  (string-join
   (mapcar (lambda (id)
             (or (car (seq-find (lambda (cell) (equal (cdr cell) id)) candidates))
                 id))
           (or ids '()))
   ","))

(defun disco-root--search-read-user-ids (prompt domain current-ids)
  "Read user ids with PROMPT within DOMAIN, defaulting to CURRENT-IDS."
  (let* ((candidates (disco-root--search-user-candidates domain))
         (defaults (disco-root--search-default-multi-input current-ids candidates))
         (picked
          (minibuffer-with-setup-hook
              (lambda ()
                (setq-local disco-root--search-completion-domain domain)
                (setq-local disco-root--search-completion-filter "from")
                (setq-local disco-root--search-completion-requested-prefixes nil)
                (setq-local completion-ignore-case t))
            (completing-read-multiple prompt
                                      (disco-root--search-user-completion-table domain "from")
                                      nil t defaults))))
    (mapcar (lambda (value)
              (or (disco-root--search-find-candidate-id value candidates)
                  value))
            picked)))

(defun disco-root--search-read-mentioned-ids (prompt domain current-ids include-everyone)
  "Read mention ids with PROMPT within DOMAIN.

Return plist fragment with `:mentions' and optional `:mention-everyone'."
  (let* ((candidates (append '(("everyone" . :everyone)
                               ("@everyone" . :everyone))
                             (disco-root--search-user-candidates domain)))
         (defaults (disco-root--search-default-multi-input
                    (append (or current-ids '())
                            (when include-everyone (list :everyone)))
                    candidates))
         (picked
          (minibuffer-with-setup-hook
              (lambda ()
                (setq-local disco-root--search-completion-domain domain)
                (setq-local disco-root--search-completion-filter "mentions")
                (setq-local disco-root--search-completion-requested-prefixes nil)
                (setq-local completion-ignore-case t))
            (completing-read-multiple
             prompt
             (completion-table-dynamic
              (lambda (input)
                (disco-root--search-maybe-request-member-completion
                 "mentions" input domain)
                (mapcar #'car candidates)))
             nil t defaults))))
    (list :mentions (delq nil
                          (mapcar (lambda (value)
                                    (let ((resolved (disco-root--search-find-candidate-id
                                                     value candidates)))
                                      (unless (eq resolved :everyone)
                                        (or resolved value))))
                                  picked))
          :mention-everyone (and (seq-some (lambda (value)
                                             (eq (disco-root--search-find-candidate-id
                                                  value candidates)
                                                 :everyone))
                                           picked)
                                 t))))

(defun disco-root--search-read-channel-ids (prompt domain current-ids)
  "Read channel ids with PROMPT within DOMAIN, defaulting to CURRENT-IDS."
  (let* ((candidates (disco-root--search-channel-candidates domain))
         (defaults (disco-root--search-default-multi-input current-ids candidates))
         (picked (completing-read-multiple prompt
                                           (disco-root--search-channel-completion-table domain)
                                           nil t defaults)))
    (mapcar (lambda (value)
              (or (disco-root--search-find-candidate-id value candidates)
                  value))
            picked)))

(defun disco-root--search-plist-remove (plist property)
  "Return copy of PLIST without PROPERTY pair."
  (let (result)
    (while plist
      (let ((key (pop plist))
            (value (pop plist)))
        (unless (eq key property)
          (setq result (append result (list key value))))))
    result))

(defun disco-root--search-set-spec-and-sync (spec)
  "Store root search SPEC and refresh derived display state."
  (setq-local disco-root--search-query-spec (disco-root--search-normalize-spec spec))
  (disco-root--search-sync-query-display)
  disco-root--search-query-spec)

(defun disco-root--search-start-current-draft ()
  "Execute root search using the current draft domain and spec."
  (disco-root--search-ensure-draft)
  (unless (disco-root--search-effective-spec-p disco-root--search-query-spec)
    (user-error "disco: set a search query or filter first"))
  (disco-root--search-sync-query-display)
  (setq-local disco-root--search-channel-table (make-hash-table :test #'equal))
  (setq-local disco-root--search-thread-table (make-hash-table :test #'equal))
  (setq-local disco-root--search-generation (1+ disco-root--search-generation))
  (setq-local disco-root--search-prev-layout
              (unless (eq disco-root--layout 'search)
                disco-root--layout))
  (disco-root--search-reset-tab-states)
  (setq disco-root--layout 'search)
  (disco-root--search-render-if-visible)
  (disco-root--search-dispatch disco-root--search-generation
                               (disco-root--search-request-tabs nil))
  (message "disco: searching in %s"
           (disco-root--search-domain-label disco-root--search-domain)))

(defun disco-root-search-edit-raw-query ()
  "Edit the raw Discord-style query string and import it into search draft state."
  (interactive)
  (disco-root--search-ensure-draft)
  (let* ((domain disco-root--search-domain)
         (raw (disco-root--read-search-query domain (or disco-root--search-query ""))))
    (setq-local disco-root--search-query raw)
    (setq-local disco-root--search-query-spec
                (disco-root--search-normalize-spec
                 (disco-root--search-parse-query raw domain)))
    (disco-root--search-sync-query-display)))

(defun disco-root-search-clear ()
  "Reset the current root search draft spec to defaults."
  (interactive)
  (disco-root--search-ensure-draft)
  (setq-local disco-root--search-query-spec (disco-root--search-default-spec))
  (setq-local disco-root--search-query "")
  (disco-root--search-sync-query-display)
  (message "disco: search filters cleared"))

(defun disco-root-search-execute ()
  "Execute root search using the current transient-managed draft state."
  (interactive)
  (disco-root--search-start-current-draft))

(defun disco-root--search-transient-buffer ()
  "Return root buffer currently edited by the search transient."
  (let ((scope (ignore-errors (transient-scope))))
    (cond
     ((and (bufferp scope) (buffer-live-p scope))
      scope)
     ((derived-mode-p 'disco-root-mode)
      (current-buffer))
     (t
      (error "disco: root search transient has no live root buffer scope")))))

(defun disco-root--search-transient-with-buffer (fn)
  "Call FN with current root search transient buffer current."
  (with-current-buffer (disco-root--search-transient-buffer)
    (disco-root--search-ensure-draft)
    (funcall fn)))

(defclass disco-root-search--field (transient-infix)
  ((getter :initarg :getter)
   (setter :initarg :setter)
   (value-formatter :initarg :value-formatter :initform #'disco-root--search-value-summary)
   (format :initform " %k %d %v")
   (always-read :initform t))
  "Transient infix backed by the current root buffer search draft state.")

(cl-defmethod transient-init-value ((obj disco-root-search--field))
  (oset obj value
        (disco-root--search-transient-with-buffer
         (lambda ()
           (funcall (oref obj getter))))))

(cl-defmethod transient-infix-set ((obj disco-root-search--field) value)
  (disco-root--search-transient-with-buffer
   (lambda ()
     (funcall (oref obj setter) value)))
  (oset obj value value))

(cl-defmethod transient-format-value ((obj disco-root-search--field))
  (let* ((value (oref obj value))
         (formatter (oref obj value-formatter))
         (text (if formatter
                   (funcall formatter value)
                 (format "%s" value))))
    (propertize (or text "")
                'face (if (or (null value)
                              (and (stringp value) (string-empty-p value))
                              (and (listp value) (null value)))
                          'transient-inactive-value
                        'transient-value))))

(defclass disco-root-search--cycle-field (disco-root-search--field)
  ((choices :initarg :choices)
   (always-read :initform nil))
  "Transient infix cycling through a fixed set of choices.")

(cl-defmethod transient-infix-read ((obj disco-root-search--cycle-field))
  (let* ((choices (oref obj choices))
         (current (oref obj value))
         (index (cl-position current choices :test #'equal)))
    (nth (mod (1+ (or index -1)) (length choices)) choices)))

(defun disco-root--search-transient-domain-getter ()
  "Return current transient root search domain."
  disco-root--search-domain)

(defun disco-root--search-transient-domain-setter (value)
  "Set transient root search domain to VALUE."
  (setq-local disco-root--search-domain value)
  (setq-local disco-root--search-query-spec
              (disco-root--search-plist-remove disco-root--search-query-spec :channel-ids))
  (disco-root--search-sync-query-display))

(defun disco-root--search-transient-spec-getter (property)
  "Return current transient root search PROPERTY value."
  (plist-get disco-root--search-query-spec property))

(defun disco-root--search-transient-spec-setter (property value)
  "Set transient root search PROPERTY to VALUE, removing nil values when needed."
  (let ((empty-p (or (null value)
                     (and (stringp value) (string-empty-p value))
                     (and (listp value) (null value)))))
    (setq-local disco-root--search-query-spec
                (if empty-p
                    (disco-root--search-plist-remove disco-root--search-query-spec property)
                  (plist-put disco-root--search-query-spec property value))))
  (disco-root--search-sync-query-display))

(defun disco-root--search-transient-mentions-getter ()
  "Return combined transient mention filter value."
  (list :mentions (plist-get disco-root--search-query-spec :mentions)
        :mention-everyone (plist-get disco-root--search-query-spec :mention-everyone)))

(defun disco-root--search-transient-mentions-setter (value)
  "Set transient mention filter from VALUE plist."
  (setq-local disco-root--search-query-spec
              (plist-put disco-root--search-query-spec
                         :mentions (let ((ids (plist-get value :mentions)))
                                     (and ids (seq-uniq ids #'equal)))))
  (setq-local disco-root--search-query-spec
              (if (plist-get value :mention-everyone)
                  (plist-put disco-root--search-query-spec :mention-everyone t)
                (disco-root--search-plist-remove disco-root--search-query-spec
                                                 :mention-everyone)))
  (disco-root--search-sync-query-display))

(defun disco-root--search-transient-domain-value (_prompt _initial _history)
  "Read a root search domain value for the transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (disco-root--read-search-domain))))

(defun disco-root--search-transient-content-value (prompt _initial _history)
  "Read free-text search content for PROMPT."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let ((value (read-string prompt (or (plist-get disco-root--search-query-spec :content) ""))))
       (unless (string-empty-p (string-trim value))
         (string-trim value))))))

(defun disco-root--search-transient-from-value (_prompt _initial _history)
  "Read `from' users for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let ((ids (disco-root--search-read-user-ids
                 "From: "
                 disco-root--search-domain
                 (plist-get disco-root--search-query-spec :author-ids))))
       (and ids (seq-uniq ids #'equal))))))

(defun disco-root--search-transient-mentions-value (_prompt _initial _history)
  "Read mention filters for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (disco-root--search-read-mentioned-ids
      "Mentions: "
      disco-root--search-domain
      (plist-get disco-root--search-query-spec :mentions)
      (plist-get disco-root--search-query-spec :mention-everyone)))))

(defun disco-root--search-transient-channels-value (_prompt _initial _history)
  "Read channel filters for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (when (eq (disco-root--search-domain-kind disco-root--search-domain) 'channel)
       (user-error "disco: in: is unavailable in channel search; switch domain instead"))
     (let ((ids (disco-root--search-read-channel-ids
                 "In channels: "
                 disco-root--search-domain
                 (plist-get disco-root--search-query-spec :channel-ids))))
       (and ids (seq-uniq ids #'equal))))))

(defun disco-root--search-transient-has-value (_prompt _initial _history)
  "Read `has' filters for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let* ((current (string-join (or (plist-get disco-root--search-query-spec :has) '()) ","))
            (picked (completing-read-multiple "Has: " disco-root--search-has-values nil t current)))
       (and picked (seq-uniq picked #'equal))))))

(defun disco-root--search-transient-author-types-value (_prompt _initial _history)
  "Read `author-type' filters for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let* ((current (string-join (or (plist-get disco-root--search-query-spec :author-types) '()) ","))
            (picked (completing-read-multiple "Author type: "
                                              disco-root--search-author-type-values
                                              nil t current)))
       (and picked (seq-uniq picked #'equal))))))

(defun disco-root--search-transient-slop-value (_prompt _initial _history)
  "Read `slop' value for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let* ((current (if (numberp (plist-get disco-root--search-query-spec :slop))
                         (number-to-string (plist-get disco-root--search-query-spec :slop))
                       ""))
            (value (completing-read "Slop: " disco-root--search-slop-values nil nil current)))
       (unless (string-empty-p (string-trim value))
         (disco-root--search-parse-slop value))))))

(defun disco-root--search-boundary-default-time (property &optional end-of-day)
  "Return default Emacs time for boundary PROPERTY, optionally END-OF-DAY."
  (let ((existing (plist-get disco-root--search-query-spec property)))
    (cond
     ((and (stringp existing)
           (string-match-p "\\`[0-9]+\\'" existing))
      (seconds-to-time (or (disco-root--snowflake-epoch-seconds existing)
                           (float-time))))
     (t
      (let* ((now (decode-time (current-time)))
             (day (nth 3 now))
             (month (nth 4 now))
             (year (nth 5 now)))
        (encode-time (if end-of-day 59 0)
                     (if end-of-day 59 0)
                     (if end-of-day 23 0)
                     day month year))))))

(defun disco-root--search-read-org-date-time (prompt default-time)
  "Read one Org-style date/time with PROMPT and DEFAULT-TIME."
  (org-read-date t t nil prompt default-time))

(defun disco-root--search-transient-boundary-value (prompt property _field-name)
  "Read message boundary PROPERTY using Org-style date picker."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let* ((default-time (disco-root--search-boundary-default-time
                           property
                           (eq property :max-id)))
            (time-value (disco-root--search-read-org-date-time prompt default-time)))
       (and time-value
            (disco-root--search-time-to-snowflake time-value))))))

(defun disco-root--search-transient-before-value (prompt _initial _history)
  "Read `before' boundary for the search transient."
  (disco-root--search-transient-boundary-value prompt :max-id "before:"))

(defun disco-root--search-transient-after-value (prompt _initial _history)
  "Read `after' boundary for the search transient."
  (disco-root--search-transient-boundary-value prompt :min-id "after:"))

(defun disco-root--search-transient-choice-value (prompt choices current)
  "Read one value from CHOICES using PROMPT and CURRENT default."
  (intern (completing-read prompt choices nil t nil nil
                           (symbol-name current))))

(defun disco-root--search-transient-sort-value (prompt _initial _history)
  "Read sort mode for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (disco-root--search-transient-choice-value
      prompt
      disco-root--search-sort-values
      (or (plist-get disco-root--search-query-spec :sort-by) 'timestamp)))))

(defun disco-root--search-transient-order-value (prompt _initial _history)
  "Read sort order for the search transient."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (disco-root--search-transient-choice-value
      prompt
      disco-root--search-order-values
      (or (plist-get disco-root--search-query-spec :sort-order) 'desc)))))

(defun disco-root--search-transient-format-domain (domain)
  "Format root search DOMAIN for transient display."
  (if domain
      (disco-root--search-domain-label domain)
    "none"))

(defun disco-root--search-transient-format-user-ids (value)
  "Format root search user id VALUE list for transient display."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (disco-root--search-format-user-ids value disco-root--search-domain))))

(defun disco-root--search-transient-format-mentions (value)
  "Format root search mention VALUE plist for transient display."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (let ((mentions (plist-get value :mentions))
           (everyone (plist-get value :mention-everyone)))
       (concat (disco-root--search-format-user-ids mentions disco-root--search-domain)
               (if everyone " +everyone" ""))))))

(defun disco-root--search-transient-format-channel-ids (value)
  "Format root search channel id VALUE list for transient display."
  (disco-root--search-transient-with-buffer
   (lambda ()
     (if (eq (disco-root--search-domain-kind disco-root--search-domain) 'channel)
         "fixed by domain"
       (disco-root--search-format-channel-ids value)))))

(defun disco-root--search-transient-format-has (value)
  "Format root search `has' VALUE list for transient display."
  (if value
      (string-join value ", ")
    "none"))

(defun disco-root--search-transient-format-author-types (value)
  "Format root search author-type VALUE list for transient display."
  (if value
      (string-join value ", ")
    "none"))

(defun disco-root--search-transient-format-pinned (value)
  "Format root search pinned VALUE for transient display."
  (cond
   ((null value) "any")
   ((eq value t) "yes")
   (t "no")))

(transient-define-infix disco-root-search--infix-domain ()
  :description "Domain"
  :class 'disco-root-search--field
  :prompt "Domain: "
  :reader #'disco-root--search-transient-domain-value
  :getter #'disco-root--search-transient-domain-getter
  :setter #'disco-root--search-transient-domain-setter
  :value-formatter #'disco-root--search-transient-format-domain)

(transient-define-infix disco-root-search--infix-content ()
  :description "Text"
  :class 'disco-root-search--field
  :prompt "Search text: "
  :reader #'disco-root--search-transient-content-value
  :getter (lambda () (disco-root--search-transient-spec-getter :content))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :content value)))

(transient-define-infix disco-root-search--infix-from ()
  :description "From"
  :class 'disco-root-search--field
  :prompt "From: "
  :reader #'disco-root--search-transient-from-value
  :getter (lambda () (disco-root--search-transient-spec-getter :author-ids))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :author-ids value))
  :value-formatter #'disco-root--search-transient-format-user-ids)

(transient-define-infix disco-root-search--infix-mentions ()
  :description "Mentions"
  :class 'disco-root-search--field
  :prompt "Mentions: "
  :reader #'disco-root--search-transient-mentions-value
  :getter #'disco-root--search-transient-mentions-getter
  :setter #'disco-root--search-transient-mentions-setter
  :value-formatter #'disco-root--search-transient-format-mentions)

(transient-define-infix disco-root-search--infix-channels ()
  :description "In"
  :class 'disco-root-search--field
  :prompt "In channels: "
  :reader #'disco-root--search-transient-channels-value
  :getter (lambda () (disco-root--search-transient-spec-getter :channel-ids))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :channel-ids value))
  :value-formatter #'disco-root--search-transient-format-channel-ids)

(transient-define-infix disco-root-search--infix-has ()
  :description "Has"
  :class 'disco-root-search--field
  :prompt "Has: "
  :reader #'disco-root--search-transient-has-value
  :getter (lambda () (disco-root--search-transient-spec-getter :has))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :has value))
  :value-formatter #'disco-root--search-transient-format-has)

(transient-define-infix disco-root-search--infix-author-type ()
  :description "Author"
  :class 'disco-root-search--field
  :prompt "Author type: "
  :reader #'disco-root--search-transient-author-types-value
  :getter (lambda () (disco-root--search-transient-spec-getter :author-types))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :author-types value))
  :value-formatter #'disco-root--search-transient-format-author-types)

(transient-define-infix disco-root-search--infix-pinned ()
  :description "Pinned"
  :class 'disco-root-search--cycle-field
  :getter (lambda () (and (plist-member disco-root--search-query-spec :pinned)
                          (plist-get disco-root--search-query-spec :pinned)))
  :setter (lambda (value)
            (if (null value)
                (setq-local disco-root--search-query-spec
                            (disco-root--search-plist-remove disco-root--search-query-spec
                                                             :pinned))
              (setq-local disco-root--search-query-spec
                          (plist-put disco-root--search-query-spec :pinned value)))
            (disco-root--search-sync-query-display))
  :value-formatter #'disco-root--search-transient-format-pinned
  :choices '(nil t :false))

(transient-define-infix disco-root-search--infix-before ()
  :description "Before"
  :class 'disco-root-search--field
  :prompt "Before (message id or time): "
  :reader #'disco-root--search-transient-before-value
  :getter (lambda () (disco-root--search-transient-spec-getter :max-id))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :max-id value)))

(transient-define-infix disco-root-search--infix-after ()
  :description "After"
  :class 'disco-root-search--field
  :prompt "After (message id or time): "
  :reader #'disco-root--search-transient-after-value
  :getter (lambda () (disco-root--search-transient-spec-getter :min-id))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :min-id value)))

(transient-define-infix disco-root-search--infix-slop ()
  :description "Slop"
  :class 'disco-root-search--field
  :prompt "Slop: "
  :reader #'disco-root--search-transient-slop-value
  :getter (lambda () (disco-root--search-transient-spec-getter :slop))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :slop value)))

(transient-define-infix disco-root-search--infix-sort ()
  :description "Sort"
  :class 'disco-root-search--field
  :prompt "Sort by: "
  :reader #'disco-root--search-transient-sort-value
  :getter (lambda () (disco-root--search-transient-spec-getter :sort-by))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :sort-by value)))

(transient-define-infix disco-root-search--infix-order ()
  :description "Order"
  :class 'disco-root-search--field
  :prompt "Order: "
  :reader #'disco-root--search-transient-order-value
  :getter (lambda () (disco-root--search-transient-spec-getter :sort-order))
  :setter (lambda (value) (disco-root--search-transient-spec-setter :sort-order value)))

(transient-define-prefix disco-root-search-transient ()
  "Structured root search editor for disco.el."
  [["Scope"
    ("d" disco-root-search--infix-domain)
    ("t" disco-root-search--infix-content)
    ("e" "Edit raw query..." disco-root-search-edit-raw-query :transient t)]
   ["People"
    ("f" disco-root-search--infix-from)
    ("m" disco-root-search--infix-mentions)
    ("A" disco-root-search--infix-author-type)]
   ["Place"
    ("i" disco-root-search--infix-channels)
    ("a" disco-root-search--infix-after)
    ("b" disco-root-search--infix-before)]
   ["Flags"
    ("h" disco-root-search--infix-has)
    ("p" disco-root-search--infix-pinned)
    ("l" disco-root-search--infix-slop)
    ("s" disco-root-search--infix-sort)
    ("o" disco-root-search--infix-order)]
   ["Actions"
    ("g" "Run search" disco-root-search-execute)
    ("x" "Clear filters" disco-root-search-clear :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (unless (derived-mode-p 'disco-root-mode)
    (user-error "disco: root search transient only works in root buffer"))
  (disco-root--search-ensure-draft)
  (transient-setup 'disco-root-search-transient nil nil :scope (current-buffer)))

(defun disco-root-menu-reset-session-state ()
  "Reset in-memory session state for disco.el."
  (interactive)
  (disco-gateway-stop)
  (disco-state-reset)
  (disco-api-reset-rate-limit-state)
  (disco-http-reset-queue-state)
  (message "disco: in-memory state reset"))

(defun disco-root-menu-toggle-active-thread-prefetch ()
  "Toggle `disco-fetch-guild-active-threads' and refresh root when relevant."
  (interactive)
  (setq disco-fetch-guild-active-threads (not disco-fetch-guild-active-threads))
  (message "disco: active thread prefetch %s"
           (if disco-fetch-guild-active-threads "enabled" "disabled"))
  (when (derived-mode-p 'disco-root-mode)
    (disco-root-refresh)))

(defun disco-root-menu-set-thread-archive-fetch-limit (limit)
  "Set archived thread fetch LIMIT in current session."
  (interactive "nArchive thread fetch limit (2-100): ")
  (setq disco-thread-archive-fetch-limit (max 2 (min 100 limit)))
  (message "disco: archive thread fetch limit set to %d"
           disco-thread-archive-fetch-limit))

(transient-define-prefix disco-root-transient ()
  "Root command menu for disco.el."
  [["Refresh"
    ("g" "Refresh root" disco-root-refresh)
    ("A" "Archived threads..." disco-root-list-archived-threads)
    ("t" "Toggle active thread prefetch"
     disco-root-menu-toggle-active-thread-prefetch)
    ("L" "Set archive fetch limit"
     disco-root-menu-set-thread-archive-fetch-limit)]
   ["View"
    ("l" "Cycle layout" disco-root-cycle-layout)
    ("V" "Set layout..." disco-root-set-layout)
    ("s" "Search..." disco-root-search-transient)
    ("U" "Toggle unread lens" disco-root-toggle-unread-lens)]
   ["Inspect"
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Session"
    ("x" "Reset session state" disco-root-menu-reset-session-state)
    ("q" "Quit window" quit-window)]])

(defun disco-root--search-tab-summary-chip (tab)
  "Return one summary chip string for root search TAB."
  (let* ((state (disco-root--search-tab-state tab))
         (loaded (length (or (plist-get state :items) '())))
         (total (plist-get state :total-results))
         (loading (plist-get state :loading))
         (suffix (cond
                  ((numberp total)
                   (format "%d/%d" loaded total))
                  ((> loaded 0)
                   (number-to-string loaded))
                  (loading
                   "…")
                  (t "0"))))
    (format "[%s %s]"
            (downcase (disco-root--search-tab-label tab))
            suffix)))


(defun disco-root--search-store-channels (channels)
  "Merge CHANNELS into current root search channel cache."
  (unless (hash-table-p disco-root--search-channel-table)
    (setq-local disco-root--search-channel-table (make-hash-table :test #'equal)))
  (dolist (channel (or channels '()))
    (when-let* ((channel-id (alist-get 'id channel)))
      (puthash channel-id channel disco-root--search-channel-table))))

(defun disco-root--search-store-threads (threads)
  "Merge THREADS into current root search thread cache."
  (unless (hash-table-p disco-root--search-thread-table)
    (setq-local disco-root--search-thread-table (make-hash-table :test #'equal)))
  (dolist (thread (or threads '()))
    (when-let* ((thread-id (alist-get 'id thread)))
      (puthash thread-id thread disco-root--search-thread-table))))

(defun disco-root--search-flatten-messages (messages)
  "Flatten nested search result MESSAGES array into a message list."
  (let (result)
    (dolist (group (or messages '()))
      (if (listp group)
          (dolist (message group)
            (when (listp message)
              (push message result)))
        (when (listp group)
          (push group result))))
    (nreverse result)))

(defun disco-root--search-merge-message-lists (existing new-items)
  "Append NEW-ITEMS to EXISTING, deduping by message id while preserving order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (message (append (or existing '()) (or new-items '())))
      (let ((message-id (alist-get 'id message)))
        (unless (and message-id (gethash message-id seen))
          (when message-id
            (puthash message-id t seen))
          (push message result))))
    (nreverse result)))

(defun disco-root--search-index-pending-p (body)
  "Return non-nil when BODY is a search index-not-ready response."
  (and (listp body)
       (= (or (alist-get 'code body) 0) 110000)))

(defun disco-root--search-domain-fixed-channel-ids (domain)
  "Return fixed channel-id list implied by channel-scoped DOMAIN, or nil."
  (when-let* ((channel-id (disco-root--search-domain-channel-id domain)))
    (list channel-id)))

(defun disco-root--search-channel-object-by-id (channel-id)
  "Return best known channel object for CHANNEL-ID."
  (or (and channel-id (disco-state-channel channel-id))
      (and (hash-table-p disco-root--search-channel-table)
           (gethash channel-id disco-root--search-channel-table))
      (and (hash-table-p disco-root--search-thread-table)
           (gethash channel-id disco-root--search-thread-table))))

(defun disco-root--search-include-nsfw-p (domain channel-ids)
  "Return non-nil when root search request should include age-restricted channels."
  (or disco-root-search-include-nsfw
      (seq-some (lambda (channel-id)
                  (when-let* ((channel (disco-root--search-channel-object-by-id channel-id)))
                    (disco-state-channel-age-restricted-p channel)))
                (or channel-ids
                    (when-let* ((channel (disco-root--search-domain-channel-object domain))
                                (channel-id (alist-get 'id channel)))
                      (list channel-id))))))

(defun disco-root--search-tab-base-spec (tab query-spec &optional cursor)
  "Return internal request plist for search TAB using QUERY-SPEC and CURSOR."
  (let ((spec (list :limit disco-root-search-tab-limit
                    :content (plist-get query-spec :content)
                    :author-types (plist-get query-spec :author-types)
                    :author-ids (plist-get query-spec :author-ids)
                    :mentions (plist-get query-spec :mentions)
                    :mention-everyone (plist-get query-spec :mention-everyone)
                    :has (copy-sequence (or (plist-get query-spec :has) '()))
                    :pinned (plist-get query-spec :pinned)
                    :sort-by (or (plist-get query-spec :sort-by) 'timestamp)
                    :sort-order (or (plist-get query-spec :sort-order) 'desc)
                    :max-id (plist-get query-spec :max-id)
                    :min-id (plist-get query-spec :min-id)
                    :slop (plist-get query-spec :slop))))
    (when cursor
      (setq spec (plist-put spec :cursor cursor)))
    (pcase tab
      ('links
       (setq spec (plist-put spec :has (seq-uniq (append (plist-get spec :has)
                                                         '("link"))
                                                 #'equal))))
      ('media
       (setq spec (plist-put spec :has (seq-uniq (append (plist-get spec :has)
                                                         '("image" "video"))
                                                 #'equal))))
      ('files
       (setq spec (plist-put spec :has (seq-uniq (append (plist-get spec :has)
                                                         '("file"))
                                                 #'equal))))
      ('pins
       (setq spec (plist-put spec :pinned t))))
    spec))

(defun disco-root--search-request-tabs (&optional load-more-tab)
  "Return tabs alist for current parsed root search query.

When LOAD-MORE-TAB is non-nil, return only that tab with its stored cursor."
  (let ((query-spec disco-root--search-query-spec))
    (if load-more-tab
        (let* ((state (disco-root--search-tab-state load-more-tab))
               (cursor (plist-get state :cursor)))
          (unless cursor
            (user-error "disco: no more search results in %s"
                        (downcase (disco-root--search-tab-label load-more-tab))))
          (list (cons load-more-tab
                      (disco-root--search-tab-base-spec load-more-tab query-spec cursor))))
      (mapcar (lambda (tab)
                (cons tab (disco-root--search-tab-base-spec tab query-spec nil)))
              disco-root--search-tab-order))))

(defun disco-root--search-tab-requested-p (tab tabs-alist)
  "Return non-nil when TAB is present in TABS-ALIST."
  (and tab (assq tab tabs-alist)))

(defun disco-root--search-render-if-visible ()
  "Rerender root buffer when currently in search layout."
  (when (and (eq major-mode 'disco-root-mode)
             (eq (disco-root--ensure-layout) 'search))
    (disco-root--render-preserving-position)))

(defun disco-root--search-filter-messages (messages)
  "Apply client-side root search filters to MESSAGES list."
  (let ((channel-ids (plist-get disco-root--search-query-spec :channel-ids)))
    (if (not channel-ids)
        messages
      (seq-filter (lambda (message)
                    (member (alist-get 'channel_id message) channel-ids))
                  messages))))

(defun disco-root--search-apply-request-state (tabs-alist loading)
  "Mark TABS-ALIST as LOADING state in current search session."
  (dolist (entry tabs-alist)
    (let* ((tab (car entry))
           (state (copy-sequence (disco-root--search-tab-state tab))))
      (setq state (plist-put state :loading (and loading t)))
      (when loading
        (setq state (plist-put state :error nil)))
      (disco-root--search-set-tab-state tab state))))

(defun disco-root--search-handle-success (generation tabs-alist body)
  "Apply successful search BODY for GENERATION and requested TABS-ALIST."
  (when (and (= generation disco-root--search-generation)
             (eq major-mode 'disco-root-mode))
    (setq-local disco-root--search-in-flight nil)
    (if (disco-root--search-index-pending-p body)
        (let ((message (or (alist-get 'message body)
                           "Index not yet available. Try again later"))
              (retry-after (alist-get 'retry_after body)))
          (dolist (entry tabs-alist)
            (let* ((tab (car entry))
                   (state (copy-sequence (disco-root--search-tab-state tab))))
              (setq state (plist-put state :loading nil))
              (setq state (plist-put state :error
                                     (if retry-after
                                         (format "%s (retry after %ss)" message retry-after)
                                       message)))
              (disco-root--search-set-tab-state tab state)))
          (disco-root--search-render-if-visible))
      (let ((tabs-body (alist-get 'tabs body)))
        (dolist (entry tabs-alist)
          (let* ((tab (car entry))
                 (result (alist-get tab tabs-body nil nil #'eq))
                 (messages (disco-root--search-filter-messages
                            (disco-root--search-flatten-messages
                             (alist-get 'messages result))))
                 (state (copy-sequence (disco-root--search-tab-state tab)))
                 (existing (plist-get state :items)))
            (disco-root--search-store-channels (alist-get 'channels result))
            (disco-root--search-store-threads (alist-get 'threads result))
            (setq state (plist-put state :items
                                   (if (plist-get (cdr entry) :cursor)
                                       (disco-root--search-merge-message-lists existing messages)
                                     messages)))
            (setq state (plist-put state :loading nil))
            (setq state (plist-put state :error nil))
            (setq state (plist-put state :cursor (alist-get 'cursor result)))
            (setq state (plist-put state :total-results (alist-get 'total_results result)))
            (setq state (plist-put state :time-spent-ms (alist-get 'time_spent_ms result)))
            (disco-root--search-set-tab-state tab state))))
      (disco-root--search-render-if-visible))))

(defun disco-root--search-handle-error (generation tabs-alist err)
  "Apply async search ERR for GENERATION and requested TABS-ALIST."
  (when (and (= generation disco-root--search-generation)
             (eq major-mode 'disco-root-mode))
    (setq-local disco-root--search-in-flight nil)
    (dolist (entry tabs-alist)
      (let* ((tab (car entry))
             (state (copy-sequence (disco-root--search-tab-state tab))))
        (setq state (plist-put state :loading nil))
        (setq state (plist-put state :error (disco-root--async-error-message err)))
        (disco-root--search-set-tab-state tab state)))
    (disco-root--search-render-if-visible)))

(defun disco-root--search-dispatch (generation tabs-alist)
  "Dispatch async root search request for GENERATION using TABS-ALIST."
  (let* ((root-buffer (current-buffer))
         (domain disco-root--search-domain)
         (channel-ids (or (disco-root--search-domain-fixed-channel-ids disco-root--search-domain)
                          (plist-get disco-root--search-query-spec :channel-ids)))
         (include-nsfw (disco-root--search-include-nsfw-p domain channel-ids)))
    (setq-local disco-root--search-in-flight t)
    (disco-root--search-apply-request-state tabs-alist t)
    (disco-root--search-render-if-visible)
    (pcase (disco-root--search-domain-kind domain)
      ('guild
       (disco-api-guild-search-messages-tabs-async
        (disco-root--search-domain-id domain)
        :tabs tabs-alist
        :channel-ids channel-ids
        :include-nsfw include-nsfw
        :track-exact-total-hits disco-root-search-track-exact-total-hits
        :on-success (lambda (body)
                      (when (buffer-live-p root-buffer)
                        (with-current-buffer root-buffer
                          (disco-root--search-handle-success generation tabs-alist body))))
        :on-error (lambda (err)
                    (when (buffer-live-p root-buffer)
                      (with-current-buffer root-buffer
                        (disco-root--search-handle-error generation tabs-alist err))))))
      ('channel
       (if-let* ((guild-id (disco-root--search-domain-guild-id domain)))
           (disco-api-guild-search-messages-tabs-async
            guild-id
            :tabs tabs-alist
            :channel-ids channel-ids
            :include-nsfw include-nsfw
            :track-exact-total-hits disco-root-search-track-exact-total-hits
            :on-success (lambda (body)
                          (when (buffer-live-p root-buffer)
                            (with-current-buffer root-buffer
                              (disco-root--search-handle-success generation tabs-alist body))))
            :on-error (lambda (err)
                        (when (buffer-live-p root-buffer)
                          (with-current-buffer root-buffer
                            (disco-root--search-handle-error generation tabs-alist err)))))
         (disco-api-channel-search-messages-tabs-async
          (disco-root--search-domain-channel-id domain)
          :tabs tabs-alist
          :include-nsfw include-nsfw
          :track-exact-total-hits disco-root-search-track-exact-total-hits
          :on-success (lambda (body)
                        (when (buffer-live-p root-buffer)
                          (with-current-buffer root-buffer
                            (disco-root--search-handle-success generation tabs-alist body))))
          :on-error (lambda (err)
                      (when (buffer-live-p root-buffer)
                        (with-current-buffer root-buffer
                          (disco-root--search-handle-error generation tabs-alist err)))))))
      (_
       (disco-api-user-search-messages-tabs-async
        :tabs tabs-alist
        :include-nsfw include-nsfw
        :track-exact-total-hits disco-root-search-track-exact-total-hits
        :on-success (lambda (body)
                      (when (buffer-live-p root-buffer)
                        (with-current-buffer root-buffer
                          (disco-root--search-handle-success generation tabs-alist body))))
        :on-error (lambda (err)
                    (when (buffer-live-p root-buffer)
                      (with-current-buffer root-buffer
                        (disco-root--search-handle-error generation tabs-alist err)))))))))

(defun disco-root-search-refresh ()
  "Rerun current root search query in the active search domain."
  (interactive)
  (unless (and disco-root--search-domain
               (disco-root--search-effective-spec-p disco-root--search-query-spec))
    (user-error "disco: no active root search"))
  (setq-local disco-root--search-query-spec
              (or disco-root--search-query-spec
                  (disco-root--search-parse-query disco-root--search-query
                                                  disco-root--search-domain)))
  (setq-local disco-root--search-generation (1+ disco-root--search-generation))
  (disco-root--search-reset-tab-states)
  (disco-root--search-dispatch disco-root--search-generation
                               (disco-root--search-request-tabs nil))
  (disco-root--search-render-if-visible)
  (message "disco: searching in %s"
           (disco-root--search-domain-label disco-root--search-domain)))

(defun disco-root-search-load-more-at-point ()
  "Load the next page of results for the search tab at point."
  (interactive)
  (let ((tab (disco-root--line-search-tab)))
    (unless tab
      (user-error "disco: no search tab at point"))
    (unless (and (eq (disco-root--ensure-layout) 'search)
                 disco-root--search-domain)
      (user-error "disco: no active root search"))
    (disco-root--search-dispatch (or disco-root--search-generation 0)
                                 (disco-root--search-request-tabs tab))
    (message "disco: loading more %s results"
             (downcase (disco-root--search-tab-label tab)))))

(defun disco-root-search (query domain)
  "Search root buffer DOMAIN for QUERY and show results in search layout."
  (interactive
   (let* ((domain (disco-root--read-search-domain))
          (initial (and (eq (disco-root--ensure-layout) 'search)
                        disco-root--search-query
                        (disco-root--search-domain-equal-p domain disco-root--search-domain)
                        disco-root--search-query)))
     (list (disco-root--read-search-query domain initial)
           domain)))
  (let ((normalized-query (string-trim (or query ""))))
    (when (string-empty-p normalized-query)
      (user-error "disco: search query cannot be empty"))
    (setq-local disco-root--search-query normalized-query)
    (setq-local disco-root--search-domain (copy-tree domain))
    (setq-local disco-root--search-query-spec
                (disco-root--search-parse-query normalized-query domain))
    (setq-local disco-root--search-channel-table (make-hash-table :test #'equal))
    (setq-local disco-root--search-thread-table (make-hash-table :test #'equal))
    (setq-local disco-root--search-generation (1+ disco-root--search-generation))
    (setq-local disco-root--search-prev-layout
                (unless (eq disco-root--layout 'search)
                  disco-root--layout))
    (disco-root--search-reset-tab-states)
    (setq disco-root--layout 'search)
    (disco-root--search-render-if-visible)
    (disco-root--search-dispatch disco-root--search-generation
                                 (disco-root--search-request-tabs nil))
    (message "disco: searching %s in %s"
             normalized-query
             (disco-root--search-domain-label domain))))

(defun disco-root-search-channel-transient (&optional channel)
  "Open root search transient scoped to CHANNEL.

CHANNEL can be a channel object or channel id. When called from the root
buffer without CHANNEL, use the channel at point."
  (interactive)
  (let* ((resolved-channel
          (cond
           ((and (listp channel) (alist-get 'id channel))
            channel)
           ((and channel (disco-state-channel channel))
            (disco-state-channel channel))
           ((derived-mode-p 'disco-root-mode)
            (when-let* ((channel-id (disco-root--line-channel-id)))
              (or (disco-state-channel channel-id)
                  (disco-root--search-channel channel-id))))
           (t nil))))
    (unless resolved-channel
      (user-error "disco: no channel available for channel search"))
    (unless (disco-channel-searchable-p resolved-channel)
      (user-error "disco: channel search is unsupported for %s channels"
                  (disco-channel-type-name resolved-channel)))
    (let ((buf (get-buffer-create disco-root-buffer-name))
          (domain (disco-root--search-channel-domain resolved-channel)))
      (with-current-buffer buf
        (unless (derived-mode-p 'disco-root-mode)
          (disco-root-mode))
        (disco-root--attach-live-updates)
        (unless (and disco-root--search-domain
                     (disco-root--search-domain-equal-p disco-root--search-domain domain))
          (setq-local disco-root--search-domain domain)
          (setq-local disco-root--search-query-spec (disco-root--search-default-spec))
          (setq-local disco-root--search-query "")
          (disco-root--search-sync-query-display)))
      (pop-to-buffer buf)
      (with-current-buffer buf
        (disco-root-search-transient)))))

(defun disco-root--ensure-markers ()
  "Ensure header and ewoc markers exist for the current buffer."
  (unless (markerp disco-root--header-marker)
    (setq-local disco-root--header-marker (copy-marker (point-min))))
  (unless (markerp disco-root--ewoc-marker)
    (setq-local disco-root--ewoc-marker (copy-marker (point-min))))
  (set-marker-insertion-type disco-root--header-marker nil)
  (set-marker-insertion-type disco-root--ewoc-marker nil))

(defun disco-root--header-start ()
  "Return buffer position of the root header start."
  (or (and (markerp disco-root--header-marker)
           (marker-position disco-root--header-marker))
      (point-min)))

(defun disco-root--ewoc-start ()
  "Return buffer position of root EWOC content start."
  (or (and (markerp disco-root--ewoc-marker)
           (marker-position disco-root--ewoc-marker))
      (and disco-root--ewoc
           (ewoc-nth disco-root--ewoc 0)
           (ewoc-location (ewoc-nth disco-root--ewoc 0)))
      (save-excursion
        (goto-char (disco-root--header-start))
        (forward-line (1+ (length (disco-root--header-lines))))
        (point))))

(defun disco-root--header-region ()
  "Return cons of header region start/end positions."
  (let ((start (disco-root--header-start))
        (end (disco-root--ewoc-start)))
    (cons start (max start end))))

(defun disco-root--debug-log-enabled-p ()
  "Return non-nil when root debug logging is enabled."
  (or disco-root--debug-log-enabled
      disco-root-debug-log-enabled))

(defun disco-root--debug-log-changes-p ()
  "Return non-nil when root change logging is enabled."
  (and (disco-root--debug-log-enabled-p)
       (or disco-root--debug-log-changes
           disco-root-debug-log-changes)))

(defun disco-root--debug-log-verbose-p ()
  "Return non-nil when verbose root debug logging is enabled."
  (and (disco-root--debug-log-enabled-p)
       (or disco-root--debug-log-verbose
           disco-root-debug-log-verbose)))

(defun disco-root--debug-log (format-string &rest args)
  "Append one debug entry formatted with FORMAT-STRING and ARGS."
  (when (disco-root--debug-log-enabled-p)
    (let ((buf (get-buffer-create disco-root-debug-log-buffer-name))
          (message-log-max nil))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (goto-char (point-max))
          (unless (derived-mode-p 'special-mode)
            (special-mode))
          (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
          (insert " ")
          (insert (apply #'format format-string args))
          (insert "\n"))))))

(defun disco-root--debug-before-change (beg end)
  "Log root buffer modifications before change from BEG to END."
  (when (and (eq major-mode 'disco-root-mode)
             (disco-root--debug-log-changes-p))
    (disco-root--debug-log
     "before-change %d..%d len=%d"
     beg
     end
     (max 0 (- end beg)))))

(defun disco-root--debug-after-change (beg end len-before)
  "Log root buffer modifications after change from BEG to END."
  (when (and (eq major-mode 'disco-root-mode)
             (disco-root--debug-log-changes-p))
    (disco-root--debug-log
     "after-change %d..%d len-before=%d len-after=%d"
     beg
     end
     (max 0 len-before)
     (max 0 (- end beg)))))

(defun disco-root-debug-log-toggle (&optional enable)
  "Toggle root debug logging in current buffer.

With prefix ENABLE, turn logging on when positive, otherwise off."
  (interactive "P")
  (setq-local disco-root--debug-log-enabled
              (if enable
                  (> (prefix-numeric-value enable) 0)
                (not disco-root--debug-log-enabled)))
  (disco-root--debug-log "debug-log %s"
                         (if disco-root--debug-log-enabled "enabled" "disabled"))
  (message "disco: root debug log %s"
           (if disco-root--debug-log-enabled "enabled" "disabled")))

(defun disco-root-debug-log-changes-toggle (&optional enable)
  "Toggle root change logging in current buffer.

With prefix ENABLE, turn logging on when positive, otherwise off."
  (interactive "P")
  (setq-local disco-root--debug-log-changes
              (if enable
                  (> (prefix-numeric-value enable) 0)
                (not disco-root--debug-log-changes)))
  (disco-root--debug-log "debug-change-log %s"
                         (if disco-root--debug-log-changes "enabled" "disabled"))
  (message "disco: root change log %s"
           (if disco-root--debug-log-changes "enabled" "disabled")))

(defun disco-root-debug-log-verbose-toggle (&optional enable)
  "Toggle verbose root debug logging in current buffer.

With prefix ENABLE, turn logging on when positive, otherwise off."
  (interactive "P")
  (setq-local disco-root--debug-log-verbose
              (if enable
                  (> (prefix-numeric-value enable) 0)
                (not disco-root--debug-log-verbose)))
  (disco-root--debug-log "debug-log verbose %s"
                         (if disco-root--debug-log-verbose "enabled" "disabled"))
  (message "disco: root debug log verbose %s"
           (if disco-root--debug-log-verbose "enabled" "disabled")))

(defun disco-root-debug-log-open ()
  "Open the root debug log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create disco-root-debug-log-buffer-name)))

(defun disco-root-debug-log-clear ()
  "Clear the root debug log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create disco-root-debug-log-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (message "disco: root debug log cleared"))

(defun disco-root--refresh-mode-divider-line ()
  "Refresh only the mode-divider header line used for width framing."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (save-excursion
      (goto-char (disco-root--header-start))
      (forward-line 2)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (disco-root--mode-divider-line)))))

(defun disco-root--reflow-layout ()
  "Reflow currently rendered root layout without rebuilding model lists."
  (let ((inhibit-read-only t))
    (if (and (eq major-mode 'disco-root-mode)
             disco-root--ewoc)
        (progn
          (disco-root--refresh-mode-divider-line)
          (ewoc-refresh disco-root--ewoc))
      (disco-root-render))))

(defun disco-root--reflow-preserving-position ()
  "Reflow root layout and keep point near previous line/column."
  (disco-view-render-preserving-position
   #'disco-root--reflow-layout
   :preserve-window-start t)
  (disco-root--update-window-points))

(defun disco-root--auto-fill-to-width (width &optional force)
  "Reflow root buffer using WIDTH when it changed.

When FORCE is non-nil, reflow even if WIDTH matches current value."
  (when (and (integerp width)
             (> width 0)
             (or force
                 (not (eq width disco-root--fill-column))))
    (setq disco-root--fill-column width)
    (disco-root--reflow-preserving-position)
    t))

(defun disco-root-buffer-auto-fill (&optional force)
  "Reflow current root buffer to match current window width.

With FORCE non-nil, rerender even if width has not changed."
  (interactive "P")
  (unless (derived-mode-p 'disco-root-mode)
    (user-error "disco: `disco-root-buffer-auto-fill' only works in root buffer"))
  (when (or force disco-root-auto-fill-on-window-size-change)
    (disco-root--auto-fill-to-width
     (disco-root--compute-fill-column)
     force)))

(defun disco-root--window-size-change (frame)
  "Reflow visible root buffers for resized FRAME windows."
  (when disco-root-auto-fill-on-window-size-change
    (let ((buffer-widths (make-hash-table :test #'eq)))
      (walk-windows
       (lambda (win)
         (let ((buf (window-buffer win)))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (eq major-mode 'disco-root-mode)
                 (let ((width (disco-root--compute-fill-column buf win)))
                   (when (and (integerp width) (> width 0))
                     (puthash buf
                              (max width (or (gethash buf buffer-widths) 0))
                              buffer-widths))))))))
       nil
       frame)
      (maphash
       (lambda (buf width)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (disco-root--auto-fill-to-width width nil))))
       buffer-widths))))

(defun disco-root--ensure-window-size-hook ()
  "Install global window-size hook used by root auto-fill."
  (unless disco-root--window-size-hook-installed
    (add-hook 'window-size-change-functions #'disco-root--window-size-change)
    (setq disco-root--window-size-hook-installed t)))

(defun disco-root--ensure-section-state (&optional sections)
  "Ensure expansion defaults for root SECTIONS."
  (dolist (section (or sections disco-root--section-order))
    (unless (assq section disco-root--section-expanded)
      (push (cons section t) disco-root--section-expanded)))
  (setq disco-root--section-expanded
        (nreverse (seq-uniq (nreverse disco-root--section-expanded)
                            (lambda (left right)
                              (eq (car left) (car right)))))))

(defun disco-root--section-expanded-p (section)
  "Return non-nil when root SECTION is expanded."
  (if-let* ((entry (assq section disco-root--section-expanded)))
      (and (cdr entry) t)
    t))

(defun disco-root--set-section-expanded (section expanded)
  "Set root SECTION expansion to EXPANDED."
  (if-let* ((entry (assq section disco-root--section-expanded)))
      (setcdr entry (and expanded t))
    (push (cons section (and expanded t)) disco-root--section-expanded))
  (and expanded t))

(defun disco-root--ensure-collapse-state-tables ()
  "Ensure root guild/category expansion tables exist."
  (unless (hash-table-p disco-root--guild-expanded)
    (setq disco-root--guild-expanded (make-hash-table :test #'equal)))
  (unless (hash-table-p disco-root--category-expanded)
    (setq disco-root--category-expanded (make-hash-table :test #'equal))))

(defun disco-root--node-expanded-p (table key)
  "Return expansion state for KEY in TABLE, defaulting to collapsed."
  (disco-root--ensure-collapse-state-tables)
  (if (null key) t (and (gethash key table) t)))

(defun disco-root--set-node-expanded (table key expanded)
  "Store EXPANDED for KEY in TABLE."
  (disco-root--ensure-collapse-state-tables)
  (when key
    (puthash key (and expanded t) table))
  (and expanded t))

(defun disco-root--guild-expanded-p (guild-id)
  "Return non-nil when GUILD-ID is expanded."
  (disco-root--node-expanded-p disco-root--guild-expanded guild-id))

(defun disco-root--set-guild-expanded (guild-id expanded)
  "Set GUILD-ID expansion to EXPANDED."
  (disco-root--set-node-expanded disco-root--guild-expanded guild-id expanded))

(defun disco-root--category-expanded-p (category-id)
  "Return non-nil when CATEGORY-ID is expanded."
  (disco-root--node-expanded-p disco-root--category-expanded category-id))

(defun disco-root--set-category-expanded (category-id expanded)
  "Set CATEGORY-ID expansion to EXPANDED."
  (disco-root--set-node-expanded disco-root--category-expanded category-id expanded))

(defun disco-root--toggle-section (section)
  "Toggle root SECTION expansion and rerender the projection."
  (disco-root--set-section-expanded
   section (not (disco-root--section-expanded-p section)))
  (disco-root--render-preserving-position))

(defun disco-root--toggle-guild (guild-id)
  "Toggle GUILD-ID expansion, hydrating its directory when opened."
  (unless guild-id
    (user-error "disco: missing guild id at point"))
  (let ((expanding (not (disco-root--guild-expanded-p guild-id))))
    (disco-root--set-guild-expanded guild-id expanding)
    (when expanding
      (disco-directory-load-guild-async guild-id))
    (disco-root--render-preserving-position)))

(defun disco-root--toggle-category (category-id)
  "Toggle CATEGORY-ID expansion and rerender the projection."
  (unless category-id
    (user-error "disco: missing category id at point"))
  (disco-root--set-category-expanded
   category-id (not (disco-root--category-expanded-p category-id)))
  (disco-root--render-preserving-position))

(defun disco-root--toggle-node-at-point ()
  "Apply the controller action for the collapsible row at point."
  (cond
   ((disco-root--line-section)
    (disco-root--toggle-section (disco-root--line-section))
    t)
   ((disco-root--line-guild-id)
    (disco-root--toggle-guild (disco-root--line-guild-id))
    t)
   ((disco-root--line-category-id)
    (disco-root--toggle-category (disco-root--line-category-id))
    t)
   (t nil)))


(defun disco-root-toggle-section-at-point ()
  "Toggle collapsible node at point.

Supports top-level sections, guild rows, and category rows. In layouts
without collapsible nodes, move to next channel row."
  (interactive)
  (unless (disco-root--toggle-node-at-point)
    (if (eq (disco-root--ensure-layout) 'activity)
        (disco-root-button-forward 1)
      (user-error "disco: point is not on a collapsible row"))))

(defun disco-root-tab-dwim ()
  "On collapsible row, toggle it; otherwise move to next channel row."
  (interactive)
  (unless (disco-root--toggle-node-at-point)
    (disco-root-button-forward 1)))

(defun disco-root--refresh-channel-node (channel-id)
  "Refresh one CHANNEL-ID row in EWOC.

Return one of symbols:
- `updated' when at least one visible row was patched.
- `missing' when CHANNEL-ID has no visible EWOC nodes.
- `stale' when node exists but backing state can no longer patch it."
  (let* ((nodes (and channel-id
                     disco-root--channel-node-table
                     (gethash channel-id disco-root--channel-node-table)))
         (result
          (cond
           ((or (null channel-id)
                (null disco-root--ewoc)
                (null nodes))
            'missing)
           (t
            (let ((channel (disco-state-channel channel-id))
                  updated)
              (if (and channel (disco-root--displayable-channel-p channel))
                  (progn
                    (let ((inhibit-read-only t))
                      (dolist (node (if (listp nodes) nodes (list nodes)))
                        (let ((entry (copy-sequence (ewoc-data node))))
                          (setf (disco-root-layout-entry-channel entry) channel)
                          (ewoc-set-data node entry)
                          (ewoc-invalidate disco-root--ewoc node)
                          (setq updated t))))
                    (if updated 'updated 'missing))
                'stale))))))
    (when (disco-root--debug-log-verbose-p)
      (disco-root--debug-log "refresh-channel %s -> %s" channel-id result))
    result))

(defun disco-root--channel-node-list (channel-id)
  "Return normalized EWOC node list for CHANNEL-ID."
  (let ((nodes (and channel-id
                    disco-root--channel-node-table
                    (gethash channel-id disco-root--channel-node-table))))
    (cond
     ((null nodes) nil)
     ((listp nodes) nodes)
     (t (list nodes)))))

(defun disco-root--activity-channel-node (channel-id)
  "Return primary activity EWOC node for CHANNEL-ID, or nil."
  (car (disco-root--channel-node-list channel-id)))

(defun disco-root--move-channel-node-before (channel-id node before-node)
  "Move CHANNEL-ID NODE before BEFORE-NODE, returning resulting node."
  (if (or (null node)
          (null disco-root--ewoc)
          (eq node before-node))
      node
    (let ((entry (copy-sequence (ewoc-data node))))
      (ewoc-delete disco-root--ewoc node)
      (setq node (if before-node
                     (ewoc-enter-before disco-root--ewoc before-node entry)
                   (ewoc-enter-last disco-root--ewoc entry)))
      (when channel-id
        (puthash channel-id (list node) disco-root--channel-node-table))
      node)))

(defun disco-root--activity-channel-before-p (left right)
  "Return non-nil when LEFT should be ordered before RIGHT in activity view."
  (pcase disco-root--sort-mode
    ('name
     (string-lessp (disco-root--channel-display-name left)
                   (disco-root--channel-display-name right)))
    (_
     (let ((left-score (disco-root--channel-activity-score left))
           (right-score (disco-root--channel-activity-score right)))
       (if (= left-score right-score)
           (string-lessp (disco-root--channel-display-name left)
                         (disco-root--channel-display-name right))
         (> left-score right-score))))))

(defun disco-root--activity-node-channel (node)
  "Return channel object stored in activity EWOC NODE, or nil."
  (when node
    (let ((entry (ewoc-data node)))
      (when (eq (disco-root-layout-entry-type entry) 'channel)
        (disco-root-layout-entry-channel entry)))))

(defun disco-root--activity-node-prev-channel (node)
  "Return previous activity channel node before NODE, or nil."
  (let ((probe (and node (ewoc-prev disco-root--ewoc node))))
    (while (and probe (null (disco-root--activity-node-channel probe)))
      (setq probe (ewoc-prev disco-root--ewoc probe)))
    probe))

(defun disco-root--activity-node-next-channel (node)
  "Return next activity channel node after NODE, or nil."
  (let ((probe (and node (ewoc-next disco-root--ewoc node))))
    (while (and probe (null (disco-root--activity-node-channel probe)))
      (setq probe (ewoc-next disco-root--ewoc probe)))
    probe))

(defun disco-root--activity-reposition-existing-node (channel-id channel node)
  "Reposition CHANNEL-ID NODE for CHANNEL using local neighbor checks."
  (let* ((prev-node (disco-root--activity-node-prev-channel node))
         (next-node (disco-root--activity-node-next-channel node))
         (prev-channel (and prev-node (disco-root--activity-node-channel prev-node)))
         (next-channel (and next-node (disco-root--activity-node-channel next-node)))
         target-before
         move-to-end)
    (cond
     ((and prev-channel
           (disco-root--activity-channel-before-p channel prev-channel))
      (setq target-before prev-node)
      (let ((probe (disco-root--activity-node-prev-channel prev-node)))
        (while (and probe
                    (disco-root--activity-channel-before-p
                     channel
                     (disco-root--activity-node-channel probe)))
          (setq target-before probe)
          (setq probe (disco-root--activity-node-prev-channel probe)))))
     ((and next-channel
           (disco-root--activity-channel-before-p next-channel channel))
      (let ((probe next-node))
        (while (and probe
                    (disco-root--activity-channel-before-p
                     (disco-root--activity-node-channel probe)
                     channel))
          (setq probe (disco-root--activity-node-next-channel probe)))
        (setq target-before probe)
        (setq move-to-end (null probe)))))
    (when (or (and target-before (not (eq target-before node)))
              move-to-end)
      (disco-root--move-channel-node-before channel-id node target-before))))

(defun disco-root--activity-reorder-channel-node (channel-id)
  "Reposition CHANNEL-ID node in activity EWOC.

Return `missing-visible' when a visible channel has no EWOC node."
  (let* ((channel (and channel-id (disco-state-channel channel-id)))
         (visible (and channel (disco-root--activity-channel-eligible-p channel)))
         (node (and channel-id (disco-root--activity-channel-node channel-id))))
    (cond
     ((and visible node)
      (disco-root--activity-reposition-existing-node channel-id channel node)
      'moved)
     ((and visible (null node))
      'missing-visible)
     ((and (not visible) node)
      (ewoc-delete disco-root--ewoc node)
      (remhash channel-id disco-root--channel-node-table)
      'removed)
     (t
      'unchanged))))

(defun disco-root--activity-reorder-visible-nodes (&optional channel-ids)
  "Reorder activity rows to match current sort/filter state.

When CHANNEL-IDS is non-nil, only reposition those rows and return non-nil
if structural reconciliation is required."
  (when (and disco-root--ewoc
             (eq (disco-root--ensure-layout) 'activity))
    (if channel-ids
        (let ((dirty-ids (seq-uniq (delq nil channel-ids) #'equal))
              missing-visible)
          (dolist (channel-id dirty-ids)
            (when (eq (disco-root--activity-reorder-channel-node channel-id)
                      'missing-visible)
              (setq missing-visible t)))
          (when (disco-root--debug-log-verbose-p)
            (disco-root--debug-log
             "activity-reorder dirty=%s missing=%s"
             dirty-ids
             (and missing-visible t)))
          missing-visible)
      (let ((cursor (ewoc-nth disco-root--ewoc 0)))
        (dolist (channel (disco-root--collect-activity-channels))
          (let* ((channel-id (alist-get 'id channel))
                 (node (and channel-id
                            (disco-root--activity-channel-node channel-id))))
            (when node
              (unless (eq node cursor)
                (setq node (disco-root--move-channel-node-before channel-id
                                                                 node
                                                                 cursor)))
              (setq cursor (ewoc-next disco-root--ewoc node)))))
        (when (disco-root--debug-log-verbose-p)
          (disco-root--debug-log "activity-reorder full"))
        nil))))

(defun disco-root--count-visible-unread-channels ()
  "Return count of unread channels visible under current root view mode."
  (let ((seen (make-hash-table :test #'equal))
        (count 0))
    (cl-labels
        ((mark (channel)
           (let ((channel-id (alist-get 'id channel)))
             (when (and channel-id
                        (not (gethash channel-id seen))
                        (disco-root--displayable-channel-p channel)
                        (disco-root--channel-visible-in-view-p channel)
                        (disco-root--channel-has-unread-p channel))
               (puthash channel-id t seen)
               (setq count (1+ count))))))
      (dolist (channel (disco-state-private-channels))
        (mark channel))
      (dolist (guild (or (disco-state-guilds) '()))
        (let ((guild-id (alist-get 'id guild)))
          (dolist (channel (or (disco-state-guild-channels guild-id) '()))
            (mark channel))
          (dolist (thread (or (disco-state-guild-threads guild-id) '()))
            (mark thread)))))
    count))

(defun disco-root--refresh-section-node (section count)
  "Patch SECTION header EWOC node COUNT in place when present."
  (let ((node (and (hash-table-p disco-root--section-node-table)
                   (gethash section disco-root--section-node-table))))
    (when (and node disco-root--ewoc)
      (let ((entry (copy-sequence (ewoc-data node))))
        (setf (disco-root-layout-entry-count entry) count)
        (ewoc-set-data node entry)
        (ewoc-invalidate disco-root--ewoc node)
        t))))

(defun disco-root--refresh-section-nodes ()
  "Refresh unread/private/guild section counters incrementally."
  (when (hash-table-p disco-root--section-node-table)
    (disco-root--refresh-section-node 'unread
                                      (disco-root--count-visible-unread-channels))
    (disco-root--refresh-section-node 'private
                                      (length
                                       (seq-filter #'disco-root--channel-visible-in-view-p
                                                   (disco-state-private-channels))))
    (disco-root--refresh-section-node 'guilds
                                      (length (or (disco-state-guilds) '())))))

(defun disco-root--category-id-for-channel (channel)
  "Return category ID that CHANNEL belongs to, or nil."
  (when channel
    (let* ((parent-id (alist-get 'parent_id channel))
           (parent-channel (and parent-id (disco-state-channel parent-id))))
      (cond
       ((disco-state-channel-thread-p channel)
        (let* ((category-id (and parent-channel
                                 (alist-get 'parent_id parent-channel)))
               (category (and category-id (disco-state-channel category-id))))
          (when (and category (disco-root--channel-category-p category))
            category-id)))
       ((and parent-channel
             (disco-root--channel-category-p parent-channel))
        parent-id)
       (t nil)))))

(defun disco-root--category-visible-children (category)
  "Return visible child parent-channels under CATEGORY."
  (let ((category-id (and category (alist-get 'id category)))
        (guild-id (and category (alist-get 'guild_id category)))
        children)
    (when (and category-id guild-id)
      (dolist (channel (or (disco-state-guild-channels guild-id) '()))
        (when (and (equal (alist-get 'parent_id channel) category-id)
                   (disco-root--displayable-channel-p channel)
                   (disco-root--channel-visible-in-view-p channel)
                   (not (disco-state-channel-thread-p channel)))
          (push channel children))))
    (disco-root--sort-channels (nreverse children))))

(defun disco-root--refresh-guild-node (guild-id)
  "Patch one guild header node by GUILD-ID, returning non-nil on update."
  (let ((node (and guild-id
                   (hash-table-p disco-root--guild-node-table)
                   (gethash guild-id disco-root--guild-node-table))))
    (when (and node disco-root--ewoc)
      (let ((guild (disco-root--guild-by-id guild-id)))
        (when guild
          (let ((entry (copy-sequence (ewoc-data node))))
            (setf (disco-root-layout-entry-guild entry) guild)
            (setf (disco-root-layout-entry-unread-count entry)
                  (disco-root--guild-unread-total guild-id t))
            (ewoc-set-data node entry)
            (ewoc-invalidate disco-root--ewoc node)
            t))))))

(defun disco-root--refresh-category-node (category-id)
  "Patch one category header node by CATEGORY-ID, returning non-nil on update."
  (let ((node (and category-id
                   (hash-table-p disco-root--category-node-table)
                   (gethash category-id disco-root--category-node-table))))
    (when (and node disco-root--ewoc)
      (let ((category (disco-state-channel category-id)))
        (when category
          (let* ((children (disco-root--category-visible-children category))
                 (entry (copy-sequence (ewoc-data node))))
            (setf (disco-root-layout-entry-category entry) category)
            (setf (disco-root-layout-entry-unread-count entry)
                  (disco-root--category-children-unread-total children))
            (ewoc-set-data node entry)
            (ewoc-invalidate disco-root--ewoc node)
            t))))))

(defun disco-root--refresh-heading-nodes (channel-ids)
  "Patch section/guild/category heading rows related to CHANNEL-IDS."
  (let (guild-ids
        category-ids)
    (dolist (channel-id channel-ids)
      (let ((channel (disco-state-channel channel-id)))
        (when channel
          (when-let* ((guild-id (alist-get 'guild_id channel)))
            (cl-pushnew guild-id guild-ids :test #'equal))
          (when-let* ((category-id (disco-root--category-id-for-channel channel)))
            (cl-pushnew category-id category-ids :test #'equal)))))
    (disco-root--refresh-section-nodes)
    (dolist (guild-id guild-ids)
      (disco-root--refresh-guild-node guild-id))
    (dolist (category-id category-ids)
      (disco-root--refresh-category-node category-id))))

(defun disco-root--cancel-live-update-timer ()
  "Cancel pending root live-update debounce timer when present."
  (when (timerp disco-root--live-update-timer)
    (cancel-timer disco-root--live-update-timer)
    (setq disco-root--live-update-timer nil)))

(defun disco-root--cancel-missing-preview-fetch-timer ()
  "Cancel pending proactive missing-preview fetch timer when present."
  (when (timerp disco-root--missing-preview-fetch-timer)
    (cancel-timer disco-root--missing-preview-fetch-timer)
    (setq disco-root--missing-preview-fetch-timer nil)))

(defun disco-root--live-updatable-buffer-mode-p ()
  "Return non-nil when current buffer supports root-style live updates."
  (memq major-mode '(disco-root-mode
                     disco-root-parent-threads-mode
                     disco-root-archived-threads-mode)))

(defun disco-root--thread-browser-buffer-mode-p ()
  "Return non-nil when current buffer is a thread browser list buffer."
  (memq major-mode '(disco-root-parent-threads-mode
                     disco-root-archived-threads-mode)))

(defun disco-root--schedule-missing-preview-fetch ()
  "Schedule debounced proactive fetch for missing preview messages."
  (unless (timerp disco-root--missing-preview-fetch-timer)
    (setq disco-root--missing-preview-fetch-timer
          (run-with-timer
           (max 0 (or disco-root-missing-preview-fetch-debounce 0))
           nil
           #'disco-root--flush-missing-preview-fetches
           (current-buffer)))))

(defun disco-root--flush-missing-preview-fetches (root-buffer)
  "Flush queued missing-preview requests in ROOT-BUFFER via op34."
  (when (buffer-live-p root-buffer)
    (with-current-buffer root-buffer
      (when (disco-root--live-updatable-buffer-mode-p)
        (setq disco-root--missing-preview-fetch-timer nil)
        (when (and (disco-gateway-running-p)
                   (hash-table-p disco-root--missing-preview-pending-by-guild)
                   (> (hash-table-count disco-root--missing-preview-pending-by-guild) 0))
          (let ((max-per-guild
                 (max 1 (or disco-root-missing-preview-fetch-max-per-guild 1)))
                next-pending)
            (maphash
             (lambda (guild-id pending-channel-ids)
               (let* ((ordered (disco-util-normalize-id-list pending-channel-ids))
                      (batch (seq-take ordered max-per-guild))
                      (remaining (nthcdr (length batch) ordered)))
                 (cond
                  ((null ordered)
                   nil)
                  ((not guild-id)
                   (push (cons guild-id ordered) next-pending))
                  ((not (disco-gateway-send-queue-slot-available-p))
                   (push (cons guild-id ordered) next-pending))
                  ((and batch
                        (disco-gateway-request-last-messages guild-id batch))
                   (when remaining
                     (push (cons guild-id remaining) next-pending)))
                  (t
                   (push (cons guild-id ordered) next-pending)))))
             disco-root--missing-preview-pending-by-guild)
            (clrhash disco-root--missing-preview-pending-by-guild)
            (dolist (entry next-pending)
              (puthash (car entry)
                       (cdr entry)
                       disco-root--missing-preview-pending-by-guild))
            (when next-pending
              (disco-root--schedule-missing-preview-fetch))))))))

(defun disco-root--queue-missing-preview-fetch (channel)
  "Queue proactive op34 fetch for CHANNEL missing cached preview message.

Each channel `last_message_id' is only requested once per root session."
  (when (and disco-root-missing-preview-fetch-enabled
             (listp channel)
             (disco-gateway-running-p))
    (let* ((guild-id (and (alist-get 'guild_id channel)
                          (format "%s" (alist-get 'guild_id channel))))
           (channel-id (and (alist-get 'id channel)
                            (format "%s" (alist-get 'id channel))))
           (last-message-id (and (alist-get 'last_message_id channel)
                                 (format "%s" (alist-get 'last_message_id channel))))
           (pending-by-guild
            (or disco-root--missing-preview-pending-by-guild
                (setq-local disco-root--missing-preview-pending-by-guild
                            (make-hash-table :test #'equal))))
           (requested-by-channel
            (or disco-root--missing-preview-requested-last-message-id-by-channel
                (setq-local
                 disco-root--missing-preview-requested-last-message-id-by-channel
                 (make-hash-table :test #'equal))))
           (requested-last-message-id
            (and channel-id
                 (gethash channel-id requested-by-channel))))
      (when (and guild-id
                 channel-id
                 (stringp last-message-id)
                 (not (equal requested-last-message-id last-message-id))
                 (not (disco-msg-channel-last-cached-message channel)))
        (puthash channel-id last-message-id requested-by-channel)
        (let ((pending (copy-sequence (or (gethash guild-id pending-by-guild)
                                          '()))))
          (cl-pushnew channel-id pending :test #'equal)
          (puthash guild-id pending pending-by-guild))
        (disco-root--schedule-missing-preview-fetch)))))

(defun disco-root--maybe-queue-missing-preview-fetch (channel)
  "Queue a fetch when CHANNEL could have a preview but none is cached."
  (when (and (listp channel)
             (alist-get 'last_message_id channel)
             (not (disco-msg-channel-last-cached-message channel)))
    (disco-root--queue-missing-preview-fetch channel)))

(defun disco-root--queue-live-update (channel-ids &optional structural-p header-p)
  "Queue CHANNEL-IDS for debounced UI update.

When STRUCTURAL-P is non-nil, the next flush performs full reconcile.
When HEADER-P is non-nil, root header line is refreshed on flush."
  (let ((ids (cond
              ((null channel-ids) nil)
              ((listp channel-ids) channel-ids)
              (t (list channel-ids)))))
    (disco-root--debug-log
     "queue-live-update ids=%s structural=%s header=%s"
     ids
     (and structural-p t)
     (and header-p t))
    (dolist (channel-id ids)
      (when channel-id
        (cl-pushnew channel-id disco-root--dirty-channel-ids :test #'equal))))
  (setq disco-root--dirty-structure-p
        (or disco-root--dirty-structure-p structural-p))
  (setq disco-root--dirty-header-p
        (or disco-root--dirty-header-p header-p))
  (unless (timerp disco-root--live-update-timer)
    (setq disco-root--live-update-timer
          (run-with-timer
           (max 0 disco-root-live-update-debounce)
           nil
           #'disco-root--flush-live-updates
           (current-buffer)))))

(defun disco-root--current-thread-browser-list-spec ()
  "Return current thread-browser list spec for the active special buffer."
  (pcase major-mode
    ('disco-root-parent-threads-mode
     (disco-root--parent-threads-list-spec))
    ('disco-root-archived-threads-mode
     (disco-root--archived-threads-list-spec))
    (_ nil)))

(defun disco-root--flush-live-updates (root-buffer)
  "Flush queued live updates into ROOT-BUFFER."
  (when (buffer-live-p root-buffer)
    (with-current-buffer root-buffer
      (when (disco-root--live-updatable-buffer-mode-p)
        (setq disco-root--live-update-timer nil)
        (if (and (eq major-mode 'disco-root-mode)
                 disco-root--refresh-in-flight)
            (setq disco-root--live-update-timer
                  (run-with-timer
                   (max 0.02 disco-root-live-update-debounce)
                   nil
                   #'disco-root--flush-live-updates
                   root-buffer))
          (let* ((dirty-channel-ids (nreverse disco-root--dirty-channel-ids))
                 (needs-structural disco-root--dirty-structure-p)
                 (needs-header disco-root--dirty-header-p))
            (setq disco-root--dirty-channel-ids nil)
            (setq disco-root--dirty-structure-p nil)
            (setq disco-root--dirty-header-p nil)
            (cond
             ((disco-root--thread-browser-buffer-mode-p)
              (when (or dirty-channel-ids needs-structural needs-header)
                (when-let* ((spec (disco-root--current-thread-browser-list-spec)))
                  (disco-view-render-list-spec-preserving-position
                   spec
                   :anchor-property 'disco-channel-id
                   :preserve-window-start t
                   :after-restore #'disco-root--update-window-points))))
             (t
              (let* ((layout (disco-root--ensure-layout))
                     (layout-update-mode
                      (disco-root-layout-update-mode layout)))
                (disco-root--debug-log
                 "flush-live-updates layout=%s view=%s dirty=%d structural=%s header=%s"
                 layout
                 disco-root--view-mode
                 (length dirty-channel-ids)
                 (and needs-structural t)
                 (and needs-header t))
                (cond
                 (needs-structural
                  (disco-root--debug-log "flush-live-updates -> structural")
                  (disco-root--render-preserving-position))
                 ((eq layout 'search)
                  (disco-root--debug-log "flush-live-updates -> search-static")
                  (when needs-header
                    (disco-root--refresh-header-line)))
                 ((and (eq layout-update-mode 'full)
                       (or dirty-channel-ids needs-header))
                  (disco-root--debug-log "flush-live-updates -> full-render")
                  (if (and needs-header (null dirty-channel-ids))
                      (disco-root--refresh-header-line)
                    (disco-root--render-preserving-position)))
                 ((and dirty-channel-ids
                       (eq disco-root--view-mode 'unread))
                  (disco-root--debug-log "flush-live-updates -> unread-render")
                  (disco-root--render-preserving-position))
                 (t
                  (disco-root--debug-log "flush-live-updates -> incremental")
                  (let ((inhibit-read-only t)
                        (buffer-undo-list t)
                        (position-snapshot
                         (disco-view-capture-position
                          :anchor-property 'disco-channel-id
                          :preserve-window-start t)))
                    (with-silent-modifications
                      (dolist (channel-id dirty-channel-ids)
                        (when (eq (disco-root--refresh-channel-node channel-id) 'stale)
                          (setq needs-structural t)
                          (disco-root--debug-log
                           "flush-live-updates -> structural(stale %s)" channel-id)))
                      (when (and (not needs-structural)
                                 dirty-channel-ids
                                 (eq layout 'activity))
                        (when (disco-root--activity-reorder-visible-nodes dirty-channel-ids)
                          (setq needs-structural t)
                          (disco-root--debug-log
                           "flush-live-updates -> structural(activity-reorder)")))
                      (when dirty-channel-ids
                        (disco-root--refresh-active-layout-headings dirty-channel-ids))
                      (cond
                       (needs-header
                        (disco-root--refresh-header-line))
                       ((and dirty-channel-ids
                             (eq layout 'activity))
                        (disco-root--maybe-refresh-activity-header-line)))
                      (when (and (not needs-structural)
                                 position-snapshot)
                        (disco-view-restore-position position-snapshot)
                        (disco-root--update-window-points))))
                  (when needs-structural
                    (disco-root--debug-log "flush-live-updates -> structural-reconcile")
                    (disco-root--render-preserving-position)))))))))))))

(defun disco-root--handle-gateway-event (event)
  "Apply one gateway EVENT to root buffer view."
  (let ((event-type (plist-get event :type)))
    (when (eq event-type 'guild-members-chunk)
      (disco-root--search-refresh-active-completions (plist-get event :guild-id)))
    (when (disco-root--live-event-p event-type)
      (let ((channel-ids (disco-root--event-channel-ids event))
            (structural (disco-root--live-event-structural-p event-type))
            (header (disco-root--live-event-header-p event-type)))
        (disco-root--debug-log
         "gateway-event %s ids=%s structural=%s header=%s"
         event-type
         channel-ids
         (and structural t)
         (and header t))
        (disco-root--queue-live-update channel-ids structural header)))))

(defun disco-root--handle-directory-event (event)
  "Project one directory lifecycle EVENT into the root buffer."
  (pcase (plist-get event :type)
    ('index-loading
     (setq disco-root--refresh-in-flight t)
     (disco-root--queue-live-update nil nil t))
    ('index-loaded
     (setq disco-root--refresh-in-flight nil)
     (disco-root--queue-live-update nil t t)
     (when-let* ((errors (plist-get event :errors)))
       (message "disco: directory index errors: %s"
                (mapconcat
                 (lambda (entry)
                   (format "%s: %s" (car entry)
                           (disco-root--async-error-message (cdr entry))))
                 errors "; "))))
    ('guild-loading
     (puthash (plist-get event :guild-id) 'loading
              disco-root--guild-load-status)
     (disco-root--queue-live-update nil t nil))
    ('guild-loaded
     (puthash (plist-get event :guild-id) 'loaded
              disco-root--guild-load-status)
     (disco-root--queue-live-update nil t nil))
    ('guild-error
     (puthash (plist-get event :guild-id) 'error
              disco-root--guild-load-status)
     (disco-root--queue-live-update nil t nil)
     (message "disco: failed to load guild %s channels: %s"
              (plist-get event :guild-id)
              (disco-root--async-error-message (plist-get event :error))))
    ('guild-enriched
     (disco-root--queue-live-update nil t nil))
    ('guild-enrichment-error
     (message "disco: failed to load guild %s active threads: %s"
              (plist-get event :guild-id)
              (disco-root--async-error-message (plist-get event :error))))))

(defun disco-root--attach-live-updates ()
  "Attach root buffer to global gateway update stream."
  (when disco-root--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-root--gateway-handler)
    (disco-gateway-unwatch-global))
  (when disco-root--directory-handler
    (remove-hook 'disco-directory-event-hook disco-root--directory-handler))
  (disco-root--cancel-live-update-timer)
  (disco-root--cancel-missing-preview-fetch-timer)
  (setq disco-root--dirty-channel-ids nil)
  (setq disco-root--dirty-structure-p nil)
  (setq disco-root--dirty-header-p nil)
  (when (hash-table-p disco-root--missing-preview-pending-by-guild)
    (clrhash disco-root--missing-preview-pending-by-guild))
  (when (hash-table-p disco-root--missing-preview-requested-last-message-id-by-channel)
    (clrhash disco-root--missing-preview-requested-last-message-id-by-channel))
  (let ((root-buffer (current-buffer)))
    (setq disco-root--gateway-handler
          (lambda (event)
            (when (buffer-live-p root-buffer)
              (with-current-buffer root-buffer
                (disco-root--handle-gateway-event event)))))
    (when (eq major-mode 'disco-root-mode)
      (setq disco-root--directory-handler
            (lambda (event)
              (when (buffer-live-p root-buffer)
                (with-current-buffer root-buffer
                  (disco-root--handle-directory-event event)))))))
  (add-hook 'disco-gateway-event-hook disco-root--gateway-handler)
  (when disco-root--directory-handler
    (add-hook 'disco-directory-event-hook disco-root--directory-handler))
  (disco-gateway-watch-global)
  (add-hook 'kill-buffer-hook #'disco-root--detach-live-updates nil t))

(defun disco-root--detach-live-updates ()
  "Detach root buffer from global gateway update stream."
  (disco-root--cancel-live-update-timer)
  (disco-root--cancel-missing-preview-fetch-timer)
  (setq disco-root--dirty-channel-ids nil)
  (setq disco-root--dirty-structure-p nil)
  (setq disco-root--dirty-header-p nil)
  (when (hash-table-p disco-root--missing-preview-pending-by-guild)
    (clrhash disco-root--missing-preview-pending-by-guild))
  (when (hash-table-p disco-root--missing-preview-requested-last-message-id-by-channel)
    (clrhash disco-root--missing-preview-requested-last-message-id-by-channel))
  (when disco-root--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-root--gateway-handler)
    (setq disco-root--gateway-handler nil))
  (when disco-root--directory-handler
    (remove-hook 'disco-directory-event-hook disco-root--directory-handler)
    (setq disco-root--directory-handler nil))
  (disco-gateway-unwatch-global))

(defun disco-root-toggle-sort-mode ()
  "Toggle root channel sort mode between activity and name."
  (interactive)
  (setq disco-root--sort-mode (if (eq disco-root--sort-mode 'activity) 'name 'activity))
  (disco-root-render)
  (message "disco: root sort mode -> %s" disco-root--sort-mode))

(defun disco-root-cycle-view-mode ()
  "Cycle root view mode across all, unread, and dms."
  (interactive)
  (disco-root--set-view-mode
   (pcase disco-root--view-mode
     ('all 'unread)
     ('unread 'dms)
     (_ 'all)))
  (disco-root-render)
  (message "disco: root view mode -> %s" disco-root--view-mode))

(defun disco-root--refresh-active-layout-headings (channel-ids)
  "Patch heading rows for current layout using CHANNEL-IDS context."
  (if-let* ((refresh-fn (disco-root-layout-refresh-headings-function
                         (disco-root--ensure-layout))))
      (funcall refresh-fn channel-ids)
    (pcase (disco-root--ensure-layout)
      ('activity nil)
      (_
       (disco-root--refresh-heading-nodes channel-ids)))))

(defun disco-root--feature-badge-summary ()
  "Return compact summary string for read-state feature badge counters."
  (let (parts)
    (dolist
        (spec
         `((,(and (boundp 'disco-read-state-type-guild-event)
                  disco-read-state-type-guild-event) . "events")
           (,(and (boundp 'disco-read-state-type-notification-center)
                  disco-read-state-type-notification-center) . "notifications")
           (,(and (boundp 'disco-read-state-type-guild-home)
                  disco-read-state-type-guild-home) . "home")
           (,(and (boundp 'disco-read-state-type-guild-onboarding-question)
                  disco-read-state-type-guild-onboarding-question) . "onboarding")
           (,(and (boundp 'disco-read-state-type-message-requests)
                  disco-read-state-type-message-requests) . "requests")))
      (let ((read-state-type (car spec))
            (label (cdr spec)))
        (when (numberp read-state-type)
          (let ((count (disco-state-read-state-counter-total read-state-type)))
            (when (> count 0)
              (push (format "%s:%d" label count) parts))))))
    (when parts
      (format "badges[%s]" (string-join (nreverse parts) "  ")))))

(defun disco-root--gateway-status-label ()
  "Return short gateway/root status label for header display."
  (cond
   (disco-root--refresh-in-flight
    "Refreshing")
   ((and (boundp 'disco-gateway--connecting)
         disco-gateway--connecting)
    "Connecting")
   ((and (boundp 'disco-gateway--ws)
         disco-gateway--ws
         (fboundp 'websocket-openp)
         (ignore-errors (websocket-openp disco-gateway--ws)))
    "Ready")
   ((and (boundp 'disco-gateway--session-id)
         disco-gateway--session-id)
    "Idle")
   (t
    "Offline")))

(defun disco-root--sessions-summary-label ()
  "Return compact sessions summary for root header, or nil."
  (let* ((sessions (disco-state-sessions))
         (overall (disco-state-overall-session))
         (overall-status (and (listp overall)
                              (let ((status (alist-get 'status overall)))
                                (and (stringp status)
                                     (not (string-empty-p status))
                                     status))))
         (session-count (length sessions))
         (device-count (if overall
                           (max 0 (1- session-count))
                         session-count)))
    (when (> session-count 0)
      (if overall-status
          (format "sessions[%s/%d]" overall-status device-count)
        (format "sessions[%d]" device-count)))))

(defun disco-root--voice-summary-label ()
  "Return compact tracked voice summary for root header, or nil."
  (let ((channel-count (disco-state-voice-active-channel-count))
        (user-count (disco-state-voice-active-user-count)))
    (when (> (+ channel-count user-count) 0)
      (format "voice[%dc %du]" channel-count user-count))))

(defun disco-root--activity-metrics-by-view ()
  "Return alist MODE -> plist metrics for activity header chips."
  (let ((all-count 0)
        (all-unread 0)
        (unread-count 0)
        (unread-unread 0)
        (dms-count 0)
        (dms-unread 0))
    (dolist (channel (disco-root--collect-activity-candidates))
      (let ((own-unread (disco-state-channel-own-unread-count channel)))
        (setq all-count (1+ all-count))
        (setq all-unread (+ all-unread own-unread))
        (when (disco-root--channel-visible-in-mode-p channel 'unread)
          (setq unread-count (1+ unread-count))
          (setq unread-unread (+ unread-unread own-unread)))
        (when (disco-root--channel-visible-in-mode-p channel 'dms)
          (setq dms-count (1+ dms-count))
          (setq dms-unread (+ dms-unread own-unread)))))
    `((all :count ,all-count :unread ,all-unread)
      (unread :count ,unread-count :unread ,unread-unread)
      (dms :count ,dms-count :unread ,dms-unread))))

(defun disco-root--filter-chip (mode label metrics)
  "Return one filter chip string for MODE and LABEL from METRICS."
  (let* ((stats (alist-get mode metrics))
         (count (or (plist-get stats :count) 0))
         (unread (or (plist-get stats :unread) 0))
         (active (eq disco-root--view-mode mode)))
    (format "[%d:%s%s %s]"
            count
            (if active "·" "")
            label
            (disco-root--human-count unread))))

(defun disco-root--search-summary-line ()
  "Return summary line for active root search session."
  (if (not (and disco-root--search-domain
                (disco-root--search-effective-spec-p disco-root--search-query-spec)))
      "[search inactive]"
    (string-join
     (append (disco-root--search-summary-chips)
             (mapcar #'disco-root--search-tab-summary-chip disco-root--search-tab-order))
     "  ")))

(defun disco-root--filters-line ()
  "Return filter-chip line inspired by telega root view."
  (if (eq (disco-root--ensure-layout) 'search)
      (disco-root--search-summary-line)
    (let* ((layout-label
            (downcase (disco-root-layout-label (disco-root--ensure-layout))))
           (metrics (disco-root--activity-metrics-by-view)))
      (string-join
       (list (disco-root--filter-chip 'all "Main" metrics)
             (disco-root--filter-chip 'unread "Important" metrics)
             (disco-root--filter-chip 'dms "DMs" metrics)
             (format "[%s sort:%s]" layout-label disco-root--sort-mode))
       "  "))))

(defun disco-root--mode-divider-line ()
  "Return divider line with active mode marker like telega root."
  (let* ((label (format "(%s/%s)"
                        (downcase (symbol-name (disco-root--ensure-layout)))
                        (symbol-name disco-root--view-mode)))
         (base-width (or disco-root--fill-column
                         (disco-root--compute-fill-column)))
         (width (max base-width (+ 8 (string-width label))))
         (filler (max 0 (- width (string-width label) 2)))
         (left (/ filler 2))
         (right (- filler left)))
    (concat "_/"
            (make-string left ?-)
            label
            (make-string right ?-))))

(defun disco-root--header-lines ()
  "Return fixed three-line root header block."
  (list
   (string-join
    (delq nil
          (list (format "Status: %s" (disco-root--gateway-status-label))
                (let ((sessions (disco-root--sessions-summary-label)))
                  (and sessions (format "  %s" sessions)))
                (let ((voice (disco-root--voice-summary-label)))
                  (and voice (format "  %s" voice)))
                (let ((badge (disco-root--feature-badge-summary)))
                  (and badge (format "  %s" badge)))
                (format "  keys[g index, C-u g full, G gw-sync, l/L layout, U unread-lens, ? menu]")))
    "")
   (disco-root--filters-line)
   (disco-root--mode-divider-line)))

(defun disco-root--refresh-header-line ()
  "Refresh root header block in place."
  (disco-root--debug-log "refresh-header-line")
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (lines (disco-root--header-lines)))
    (disco-root--ensure-markers)
    (save-excursion
      (let ((start (disco-root--header-start))
            (end-marker disco-root--ewoc-marker))
        (goto-char start)
        (dolist (line lines)
          (insert line "\n"))
        (insert "\n")
        (when (and (markerp end-marker)
                   (> (marker-position end-marker) (point)))
          (delete-region (point) end-marker))
        (set-marker disco-root--header-marker start)
        (set-marker disco-root--ewoc-marker (point))
        (set-marker-insertion-type disco-root--header-marker nil)
        (set-marker-insertion-type disco-root--ewoc-marker nil)
        (when (and disco-root--ewoc
                   (fboundp 'ewoc--header)
                   (fboundp 'ewoc--node-start-marker))
          (set-marker (ewoc--node-start-marker (ewoc--header disco-root--ewoc))
                      (marker-position disco-root--ewoc-marker)))))
    (setq disco-root--last-header-refresh-at (float-time))))

(defun disco-root--maybe-refresh-activity-header-line ()
  "Refresh activity header line when throttle interval has elapsed."
  (let ((interval (max 0 (or disco-root-activity-header-refresh-interval 0)))
        (now (float-time))
        (last (or disco-root--last-header-refresh-at 0.0)))
    (when (or (zerop interval)
              (>= (- now last) interval))
      (disco-root--refresh-header-line)
      t)))

(defun disco-root-render ()
  "Render root dashboard from in-memory state."
  (if disco-root--rendering
      (setq disco-root--render-pending t)
    (let ((disco-root--rendering t))
      (unwind-protect
          (let* ((inhibit-read-only t)
                 (buffer-undo-list t)
                 (layout (disco-root--ensure-layout)))
            (setq-local disco-root--fill-column (disco-root--render-fill-column))
            (disco-root--debug-log
             "render layout=%s view=%s fill=%s"
             layout
             disco-root--view-mode
             disco-root--fill-column)
            (erase-buffer)
            (disco-root--ensure-markers)
            (set-marker disco-root--header-marker (point-min))
            (set-marker-insertion-type disco-root--header-marker nil)
            (goto-char (point-min))
            (dolist (line (disco-root--header-lines))
              (insert line "\n"))
            (setq-local disco-root--last-header-refresh-at (float-time))
            (insert "\n")
            (set-marker disco-root--ewoc-marker (point))
            (set-marker-insertion-type disco-root--ewoc-marker nil)
            (disco-root--clear-ewoc-state)
            (disco-root-layout-render layout)
            (goto-char (point-min)))
        (when disco-root--render-pending
          (setq disco-root--render-pending nil)
          (disco-root--queue-live-update nil t nil))))))

(defun disco-root--gateway-sync-guild-ids ()
  "Return guild IDs prioritized for gateway context requests."
  (let* ((max-guilds (max 0 (or disco-root-gateway-context-max-guilds 0)))
         (guilds (copy-sequence (or (disco-state-guilds) '()))))
    (if (or (<= max-guilds 0)
            (null guilds))
        '()
      (let ((sorted
             (sort guilds
                   (lambda (left right)
                     (let* ((left-id (alist-get 'id left))
                            (right-id (alist-get 'id right))
                            (left-unread (disco-root--guild-unread-total left-id))
                            (right-unread (disco-root--guild-unread-total right-id)))
                       (if (= left-unread right-unread)
                           (string-lessp (or left-id "") (or right-id ""))
                         (> left-unread right-unread)))))))
        (disco-util-normalize-id-list
         (mapcar (lambda (guild)
                   (alist-get 'id guild))
                 (seq-take sorted max-guilds)))))))

(defun disco-root--gateway-last-message-eligible-p (channel)
  "Return non-nil when CHANNEL should be included in op34 last-message sync."
  (and (listp channel)
       (alist-get 'guild_id channel)
       (disco-permission-channel-viewable-p channel t)
       (memq (alist-get 'type channel) '(0 2 5 10 11 12 13 15 16))))

(defun disco-root--gateway-last-message-channel-ids (guild-id)
  "Return prioritized channel IDs for op34 request in GUILD-ID."
  (let* ((limit (max 0 (or disco-root-gateway-context-last-messages-per-guild 0)))
         (channels
          (append (or (disco-state-guild-channels guild-id) '())
                  (or (disco-state-guild-threads guild-id) '()))))
    (if (<= limit 0)
        '()
      (let ((eligible
             (seq-filter #'disco-root--gateway-last-message-eligible-p channels)))
        (setq eligible
              (sort eligible
                    (lambda (left right)
                      (> (disco-root--channel-activity-score left)
                         (disco-root--channel-activity-score right)))))
        (disco-util-normalize-id-list
         (mapcar (lambda (channel)
                   (alist-get 'id channel))
                 (seq-take eligible limit)))))))

(defun disco-root-sync-gateway-context (&optional quiet)
  "Request additional gateway context for current root state.

When QUIET is non-nil, suppress minibuffer status messages."
  (interactive)
  (let ((guild-ids (disco-root--gateway-sync-guild-ids))
        (requests 0)
        (requested-guilds 0))
    (when (disco-gateway-running-p)
      (dolist (guild-id guild-ids)
        (let ((guild-requested nil)
              (channel-ids (disco-root--gateway-last-message-channel-ids guild-id)))
          (when (disco-gateway-request-channel-statuses guild-id)
            (setq requests (1+ requests))
            (setq guild-requested t))
          (when (and disco-root-gateway-context-request-channel-info
                     (disco-gateway-request-channel-info
                      guild-id
                      '("status" "voice_start_time")))
            (setq requests (1+ requests))
            (setq guild-requested t))
          (when (and channel-ids
                     (disco-gateway-request-last-messages guild-id channel-ids))
            (setq requests (1+ requests))
            (setq guild-requested t))
          (when guild-requested
            (setq requested-guilds (1+ requested-guilds))))))
    (unless quiet
      (message "disco: requested %d gateway context ops across %d guilds"
               requests
               requested-guilds))
    requests))

(defun disco-root-refresh (&optional full)
  "Refresh the root directory index without hydrating every guild.

With prefix argument FULL, explicitly refresh every guild channel snapshot."
  (interactive "P")
  (if (and (eq (disco-root--ensure-layout) 'search)
           disco-root--search-domain
           (disco-root--search-effective-spec-p disco-root--search-query-spec))
      (disco-root-search-refresh)
    (if full
        (progn
          (message "disco: refreshing directory and all guilds...")
          (disco-directory-refresh-all-async))
      (message "disco: refreshing guild and DM index...")
      (disco-directory-refresh-index-async))))

(defvar disco-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-refresh)
    (define-key map (kbd "G") #'disco-root-sync-gateway-context)
    (define-key map (kbd "A") #'disco-root-list-archived-threads)
    (define-key map (kbd "l") #'disco-root-cycle-layout)
    (define-key map (kbd "L") #'disco-root-set-layout)
    (define-key map (kbd "\\") #'disco-root-toggle-sort-mode)
    (define-key map (kbd "v") #'disco-root-cycle-view-mode)
    (define-key map (kbd "U") #'disco-root-toggle-unread-lens)
    (define-key map (kbd "s") #'disco-root-search-transient)
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
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local disco-root--debug-log-enabled disco-root-debug-log-enabled)
  (setq-local disco-root--debug-log-changes disco-root-debug-log-changes)
  (setq-local disco-root--debug-log-verbose disco-root-debug-log-verbose)
  (add-hook 'before-change-functions #'disco-root--debug-before-change nil t)
  (add-hook 'after-change-functions #'disco-root--debug-after-change nil t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (disco-root--cancel-live-update-timer)
  (disco-root--cancel-missing-preview-fetch-timer)
  (disco-root--ensure-window-size-hook)
  (add-hook 'text-scale-mode-hook #'disco-root-buffer-auto-fill nil t)
  (setq-local disco-root--sort-mode 'activity)
  (setq-local disco-root--view-mode 'all)
  (setq-local disco-root--pre-unread-view-mode 'all)
  (setq-local disco-root--layout disco-root-default-layout)
  (setq-local disco-root--tree-show-unread-section
              disco-root-tree-default-show-unread-section)
  (disco-root--ensure-layout)
  (setq-local disco-root--ewoc nil)
  (setq-local disco-root--channel-node-table (make-hash-table :test #'equal))
  (setq-local disco-root--section-node-table (make-hash-table :test #'eq))
  (setq-local disco-root--guild-node-table (make-hash-table :test #'equal))
  (setq-local disco-root--category-node-table (make-hash-table :test #'equal))
  (setq-local disco-root--live-update-timer nil)
  (setq-local disco-root--directory-handler nil)
  (setq-local disco-root--rendering nil)
  (setq-local disco-root--render-pending nil)
  (setq-local disco-root--missing-preview-fetch-timer nil)
  (setq-local disco-root--missing-preview-pending-by-guild
              (make-hash-table :test #'equal))
  (setq-local disco-root--missing-preview-requested-last-message-id-by-channel
              (make-hash-table :test #'equal))
  (setq-local disco-root--dirty-channel-ids nil)
  (setq-local disco-root--dirty-structure-p nil)
  (setq-local disco-root--dirty-header-p nil)
  (setq-local disco-root--last-header-refresh-at nil)
  (setq-local disco-root--fill-column nil)
  (setq-local disco-root--header-marker nil)
  (setq-local disco-root--ewoc-marker nil)
  (setq-local disco-root--search-query nil)
  (setq-local disco-root--search-domain nil)
  (setq-local disco-root--search-query-spec nil)
  (setq-local disco-root--search-tabs nil)
  (setq-local disco-root--search-generation 0)
  (setq-local disco-root--search-in-flight nil)
  (setq-local disco-root--search-channel-table (make-hash-table :test #'equal))
  (setq-local disco-root--search-thread-table (make-hash-table :test #'equal))
  (setq-local disco-root--search-prev-layout nil)
  (setq-local disco-root--guild-expanded (make-hash-table :test #'equal))
  (setq-local disco-root--category-expanded (make-hash-table :test #'equal))
  (setq-local disco-root--guild-load-status (make-hash-table :test #'equal))
  (dolist (guild (disco-state-guilds))
    (when-let* ((guild-id (alist-get 'id guild))
                (status (disco-directory-guild-status guild-id)))
      (unless (memq status '(loaded unloaded))
        (puthash guild-id status disco-root--guild-load-status)))))

(defun disco-root-open ()
  "Open root buffer and render current state."
  (interactive)
  (let ((buf (get-buffer-create disco-root-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'disco-root-mode)
        (disco-root-mode))
      (setq-local buffer-undo-list t)
      (disco-root--attach-live-updates)
      (disco-root-render))
    (pop-to-buffer buf)))

(setq disco-root-view-attach-live-updates-function
      #'disco-root--attach-live-updates
      disco-root-view-load-more-function
      #'disco-root-search-load-more-at-point
      disco-root-view-missing-preview-fetch-function
      #'disco-root--maybe-queue-missing-preview-fetch
      disco-root-view-queue-live-update-function
      #'disco-root--queue-live-update
      disco-root-view-render-preserving-position-function
      #'disco-root--render-preserving-position
      disco-root-view-transient-function
      #'disco-root-transient)

(provide 'disco-root)

;;; disco-root.el ends here
