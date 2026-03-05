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
(require 'disco-msg)
(require 'disco-room)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-permission)
(require 'disco-root-layout)
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

(defcustom disco-root-gateway-context-sync-on-refresh t
  "When non-nil, request extra root context via Gateway after root refresh."
  :type 'boolean
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
  "When non-nil, proactively request missing last-message previews in activity rows."
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
          guild-feature-ack user-non-channel-ack notification-center-items-ack
          thread-create thread-update thread-delete thread-list-sync)))

(defun disco-root--live-event-structural-p (event-type)
  "Return non-nil when EVENT-TYPE requires a full tree reconcile."
  (memq event-type
        '(channel-create channel-update channel-delete
          guild-create guild-update guild-delete guild-sync
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

(defun disco-root--ensure-layout ()
  "Ensure active root layout is available and return it."
  (unless (memq disco-root--layout (disco-root-layout-names))
    (setq disco-root--layout disco-root-default-layout)
    (unless (memq disco-root--layout (disco-root-layout-names))
      (setq disco-root--layout 'tree)))
  disco-root--layout)

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
   :preserve-window-start t))

(defun disco-root--buffer-visible-p (&optional buffer)
  "Return non-nil when BUFFER is currently displayed in any live window."
  (or noninteractive
      (window-live-p (get-buffer-window (or buffer (current-buffer)) t))))

(defun disco-root--buffer-corrupted-p ()
  "Return non-nil when root buffer appears to have duplicated header artifacts."
  (and (eq major-mode 'disco-root-mode)
       disco-root--ewoc
       (save-excursion
         (goto-char (point-min))
         (let ((line1 (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))
               (line3 (progn
                        (forward-line 2)
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
               (scan-start
                (or (when-let* ((node (ewoc-nth disco-root--ewoc 0)))
                      (ewoc-location node))
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line 4)
                      (point)))))
           (or (not (string-prefix-p "Status: " line1))
               (not (string-prefix-p "_/" line3))
               (save-excursion
                 (goto-char scan-start)
                 (re-search-forward "^Status: " nil t)))))))

(defun disco-root--refresh-mode-divider-line ()
  "Refresh only the mode-divider header line used for width framing."
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (save-excursion
      (goto-char (point-min))
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
   :preserve-window-start t))

(defun disco-root--display-window (&optional buffer)
  "Return preferred live window displaying BUFFER."
  (let* ((buf (or buffer (current-buffer)))
         (selected (selected-window)))
    (cond
     ((and (window-live-p selected)
           (eq (window-buffer selected) buf))
      selected)
     (t
      (get-buffer-window buf t)))))

(defun disco-root--window-width-remap (window)
  "Return WINDOW width in columns, respecting face remapping when possible."
  (if (not (window-live-p window))
      (window-width)
    (condition-case _err
        (window-width window 'remap)
      (wrong-number-of-arguments
       (window-width window))
      (error
       (window-width window)))))

(defun disco-root--chars-xwidth (columns &optional buffer window)
  "Return pixel width for COLUMNS in BUFFER/WINDOW metrics."
  (let* ((win (or window (disco-root--display-window buffer)))
         (char-width (or (and (window-live-p win)
                              (fboundp 'window-font-width)
                              (window-font-width win))
                         (frame-char-width)
                         1)))
    (* (max 0 columns) (max 1 char-width))))

(defun disco-root--chars-in-width (pixels &optional buffer window)
  "Return character columns required to cover PIXELS in BUFFER/WINDOW."
  (max 0
       (ceiling (/ (max 0 pixels)
                   (float (max 1 (disco-root--chars-xwidth 1 buffer window)))))))

(defun disco-root--text-scale-factor (&optional buffer)
  "Return text scale factor for BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((step (if (boundp 'text-scale-mode-step)
                    text-scale-mode-step
                  1.2))
          (amount (if (boundp 'text-scale-mode-amount)
                      text-scale-mode-amount
                    0)))
      (if (= amount 0)
          1.0
        (expt step amount)))))

(defun disco-root--scaled-image (image &optional buffer)
  "Return IMAGE spec scaled for BUFFER text scale when possible."
  (let ((factor (disco-root--text-scale-factor buffer)))
    (if (and (consp image)
             (eq (car image) 'image)
             (numberp factor)
             (> factor 0)
             (/= factor 1.0))
        (let ((scaled (copy-tree image)))
          (setcdr scaled (plist-put (cdr scaled) :scale factor))
          scaled)
      image)))

(defun disco-root--compute-fill-column (&optional buffer window)
  "Return effective render width for BUFFER.

When WINDOW is non-nil, compute using WINDOW directly."
  (let* ((buf (or buffer (current-buffer)))
         (win (or window (disco-root--display-window buf))))
    (max 20
         (if (not (window-live-p win))
             (window-width)
           (let* ((margins (window-margins win))
                  (raw-width (+ (disco-root--window-width-remap win)
                                (or (car margins) 0)
                                (or (cdr margins) 0)))
                  (line-number-columns
                   (with-selected-window win
                     (disco-root--chars-in-width
                      (line-number-display-width 'pixels)
                      buf
                      win)))
                  (adjusted-width (- raw-width
                                     (or disco-root-auto-fill-margin-columns 0)
                                     line-number-columns)))
             adjusted-width)))))

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
  "Ensure `disco-root--section-expanded' has defaults for SECTIONS.

When SECTIONS is nil, use `disco-root--section-order'."
  (dolist (section (or sections disco-root--section-order))
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
  "Store EXPANDED for KEY in TABLE and return resulting state."
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
  "Return mention badge count for row at POS, defaulting to 0."
  (or (disco-root--line-property 'disco-unread-count pos) 0))

(defun disco-root--line-has-unread-p (&optional pos)
  "Return non-nil when row at POS has unread state."
  (or (disco-root--line-property 'disco-has-unread pos)
      (> (disco-root--line-unread-count pos) 0)))

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
  (let ((nodes (and channel-id
                    disco-root--channel-node-table
                    (gethash channel-id disco-root--channel-node-table))))
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
                    (setq entry (plist-put entry :channel channel))
                    (ewoc-set-data node entry)
                    (ewoc-invalidate disco-root--ewoc node)
                    (setq updated t))))
              (if updated 'updated 'missing))
          'stale))))))

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
      (when (eq (plist-get entry :entry-type) 'channel)
        (plist-get entry :channel)))))

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
if structural fallback is required."
  (when (and disco-root--ewoc
             (eq (disco-root--ensure-layout) 'activity))
    (if channel-ids
        (let (missing-visible)
          (dolist (channel-id (seq-uniq (delq nil channel-ids) #'equal))
            (when (eq (disco-root--activity-reorder-channel-node channel-id)
                      'missing-visible)
              (setq missing-visible t)))
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
        (setq entry (plist-put entry :count count))
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

(defun disco-root--guild-by-id (guild-id)
  "Return guild object for GUILD-ID from current state."
  (seq-find (lambda (guild)
              (equal (alist-get 'id guild) guild-id))
            (or (disco-state-guilds) '())))

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
            (setq entry (plist-put entry :guild guild))
            (setq entry (plist-put
                         entry
                         :unread-count
                         (disco-root--guild-unread-total guild-id t)))
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
            (setq entry (plist-put entry :category category))
            (setq entry (plist-put
                         entry
                         :unread-count
                         (disco-root--category-children-unread-total children)))
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
      (when (eq major-mode 'disco-root-mode)
        (setq disco-root--missing-preview-fetch-timer nil)
        (when (and (disco-gateway-running-p)
                   (hash-table-p disco-root--missing-preview-pending-by-guild)
                   (> (hash-table-count disco-root--missing-preview-pending-by-guild) 0))
          (let ((max-per-guild
                 (max 1 (or disco-root-missing-preview-fetch-max-per-guild 1)))
                next-pending)
            (maphash
             (lambda (guild-id pending-channel-ids)
               (let* ((ordered (disco-root--normalize-id-list pending-channel-ids))
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

(defun disco-root--queue-live-update (channel-ids &optional structural-p header-p)
  "Queue CHANNEL-IDS for debounced UI update.

When STRUCTURAL-P is non-nil, the next flush performs full reconcile.
When HEADER-P is non-nil, root header line is refreshed on flush."
  (let ((ids (cond
              ((null channel-ids) nil)
              ((listp channel-ids) channel-ids)
              (t (list channel-ids)))))
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

(defun disco-root--flush-live-updates (root-buffer)
  "Flush queued live updates into ROOT-BUFFER."
  (when (buffer-live-p root-buffer)
    (with-current-buffer root-buffer
      (when (eq major-mode 'disco-root-mode)
        (setq disco-root--live-update-timer nil)
        (if disco-root--refresh-in-flight
            (setq disco-root--live-update-timer
                  (run-with-timer
                   (max 0.02 disco-root-live-update-debounce)
                   nil
                   #'disco-root--flush-live-updates
                   root-buffer))
          (let ((dirty-channel-ids (nreverse disco-root--dirty-channel-ids))
                (needs-structural disco-root--dirty-structure-p)
                (needs-header disco-root--dirty-header-p)
                (layout-update-mode
                 (disco-root-layout-update-mode (disco-root--ensure-layout))))
            (setq disco-root--dirty-channel-ids nil)
            (setq disco-root--dirty-structure-p nil)
            (setq disco-root--dirty-header-p nil)
            (unless (disco-root--buffer-visible-p root-buffer)
              ;; Hidden root buffers are more prone to stale incremental layout state.
              ;; Reconcile structurally to keep EWOC/header markers coherent.
              (setq needs-structural t))
            (cond
             (needs-structural
              (disco-root--render-preserving-position))
             ((and (eq layout-update-mode 'full)
                   (or dirty-channel-ids needs-header))
              (if (and needs-header (null dirty-channel-ids))
                  (disco-root--refresh-header-line)
                (disco-root--render-preserving-position)))
             ((and dirty-channel-ids
                   (eq disco-root--view-mode 'unread))
              (disco-root--render-preserving-position))
             (t
              (let ((inhibit-read-only t)
                    (buffer-undo-list t)
                    (layout (disco-root--ensure-layout)))
                (dolist (channel-id dirty-channel-ids)
                  (when (eq (disco-root--refresh-channel-node channel-id) 'stale)
                    (setq needs-structural t)))
                (when (and (not needs-structural)
                           dirty-channel-ids
                           (eq layout 'activity))
                  (when (disco-root--activity-reorder-visible-nodes dirty-channel-ids)
                    (setq needs-structural t)))
                (when dirty-channel-ids
                  (disco-root--refresh-active-layout-headings dirty-channel-ids))
                (cond
                 (needs-header
                  (disco-root--refresh-header-line))
                 ((and dirty-channel-ids
                       (eq layout 'activity))
                  (disco-root--maybe-refresh-activity-header-line)))
                (when (and (not needs-structural)
                           (disco-root--buffer-corrupted-p))
                  (setq needs-structural t)))
              (when needs-structural
                (disco-root--render-preserving-position))))))))))

(defun disco-root--handle-gateway-event (event)
  "Apply one gateway EVENT to root buffer view."
  (let ((event-type (plist-get event :type)))
    (when (disco-root--live-event-p event-type)
      (disco-root--queue-live-update
       (disco-root--event-channel-ids event)
       (disco-root--live-event-structural-p event-type)
       (disco-root--live-event-header-p event-type)))))

(defun disco-root--attach-live-updates ()
  "Attach root buffer to global gateway update stream."
  (when disco-root--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-root--gateway-handler)
    (disco-gateway-unwatch-global))
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
                (disco-root--handle-gateway-event event))))))
  (add-hook 'disco-gateway-event-hook disco-root--gateway-handler)
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

(defun disco-root--normalize-id-list (ids &optional max-items)
  "Normalize IDS as unique string list, optionally capped by MAX-ITEMS."
  (let (result)
    (dolist (it (or ids '()))
      (let ((normalized (and it (format "%s" it))))
        (when normalized
          (cl-pushnew normalized result :test #'equal))))
    (let ((ordered (nreverse result)))
      (if (and (integerp max-items)
               (> (length ordered) max-items))
          (seq-take ordered max-items)
        ordered))))

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
  (and (memq (alist-get 'type channel) '(0 1 2 3 5 10 11 12 13 15 16))
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
  (let* ((fallback (disco-root--guild-icon-fallback guild))
         (image (disco-root--guild-icon-image guild))
         (display-image (and (disco-root--guild-icon-image-valid-p image)
                             (disco-root--scaled-image image))))
    (if (disco-root--guild-icon-image-valid-p display-image)
        (insert-image display-image fallback)
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

(defun disco-root--normalize-extra-info-value (value)
  "Normalize one provider VALUE into a flat list of non-empty strings."
  (cond
   ((null value) nil)
   ((stringp value)
    (unless (string-empty-p value)
      (list value)))
   ((listp value)
    (cl-mapcan #'disco-root--normalize-extra-info-value value))
   (t
    (list (format "%s" value)))))

(defun disco-root--collect-extra-info (kind object context)
  "Collect extra display fragments for KIND OBJECT with CONTEXT."
  (let (parts)
    (dolist (provider disco-root-extra-info-functions)
      (condition-case err
          (setq parts
                (nconc parts
                       (disco-root--normalize-extra-info-value
                        (funcall provider kind object context))))
        (error
         (unless (gethash provider disco-root--extra-info-provider-error-cache)
           (puthash provider t disco-root--extra-info-provider-error-cache)
           (message "disco: root extra info provider error (%S): %s"
                    provider
                    (error-message-string err))))))
    parts))

(defun disco-root--append-extra-info (label kind object context)
  "Append provider-driven fragments to LABEL for KIND OBJECT CONTEXT."
  (let ((parts (disco-root--collect-extra-info kind object context)))
    (if parts
        (concat label " " (string-join parts " "))
      label)))

(defun disco-root--channel-read-p (channel)
  "Return non-nil when CHANNEL is considered fully read."
  (let* ((channel-id (alist-get 'id channel))
         (unread (disco-state-channel-effective-unread-count channel))
         (last-read-id (disco-state-channel-last-read-message-id channel-id))
         (last-message-id (alist-get 'last_message_id channel)))
    (and (= unread 0)
         (disco-state-snowflake>= last-read-id last-message-id))))

(defun disco-root--format-trail-tags (tags)
  "Return human-readable trail string for TAGS list."
  (when tags
    (mapconcat (lambda (tag)
                 (format "[%s]" tag))
               tags
               " ")))

(defun disco-root--channel-static-trail-tags (channel)
  "Return static status trail tags for CHANNEL."
  (let* ((channel-id (alist-get 'id channel))
         (channel-type (alist-get 'type channel))
         (voice-member-count (and channel-id
                                  (disco-state-channel-voice-member-count channel-id)))
         (member-count (and channel-id
                            (disco-state-channel-member-count channel-id)))
         (voice-start-time (alist-get 'voice_start_time channel))
         tags)
    (when (disco-state-channel-has-unread-pins-p channel)
      (push "pins" tags))
    (when (eq t (alist-get 'muted channel))
      (push "muted" tags))
    (when (and (memq channel-type '(2 13))
               (numberp voice-member-count)
               (> voice-member-count 0))
      (push (format "voice:%d" voice-member-count) tags))
    (when (and (memq channel-type '(2 13))
               (numberp member-count)
               (> member-count 0))
      (push (format "members:%d" member-count) tags))
    (when (and (memq channel-type '(2 13))
               voice-start-time)
      (push "live" tags))
    (nreverse tags)))

(defun disco-root--channel-dynamic-trail-tags (channel)
  "Return dynamic status trail tags for CHANNEL."
  (let ((mention-count (disco-state-channel-effective-unread-count channel))
        (has-unread (disco-root--channel-has-unread-p channel))
        tags)
    (when has-unread
      (push "unread" tags))
    (when (> mention-count 0)
      (push (format "@%d" mention-count) tags))
    (nreverse tags)))

(defun disco-root--channel-context-label (channel)
  "Return context label for CHANNEL in activity layout."
  (pcase (alist-get 'type channel)
    ((or 1 3)
     "direct-messages")
    (_
     (let* ((guild-id (alist-get 'guild_id channel))
            (guild-name (and guild-id (disco-root--guild-name-by-id guild-id)))
            (parent-id (alist-get 'parent_id channel))
            (parent-channel (and parent-id (disco-state-channel parent-id)))
            (parent-name (and parent-channel
                              (disco-root--channel-display-name parent-channel)))
            (category-id (cond
                          ((and parent-channel
                                (disco-state-channel-thread-p channel))
                           (alist-get 'parent_id parent-channel))
                          (parent-id
                           parent-id)
                          (t nil)))
            (category-channel (and category-id (disco-state-channel category-id)))
            (category-name (and category-channel
                                (disco-root--channel-category-p category-channel)
                                (disco-root--channel-display-name category-channel)))
            parts)
       (when (and (stringp guild-name)
                  (not (string-empty-p guild-name)))
         (push guild-name parts))
       (when (and (stringp category-name)
                  (not (string-empty-p category-name)))
         (push category-name parts))
       (when (and (disco-state-channel-thread-p channel)
                  (stringp parent-name)
                  (not (string-empty-p parent-name)))
         (push (format "#%s" parent-name) parts))
       (when parts
         (string-join (nreverse parts) " / "))))))

(defun disco-root--channel-category-name (channel)
  "Return category name for CHANNEL, or nil when not categorized."
  (let* ((parent-id (alist-get 'parent_id channel))
         (parent-channel (and parent-id (disco-state-channel parent-id)))
         (category-id (cond
                       ((and parent-channel
                             (disco-state-channel-thread-p channel))
                        (alist-get 'parent_id parent-channel))
                       ((and parent-channel
                             (disco-root--channel-category-p parent-channel))
                        parent-id)
                       (t nil)))
         (category (and category-id (disco-state-channel category-id))))
    (when (and category (disco-root--channel-category-p category))
      (disco-root--channel-display-name category))))

(defun disco-root--channel-guild-name (channel)
  "Return guild name for CHANNEL, or nil when channel is non-guild."
  (when-let* ((guild-id (alist-get 'guild_id channel)))
    (disco-root--guild-name-by-id guild-id)))

(defun disco-root--activity-primary-label (channel)
  "Return activity-layout primary label for CHANNEL."
  (let* ((parts (delq nil
                      (list (disco-root--channel-display-name channel)
                            (disco-root--channel-category-name channel)
                            (disco-root--channel-guild-name channel)))))
    (if parts
        (string-join parts " | ")
      (disco-root--channel-display-name channel))))

(defun disco-root--activity-secondary-label (channel)
  "Return fallback activity preview label for CHANNEL.

Fallback stays message-oriented and avoids status-tag placeholders."
  (let ((channel-id (alist-get 'id channel)))
    (or (and channel-id
             (disco-state-channel-conversation-summary-preview channel-id))
        (and (alist-get 'last_message_id channel)
             (progn
               (disco-root--queue-missing-preview-fetch channel)
               ;; Keep this neutral: missing preview here means op34/cache did not
               ;; materialize a message, not necessarily that summaries are
               ;; unavailable for the guild/channel.
               "(preview unavailable)"))
        "(no messages)")))

(defun disco-root--canonicalize-number (spec base)
  "Resolve SPEC against BASE columns.

SPEC can be an integer, float ratio, or list (VALUE MIN MAX)."
  (let* ((raw (if (consp spec) (car spec) spec))
         (min-value (when (consp spec) (nth 1 spec)))
         (max-value (when (consp spec) (nth 2 spec)))
         (value (cond
                 ((integerp raw) raw)
                 ((floatp raw) (round (* raw base)))
                 ((numberp raw) (round raw))
                 (t base))))
    (when (numberp min-value)
      (setq value (max value min-value)))
    (when (numberp max-value)
      (setq value (min value max-value)))
    value))

(defun disco-root--human-count (value)
  "Return VALUE formatted in compact human-readable form."
  (let ((n (max 0 (or value 0))))
    (cond
     ((>= n 1000000)
      (replace-regexp-in-string "\\.0m\\'" "m"
                                (format "%.1fm" (/ n 1000000.0))))
     ((>= n 1000)
      (replace-regexp-in-string "\\.0k\\'" "k"
                                (format "%.1fk" (/ n 1000.0))))
     (t
      (number-to-string n)))))

(defun disco-root--truncate-fill (text width &optional right-align)
  "Return TEXT truncated and padded to WIDTH.

When RIGHT-ALIGN is non-nil, pad on the left instead of right."
  (let* ((target (max 0 (or width 0)))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (padding (max 0 (- target (string-width trimmed)))))
    (if right-align
        (concat (make-string padding ?\s) trimmed)
      (concat trimmed (make-string padding ?\s)))))

(defun disco-root--string-width (str &optional from to)
  "Return width of STR, optionally limited to FROM..TO."
  (string-width
   (if (or from to)
       (substring str (or from 0) to)
     str)))

(defun disco-root--elide-string (str max &optional face)
  "Return STR visually elided to MAX columns using display properties.

This mirrors telega's `telega-fmt-eval-eliding' behavior for right-elision."
  (let* ((text (or str ""))
         (str-width (disco-root--string-width text))
         (limit (max 0 (or max 0))))
    (if (<= str-width limit)
        text
      (let* ((elide-str "…")
             (elide-width (disco-root--string-width elide-str))
             (elide-pos 1)
             (str-len (length text))
             (elide-trail (floor (* limit (- 1 elide-pos))))
             (trail-width
              (progn
                (while (and (> elide-trail 0)
                            (> (disco-root--string-width text (- str-len elide-trail))
                               (floor (* limit (- 1 elide-pos)))))
                  (setq elide-trail (1- elide-trail)))
                (disco-root--string-width text (- str-len elide-trail))))
             (elide-lead (- (min limit str-len) elide-width trail-width))
             (result (copy-sequence text)))
        (when (< elide-lead 0)
          (setq elide-lead 0))
        (while (and (> elide-lead 0)
                    (> (+ (disco-root--string-width result 0 elide-lead)
                          elide-width trail-width)
                       limit))
          (setq elide-lead (1- elide-lead)))
        (add-text-properties
         elide-lead
         (- str-len elide-trail)
         (list 'display elide-str
               'rear-nonsticky '(display)
               'face face)
         result)
        result))))

(defun disco-root--current-column ()
  "Like `current-column', but account for prior `:align-to' spacers."
  (let* ((bol (line-beginning-position))
         (point-now (point))
         (scan point-now)
         align-column)
    (while (and (not align-column)
                (> scan bol)
                (setq scan (previous-single-char-property-change
                            scan 'display nil bol)))
      (let ((display (get-text-property scan 'display)))
        (when (and (listp display)
                   (> (length display) 2)
                   (eq (nth 0 display) 'space)
                   (eq (nth 1 display) :align-to))
          (let ((align-val (nth 2 display)))
            (setq align-column
                  (+ (if (listp align-val)
                         (disco-root--chars-in-width (or (car align-val) 0))
                       (or align-val 0))
                     (disco-root--string-width
                      (buffer-substring scan point-now))))))))
    (or align-column (current-column))))

(defun disco-root--move-to-column (column)
  "Insert one align-to spacer for COLUMN, matching telega inserters."
  (let* ((target (max 0 (or column 0)))
         (align-to (if (display-graphic-p)
                       (list (disco-root--chars-xwidth target))
                     target)))
    (insert (propertize " " 'display `(space :align-to ,align-to)))))

(defun disco-root--snowflake-epoch-seconds (snowflake)
  "Return unix epoch seconds extracted from Discord SNOWFLAKE, or nil."
  (when (and (stringp snowflake)
             (string-match-p "\\`[0-9]+\\'" snowflake))
    (let ((value (string-to-number snowflake)))
      (when (integerp value)
        (+ disco-state-discord-epoch-seconds
           (/ (float (ash value -22)) 1000.0))))))

(defun disco-root--activity-time-format-type (seconds)
  "Return time format type symbol for SECONDS.

Values match keys in `disco-root-activity-time-format-alist'."
  (let* ((now (float-time))
         (day-seconds (* 24 60 60))
         (ctime (decode-time now))
         (today00 (float-time (encode-time 0 0 0
                                           (nth 3 ctime)
                                           (nth 4 ctime)
                                           (nth 5 ctime)))))
    (if (and (> seconds today00)
             (< seconds (+ today00 day-seconds)))
        'today
      (let* ((week-day (nth 6 ctime))
             (mdays (+ week-day
                       (- (if (< week-day disco-root-week-start-day) 7 0)
                          disco-root-week-start-day)))
             (week-start00 (- today00 (* mdays day-seconds))))
        (if (and (> seconds week-start00)
                 (< seconds (+ week-start00 (* 7 day-seconds))))
            'this-week
          'old)))))

(defun disco-root--activity-time-string (seconds &optional fmt-type)
  "Return formatted activity time string for SECONDS.

FMT-TYPE can be a symbol key from
`disco-root-activity-time-format-alist' or a format string accepted by
`format-time-string'."
  (let* ((kind (or fmt-type (disco-root--activity-time-format-type seconds)))
         (fmt (or (and (stringp kind) kind)
                  (cdr (assq kind disco-root-activity-time-format-alist))
                  (cdr (assq 'time disco-root-activity-time-format-alist))
                  "%H:%M")))
    (format-time-string fmt (seconds-to-time seconds))))

(defun disco-root--message-author-id (message)
  "Return author ID from MESSAGE, or nil."
  (let ((author (and (listp message) (alist-get 'author message))))
    (and (listp author)
         (alist-get 'id author))))

(defun disco-root--channel-last-activity-seconds (channel &optional message)
  "Return latest activity timestamp seconds for CHANNEL, or nil.

When MESSAGE is non-nil, use it as the cached latest message."
  (or (when-let* ((msg (or message (disco-msg-channel-last-cached-message channel)))
                  (timestamp (alist-get 'timestamp msg)))
        (ignore-errors
          (float-time (date-to-time timestamp))))
      (disco-root--snowflake-epoch-seconds
       (alist-get 'last_message_id channel))))

(defun disco-root--activity-time-status-symbol (channel &optional message)
  "Return telega-like status symbol for CHANNEL timestamp column."
  (let* ((latest-message (or message (disco-msg-channel-last-cached-message channel)))
         (current-user-id (and (fboundp 'disco-gateway-current-user-id)
                               (disco-gateway-current-user-id)))
         (own-latest-message
          (and latest-message
               current-user-id
               (equal (format "%s" (disco-root--message-author-id latest-message))
                      (format "%s" current-user-id))))
         (has-unread (disco-root--channel-has-unread-p channel)))
    (cond
     (own-latest-message
      (if (disco-root--channel-read-p channel)
          "✔"
        "✓"))
     (has-unread
      "•")
     (t
      " "))))

(defun disco-root--activity-time-status-face (channel &optional message)
  "Return face used for timestamp status indicator in CHANNEL row."
  (let ((status (disco-root--activity-time-status-symbol channel message)))
    (cond
     ((equal status "•") 'font-lock-warning-face)
     ((or (equal status "✓")
          (equal status "✔"))
      'success)
     (t 'shadow))))

(defun disco-root--channel-last-activity-time-label (channel &optional message)
  "Return timestamp column text for CHANNEL.

Output includes formatted date/time and a trailing status symbol."
  (let* ((seconds (disco-root--channel-last-activity-seconds channel message))
         (time-part (and seconds
                         (ignore-errors
                           (disco-root--activity-time-string seconds))))
         (status (disco-root--activity-time-status-symbol channel message)))
    (if (and time-part (not (string-empty-p time-part)))
        (concat time-part status)
      status)))

(defun disco-root--activity-preview-line (channel &optional message)
  "Return one-line preview text for activity row CHANNEL.

When MESSAGE is non-nil, use it as cached preview source."
  (or (and message (disco-msg-preview-line message))
      (disco-msg-channel-preview-line channel)
      (disco-root--activity-secondary-label channel)))

(defun disco-root--insert-activity-icon (channel)
  "Insert activity icon for CHANNEL.

Guild rows use real guild icons when available, with fixed text fallback."
  (let ((guild (and (alist-get 'guild_id channel)
                    (disco-root--guild-by-id (alist-get 'guild_id channel))))
        (channel-type (alist-get 'type channel))
        (start (point)))
    (cond
     (guild
      (if disco-root-show-guild-icons
          (disco-root--insert-guild-icon guild)
        (insert (disco-root--guild-icon-fallback guild))))
     ((eq channel-type 1)
      (insert "[D]"))
     ((eq channel-type 3)
      (insert "[G]"))
     ((disco-state-channel-thread-p channel)
      (insert "[T]"))
     (t
      (insert "[#]")))
    (add-text-properties start (point) (list 'face 'shadow))))

(defun disco-root--activity-column-widths (content-width)
  "Return telega-like context/preview width split for CONTENT-WIDTH."
  (let* ((max-context-inner (max 8 (- content-width 3)))
         (context-inner-width
          (max 8
               (min max-context-inner
                    (disco-root--canonicalize-number
                     disco-root-activity-context-width
                     content-width))))
         (preview-width (max 0 (- content-width context-inner-width 3))))
    (list :context-inner-width context-inner-width
          :preview-width preview-width
          :separator-width (if (> preview-width 0) 1 0))))

(defun disco-root--insert-activity-channel-line (channel indent)
  "Insert one activity-style CHANNEL row with INDENT."
  (let* ((channel-id (alist-get 'id channel))
         (channel-type (alist-get 'type channel))
         (latest-message (disco-msg-channel-last-cached-message channel))
         (mention-count (disco-state-channel-effective-unread-count channel))
         (has-unread (disco-root--channel-has-unread-p channel))
         (padding (make-string indent ?\s))
         (context-text (disco-root--activity-primary-label channel))
         (preview-text (disco-root--activity-preview-line channel latest-message))
         (time-text (disco-root--channel-last-activity-time-label channel latest-message))
         (line-width (max 60 (or disco-root--fill-column
                                 (disco-root--compute-fill-column))))
         (time-width (max 6 (string-width time-text)))
         (line-start (point)))
    (insert padding)
    (let* ((icon-start (disco-root--current-column))
           (icon-slot-width
            (max 2
                 (ceiling (* disco-root--activity-icon-slot-width
                             (disco-root--text-scale-factor)))))
           icon-width)
      (disco-root--insert-activity-icon channel)
      (setq icon-width (- (disco-root--current-column) icon-start))
      (when (< icon-width icon-slot-width)
        (insert (make-string (- icon-slot-width icon-width) ?\s)))
      (insert " "))
    (let* ((content-start (disco-root--current-column))
           (content-width (max 20 (- line-width content-start time-width 1)))
           (widths (disco-root--activity-column-widths content-width))
           (context-inner-width (or (plist-get widths :context-inner-width) 8))
           (preview-width (or (plist-get widths :preview-width) 0))
           (separator-width (or (plist-get widths :separator-width) 0)))
      (let ((context-start (disco-root--current-column)))
        (insert "[")
        (insert (disco-root--elide-string context-text context-inner-width 'default))
        (disco-root--move-to-column (+ context-start 1 context-inner-width))
        (insert "]"))
      (when (> preview-width 0)
        (when (> separator-width 0)
          (insert " "))
        (let* ((preview-start (point))
               (preview-value (or preview-text "")))
          (insert (disco-root--elide-string preview-value preview-width 'shadow))
          (add-text-properties preview-start (point) (list 'face 'shadow))
          (let ((author (disco-msg-author-display-name latest-message)))
            (when (and author
                       (string-match (format "\\`%s>" (regexp-quote author))
                                     preview-value))
              (add-text-properties
               preview-start
               (+ preview-start (length author) 1)
               (list 'face 'font-lock-keyword-face))))))
      (let ((target-time-col (- line-width time-width)))
        (disco-root--move-to-column target-time-col)
        (let ((time-start (point))
              (status-face (disco-root--activity-time-status-face
                            channel latest-message)))
          (insert (disco-root--truncate-fill time-text time-width t))
          (let ((status-pos (max time-start (1- (point)))))
            (add-text-properties time-start status-pos (list 'face 'shadow))
            (add-text-properties status-pos (point)
                                 (list 'face status-face))))))
    (insert "\n")
    (add-text-properties
     line-start
     (point)
     (list 'disco-root-row-type 'channel
           'disco-channel-id channel-id
           'disco-unread-count mention-count
           'disco-has-unread (and has-unread t)))
    (when (disco-root--openable-channel-p channel)
      (add-text-properties
       line-start
       (point)
       (list 'mouse-face 'highlight
             'help-echo (if (memq channel-type '(15 16))
                            (format "Open threads under channel %s" channel-id)
                          (format "Open channel %s" channel-id)))))))

(defun disco-root--channel-label (channel &optional scope)
  "Return display label for CHANNEL.

SCOPE is a symbol describing where the row is rendered."
  (let ((name (disco-root--channel-display-name channel))
        (channel-type (alist-get 'type channel))
        (channel-id (alist-get 'id channel))
        (mention-count (disco-state-channel-effective-unread-count channel))
        (has-unread (disco-root--channel-has-unread-p channel))
        base-label)
    (let ((state-suffix (cond
                         ((> mention-count 0)
                          (format " [@%d]" mention-count))
                         (has-unread
                          " [unread]")
                         (t
                          "")))
          (trail-suffix
           (if (eq scope 'activity)
               ""
             (let ((trail (disco-root--format-trail-tags
                           (disco-root--channel-static-trail-tags channel))))
               (if trail
                   (concat " " trail)
                 "")))))
      (setq base-label
            (pcase channel-type
              (1 (format "[dm] %s%s%s" name state-suffix trail-suffix))
              (3 (format "[group] %s%s%s" name state-suffix trail-suffix))
              ((or 10 11 12)
               (let ((tags (disco-root--thread-status-tags channel)))
                 (format "[thread] %s%s%s%s"
                         name
                         (if (string-empty-p tags)
                             ""
                           (format " (%s)" tags))
                         state-suffix
                         trail-suffix)))
              ((or 0 5 15 16)
               (let* ((thread-count (disco-root--thread-count-under-parent channel))
                      (suffix (if (> thread-count 0)
                                  (format " (%d threads)" thread-count)
                                "")))
                 (pcase channel-type
                   ((or 0 5) (format "#%s%s%s%s" name suffix state-suffix trail-suffix))
                   (15 (format "[forum] %s%s%s%s" name suffix state-suffix trail-suffix))
                   (16 (format "[media] %s%s%s%s" name suffix state-suffix trail-suffix)))))
              (2 (format "[voice] %s%s%s" name state-suffix trail-suffix))
              (13 (format "[stage] %s%s%s" name state-suffix trail-suffix))
              (_ (format "[type-%s] %s%s%s" channel-type name state-suffix trail-suffix)))))
    (disco-root--append-extra-info
     base-label
     'channel
     channel
     (list :scope (or scope 'root)
           :channel-id channel-id
           :channel-type channel-type
           :unread mention-count
           :has-unread has-unread))))

(defun disco-root--guild-label (guild unread-count &optional scope)
  "Return display label for GUILD with UNREAD-COUNT badge."
  (let* ((guild-name (or (alist-get 'name guild) "(unnamed-guild)"))
         (guild-id (alist-get 'id guild))
         (base-label (format "%s%s"
                             guild-name
                             (if (> unread-count 0)
                                 (format " [%d]" unread-count)
                               ""))))
    (disco-root--append-extra-info
     base-label
     'guild
     guild
     (list :scope (or scope 'root)
           :guild-id guild-id
           :unread unread-count))))

(defun disco-root--category-label (category unread-count &optional scope)
  "Return display label for CATEGORY with UNREAD-COUNT badge."
  (let* ((category-name (or (disco-root--channel-display-name category)
                            "(unnamed-category)"))
         (category-id (alist-get 'id category))
         (base-label (format "[category] %s%s"
                             category-name
                             (if (> unread-count 0)
                                 (format " [%d]" unread-count)
                               ""))))
    (disco-root--append-extra-info
     base-label
     'category
     category
     (list :scope (or scope 'root)
           :category-id category-id
           :unread unread-count))))

(defun disco-root--channel-has-unread-p (channel)
  "Return non-nil when CHANNEL has unread messages tracked locally."
  (disco-state-channel-has-unread-p channel))

(defun disco-root--channel-visible-in-mode-p (channel mode)
  "Return non-nil when CHANNEL should be visible for MODE."
  (pcase mode
    ('unread
     (disco-root--channel-has-unread-p channel))
    ('dms
     (memq (alist-get 'type channel) '(1 3)))
    (_ t)))

(defun disco-root--channel-visible-in-view-p (channel)
  "Return non-nil when CHANNEL should appear under current view mode."
  (disco-root--channel-visible-in-mode-p channel disco-root--view-mode))

(defun disco-root--activity-channel-base-eligible-p (channel)
  "Return non-nil when CHANNEL is eligible for activity regardless of filter."
  (and (disco-root--displayable-channel-p channel)
       (or disco-root-activity-include-threads
           (not (disco-state-channel-thread-p channel)))))

(defun disco-root--activity-channel-eligible-p (channel)
  "Return non-nil when CHANNEL should appear in activity layout."
  (and (disco-root--activity-channel-base-eligible-p channel)
       (disco-root--channel-visible-in-view-p channel)))

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

(defun disco-root--collect-activity-candidates ()
  "Return unique channels eligible for activity independent of view filter."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (cl-labels
        ((push-unique (channel)
           (let ((channel-id (alist-get 'id channel)))
             (when (and channel-id
                        (not (gethash channel-id seen))
                        (disco-root--activity-channel-base-eligible-p channel))
               (puthash channel-id t seen)
               (push channel result)))))
      (dolist (channel (disco-state-private-channels))
        (push-unique channel))
      (dolist (guild (or (disco-state-guilds) '()))
        (let ((guild-id (alist-get 'id guild)))
          (dolist (channel (or (disco-state-guild-channels guild-id) '()))
            (push-unique channel))
          (when disco-root-activity-include-threads
            (dolist (thread (or (disco-state-guild-threads guild-id) '()))
              (push-unique thread))))))
    (nreverse result)))

(defun disco-root--collect-activity-channels ()
  "Return unique channels for activity layout under current filter mode."
  (disco-root--sort-channels
   (seq-filter #'disco-root--channel-visible-in-view-p
               (disco-root--collect-activity-candidates))))

(defun disco-root--tree-unread-section-channels (unread-channels)
  "Return channels to render in tree unread section from UNREAD-CHANNELS."
  (let ((limit disco-root-tree-unread-section-limit))
    (if (and (integerp limit)
             (> limit 0)
             (> (length unread-channels) limit))
        (seq-take unread-channels limit)
      unread-channels)))

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
  (disco-root--set-view-mode
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
            (unread (or (plist-get entry :unread-count) 0))
            (expanded (disco-root--guild-expanded-p guild-id))
            (indicator (if expanded "[-]" "[+]"))
            (label (disco-root--guild-label guild unread 'root))
            (start (point)))
       (insert (format "  %s " indicator))
       (disco-root--insert-guild-icon guild)
       (insert (format " %s\n" label))
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
            (unread (or (plist-get entry :unread-count) 0))
            (expanded (disco-root--category-expanded-p category-id))
            (indicator (if expanded "[-]" "[+]"))
            (label (disco-root--category-label category unread 'root))
            (start (point)))
       (insert (format "    %s %s\n" indicator label))
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
      (or (plist-get entry :indent) 0)
      (or (plist-get entry :scope) 'root)))
    (_
     (insert "\n"))))

(defun disco-root--ewoc-insert-text (text)
  "Insert one plain TEXT row in root EWOC."
  (ewoc-enter-last disco-root--ewoc
                   (list :entry-type 'text :text text)))

(defun disco-root--ewoc-insert-section (section title &optional count)
  "Insert one clickable SECTION row with TITLE and optional COUNT."
  (let ((node (ewoc-enter-last disco-root--ewoc
                               (list :entry-type 'section
                                     :section section
                                     :title title
                                     :count count))))
    (when (hash-table-p disco-root--section-node-table)
      (puthash section node disco-root--section-node-table))
    node))

(defun disco-root--ewoc-insert-guild (guild unread-count)
  "Insert one collapsible GUILD row with UNREAD-COUNT badge."
  (let* ((node (ewoc-enter-last disco-root--ewoc
                                (list :entry-type 'guild
                                      :guild guild
                                      :unread-count unread-count)))
         (guild-id (alist-get 'id guild)))
    (when (and guild-id
               (hash-table-p disco-root--guild-node-table))
      (puthash guild-id node disco-root--guild-node-table))
    node))

(defun disco-root--ewoc-insert-category (category unread-count)
  "Insert one collapsible CATEGORY row with UNREAD-COUNT badge."
  (let* ((node (ewoc-enter-last disco-root--ewoc
                                (list :entry-type 'category
                                      :category category
                                      :unread-count unread-count)))
         (category-id (alist-get 'id category)))
    (when (and category-id
               (hash-table-p disco-root--category-node-table))
      (puthash category-id node disco-root--category-node-table))
    node))

(defun disco-root--ewoc-insert-blank ()
  "Insert one blank row in root EWOC."
  (ewoc-enter-last disco-root--ewoc (list :entry-type 'blank)))

(defun disco-root--ewoc-insert-channel (channel indent &optional scope)
  "Insert CHANNEL row at INDENT into root EWOC and index node by channel ID."
  (let* ((entry (list :entry-type 'channel
                      :channel channel
                      :indent indent
                      :scope (or scope 'root)))
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
                    (disco-root--channel-label parent-channel 'archived-parent))
     :key-hints "g: refresh   n: next page   RET/mouse-1: open thread   q: quit"
     :summary (format "Loaded: %d   Sources: %s"
                      (length threads)
                      (disco-root--archived-source-status-string))
     :loading-note (unless (disco-root--archived-any-source-has-more-p)
                     "(no more archived pages)")
     :items threads
     :item-inserter (lambda (thread)
                      (disco-root--insert-channel-line thread 2 'archived-thread))
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
                        (disco-root--channel-label parent-channel 'parent-threads-parent)
                      "(no parent)"))
     :key-hints "g: refresh active   A: archived threads   RET/mouse-1: open thread   n/p/TAB: nav   q: quit"
     :summary (format "Active threads indexed: %d"
                      (length (or threads '())))
     :loading-note (when disco-root--parent-threads-refresh-in-flight
                     "[refreshing active threads...]")
     :items threads
     :item-inserter (lambda (thread)
                      (disco-root--insert-channel-line thread 2 'parent-thread))
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
    (unless (disco-root--openable-channel-p channel)
      (user-error "disco: channel %s does not support timeline view" channel-id))
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

(defun disco-root--insert-channel-line (channel indent &optional scope)
  "Insert one CHANNEL at INDENT spaces.

SCOPE is forwarded to extra-info providers."
  (if (eq scope 'activity)
      (disco-root--insert-activity-channel-line channel indent)
    (let* ((channel-id (alist-get 'id channel))
           (channel-type (alist-get 'type channel))
           (label (disco-root--channel-label channel scope))
           (unread-count (disco-state-channel-effective-unread-count channel))
           (has-unread (disco-root--channel-has-unread-p channel))
           (padding (make-string indent ?\s)))
      (let ((line-start (point)))
        (insert (format "%s%s\n" padding label))
        (add-text-properties
         line-start
         (point)
         (list 'disco-root-row-type 'channel
               'disco-channel-id channel-id
               'disco-unread-count unread-count
               'disco-has-unread (and has-unread t)))
        (when (disco-root--openable-channel-p channel)
          (add-text-properties
           line-start
           (point)
           (list 'mouse-face 'highlight
                 'help-echo (if (memq channel-type '(15 16))
                                (format "Open threads under channel %s" channel-id)
                              (format "Open channel %s" channel-id)))))))))

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
  "Jump to next channel row with unread state."
  (interactive)
  (let* ((positions
          (disco-root--channel-line-positions
           (lambda (pos) (disco-root--line-has-unread-p pos))))
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

(defun disco-root--prepare-ewoc-state ()
  "Reset root EWOC and node indexes before layout rendering."
  (setq disco-root--channel-node-table (make-hash-table :test #'equal))
  (setq disco-root--section-node-table (make-hash-table :test #'eq))
  (setq disco-root--guild-node-table (make-hash-table :test #'equal))
  (setq disco-root--category-node-table (make-hash-table :test #'equal))
  (setq disco-root--ewoc (ewoc-create #'disco-root--ewoc-printer nil nil t)))

(defun disco-root--render-layout-tree ()
  "Render guild/category tree layout."
  (let* ((show-unread disco-root--tree-show-unread-section)
         (unread-channels (and show-unread
                               (disco-root--collect-visible-unread-channels)))
         (unread-visible (and show-unread
                              (disco-root--tree-unread-section-channels unread-channels)))
         (unread-hidden (if (and show-unread unread-channels unread-visible)
                            (- (length unread-channels)
                               (length unread-visible))
                          0))
         (private-channels (disco-root--visible-private-channels))
         (guilds (or (disco-state-guilds) '())))
    (disco-root--ensure-section-state
     (append (when show-unread '(unread))
             '(private guilds)))

    (when show-unread
      (disco-root--ewoc-insert-section 'unread "Unread" (length unread-channels))
      (when (disco-root--section-expanded-p 'unread)
        (if unread-visible
            (dolist (channel unread-visible)
              (disco-root--ewoc-insert-channel channel 2))
          (disco-root--ewoc-insert-text "  (no unread channels)"))
        (when (> unread-hidden 0)
          (disco-root--ewoc-insert-text
           (format "  (... %d more unread channels)" unread-hidden))))
      (disco-root--ewoc-insert-blank))

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
            (disco-root--ewoc-insert-text "  (no visible guild channels)")))))))

(defun disco-root--render-layout-activity ()
  "Render activity-sorted channel list layout."
  (let ((channels (disco-root--collect-activity-channels)))
    (if channels
        (dolist (channel channels)
          (disco-root--ewoc-insert-channel channel 2 'activity))
      (disco-root--ewoc-insert-text "  (no visible channels)"))))

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

(defun disco-root--filters-line ()
  "Return filter-chip line inspired by telega root view."
  (let* ((layout-label
          (downcase (disco-root-layout-label (disco-root--ensure-layout))))
         (metrics (disco-root--activity-metrics-by-view)))
    (string-join
     (list (disco-root--filter-chip 'all "Main" metrics)
           (disco-root--filter-chip 'unread "Important" metrics)
           (disco-root--filter-chip 'dms "DMs" metrics)
           (format "[%s sort:%s]" layout-label disco-root--sort-mode))
     "  ")))

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
                (format "  keys[g refresh, G gw-sync, l/L layout, U unread-lens, ? menu]")))
    "")
   (disco-root--filters-line)
   (disco-root--mode-divider-line)))

(defun disco-root--refresh-header-line ()
  "Refresh root header block in place."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (lines (disco-root--header-lines)))
    (save-excursion
      (goto-char (point-min))
      (let ((start (point))
            (end (progn
                   (forward-line 3)
                   (point))))
        (delete-region start end)
        (goto-char start)
        (dolist (line lines)
          (insert line "\n"))))
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
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)
         (layout (disco-root--ensure-layout))
         (renderer (disco-root-layout-renderer layout)))
    (setq-local disco-root--fill-column (disco-root--compute-fill-column))
    (erase-buffer)
    (dolist (line (disco-root--header-lines))
      (insert line "\n"))
    (setq-local disco-root--last-header-refresh-at (float-time))
    (insert "\n")
    (disco-root--prepare-ewoc-state)
    (if (functionp renderer)
        (funcall renderer)
      (disco-root--render-layout-tree))
    (goto-char (point-min))))

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
        (disco-root--normalize-id-list
         (mapcar (lambda (guild)
                   (alist-get 'id guild))
                 (seq-take sorted max-guilds)))))))

(defun disco-root--gateway-last-message-eligible-p (channel)
  "Return non-nil when CHANNEL should be included in op34 last-message sync."
  (and (listp channel)
       (alist-get 'guild_id channel)
       (disco-root--channel-viewable-p channel)
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
        (disco-root--normalize-id-list
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
                 (when (or disco-root--dirty-structure-p
                           disco-root--dirty-header-p
                           disco-root--dirty-channel-ids)
                   (disco-root--queue-live-update nil nil nil))
                 (when disco-root-gateway-context-sync-on-refresh
                   (ignore-errors
                     (disco-root-sync-gateway-context t)))
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
    (define-key map (kbd "G") #'disco-root-sync-gateway-context)
    (define-key map (kbd "A") #'disco-root-list-archived-threads)
    (define-key map (kbd "l") #'disco-root-cycle-layout)
    (define-key map (kbd "L") #'disco-root-set-layout)
    (define-key map (kbd "\\") #'disco-root-toggle-sort-mode)
    (define-key map (kbd "v") #'disco-root-cycle-view-mode)
    (define-key map (kbd "U") #'disco-root-toggle-unread-lens)
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
  (setq-local disco-root--guild-expanded (make-hash-table :test #'equal))
  (setq-local disco-root--category-expanded (make-hash-table :test #'equal)))

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

(provide 'disco-root)

;;; disco-root.el ends here
