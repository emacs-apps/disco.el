;;; disco-root-view.el --- Root view builders for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root-specific view state, row models, inserters, EWOC rendering, and layout
;; builders. This keeps `disco-root.el' focused on controller, live-update,
;; and buffer lifecycle logic while `appkit-view.el' continues to provide
;; reusable generic UI primitives.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'disco-util)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-permission)
(require 'disco-preview)
(require 'disco-room)
(require 'disco-thread)
(require 'appkit-ui)
(require 'appkit-view)
(require 'disco-state)
(require 'disco-root-layout)

(defvar disco-root--archived-parent-channel)
(defvar disco-root--archived-before-cursors)
(defvar disco-root--archived-source-has-more)
(defvar disco-root--archived-threads-cache)
(defvar disco-root--archived-last-errors)
(defvar disco-root--archived-thread-sources)
(defvar disco-root--inspect-channel)
(defvar disco-root--tree-show-unread-section)
(defvar disco-root--view-mode)
(defvar disco-root--search-domain)
(defvar disco-root--search-query-spec)
(defvar disco-root--search-tab-order)
(defvar disco-root--search-tabs)
(defvar disco-root--search-tab-label-alist)
(defvar disco-root--search-channel-table)
(defvar disco-root--search-thread-table)
(defvar disco-root--section-order)
(defvar disco-root--section-expanded)
(defvar disco-root--ewoc)
(defvar disco-root--channel-node-table)
(defvar disco-root--section-node-table)
(defvar disco-root--guild-node-table)
(defvar disco-root--layout)
(defvar disco-root--sort-mode)
(defvar disco-root--fill-column)
(defvar disco-root--activity-icon-slot-width)
(defvar disco-root--guild-icon-fetching)
(defvar disco-root--guild-icon-image-cache)
(defvar disco-root--extra-info-provider-error-cache)
(defvar disco-root-extra-info-functions)
(defvar disco-root-default-layout)
(defvar disco-root-guild-icon-size)
(defvar disco-root-show-guild-icons)
(defvar disco-root-activity-context-width)
(defvar disco-root-activity-context-separator)
(defvar disco-root-activity-include-threads)
(defvar disco-root-activity-time-format-alist)
(defvar disco-root-activity-time-column-width)
(defvar disco-root-auto-fill-margin-columns)
(defvar disco-root-tree-unread-section-limit)
(defvar disco-root-tree-default-expanded-sections)
(defvar disco-root-week-start-day)
(defvar disco-thread-archive-fetch-limit)

(declare-function disco-root--ensure-section-state "disco-root" (&optional sections))
(declare-function disco-root--section-expanded-p "disco-root" (section))
(declare-function disco-root--toggle-node-at-point "disco-root" ())
(declare-function disco-channel-directory-open "disco-channel-directory" (guild-id))
(declare-function disco-channel-directory-open-thread-parent
                  "disco-channel-directory" (parent-channel-id))

(defvar disco-root-view-attach-live-updates-function nil
  "Function used by root view buffers to attach live updates.")

(defvar disco-root-view-load-more-function nil
  "Function used by root search rows to load more results.")

(defvar disco-root-view-queue-live-update-function nil
  "Function used by root view helpers to queue live rerenders.")

(defvar disco-root-view-render-preserving-position-function nil
  "Function used by root view toggles to rerender with position preserved.")

(defvar disco-root-view-transient-function nil
  "Interactive command used for root view transient menus.")

(defface disco-root-section-heading
  '((t :inherit bold))
  "Face for semantic section headings in the root buffer."
  :group 'disco)

(defface disco-root-active-lens
  '((t :inherit mode-line-emphasis :weight bold))
  "Face for the active root header lens."
  :group 'disco)

(defface disco-root-unread-badge
  '((t :inherit font-lock-warning-face :weight semi-bold))
  "Face for unread and mention indicators in root rows."
  :group 'disco)

(defface disco-root-context-separator
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for hierarchy separators in root activity rows."
  :group 'disco)

(defun disco-root-view--call-controller (function label &rest args)
  "Call controller FUNCTION named by LABEL with ARGS.

Signal a user-facing error when the root controller callback is missing."
  (unless (and function
               (or (functionp function)
                   (and (symbolp function)
                        (fboundp function))))
    (error "disco: root view callback is not configured: %s" label))
  (apply function args))

(defun disco-root-view--attach-live-updates ()
  "Attach live updates for the current root-related buffer."
  (disco-root-view--call-controller
   disco-root-view-attach-live-updates-function
   'attach-live-updates))

(defun disco-root-view--load-more (tab)
  "Load more search results for exact search TAB."
  (disco-root-view--call-controller
   disco-root-view-load-more-function
   'load-more
   tab))

(defun disco-root-view--queue-live-update (channel-ids &optional structural-p header-p)
  "Queue one controller-driven live update for CHANNEL-IDS."
  (disco-root-view--call-controller
   disco-root-view-queue-live-update-function
   'queue-live-update
   channel-ids structural-p header-p))

(defun disco-root-view--render-preserving-position ()
  "Rerender root buffer while preserving point/window state."
  (disco-root-view--call-controller
   disco-root-view-render-preserving-position-function
   'render-preserving-position))

(defun disco-root-view--transient ()
  "Open the root transient menu through the controller."
  (interactive)
  (disco-root-view--call-controller
   disco-root-view-transient-function
   'transient))

(defun disco-root--ensure-layout ()
  "Return the active root layout, signaling when it is not registered."
  (unless (memq disco-root--layout (disco-root-layout-names))
    (error "disco: root layout is not registered: %S" disco-root--layout))
  disco-root--layout)

(defun disco-root--search-domain-kind (domain)
  "Return kind symbol from root search DOMAIN plist."
  (plist-get domain :kind))

(defun disco-root--search-domain-id (domain)
  "Return identifier from root search DOMAIN plist."
  (plist-get domain :id))

(defun disco-root--search-domain-channel-id (domain)
  "Return fixed channel id from root search DOMAIN when it is channel-scoped."
  (when (eq (disco-root--search-domain-kind domain) 'channel)
    (disco-root--search-domain-id domain)))

(defun disco-root--search-domain-guild-id (domain)
  "Return guild id associated with root search DOMAIN, or nil."
  (pcase (disco-root--search-domain-kind domain)
    ('guild
     (disco-root--search-domain-id domain))
    ('channel
     (plist-get domain :guild-id))
    (_ nil)))

(defun disco-root--search-domain-channel-object (domain)
  "Return fixed channel object for root search DOMAIN, or nil."
  (when-let* ((channel-id (disco-root--search-domain-channel-id domain)))
    (or (disco-state-channel channel-id)
        (and (hash-table-p disco-root--search-channel-table)
             (gethash channel-id disco-root--search-channel-table))
        (and (hash-table-p disco-root--search-thread-table)
             (gethash channel-id disco-root--search-thread-table)))))

(defun disco-root--search-channel-domain (channel)
  "Return channel-scoped search domain plist for CHANNEL."
  (when (disco-channel-searchable-p channel)
    (when-let* ((channel-id (and (listp channel) (alist-get 'id channel))))
      (let ((label (or (disco-root--channel-display-name channel)
                       channel-id)))
        (list :kind 'channel
              :id channel-id
              :guild-id (alist-get 'guild_id channel)
              :label label)))))

(defun disco-root--search-current-channel-domain ()
  "Return current channel-scoped search domain inferred from point, or nil."
  (or (when (and (eq (disco-root--ensure-layout) 'search)
                 (eq (disco-root--search-domain-kind disco-root--search-domain) 'channel))
        disco-root--search-domain)
      (when-let* ((channel-id (disco-root--line-channel-id))
                  (channel (or (disco-state-channel channel-id)
                               (and (hash-table-p disco-root--search-channel-table)
                                    (gethash channel-id disco-root--search-channel-table))
                               (and (hash-table-p disco-root--search-thread-table)
                                    (gethash channel-id disco-root--search-thread-table)))))
        (disco-root--search-channel-domain channel))))

(defun disco-root--search-domain-label (domain)
  "Return display label from root search DOMAIN plist."
  (or (plist-get domain :label)
      (pcase (disco-root--search-domain-kind domain)
        ('dms "DMs")
        ('guild (or (disco-root--guild-name-by-id (disco-root--search-domain-id domain))
                    (disco-root--search-domain-id domain)
                    "Guild"))
        ('channel (let ((channel (disco-root--search-domain-channel-object domain)))
                    (or (and channel (disco-root--channel-display-name channel))
                        (disco-root--search-domain-channel-id domain)
                        "Channel")))
        (_ "Search"))))


(defun disco-root--search-empty-tab-state ()
  "Return freshly initialized root search tab state plist."
  (list :items nil
        :loading nil
        :error nil
        :cursor nil
        :total-results nil
        :time-spent-ms nil))

(defun disco-root--search-reset-tab-states ()
  "Reset root search tab state alist for the current buffer."
  (setq-local disco-root--search-tabs
              (mapcar (lambda (tab)
                        (cons tab (disco-root--search-empty-tab-state)))
                      disco-root--search-tab-order)))

(defun disco-root--search-tab-state (tab)
  "Return root search TAB state plist, initializing when needed."
  (or (alist-get tab disco-root--search-tabs nil nil #'eq)
      (let ((state (disco-root--search-empty-tab-state)))
        (push (cons tab state) disco-root--search-tabs)
        state)))

(defun disco-root--search-set-tab-state (tab state)
  "Set root search TAB STATE plist and return STATE."
  (if-let* ((cell (assq tab disco-root--search-tabs)))
      (setcdr cell state)
    (push (cons tab state) disco-root--search-tabs))
  state)

(defun disco-root--search-tab-label (tab)
  "Return display label for root search TAB symbol."
  (or (alist-get tab disco-root--search-tab-label-alist nil nil #'eq)
      (capitalize (symbol-name tab))))


(defun disco-root--search-effective-spec-p (&optional spec)
  "Return non-nil when root search SPEC contains an actual query/filter."
  (let ((it (or spec disco-root--search-query-spec)))
    (or (let ((content (plist-get it :content)))
          (and (stringp content)
               (not (string-empty-p content))))
        (plist-get it :author-ids)
        (plist-get it :author-types)
        (plist-get it :mentions)
        (plist-get it :mention-everyone)
        (plist-get it :has)
        (plist-get it :slop)
        (plist-member it :pinned)
        (plist-get it :channel-ids)
        (plist-get it :max-id)
        (plist-get it :min-id))))


(defun disco-root--search-channel (channel-id)
  "Return best-effort channel object for CHANNEL-ID in search results."
  (or (and channel-id (disco-state-channel channel-id))
      (and channel-id
           (hash-table-p disco-root--search-thread-table)
           (gethash channel-id disco-root--search-thread-table))
      (and channel-id
           (hash-table-p disco-root--search-channel-table)
           (gethash channel-id disco-root--search-channel-table))))


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
         (frame (and (window-live-p win)
                     (window-frame win)))
         (char-width
          (or (and (frame-live-p frame)
                   (let* ((font (ignore-errors (face-font 'default frame)))
                          (info (and font (ignore-errors (font-info font frame)))))
                     (when info
                       (let ((width (aref info 11)))
                         (if (> width 0)
                             width
                           (aref info 10))))))
              (and (frame-live-p frame)
                   (frame-char-width frame))
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

(defun disco-root--selected-window-for-buffer (&optional buffer)
  "Return selected window when it displays BUFFER, otherwise nil."
  (let* ((buf (or buffer (current-buffer)))
         (win (selected-window)))
    (when (and (window-live-p win)
               (eq (window-buffer win) buf))
      win)))

(defun disco-root--render-fill-column (&optional buffer)
  "Return stable fill width for BUFFER before one render pass.

Mirror telega's root autofill behavior: prefer the selected BUFFER
window, otherwise reuse the last width so passive background rerenders do
not jump between windows or reflow unpredictably."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((buf (current-buffer))
           (selected-win (disco-root--selected-window-for-buffer buf))
           (fallback-win (or selected-win
                             (disco-root--display-window buf)))
           (computed-width
            (and (window-live-p selected-win)
                 (disco-root--compute-fill-column buf selected-win))))
      (or (and (integerp computed-width)
               (> computed-width 0)
               computed-width)
          (and (integerp disco-root--fill-column)
               (> disco-root--fill-column 0)
               disco-root--fill-column)
          (and (window-live-p fallback-win)
               (disco-root--compute-fill-column buf fallback-win))
          (disco-root--compute-fill-column buf fallback-win)))))

(defun disco-root--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (format "%S" err)))

(defun disco-root--archived-source-fetch-allowed-p (source-name parent-channel)
  "Return non-nil when archived SOURCE-NAME is expected to be fetchable.

This prevents noisy permission errors for sources that require elevated access."
  (cond
   ;; Discord private archived thread listing requires MANAGE_THREADS.
   ((equal source-name "private")
    (disco-permission-channel-has-p parent-channel 'manage-threads nil))
   (t t)))

(defun disco-root--displayable-channel-p (channel)
  "Return non-nil when CHANNEL should appear in root buffer."
  (and (disco-channel-root-visible-p channel)
       (disco-permission-channel-viewable-p channel nil)))

(defun disco-root--openable-channel-p (channel)
  "Return non-nil when CHANNEL has a supported open action."
  (not (null (disco-channel-open-mode channel))))

(defun disco-root--channel-open-help-echo (channel)
  "Return hover help text describing how CHANNEL will open."
  (let ((channel-id (alist-get 'id channel)))
    (pcase (disco-channel-open-mode channel)
      ('thread-directory
       (format "Expand active posts under channel %s in its guild directory"
               channel-id))
      ('inspect
       (format "Inspect channel %s" channel-id))
      (_
       (format "Open channel %s" channel-id)))))

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

(defun disco-root--private-channel-recipient-display-names (channel)
  "Return display names for CHANNEL recipients.

Exclude the current user when its ID is known."
  (let* ((current-user-id (and (fboundp 'disco-gateway-current-user-id)
                               (disco-gateway-current-user-id)))
         (recipients (or (alist-get 'recipients channel) '()))
         (filtered (if current-user-id
                       (seq-remove
                        (lambda (recipient)
                          (equal (format "%s" (alist-get 'id recipient))
                                 (format "%s" current-user-id)))
                        recipients)
                     recipients))
         (effective-recipients (if filtered filtered recipients)))
    (delq nil
          (mapcar (lambda (recipient)
                    (when (listp recipient)
                      (disco-root--recipient-display-name recipient)))
                  effective-recipients))))

(defun disco-root--private-channel-display-name (channel)
  "Return best display name for private CHANNEL."
  (let* ((channel-type (alist-get 'type channel))
         (explicit-name (and (stringp (alist-get 'name channel))
                             (not (string-empty-p (alist-get 'name channel)))
                             (alist-get 'name channel)))
         (recipient-names (disco-root--private-channel-recipient-display-names channel)))
    (cond
     ((disco-channel-direct-message-p channel-type)
      (or (car recipient-names) explicit-name "direct-message"))
     ((disco-channel-group-dm-p channel-type)
      (or explicit-name
          (and recipient-names (mapconcat #'identity recipient-names ", "))
          "group-dm"))
     (t
      (or explicit-name "(no-name)")))))

(defun disco-root--channel-display-name (channel)
  "Return display name for CHANNEL independent of badge suffixes."
  (if (disco-channel-private-p channel)
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
  "Schedule debounced rerender for live root buffers after icon updates."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq major-mode 'disco-root-mode)
          (disco-root-view--queue-live-update nil t nil))))))

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

(defun disco-root--thread-count-under-parent (channel)
  "Return number of indexed active threads under CHANNEL."
  (length
   (seq-remove #'disco-thread-archived-p
               (disco-state-parent-threads (alist-get 'id channel)))))

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
    (when (disco-state-channel-age-restricted-p channel)
      (push "18+" tags))
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

(defun disco-root--thread-parent-channel (thread)
  "Return parent channel object for THREAD, or nil."
  (when-let* ((parent-id (alist-get 'parent_id thread)))
    (disco-state-channel parent-id)))

(defun disco-root--thread-tag-label (tag)
  "Return human-readable label for one forum/media TAG object."
  (when (listp tag)
    (let ((emoji-name (alist-get 'emoji_name tag))
          (name (alist-get 'name tag)))
      (string-join
       (delq nil
             (list (and (stringp emoji-name)
                        (not (string-empty-p emoji-name))
                        emoji-name)
                   (and (stringp name)
                        (not (string-empty-p name))
                        name)))
       " "))))

(defun disco-root--thread-applied-tag-labels (thread)
  "Return applied forum/media tag labels for THREAD."
  (let* ((parent-channel (disco-root--thread-parent-channel thread))
         (available-tags (and (listp parent-channel)
                              (alist-get 'available_tags parent-channel)))
         labels)
    (dolist (tag-id (or (alist-get 'applied_tags thread) '()))
      (let* ((normalized-tag-id (and tag-id (format "%s" tag-id)))
             (tag (and normalized-tag-id
                       (seq-find
                        (lambda (candidate)
                          (equal normalized-tag-id
                                 (and (listp candidate)
                                      (format "%s" (alist-get 'id candidate)))))
                        available-tags)))
             (label (and tag (disco-root--thread-tag-label tag))))
        (when (and (stringp label)
                   (not (string-empty-p label)))
          (push label labels))))
    (nreverse labels)))

(defun disco-root--thread-browser-context-label (thread)
  "Return one-line context label for THREAD browser rows."
  (let* ((name (disco-root--channel-display-name thread))
         (tag-labels (disco-root--thread-applied-tag-labels thread))
         (parts (cons name tag-labels)))
    (string-join (delq nil parts) " | ")))

(defun disco-root--archived-thread-metadata-preview (thread)
  "Return metadata preview for archived THREAD rows, or nil."
  (let* ((status-tags (disco-thread-status-tags thread))
         (message-count (or (alist-get 'total_message_sent thread)
                            (alist-get 'message_count thread)))
         (thread-id (alist-get 'id thread))
         (member-count (or (alist-get 'member_count thread)
                           (and thread-id
                                (disco-state-thread-member-count thread-id))))
         parts)
    (dolist (status status-tags)
      (push status parts))
    (when (numberp message-count)
      (push (format "%s msg%s"
                    (disco-root--human-count message-count)
                    (if (= message-count 1) "" "s"))
            parts))
    (when (numberp member-count)
      (push (format "%s member%s"
                    (disco-root--human-count member-count)
                    (if (= member-count 1) "" "s"))
            parts))
    (when parts
      (string-join (nreverse parts) " · "))))

(defun disco-root--thread-browser-time-label (thread scope &optional message)
  "Return time label for THREAD browser row under SCOPE."
  (if (eq scope 'archived-thread)
      (let* ((archive-timestamp (or (alist-get 'archive_timestamp
                                               (disco-root--thread-metadata thread))
                                    (alist-get 'archive_timestamp thread)))
             (seconds (and archive-timestamp
                           (ignore-errors
                             (float-time (date-to-time archive-timestamp))))))
        (or (and seconds
                 (ignore-errors
                   (disco-root--activity-time-string seconds)))
            (disco-root--channel-last-activity-time-label thread message)))
    (disco-root--channel-last-activity-time-label thread message)))

(defun disco-root--activity-context-label (channel &optional scope)
  "Return context label for CHANNEL one-line rows under SCOPE."
  (pcase scope
    ((or 'parent-thread 'archived-thread)
     (disco-root--thread-browser-context-label channel))
    ('directory
     (disco-root--channel-display-name channel))
    (_
     (disco-root--activity-primary-label channel))))

(defun disco-root--activity-primary-label (channel)
  "Return activity-layout primary label for CHANNEL."
  (let* ((parts (delq nil
                      (list (disco-root--channel-guild-name channel)
                            (disco-root--channel-category-name channel)
                            (disco-root--channel-display-name channel))))
         (separator
          (propertize disco-root-activity-context-separator
                      'face 'disco-root-context-separator)))
    (if parts
        (mapconcat #'identity parts separator)
      (disco-root--channel-display-name channel))))

(defun disco-root--activity-secondary-placeholder (channel)
  "Return non-message placeholder preview for CHANNEL, or nil."
  (pcase (disco-channel-open-mode channel)
    ('thread-directory
     (let ((count (disco-root--thread-count-under-parent channel)))
       (format "%d active post%s" count (if (= count 1) "" "s"))))
    ('inspect
     (format "(%s view)" (disco-channel-type-name channel)))
    (_ nil)))

(defun disco-root--activity-secondary-label (channel)
  "Return fallback activity preview label for CHANNEL.

The label stays message-oriented and avoids transport-status placeholders."
  (let ((channel-id (alist-get 'id channel)))
    (or (disco-root--activity-secondary-placeholder channel)
        (and channel-id
             (disco-state-channel-conversation-summary-preview channel-id))
        "")))

(defun disco-root--activity-preview-label (channel &optional scope)
  "Return fallback preview label for CHANNEL one-line rows under SCOPE."
  (pcase scope
    ('archived-thread
     (or (disco-root--archived-thread-metadata-preview channel)
         (disco-root--activity-secondary-label channel)))
    (_
     (disco-root--activity-secondary-label channel))))

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

(defun disco-root--activity-preview-line (channel &optional message scope)
  "Return one-line preview text for CHANNEL row under SCOPE.

When MESSAGE is non-nil, use it as cached preview source."
  (if (eq scope 'parent-thread)
      (if-let* ((starter (disco-thread-starter-message channel)))
          (disco-msg-preview-line starter)
        (propertize "Original post unavailable" 'face 'shadow))
    (or (and message (disco-msg-preview-line message))
        (disco-msg-channel-preview-line channel)
        (disco-root--activity-secondary-placeholder channel)
        (progn
          (disco-preview-request-channel channel)
          (disco-root--activity-preview-label channel scope)))))

(defun disco-root--insert-activity-icon (channel &optional scope)
  "Insert activity icon for CHANNEL.

Guild rows use real guild icons when available, with fixed text fallback.
SCOPE distinguishes guild activity rows from channel-directory rows."
  (let ((guild (and (alist-get 'guild_id channel)
                    (disco-root--guild-by-id (alist-get 'guild_id channel))))
        (channel-type (alist-get 'type channel))
        (start (point)))
    (cond
     ((and guild (eq scope 'activity))
      (if disco-root-show-guild-icons
          (disco-root--insert-guild-icon guild)
        (insert (disco-root--guild-icon-fallback guild))))
     ((eq channel-type 1)
      (insert "@"))
     ((eq channel-type 3)
      (insert "◎"))
     ((disco-state-channel-thread-p channel)
      (insert "↳"))
     ((eq channel-type 2)
      (insert "◉"))
     ((eq channel-type 13)
      (insert "◆"))
     ((eq channel-type 15)
      (insert "▤"))
     ((eq channel-type 16)
      (insert "▦"))
     ((eq channel-type 14)
      (insert "◇"))
     (t
      (insert "#")))
    (add-text-properties start (point) (list 'face 'shadow))))

(defun disco-root--preview-leading-length (preview-text message)
  "Return highlighted author-prefix length for PREVIEW-TEXT from MESSAGE."
  (let ((author (disco-msg-author-display-name message)))
    (when (and author
               (string-match (format "\\`%s>" (regexp-quote author))
                             (or preview-text "")))
      (1+ (length author)))))

(defun disco-root--channel-one-line-row (channel &optional scope)
  "Return one-line row model for CHANNEL under SCOPE."
  (let* ((channel-id (alist-get 'id channel))
         (latest-message (disco-msg-channel-last-cached-message channel))
         (preview-message
          (if (eq scope 'parent-thread)
              (disco-thread-starter-message channel)
            latest-message))
         (mention-count (disco-state-channel-effective-unread-count channel))
         (has-unread (disco-root--channel-has-unread-p channel))
         (preview-text (disco-root--activity-preview-line
                        channel preview-message scope))
         (time-text (if (memq scope '(parent-thread archived-thread))
                        (disco-root--thread-browser-time-label channel scope latest-message)
                      (disco-root--channel-last-activity-time-label channel latest-message))))
    (appkit-view-one-line-row-create
     :icon-inserter (lambda ()
                      (disco-root--insert-activity-icon channel scope))
     :context (disco-root--activity-context-label channel scope)
     :preview preview-text
     :preview-leading-length
     (disco-root--preview-leading-length preview-text preview-message)
     :preview-leading-face 'font-lock-keyword-face
     :time time-text
     :time-face 'shadow
     :time-tail-face (unless (eq scope 'archived-thread)
                       (disco-root--activity-time-status-face channel latest-message))
     :line-properties
     (list 'disco-root-row-type 'channel
           'disco-channel-id channel-id
           'disco-unread-count mention-count
           'disco-has-unread (and has-unread t))
     :help-echo (and (disco-root--openable-channel-p channel)
                     (disco-root--channel-open-help-echo channel)))))

(defun disco-root--insert-activity-channel-line
    (channel indent &optional scope width)
  "Insert one activity-style CHANNEL row with INDENT under SCOPE.

WIDTH overrides the root buffer's responsive fill column."
  (appkit-view-insert-one-line-row
   (disco-root--channel-one-line-row channel scope)
   :indent indent
   :width (max 60 (or width
                      disco-root--fill-column
                      (disco-root--compute-fill-column)))
   :icon-slot-width
   (max 2
        (ceiling (* disco-root--activity-icon-slot-width
                    (disco-root--text-scale-factor))))
   :context-width-spec disco-root-activity-context-width
   :time-slot-width disco-root-activity-time-column-width))

(defun disco-root--search-message-seconds (message)
  "Return MESSAGE timestamp as float seconds, or nil on parse failure."
  (when-let* ((timestamp (alist-get 'timestamp message)))
    (ignore-errors
      (float-time (date-to-time timestamp)))))

(defun disco-root--search-message-time-label (message)
  "Return formatted time label for search result MESSAGE."
  (when-let* ((seconds (disco-root--search-message-seconds message)))
    (disco-root--activity-time-string seconds)))

(defun disco-root--search-context-label (channel)
  "Return context label for search hit CHANNEL."
  (cond
   ((null channel)
    (disco-root--search-domain-label disco-root--search-domain))
   ((and (disco-state-private-channel-p channel)
         (not (alist-get 'guild_id channel)))
    (or (disco-root--channel-display-name channel)
        (disco-root--search-domain-label disco-root--search-domain)))
   (t
    (or (disco-root--activity-primary-label channel)
        (disco-root--channel-display-name channel)
        (disco-root--search-domain-label disco-root--search-domain)))))

(defun disco-root--search-message-one-line-row (message &optional tab)
  "Return one-line row model for search result MESSAGE in TAB."
  (let* ((message-id (alist-get 'id message))
         (channel-id (alist-get 'channel_id message))
         (channel (disco-root--search-channel channel-id))
         (preview-text (or (disco-msg-preview-line message)
                           (disco-msg-preview-content message)
                           "(message)")))
    (appkit-view-one-line-row-create
     :icon-inserter (lambda ()
                      (if channel
                          (disco-root--insert-activity-icon channel)
                        (let ((start (point)))
                          (insert "[?]")
                          (add-text-properties start (point) (list 'face 'shadow)))))
     :context (disco-root--search-context-label channel)
     :preview preview-text
     :preview-leading-length
     (disco-root--preview-leading-length preview-text message)
     :preview-leading-face 'font-lock-keyword-face
     :time (or (disco-root--search-message-time-label message) "")
     :time-face 'shadow
     :line-properties
     (list 'disco-root-row-type 'search-message
           'disco-root-search-tab tab
           'disco-root-search-message-id message-id
           'disco-channel-id channel-id
           'disco-unread-count 0
           'disco-has-unread nil)
     :help-echo (and channel-id message-id
                     (format "Open channel %s and jump to message %s"
                             channel-id message-id)))))

(defun disco-root--open-search-message (message)
  "Open exact root search result MESSAGE."
  (let ((message-id (alist-get 'id message))
        (channel-id (alist-get 'channel_id message)))
    (unless (and message-id channel-id)
      (user-error "disco: search result has no openable message"))
    (disco-room-jump-to-message message-id channel-id)))

(defun disco-root--insert-search-message-line (message indent &optional tab)
  "Insert one root search result MESSAGE row with INDENT for TAB."
  (let ((row (disco-root--search-message-one-line-row message tab))
        (start (point)))
    (appkit-view-insert-one-line-row
     row
     :indent indent
     :width (max 60 (or disco-root--fill-column
                        (disco-root--compute-fill-column)))
     :icon-slot-width
     (max 2
          (ceiling (* disco-root--activity-icon-slot-width
                      (disco-root--text-scale-factor))))
     :context-width-spec disco-root-activity-context-width
     :time-slot-width disco-root-activity-time-column-width)
    (appkit-ui-make-action-row
     start (point) message #'disco-root--open-search-message
     :help-echo (appkit-view-one-line-row-help-echo row))))

(defun disco-root--channel-label (channel &optional scope)
  "Return display label for CHANNEL.

SCOPE is a symbol describing where the row is rendered."
  (let ((name (disco-root--channel-display-name channel))
        (channel-type (alist-get 'type channel))
        (channel-id (alist-get 'id channel))
        (mention-count (disco-state-channel-effective-unread-count channel))
        (has-unread (disco-root--channel-has-unread-p channel))
        base-label)
    (let ((state-suffix
           (cond
            ((> mention-count 0)
             (propertize (format "  @%d" mention-count)
                         'face 'disco-root-unread-badge))
            (has-unread
             (propertize "  •" 'face 'disco-root-unread-badge))
            (t "")))
          (trail-suffix
           (if (eq scope 'activity)
               ""
             (let ((trail (disco-root--format-trail-tags
                           (disco-root--channel-static-trail-tags channel))))
               (if trail
                   (concat " " trail)
                 "")))))
      (setq base-label
            (cond
             ((disco-channel-direct-message-p channel-type)
              (format "@  %s%s%s" name state-suffix trail-suffix))
             ((disco-channel-group-dm-p channel-type)
              (format "◎  %s%s%s" name state-suffix trail-suffix))
             (t
              (pcase channel-type
                ((or 10 11 12)
                 (let ((tags (disco-root--thread-status-tags channel)))
                   (format "↳  %s%s%s%s"
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
                     ((or 0 5)
                      (format "#  %s%s%s%s" name suffix state-suffix trail-suffix))
                     (15
                      (format "▤  %s%s%s%s" name suffix state-suffix trail-suffix))
                     (16
                      (format "▦  %s%s%s%s" name suffix state-suffix trail-suffix)))))
                (2 (format "◉  %s%s%s" name state-suffix trail-suffix))
                (13 (format "◆  %s%s%s" name state-suffix trail-suffix))
                (14 (format "◇  %s%s%s" name state-suffix trail-suffix))
                (17 (format "○  %s%s%s" name state-suffix trail-suffix))
                (_
                 (format "?%s  %s%s%s"
                         channel-type name state-suffix trail-suffix)))))))
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
         (base-label
          (concat guild-name
                  (if (> unread-count 0)
                      (propertize (format "  %d" unread-count)
                                  'face 'disco-root-unread-badge)
                    ""))))
    (disco-root--append-extra-info
     base-label
     'guild
     guild
     (list :scope (or scope 'root)
           :guild-id guild-id
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
     (disco-channel-private-p channel))
    (_ t)))

(defun disco-root--channel-visible-in-view-p (channel)
  "Return non-nil when CHANNEL should appear under current view mode."
  (and (disco-root--displayable-channel-p channel)
       (disco-root--channel-visible-in-mode-p channel disco-root--view-mode)))

(defun disco-root--activity-channel-base-eligible-p (channel)
  "Return non-nil when CHANNEL is eligible for activity regardless of filter."
  (and (disco-root--displayable-channel-p channel)
       (or disco-root-activity-include-threads
           (not (disco-state-channel-thread-p channel)))))

(defun disco-root--activity-channel-eligible-p (channel)
  "Return non-nil when CHANNEL should appear in activity layout."
  (and (or disco-root-activity-include-threads
           (not (disco-state-channel-thread-p channel)))
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
  "Return home quick-section channels from UNREAD-CHANNELS."
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

(defun disco-root--entry-text (text)
  "Return one plain text layout entry for TEXT."
  (disco-root-layout-entry-create :type 'text :text text))

(defun disco-root--entry-blank ()
  "Return one blank layout entry."
  (disco-root-layout-entry-create :type 'blank))

(defun disco-root--entry-section (section title &optional count)
  "Return one section layout entry."
  (disco-root-layout-entry-create :type 'section
                                  :section section
                                  :title title
                                  :count count))

(defun disco-root--entry-guild (guild unread-count)
  "Return one guild layout entry."
  (disco-root-layout-entry-create :type 'guild
                                  :guild guild
                                  :unread-count unread-count))

(defun disco-root--entry-channel (channel indent &optional scope)
  "Return one channel layout entry for CHANNEL at INDENT in SCOPE."
  (disco-root-layout-entry-create :type 'channel
                                  :channel channel
                                  :indent indent
                                  :scope (or scope 'root)))

(defun disco-root--entry-search-section (tab title loaded-count &optional total-count loading)
  "Return one search-section layout entry."
  (disco-root-layout-entry-create :type 'search-section
                                  :tab tab
                                  :title title
                                  :loaded-count loaded-count
                                  :total-count total-count
                                  :loading loading))

(defun disco-root--entry-search-message (message indent &optional tab)
  "Return one search-message layout entry."
  (disco-root-layout-entry-create :type 'search-message
                                  :message message
                                  :indent (or indent 2)
                                  :tab tab))

(defun disco-root--entry-search-note (text &optional face)
  "Return one search-note layout entry."
  (disco-root-layout-entry-create :type 'search-note
                                  :text text
                                  :face face))

(defun disco-root--entry-search-action (label action tab)
  "Return one search-action layout entry."
  (disco-root-layout-entry-create :type 'search-action
                                  :label label
                                  :action action
                                  :tab tab))

(defun disco-root--section-label-row (section title &optional count)
  "Return label row model for one root SECTION heading."
  (let* ((expanded (disco-root--section-expanded-p section))
         (indicator (if expanded "▾" "▸")))
    (appkit-view-label-row-create
     :label (or title "Section")
     :prefix (format "%s " indicator)
     :suffix (if (numberp count) (format "  %d" count) "")
     :face 'disco-root-section-heading
     :line-properties (list 'disco-root-row-type 'section
                            'disco-root-section section)
     :help-echo "RET or TAB toggles this section"
     :mouse-face 'highlight)))

(defun disco-root--guild-label-row (guild unread-count)
  "Return navigation row model for GUILD with UNREAD-COUNT."
  (let* ((guild-id (alist-get 'id guild))
         (label (disco-root--guild-label guild unread-count 'root)))
    (appkit-view-label-row-create
     :label label
     :prefix "  "
     :suffix "  ›"
     :icon-inserter (lambda ()
                      (disco-root--insert-guild-icon guild))
     :icon-separator " "
     :face (and (> unread-count 0) 'bold)
     :line-properties (list 'disco-root-row-type 'guild
                            'disco-root-guild-id guild-id)
     :help-echo "RET or TAB opens this guild's channel directory")))

(defun disco-root--search-section-label-row (title loaded-count &optional total-count loading)
  "Return label row model for one search section heading."
  (let ((suffix (cond
                 ((numberp total-count)
                  (format " (%d/%d)" loaded-count total-count))
                 (loading
                  (format " (%d loaded, loading...)" loaded-count))
                 (t
                  (format " (%d)" loaded-count)))))
    (appkit-view-label-row-create
     :label (format "%s%s" (or title "Results") suffix)
     :face 'font-lock-keyword-face
     :line-properties (list 'disco-root-row-type 'search-section))))

(defun disco-root--search-note-label-row (text &optional face)
  "Return label row model for one search note TEXT."
  (appkit-view-label-row-create
   :label (or text "")
   :face (or face 'shadow)
   :line-properties (list 'disco-root-row-type 'search-note)))

(defun disco-root--search-action-label-row (label action tab)
  "Return label row model for one search action LABEL, ACTION, and TAB."
  (appkit-view-label-row-create
   :label (or label "Action")
   :prefix "  ["
   :suffix "]"
   :face 'link
   :line-properties (list 'disco-root-row-type 'search-action
                          'disco-root-search-action action
                          'disco-root-search-tab tab)
   :help-echo label
   :mouse-face 'highlight))

(defun disco-root--activate-search-action (entry)
  "Activate exact root search action ENTRY."
  (pcase (disco-root-layout-entry-action entry)
    ('load-more
     (disco-root-view--load-more (disco-root-layout-entry-tab entry)))
    (action
     (user-error "disco: unsupported search action: %S" action))))

(defun disco-root--insert-search-action-line (entry)
  "Insert one exact actionable root search ENTRY."
  (let* ((row (disco-root--search-action-label-row
               (disco-root-layout-entry-label entry)
               (disco-root-layout-entry-action entry)
               (disco-root-layout-entry-tab entry)))
         (start (point)))
    (appkit-view-insert-label-row row)
    (appkit-ui-make-action-row
     start (point) entry #'disco-root--activate-search-action
     :help-echo (appkit-view-label-row-help-echo row)
     :mouse-face (appkit-view-label-row-mouse-face row))))

(defun disco-root--layout-entry-label-row (entry)
  "Return label row model for renderable root layout ENTRY, or nil."
  (pcase (disco-root-layout-entry-type entry)
    ('section
     (disco-root--section-label-row (disco-root-layout-entry-section entry)
                                    (disco-root-layout-entry-title entry)
                                    (disco-root-layout-entry-count entry)))
    ('guild
     (disco-root--guild-label-row (disco-root-layout-entry-guild entry)
                                  (or (disco-root-layout-entry-unread-count entry) 0)))
    ('search-section
     (disco-root--search-section-label-row (disco-root-layout-entry-title entry)
                                           (or (disco-root-layout-entry-loaded-count entry) 0)
                                           (disco-root-layout-entry-total-count entry)
                                           (disco-root-layout-entry-loading entry)))
    ('search-note
     (disco-root--search-note-label-row (disco-root-layout-entry-text entry)
                                        (disco-root-layout-entry-face entry)))
    ('search-action
     (disco-root--search-action-label-row (disco-root-layout-entry-label entry)
                                          (disco-root-layout-entry-action entry)
                                          (disco-root-layout-entry-tab entry)))
    (_ nil)))

(defun disco-root--insert-layout-entry (entry)
  "Insert one root layout ENTRY into the current buffer."
  (pcase (disco-root-layout-entry-type entry)
    ('search-action
     (disco-root--insert-search-action-line entry))
    (_
     (if-let* ((row (disco-root--layout-entry-label-row entry)))
         (appkit-view-insert-label-row row)
       (pcase (disco-root-layout-entry-type entry)
         ('search-message
          (disco-root--insert-search-message-line
           (disco-root-layout-entry-message entry)
           (or (disco-root-layout-entry-indent entry) 2)
           (disco-root-layout-entry-tab entry)))
         ('text
          (insert (or (disco-root-layout-entry-text entry) "") "\n"))
         ('blank
          (insert "\n"))
         ('channel
          (disco-root--insert-channel-line
           (disco-root-layout-entry-channel entry)
           (or (disco-root-layout-entry-indent entry) 0)
           (or (disco-root-layout-entry-scope entry) 'root)))
         (_
          (error "Unknown root layout entry type: %S"
                 (disco-root-layout-entry-type entry))))))))

(defun disco-root--ewoc-printer (entry)
  "Pretty-printer for one root EWOC ENTRY."
  (disco-root--insert-layout-entry entry))

(defun disco-root--ewoc-insert-text (text)
  "Insert one plain TEXT row in root EWOC."
  (ewoc-enter-last disco-root--ewoc
                   (disco-root--entry-text text)))

(defun disco-root--ewoc-insert-section (section title &optional count)
  "Insert one clickable SECTION row with TITLE and optional COUNT."
  (let ((node (ewoc-enter-last disco-root--ewoc
                               (disco-root--entry-section section title count))))
    (when (hash-table-p disco-root--section-node-table)
      (puthash section node disco-root--section-node-table))
    node))

(defun disco-root--ewoc-insert-guild (guild unread-count)
  "Insert one navigable GUILD row with UNREAD-COUNT badge."
  (let* ((node (ewoc-enter-last disco-root--ewoc
                                (disco-root--entry-guild guild unread-count)))
         (guild-id (alist-get 'id guild)))
    (when (and guild-id
               (hash-table-p disco-root--guild-node-table))
      (puthash guild-id node disco-root--guild-node-table))
    node))

(defun disco-root--ewoc-insert-blank ()
  "Insert one blank row in root EWOC."
  (ewoc-enter-last disco-root--ewoc (disco-root--entry-blank)))

(defun disco-root--ewoc-insert-channel (channel indent &optional scope)
  "Insert CHANNEL row at INDENT into root EWOC and index node by channel ID."
  (let* ((entry (disco-root--entry-channel channel indent (or scope 'root)))
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

(defun disco-root--ewoc-insert-entry (entry)
  "Insert one generic EWOC ENTRY and update node indexes as needed."
  (pcase (disco-root-layout-entry-type entry)
    ('section
     (disco-root--ewoc-insert-section (disco-root-layout-entry-section entry)
                                      (disco-root-layout-entry-title entry)
                                      (disco-root-layout-entry-count entry)))
    ('guild
     (disco-root--ewoc-insert-guild (disco-root-layout-entry-guild entry)
                                    (or (disco-root-layout-entry-unread-count entry) 0)))
    ('channel
     (disco-root--ewoc-insert-channel (disco-root-layout-entry-channel entry)
                                      (or (disco-root-layout-entry-indent entry) 0)
                                      (or (disco-root-layout-entry-scope entry) 'root)))
    ((or 'text 'blank 'search-section 'search-message 'search-note 'search-action)
     (ewoc-enter-last disco-root--ewoc entry))
    (_
     (error "Unknown EWOC entry type: %S"
            (disco-root-layout-entry-type entry)))))

(defun disco-root--guild-by-id (guild-id)
  "Return guild object for GUILD-ID from current state."
  (seq-find (lambda (guild)
              (equal (alist-get 'id guild) guild-id))
            (or (disco-state-guilds) '())))

(defun disco-root--guild-name-by-id (guild-id)
  "Return guild display name for GUILD-ID."
  (let ((guild (disco-root--guild-by-id guild-id)))
    (or (alist-get 'name guild) guild-id "unknown-guild")))

(defun disco-root--thread-parent-candidates ()
  "Return list of (DISPLAY . CHANNEL) suitable for archived thread lookup."
  (let (candidates)
    (dolist (guild (or (disco-state-guilds) '()))
      (let* ((guild-id (alist-get 'id guild))
             (guild-name (or (alist-get 'name guild) guild-id "unknown-guild")))
        (dolist (channel (disco-state-guild-channels guild-id))
          (when (and (disco-root--thread-parent-channel-p channel)
                     (disco-permission-channel-viewable-p channel t))
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
             (has-more (disco-util-json-true-p (alist-get 'has_more resp)))
             (next-before (disco-root--archived-next-before-cursor source-name threads)))
        (when (and has-more (null threads))
          ;; Prevent endless pagination loops when server returns
          ;; an empty page without advancing cursor semantics.
          (setq has-more nil))
        (list :threads threads
              :has-more has-more
              :next-before next-before))
    (error
     (if (disco-permission-error-missing-access-p err)
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

(defun disco-root--channel-list-entries (channels indent scope)
  "Return channel layout entries for CHANNELS at INDENT in SCOPE."
  (mapcar (lambda (channel)
            (disco-root--entry-channel channel indent scope))
          (or channels '())))

(defun disco-root--archived-threads-list-spec ()
  "Return list spec for the current archived-thread buffer."
  (let* ((parent-channel disco-root--archived-parent-channel)
         (threads (or disco-root--archived-threads-cache '()))
         (errors (or disco-root--archived-last-errors '())))
    (appkit-view-list-spec-create
     :title (format "Archived Threads: %s"
                    (disco-root--channel-label parent-channel 'archived-parent))
     :summary (format "Loaded: %d   Sources: %s"
                      (length threads)
                      (disco-root--archived-source-status-string))
     :loading-note (unless (disco-root--archived-any-source-has-more-p)
                     "(no more archived pages)")
     :items (disco-root--channel-list-entries threads 2 'archived-thread)
     :item-inserter #'disco-root--insert-layout-entry
     :empty-text "(no archived threads)"
     :footer-lines (when errors
                     (append (list "Errors:")
                             (mapcar (lambda (err)
                                       (format "  - %s" err))
                                     errors))))))

(defun disco-root--render-archived-threads-buffer ()
  "Render archived-thread buffer from local pagination/cache state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (appkit-view-render-list-spec
     (disco-root--archived-threads-list-spec))
    (goto-char (point-min))))

(defun disco-root--archived-buffer-name (parent-channel)
  "Return archived-thread buffer name for PARENT-CHANNEL."
  (format "*disco:archived:%s (%s)*"
          (or (alist-get 'name parent-channel) "(no-name)")
          (alist-get 'id parent-channel)))

(defun disco-root--channel-inspect-buffer-name (channel)
  "Return inspect buffer name for CHANNEL."
  (format "*disco-channel:%s*"
          (or (disco-root--channel-display-name channel)
              (alist-get 'id channel)
              "unknown")))

(defun disco-root--render-channel-inspect-buffer ()
  "Render the current inspect buffer for `disco-root--inspect-channel'."
  (unless disco-root--inspect-channel
    (user-error "disco: inspect buffer has no channel context"))
  (let* ((channel disco-root--inspect-channel)
         (channel-id (alist-get 'id channel))
         (channel-name (disco-root--channel-display-name channel))
         (guild-name (disco-root--channel-guild-name channel))
         (category-name (disco-root--channel-category-name channel))
         (parent-id (alist-get 'parent_id channel))
         (parent (and parent-id (disco-state-channel parent-id)))
         (parent-name (and parent (disco-root--channel-display-name parent)))
         (open-mode (disco-channel-open-mode channel))
         (inspect-note (disco-channel-inspect-note channel))
         (linked-lobby (alist-get 'linked_lobby channel))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n" (or channel-name "(no-name)")))
    (insert (make-string (length (or channel-name "(no-name)")) ?=))
    (insert "\n\n")
    (insert (format "Type: %s\n" (disco-channel-type-name channel)))
    (insert (format "ID: %s\n" (or channel-id "(unknown)")))
    (when guild-name
      (insert (format "Guild: %s\n" guild-name)))
    (when category-name
      (insert (format "Category: %s\n" category-name)))
    (when parent-name
      (insert (format "Parent: %s\n" parent-name)))
    (when open-mode
      (insert (format "Open mode: %s\n" open-mode)))
    (when-let* ((last-message-id (alist-get 'last_message_id channel)))
      (insert (format "Last message id: %s\n" last-message-id)))
    (when-let* ((position (alist-get 'position channel)))
      (insert (format "Position: %s\n" position)))
    (when (listp linked-lobby)
      (insert "\nLinked lobby:\n")
      (when-let* ((lobby-id (alist-get 'lobby_id linked-lobby)))
        (insert (format "  Lobby ID: %s\n" lobby-id)))
      (when-let* ((linked-by (alist-get 'linked_by linked-lobby)))
        (insert (format "  Linked by: %s\n" linked-by)))
      (when-let* ((linked-at (alist-get 'linked_at linked-lobby)))
        (insert (format "  Linked at: %s\n" linked-at))))
    (when inspect-note
      (insert "\n")
      (insert inspect-note)
      (insert "\n"))
    (insert "\nRaw channel object:\n\n")
    (insert (pp-to-string channel))
    (goto-char (point-min))))

(defun disco-root-channel-inspect-refresh ()
  "Refresh the current inspect buffer from local state."
  (interactive)
  (unless disco-root--inspect-channel
    (user-error "disco: inspect buffer has no channel context"))
  (when-let* ((channel-id (alist-get 'id disco-root--inspect-channel))
              (updated (disco-state-channel channel-id)))
    (setq disco-root--inspect-channel updated))
  (disco-root--render-channel-inspect-buffer)
  (message "disco: refreshed channel inspect"))

(defvar disco-root-channel-inspect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-channel-inspect-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-root-channel-inspect-mode'.")

(define-derived-mode disco-root-channel-inspect-mode special-mode "Disco-Inspect"
  "Major mode for channel inspect buffers."
  (setq buffer-read-only t)
  (setq truncate-lines nil))

(defun disco-root-open-channel-inspect (&optional channel-id)
  "Open inspect view for CHANNEL-ID."
  (interactive)
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (unless channel
      (user-error "disco: channel %s is unavailable" channel-id))
    (let ((buf (get-buffer-create
                (disco-root--channel-inspect-buffer-name channel))))
      (with-current-buffer buf
        (disco-root-channel-inspect-mode)
        (setq disco-root--inspect-channel channel)
        (disco-root--render-channel-inspect-buffer))
      (pop-to-buffer buf))))

(defun disco-root--open-channel (channel-id)
  "Open CHANNEL-ID according to channel semantics."
  (let* ((channel (and channel-id (disco-state-channel channel-id)))
         (open-mode (and channel (disco-channel-open-mode channel))))
    (unless channel
      (user-error "disco: channel %s is unavailable" channel-id))
    (unless open-mode
      (user-error "disco: channel %s does not support opening" channel-id))
    (pcase open-mode
      ('thread-directory
       (disco-channel-directory-open-thread-parent channel-id))
      ('inspect
       (disco-root-open-channel-inspect channel-id))
      (_
       (disco-room-open channel-id (disco-root--channel-display-name channel))))))

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
    (define-key map (kbd "?") #'disco-root-view--transient)
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
      (disco-root-view--attach-live-updates)
      (disco-root-archived-threads-refresh))
    (pop-to-buffer buf)))

(defun disco-root--insert-channel-line (channel indent &optional scope)
  "Insert one CHANNEL at INDENT spaces.

SCOPE is forwarded to extra-info providers."
  (if (memq scope '(activity parent-thread archived-thread))
      (disco-root--insert-activity-channel-line channel indent scope)
    (let* ((channel-id (alist-get 'id channel))
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
           (list 'help-echo (disco-root--channel-open-help-echo channel))))))))

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
  "Open or toggle the actionable row at point.

Search layouts require point to be on the exact action button.  In other
layouts, a non-actionable row falls forward to the next channel row."
  (interactive)
  (if (eq disco-root--layout 'search)
      (if (button-at (point))
          (push-button (point))
        (user-error "disco: no search action at point"))
    (let ((guild-id (disco-root--line-guild-id))
          (channel-id (disco-root--line-channel-id)))
      (cond
       ((disco-root--toggle-node-at-point))
       (guild-id
        (disco-channel-directory-open guild-id))
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
                (disco-root--open-channel
                 (disco-root--line-channel-id next)))
            (user-error "disco: no openable channel at point"))))))))

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
  (if (eq disco-root--layout 'search)
      (when (button-at (point))
        (push-button (point)))
    (disco-root-open-at-point)))

(defun disco-root--clear-ewoc-state ()
  "Clear root EWOC and node indexes for non-EWOC layouts."
  (setq disco-root--channel-node-table (make-hash-table :test #'equal))
  (setq disco-root--section-node-table (make-hash-table :test #'eq))
  (setq disco-root--guild-node-table (make-hash-table :test #'equal))
  (setq disco-root--ewoc nil))

(defun disco-root--prepare-ewoc-state ()
  "Reset root EWOC and node indexes before layout rendering."
  (disco-root--clear-ewoc-state)
  (setq disco-root--ewoc (ewoc-create #'disco-root--ewoc-printer nil nil t)))

(defun disco-root--tree-layout-entries ()
  "Return EWOC entries for the root home layout."
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
         (guilds (or (disco-state-guilds) '()))
         items)
    (disco-root--ensure-section-state
     (append (when show-unread '(unread))
             '(private guilds)))
    (when show-unread
      (push (disco-root--entry-section 'unread "Unread" (length unread-channels))
            items)
      (when (disco-root--section-expanded-p 'unread)
        (if unread-visible
            (dolist (channel unread-visible)
              (push (disco-root--entry-channel channel 2 'activity)
                    items))
          (push (disco-root--entry-text "  No unread channels")
                items))
        (when (> unread-hidden 0)
          (push (disco-root--entry-text
                 (format "  %d more unread channels" unread-hidden))
                items)))
      (push (disco-root--entry-blank) items))
    (push (disco-root--entry-section
           'private "Direct messages" (length private-channels))
          items)
    (when (disco-root--section-expanded-p 'private)
      (if private-channels
          (dolist (channel private-channels)
            (push (disco-root--entry-channel channel 2 'activity)
                  items))
        (push (disco-root--entry-text "  No direct messages")
              items)))
    (push (disco-root--entry-blank) items)
    (push (disco-root--entry-section 'guilds "Servers" (length guilds))
          items)
    (when (disco-root--section-expanded-p 'guilds)
      (if (eq disco-root--view-mode 'dms)
          (push (disco-root--entry-text "  Servers hidden by the DMs lens")
                items)
        (if guilds
            (dolist (guild guilds)
              (push (disco-root--entry-guild
                     guild
                     (disco-root--guild-unread-total
                      (alist-get 'id guild) t))
                    items))
          (push (disco-root--entry-text "  No servers")
                items))))
    (nreverse items)))

(defun disco-root--build-tree-layout-view-spec ()
  "Return view spec for the root home layout."
  (disco-root-layout-ewoc-entry-view-spec-create
   (disco-root--tree-layout-entries)))

(defun disco-root--activity-layout-entries ()
  "Return EWOC layout entries for the activity-sorted channel list layout."
  (let ((channels (disco-root--collect-activity-channels))
        items)
    (if channels
        (dolist (channel channels)
          (push (disco-root--entry-channel channel 2 'activity)
                items))
      (push (disco-root--entry-text "  (no visible channels)")
            items))
    (nreverse items)))

(defun disco-root--build-activity-layout-view-spec ()
  "Return view spec for the activity-sorted channel list layout."
  (disco-root-layout-ewoc-entry-view-spec-create
   (disco-root--activity-layout-entries)))

(defun disco-root--search-layout-entries ()
  "Return root layout ENTRY list for the current root search layout."
  (let (result)
    (dolist (tab disco-root--search-tab-order)
      (let* ((state (disco-root--search-tab-state tab))
             (items (or (plist-get state :items) '()))
             (loading (plist-get state :loading))
             (error (plist-get state :error))
             (cursor (plist-get state :cursor))
             (total (plist-get state :total-results)))
        (push (disco-root--entry-search-section
               tab
               (disco-root--search-tab-label tab)
               (length items)
               total
               loading)
              result)
        (cond
         (items
          (dolist (message items)
            (push (disco-root--entry-search-message message 2 tab)
                  result)))
         (loading
          (push (disco-root--entry-search-note "  (loading...)" 'shadow)
                result))
         (error
          (push (disco-root--entry-search-note (format "  (%s)" error)
                                               'font-lock-warning-face)
                result))
         (t
          (push (disco-root--entry-search-note "  (no results)" 'shadow)
                result)))
        (when cursor
          (push (disco-root--entry-search-action "Show more" 'load-more tab)
                result))
        (push (disco-root--entry-blank) result)))
    (nreverse result)))

(defun disco-root--build-search-layout-list-spec ()
  "Return list spec for the current root search layout."
  (if (not (and disco-root--search-domain
                (disco-root--search-effective-spec-p disco-root--search-query-spec)))
      (appkit-view-list-spec-create
       :title "Search root with s"
       :empty-text "  (no active search)")
    (appkit-view-list-spec-create
     :title (format "Search results in %s"
                    (disco-root--search-domain-label disco-root--search-domain))
     :items (disco-root--search-layout-entries)
     :item-inserter #'disco-root--insert-layout-entry)))

(defun disco-root--build-search-layout-view-spec ()
  "Return view spec for the current root search session layout."
  (disco-root-layout-list-spec-view-spec-create
   (disco-root--build-search-layout-list-spec)))


(provide 'disco-root-view)

;;; disco-root-view.el ends here
