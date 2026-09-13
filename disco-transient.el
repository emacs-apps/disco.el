;;; disco-transient.el --- Transient menus for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root, search, room, message, poll, and input-option menus.  The application
;; entry point loads these menus alongside their business components.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'disco-room)
(require 'disco-root)

(autoload 'org-read-date "org" nil nil)
(declare-function disco-reset-session-state "disco" ())

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

;;;###autoload(autoload 'disco-root-search-transient "disco" nil t)
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
  (disco-reset-session-state))

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

;;;###autoload(autoload 'disco-root-transient "disco" nil t)
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
    ("s" "Search..." disco-root-search)
    ("S" "Advanced search..." disco-root-search-transient)
    ("b" "Exit search" disco-root-search-exit)
    ("U" "Toggle unread lens" disco-root-toggle-unread-lens)]
   ["Inspect"
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Session"
    ("x" "Reset session state" disco-root-menu-reset-session-state)
    ("q" "Quit window" quit-window)]])

(defun disco-room-menu--message-at-point ()
  "Return message at point, suppressing user errors for menu checks."
  (ignore-errors (disco-room--message-at-point)))

;;;###autoload(autoload 'disco-transient-msg-operate "disco" nil t)
(transient-define-prefix disco-transient-msg-operate ()
  "Transient for msg-centric room actions at point."
  [["Message"
    ("c" "Copy dwim" disco-msg-copy-dwim)
    ("l" "Copy link" disco-msg-copy-link)
    ("t" "Copy text" disco-msg-copy-text)
    ("i" "Describe" disco-msg-describe-message)
    ("L" "Redisplay" disco-msg-redisplay)
    ("r" "Reply" disco-msg-reply
     :if-not disco-room--reply-unavailable-reason)
    ("f" "Forward" disco-msg-forward
     :if-not disco-room--forward-unavailable-reason)
    ("e" "Edit" disco-msg-edit
     :if-not (lambda ()
               (disco-room--edit-start-unavailable-reason
                (disco-room-menu--message-at-point))))
    ("d" "Delete" disco-msg-delete
     :if-not (lambda ()
               (disco-room--delete-message-unavailable-reason
                (disco-room-menu--message-at-point))))
    ("P" "Pin / unpin" disco-msg-toggle-pin
     :if-not (lambda ()
               (disco-room--pin-message-unavailable-reason
                (disco-room-menu--message-at-point))))
    ("!" "Add reaction" disco-msg-add-reaction
     :if-not disco-room--reaction-unavailable-reason)
    ("T" "Open thread" disco-msg-open-thread
     :if-not disco-room-thread--open-from-message-unavailable-reason)]
   ["Poll"
    ("p" "Poll actions…" disco-room-poll-transient
     :if disco-room-poll-actionable-at-point-p)]
   ["Media"
    ("o" "Open / play" appkit-media-card-open
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'open)))
    ("D" "Download / retry" appkit-media-card-download
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'download)))
    ("C" "Cancel download" appkit-media-card-cancel-download
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'cancel)))
    ("s" "Save as" appkit-media-card-save-as
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'save-as)))
    ("y" "Copy media URL" appkit-media-card-copy-url
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'copy-url)))]])

;;;###autoload(autoload 'disco-room-transient "disco" nil t)
(transient-define-prefix disco-room-transient ()
  "Room command menu for disco.el."
  [["Timeline"
    ("g" "Refresh room" disco-room-refresh)
    ("o" "Message actions..." disco-transient-msg-operate
     :if disco-room-menu--message-at-point)
    ("c" "Send message" disco-room-send-message
     :if-not disco-room--send-message-unavailable-reason)
    ("f" "Attach file" disco-room-attach-file
     :if-not disco-room--attach-unavailable-reason)
    ("D" "Remove attachment" disco-room-remove-attachment-token-at-point
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("x" "Clear attachments" disco-room-clear-attachments
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("v" "List attachments" disco-room-list-attachments
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("V" "Edit attachment desc" disco-room-edit-attachment-description
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("S" "Toggle attachment spoiler" disco-room-toggle-attachment-spoiler
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("O" "Reorder attachments" disco-room-reorder-attachments
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 2)))
    ("k" "Cancel reply/edit" disco-room-cancel-reply
     :if disco-room--composer-aux-active-p)
    ("p" "Send poll" disco-room-send-poll
     :if-not disco-room--poll-unavailable-reason)
    ("i" "Send Sticker" disco-room-send-sticker
     :if-not disco-room--sticker-unavailable-reason)
    ("B" "Browse pinned msgs" disco-room-list-pinned-messages)
    ("P" "Ack pinned msgs" disco-room-ack-channel-pins)]
   ["Thread"
    ("m" "Create from message" disco-room-thread-create-from-message
     :if-not disco-room-thread--create-from-message-unavailable-reason)
    ("n" "Create detached" disco-room-thread-create
     :if-not (lambda ()
               (disco-room-thread--create-unavailable-reason :any)))
    ("R" "Rename thread" disco-room-thread-rename
     :if-not disco-room-thread--update-unavailable-reason)
    ("L" "Toggle locked" disco-room-thread-toggle-locked
     :if-not disco-room-thread--update-unavailable-reason)
    ("S" "Set slowmode" disco-room-thread-set-slowmode
     :if-not disco-room-thread--update-unavailable-reason)
    ("U" "Set auto-archive" disco-room-thread-set-auto-archive-duration
     :if-not disco-room-thread--update-unavailable-reason)
    ("E" "Edit thread settings" disco-room-thread-edit-settings
     :if-not disco-room-thread--update-unavailable-reason)
    ("M" "Set muted" disco-room-thread-set-muted
     :if-not disco-room-thread--mute-unavailable-reason)
    ("j" "Join thread" disco-room-thread-join
     :if-not disco-room-thread--join-unavailable-reason)
    ("l" "Leave thread" disco-room-thread-leave
     :if-not disco-room-thread--leave-unavailable-reason)
    ("a" "Toggle archived" disco-room-thread-toggle-archived
     :if-not disco-room-thread--toggle-archived-unavailable-reason)
    ("A" "Parent archived threads..." disco-room-thread-open-parent-archived
     :if (lambda ()
           (alist-get 'parent_id (disco-room--channel-object))))]
   ["Inspect"
    ("/" "Structured search..." disco-room-search-channel)
    ("f" "Filter search" disco-room-filter-search)
    ("F" "Cancel filter" disco-room-filter-cancel)
    ("v" "Refetch avatars" disco-avatar-refetch)
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Window"
    ("q" "Quit window" quit-window)]])

;;;###autoload(autoload 'disco-room-input-options-transient "disco" nil t)
(transient-define-prefix disco-room-input-options-transient ()
  "Transient for telega-like room input options."
  [["Input Options"
    ("RET" "Toggle RET send/editor" disco-room-toggle-send-on-return)
    ("l" "Cycle long-message action" disco-room-cycle-long-message-action)
    ("m" "Cycle allowed mentions" disco-room-cycle-allowed-mentions)
    ("r" "Toggle reply mention" disco-room-toggle-reply-mention-replied-user)
    ("0" "Reset room-local options" disco-room-reset-input-options)]])

(defun disco-room-poll-actionable-at-point-p ()
  "Return non-nil when point is on a poll with an available action."
  (let ((message (disco-room-menu--message-at-point)))
    (and (disco-msg-poll message)
         (or (not (disco-room--poll-vote-unavailable-reason message))
             (not (disco-room--poll-expire-unavailable-reason message))))))

;;;###autoload(autoload 'disco-room-poll-transient "disco" nil t)
(transient-define-prefix disco-room-poll-transient ()
  "Transient for the poll at point."
  :refresh-suffixes t
  [["Vote"
    :if-not disco-room--poll-vote-unavailable-reason
    ("t" "Toggle answer" disco-room-toggle-poll-answer :transient t)
    ("s" "Submit staged vote" disco-room-submit-poll-vote
     :if-not disco-room--poll-submit-unavailable-reason)]
   ["Manage"
    ("c" "Remove my vote" disco-room-clear-poll-votes
     :if-not disco-room--poll-clear-unavailable-reason)
    ("x" "End poll" disco-room-expire-poll
     :if-not disco-room--poll-expire-unavailable-reason)]]
  (interactive)
  (unless (disco-msg-poll (disco-room-menu--message-at-point))
    (user-error "disco: point is not on a poll"))
  (transient-setup 'disco-room-poll-transient))

(provide 'disco-transient)

;;; disco-transient.el ends here
