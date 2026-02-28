;;; disco-root.el --- Root buffer for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root dashboard showing guilds and channels, inspired by telega/ement
;; list-driven navigation style.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-room)
(require 'disco-state)
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
  "Hash table mapping channel IDs to root EWOC nodes.")

(defun disco-root--live-event-p (event-type)
  "Return non-nil when EVENT-TYPE should trigger root rerender."
  (memq event-type
        '(message-create message-ack
          channel-create channel-update channel-delete
          guild-create guild-update guild-delete
          thread-create thread-update thread-delete thread-list-sync)))

(defun disco-root--render-preserving-position ()
  "Render root tree and keep point near previous line/column."
  (let ((line (line-number-at-pos))
        (col (current-column)))
    (disco-root-render)
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (move-to-column col)))

(defun disco-root--refresh-channel-node (channel-id)
  "Refresh one CHANNEL-ID row in EWOC, returning non-nil on success."
  (let ((node (and channel-id
                   disco-root--channel-node-table
                   (gethash channel-id disco-root--channel-node-table))))
    (when (and node disco-root--ewoc)
      (let ((channel (disco-state-channel channel-id)))
        (when (and channel (disco-root--displayable-channel-p channel))
          (let ((inhibit-read-only t)
                (entry (copy-sequence (ewoc-data node))))
            (setq entry (plist-put entry :channel channel))
            (ewoc-set-data node entry)
            (ewoc-invalidate disco-root--ewoc node)
            t))))))

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

(defconst disco-root--permission-view-channel-bit (ash 1 10)
  "Permission bit mask for VIEW_CHANNEL.")

(defconst disco-root--permission-manage-threads-bit (ash 1 34)
  "Permission bit mask for MANAGE_THREADS.")

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
  (let ((channel-type (alist-get 'type channel))
        (guild-id (alist-get 'guild_id channel))
        (permissions (alist-get 'permissions channel)))
    (cond
     ((memq channel-type '(1 3))
      t)
     ((null guild-id)
      t)
     ((null permissions)
      t)
     (t
      (let ((bits (disco-root--parse-decimal-integer permissions)))
        (if (integerp bits)
            (not (zerop (logand bits disco-root--permission-view-channel-bit)))
          t))))))

(defun disco-root--channel-has-permission-p (channel permission-bit)
  "Return non-nil when CHANNEL has PERMISSION-BIT in computed permissions.

If permissions are missing or unparsable, return t to avoid false negatives."
  (let ((permissions (alist-get 'permissions channel)))
    (if (null permissions)
        t
      (let ((bits (disco-root--parse-decimal-integer permissions)))
        (if (integerp bits)
            (not (zerop (logand bits permission-bit)))
          t)))))

(defun disco-root--archived-source-fetch-allowed-p (source-name parent-channel)
  "Return non-nil when archived SOURCE-NAME is expected to be fetchable.

This prevents noisy permission errors for sources that require elevated access."
  (cond
   ;; Discord private archived thread listing requires MANAGE_THREADS.
   ((equal source-name "private")
    (disco-root--channel-has-permission-p
     parent-channel
     disco-root--permission-manage-threads-bit))
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
  (memq (alist-get 'type channel) '(0 5 15 16)))

(defun disco-root--forum-or-media-channel-p (channel)
  "Return non-nil when CHANNEL is a forum/media parent channel."
  (memq (alist-get 'type channel) '(15 16)))

(defun disco-root--thread-metadata (channel)
  "Return thread metadata for CHANNEL."
  (or (alist-get 'thread_metadata channel) '()))

(defun disco-root--thread-status-tags (thread)
  "Return comma-joined status tags for THREAD."
  (let* ((meta (disco-root--thread-metadata thread))
         (thread-type (alist-get 'type thread))
         tags)
    (when (or (disco-root--json-true-p (alist-get 'archived meta))
              (disco-root--json-true-p (alist-get 'archived thread)))
      (push "archived" tags))
    (when (or (disco-root--json-true-p (alist-get 'locked meta))
              (disco-root--json-true-p (alist-get 'locked thread)))
      (push "locked" tags))
    (when (= thread-type 12)
      (push "private" tags))
    (mapconcat #'identity (nreverse tags) ", ")))

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

(defun disco-root--thread-count-under-parent (channel)
  "Return number of indexed threads under CHANNEL."
  (length (disco-state-parent-threads (alist-get 'id channel))))

(defun disco-root--channel-label (channel)
  "Return display label for CHANNEL."
  (let ((name (disco-root--channel-display-name channel))
        (channel-type (alist-get 'type channel))
        (channel-id (alist-get 'id channel))
        (unread (disco-state-channel-unread-count (alist-get 'id channel))))
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
  (> (disco-state-channel-unread-count (alist-get 'id channel)) 0))

(defun disco-root--parent-has-unread-thread-p (channel)
  "Return non-nil when CHANNEL has at least one unread thread child."
  (let ((parent-id (alist-get 'id channel)))
    (seq-some #'disco-root--channel-has-unread-p
              (disco-state-parent-threads parent-id))))

(defun disco-root--channel-visible-in-view-p (channel)
  "Return non-nil when CHANNEL should appear under current view mode."
  (pcase disco-root--view-mode
    ('unread
     (or (disco-root--channel-has-unread-p channel)
         (and (disco-root--thread-parent-channel-p channel)
              (disco-root--parent-has-unread-thread-p channel))))
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

(defun disco-root--ewoc-insert-blank ()
  "Insert one blank row in root EWOC."
  (ewoc-enter-last disco-root--ewoc (list :entry-type 'blank)))

(defun disco-root--ewoc-insert-channel (channel indent)
  "Insert CHANNEL row at INDENT into root EWOC and index node by channel ID."
  (let* ((entry (list :entry-type 'channel :channel channel :indent indent))
         (node (ewoc-enter-last disco-root--ewoc entry))
         (channel-id (alist-get 'id channel)))
    (when channel-id
      (puthash channel-id node disco-root--channel-node-table))
    node))

(defun disco-root--insert-private-channels ()
  "Insert private-channel (DM/group DM) section into root buffer."
  (let ((channels (seq-filter #'disco-root--channel-visible-in-view-p
                              (disco-root--private-channels-sorted))))
    (disco-root--ewoc-insert-text "Direct Messages")
    (if channels
        (dolist (channel channels)
          (disco-root--ewoc-insert-channel channel 4))
      (disco-root--ewoc-insert-text "    (no private channels loaded)"))
    (disco-root--ewoc-insert-blank)))

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
    (insert (format "Archived Threads: %s\n"
                    (disco-root--channel-label parent-channel)))
    (insert "g: refresh   n: next page   RET/mouse-1: open thread   q: quit\n")
    (insert (format "Loaded: %d   Sources: %s\n"
                    (length threads)
                    (disco-root--archived-source-status-string)))
    (unless (disco-root--archived-any-source-has-more-p)
      (insert "(no more archived pages)\n"))
    (insert "\n")
    (if threads
        (dolist (thread threads)
          (disco-root--insert-channel-line thread 2))
      (insert "(no archived threads)\n"))
    (when errors
      (insert "\nErrors:\n")
      (dolist (err errors)
        (insert (format "  - %s\n" err))))
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
    (insert (format "Threads: %s\n"
                    (if parent-channel
                        (disco-root--channel-label parent-channel)
                      "(no parent)")))
    (insert "g: refresh active   A: archived threads   RET/mouse-1: open thread   n/p/TAB: nav   q: quit\n")
    (insert (format "Active threads indexed: %d\n"
                    (length (or threads '()))))
    (when disco-root--parent-threads-refresh-in-flight
      (insert "[refreshing active threads...]\n"))
    (insert "\n")
    (if threads
        (dolist (thread threads)
          (disco-root--insert-channel-line thread 2))
      (insert "(no active threads indexed)\n"))
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
        (unread-count (disco-state-channel-unread-count (alist-get 'id channel)))
        (padding (make-string indent ?\s)))
    (if (disco-root--openable-channel-p channel)
        (insert-text-button
         (format "%s%s\n" padding label)
         'action (lambda (_)
                   (disco-root--open-channel channel-id))
         'disco-channel-id channel-id
         'disco-unread-count unread-count
         'follow-link t
         'help-echo (if (memq channel-type '(15 16))
                        (format "Open threads under channel %s" channel-id)
                      (format "Open channel %s" channel-id)))
      (insert (format "%s%s\n" padding label)))))

(defun disco-root-button-forward (&optional n)
  "Move point to next channel button by N steps."
  (interactive "p")
  (condition-case _
      (forward-button (or n 1) t)
    (error
     (message "disco: no next channel"))))

(defun disco-root-button-backward (&optional n)
  "Move point to previous channel button by N steps."
  (interactive "p")
  (condition-case _
      (backward-button (or n 1) t)
    (error
     (message "disco: no previous channel"))))

(defun disco-root-open-at-point ()
  "Open channel button at point.

If point is not on a button, jump to the next button and open it."
  (interactive)
  (let ((button (button-at (point))))
    (unless button
      (condition-case _
          (progn
            (forward-button 1 t)
            (setq button (button-at (point))))
        (error
         (setq button nil))))
    (if button
        (push-button (button-start button))
      (user-error "disco: no openable channel at point"))))

(defun disco-root--next-button-matching (predicate start limit)
  "Return next button from START before LIMIT satisfying PREDICATE."
  (let ((pos start)
        found
        next)
    (while (and (not found)
                (setq next (next-button pos))
                (< (button-start next) limit))
      (if (funcall predicate next)
          (setq found next)
        (setq pos (button-end next))))
    found))

(defun disco-root-next-unread ()
  "Jump to next channel button with unread count > 0."
  (interactive)
  (let* ((origin (point))
         (current-button (button-at origin))
         (scan-start (if current-button
                         (button-end current-button)
                       origin))
         (predicate (lambda (button)
                      (> (or (button-get button 'disco-unread-count) 0) 0)))
         (found (or (disco-root--next-button-matching predicate scan-start (point-max))
                    (disco-root--next-button-matching predicate (point-min) origin))))
    (if found
        (goto-char (button-start found))
      (message "disco: no unread channels"))))

(defun disco-root--insert-parent-threads (parent-channel rendered-thread-ids)
  "Insert threads under PARENT-CHANNEL and mark IDs in RENDERED-THREAD-IDS."
  (let ((parent-id (alist-get 'id parent-channel)))
    (dolist (thread (disco-state-parent-threads parent-id))
      (let ((thread-id (alist-get 'id thread)))
        (when (and thread-id
                   (disco-root--displayable-channel-p thread)
                   (disco-root--channel-visible-in-view-p thread))
          (puthash thread-id t rendered-thread-ids)
          (disco-root--ewoc-insert-channel thread 8))))))

(defun disco-root--guild-visible-parent-channels (guild-id)
  "Return non-thread display channels for GUILD-ID."
  (let (parents)
    (dolist (channel (or (disco-state-guild-channels guild-id) '()))
      (when (and (disco-root--displayable-channel-p channel)
                 (disco-root--channel-visible-in-view-p channel)
                 (not (disco-state-channel-thread-p channel)))
        (push channel parents)))
    (disco-root--sort-channels (nreverse parents))))

(defun disco-root--insert-guild (guild)
  "Insert one GUILD and its channels in root buffer."
  (let* ((guild-id (alist-get 'id guild))
         (guild-name (or (alist-get 'name guild) "(unnamed-guild)"))
         (parents (disco-root--guild-visible-parent-channels guild-id))
         (rendered-thread-ids (make-hash-table :test #'equal)))
    (disco-root--ewoc-insert-text guild-name)
    (if parents
        (dolist (channel parents)
          (disco-root--ewoc-insert-channel channel 4)
          (when (disco-root--thread-parent-channel-p channel)
            (disco-root--insert-parent-threads channel rendered-thread-ids)))
      (disco-root--ewoc-insert-text "    (no channels loaded)"))

    (let (orphan-threads)
      (dolist (thread (disco-state-guild-threads guild-id))
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
          (disco-root--ewoc-insert-channel thread 8))))

    (disco-root--ewoc-insert-blank)))

(defun disco-root-render ()
  "Render root dashboard from in-memory state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "disco.el\n")
    (insert (format "g: refresh   A: archived threads   \\: sort(%s)   v: view(%s)   RET/mouse-1: open (forum/media -> threads)   n/p/TAB: nav   u: next unread   q: quit"
                    disco-root--sort-mode
                    disco-root--view-mode))
    (when disco-root--refresh-in-flight
      (insert "   [refreshing...]"))
    (insert "\n\n")
    (setq disco-root--channel-node-table (make-hash-table :test #'equal))
    (setq disco-root--ewoc (ewoc-create #'disco-root--ewoc-printer nil nil t))
    (disco-root--insert-private-channels)
    (if (eq disco-root--view-mode 'dms)
        (disco-root--ewoc-insert-text "(guild tree hidden in dms view)")
      (let ((guilds (or (disco-state-guilds) '())))
        (if guilds
            (dolist (guild guilds)
              (disco-root--insert-guild guild))
          (disco-root--ewoc-insert-text "No guilds loaded. Press g to refresh."))))
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
                  (disco-state-put-channels guild-id channels))
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
                      (disco-state-upsert-channel thread)))
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

           (inc-pending)
           (disco-api-user-private-channels-async
            :on-success
            (lambda (private-channels)
              (when (callback-active-p)
                (disco-state-set-private-channels private-channels))
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
         (dec-pending))))))

(defvar disco-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-refresh)
    (define-key map (kbd "A") #'disco-root-list-archived-threads)
    (define-key map (kbd "\\") #'disco-root-toggle-sort-mode)
    (define-key map (kbd "v") #'disco-root-cycle-view-mode)
    (define-key map (kbd "RET") #'disco-root-open-at-point)
    (define-key map (kbd "n") #'disco-root-button-forward)
    (define-key map (kbd "p") #'disco-root-button-backward)
    (define-key map (kbd "TAB") #'disco-root-button-forward)
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
  (setq-local disco-root--channel-node-table (make-hash-table :test #'equal)))

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
