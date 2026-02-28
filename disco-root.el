;;; disco-root.el --- Root buffer for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root dashboard showing guilds and channels, inspired by telega/ement
;; list-driven navigation style.

;;; Code:

(require 'button)
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

(defun disco-root--live-event-p (event-type)
  "Return non-nil when EVENT-TYPE should trigger root rerender."
  (memq event-type
        '(message-create message-ack
          channel-create channel-update channel-delete
          guild-create guild-update guild-delete
          thread-create thread-update thread-delete thread-list-sync)))

(defun disco-root--handle-gateway-event (event)
  "Apply one gateway EVENT to root buffer view."
  (when (disco-root--live-event-p (plist-get event :type))
    (let ((line (line-number-at-pos))
          (col (current-column)))
      (disco-root-render)
      (goto-char (point-min))
      (forward-line (max 0 (1- line)))
      (move-to-column col))))

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

(defun disco-root--displayable-channel-p (channel)
  "Return non-nil when CHANNEL should appear in root buffer."
  (memq (alist-get 'type channel) '(0 1 3 5 10 11 12 15 16)))

(defun disco-root--openable-channel-p (channel)
  "Return non-nil when CHANNEL can be opened as a room timeline."
  (memq (alist-get 'type channel) '(0 1 3 5 10 11 12)))

(defun disco-root--thread-parent-channel-p (channel)
  "Return non-nil when CHANNEL can contain visible threads in UI."
  (memq (alist-get 'type channel) '(0 5 15 16)))

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

(defun disco-root--insert-private-channels ()
  "Insert private-channel (DM/group DM) section into root buffer."
  (let ((channels (disco-root--private-channels-sorted)))
    (insert "Direct Messages\n")
    (if channels
        (dolist (channel channels)
          (disco-root--insert-channel-line channel 4))
      (insert "    (no private channels loaded)\n"))
    (insert "\n")))

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
          (when (disco-root--thread-parent-channel-p channel)
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

(defun disco-root--fetch-archived-threads-page (parent-channel-id &optional reset)
  "Fetch one archived thread page for PARENT-CHANNEL-ID.

When RESET is non-nil, start pagination from first page for all sources.
Return plist with keys :threads and :errors for this page only."
  (when reset
    (disco-root--reset-archived-pagination-state))
  (let (page-threads errors)
    (dolist (source disco-root--archived-thread-sources)
      (let* ((source-name (car source))
             (source-fn (cdr source))
             (should-fetch (or reset
                               (alist-get source-name
                                          disco-root--archived-source-has-more
                                          nil nil #'equal))))
        (when should-fetch
          (let ((before (alist-get source-name disco-root--archived-before-cursors nil nil #'equal)))
            (condition-case err
                (let* ((resp (funcall source-fn parent-channel-id before disco-thread-archive-fetch-limit))
                       (threads (or (alist-get 'threads resp) '()))
                       (has-more (disco-root--json-true-p (alist-get 'has_more resp)))
                       (next-before (disco-root--archived-next-before-cursor source-name threads)))
                  (when (and has-more (null threads))
                    ;; Prevent endless pagination loops when server returns
                    ;; an empty page without advancing cursor semantics.
                    (setq has-more nil))
                  (setf (alist-get source-name disco-root--archived-source-has-more nil nil #'equal)
                        has-more)
                  (when next-before
                    (setf (alist-get source-name disco-root--archived-before-cursors nil nil #'equal)
                          next-before))
                  (setq page-threads (append page-threads threads)))
              (error
               ;; Keep source marked as has-more so temporary failures can be retried.
               (setf (alist-get source-name disco-root--archived-source-has-more nil nil #'equal) t)
               (push (format "%s: %s" source-name (error-message-string err))
                     errors)))))))
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
        (channel-name (disco-root--channel-display-name channel))
        (label (disco-root--channel-label channel))
        (padding (make-string indent ?\s)))
    (if (disco-root--openable-channel-p channel)
        (insert-text-button
         (format "%s%s\n" padding label)
         'action (lambda (_)
                   (disco-room-open channel-id channel-name))
         'follow-link t
         'help-echo (format "Open channel %s" channel-id))
      (insert (format "%s%s\n" padding label)))))

(defun disco-root--insert-parent-threads (parent-channel rendered-thread-ids)
  "Insert threads under PARENT-CHANNEL and mark IDs in RENDERED-THREAD-IDS."
  (let ((parent-id (alist-get 'id parent-channel)))
    (dolist (thread (disco-state-parent-threads parent-id))
      (let ((thread-id (alist-get 'id thread)))
        (when (and thread-id (disco-root--displayable-channel-p thread))
          (puthash thread-id t rendered-thread-ids)
          (disco-root--insert-channel-line thread 8))))))

(defun disco-root--guild-visible-parent-channels (guild-id)
  "Return non-thread display channels for GUILD-ID."
  (let (parents)
    (dolist (channel (or (disco-state-guild-channels guild-id) '()))
      (when (and (disco-root--displayable-channel-p channel)
                 (not (disco-state-channel-thread-p channel)))
        (push channel parents)))
    (nreverse parents)))

(defun disco-root--insert-guild (guild)
  "Insert one GUILD and its channels in root buffer."
  (let* ((guild-id (alist-get 'id guild))
         (guild-name (or (alist-get 'name guild) "(unnamed-guild)"))
         (parents (disco-root--guild-visible-parent-channels guild-id))
         (rendered-thread-ids (make-hash-table :test #'equal)))
    (insert (format "%s\n" guild-name))
    (if parents
        (dolist (channel parents)
          (disco-root--insert-channel-line channel 4)
          (when (disco-root--thread-parent-channel-p channel)
            (disco-root--insert-parent-threads channel rendered-thread-ids)))
      (insert "    (no channels loaded)\n"))

    (let (orphan-threads)
      (dolist (thread (disco-state-guild-threads guild-id))
        (let ((thread-id (alist-get 'id thread)))
          (when (and thread-id (not (gethash thread-id rendered-thread-ids)))
            (push thread orphan-threads))))
      (setq orphan-threads (nreverse orphan-threads))
      (when orphan-threads
        (insert "    [threads]\n")
        (dolist (thread orphan-threads)
          (disco-root--insert-channel-line thread 8))))

    (insert "\n")))

(defun disco-root-render ()
  "Render root dashboard from in-memory state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "disco.el\n")
    (insert "g: refresh   A: archived threads   RET/mouse-1: open channel/thread   q: quit\n\n")
    (disco-root--insert-private-channels)
    (let ((guilds (or (disco-state-guilds) '())))
      (if guilds
          (dolist (guild guilds)
            (disco-root--insert-guild guild))
        (insert "No guilds loaded. Press g to refresh.\n")))
    (goto-char (point-min))))

(defun disco-root-refresh ()
  "Fetch guild/channel data and redraw root buffer."
  (interactive)
  (let* ((guilds (disco-api-user-guilds))
         (private-channels nil)
         (private-channels-fetched nil)
         (guild-count (length guilds))
         (channel-count 0))
    (condition-case err
        (progn
          (setq private-channels (disco-api-user-private-channels))
          (setq private-channels-fetched t))
      (error
       (message "disco: private-channel fetch failed: %s"
                (error-message-string err))))
    (when private-channels-fetched
      (disco-state-set-private-channels private-channels))
    (disco-state-set-guilds guilds)
    (dolist (guild guilds)
      (let* ((guild-id (alist-get 'id guild))
             (channels (disco-api-guild-channels guild-id)))
        (setq channel-count (+ channel-count (length channels)))
        (disco-state-put-channels guild-id channels)

        (when disco-fetch-guild-active-threads
          (condition-case err
              (let ((active (disco-api-guild-active-threads guild-id)))
                (dolist (thread (or (alist-get 'threads active) '()))
                  (disco-state-upsert-channel thread)))
            (error
             (message "disco: active thread fetch failed for guild %s: %s"
                      guild-id (error-message-string err)))))))

    (let ((thread-count 0))
      (dolist (guild guilds)
        (let ((guild-id (alist-get 'id guild)))
          (setq thread-count (+ thread-count
                                (length (disco-state-guild-threads guild-id))))))
      (disco-root-render)
      (message "disco: loaded %d guilds, %d channels (%d threads), %d DMs"
               guild-count
               channel-count
               thread-count
               (length (disco-state-private-channels))))))

(defvar disco-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-refresh)
    (define-key map (kbd "A") #'disco-root-list-archived-threads)
    (define-key map (kbd "?") #'disco-root-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-root-mode'.")

(define-derived-mode disco-root-mode special-mode "Disco-Root"
  "Major mode for disco.el root buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

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
