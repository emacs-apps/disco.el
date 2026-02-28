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
(require 'disco-room)
(require 'disco-state)
(require 'disco-transient)

(defconst disco-root-buffer-name "*disco*"
  "Main root buffer name.")

(defvar-local disco-root--archived-parent-channel nil
  "Parent channel object for archived thread list buffers.")

(defun disco-root--json-true-p (value)
  "Return non-nil when VALUE semantically represents JSON true."
  (or (eq value t)
      (eq value 'true)
      (equal value "true")))

(defun disco-root--displayable-channel-p (channel)
  "Return non-nil when CHANNEL should appear in root buffer."
  (memq (alist-get 'type channel) '(0 5 10 11 12 15 16)))

(defun disco-root--openable-channel-p (channel)
  "Return non-nil when CHANNEL can be opened as a room timeline."
  (memq (alist-get 'type channel) '(0 5 10 11 12)))

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

(defun disco-root--thread-count-under-parent (channel)
  "Return number of indexed threads under CHANNEL."
  (length (disco-state-parent-threads (alist-get 'id channel))))

(defun disco-root--channel-label (channel)
  "Return display label for CHANNEL."
  (let ((name (or (alist-get 'name channel) "(no-name)"))
        (channel-type (alist-get 'type channel)))
    (pcase channel-type
      ((or 10 11 12)
       (let ((tags (disco-root--thread-status-tags channel)))
         (format "[thread] %s%s"
                 name
                 (if (string-empty-p tags)
                     ""
                   (format " (%s)" tags)))))
      ((or 0 5 15 16)
       (let* ((thread-count (disco-root--thread-count-under-parent channel))
              (suffix (if (> thread-count 0)
                          (format " (%d threads)" thread-count)
                        "")))
         (pcase channel-type
           ((or 0 5) (format "#%s%s" name suffix))
           (15 (format "[forum] %s%s" name suffix))
           (16 (format "[media] %s%s" name suffix)))))
      (_ (format "[type-%s] %s" channel-type name)))))

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

(defun disco-root--fetch-archived-threads (parent-channel-id)
  "Fetch archived thread lists for PARENT-CHANNEL-ID.

Return plist with keys :threads and :errors."
  (let ((sources
         `(("public" . ,#'disco-api-channel-archived-public-threads)
           ("private" . ,#'disco-api-channel-archived-private-threads)
           ("joined-private" . ,#'disco-api-channel-joined-private-archived-threads)))
        all-threads
        errors)
    (dolist (source sources)
      (let ((source-name (car source))
            (source-fn (cdr source)))
        (condition-case err
            (let* ((resp (funcall source-fn parent-channel-id nil disco-thread-archive-fetch-limit))
                   (threads (or (alist-get 'threads resp) '())))
              (setq all-threads (append all-threads threads)))
          (error
           (push (format "%s: %s" source-name (error-message-string err))
                 errors)))))
    (list :threads (disco-root--sort-threads-by-archive-time
                    (disco-root--dedupe-threads all-threads))
          :errors (nreverse errors))))

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
           (result (disco-root--fetch-archived-threads parent-id))
           (threads (plist-get result :threads))
           (errors (plist-get result :errors))
           (inhibit-read-only t))
      (dolist (thread threads)
        (disco-state-upsert-channel thread))
      (erase-buffer)
      (insert (format "Archived Threads: %s\n"
                      (disco-root--channel-label parent-channel)))
      (insert "g: refresh   RET/mouse-1: open thread   q: quit\n\n")
      (if threads
          (dolist (thread threads)
            (disco-root--insert-channel-line thread 2))
        (insert "(no archived threads)\n"))
      (when errors
        (insert "\nErrors:\n")
        (dolist (err errors)
          (insert (format "  - %s\n" err))))
      (goto-char (point-min))
      (message "disco: loaded %d archived threads" (length threads)))))

(defvar disco-root-archived-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-root-archived-threads-refresh)
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
        (channel-name (or (alist-get 'name channel) "(no-name)"))
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
         (guild-count (length guilds))
         (channel-count 0))
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
      (message "disco: loaded %d guilds, %d channels (%d threads)"
               guild-count channel-count thread-count))))

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
      (disco-root-render))
    (pop-to-buffer buf)))

(provide 'disco-root)

;;; disco-root.el ends here
