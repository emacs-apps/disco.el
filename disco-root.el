;;; disco-root.el --- Root buffer for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Root dashboard showing guilds and channels, inspired by telega/ement
;; list-driven navigation style.

;;; Code:

(require 'button)
(require 'subr-x)
(require 'disco-api)
(require 'disco-room)
(require 'disco-state)

(defconst disco-root-buffer-name "*disco*"
  "Main root buffer name.")

(defun disco-root--displayable-channel-p (channel)
  "Return non-nil when CHANNEL should appear in root buffer."
  (memq (alist-get 'type channel) '(0 5 10 11 12 15 16)))

(defun disco-root--openable-channel-p (channel)
  "Return non-nil when CHANNEL can be opened as a room timeline."
  (memq (alist-get 'type channel) '(0 5 10 11 12)))

(defun disco-root--thread-parent-channel-p (channel)
  "Return non-nil when CHANNEL can contain visible threads in UI."
  (memq (alist-get 'type channel) '(0 5 15 16)))

(defun disco-root--channel-label (channel)
  "Return display label for CHANNEL."
  (let ((name (or (alist-get 'name channel) "(no-name)"))
        (channel-type (alist-get 'type channel)))
    (pcase channel-type
      ((or 10 11 12) (format "[thread] %s" name))
      (15 (format "[forum] %s" name))
      (16 (format "[media] %s" name))
      ((or 0 5) (format "#%s" name))
      (_ (format "[type-%s] %s" channel-type name)))))

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
    (insert "g: refresh   RET/mouse-1: open channel/thread   q: quit\n\n")
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
