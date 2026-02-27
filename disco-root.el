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

(defun disco-root--insert-channel-button (channel)
  "Insert clickable line for CHANNEL alist."
  (let ((channel-id (alist-get 'id channel))
        (channel-name (or (alist-get 'name channel) "(no-name)"))
        (channel-type (alist-get 'type channel)))
    ;; Type 0 (text) and 5 (news) are message-centric for MVP.
    (when (member channel-type '(0 5))
      (insert-text-button
       (format "    #%s\n" channel-name)
       'action (lambda (_)
                 (disco-room-open channel-id channel-name))
       'follow-link t
       'help-echo (format "Open channel %s" channel-id)))))

(defun disco-root--insert-guild (guild)
  "Insert one GUILD and its channels in root buffer."
  (let* ((guild-id (alist-get 'id guild))
         (guild-name (or (alist-get 'name guild) "(unnamed-guild)"))
         (channels (or (disco-state-guild-channels guild-id) '())))
    (insert (format "%s\n" guild-name))
    (if channels
        (dolist (channel channels)
          (disco-root--insert-channel-button channel))
      (insert "    (no channels loaded)\n"))
    (insert "\n")))

(defun disco-root-render ()
  "Render root dashboard from in-memory state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "disco.el\n")
    (insert "g: refresh   RET/mouse-1: open channel   q: quit\n\n")
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
        (disco-state-put-channels guild-id channels)))
    (disco-root-render)
    (message "disco: loaded %d guilds and %d channels" guild-count channel-count)))

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
