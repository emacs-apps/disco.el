;;; disco-transient.el --- Transient command menus for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Central command menus inspired by ement's transient-driven room workflow.

;;; Code:

(require 'transient)

(require 'disco-api)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-http)
(require 'disco-state)

(declare-function disco-root-refresh "disco-root")
(declare-function disco-root-list-archived-threads "disco-root")

(declare-function disco-room-refresh "disco-room")
(declare-function disco-room-load-older-messages "disco-room")
(declare-function disco-room-send-message "disco-room")
(declare-function disco-room-attach-file "disco-room")
(declare-function disco-room-remove-attachment-token-at-point "disco-room")
(declare-function disco-room-clear-attachments "disco-room")
(declare-function disco-room-list-attachments "disco-room")
(declare-function disco-room-edit-attachment-description "disco-room")
(declare-function disco-room-reorder-attachments "disco-room")
(declare-function disco-room-reply-to-message "disco-room")
(declare-function disco-room-cancel-reply "disco-room")
(declare-function disco-room-edit-message "disco-room")
(declare-function disco-room-delete-message "disco-room")
(declare-function disco-room-toggle-reaction "disco-room")
(declare-function disco-room-add-reaction "disco-room")
(declare-function disco-room-remove-reaction "disco-room")
(declare-function disco-room-create-thread-from-message "disco-room")
(declare-function disco-room-open-thread-from-message-at-point "disco-room")
(declare-function disco-room-create-thread "disco-room")
(declare-function disco-room-rename-thread "disco-room")
(declare-function disco-room-toggle-thread-locked "disco-room")
(declare-function disco-room-set-thread-slowmode "disco-room")
(declare-function disco-room-set-thread-auto-archive-duration "disco-room")
(declare-function disco-room-edit-thread-settings "disco-room")
(declare-function disco-room-set-thread-muted "disco-room")
(declare-function disco-room-join-thread "disco-room")
(declare-function disco-room-leave-thread "disco-room")
(declare-function disco-room-toggle-thread-archived "disco-room")

(defvar-local disco-room--channel-id nil)

(defun disco-transient-reset-session-state ()
  "Reset in-memory session state for disco.el."
  (interactive)
  (disco-gateway-stop)
  (disco-state-reset)
  (disco-api-reset-rate-limit-state)
  (disco-http-reset-queue-state)
  (message "disco: in-memory state reset"))

(defun disco-transient-toggle-active-thread-prefetch ()
  "Toggle `disco-fetch-guild-active-threads' and refresh root when relevant."
  (interactive)
  (setq disco-fetch-guild-active-threads (not disco-fetch-guild-active-threads))
  (message "disco: active thread prefetch %s"
           (if disco-fetch-guild-active-threads "enabled" "disabled"))
  (when (derived-mode-p 'disco-root-mode)
    (disco-root-refresh)))

(defun disco-transient-set-thread-archive-fetch-limit (limit)
  "Set archived thread fetch LIMIT in current session."
  (interactive "nArchive thread fetch limit (2-100): ")
  (setq disco-thread-archive-fetch-limit (max 2 (min 100 limit)))
  (message "disco: archive thread fetch limit set to %d"
           disco-thread-archive-fetch-limit))

(defun disco-transient-room-open-parent-archived-threads ()
  "Open archived thread browser for current room's parent channel."
  (interactive)
  (let* ((channel (and disco-room--channel-id
                       (disco-state-channel disco-room--channel-id)))
         (parent-id (and channel (alist-get 'parent_id channel))))
    (unless parent-id
      (user-error "disco: current room has no parent channel"))
    (disco-root-list-archived-threads parent-id)))

(transient-define-prefix disco-root-transient ()
  "Root command menu for disco.el."
  [["Refresh"
    ("g" "Refresh root" disco-root-refresh)
    ("A" "Archived threads..." disco-root-list-archived-threads)
    ("t" "Toggle active thread prefetch" disco-transient-toggle-active-thread-prefetch)
    ("L" "Set archive fetch limit" disco-transient-set-thread-archive-fetch-limit)]
   ["Inspect"
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Session"
    ("x" "Reset session state" disco-transient-reset-session-state)
    ("q" "Quit window" quit-window)]])

(transient-define-prefix disco-room-transient ()
  "Room command menu for disco.el."
  [["Timeline"
   ("g" "Refresh room" disco-room-refresh)
   ("o" "Load older" disco-room-load-older-messages)
   ("c" "Send message" disco-room-send-message)
   ("f" "Attach file" disco-room-attach-file)
   ("D" "Remove attach token" disco-room-remove-attachment-token-at-point)
   ("x" "Clear attachments" disco-room-clear-attachments)
   ("v" "List attachments" disco-room-list-attachments)
   ("V" "Edit attach desc" disco-room-edit-attachment-description)
   ("O" "Reorder attachments" disco-room-reorder-attachments)
   ("r" "Reply to message" disco-room-reply-to-message)
   ("k" "Cancel reply" disco-room-cancel-reply)
   ("e" "Edit at point" disco-room-edit-message)
   ("d" "Delete at point" disco-room-delete-message)
   ("!" "Toggle reaction" disco-room-toggle-reaction)
   ("+" "Add reaction" disco-room-add-reaction)
   ("-" "Remove reaction" disco-room-remove-reaction)]
   ["Thread"
    ("m" "Create from message" disco-room-create-thread-from-message)
    ("o" "Open msg thread" disco-room-open-thread-from-message-at-point)
    ("n" "Create detached" disco-room-create-thread)
    ("R" "Rename thread" disco-room-rename-thread)
    ("L" "Toggle locked" disco-room-toggle-thread-locked)
    ("S" "Set slowmode" disco-room-set-thread-slowmode)
    ("U" "Set auto-archive" disco-room-set-thread-auto-archive-duration)
    ("E" "Edit thread settings" disco-room-edit-thread-settings)
    ("M" "Set muted" disco-room-set-thread-muted)
    ("j" "Join thread" disco-room-join-thread)
    ("l" "Leave thread" disco-room-leave-thread)
    ("a" "Toggle archived" disco-room-toggle-thread-archived)
    ("A" "Parent archived threads..." disco-transient-room-open-parent-archived-threads)]
   ["Inspect"
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Window"
    ("q" "Quit window" quit-window)]])

(provide 'disco-transient)

;;; disco-transient.el ends here
