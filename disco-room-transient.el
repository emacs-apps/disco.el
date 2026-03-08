;;; disco-room-transient.el --- Room transients for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Room command menus extracted from `disco-room.el'.  This file keeps transient
;; definitions separate from room state, render, and composer logic.

;;; Code:

(require 'transient)

(require 'disco-api)
(require 'disco-gateway)
(require 'disco-http)

(declare-function disco-room--attach-unavailable-reason "disco-room")
(declare-function disco-room--attachment-token-action-unavailable-reason
                  "disco-room" (&optional min-count))
(declare-function disco-room--composer-aux-active-p "disco-room")
(declare-function disco-room--delete-message-unavailable-reason
                  "disco-room" (&optional msg))
(declare-function disco-room--edit-start-unavailable-reason
                  "disco-room" (&optional msg))
(declare-function disco-room--forward-unavailable-reason "disco-room")
(declare-function disco-room--message-at-point "disco-room")
(declare-function disco-room--poll-clear-unavailable-reason
                  "disco-room" (&optional msg))
(declare-function disco-room--poll-expire-unavailable-reason
                  "disco-room" (&optional msg))
(declare-function disco-room--poll-submit-unavailable-reason
                  "disco-room" (&optional msg))
(declare-function disco-room--poll-unavailable-reason "disco-room")
(declare-function disco-room--poll-vote-unavailable-reason
                  "disco-room" (&optional msg))
(declare-function disco-room--reaction-unavailable-reason "disco-room"
                  (&optional msg))
(declare-function disco-room--reply-unavailable-reason "disco-room")
(declare-function disco-room--send-message-unavailable-reason "disco-room")
(declare-function disco-room--thread-create-from-message-unavailable-reason
                  "disco-room")
(declare-function disco-room--thread-create-unavailable-reason
                  "disco-room" (&optional type))
(declare-function disco-room--thread-join-unavailable-reason "disco-room"
                  (&optional channel))
(declare-function disco-room--thread-leave-unavailable-reason "disco-room"
                  (&optional channel))
(declare-function disco-room--thread-mute-unavailable-reason "disco-room"
                  (&optional channel))
(declare-function disco-room--thread-toggle-archived-unavailable-reason
                  "disco-room" (&optional channel))
(declare-function disco-room--thread-update-unavailable-reason "disco-room"
                  (&optional channel))
(declare-function disco-room-ack-channel-pins "disco-room")
(declare-function disco-room-add-reaction "disco-room"
                  (&optional emoji message-id))
(declare-function disco-room-attach-clipboard "disco-room")
(declare-function disco-room-attach-file "disco-room"
                  (path &optional description))
(declare-function disco-room-cancel-reply "disco-room")
(declare-function disco-room-clear-attachments "disco-room")
(declare-function disco-room-clear-poll-votes "disco-room"
                  (&optional message-id))
(declare-function disco-room-create-thread "disco-room"
                  (name &optional type auto-archive-duration invitable rate-limit-per-user))
(declare-function disco-room-create-thread-from-message "disco-room"
                  (name message-id &optional auto-archive-duration rate-limit-per-user))
(declare-function disco-room-delete-message "disco-room")
(declare-function disco-room-edit-attachment-description "disco-room")
(declare-function disco-room-edit-message "disco-room")
(declare-function disco-room-edit-thread-settings "disco-room")
(declare-function disco-room-expire-poll "disco-room" (&optional message-id))
(declare-function disco-room-filter-cancel "disco-room-search")
(declare-function disco-room-filter-search "disco-room-search"
                  (&optional query by-sender-p))
(declare-function disco-room-forward-message "disco-room"
                  (&optional message-id source-channel-id content forward-only))
(declare-function disco-room-join-thread "disco-room")
(declare-function disco-room-leave-thread "disco-room")
(declare-function disco-room-list-attachments "disco-room")
(declare-function disco-room-load-older-messages "disco-room")
(declare-function disco-room-open-parent-archived-threads "disco-room")
(declare-function disco-room-open-thread-from-message-at-point "disco-room")
(declare-function disco-room-refetch-avatars "disco-room")
(declare-function disco-room-refresh "disco-room")
(declare-function disco-room-remove-attachment-token-at-point "disco-room")
(declare-function disco-room-remove-poll-vote "disco-room"
                  (&optional answer-id message-id))
(declare-function disco-room-remove-reaction "disco-room"
                  (&optional emoji message-id))
(declare-function disco-room-reorder-attachments "disco-room")
(declare-function disco-room-reply-to-message "disco-room"
                  (&optional message-id))
(declare-function disco-room-rename-thread "disco-room" (name))
(declare-function disco-room-search-channel "disco-room-search")
(declare-function disco-room-send-message "disco-room")
(declare-function disco-room-send-poll "disco-room"
                  (question options &optional duration allow-multiselect content))
(declare-function disco-room-set-thread-auto-archive-duration "disco-room"
                  (minutes))
(declare-function disco-room-set-thread-muted "disco-room" (muted))
(declare-function disco-room-set-thread-slowmode "disco-room" (seconds))
(declare-function disco-room-submit-poll-vote "disco-room"
                  (&optional message-id))
(declare-function disco-room-toggle-poll-answer "disco-room"
                  (&optional answer-id message-id))
(declare-function disco-room-toggle-reaction "disco-room"
                  (&optional emoji message-id))
(declare-function disco-room-toggle-thread-archived "disco-room")
(declare-function disco-room-toggle-thread-locked "disco-room")
(declare-function disco-room-vote-poll-answer "disco-room"
                  (&optional answer-id message-id))

(defun disco-room-transient--attachment-action-inapt-reason (min-count)
  "Return inapt text for attachment actions requiring MIN-COUNT items."
  (disco-room--attachment-token-action-unavailable-reason min-count))

(defun disco-room-transient--message-at-point ()
  "Return message at point, suppressing user errors for transient checks."
  (ignore-errors (disco-room--message-at-point)))

(defun disco-room-transient--edit-inapt-reason ()
  "Return inapt text for editing the message at point."
  (disco-room--edit-start-unavailable-reason
   (disco-room-transient--message-at-point)))

(defun disco-room-transient--delete-inapt-reason ()
  "Return inapt text for deleting the message at point."
  (disco-room--delete-message-unavailable-reason
   (disco-room-transient--message-at-point)))

(defun disco-room-transient--reaction-inapt-reason ()
  "Return inapt text for reaction actions at point."
  (disco-room--reaction-unavailable-reason
   (disco-room-transient--message-at-point)))

(defun disco-room-transient--poll-vote-inapt-reason ()
  "Return inapt text for poll vote actions at point."
  (disco-room--poll-vote-unavailable-reason
   (disco-room-transient--message-at-point)))

(defun disco-room-transient--poll-submit-inapt-reason ()
  "Return inapt text for submitting a staged poll vote at point."
  (disco-room--poll-submit-unavailable-reason
   (disco-room-transient--message-at-point)))

(defun disco-room-transient--poll-clear-inapt-reason ()
  "Return inapt text for clearing a poll vote at point."
  (disco-room--poll-clear-unavailable-reason
   (disco-room-transient--message-at-point)))

(defun disco-room-transient--poll-expire-inapt-reason ()
  "Return inapt text for expiring a poll at point."
  (disco-room--poll-expire-unavailable-reason
   (disco-room-transient--message-at-point)))

(transient-define-prefix disco-room-attach-transient ()
  "Transient for telega-like room attachment commands."
  [["Attach"
    ("f" "Attach file" disco-room-attach-file
     :inapt-if disco-room--attach-unavailable-reason)
    ("p" "Send poll" disco-room-send-poll
     :inapt-if disco-room--poll-unavailable-reason)
    ("v" "Attach clipboard" disco-room-attach-clipboard)]])

(transient-define-prefix disco-room-transient ()
  "Room command menu for disco.el."
  [["Timeline"
    ("g" "Refresh room" disco-room-refresh)
    ("o" "Load older" disco-room-load-older-messages)
    ("c" "Send message" disco-room-send-message
     :inapt-if disco-room--send-message-unavailable-reason)
    ("f" "Attach file" disco-room-attach-file
     :inapt-if disco-room--attach-unavailable-reason)
    ("D" "Remove attachment" disco-room-remove-attachment-token-at-point
     :inapt-if (lambda ()
                 (disco-room-transient--attachment-action-inapt-reason 1)))
    ("x" "Clear attachments" disco-room-clear-attachments
     :inapt-if (lambda ()
                 (disco-room-transient--attachment-action-inapt-reason 1)))
    ("v" "List attachments" disco-room-list-attachments
     :inapt-if (lambda ()
                 (disco-room-transient--attachment-action-inapt-reason 1)))
    ("V" "Edit attachment desc" disco-room-edit-attachment-description
     :inapt-if (lambda ()
                 (disco-room-transient--attachment-action-inapt-reason 1)))
    ("O" "Reorder attachments" disco-room-reorder-attachments
     :inapt-if (lambda ()
                 (disco-room-transient--attachment-action-inapt-reason 2)))
    ("r" "Reply to message" disco-room-reply-to-message
     :inapt-if disco-room--reply-unavailable-reason)
    ("F" "Forward message" disco-room-forward-message
     :inapt-if disco-room--forward-unavailable-reason)
    ("k" "Cancel reply/edit" disco-room-cancel-reply
     :inapt-if (lambda () (not (disco-room--composer-aux-active-p))))
    ("e" "Edit at point" disco-room-edit-message
     :inapt-if #'disco-room-transient--edit-inapt-reason)
    ("d" "Delete at point" disco-room-delete-message
     :inapt-if #'disco-room-transient--delete-inapt-reason)
    ("!" "Toggle reaction" disco-room-toggle-reaction
     :inapt-if #'disco-room-transient--reaction-inapt-reason)
    ("+" "Add reaction" disco-room-add-reaction
     :inapt-if #'disco-room-transient--reaction-inapt-reason)
    ("-" "Remove reaction" disco-room-remove-reaction
     :inapt-if #'disco-room-transient--reaction-inapt-reason)
    ("p" "Send poll" disco-room-send-poll
     :inapt-if disco-room--poll-unavailable-reason)
    ("w" "Select answer" disco-room-vote-poll-answer
     :inapt-if #'disco-room-transient--poll-vote-inapt-reason)
    ("u" "Unselect answer" disco-room-remove-poll-vote
     :inapt-if #'disco-room-transient--poll-vote-inapt-reason)
    ("t" "Toggle staged answer" disco-room-toggle-poll-answer
     :inapt-if #'disco-room-transient--poll-vote-inapt-reason)
    ("W" "Submit staged vote" disco-room-submit-poll-vote
     :inapt-if #'disco-room-transient--poll-submit-inapt-reason)
    ("C" "Remove my vote" disco-room-clear-poll-votes
     :inapt-if #'disco-room-transient--poll-clear-inapt-reason)
    ("X" "End poll" disco-room-expire-poll
     :inapt-if #'disco-room-transient--poll-expire-inapt-reason)
    ("P" "Ack pinned msgs" disco-room-ack-channel-pins)]
   ["Thread"
    ("m" "Create from message" disco-room-create-thread-from-message
     :inapt-if disco-room--thread-create-from-message-unavailable-reason)
    ("o" "Open msg thread" disco-room-open-thread-from-message-at-point)
    ("n" "Create detached" disco-room-create-thread
     :inapt-if (lambda () (disco-room--thread-create-unavailable-reason :any)))
    ("R" "Rename thread" disco-room-rename-thread
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("L" "Toggle locked" disco-room-toggle-thread-locked
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("S" "Set slowmode" disco-room-set-thread-slowmode
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("U" "Set auto-archive" disco-room-set-thread-auto-archive-duration
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("E" "Edit thread settings" disco-room-edit-thread-settings
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("M" "Set muted" disco-room-set-thread-muted
     :inapt-if disco-room--thread-mute-unavailable-reason)
    ("j" "Join thread" disco-room-join-thread
     :inapt-if disco-room--thread-join-unavailable-reason)
    ("l" "Leave thread" disco-room-leave-thread
     :inapt-if disco-room--thread-leave-unavailable-reason)
    ("a" "Toggle archived" disco-room-toggle-thread-archived
     :inapt-if disco-room--thread-toggle-archived-unavailable-reason)
    ("A" "Parent archived threads..." disco-room-open-parent-archived-threads)]
   ["Inspect"
    ("/" "Search channel..." disco-room-search-channel)
    ("f" "Filter search" disco-room-filter-search)
    ("F" "Cancel filter" disco-room-filter-cancel)
    ("v" "Refetch avatars" disco-room-refetch-avatars)
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Window"
    ("q" "Quit window" quit-window)]])

(provide 'disco-room-transient)

;;; disco-room-transient.el ends here
