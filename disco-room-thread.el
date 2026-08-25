;;; disco-room-thread.el --- Thread interactions in Disco rooms -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Thread-specific room commands and starter-thread references.  The room
;; controller retains Appkit view ownership and exposes only the operations
;; needed to commit controller state and request projection.

;;; Code:

(require 'subr-x)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-ins)
(require 'disco-permission)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-room-compose)

(declare-function disco-room--channel-object "disco-room" ())
(declare-function disco-room--ensure-view "disco-room" ())
(declare-function disco-room--latest-message-id "disco-room" ())
(declare-function disco-room--message-at-point "disco-room" ())
(declare-function disco-room--message-flags "disco-room" (message))
(declare-function disco-room--request-render "disco-room" (view))
(declare-function disco-room--resolve-thread-update "disco-room" (updated))
(declare-function disco-room-open "disco-room" (channel-id channel-name))
(declare-function disco-root-list-archived-threads "disco-root" (&optional parent-channel-id))

(defconst disco-room-thread--message-flag-has-thread (ash 1 5)
  "Bit mask indicating message has an associated starter thread.")

(defun disco-room-thread--ensure-current-thread ()
  "Signal `user-error' unless the current room is a thread."
  (unless (disco-thread-channel-p (disco-room--channel-object))
    (user-error "disco: current room is not a thread")))

(defun disco-room-thread--ensure-parent-channel ()
  "Signal `user-error' when the current room is itself a thread."
  (when (disco-thread-channel-p (disco-room--channel-object))
    (user-error "disco: open a parent channel room to create a new thread")))

(defun disco-room-thread--ensure-action-available (reason action)
  "Signal `user-error' when ACTION is unavailable for REASON."
  (when reason
    (user-error "disco: cannot %s: %s" action reason)))

(defun disco-room-thread--commit-update (updated)
  "Commit complete UPDATED state and request the controller projection."
  (disco-room--resolve-thread-update updated)
  (disco-room--request-render (disco-room--ensure-view)))

(defun disco-room-thread--channel-permission-reason (channel permissions)
  "Return missing-permission reason for PERMISSIONS in CHANNEL, or nil."
  (let ((missing (and channel
                      (disco-permission-channel-known-p channel)
                      (disco-permission-channel-missing channel permissions nil))))
    (when missing
      (format "missing %s"
              (mapconcat #'disco-permission-display-name missing ", ")))))

(defun disco-room-thread--update-unavailable-reason (&optional channel)
  "Return reason thread update actions are unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p channel))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     (t
      (disco-room-thread--channel-permission-reason channel '(manage-threads))))))

(defun disco-room-thread--toggle-archived-unavailable-reason (&optional channel)
  "Return reason toggling archive is unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p channel))
      "current room is not a thread")
     ((not (disco-thread-archived-p channel))
      (disco-room-thread--channel-permission-reason channel '(manage-threads)))
     ((disco-thread-locked-p channel)
      (disco-room-thread--channel-permission-reason channel '(manage-threads)))
     (t
      (disco-room-thread--channel-permission-reason
       channel
       (disco-room--required-send-permissions channel))))))

(defun disco-room-thread--joined-p (&optional channel)
  "Return non-nil when the current user is known to join CHANNEL."
  (let* ((channel (or channel (disco-room--channel-object)))
         (thread-id (and (listp channel) (alist-get 'id channel)))
         (self-id (disco-gateway-current-user-id)))
    (and thread-id
         self-id
         (member (format "%s" self-id)
                 (disco-state-thread-member-ids thread-id)))))

(defun disco-room-thread--join-unavailable-reason (&optional channel)
  "Return reason joining CHANNEL is unavailable, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p channel))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     ((disco-room-thread--joined-p channel)
      "already joined to this thread"))))

(defun disco-room-thread--leave-unavailable-reason (&optional channel)
  "Return reason leaving CHANNEL is unavailable, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p channel))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     ((not (disco-room-thread--joined-p channel))
      "not joined to this thread"))))

(defun disco-room-thread--mute-unavailable-reason (&optional channel)
  "Return reason changing mute state is unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p channel))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     ((not (disco-room-thread--joined-p channel))
      "join the thread before changing mute state"))))

(defun disco-room-thread--create-unavailable-reason (&optional type)
  "Return reason detached thread creation is unavailable for TYPE, or nil."
  (let ((channel (disco-room--channel-object)))
    (cond
     ((disco-thread-channel-p channel)
      "open a parent channel room to create a new thread")
     ((and channel (not (disco-thread-parent-channel-p channel)))
      "current room channel does not support threads")
     ((not (and channel (disco-permission-channel-known-p channel)))
      nil)
     ((and (eq type :any)
           (not (disco-thread-forum-or-media-channel-p channel)))
      (unless (or (disco-permission-channel-has-p channel 'create-public-threads nil)
                  (disco-permission-channel-has-p channel 'create-private-threads nil))
        (format "missing one of %s"
                (mapconcat #'disco-permission-display-name
                           '(create-public-threads create-private-threads)
                           ", "))))
     (t
      (disco-room-thread--channel-permission-reason
       channel
       (if (equal type 12)
           '(create-private-threads)
         '(create-public-threads)))))))

(defun disco-room-thread--create-from-message-unavailable-reason ()
  "Return reason creating a starter thread is unavailable, or nil."
  (disco-room-thread--create-unavailable-reason 11))

(defun disco-room-thread--message-has-thread-p (message)
  "Return non-nil when MESSAGE is known to have a starter thread."
  (let ((message-id (alist-get 'id message))
        (flags (disco-room--message-flags message)))
    (or (and (stringp message-id)
             (listp (disco-state-channel message-id))
             (disco-state-channel-thread-p (disco-state-channel message-id)))
        (not (zerop (logand flags disco-room-thread--message-flag-has-thread))))))

(defun disco-room-thread--from-message (message)
  "Return thread channel resolved from starter MESSAGE, or nil."
  (let ((message-id (alist-get 'id message)))
    (when (stringp message-id)
      (let ((channel (disco-state-channel message-id)))
        (when (and (listp channel)
                   (disco-state-channel-thread-p channel))
          channel)))))

(defun disco-room-thread--open-from-message-unavailable-reason (&optional message)
  "Return reason opening starter thread for MESSAGE is unavailable, or nil."
  (let* ((message (or message
                      (ignore-errors (disco-room--message-at-point))))
         (message-id (and (listp message) (alist-get 'id message))))
    (cond
     ((not (listp message)) "point is not on a message")
     ((not (stringp message-id)) "message has no id")
     ((not (disco-room-thread--message-has-thread-p message))
      (format "message %s has no starter thread" message-id)))))

(defun disco-room-thread-open-from-message (message)
  "Open starter thread associated with MESSAGE."
  (let* ((message-id (alist-get 'id message))
         (thread (disco-room-thread--from-message message))
         (target-thread-id (or (and (listp thread) (alist-get 'id thread))
                               (and (disco-room-thread--message-has-thread-p message)
                                    (stringp message-id)
                                    message-id)))
         (target-thread-name (or (and (listp thread) (alist-get 'name thread))
                                 (and (stringp message-id)
                                      (format "thread:%s" message-id)))))
    (when-let* ((reason
                 (disco-room-thread--open-from-message-unavailable-reason message)))
      (user-error "disco: %s" reason))
    (unless target-thread-id
      (user-error "disco: cannot resolve starter thread id from message %s" message-id))
    (disco-room-open target-thread-id (or target-thread-name target-thread-id))))

(defun disco-room-thread-open-from-message-at-point ()
  "Open starter thread associated with the message at point."
  (interactive)
  (disco-room-thread-open-from-message (disco-room--message-at-point)))

(defun disco-room-thread-insert-reference (message prefix)
  "Insert a navigable starter-thread reference for MESSAGE using PREFIX."
  (when (disco-room-thread--message-has-thread-p message)
    (let* ((message-id (alist-get 'id message))
           (thread (disco-room-thread--from-message message))
           (target-thread-id (or (and (listp thread) (alist-get 'id thread))
                                 (and (stringp message-id) message-id)))
           (target-thread-name (or (and (listp thread) (alist-get 'name thread))
                                   (and (stringp message-id)
                                        (format "thread:%s" message-id)))))
      (disco-ins-insert-reference-line
       (if target-thread-id
           (format "Thread: %s" (or target-thread-name target-thread-id))
         "Thread unavailable")
       :prefix prefix
       :face (if target-thread-id 'disco-room-message-meta 'shadow)
       :action (and target-thread-id
                    (lambda ()
                      (disco-room-open target-thread-id (or target-thread-name target-thread-id))))
       :help-echo "Open starter thread for this message"))))

(defun disco-room-thread--read-optional-nonnegative-int (prompt)
  "Read optional non-negative integer using PROMPT."
  (let ((raw (read-string prompt)))
    (unless (string-empty-p raw)
      (let ((number (string-to-number raw)))
        (when (< number 0)
          (user-error "disco: value must be >= 0"))
        number))))

(defun disco-room-thread-create-from-message
    (name message-id &optional auto-archive-duration rate-limit-per-user)
  "Create thread NAME from MESSAGE-ID in the current channel."
  (interactive
   (progn
     (disco-room-thread--ensure-action-available
      (disco-room-thread--create-from-message-unavailable-reason)
      "create threads from messages")
     (let* ((name (read-string "Thread name: "))
            (default-message-id (disco-room--latest-message-id))
            (message-raw (read-string
                          (if default-message-id
                              (format "Message ID (default %s): " default-message-id)
                            "Message ID: ")))
            (message-id (if (string-empty-p message-raw)
                            (or default-message-id
                                (user-error "disco: no message id provided and no loaded messages"))
                          message-raw))
            (auto-archive-duration (disco-thread-read-auto-archive-duration nil nil))
            (rate-limit-per-user
             (disco-room-thread--read-optional-nonnegative-int
              "Slowmode seconds (empty for none): ")))
       (list name message-id auto-archive-duration rate-limit-per-user))))
  (disco-room-thread--ensure-action-available
   (disco-room-thread--create-from-message-unavailable-reason)
   "create threads from messages")
  (disco-room-thread--ensure-parent-channel)
  (let* ((thread (disco-api-create-thread-from-message
                  (alist-get 'id (disco-room--channel-object))
                  message-id name auto-archive-duration rate-limit-per-user))
         (thread-id (and (listp thread) (alist-get 'id thread)))
         (thread-name (or (and (listp thread) (alist-get 'name thread)) name)))
    (when thread-id
      (disco-state-upsert-channel thread)
      (disco-room-open thread-id thread-name))
    (message "disco: created thread %s" name)))

(defun disco-room-thread-create
    (name &optional type auto-archive-duration invitable rate-limit-per-user)
  "Create detached thread NAME in the current channel."
  (interactive
   (progn
     (disco-room-thread--ensure-action-available
      (disco-room-thread--create-unavailable-reason :any)
      "create detached threads")
     (let* ((name (read-string "Thread name: "))
            (type (unless (disco-thread-forum-or-media-channel-p
                           (disco-room--channel-object))
                    (disco-thread-read-detached-type)))
            (auto-archive-duration (disco-thread-read-auto-archive-duration nil nil))
            (invitable (when (equal type 12)
                         (y-or-n-p "Invitable by non-moderators? ")))
            (rate-limit-per-user
             (disco-room-thread--read-optional-nonnegative-int
              "Slowmode seconds (empty for none): ")))
       (list name type auto-archive-duration invitable rate-limit-per-user))))
  (disco-room-thread--ensure-action-available
   (disco-room-thread--create-unavailable-reason (or type :any))
   "create detached threads")
  (disco-room-thread--ensure-parent-channel)
  (let* ((thread (disco-api-create-thread
                  (alist-get 'id (disco-room--channel-object))
                  name type auto-archive-duration invitable rate-limit-per-user))
         (thread-id (and (listp thread) (alist-get 'id thread)))
         (thread-name (or (and (listp thread) (alist-get 'name thread)) name)))
    (when thread-id
      (disco-state-upsert-channel thread)
      (disco-room-open thread-id thread-name))
    (message "disco: created detached thread %s" name)))

(defun disco-room-thread-join ()
  "Join the current thread as the current user."
  (interactive)
  (disco-room-thread--ensure-action-available
   (disco-room-thread--join-unavailable-reason) "join threads")
  (disco-room-thread--ensure-current-thread)
  (let ((channel-id (alist-get 'id (disco-room--channel-object))))
    (disco-api-join-thread channel-id)
    (when-let* ((self-id (disco-gateway-current-user-id)))
      (disco-state-upsert-thread-member channel-id self-id)))
  (message "disco: joined thread %s" (alist-get 'name (disco-room--channel-object))))

(defun disco-room-thread-leave ()
  "Leave the current thread as the current user."
  (interactive)
  (disco-room-thread--ensure-action-available
   (disco-room-thread--leave-unavailable-reason) "leave threads")
  (disco-room-thread--ensure-current-thread)
  (let ((channel-id (alist-get 'id (disco-room--channel-object))))
    (disco-api-leave-thread channel-id)
    (when-let* ((self-id (disco-gateway-current-user-id)))
      (disco-state-delete-thread-member channel-id self-id)))
  (message "disco: left thread %s" (alist-get 'name (disco-room--channel-object))))

(defun disco-room-thread-toggle-archived ()
  "Toggle archived state for the current thread."
  (interactive)
  (disco-room-thread--ensure-action-available
   (disco-room-thread--toggle-archived-unavailable-reason)
   "toggle thread archived state")
  (disco-room-thread--ensure-current-thread)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-archived (not (disco-thread-archived-p channel)))
         (updated (disco-api-set-thread-archived
                   (alist-get 'id (disco-room--channel-object))
                   next-archived nil)))
    (disco-room-thread--commit-update updated)
    (message "disco: thread %s"
             (if next-archived "archived" "unarchived"))))

(defun disco-room-thread-rename (name)
  "Rename the current thread to NAME."
  (interactive
   (progn
     (disco-room-thread--ensure-action-available
      (disco-room-thread--update-unavailable-reason) "rename threads")
     (let* ((channel (or (disco-room--channel-object)
                         (user-error "disco: unknown thread in state")))
            (current-name (or (alist-get 'name channel) "")))
       (list (string-trim (read-string "Thread name: " current-name))))))
  (disco-room-thread--ensure-action-available
   (disco-room-thread--update-unavailable-reason) "rename threads")
  (disco-room-thread--ensure-current-thread)
  (when (string-empty-p name)
    (user-error "disco: thread name cannot be empty"))
  (unless (disco-room--channel-object)
    (user-error "disco: unknown thread in state"))
  (disco-room-thread--commit-update (disco-api-update-thread (alist-get 'id (disco-room--channel-object)) :name name))
  (message "disco: thread renamed to %s" name))

(defun disco-room-thread-toggle-locked ()
  "Toggle locked state for the current thread."
  (interactive)
  (disco-room-thread--ensure-action-available
   (disco-room-thread--update-unavailable-reason) "toggle thread locked state")
  (disco-room-thread--ensure-current-thread)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-locked (not (disco-thread-locked-p channel)))
         (updated (disco-api-update-thread
                   (alist-get 'id (disco-room--channel-object))
                   :locked next-locked)))
    (disco-room-thread--commit-update updated)
    (message "disco: thread %s"
             (if next-locked "locked" "unlocked"))))

(defun disco-room-thread-set-slowmode (seconds)
  "Set the current thread slowmode to SECONDS."
  (interactive
   (progn
     (disco-room-thread--ensure-action-available
      (disco-room-thread--update-unavailable-reason) "set thread slowmode")
     (list (or (disco-room-thread--read-optional-nonnegative-int
                "Slowmode seconds (empty clears to 0): ")
               0))))
  (disco-room-thread--ensure-action-available
   (disco-room-thread--update-unavailable-reason) "set thread slowmode")
  (disco-room-thread--ensure-current-thread)
  (unless (disco-room--channel-object)
    (user-error "disco: unknown thread in state"))
  (disco-room-thread--commit-update (disco-api-update-thread (alist-get 'id (disco-room--channel-object))
                            :rate-limit-per-user seconds))
  (message "disco: thread slowmode -> %ss" seconds))

(defun disco-room-thread-set-auto-archive-duration (minutes)
  "Set current thread auto archive duration to MINUTES."
  (interactive
   (progn
     (disco-room-thread--ensure-action-available
      (disco-room-thread--update-unavailable-reason)
      "set thread auto archive duration")
     (let* ((channel (or (disco-room--channel-object)
                         (user-error "disco: unknown thread in state")))
            (meta (disco-thread-metadata channel))
            (current (or (alist-get 'auto_archive_duration meta)
                         (alist-get 'auto_archive_duration channel))))
       (list (disco-thread-read-auto-archive-duration t current)))))
  (disco-room-thread--ensure-action-available
   (disco-room-thread--update-unavailable-reason)
   "set thread auto archive duration")
  (disco-room-thread--ensure-current-thread)
  (unless (disco-room--channel-object)
    (user-error "disco: unknown thread in state"))
  (disco-room-thread--commit-update (disco-api-update-thread (alist-get 'id (disco-room--channel-object))
                            :auto-archive-duration minutes))
  (message "disco: auto archive -> %s minutes" minutes))

(defun disco-room-thread-set-muted (muted)
  "Set the current user's muted state for the current thread to MUTED."
  (interactive
   (progn
     (disco-room-thread--ensure-action-available
      (disco-room-thread--mute-unavailable-reason) "set thread mute state")
     (list (y-or-n-p "Mute this thread? "))))
  (disco-room-thread--ensure-action-available
   (disco-room-thread--mute-unavailable-reason) "set thread mute state")
  (disco-room-thread--ensure-current-thread)
  (disco-api-update-thread-member-settings
   (alist-get 'id (disco-room--channel-object)) :muted muted)
  (message "disco: thread notifications %s" (if muted "muted" "unmuted")))

(defun disco-room-thread-edit-settings ()
  "Edit multiple thread settings in one PATCH request."
  (interactive)
  (disco-room-thread--ensure-action-available
   (disco-room-thread--update-unavailable-reason) "edit thread settings")
  (disco-room-thread--ensure-current-thread)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (meta (disco-thread-metadata channel))
         (current-name (or (alist-get 'name channel) ""))
         (name-input (string-trim
                      (read-string
                       (format "Thread name (empty keeps %s): " current-name))))
         (name (unless (string-empty-p name-input) name-input))
         (current-auto (or (alist-get 'auto_archive_duration meta)
                           (alist-get 'auto_archive_duration channel)))
         (auto-input (completing-read
                      (format "Auto archive minutes (empty keeps %s): "
                              (or current-auto "unset"))
                      '("" "60" "1440" "4320" "10080") nil t nil nil ""))
         (auto-archive-duration
          (unless (string-empty-p auto-input)
            (string-to-number auto-input)))
         (slow-input (read-string
                      (format "Slowmode seconds (empty keeps %s): "
                              (or (alist-get 'rate_limit_per_user channel) 0))))
         (rate-limit-per-user
          (unless (string-empty-p slow-input)
            (let ((number (string-to-number slow-input)))
              (when (< number 0)
                (user-error "disco: value must be >= 0"))
              number)))
         (archived-choice
          (disco-thread-read-tristate-bool
           "Archived" (disco-thread-archived-p channel)))
         (locked-choice
          (disco-thread-read-tristate-bool
           "Locked" (disco-thread-locked-p channel)))
         (archived (unless (eq archived-choice 'keep) archived-choice))
         (locked (unless (eq locked-choice 'keep) locked-choice))
         (has-change (or name auto-archive-duration
                         (not (null rate-limit-per-user))
                         (not (eq archived-choice 'keep))
                         (not (eq locked-choice 'keep)))))
    (unless has-change
      (user-error "disco: no thread setting changes provided"))
    (disco-room-thread--commit-update (disco-api-update-thread
     (alist-get 'id (disco-room--channel-object))
     :name name
     :auto-archive-duration auto-archive-duration
     :rate-limit-per-user rate-limit-per-user
     :archived archived
     :locked locked))
    (message "disco: updated thread settings")))

(defun disco-room-thread-open-parent-archived ()
  "Open archived thread browser for the current room's parent channel."
  (interactive)
  (let* ((channel (disco-room--channel-object))
         (parent-id (and channel (alist-get 'parent_id channel))))
    (unless parent-id
      (user-error "disco: current room has no parent channel"))
    (disco-root-list-archived-threads parent-id)))

(provide 'disco-room-thread)

;;; disco-room-thread.el ends here
