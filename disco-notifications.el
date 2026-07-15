;;; disco-notifications.el --- Desktop notifications for disco.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Delayed, visibility-aware Discord desktop notifications.

;;; Code:

(require 'cl-lib)
(require 'notifications)
(require 'ring)
(require 'seq)
(require 'subr-x)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-room)
(require 'disco-root-view)
(require 'disco-runtime)
(require 'disco-state)

(defvar disco-notifications--last-id nil
  "Currently displayed desktop notification id.")

(defvar disco-notifications--last-display-op nil
  "Exact display operation owning `disco-notifications--last-id'.")

(defvar disco-notifications--seen (make-hash-table :test #'equal)
  "Discord message ids already scheduled for notification.")

(defvar disco-notifications--seen-order nil
  "Newest-first bounded order for `disco-notifications--seen'.")

(defconst disco-notifications--seen-limit 512
  "Maximum number of notification deduplication ids retained.")

(defvar disco-notifications--history nil
  "Ring of Discord messages shown as desktop notifications.")

(defconst disco-notifications--history-buffer-name "*Disco Notifications*"
  "Fallback name of the account-scoped notification history buffer.")

(defvar disco-notifications--history-buffer nil
  "Live notification history buffer, including after a user rename.")

(defvar disco-notifications--generation 0
  "Account generation owning notification callbacks and timers.")

(defvar disco-notifications--display-sequence 0
  "Monotonic sequence used to identify one notification display attempt.")

(defvar disco-notifications--active-display-op nil
  "Token of the only notification display attempt allowed to publish state.")

(defvar disco-notifications--reset-in-progress nil
  "Non-nil while notification account state is being destructively reset.")

(defvar disco-notifications--delay-owners nil
  "Owned delayed-delivery timers for the current account generation.")

(defvar disco-notifications--timeout-owners nil
  "Owned desktop-notification timeout timers for the current generation.")

(defconst disco-notifications--history-drain-limit 8
  "Maximum normal history-buffer disposal passes during one reset.")

(defvar-local disco-notifications--history-owner-p nil
  "Non-nil when this buffer is a Disco notification history projection.")

(put 'disco-notifications--history-owner-p 'permanent-local t)

(defun disco-notifications--owned-history-buffer ()
  "Return the explicitly owned notification history buffer."
  (or (and (buffer-live-p disco-notifications--history-buffer)
           (buffer-local-value 'disco-notifications--history-owner-p
                               disco-notifications--history-buffer)
           disco-notifications--history-buffer)
      (let* ((named (get-buffer disco-notifications--history-buffer-name))
             (buffer
              (if (and (buffer-live-p named)
                       (buffer-local-value
                        'disco-notifications--history-owner-p named))
                  named
                (generate-new-buffer
                 disco-notifications--history-buffer-name))))
        (with-current-buffer buffer
          (setq-local disco-notifications--history-owner-p t))
        (setq disco-notifications--history-buffer buffer))))

(defun disco-notifications--session-current-p (app generation)
  "Return non-nil when APP and GENERATION own the current Disco session."
  (and (not disco-notifications--reset-in-progress)
       (appkit-app-live-p app)
       (eq app disco-runtime--app)
       (= generation disco-notifications--generation)))

(defun disco-notifications--display-current-p (app generation display-op)
  "Return non-nil when DISPLAY-OP may publish for APP and GENERATION."
  (and disco-notifications-mode
       (equal display-op disco-notifications--active-display-op)
       (disco-notifications--session-current-p app generation)))

(defun disco-notifications--display-reservation-current-p
    (app generation display-op)
  "Return non-nil when DISPLAY-OP is the newest reservation for APP.

A reservation advances the sequence without replacing the active display.
This lets fallible or synchronously reentrant policy/presentation work leave
the previous notification authoritative unless the reservation later commits."
  (and disco-notifications-mode
       (= display-op disco-notifications--display-sequence)
       (disco-notifications--session-current-p app generation)))

(defun disco-notifications--owner-current-p (owner owners)
  "Return non-nil when OWNER is current and present in OWNERS."
  (and disco-notifications-mode
       (memq owner owners)
       (disco-notifications--session-current-p
        (plist-get owner :app)
        (or (plist-get owner :generation) -1))
       (or (not (plist-member owner :display-op))
           (equal (plist-get owner :display-op)
                  disco-notifications--active-display-op))))

(defun disco-notifications--cancel-owner-timer (owner)
  "Cancel the timer retained by OWNER without allowing failure to escape."
  (let ((handle (plist-get owner :handle))
        (timer (plist-get owner :timer))
        handle-cancelled-p)
    (unwind-protect
        (when handle
          (condition-case err
              (setq handle-cancelled-p (appkit-cancel-handle handle))
            (error
             (message "disco: notification handle cancellation failed: %s"
                      (error-message-string err)))
            (quit
             (message
              "disco: notification handle cancellation was interrupted"))))
      ;; Appkit marks a handle dead before invoking its cancellation callback.
      ;; If that first attempt fails, a later handle call is a no-op; therefore
      ;; retain and directly cancel the underlying timer as a fallback.
      (unwind-protect
          (unless handle-cancelled-p
            (condition-case err
                (when (timerp timer)
                  (cancel-timer timer))
              (error
               (message "disco: notification timer cancellation failed: %s"
                        (error-message-string err)))
              (quit
               (message
                "disco: notification timer cancellation was interrupted"))))
        (setf (plist-get owner :handle) nil
              (plist-get owner :timer) nil)))))

(defun disco-notifications--appkit-cancel-owner-timer (owner timer)
  "Cancel TIMER for OWNER and retire it after successful Appkit teardown."
  (let (cancelled-p)
    (unwind-protect
        (progn
          (when (timerp timer)
            (cancel-timer timer))
          (setq cancelled-p t))
      ;; Appkit retires its handle even if cancellation fails.  Preserve the
      ;; raw timer and global owner only on failure so reset can retry it.
      (setf (plist-get owner :handle) nil)
      (when cancelled-p
        (setq disco-notifications--delay-owners
              (delq owner disco-notifications--delay-owners)
              disco-notifications--timeout-owners
              (delq owner disco-notifications--timeout-owners))
        (setf (plist-get owner :timer) nil)))))

(defun disco-notifications--register-owner-timer (owner timer)
  "Register TIMER under OWNER's exact Appkit application lifecycle."
  (setf (plist-get owner :timer) timer)
  (let ((owners (cond
                 ((memq owner disco-notifications--delay-owners)
                  disco-notifications--delay-owners)
                 ((memq owner disco-notifications--timeout-owners)
                  disco-notifications--timeout-owners))))
    (if (and owners (disco-notifications--owner-current-p owner owners))
        (progn
          (setf (plist-get owner :handle)
                (appkit-register-handle
                 (plist-get owner :app) 'timer timer
                 (lambda (owned-timer)
                   (disco-notifications--appkit-cancel-owner-timer
                    owner owned-timer))))
          ;; Registration itself is an external lifecycle boundary.  A
          ;; synchronous stop/reset must not resurrect the returned handle.
          (unless (disco-notifications--owner-current-p owner owners)
            (setq disco-notifications--delay-owners
                  (delq owner disco-notifications--delay-owners)
                  disco-notifications--timeout-owners
                  (delq owner disco-notifications--timeout-owners))
            (disco-notifications--cancel-owner-timer owner)))
      ;; A test timer or unusual timer backend may run synchronously.  Do not
      ;; resurrect an owner that its callback already retired, nor retain a
      ;; cancelled owner in either account-owned list.
      (setq disco-notifications--delay-owners
            (delq owner disco-notifications--delay-owners)
            disco-notifications--timeout-owners
            (delq owner disco-notifications--timeout-owners))
      (disco-notifications--cancel-owner-timer owner))))

(defun disco-notifications--run-cleanup-items (items function)
  "Apply FUNCTION to every element of ITEMS during privacy cleanup.

Errors and quits are logged and isolated.  If FUNCTION performs another
nonlocal transfer, remaining items still run while that transfer unwinds."
  (let ((remaining items)
        complete-p)
    (unwind-protect
        (progn
          (while remaining
            (let ((item (pop remaining)))
              (condition-case err
                  (funcall function item)
                (error
                 (message "disco: notification cleanup failed: %s"
                          (error-message-string err)))
                (quit
                 (message "disco: notification cleanup was interrupted")))))
          (setq complete-p t))
      (unless complete-p
        (disco-notifications--run-cleanup-items remaining function)))))

(defun disco-notifications--revoke-async-work ()
  "Revoke and cancel all notification work owned by the current account."
  ;; Revoke owners before cancellation: cancellation may synchronously invoke
  ;; callbacks, which must already observe themselves as stale.
  (cl-incf disco-notifications--generation)
  (cl-incf disco-notifications--display-sequence)
  (setq disco-notifications--active-display-op nil)
  (let ((owners (append disco-notifications--delay-owners
                        disco-notifications--timeout-owners))
        (last-id disco-notifications--last-id)
        (last-display-op disco-notifications--last-display-op))
    (setq disco-notifications--delay-owners nil
          disco-notifications--timeout-owners nil)
    (disco-notifications--run-cleanup-items
     (append owners
             (when last-id
               (list (list :notification-id last-id
                           :display-op last-display-op))))
     (lambda (item)
       (if-let* ((id (plist-get item :notification-id)))
           (disco-notifications--close id (plist-get item :display-op))
         (disco-notifications--cancel-owner-timer item))))))

(defun disco-notifications--clear-session-data ()
  "Clear all in-memory notification data for the retired account."
  (clrhash disco-notifications--seen)
  (setq disco-notifications--seen-order nil
        disco-notifications--history nil
        disco-notifications--history-buffer nil
        disco-notifications--delay-owners nil
        disco-notifications--timeout-owners nil
        disco-notifications--last-id nil
        disco-notifications--last-display-op nil
        disco-notifications--active-display-op nil))

(defun disco-notifications--erase-history-buffer (buffer)
  "Erase account data from live notification history BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-disable-undo)
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (widen)
        (erase-buffer)
        (set-buffer-modified-p nil)))))

(defun disco-notifications--dispose-history-buffer (buffer)
  "Close account-scoped notification history BUFFER, erasing on failure."
  (when (buffer-live-p buffer)
    (let (complete-p)
      (unwind-protect
          (progn
            (condition-case err
                (with-current-buffer buffer
                  (let ((kill-buffer-query-functions nil)
                        (buffer-offer-save nil))
                    (set-buffer-modified-p nil)
                    (unless (kill-buffer buffer)
                      (error "Notification history buffer refused closure"))))
              (error
               (disco-notifications--erase-history-buffer buffer)
               (message "disco: notification history cleanup failed: %s"
                        (error-message-string err)))
              (quit
               (disco-notifications--erase-history-buffer buffer)
               (message
                "disco: notification history cleanup was interrupted")))
            (setq complete-p t))
        ;; An arbitrary throw from a kill hook cannot bypass privacy erasure.
        (unless complete-p
          (disco-notifications--erase-history-buffer buffer))))))

(defun disco-notifications--collect-history-buffers ()
  "Return every live buffer explicitly owned by notification history."
  (delq nil
        (delete-dups
         (append
          (list disco-notifications--history-buffer)
          (seq-filter
           (lambda (buffer)
             (and (buffer-live-p buffer)
                  (buffer-local-value
                   'disco-notifications--history-owner-p buffer)))
           (buffer-list))))))

(defun disco-notifications--force-dispose-history-buffer (buffer)
  "Erase and close history BUFFER without running buffer-local hooks."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-disable-undo)
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (kill-buffer-hook nil)
            (kill-buffer-query-functions nil)
            (buffer-offer-save nil)
            (quit-flag nil))
        (widen)
        (erase-buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

(defun disco-notifications--drain-history-buffers (&optional initial-buffers)
  "Dispose INITIAL-BUFFERS and history successors created by kill hooks."
  (let ((pending (delete-dups (copy-sequence initial-buffers)))
        stable-p)
    (catch 'stable
      (dotimes (_ disco-notifications--history-drain-limit)
        (setq pending
              (delete-dups
               (append pending
                       (disco-notifications--collect-history-buffers))))
        (when (null pending)
          (setq stable-p t)
          (throw 'stable t))
        (disco-notifications--run-cleanup-items
         pending #'disco-notifications--dispose-history-buffer)
        (setq pending nil)))
    (unless stable-p
      (disco-notifications--run-cleanup-items
       (disco-notifications--collect-history-buffers)
       #'disco-notifications--force-dispose-history-buffer))))

(defun disco-notifications-reset-session-state ()
  "Clear timers, history, and presentation belonging to the old account."
  (let ((buffers (disco-notifications--collect-history-buffers))
        (disco-notifications--reset-in-progress t))
    ;; Cancellation and kill hooks are external boundaries.  Keep a reset
    ;; barrier raised throughout, then revoke and clear once more so even an
    ;; arbitrary nonlocal transfer cannot retain work created reentrantly.
    (disco-notifications--run-cleanup-items
     (append
      (list #'disco-notifications--revoke-async-work
            #'disco-notifications--clear-session-data
            (lambda ()
              (disco-notifications--drain-history-buffers buffers))
            #'disco-notifications--drain-history-buffers
            #'disco-notifications--revoke-async-work
            #'disco-notifications--clear-session-data
            #'disco-notifications--drain-history-buffers
            #'disco-notifications--clear-session-data))
     #'funcall)))

(defun disco-notifications--history-ring ()
  "Return notification history ring at its configured size."
  (unless (and (ring-p disco-notifications--history)
               (= (ring-size disco-notifications--history)
                  disco-notifications-history-ring-size))
    (let ((old (and (ring-p disco-notifications--history)
                    (ring-elements disco-notifications--history))))
      (setq disco-notifications--history
            (make-ring (max 1 disco-notifications-history-ring-size)))
      (dolist (entry
               (reverse
                (seq-take old disco-notifications-history-ring-size)))
        (ring-insert disco-notifications--history entry))))
  disco-notifications--history)

(defun disco-notifications--remember (message-id)
  "Remember MESSAGE-ID in bounded deduplication state."
  (puthash message-id t disco-notifications--seen)
  (push message-id disco-notifications--seen-order)
  (when (> (length disco-notifications--seen-order)
           disco-notifications--seen-limit)
    (let ((expired (car (last disco-notifications--seen-order))))
      (setq disco-notifications--seen-order
            (butlast disco-notifications--seen-order))
      (remhash expired disco-notifications--seen))))

(defun disco-notifications--message-time (message)
  "Return MESSAGE timestamp as float seconds, or nil."
  (when-let* ((timestamp (alist-get 'timestamp message)))
    (condition-case nil
        (float-time (date-to-time timestamp))
      (error nil))))

(defun disco-notifications--message-ping-p (message)
  "Return non-nil when MESSAGE actually pings the current user."
  (disco-state--message-mentions-user-p
   message (disco-gateway-current-user-id)))

(defun disco-notifications--room-visible-p (channel-id)
  "Return non-nil when CHANNEL-ID is visible on a focused frame."
  (seq-some
   (lambda (frame)
     (and (frame-live-p frame)
          (eq (frame-focus-state frame) t)
          (seq-some
           (lambda (window)
             (when-let* ((buffer (window-buffer window)))
               (with-current-buffer buffer
                 (and (derived-mode-p 'disco-room-mode)
                      (equal disco-room--channel-id channel-id)))))
           (window-list frame 'no-minibuffer))))
   (frame-list)))

(defun disco-notifications-message-notify-p (message)
  "Return non-nil when incoming MESSAGE should produce a notification."
  (let* ((channel-id (disco-msg-normalize-id (alist-get 'channel_id message)))
         (channel (and channel-id (disco-state-channel channel-id)))
         (author (alist-get 'author message))
         (author-id (and (listp author) (alist-get 'id author)))
         (ping-p (disco-notifications--message-ping-p message))
         (time (disco-notifications--message-time message)))
    (and channel
         (not (equal (disco-msg-normalize-id author-id)
                     (disco-msg-normalize-id (disco-gateway-current-user-id))))
         (or (null time)
             (<= (- (float-time) time) disco-notifications-max-message-age))
         (or ping-p
             (and (not (disco-state-channel-muted-p channel))
                  (= 0 (disco-state-channel-notification-level channel))))
         (not (disco-notifications--room-visible-p channel-id)))))

(defun disco-notifications--guild-name (channel)
  "Return guild name for CHANNEL, or nil."
  (when-let* ((guild-id (alist-get 'guild_id channel))
              (guild (seq-find (lambda (it) (equal (alist-get 'id it) guild-id))
                               (disco-state-guilds))))
    (alist-get 'name guild)))

(defun disco-notifications--title (message channel)
  "Return notification title for MESSAGE in CHANNEL."
  (let ((author (or (disco-msg-author-display-name message) "Discord"))
        (channel-name (disco-root--channel-display-name channel))
        (guild-name (disco-notifications--guild-name channel)))
    (if guild-name
        (format "%s — #%s · %s" author channel-name guild-name)
      (format "%s — %s" author channel-name))))

(defun disco-notifications--body (message)
  "Return compact notification body for MESSAGE."
  (if disco-notifications-show-preview
      (truncate-string-to-width
       (concat (if (disco-notifications--message-ping-p message) "@你  " "")
               (disco-msg-preview-content message))
       (max 1 disco-notifications-body-limit) nil nil "…")
    "New Discord message"))

(defun disco-notifications-open-message (channel-id message-id)
  "Open CHANNEL-ID and jump to MESSAGE-ID."
  (when (fboundp 'x-focus-frame)
    (ignore-errors (x-focus-frame (selected-frame))))
  (ignore-errors (raise-frame (selected-frame)))
  (let* ((channel (disco-state-channel channel-id))
         (name (if channel (disco-root--channel-display-name channel) channel-id)))
    (disco-room-open channel-id name)
    (when message-id
      (disco-room-jump-to-message message-id channel-id))))

(defun disco-notifications--close (id &optional display-op)
  "Close ID if current and, when non-nil, owned by DISPLAY-OP."
  (when (and (equal id disco-notifications--last-id)
             (or (null display-op)
                 (equal display-op disco-notifications--last-display-op)))
    (setq disco-notifications--last-id nil
          disco-notifications--last-display-op nil)
    (ignore-errors (notifications-close-notification id))))

(defun disco-notifications--close-returned-id (id display-op)
  "Close stale ID from DISPLAY-OP unless a newer operation owns that ID."
  (when (and id
             (not (and (equal id disco-notifications--last-id)
                       (not (equal display-op
                                   disco-notifications--last-display-op)))))
    (ignore-errors (notifications-close-notification id))))

(defun disco-notifications--timeout-fired (owner)
  "Close the desktop notification owned by current timeout OWNER."
  (when (disco-notifications--owner-current-p
         owner disco-notifications--timeout-owners)
    (let ((app (plist-get owner :app))
          (generation (plist-get owner :generation))
          (display-op (plist-get owner :display-op))
          (id (plist-get owner :id)))
      (setq disco-notifications--timeout-owners
            (delq owner disco-notifications--timeout-owners))
      (disco-notifications--cancel-owner-timer owner)
      ;; Timer cancellation is an external synchronous boundary.  Do not let
      ;; an old callback close an identifier reused by a reset/nested display.
      (when (disco-notifications--display-current-p
             app generation display-op)
        (disco-notifications--close id display-op)))))

(defun disco-notifications--schedule-timeout
    (id app generation display-op)
  "Schedule ID timeout for exact APP, GENERATION, and DISPLAY-OP."
  (when (and id
             (disco-notifications--display-current-p
              app generation display-op))
    (let ((owner (list :app app :generation generation
                       :display-op display-op
                       :id id :timer nil :handle nil)))
      (push owner disco-notifications--timeout-owners)
      (condition-case err
          (disco-notifications--register-owner-timer
           owner
           (run-with-timer disco-notifications-timeout nil
                           #'disco-notifications--timeout-fired owner))
        (error
         (setq disco-notifications--timeout-owners
               (delq owner disco-notifications--timeout-owners))
         (disco-notifications--cancel-owner-timer owner)
         (message "disco: failed to schedule notification timeout: %s"
                  (error-message-string err)))
        (quit
         (setq disco-notifications--timeout-owners
               (delq owner disco-notifications--timeout-owners))
         (disco-notifications--cancel-owner-timer owner)
         (message "disco: notification timeout scheduling was interrupted")))
      (when (memq owner disco-notifications--timeout-owners)
        owner))))

(defun disco-notifications--show (message &optional app generation)
  "Show MESSAGE when APP and GENERATION still own the Disco session."
  (let ((app (or app disco-runtime--app))
        (generation (or generation disco-notifications--generation)))
    (when (and (disco-notifications--session-current-p app generation)
               disco-notifications-mode)
      ;; Reserve before every potentially reentrant policy/presentation step,
      ;; but leave the previous display active until all preparation succeeds.
      ;; A nested display advances the sequence and makes this attempt stale.
      (let ((display-op (cl-incf disco-notifications--display-sequence)))
        (when (and (disco-notifications-message-notify-p message)
                   (disco-notifications--display-reservation-current-p
                    app generation display-op))
          (let* ((channel-id
                  (disco-msg-normalize-id (alist-get 'channel_id message)))
                 (message-id (disco-msg-normalize-id (alist-get 'id message)))
                 (channel (disco-state-channel channel-id))
                 (title (disco-notifications--title message channel))
                 (body (disco-notifications--body message))
                 (urgency (if (disco-notifications--message-ping-p message)
                              "critical"
                            "normal"))
                 (extra-args (copy-sequence disco-notifications-extra-args)))
            (when (disco-notifications--display-reservation-current-p
                   app generation display-op)
              (setq disco-notifications--active-display-op display-op)
              (let ((args
                   (append
                    (list :app-name "disco.el"
                          :title title
                          :body body
                          :urgency urgency
                          :timeout -1
                          :actions '("default" "Open")
                          :on-action
                          (lambda (&rest _)
                            (when (disco-notifications--display-current-p
                                   app generation display-op)
                              (disco-notifications-open-message
                               channel-id message-id))))
                    extra-args)))
              (catch 'stale-display
                (unless (disco-notifications--display-current-p
                         app generation display-op)
                  (throw 'stale-display nil))
                (when disco-notifications--last-id
                  (let ((old-id disco-notifications--last-id)
                        (old-display-op
                         disco-notifications--last-display-op))
                    (dolist (owner (copy-sequence
                                    disco-notifications--timeout-owners))
                      (when (equal old-id (plist-get owner :id))
                        (setq disco-notifications--timeout-owners
                              (delq owner
                                    disco-notifications--timeout-owners))
                        (disco-notifications--cancel-owner-timer owner)
                        (unless (disco-notifications--display-current-p
                                 app generation display-op)
                          (throw 'stale-display nil))))
                    (disco-notifications--close old-id old-display-op)
                    ;; Closing the old desktop notification may synchronously
                    ;; reset the session or begin a nested display operation.
                    (unless (disco-notifications--display-current-p
                             app generation display-op)
                      (throw 'stale-display nil))))
                ;; Validate after old-close/cancellation callbacks and before
                ;; backend, history, or authoritative global state is touched.
                (unless (disco-notifications--display-current-p
                         app generation display-op)
                  (throw 'stale-display nil))
                (let (returned-id notified-p)
                  (condition-case err
                      (setq returned-id (apply #'notifications-notify args)
                            notified-p t)
                    (error
                     (message "disco: desktop notification failed: %s"
                              (error-message-string err)))
                    (quit
                     (message
                      "disco: desktop notification was interrupted")))
                  (when notified-p
                    (let (published-p)
                      ;; Once the backend returned an id, every exit before
                      ;; publication must retire it (unless a newer exact op
                      ;; has already claimed the same backend identifier).
                      (unwind-protect
                          (progn
                            (unless
                                (disco-notifications--display-current-p
                                 app generation display-op)
                              (throw 'stale-display nil))
                            (ring-insert
                             (disco-notifications--history-ring)
                             (copy-tree message))
                            (unless
                                (disco-notifications--display-current-p
                                 app generation display-op)
                              (throw 'stale-display nil))
                            (setq disco-notifications--last-id returned-id
                                  disco-notifications--last-display-op
                                  (and returned-id display-op)
                                  published-p t)
                            (when (and disco-notifications-timeout returned-id)
                              (disco-notifications--schedule-timeout
                               returned-id app generation display-op)))
                        (unless published-p
                          (disco-notifications--close-returned-id
                           returned-id display-op)))))))))))))))

(defun disco-notifications--deliver-delayed (owner message)
  "Deliver MESSAGE only while delayed OWNER belongs to this account."
  (when (disco-notifications--owner-current-p
         owner disco-notifications--delay-owners)
    (setq disco-notifications--delay-owners
          (delq owner disco-notifications--delay-owners))
    (disco-notifications--cancel-owner-timer owner)
    (disco-notifications--show
     message (plist-get owner :app) (plist-get owner :generation))))

(defun disco-notifications--schedule-delayed (message)
  "Schedule one account-owned delayed notification for MESSAGE."
  (let* ((app disco-runtime--app)
         (generation disco-notifications--generation)
         (owner (list :app app :generation generation
                      :timer nil :handle nil)))
    (when (and disco-notifications-mode
               (disco-notifications--session-current-p app generation))
      (push owner disco-notifications--delay-owners)
      (condition-case err
          (disco-notifications--register-owner-timer
           owner
           (run-with-timer disco-notifications-delay nil
                           #'disco-notifications--deliver-delayed
                           owner (copy-tree message)))
        (error
         (setq disco-notifications--delay-owners
               (delq owner disco-notifications--delay-owners))
         (disco-notifications--cancel-owner-timer owner)
         (message "disco: failed to schedule delayed notification: %s"
                  (error-message-string err)))
        (quit
         (setq disco-notifications--delay-owners
               (delq owner disco-notifications--delay-owners))
         (disco-notifications--cancel-owner-timer owner)
         (message "disco: delayed notification scheduling was interrupted")))
      owner)))

(defun disco-notifications--handle-event (event)
  "Schedule a notification for one Gateway EVENT."
  (let ((app disco-runtime--app)
        (generation disco-notifications--generation))
    (when (and disco-notifications-mode
               (disco-notifications--session-current-p app generation))
      (when (eq (plist-get event :type) 'ready)
        (clrhash disco-notifications--seen)
        (setq disco-notifications--seen-order nil))
      (when (eq (plist-get event :type) 'message-create)
        (when-let* ((message (plist-get event :message))
                    (message-id
                     (disco-msg-normalize-id (alist-get 'id message))))
          (unless (gethash message-id disco-notifications--seen)
            (disco-notifications--remember message-id)
            (if (> disco-notifications-delay 0)
                (disco-notifications--schedule-delayed message)
              (disco-notifications--show message app generation))))))))

;;;###autoload
(define-minor-mode disco-notifications-mode
  "Toggle visibility-aware Discord desktop notifications."
  :global t :group 'disco-notifications
  (if disco-notifications-mode
      (progn
        (add-hook 'disco-gateway-event-hook
                  #'disco-notifications--handle-event)
        (add-hook 'disco-state-reset-hook
                  #'disco-notifications-reset-session-state))
    (remove-hook 'disco-gateway-event-hook
                 #'disco-notifications--handle-event)
    (remove-hook 'disco-state-reset-hook
                 #'disco-notifications-reset-session-state)
    (disco-notifications--revoke-async-work)))

;;;###autoload
(defun disco-notifications-history ()
  "Show recent disco desktop notifications."
  (interactive)
  (let ((buffer (disco-notifications--owned-history-buffer))
        (app disco-runtime--app)
        (generation disco-notifications--generation))
    (setq disco-notifications--history-buffer buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq-local disco-notifications--history-owner-p t)
        (dolist (message (ring-elements (disco-notifications--history-ring)))
          (let* ((channel-id (alist-get 'channel_id message))
                 (message-id (alist-get 'id message))
                 (channel (disco-state-channel channel-id)))
            (insert-text-button
             (disco-notifications--title message channel)
             'follow-link t
             'action (lambda (_button)
                       (when (disco-notifications--session-current-p
                              app generation)
                         (disco-notifications-open-message
                          channel-id message-id))))
            (insert (format "  %s\n" (disco-notifications--body message)))))))
    (pop-to-buffer buffer)))

(provide 'disco-notifications)

;;; disco-notifications.el ends here
