;;; disco-room.el --- Channel room buffers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Per-channel room lifecycle, history, Gateway dispatch, projection, and UI.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'seq)
(require 'cl-lib)
(require 'ewoc)
(require 'plz)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-media)
(require 'appkit-chat-history)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-ui)
(require 'appkit-scroll)
(require 'disco-customize)
(require 'disco-msg)
(require 'disco-thread)
(require 'disco-typing)
(require 'disco-markdown)
(require 'disco-media)
(require 'disco-avatar)
(require 'disco-emoji-image)
(require 'disco-sticker)
(require 'disco-embed)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-permission)
(require 'disco-company)
(require 'disco-room-compose)
(require 'disco-room-search)
(require 'disco-room-thread)
(require 'disco-room-poll)
(require 'disco-room-reaction)
(require 'disco-room-pin)
(require 'disco-room-render)
(require 'disco-runtime)

(declare-function disco-transient-msg-operate "disco-transient" ())
(declare-function disco-room-transient "disco-transient" ())
(declare-function disco-room-input-options-transient "disco-transient" ())
(declare-function appkit-translate-enable "appkit-translate" (owner &optional notify))
(declare-function appkit-translate-request-many "appkit-translate"
                  (sources &optional backend language force notify-or-surface surface))

(declare-function disco-api--validate-message-content-length "disco-api-normalize"
                  (content field-name))
(declare-function disco-company--teardown-room-buffer "disco-company" ())
(defvar disco-api--message-content-limit)

;;; Room state

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--remote-latest-message-id nil
  "Newest canonical Discord message observed for this room.

This protocol frontier stays separate from AppKit's nil newer edge, whose
meaning is only that the projected window is attached to latest.")
(defvar-local disco-room--oldest-message-id nil
  "Oldest canonical message in the currently visible history window.

Retained as the search boundary cache; pagination ownership and exhaustion
live exclusively in `appkit-chat-history'.")
(defvar-local disco-room--newest-message-id nil
  "Newest canonical message in the currently visible history window.

This is a search boundary, not the remote/latest protocol frontier.")
(defvar-local disco-room--pending-jump-message-id nil)
(defvar-local disco-room--gateway-handler nil)
(defvar-local disco-room--live-update-handle nil
  "Appkit lifecycle handle owning this room's gateway hook and watch.")
(defvar-local disco-room--typing-users nil)
(defvar-local disco-room--typing-expire-timer nil)
(defvar-local disco-room--revealed-spoiler-message-id nil)
(defvar-local disco-room--optimistic-read-ack-seq 0)
(defvar-local disco-room--pending-optimistic-read-ack nil)
(defvar-local disco-room--scroll-observer nil)
(defvar-local disco-room--media-status nil
  "Media presentation status derived from the committed Surface model.")

(defconst disco-room--message-flag-has-thread (ash 1 5)
  "Bit mask indicating message has an associated starter thread.")

(defconst disco-room--message-flag-has-snapshot (ash 1 14)
  "Bit mask indicating message carries a forward snapshot payload.")

;;; Interaction maps

(defvar-keymap disco-room-timeline-mode-map
  :doc "Timeline-only keymap active when point is outside the room draft."
  "q" #'quit-window
  "c" #'disco-msg-copy-dwim
  "l" #'disco-msg-copy-link
  "n" #'disco-msg-next
  "p" #'disco-msg-previous
  "o" #'disco-msg-operate
  "r" #'disco-msg-reply
  "f" #'disco-msg-forward
  "e" #'disco-msg-edit
  "d" #'disco-msg-delete
  "P" #'disco-msg-toggle-pin
  "i" #'disco-msg-describe-message
  "L" #'disco-msg-redisplay
  "!" #'disco-msg-add-reaction
  "?" #'disco-room-transient)

(define-minor-mode disco-room-timeline-mode
  "Buffer-local navigation bindings active outside the room draft."
  :init-value nil
  :lighter nil
  :keymap disco-room-timeline-mode-map)

;;; Room identity and typing

(defun disco-room--buffer-name (channel-name channel-id)
  "Build room buffer name for CHANNEL-NAME and CHANNEL-ID."
  (format "*disco:%s (%s)*" channel-name channel-id))

(defun disco-room--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-room--channel-header-suffix (&optional channel)
  "Return human-readable suffixes for room header CHANNEL."
  (let ((channel (or channel (disco-room--channel-object))))
    (concat
     (if (disco-state-channel-age-restricted-p channel)
         " [18+]"
       "")
     (disco-thread-header-suffix channel))))

(defun disco-room--typing-timeout-seconds ()
  "Return normalized typing indicator timeout in seconds."
  (disco-typing-timeout-seconds disco-room-typing-indicator-timeout))

(defun disco-room--typing-normalize-user-id (user-id)
  "Return USER-ID as string, or nil when missing."
  (disco-typing-normalize-user-id user-id))

(defun disco-room--typing-member-display-name (member)
  "Extract display name from gateway MEMBER payload."
  (when (listp member)
    (let* ((nick (alist-get 'nick member))
           (user (alist-get 'user member))
           (global-name (and (listp user) (alist-get 'global_name user)))
           (username (and (listp user) (alist-get 'username user))))
      (seq-find (lambda (candidate)
                  (and (stringp candidate)
                       (not (string-empty-p candidate))))
                (list nick global-name username)))))

(defun disco-room--typing-channel-recipient-display-name (user-id)
  "Resolve USER-ID display name from current DM/group channel metadata."
  (let* ((channel (disco-room--channel-object))
         (recipients (and (listp channel) (alist-get 'recipients channel)))
         (match
          (seq-find
           (lambda (recipient)
             (and (listp recipient)
                  (equal (disco-room--typing-normalize-user-id
                          (alist-get 'id recipient))
                         user-id)))
           (or recipients '()))))
    (when (listp match)
      (let ((global-name (alist-get 'global_name match))
            (username (alist-get 'username match)))
        (seq-find (lambda (candidate)
                    (and (stringp candidate)
                         (not (string-empty-p candidate))))
                  (list global-name username))))))

(defun disco-room--typing-history-display-name (user-id)
  "Resolve USER-ID display name from loaded room message history."
  (let ((found nil))
    (dolist (msg (or (disco-state-messages disco-room--channel-id) '()))
      (when (and (null found)
                 (equal (disco-room--typing-normalize-user-id
                         (disco-room--message-author-id msg))
                        user-id))
        (let ((candidate (disco-room--message-author msg)))
          (when (and (stringp candidate) (not (string-empty-p candidate)))
            (setq found candidate)))))
    found))

(defun disco-room--typing-display-name (user-id &optional member)
  "Return best-effort display name for typing USER-ID and MEMBER payload."
  (or (disco-room--typing-member-display-name member)
      (let ((existing (and (hash-table-p disco-room--typing-users)
                           (gethash user-id disco-room--typing-users))))
        (and (listp existing)
             (plist-get existing :display-name)))
      (disco-room--typing-channel-recipient-display-name user-id)
      (disco-room--typing-history-display-name user-id)
      (format "user-%s"
              (if (> (length user-id) 4)
                  (substring user-id (- (length user-id) 4))
                user-id))))

(defun disco-room--typing-prune-expired (&optional now)
  "Drop expired typing entries and return non-nil when anything changed."
  (disco-typing-prune-expired disco-room--typing-users now))

(defun disco-room--typing-active-entries ()
  "Return active typing entries sorted by recent typing timestamp."
  (disco-typing-active-entries disco-room--typing-users (float-time)))

(defun disco-room--typing-indicator-text ()
  "Return one-line typing indicator text, or nil when idle."
  (when disco-room-show-typing-indicators
    (disco-typing-indicator-text-from-table
     disco-room--typing-users
     (float-time))))

(defun disco-room--typing-next-expiry ()
  "Return nearest typing expiry timestamp, or nil when none remain."
  (disco-typing-next-expiry disco-room--typing-users))

(defun disco-room--typing-cancel-expire-timer ()
  "Cancel room-local typing expiry timer when active."
  (when (timerp disco-room--typing-expire-timer)
    (cancel-timer disco-room--typing-expire-timer))
  (setq disco-room--typing-expire-timer nil))

(defun disco-room--typing-expire-timer-callback (buffer view)
  "Expire stale typing entries for room BUFFER owned by VIEW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; A room buffer can survive its Generated Surface.  Never let the
      ;; predecessor's timer alter a replacement Surface.
      (when (and (eq view (appkit-current-surface))
                 (appkit-surface-live-p view))
        (setq disco-room--typing-expire-timer nil)
        (when (disco-room--typing-prune-expired)
          (disco-room--queue-update view 'frame))
        (disco-room--typing-reschedule-expire-timer)))))

(defun disco-room--typing-reschedule-expire-timer ()
  "Reschedule room-local timer for the next typing expiry."
  (disco-room--typing-cancel-expire-timer)
  (let ((next-expiry (disco-room--typing-next-expiry)))
    (when next-expiry
      (let ((delay (max 0.1 (- next-expiry (float-time))))
            (room-buffer (current-buffer))
            (view (appkit-current-surface)))
        (when (appkit-surface-live-p view)
          (setq disco-room--typing-expire-timer
                (run-at-time delay nil
                             #'disco-room--typing-expire-timer-callback
                             room-buffer view)))))))

(defun disco-room--typing-reset ()
  "Clear all local typing indicator state for current room."
  (disco-room--typing-cancel-expire-timer)
  (setq disco-room--typing-users (make-hash-table :test #'equal)))

(defun disco-room--typing-track-user (user-id &optional member timestamp)
  "Track typing state for USER-ID with optional MEMBER and TIMESTAMP."
  (when disco-room-show-typing-indicators
    (let* ((normalized-id (disco-room--typing-normalize-user-id user-id))
           (self-id (disco-gateway-current-user-id)))
      (when (and normalized-id
                 (not (equal normalized-id self-id)))
        (unless (hash-table-p disco-room--typing-users)
          (setq disco-room--typing-users (make-hash-table :test #'equal)))
        (let* ((now (float-time))
               (base-time (if (numberp timestamp)
                              (float timestamp)
                            now))
               (expires-at (+ base-time (disco-room--typing-timeout-seconds))))
          (when (> expires-at now)
            (let* ((display-name (disco-room--typing-display-name normalized-id member))
                   (existing (gethash normalized-id disco-room--typing-users))
                   (changed (or (null existing)
                                (not (equal (plist-get existing :display-name) display-name)))))
              (puthash normalized-id
                       (list :user-id normalized-id
                             :display-name display-name
                             :expires-at expires-at
                             :updated-at now)
                       disco-room--typing-users)
              (disco-room--typing-reschedule-expire-timer)
              changed)))))))

(defun disco-room--typing-stop-user (user-id &optional _no-rerender)
  "Remove USER-ID from typing indicators and return non-nil when changed.

This helper is controller-only.  Its caller owns any Appkit invalidation."
  (let ((normalized-id (disco-room--typing-normalize-user-id user-id)))
    (when (and normalized-id
               (hash-table-p disco-room--typing-users)
               (gethash normalized-id disco-room--typing-users))
      (remhash normalized-id disco-room--typing-users)
      (disco-room--typing-reschedule-expire-timer)
      t)))

;;; Controller utilities

(defun disco-room--latest-message-id ()
  "Return newest message ID visible in the current room window, or nil."
  (disco-room--message-id
   (seq-find #'disco-room--canonical-message-p
             (disco-room--display-messages))))

(defun disco-room--plz-error-object (err)
  "Return the plz error object carried by ERR, or nil."
  (cond
   ((and (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p err)))
    err)
   ((and (consp err)
         (symbolp (car err))
         (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p (cadr err))))
    (cadr err))
   (t nil)))

(defun disco-room--plz-error-http-status (err)
  "Return HTTP status from ERR, or nil if unavailable."
  (or (and (listp err) (plist-get err :status))
      (when-let* ((plz-error (disco-room--plz-error-object err)))
        (when (and (fboundp 'plz-error-response)
                   (fboundp 'plz-response-status))
          (let ((response (ignore-errors (plz-error-response plz-error))))
            (and response
                 (ignore-errors (plz-response-status response))))))))

(defun disco-room--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (when-let* ((plz-error (disco-room--plz-error-object err)))
        (let* ((msg (and (fboundp 'plz-error-message)
                         (ignore-errors (plz-error-message plz-error))))
               (status (disco-room--plz-error-http-status err)))
          (cond
           ((and (stringp msg) (not (string-empty-p msg)))
            (if status
                (format "%s (HTTP %s)" msg status)
              msg))
           (status (format "HTTP %s" status))
           (t nil))))
      (condition-case _
          (error-message-string err)
        (error
         (format "%S" err)))))

(defun disco-room--callback-active-p (room-buffer channel-id view)
  "Return non-nil when ROOM-BUFFER is still bound to CHANNEL-ID and VIEW.

History callbacks additionally use AppKit request-owner identity.  Other
operations keep their own protocol-specific sequence/token validation."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (eq view (appkit-current-surface))
              (appkit-surface-live-p view)))))

(defun disco-room--channel-buffer-p (room-buffer channel-id view)
  "Return non-nil when ROOM-BUFFER is still bound to CHANNEL-ID and VIEW.

VIEW is the exact Generated Surface captured when asynchronous work began.
A nil or stopped Surface never degrades this guard to channel identity alone."
  (and (buffer-live-p room-buffer)
       (appkit-surface-live-p view)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (eq view (appkit-current-surface))))))

;;; History and message windows

(defun disco-room--maybe-auto-load-older (&optional position)
  "Load older channel history when POSITION approaches the timeline top."
  (when (and disco-room--channel-id
             (not (disco-room--msg-filter-active-p))
             (not (appkit-chatbuf-point-in-input-p))
             (appkit-chat-history-autoload-older-p
              (or position (point)) (point-min)
              disco-room-history-auto-load-threshold))
    (disco-room-load-older-messages t)))

(defun disco-room--maybe-auto-load-newer (&optional position)
  "Load newer history when POSITION approaches a partial window's footer."
  (let ((position (or position (point)))
        (footer (or (appkit-chat-timeline-footer-start-position)
                    (appkit-chatbuf-input-start-position)
                    (point-max))))
    (when (and disco-room--channel-id
               (not (disco-room--msg-filter-active-p))
               (appkit-chat-history-autoload-newer-p
                position footer disco-room-history-auto-load-threshold
                (appkit-chatbuf-composer-idle-p)))
      (disco-room-load-newer-messages t))))

(defun disco-room--install-scroll-observer (surface)
  "Install SURFACE's lifecycle-owned history edge observer."
  (unless (and (appkit-scroll-observer-p disco-room--scroll-observer)
               (appkit-scroll-observer-active-p
                disco-room--scroll-observer)
               (eq surface
                   (appkit-scroll-observer-owner
                    disco-room--scroll-observer)))
    (when (appkit-scroll-observer-p disco-room--scroll-observer)
      (appkit-scroll-observer-cancel disco-room--scroll-observer))
    (setq-local
     disco-room--scroll-observer
     (appkit-scroll-observer-install
      surface
      :end-boundary-function #'appkit-chat-timeline-footer-start-position
      :start-function
      (lambda (_window position _start)
        (disco-room--maybe-auto-load-older position))
      :end-function
      (lambda (_window position _end)
        (disco-room--maybe-auto-load-newer position))))))

(defun disco-room--post-command ()
  "Maintain Disco-specific row behavior after each command."
  (unless (appkit-chatbuf-rendering-p)
    (let ((current-message-id (or (get-text-property (point) 'disco-message-id)
                                  (get-text-property (line-beginning-position)
                                                     'disco-message-id))))
      (when (and disco-room--revealed-spoiler-message-id
                 (not (equal current-message-id
                             disco-room--revealed-spoiler-message-id)))
        (let ((previous disco-room--revealed-spoiler-message-id))
          (setq disco-room--revealed-spoiler-message-id nil)
          (when-let* ((view (appkit-current-surface)))
            (when (appkit-surface-live-p view)
              (disco-room--queue-update view (list 'rows-changed (list previous))))))))))

(defun disco-room--read-state-snapshot-fields (state)
  "Return writable read-state fields copied from STATE."
  (let (fields)
    (dolist (field '(last_message_id
                     mention_count
                     last_pin_timestamp
                     flags
                     last_viewed
                     version))
      (when (assq field state)
        (push (cons field (alist-get field state)) fields)))
    (nreverse fields)))

(defun disco-room--restore-channel-read-state (channel-id state ack-token)
  "Restore CHANNEL-ID read STATE and ACK-TOKEN snapshot."
  (if state
      (disco-state--upsert-read-state
       disco-read-state-type-channel
       channel-id
       (disco-room--read-state-snapshot-fields state))
    (progn
      (disco-state--delete-read-state disco-read-state-type-channel channel-id)
      (when ack-token
        (disco-state-set-channel-ack-token channel-id ack-token)))))

(defun disco-room--optimistic-read-ack-begin (channel-id target-id ack-fields)
  "Apply optimistic read ACK for CHANNEL-ID/TARGET-ID and return op seq."
  (let ((seq (1+ disco-room--optimistic-read-ack-seq)))
    (setq disco-room--optimistic-read-ack-seq seq)
    (setq disco-room--pending-optimistic-read-ack
          (list :seq seq
                :channel-id channel-id
                :target-id target-id
                :previous-state (disco-state-read-state
                                 disco-read-state-type-channel channel-id)
                :previous-token (disco-state-channel-ack-token channel-id)))
    (disco-state-apply-message-ack
     channel-id
     target-id
     0
     (plist-get ack-fields :flags)
     (plist-get ack-fields :last-viewed))
    seq))

(defun disco-room--optimistic-read-ack-clear (seq)
  "Clear pending optimistic read ACK when it matches SEQ."
  (when (and (listp disco-room--pending-optimistic-read-ack)
             (= (or (plist-get disco-room--pending-optimistic-read-ack :seq) -1)
                seq))
    (setq disco-room--pending-optimistic-read-ack nil)
    t))

(defun disco-room--optimistic-read-ack-confirm (message-id)
  "Confirm pending optimistic read ACK using MESSAGE-ID from gateway/server."
  (when (and (listp disco-room--pending-optimistic-read-ack)
             (stringp message-id))
    (let ((target-id (plist-get disco-room--pending-optimistic-read-ack :target-id)))
      (when (and (stringp target-id)
                 (or (equal message-id target-id)
                     (disco-state-snowflake< target-id message-id)))
        (setq disco-room--pending-optimistic-read-ack nil)
        t))))

(defun disco-room--optimistic-read-ack-rollback (seq)
  "Rollback pending optimistic read ACK when it still matches SEQ."
  (when (and (listp disco-room--pending-optimistic-read-ack)
             (= (or (plist-get disco-room--pending-optimistic-read-ack :seq) -1)
                seq))
    (let ((channel-id (plist-get disco-room--pending-optimistic-read-ack :channel-id))
          (previous-state (plist-get disco-room--pending-optimistic-read-ack :previous-state))
          (previous-token (plist-get disco-room--pending-optimistic-read-ack :previous-token)))
      (setq disco-room--pending-optimistic-read-ack nil)
      (disco-room--restore-channel-read-state channel-id previous-state previous-token)
      t)))

(defun disco-room--mark-read (&optional message-id defer-sync-p)
  "Mark current room as read and acknowledge MESSAGE-ID.

When MESSAGE-ID is nil, acknowledge the newest visible message in the room.
Unread counters are always cleared locally.  When DEFER-SYNC-P is non-nil, the
caller is already inside, or will request, an Appkit projection transaction."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         (channel (disco-room--channel-object))
         (target-id (or message-id
                        (disco-room--latest-message-id)
                        (and (not (appkit-chat-history-window-known-p))
                             channel
                             (alist-get 'last_message_id channel))))
         (last-read-id (disco-state-channel-last-read-message-id channel-id))
         (should-ack (and target-id
                          (or (null last-read-id)
                              (disco-state-snowflake< last-read-id target-id)))))
    (if should-ack
        (let* ((ack-fields (disco-state-channel-ack-request-fields channel-id))
               (optimistic-seq (disco-room--optimistic-read-ack-begin
                                channel-id target-id ack-fields)))
          (disco-api-ack-message-async
           channel-id
           target-id
           :token (plist-get ack-fields :token)
           :flags (plist-get ack-fields :flags)
           :last-viewed (plist-get ack-fields :last-viewed)
           :on-success
           (lambda (response)
             (when (disco-room--callback-active-p room-buffer channel-id view)
               (with-current-buffer room-buffer
                 ;; A later optimistic ACK supersedes this captured response.
                 ;; Gate both the message frontier and token update on the
                 ;; operation sequence so older successes cannot regress them.
                 (when (disco-room--optimistic-read-ack-clear optimistic-seq)
                   (disco-state-apply-message-ack channel-id target-id 0)
                   (disco-state-apply-channel-ack-response channel-id response)
                   (disco-room--queue-update view 'timeline)))))
           :on-error
           (lambda (err)
             (when (disco-room--callback-active-p room-buffer channel-id view)
               (with-current-buffer room-buffer
                 (when (disco-room--optimistic-read-ack-rollback optimistic-seq)
                   (disco-room--queue-update view 'timeline))
                 (message "disco: read-state ack failed for %s: %s"
                          channel-id
                          (disco-room--async-error-message err)))))))
      (disco-state-apply-message-ack channel-id nil 0))
    (unless defer-sync-p
      (disco-room--queue-update view 'timeline)

      ;; This state transition is also used as an explicit local action in
      ;; tests/commands.  Consume its invalidation through Appkit, never by
      ;; calling the timeline projector directly.
      (disco-room--flush-updates view))))

(defun disco-room--pending-message-p (message)
  "Return non-nil when MESSAGE is a local optimistic row."
  (and (listp message) (alist-get 'pending message)))

(defun disco-room--message-id (message)
  "Return MESSAGE's normalized stable id, or nil."
  (disco-msg-normalize-id (and (listp message) (alist-get 'id message))))

(defun disco-room--canonical-message-p (message)
  "Return non-nil when MESSAGE has a canonical non-pending Discord id."
  (and (disco-room--message-id message)
       (not (disco-room--pending-message-p message))))

(defun disco-room--normalize-history-page (messages)
  "Return transport MESSAGES explicitly normalized newest-first."
  (disco-room--sort-messages-newest-first
   (seq-filter #'disco-room--canonical-message-p (or messages '()))))

(defun disco-room--history-page-bounds (messages)
  "Return (OLDEST . NEWEST) ids for newest-first MESSAGES."
  (cons (disco-room--message-id (car (last messages)))
        (disco-room--message-id (car messages))))

(defun disco-room--canonical-cache-newest-first (&optional channel-id)
  "Return CHANNEL-ID canonical cache explicitly sorted newest-first."
  (disco-room--normalize-history-page
   (disco-state-messages (or channel-id disco-room--channel-id))))

(defun disco-room--canonical-cache-oldest-first (&optional channel-id)
  "Return CHANNEL-ID canonical cache explicitly sorted oldest-first."
  (reverse (disco-room--canonical-cache-newest-first channel-id)))

(defun disco-room--history-page-retained-in-cache (page cache)
  "Return PAGE entries whose ids remain canonical in CACHE.

PAGE and CACHE use newest-first order.  The returned objects come from CACHE,
so a concurrent Gateway update wins over a stale REST copy while PAGE still
defines which response entries may move exact history edges."
  (let ((canonical-by-id (make-hash-table :test #'equal)))
    (dolist (message (disco-room--normalize-history-page cache))
      (puthash (disco-room--message-id message) message canonical-by-id))
    (delq nil
          (mapcar
           (lambda (message)
             (gethash (disco-room--message-id message) canonical-by-id))
           page))))

(defun disco-room--message-id-after-p (candidate-id reference-id messages)
  "Return non-nil when CANDIDATE-ID follows REFERENCE-ID in MESSAGES.

MESSAGES must be canonical oldest-first order.  Missing ids are unknown and
therefore never count as progress."
  (let ((candidate-index
         (seq-position messages candidate-id
                       (lambda (message id)
                         (equal (disco-room--message-id message) id))))
        (reference-index
         (seq-position messages reference-id
                       (lambda (message id)
                         (equal (disco-room--message-id message) id)))))
    (and candidate-index reference-index
         (> candidate-index reference-index))))

(defun disco-room--newest-canonical-id-among (ids messages)
  "Return newest member of IDS in newest-first canonical MESSAGES."
  (when-let* ((wanted (delq nil (copy-sequence ids)))
              (message
               (seq-find
                (lambda (candidate)
                  (member (disco-room--message-id candidate) wanted))
                messages)))
    (disco-room--message-id message)))

(defun disco-room--observe-live-create (message-id)
  "Observe canonical live MESSAGE-ID without widening a partial window."
  (when-let* ((id (disco-msg-normalize-id message-id)))
    (when (or (null disco-room--remote-latest-message-id)
              (disco-state-snowflake<
               disco-room--remote-latest-message-id id))
      (setq disco-room--remote-latest-message-id id)
      ;; A prior no-progress response at the same partial edge no longer
      ;; proves that another automatic attempt would stall.
      (appkit-chat-history-newer-stalled-clear))
    (when (and (equal id disco-room--remote-latest-message-id)
               (appkit-chat-history-window-empty-p))
      (appkit-chat-history-window-seed-live id))
    id))

(defun disco-room--repair-history-window-after-delete (message-id)
  "Move exact edges inward after canonical MESSAGE-ID was deleted.

The pre-event timeline keys are the only proved members of the old continuous
window; unrelated canonical cache islands are never used as replacements."
  (when (and (appkit-chat-history-window-known-p)
             (not (appkit-chat-history-window-empty-p))
             (appkit-chat-timeline-live-p))
    (let* ((id (disco-msg-normalize-id message-id))
           (first (appkit-chat-history-window-first-key))
           (last (appkit-chat-history-window-last-key))
           (cached-ids
            (mapcar #'disco-room--message-id
                    (disco-state-messages disco-room--channel-id)))
           (remaining
            (seq-filter
             (lambda (key)
               (and (not (equal key id)) (member key cached-ids)))
             (appkit-chat-timeline-keys))))
      (cond
       (remaining
        (appkit-chat-history-window-set
         (if (equal id first) (car remaining) first)
         (if (and last (equal id last)) (car (last remaining)) last))
        (when (equal id disco-room--remote-latest-message-id)
          (setq disco-room--remote-latest-message-id
                (and (null last) (car (last remaining))))))
       ((and (null last) (appkit-chat-history-older-loaded-p))
        (setq disco-room--remote-latest-message-id nil)
        (appkit-chat-history-window-establish-empty))
       (t
        (when (equal id disco-room--remote-latest-message-id)
          (setq disco-room--remote-latest-message-id nil))
        (appkit-chat-history-window-clear))))))

(defun disco-room--establish-latest-history-window
    (page frontier-at-start response-count request-limit)
  "Establish authoritative latest history from PAGE.

FRONTIER-AT-START distinguishes a live create delivered while the request was
in flight.  RESPONSE-COUNT is the raw transport page length; REQUEST-LIMIT
uses that count to prove completion without mistaking Gateway-conflict drops
for a short page.  PAGE contains only response entries retained in canonical
state and is normalized newest-first."
  (let* ((current-frontier disco-room--remote-latest-message-id)
         (live-frontier
          (and current-frontier
               (not (equal current-frontier frontier-at-start))
               current-frontier))
         (canonical (disco-room--canonical-cache-newest-first))
         (bounds (disco-room--history-page-bounds page))
         (oldest (car bounds))
         (page-newest (cdr bounds)))
    (cond
     (page-newest
      (setq disco-room--remote-latest-message-id
            (or (disco-room--newest-canonical-id-among
                 (list page-newest live-frontier) canonical)
                page-newest))
      (appkit-chat-history-older-loaded-set nil)
      (appkit-chat-history-window-set oldest nil)
      (when (< response-count request-limit)
        (appkit-chat-history-older-loaded-set t))
      'established)
     ((>= response-count request-limit)
      ;; Every full-page response entry lost a revision race.  It cannot prove
      ;; an empty channel.  A concurrently observed live frontier can still
      ;; establish a one-entry latest window whose older side remains open.
      (if live-frontier
          (progn
            (setq disco-room--remote-latest-message-id live-frontier)
            (appkit-chat-history-older-loaded-set nil)
            (appkit-chat-history-window-set live-frontier nil)
            'established)
        (appkit-chat-history-window-clear)
        'conflicted))
     (live-frontier
      ;; The REST snapshot was empty, then Gateway/API delivery created the
      ;; first canonical row before its callback completed.
      (setq disco-room--remote-latest-message-id live-frontier)
      (appkit-chat-history-window-establish-empty)
      (appkit-chat-history-window-seed-live live-frontier)
      'established)
     (t
      (setq disco-room--remote-latest-message-id nil)
      (appkit-chat-history-window-establish-empty)
      'empty))))

(defun disco-room--message-id-at-point ()
  "Return message ID at point, or signal a user error.

Message lines carry the `disco-message-id' text property."
  (or (get-text-property (point) 'disco-message-id)
      (get-text-property (line-beginning-position) 'disco-message-id)
      (user-error "disco: point is not on a message line")))

(defun disco-room--message-spoilers-revealed-p (message-id)
  "Return non-nil when MESSAGE-ID currently shows revealed spoilers."
  (and (stringp message-id)
       (equal message-id disco-room--revealed-spoiler-message-id)))

(defun disco-room--invalidate-message-node (message-id)
  "Invalidate the rendered node for MESSAGE-ID when present."
  (when (and message-id
             (appkit-chat-timeline-live-p)
             (appkit-chat-timeline-node message-id))
    (appkit-chat-timeline-invalidate (list message-id))))

(defun disco-room--redisplay-msg (msg)
  "Force MSG to be rerendered in the current room."
  (let ((message-id (and (listp msg) (alist-get 'id msg))))
    (unless (and (stringp message-id) (not (string-empty-p message-id)))
      (user-error "disco: message has no id to redisplay"))
    (disco-room--invalidate-message-node message-id)))

(defun disco-room--translate-messages (messages)
  "Request translations of MESSAGES with text, leaving originals and draft intact."
  (let* ((surface (appkit-current-surface))
         (buffer (current-buffer))
         (channel-id disco-room--channel-id)
         sources)
    (unless (disco-room--callback-active-p buffer channel-id surface)
      (user-error "disco: translation requires a live room"))
    (dolist (msg messages)
      (unless (disco-room--message-system-divider-p msg)
        (let ((source (disco-room--translation-source msg t)))
          (unless (string-empty-p (string-trim (plist-get source :text)))
            (push source sources)))))
    (unless sources
      (user-error "disco: no text to translate in these messages"))
    (require 'appkit-translate)
    (appkit-translate-enable
     surface
     (lambda (key)
       (when (disco-room--callback-active-p buffer channel-id surface)
         (disco-room--queue-update surface (list 'rows-changed (list (nth 2 key)))))))
    (appkit-translate-request-many (nreverse sources))))

(defun disco-room--messages-in-range (begin end)
  "Return distinct messages intersecting the half-open range BEGIN to END.
Use exact row properties, not nearby-message fallback at an empty boundary.
Only already rendered messages count; the composer is never included."
  (let ((position begin)
        (end (min end (or (appkit-chatbuf-prompt-start-position) (point-max))))
        (seen (make-hash-table :test #'equal))
        messages)
    (while (< position end)
      (when-let* ((id (get-text-property position 'disco-message-id))
                  (channel (get-text-property position 'disco-message-channel-id))
                  (key (cons channel id))
                  ((not (gethash key seen)))
                  (msg (disco-msg-at position)))
        (puthash key t seen)
        (push msg messages))
      (setq position
            (next-single-property-change position 'disco-message-id nil end)))
    (nreverse messages)))

(defun disco-room-translate-region (begin end)
  "Translate whole messages intersecting the active region from BEGIN to END.
Skip messages without text, including spoiler-only and attachment-only rows.
No history is fetched, and selected composer text is never sent."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (user-error "disco: select a message region first")))
  (disco-room--translate-messages (disco-room--messages-in-range begin end)))

(defun disco-room-translate-visible ()
  "Translate whole messages visible in the selected room window.
Partly visible messages are included.  Capture the range before any translation
can resize rows; do not fetch or translate off-screen history."
  (interactive)
  (let ((window (selected-window)))
    (unless (eq (window-buffer window) (current-buffer))
      (user-error "disco: the room must be displayed in the selected window"))
    (disco-room--translate-messages
     (disco-room--messages-in-range
      (window-start window) (window-end window t)))))

(defun disco-room-translate-message ()
  "Translate the message at point without changing its original or the draft.
Use the shared Appkit backend and target language, loading translation only
on explicit request.  Spoiler bodies are excluded even when revealed."
  (interactive)
  (car (disco-room--translate-messages (list (disco-msg-for-interactive)))))

(defun disco-room-toggle-message-spoilers (message-id)
  "Toggle all rendered spoilers for MESSAGE-ID, telega-style."
  (interactive (list (disco-room--message-id-at-point)))
  (unless (stringp message-id)
    (user-error "disco: invalid spoiler message id"))
  (let ((previous disco-room--revealed-spoiler-message-id))
    (setq disco-room--revealed-spoiler-message-id
          (unless (equal previous message-id)
            message-id))
    (when (and previous (not (equal previous message-id)))
      (disco-room--invalidate-message-node previous))
    (disco-room--invalidate-message-node message-id)))

(defun disco-room--filtered-message-by-id (message-id)
  "Return filtered room message object for MESSAGE-ID, or nil."
  (when-let* ((items (and (listp disco-room--msg-filter)
                          (plist-get disco-room--msg-filter :items))))
    (seq-find (lambda (message)
                (equal (alist-get 'id message) message-id))
              items)))

(defun disco-room--message-by-id (message-id)
  "Return room message object for MESSAGE-ID, or nil."
  (or (disco-msg-find-in-channel disco-room--channel-id message-id)
      (disco-room--filtered-message-by-id message-id)))

(defun disco-room--channel-message-by-id (channel-id message-id)
  "Return cached MESSAGE-ID from CHANNEL-ID, or nil."
  (disco-msg-find-in-channel channel-id message-id))

(defun disco-room--resolve-message (message-id &optional channel-id _position)
  "Resolve MESSAGE-ID in current room context, optionally using CHANNEL-ID."
  (let ((target-channel-id (disco-msg-normalize-id (or channel-id disco-room--channel-id))))
    (if (and target-channel-id
             (equal target-channel-id
                    (disco-msg-normalize-id disco-room--channel-id)))
        (disco-room--message-by-id message-id)
      (disco-room--channel-message-by-id target-channel-id message-id))))

(defun disco-room--display-messages ()
  "Return current room rows using the existing newest-first contract.

The canonical cache is first converted to oldest-first and strictly sliced by
AppKit.  Only then is the selected window converted back to Disco's historical
newest-first render contract.  Local pending rows join attached latest windows
(including authoritative empty) but never cross a partial around-window edge."
  (if (disco-room--msg-filter-active-p)
      (or (plist-get disco-room--msg-filter :items) '())
    (let* ((cache (or (disco-state-messages disco-room--channel-id) '()))
           (canonical-oldest
            (reverse
             (disco-room--normalize-history-page cache)))
           (slice
            (appkit-chat-history-window-slice
             canonical-oldest #'disco-room--message-id)))
      (if (not (plist-get slice :valid-p))
          nil
        (let* ((selected (plist-get slice :entries))
               (pending
                (and (not (appkit-chat-history-window-partial-p))
                     (seq-filter #'disco-room--pending-message-p cache))))
          (disco-room--sort-messages-newest-first
           (append selected pending)))))))

(defun disco-room--sync-visible-window-cursors (messages)
  "Update search-compatible cursors from newest-first visible MESSAGES."
  (unless (disco-room--msg-filter-active-p)
    (let ((canonical (seq-filter #'disco-room--canonical-message-p messages)))
      (setq disco-room--newest-message-id
            (disco-room--message-id (car canonical))
            disco-room--oldest-message-id
            (disco-room--message-id (car (last canonical)))))))

(defun disco-room--message-position (message-id)
  "Return buffer position for MESSAGE-ID in current room render, or nil."
  (when (and (stringp message-id)
             (not (string-empty-p message-id))
             (appkit-chat-timeline-live-p))
    (appkit-chat-timeline-key-position message-id)))

(defun disco-room--message-list-contains-id-p (messages message-id)
  "Return non-nil when MESSAGES contains MESSAGE-ID."
  (seq-some (lambda (message)
              (equal (alist-get 'id message) message-id))
            (or messages '())))

(defun disco-room--sort-messages-newest-first (messages)
  "Return MESSAGES sorted newest-first by Discord snowflake id."
  (sort (copy-sequence (or messages '()))
        (lambda (left right)
          (let ((left-id (alist-get 'id left))
                (right-id (alist-get 'id right)))
            (cond
             ((equal left-id right-id)
              nil)
             ((null left-id)
              nil)
             ((null right-id)
              t)
             (t
              (disco-state-snowflake< right-id left-id)))))))

(defun disco-room--merge-message-sets (&rest message-lists)
  "Merge MESSAGE-LISTS into one newest-first list without duplicates."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (messages message-lists)
      (dolist (message (or messages '()))
        (let ((message-id (alist-get 'id message)))
          (unless (and message-id (gethash message-id seen))
            (when message-id
              (puthash message-id t seen))
            (push message merged)))))
    (disco-room--sort-messages-newest-first (nreverse merged))))

(defun disco-room--fetch-around-pending-jump ()
  "Fetch one message page around current pending jump target."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         (target-id disco-room--pending-jump-message-id)
         (request-revision (disco-state-message-revision channel-id))
         (limit (max 1 (or disco-room-jump-context-limit 50)))
         owner)
    (unless (and (stringp target-id) (not (string-empty-p target-id)))
      (user-error "disco: pending jump target is empty"))
    (setq owner (appkit-chat-history-request-start view 'around))
    (appkit-chat-history-older-loaded-set nil)
    (appkit-chat-history-newer-stalled-clear)
    (disco-room--queue-update view 'frame)

    (appkit-chat-history-request-bind-handle
     owner
     (disco-api-channel-messages-around-async
      channel-id
      target-id
      :limit limit
      :owner view
      :on-success
      (lambda (messages)
        (when (disco-room--callback-active-p room-buffer channel-id view)
          (with-current-buffer room-buffer
            (when (appkit-chat-history-request-end owner)
              (let* ((raw-page (disco-room--normalize-history-page messages))
                     (merged
                      (disco-state-merge-message-page
                       channel-id raw-page request-revision))
                     (page
                      (disco-room--history-page-retained-in-cache
                       raw-page merged)))
                (if (disco-room--message-list-contains-id-p page target-id)
                    (let* ((bounds (disco-room--history-page-bounds page))
                           (oldest (car bounds))
                           (newest (cdr bounds))
                           (canonical-oldest
                            (reverse
                             (disco-room--normalize-history-page merged)))
                           (canonical-newest
                            (disco-room--message-id
                             (car (disco-room--normalize-history-page merged))))
                           (frontier disco-room--remote-latest-message-id)
                           (at-latest
                            (and frontier
                                 (equal frontier newest)
                                 (equal canonical-newest newest)))
                           (stale-frontier
                            (and frontier
                                 (not at-latest)
                                 (disco-room--message-id-after-p
                                  canonical-newest frontier
                                  canonical-oldest))))
                      (when stale-frontier
                        (setq disco-room--remote-latest-message-id nil))
                      (appkit-chat-history-window-set
                       oldest (unless at-latest newest))
                      (appkit-chat-history-older-loaded-set nil)
                      (disco-room--request-render view))
                  (setq disco-room--pending-jump-message-id nil)
                  (disco-room--request-render view)
                  (message "disco: message %s not found in around fetch"
                           target-id)))))))
      :on-error
      (lambda (err)
        (when (disco-room--callback-active-p room-buffer channel-id view)
          (with-current-buffer room-buffer
            (when (appkit-chat-history-request-end owner)
              (setq disco-room--pending-jump-message-id nil)
              (disco-room--request-render view)
              (message "disco: jump fetch failed: %s"
                       (disco-room--async-error-message err))))))))))

(defun disco-room--jump-to-visible-message (message-id)
  "Jump to visible MESSAGE-ID in current room buffer and recenter.

Return non-nil when jump succeeds without fetching older history."
  (let ((pos (disco-room--message-position message-id)))
    (when (number-or-marker-p pos)
      (goto-char pos)
      (when-let* ((win (get-buffer-window (current-buffer) t)))
        (set-window-point win pos)
        (with-selected-window win
          (goto-char pos)
          (recenter)))
      t)))

(defun disco-room--resolve-pending-jump ()
  "Resolve a pending jump only when its row is now visible."
  (when (and (stringp disco-room--pending-jump-message-id)
             (not (string-empty-p disco-room--pending-jump-message-id))
             (disco-room--jump-to-visible-message
              disco-room--pending-jump-message-id))
    (let ((target disco-room--pending-jump-message-id))
      (setq disco-room--pending-jump-message-id nil)
      (message "disco: jumped to message %s" target))))

(defun disco-room--queue-jump (message-id surface)
  "Record a jump to MESSAGE-ID for the originating SURFACE."
  (when (and (appkit-surface-live-p surface)
             (eq surface (appkit-current-surface)))
    (setq disco-room--pending-jump-message-id
          (disco-msg-normalize-id message-id))
    (if (disco-room--jump-to-visible-message
         disco-room--pending-jump-message-id)
        (progn
          (setq disco-room--pending-jump-message-id nil)
          (disco-room--queue-update surface 'position))
      (unless (eq (appkit-chat-history-loading) 'around)
        (disco-room--fetch-around-pending-jump)))))

(defun disco-room--jump-required-permissions (channel)
  "Return channel permissions required to jump into CHANNEL.

Guild channels require both visibility and read-history access."
  (when (and (listp channel)
             (alist-get 'guild_id channel)
             (not (disco-state-private-channel-p channel)))
    '(view-channel read-message-history)))

(defun disco-room--resolve-target-channel (channel-id)
  "Return channel object for CHANNEL-ID, fetching when not indexed locally."
  (or (disco-state-channel channel-id)
      (let ((fetched
             (condition-case err
                 (disco-api-channel channel-id)
               (error
                (user-error
                 "disco: cannot fetch jump target channel %s: %s"
                 channel-id
                 (disco-room--async-error-message err))))))
        (when (and fetched (listp fetched))
          (disco-state-upsert-channel fetched))
        fetched)))

(defun disco-room--ensure-jump-permissions (channel-id channel)
  "Signal user error when jump target CHANNEL-ID cannot be viewed/read."
  (unless (and channel (listp channel))
    (user-error "disco: cannot resolve jump target channel %s" channel-id))
  (let ((required (disco-room--jump-required-permissions channel)))
    (when required
      (if (disco-permission-channel-known-p channel)
          (disco-permission-ensure-channel
           channel
           required
           :unknown-value nil
           :action (format "jump target channel %s" channel-id))
        ;; Fallback probe: if computed permissions are missing, a 1-message fetch
        ;; verifies effective read access before opening the target room.
        (condition-case err
            (disco-api-channel-messages channel-id nil 1)
          (error
           (user-error
            "disco: cannot access jump target channel %s: %s"
            channel-id
            (disco-room--async-error-message err))))))))

(defun disco-room-jump-to-message (message-id &optional channel-id)
  "Jump to MESSAGE-ID, optionally in CHANNEL-ID.

When MESSAGE-ID is not currently visible, fetch one page centered around the
message id and render that context before jumping."
  (interactive
   (list (read-string "Jump to message ID: "
                      (or (ignore-errors (disco-room--message-id-at-point))
                          ""))
         nil))
  (let* ((target-id (disco-msg-normalize-id message-id))
         (target-channel (disco-msg-normalize-id (or channel-id disco-room--channel-id)))
         (current-channel (disco-msg-normalize-id disco-room--channel-id)))
    (unless (and (stringp target-id) (not (string-empty-p target-id)))
      (user-error "disco: message id is empty"))
    (if (or (null target-channel) (equal target-channel current-channel))
        (let ((view (disco-room--ensure-surface)))
          (disco-room--queue-jump target-id view)
          ;; Explicit commands may consume the request immediately, while all
          ;; actual projection and positioning still runs through room sync.
          (disco-room--flush-updates view))
      (let* ((target-chan-obj (disco-room--resolve-target-channel target-channel))
             (target-name (or (and (listp target-chan-obj)
                                   (alist-get 'name target-chan-obj))
                              target-channel)))
        (disco-room--ensure-jump-permissions target-channel target-chan-obj)
        ;; Appkit owns room identity.  A live room buffer may have been renamed,
        ;; so use the actual buffer returned by the view opener instead of
        ;; reconstructing and looking up its original display name.
        (when-let* ((target-buffer (disco-room-open target-channel target-name)))
          (with-current-buffer target-buffer
            (let ((view (disco-room--ensure-surface)))
              (disco-room--queue-jump target-id view)
              (disco-room--flush-updates view))))))))

(defun disco-room--message-flags (msg)
  "Return normalized integer flags value from message MSG."
  (let ((flags (alist-get 'flags msg)))
    (cond
     ((integerp flags) flags)
     ((and (stringp flags)
           (string-match-p "\\`[0-9]+\\'" flags))
      (string-to-number flags))
     (t 0))))

(defun disco-room--message-at-point ()
  "Return message object at point, or signal user error."
  (or (disco-msg-at)
      (user-error "disco: message not found in local room cache")))

(defun disco-room--resolve-thread-update (updated)
  "Store complete UPDATED thread channel response."
  (disco-thread-resolve-update
   updated
   (lambda (channel)
     (when (alist-get 'name channel)
       (setq disco-room--channel-name (alist-get 'name channel))))))

(defun disco-room--event-self-p (event)
  "Return frozen current-user identity for reaction/poll EVENT.

New Gateway events carry `:self-p' because queued events can outlive the READY
session that supplied `disco-gateway-current-user-id'.  Events without that marker fall back to the captured Gateway session identity."
  (if (plist-member event :self-p)
      (and (plist-get event :self-p) t)
    (disco-room--same-user-id-p
     (disco-gateway-current-user-id)
     (plist-get event :user-id))))

(defun disco-room--forget-message-async-state (message-id)
  "Discard drafts and operation owners belonging to deleted MESSAGE-ID."
  (disco-room-poll-forget-message message-id)
  (disco-room-reaction-forget-message message-id)
  (disco-room-pin-forget-message message-id))

(defun disco-room--update-message-locally (message-id updater)
  "Apply UPDATER to MESSAGE-ID in controller state and return the new message.

This helper never mutates the projected timeline.  External callbacks request
an Appkit entry sync; gateway events are projected by their enclosing room sync."
  (let* ((messages (or (disco-state-messages disco-room--channel-id) '()))
         (updated-list nil)
         (updated-msg nil))
    (dolist (msg messages)
      (if (and message-id (equal (alist-get 'id msg) message-id))
          (let ((next (funcall updater msg)))
            (push next updated-list)
            (setq updated-msg next))
        (push msg updated-list)))
    (setq updated-list (nreverse updated-list))
    (disco-state-put-messages disco-room--channel-id updated-list)
    updated-msg))

;;; Frame and projection

(defun disco-room--header-text (&optional channel)
  "Build EWOC header text for the current room state."
  (let* ((channel (or channel (disco-room--channel-object)))
         (channel-name (or disco-room--channel-name ""))
         (channel-suffix (disco-room--channel-header-suffix channel))
         (composer-visible-p (disco-room--composer-visible-p channel))
         (context-text (and (not composer-visible-p)
                            (disco-room--input-footer-context-text)))
         (filter-line (disco-room--msg-filter-status-line))
         (composer-status-line (disco-room--composer-hidden-status-line channel))
         (text
          (with-temp-buffer
            (insert (format "Channel: %s%s" channel-name channel-suffix))
            (when disco-room--send-in-flight
              (insert "   [sending...]"))
            (insert "\n")
            (when (and (stringp context-text)
                       (not (string-empty-p context-text)))
              (insert context-text))
            (when (stringp filter-line)
              (insert filter-line "\n"))
            (when (stringp composer-status-line)
              (insert composer-status-line "\n"))
            (insert "\n")
            (buffer-string))))
    (add-text-properties
     0 (length text)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only))
     text)
    text))

(defun disco-room--footer-text (&optional _draft)
  "Build EWOC footer text for the current room state."
  (concat
   (unless (disco-room--msg-filter-active-p)
     (concat
      (appkit-chat-history-delimiter-string
       (max 1 (disco-room--line-fill-column))
       :loading-text "loading…")
      "\n"))
   (when disco-room--media-status
     (concat (propertize disco-room--media-status 'face 'warning) "\n"))
   (disco-room--input-footer-text)))

(defun disco-room--ensure-timeline (&optional channel draft)
  "Ensure current room buffer owns one shared projected timeline."
  (disco-room--ensure-surface)
  (appkit-chat-timeline-ensure
   :printer #'disco-room--ewoc-printer
   :anchor-property 'disco-message-id
   :header (disco-room--header-text channel)
   :footer (disco-room--footer-text draft)
   :after-mutation-function #'appkit-chatbuf-update-context-mode))

(defun disco-room--surface-id ()
  "Return the opaque Appkit Surface identity for the current room."
  (unless disco-room--channel-id
    (error "disco: room buffer has no channel id"))
  (list 'room disco-room--channel-id))

(defun disco-room--enqueue-local-create-response (view channel-id message)
  "Queue canonical create MESSAGE for VIEW's keyed room projection.

The REST create response is authoritative state, but presentation still uses
the same message-create lifecycle as Gateway delivery.  This preserves the
optimistic row node while its nonce key becomes the server message id."
  (when (and (appkit-surface-live-p view)
             (listp message)
             (alist-get 'id message))
    (disco-room--queue-update view
                              (list 'gateway-event
                                    (list :type 'message-create
                                          :channel-id
                                          (disco-msg-normalize-id
                                           channel-id)
                                          :message (copy-tree message)
                                          :self-p t)))

    t))

(cl-defstruct (disco-room--render-request
               (:constructor disco-room--render-request-create)
               (:copier nil))
  change
  events)

(defun disco-room--queue-update (surface message)
  "Queue a room presentation MESSAGE on its exact SURFACE."
  (when (appkit-surface-live-p surface)
    (appkit-surface-post surface message)))

(defun disco-room--flush-updates (&optional surface)
  "Synchronously commit queued room work on SURFACE."
  (when-let* ((surface (or surface (appkit-current-surface)))
              ((appkit-surface-live-p surface)))
    (appkit-surface-send surface 'synchronize)))

(defun disco-room--merge-render-requests (left right)
  "Merge projection changes without reordering committed gateway events."
  (disco-room--render-request-create
   :change (appkit-projection-change-merge
            (disco-room--render-request-change left)
            (disco-room--render-request-change right))
   :events (append (disco-room--render-request-events left)
                   (disco-room--render-request-events right))))

(defun disco-room--request-render (surface)
  "Request one coalesced full room projection for live SURFACE."
  (disco-room--queue-update surface 'refresh))

(defun disco-room--render-request (_surface request)
  "Apply REQUEST's projection changes and committed gateway events."
  (let* ((change (disco-room--render-request-change request))
         (keys (appkit-projection-change-keys change))
         (resources (appkit-projection-change-resources change))
         (geometry (appkit-projection-change-geometry-p change)))
    (dolist (event (disco-room--render-request-events request))
      (disco-room--apply-gateway-event event))
    (cond
     ((or (appkit-projection-change-full-p change) geometry
          (appkit-projection-change-position change))
      (disco-room-render)
      (when geometry
        (disco-room--refresh-timeline-layout))
      (disco-room--resolve-pending-jump))
     (t
      (when (or keys resources)
        (disco-room--sync-timeline
         :force-keys (if (memq 'all resources)
                         (appkit-chat-timeline-keys)
                       keys)
         :changed-resources resources))
      (when (appkit-projection-change-frame-p change)
        (disco-room--update-frame))))
    (when (appkit-scroll-observer-p disco-room--scroll-observer)
      (appkit-scroll-observer-check disco-room--scroll-observer))))

(defun disco-room--ensure-surface ()
  "Return the live Generated Surface owning the current room buffer."
  (let* ((app (disco-runtime-app))
         (identity (disco-room--surface-id))
         (current (appkit-current-surface))
         (surface
          (cond
           ((and (appkit-surface-p current)
                 (eq (appkit-surface-status current) 'running)
                 (eq app (appkit-surface-app current))
                 (equal identity (appkit-surface-identity current)))
            current)
           ((appkit-surface-p current)
            (error "disco: room buffer belongs to another Surface"))
           (t
            (appkit-open-generated-surface
             disco-room--surface-type
             :app app
             :identity identity
             :input (list :channel-id disco-room--channel-id
                          :channel-name disco-room--channel-name)
             :buffer (current-buffer))))))
    (when (appkit-surface-live-p surface)
      (appkit-surface-enable-responsive-geometry
       surface #'disco-room--responsive-geometry-changed)
      (disco-room--install-scroll-observer surface))
    surface))

(defun disco-room--update-frame (&optional channel draft)
  "Update current room header, footer, and composer in place."
  (disco-room--ensure-timeline channel draft)
  (appkit-chat-timeline-set-frame
   (disco-room--header-text channel)
   (disco-room--footer-text draft)
   :bind-input-function #'disco-room--bind-input-region-from-footer
   :composer-visible-p (disco-room--composer-visible-p channel))
  ;; A committed recovery changes canonical input without mutating the live
  ;; composer in its transport callback.  Frame updates preserve that region,
  ;; so project the restored source explicitly after the commit.
  (when (and draft (appkit-chatbuf-input-start-position)
             (not (equal-including-properties
                   draft (appkit-chatbuf-input-string))))
    (appkit-chatbuf-with-generated-update
      (appkit-chatbuf-input-replace draft)
      (disco-room--apply-input-text-properties))))

(defun disco-room--first-unread-message-id (ordered-messages)
  "Return first unread message id in ORDERED-MESSAGES, or nil."
  (let ((last-read-id (disco-state-channel-last-read-message-id disco-room--channel-id))
        found)
    (dolist (msg ordered-messages)
      (let ((message-id (alist-get 'id msg)))
        (when (and (not found)
                   (or (null last-read-id)
                       (and (stringp message-id)
                            (stringp last-read-id)
                            (disco-state-snowflake< last-read-id message-id))))
          (setq found message-id))))
    found))

(defun disco-room--compute-message-render-context (previous-msg msg first-unread-id)
  "Return render context for MSG given PREVIOUS-MSG and FIRST-UNREAD-ID."
  (let* ((message-id (alist-get 'id msg))
         (day-key (disco-msg-day-key msg))
         (previous-day (and previous-msg (disco-msg-day-key previous-msg)))
         (insert-date (and disco-room-show-date-separators
                           (stringp day-key)
                           (not (equal day-key previous-day))
                           day-key))
         (compact (and previous-msg
                       (disco-room--messages-compact-group-p previous-msg msg)))
         (insert-unread (and disco-room-show-unread-divider
                             (stringp message-id)
                             (equal message-id first-unread-id))))
    (list :highlight-query (disco-room--active-highlight-query)
          :compact (and compact t)
          :insert-date insert-date
          :insert-unread (and insert-unread t))))

(defun disco-room--message-reference-targets-current-room-p (msg)
  "Return non-nil when MSG references a message in the current room."
  (let ((ref-channel-id (disco-msg-reference-channel-id msg)))
    (or (null ref-channel-id)
        (equal (disco-msg-normalize-id ref-channel-id)
               (disco-msg-normalize-id disco-room--channel-id)))))

(defun disco-room--message-dependency-keys (msg)
  "Return opaque resource keys that can change rendered MSG."
  (let ((reference-id (disco-msg-reference-id msg))
        (reference-channel-id (disco-msg-reference-channel-id msg))
        dependencies)
    (when-let* ((user (disco-room--avatar-user msg))
                (resource (disco-avatar-resource-key user)))
      (push resource dependencies))
    (dolist
        (identity
         (disco-markdown-custom-emoji-identities
          (and (listp msg) (alist-get 'content msg))))
      (when-let* ((resource
                   (disco-emoji-image-resource-key
                    (plist-get identity :id)
                    (plist-get identity :animated))))
        (push resource dependencies)))
    (dolist (sticker (disco-sticker-message-items msg))
      (when-let* ((resource (disco-sticker-resource-key sticker)))
        (push resource dependencies)))
    (when (and (stringp reference-id)
               (disco-room--message-reference-targets-current-room-p msg)
               (or (disco-msg-reply-type-p msg)
                   (= (disco-msg-type msg) 21)))
      (push (list :message reference-id) dependencies))
    (when (disco-room--message-forwarded-p msg)
      (when-let* ((channel-id (disco-msg-normalize-id reference-channel-id)))
        (push (list :channel channel-id) dependencies))
      (when-let* ((guild-id
                   (or (disco-msg-reference-guild-id msg)
                       (and reference-channel-id
                            (disco-msg-normalize-id
                             (alist-get 'guild_id
                                        (disco-state-channel
                                         reference-channel-id)))))))
        (push (list :guild guild-id) dependencies)))
    (dolist (attachment (disco-room--message-effective-attachments msg))
      (when-let* ((key (disco-media-attachment-download-key attachment)))
        (push (list :attachment key) dependencies))
      (when-let* ((key (disco-media-attachment-preview-cache-key attachment)))
        (push (list :preview key) dependencies)))
    (dolist (key (disco-embed-message-preview-cache-keys msg))
      (push (list :preview key) dependencies))
    (delete-dups (delq nil dependencies))))

(defun disco-room--message-affects-composer-context-p (message-id)
  "Return non-nil when MESSAGE-ID is used by current composer context."
  (or (equal message-id (disco-room--composer-reply-message-id))
      (equal message-id (disco-room--composer-edit-message-id))))

(defun disco-room--retire-deleted-composer-context (message-id)
  "Retire aux state targeting deleted MESSAGE-ID without changing the draft."
  (when (disco-room--message-affects-composer-context-p message-id)
    (disco-room--set-composer-aux-state nil nil)
    t))

(defun disco-room--project-timeline (ordered-messages)
  "Project ORDERED-MESSAGES into shared timeline rows."
  (let ((first-unread-id
         (disco-room--first-unread-message-id ordered-messages)))
    (appkit-chat-timeline-project
     ordered-messages
     (lambda (message) (alist-get 'id message))
     :context-function
     (lambda (previous message)
       (disco-room--compute-message-render-context
        previous message first-unread-id))
     :dependencies-function #'disco-room--message-dependency-keys)))

(cl-defun disco-room--sync-timeline
    (&key ordered-messages force-keys changed-resources rekeys)
  "Synchronize projected room rows through the shared keyed controller."
  (disco-room--ensure-timeline)
  (let ((messages (or ordered-messages
                      (reverse (or (disco-room--display-messages) '())))))
    (appkit-chat-timeline-sync
     (disco-room--project-timeline messages)
     :force-keys force-keys
     :changed-resources changed-resources
     :rekeys rekeys)))

(defun disco-room--apply-read-state-change ()
  "Synchronize projected unread-divider context after read-state changes."
  (when (and (appkit-chat-timeline-live-p)
             (not (disco-room--msg-filter-active-p)))
    (disco-room--sync-timeline)
    t))

(defun disco-room--apply-forward-source-change (&optional source-channel-id
                                                          source-guild-id)
  "Synchronize rows depending on SOURCE-CHANNEL-ID or SOURCE-GUILD-ID."
  (when (and (appkit-chat-timeline-live-p)
             (not (disco-room--msg-filter-active-p)))
    (let ((resources
           (delq nil
                 (list
                  (and source-channel-id
                       (list :channel
                             (disco-msg-normalize-id source-channel-id)))
                  (and source-guild-id
                       (list :guild
                             (disco-msg-normalize-id source-guild-id)))))))
      (when resources
        (disco-room--sync-timeline :changed-resources resources)
        t))))

(defun disco-room--apply-filtered-message-delete (message-id)
  "Remove MESSAGE-ID from the active filter and reject stale filter pages."
  (let* ((filter disco-room--msg-filter)
         (items (or (plist-get filter :items) '()))
         (remaining
          (seq-remove
           (lambda (message)
             (equal message-id (disco-room--message-id message)))
           items))
         (removed-p (< (length remaining) (length items)))
         (request-invalidated-p disco-room--filter-in-flight))
    (when request-invalidated-p
      ;; Load-more callbacks capture the old item list.  Invalidate them so a
      ;; response cannot resurrect a Gateway-deleted search result.
      (setq disco-room--filter-generation
            (1+ (or disco-room--filter-generation 0)))
      (setq disco-room--filter-in-flight nil))
    (when removed-p
      (let ((updated (copy-sequence filter))
            (total (plist-get filter :total-count)))
        (setq updated (plist-put updated :items remaining))
        (when (and (numberp total) (> total 0))
          (setq updated (plist-put updated :total-count (1- total))))
        (setq disco-room--msg-filter updated)))
    removed-p))

;;; Gateway events and message mutations

(defun disco-room--apply-live-message-event (event)
  "Apply live message EVENT through canonical projected synchronization."
  (let* ((event-type (plist-get event :type))
         (event-message (plist-get event :message))
         (message-id (or (and (listp event-message) (alist-get 'id event-message))
                         (plist-get event :message-id)))
         (composer-context-p
          (and message-id
               (disco-room--message-affects-composer-context-p message-id)))
         (nonce (and (listp event-message)
                     (disco-msg-normalize-id
                      (alist-get 'nonce event-message))))
         (rekeys
          (and nonce message-id
               (not (equal nonce message-id))
               (appkit-chat-timeline-node nonce)
               (list (cons nonce message-id)))))
    (cond
     ((not (memq event-type '(message-create message-update message-delete)))
      (error "disco: unsupported live message event: %S" event-type))
     ((not message-id)
      (error "disco: live message event has no message id: %S" event))
     (t
      (when (eq event-type 'message-create)
        (disco-room--observe-live-create message-id))
      (when (eq event-type 'message-delete)
        ;; The canonical message is already gone when the room consumes this
        ;; event.  Retire controller owners and matching composer aux state.
        (disco-room--forget-message-async-state message-id)
        (disco-room--retire-deleted-composer-context message-id))
      (when composer-context-p
        (disco-room--update-frame))
      (if (disco-room--msg-filter-active-p)
          (progn
            ;; Search results are a separate projection.  A deleted exact edge
            ;; invalidates the old continuous-window proof.
            (when (eq event-type 'message-delete)
              (when (equal message-id disco-room--remote-latest-message-id)
                (setq disco-room--remote-latest-message-id
                      (disco-room--message-id
                       (car (disco-room--canonical-cache-newest-first)))))
              (when (or (equal message-id
                               (appkit-chat-history-window-first-key))
                        (equal message-id
                               (appkit-chat-history-window-last-key)))
                (appkit-chat-history-request-cancel)
                (appkit-chat-history-window-clear))
              (disco-room--apply-filtered-message-delete message-id))
            'filtered)
        (when (eq event-type 'message-delete)
          (disco-room--repair-history-window-after-delete message-id))
        ;; This helper runs only while `disco-room--render-request' consumes
        ;; a gateway event.  Preserve keyed-node identity for optimistic sends
        ;; inside that projection transaction.
        (disco-room--sync-timeline
         :changed-resources (list (list :message message-id))
         :rekeys rekeys)
        (disco-room--sync-visible-window-cursors
         (disco-room--display-messages))
        'updated)))))

(defun disco-room-render ()
  "Synchronize the room frame and projected timeline from local state."
  (let* ((channel (disco-room--channel-object))
         (messages (disco-room--display-messages))
         (draft (disco-room--current-draft))
         (initial-p (not (appkit-chat-timeline-live-p)))
         (preview-fetch-budget
          (when (numberp disco-media-preview-max-fetches-per-render)
            (max 0 disco-media-preview-max-fetches-per-render))))
    (disco-room--sync-visible-window-cursors messages)
    (disco-media-set-preview-fetch-budget preview-fetch-budget)
    (unwind-protect
        (progn
          ;; API returns newest-first by default; reverse for chat-like display.
          (disco-room--sync-timeline :ordered-messages (reverse messages))
          ;; Bind the trailing composer only after first EWOC reconciliation;
          ;; before that the empty footer has no stable external tail boundary.
          (disco-room--update-frame channel draft)
          (when (and initial-p (appkit-chatbuf-input-start-position))
            (let ((logical-end (appkit-chatbuf-input-logical-end-position)))
              (when logical-end
                (goto-char logical-end)))))
      (disco-media-set-preview-fetch-budget nil)
      (appkit-chatbuf-update-context-mode))))

(defun disco-room-refresh ()
  "Fetch and redraw latest messages for current room asynchronously."
  (interactive)
  (if (disco-room--msg-filter-active-p)
      (disco-room-filter-refresh)
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-surface))
           (request-revision (disco-state-message-revision channel-id))
           (request-limit (max 1 disco-message-fetch-limit))
           (frontier-at-start disco-room--remote-latest-message-id)
           (owner (appkit-chat-history-request-start view 'latest)))
      (disco-room--queue-update view 'frame)

      (appkit-chat-history-request-bind-handle
       owner
       (disco-api-channel-messages-async
        channel-id
        :limit request-limit
        :owner view
        :on-success
        (lambda (messages)
          (when (disco-room--callback-active-p room-buffer channel-id view)
            (with-current-buffer room-buffer
              (when (appkit-chat-history-request-end owner)
                (let* ((raw-page (disco-room--normalize-history-page messages))
                       (merged
                        (disco-state-merge-message-page
                         channel-id raw-page request-revision))
                       (page
                        (disco-room--history-page-retained-in-cache
                         raw-page merged))
                       (result
                        (disco-room--establish-latest-history-window
                         page
                         frontier-at-start
                         (length raw-page)
                         request-limit)))
                  (unless (eq result 'conflicted)
                    (disco-room--mark-read nil t))
                  (disco-room--request-render view)
                  (if (eq result 'conflicted)
                      (message "disco: history changed concurrently; refresh again")
                    (message "disco: loaded %d messages" (length page))))))))
        :on-error
        (lambda (err)
          (when (disco-room--callback-active-p room-buffer channel-id view)
            (with-current-buffer room-buffer
              (when (appkit-chat-history-request-end owner)
                (disco-room--request-render view)
                (message "disco: room refresh failed: %s"
                         (disco-room--async-error-message err)))))))))))

(defun disco-room--close-for-deleted-channel (reason)
  "Close current room because its backing channel is no longer valid.

REASON is shown in the minibuffer."
  (let ((buf (current-buffer)))
    (disco-room--detach-live-updates)
    (kill-buffer buf)
    (message "%s" reason)))

(defun disco-room--apply-gateway-event (event)
  "Handle one EVENT plist from `disco-gateway-event-hook'."
  (let ((event-type (plist-get event :type))
        (event-channel-id (plist-get event :channel-id))
        (event-guild-id (plist-get event :guild-id)))
    (cond
     ((and (memq event-type '(channel-delete thread-delete))
           (equal event-channel-id disco-room--channel-id))
      (disco-room--close-for-deleted-channel
       (format "disco: channel %s was deleted"
               (or disco-room--channel-name disco-room--channel-id))))
     ((and (eq event-type 'guild-delete)
           disco-room--guild-id
           (equal event-guild-id disco-room--guild-id))
      (disco-room--close-for-deleted-channel
       (format "disco: guild for channel %s was deleted"
               (or disco-room--channel-name disco-room--channel-id))))
     ((and (memq event-type '(guild-update guild-delete))
           event-guild-id)
      (when (disco-room--apply-forward-source-change nil event-guild-id)
        t))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(channel-update
                              thread-update
                              channel-update-partial
                              channel-unread-update
                              channel-pins-update
                              channel-pins-ack)))
      (let ((channel (disco-room--channel-object)))
        (when (and channel (alist-get 'name channel))
          (setq disco-room--channel-name (alist-get 'name channel)))
        (disco-room--update-frame)
        (disco-room--apply-forward-source-change event-channel-id event-guild-id)))
     ((and event-channel-id
           (memq event-type '(channel-update thread-update channel-delete thread-delete)))
      (when (disco-room--apply-forward-source-change event-channel-id event-guild-id)
        t))
     ((and (equal event-channel-id disco-room--channel-id)
           (eq event-type 'message-ack))
      (disco-room--optimistic-read-ack-confirm (plist-get event :message-id))
      (unless (disco-room--apply-read-state-change)
        (disco-room--update-frame)))
     ((and (equal event-channel-id disco-room--channel-id)
           (eq event-type 'typing-start))
      (disco-room--typing-track-user
       (plist-get event :user-id)
       (plist-get event :member)
       (plist-get event :timestamp)))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-create message-update message-delete)))
      (let* ((message (and (eq event-type 'message-create)
                           (plist-get event :message)))
             (author (and (listp message) (alist-get 'author message)))
             (author-id (and (listp author) (alist-get 'id author)))
             (message-id (or (and (listp message) (alist-get 'id message))
                             (plist-get event :message-id))))
        (when author-id
          ;; Message arrival implicitly ends visible typing state for sender.
          (disco-room--typing-stop-user author-id t))
        (disco-room--apply-live-message-event event)
        (when (and (eq event-type 'message-create)
                   (stringp message-id)
                   (disco-room--message-list-contains-id-p
                    (disco-room--display-messages) message-id))
          (disco-room--mark-read message-id t))))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-reaction-add
                              message-reaction-remove
                              message-reaction-remove-all
                              message-reaction-remove-emoji)))
      (disco-room--apply-live-reaction-event event))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-poll-vote-add
                              message-poll-vote-remove)))
      (disco-room--apply-live-poll-vote-event event)))))

(defun disco-room--attach-live-updates ()
  "Attach this room's Generated Surface to the live gateway event stream."
  (let ((surface (disco-room--ensure-surface)))
    (if (and (functionp disco-room--gateway-handler)
             (appkit-handle-p disco-room--live-update-handle)
             (appkit-handle-alive-p disco-room--live-update-handle)
             (eq surface (appkit-handle-owner disco-room--live-update-handle)))
        surface
      (disco-room--detach-live-updates)
      (let* ((buffer (current-buffer))
             (channel-id disco-room--channel-id)
             (handler
              (lambda (event)
                (when (appkit-surface-live-p surface)
                  (disco-room--queue-update surface (list 'gateway-event event)))))
             (hook-installed-p nil)
             (watch-installed-p nil)
             (cleanup-active-p t)
             handle
             (cleanup
              (lambda ()
                ;; The handle and this guard jointly make cleanup idempotent.  The
                ;; captured identities also keep an old surface from removing a
                ;; replacement surface's handler or watch.
                (when cleanup-active-p
                  (setq cleanup-active-p nil)
                  (when hook-installed-p
                    (setq hook-installed-p nil)
                    (remove-hook 'disco-gateway-event-hook handler))
                  (when watch-installed-p
                    (setq watch-installed-p nil)
                    (disco-gateway-unwatch-channel channel-id))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (eq disco-room--gateway-handler handler)
                        (setq disco-room--gateway-handler nil))
                      (when (eq disco-room--live-update-handle handle)
                        (setq disco-room--live-update-handle nil))))))))
        (condition-case err
            (progn
              (add-hook 'disco-gateway-event-hook handler)
              (setq hook-installed-p t)
              (setq watch-installed-p t)
              (disco-gateway-watch-channel channel-id)
              (setq handle (appkit-register-handle surface 'function cleanup))
              (setq disco-room--gateway-handler handler
                    disco-room--live-update-handle handle))
          (error
           (funcall cleanup)
           (signal (car err) (cdr err))))
        surface))))

(defun disco-room--detach-live-updates ()
  "Detach this room buffer from the live update event stream exactly once."
  (let ((handle disco-room--live-update-handle)
        (handler disco-room--gateway-handler)
        (channel-id disco-room--channel-id))
    (setq disco-room--live-update-handle nil)
    (cond
     ((and (appkit-handle-p handle) (appkit-handle-alive-p handle))
      (appkit-cancel-handle handle))
     (handler
      (remove-hook 'disco-gateway-event-hook handler)
      (setq disco-room--gateway-handler nil)
      (when channel-id
        (disco-gateway-unwatch-channel channel-id)))))
  (disco-room--typing-reset))

(defun disco-room-load-older-messages (&optional quiet)
  "Load one older page for the current room view asynchronously.

When QUIET is non-nil, suppress progress messages."
  (interactive)
  (cond
   ((disco-room--msg-filter-active-p)
    (disco-room-filter-load-more))
   ((not (appkit-chat-history-window-known-p))
    (unless quiet (message "disco: history window is not initialized")))
   ((appkit-chat-history-older-loaded-p)
    (unless quiet (message "disco: no older messages available")))
   ((appkit-chat-history-loading-p)
    (unless quiet (message "disco: history load already in progress")))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-surface))
           (request-revision (disco-state-message-revision channel-id))
           (before (or (appkit-chat-history-window-first-key)
                       (user-error
                        "disco: no oldest message cursor; refresh first")))
           (request-limit (max 1 disco-message-fetch-limit))
           (owner (appkit-chat-history-request-start view 'older)))
      (disco-room--queue-update view 'frame)

      (appkit-chat-history-request-bind-handle
       owner
       (disco-api-channel-messages-async
        channel-id
        :before before
        :limit request-limit
        :owner view
        :on-success
        (lambda (older)
          (when (disco-room--callback-active-p room-buffer channel-id view)
            (with-current-buffer room-buffer
              (when (appkit-chat-history-request-end owner)
                (let* ((raw-page (disco-room--normalize-history-page older))
                       (merged
                        (disco-state-merge-message-page
                         channel-id raw-page request-revision))
                       (page
                        (disco-room--history-page-retained-in-cache
                         raw-page merged))
                       (oldest (car (disco-room--history-page-bounds page)))
                       (canonical-oldest
                        (reverse (disco-room--normalize-history-page merged)))
                       (complete (< (length raw-page) request-limit))
                       (progressed
                        (and oldest
                             (disco-room--message-id-after-p
                              before oldest canonical-oldest))))
                  (when progressed
                    (appkit-chat-history-window-set
                     oldest (appkit-chat-history-window-last-key)))
                  (when complete
                    (appkit-chat-history-older-loaded-set t))
                  (disco-room--request-render view)
                  (unless quiet
                    (cond
                     (progressed
                      (message "disco: loaded %d older messages"
                               (length page)))
                     (complete
                      (message "disco: reached beginning of history"))
                     (t
                      (message "disco: older history changed concurrently; retry"))))))))
          :on-error
          (lambda (err)
            (when (disco-room--callback-active-p room-buffer channel-id view)
              (with-current-buffer room-buffer
                (when (appkit-chat-history-request-end owner)
                  (disco-room--request-render view)
                  (message "disco: older history load failed: %s"
                           (disco-room--async-error-message err)))))))))))))

(defun disco-room-load-newer-messages (&optional quiet)
  "Extend a partial around-message window toward the live frontier.

When QUIET is non-nil, suppress progress messages."
  (interactive)
  (cond
   ((disco-room--msg-filter-active-p)
    (unless quiet (message "disco: newer paging is unavailable in filters")))
   ((not (appkit-chat-history-window-known-p))
    (if quiet
        nil
      (disco-room-refresh)))
   ((not (appkit-chat-history-window-partial-p))
    (unless quiet (message "disco: latest history is already loaded")))
   ((appkit-chat-history-loading-p)
    (unless quiet (message "disco: history load already in progress")))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-surface))
           (cursor (appkit-chat-history-window-last-key))
           (request-revision (disco-state-message-revision channel-id))
           (request-limit (max 1 disco-message-fetch-limit))
           (owner (appkit-chat-history-request-start view 'newer)))
      (disco-room--queue-update view 'frame)

      (appkit-chat-history-request-bind-handle
       owner
       (disco-api-channel-messages-async
        channel-id
        :after cursor
        :limit request-limit
        :owner view
        :on-success
        (lambda (newer)
          (when (disco-room--callback-active-p room-buffer channel-id view)
            (with-current-buffer room-buffer
              (when (appkit-chat-history-request-end owner)
                (let* ((raw-page (disco-room--normalize-history-page newer))
                       (merged
                        (disco-state-merge-message-page
                         channel-id raw-page request-revision))
                       (page
                        (disco-room--history-page-retained-in-cache
                         raw-page merged))
                       (bounds (disco-room--history-page-bounds page))
                       (newest (cdr bounds))
                       (edge (or newest cursor))
                       (canonical-oldest
                        (reverse (disco-room--normalize-history-page merged)))
                       (progressed
                        (and newest
                             (disco-room--message-id-after-p
                              newest cursor canonical-oldest)))
                       (frontier disco-room--remote-latest-message-id)
                       (edge-after-frontier
                        (and frontier edge
                             (disco-state-snowflake< frontier edge)))
                       (short-page (< (length raw-page) request-limit))
                       (finished
                        (or (equal edge frontier)
                            (and short-page
                                 (or (null frontier)
                                     edge-after-frontier)))))
                  (cond
                   (finished
                    (setq disco-room--remote-latest-message-id edge)
                    (appkit-chat-history-window-set
                     (appkit-chat-history-window-first-key) nil))
                   (progressed
                    (when edge-after-frontier
                      (setq disco-room--remote-latest-message-id nil))
                    (appkit-chat-history-window-set
                     (appkit-chat-history-window-first-key) newest))
                   (t
                    (appkit-chat-history-newer-stalled-set cursor)))
                  (disco-room--request-render view)
                  (unless quiet
                    (cond
                     (finished
                      (message "disco: newer history caught up"))
                     (progressed
                      (message "disco: loaded %d newer messages"
                               (length page)))
                     (t
                      (message "disco: newer history made no progress"))))))))
          :on-error
          (lambda (err)
            (when (disco-room--callback-active-p room-buffer channel-id view)
              (with-current-buffer room-buffer
                (when (appkit-chat-history-request-end owner)
                  (disco-room--request-render view)
                  (message "disco: newer history load failed: %s"
                           (disco-room--async-error-message err)))))))))))))

(defun disco-room--lottie-sticker-at-point ()
  "Return the native Lottie Sticker projected at point, or nil."
  (let ((sticker
         (or (get-text-property (point) 'disco-sticker-object)
             (and (> (point) (point-min))
                  (get-text-property (1- (point)) 'disco-sticker-object)))))
    (and (= (or (disco-sticker-format-type sticker) 0) 3)
         sticker)))

(defun disco-room-play-sticker-at-point ()
  "Play the native Lottie Sticker projected at point."
  (interactive)
  (if-let* ((sticker (disco-room--lottie-sticker-at-point)))
      (disco-sticker-play sticker)
    (user-error "disco: no Lottie Sticker at point")))

(defun disco-room-toggle-breakline ()
  "Toggle visual breakline wrapping in the current room buffer."
  (interactive)
  (appkit-chatbuf-set-soft-wrap (not appkit-chatbuf-wrap-long-lines))
  (message "disco: breakline wrapping %s"
           (if appkit-chatbuf-wrap-long-lines "enabled" "disabled")))

(defun disco-room--delete-msg (msg)
  "Delete MSG in current room."
  (let ((message-id (alist-get 'id msg)))
    (disco-room--ensure-action-available
     (disco-room--delete-message-unavailable-reason msg)
     "delete messages")
    (when (y-or-n-p (format "Delete message %s? " message-id))
      (let ((room-buffer (current-buffer))
            (channel-id disco-room--channel-id)
            (view (disco-room--ensure-surface)))
        (disco-api-delete-message-async
         channel-id
         message-id
         :on-success
         (lambda (_response)
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (disco-state-delete-message channel-id message-id)
               (disco-room--repair-history-window-after-delete message-id)
               (disco-room--request-render view)
               (message "disco: deleted message %s" message-id))))
         :on-error
         (lambda (err)
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (message "disco: delete failed for %s: %s"
                      message-id
                      (disco-room--async-error-message err)))))))))

(defun disco-room-delete-message ()
  "Delete message at point in current room."
  (interactive)
  (disco-room--delete-msg (disco-room--message-at-point)))

;;; Commands and keymaps

(defun disco-room--operate-msg (_msg)
  "Open the message transient for the current room.

_MSG is ignored because the transient resolves availability from point."
  (call-interactively #'disco-transient-msg-operate))

(defvar-keymap disco-room-mode-map
  :doc "Keymap for `disco-room-mode'."
  "C-l" #'recenter-top-bottom
  "TAB" #'disco-room-complete-mention
  "<tab>" #'disco-room-complete-mention
  "C-M-i" #'disco-room-complete-mention
  "C-c g" #'disco-room-refresh
  "C-c f" #'appkit-markup-compose-set-active-codec
  "RET" #'disco-room-return-dwim
  "M-RET" #'disco-room-input-preview
  "C-c '" #'disco-room-edit-draft
  "M-p" #'disco-room-draft-prev
  "M-n" #'disco-room-draft-next
  "M-r" #'disco-room-draft-history-search
  "M-g s" #'disco-room-inplace-search
  "M-g n" #'disco-room-inplace-search-next
  "M-g p" #'disco-room-inplace-search-prev
  "C-c C-r" #'disco-room-inplace-search-query
  "C-c C-s" #'disco-room-inplace-search-query-forward
  "C-c /" #'disco-room-filter-search
  "C-c C-c" #'disco-room-filter-cancel
  "C-c M-/" #'disco-room-search-channel
  "C-c C-p s" #'disco-room-send-poll
  "C-c C-p +" #'disco-room-vote-poll-answer
  "C-c C-p -" #'disco-room-remove-poll-vote
  "C-c C-p t" #'disco-room-toggle-poll-answer
  "C-c C-p v" #'disco-room-submit-poll-vote
  "C-c C-p c" #'disco-room-clear-poll-votes
  "C-c C-p e" #'disco-room-expire-poll
  "C-c M-p" #'disco-room-list-pinned-messages
  "C-c C-P" #'disco-room-ack-channel-pins
  "C-c e" #'disco-room-send-message-with-codec
  "C-c C-a" #'disco-room-attach
  "C-c C-f" #'disco-room-attach-file
  "C-c C-i" #'disco-room-send-sticker
  "C-c C-o" #'disco-room-input-options-transient
  "C-c C-d" #'disco-room-remove-attachment-token-at-point
  "C-c C-x" #'disco-room-clear-attachments
  "C-c M-l" #'disco-room-list-attachments
  "C-c M-e" #'disco-room-edit-attachment-description
  "C-c M-s" #'disco-room-toggle-attachment-spoiler
  "C-c M-r" #'disco-room-reorder-attachments
  "C-c C-k" #'disco-room-cancel-reply
  "ESC ESC" #'disco-room-cancel-reply
  "C-M-c" #'disco-room-cancel-reply
  "C-c C-g" #'disco-room-jump-to-message
  "C-c C-w" #'disco-room-toggle-breakline
  "C-c C-t m" #'disco-room-thread-create-from-message
  "C-c C-t o" #'disco-room-thread-open-from-message-at-point
  "C-c C-t c" #'disco-room-thread-create
  "C-c C-t r" #'disco-room-thread-rename
  "C-c C-t k" #'disco-room-thread-toggle-locked
  "C-c C-t s" #'disco-room-thread-set-slowmode
  "C-c C-t a" #'disco-room-thread-toggle-archived
  "C-c C-t A" #'disco-room-thread-set-auto-archive-duration
  "C-c C-t e" #'disco-room-thread-edit-settings
  "C-c C-t u" #'disco-room-thread-set-muted
  "C-c C-j" #'disco-room-thread-join
  "C-c C-l" #'disco-room-thread-leave
  "C-c M-v" #'disco-avatar-refetch
  "C-c ?" #'disco-room-transient)

;;; View lifecycle and session cleanup

(defun disco-room--clear-session-cache-memory ()
  "Clear account-scoped room cache bookkeeping without running callbacks."
  (disco-room-compose--clear-session-cache-memory)
  (disco-room-search--clear-session-cache-memory)
  (disco-room-render--clear-session-cache-memory))

(defun disco-room-reset-session-cache-state ()
  "Destructively clear account-scoped room state without redrawing."
  (let ((disco-room--session-cache-reset-in-progress t))
    (unwind-protect
        (disco-room-render-reset-session-cache-state)
      ;; A cancellation hook may touch any room-owned account history.
      (disco-room--clear-session-cache-memory))))

(defun disco-room--reset-view-local-state (&optional channel-id channel-name)
  "Reset controller state owned by one room view.

CHANNEL-ID and CHANNEL-NAME bind a newly attached replacement Surface.  This
is separate from major-mode initialization because a Surface can stop while
its same-mode buffer survives."
  (disco-room--detach-live-updates)
  (disco-company--teardown-room-buffer)
  (disco-room--typing-cancel-expire-timer)
  (appkit-chatbuf-reset-state disco-room-input-history-size)
  (appkit-chat-history-reset-state)
  (setq-local disco-room--channel-id channel-id)
  (setq-local disco-room--channel-name channel-name)
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (setq-local disco-room--guild-id
                (and channel (alist-get 'guild_id channel))))
  (setq-local disco-room--remote-latest-message-id nil)
  (setq-local disco-room--oldest-message-id nil)
  (setq-local disco-room--newest-message-id nil)
  (disco-room-compose-reset)
  (setq-local disco-room--pending-jump-message-id nil)
  (disco-room-search-reset)
  (setq-local disco-msg-resolve-function #'disco-room--resolve-message)
  (setq-local disco-msg-content-text-function #'disco-room--message-copy-text)
  (setq-local disco-msg-reply-function #'disco-room--reply-to-msg)
  (setq-local disco-msg-forward-function #'disco-room--forward-msg)
  (setq-local disco-msg-operate-function #'disco-room--operate-msg)
  (setq-local disco-msg-edit-function #'disco-room--edit-msg)
  (setq-local disco-msg-delete-function #'disco-room--delete-msg)
  (setq-local disco-msg-toggle-pin-function #'disco-room--toggle-pin-on-msg)
  (setq-local disco-msg-open-thread-function #'disco-room-thread-open-from-message)
  (setq-local disco-msg-toggle-reaction-function #'disco-room--toggle-reaction-on-msg)
  (setq-local disco-msg-add-reaction-function #'disco-room--add-reaction-to-msg)
  (setq-local disco-msg-remove-reaction-function #'disco-room--remove-reaction-from-msg)
  (setq-local disco-msg-redisplay-function #'disco-room--redisplay-msg)
  (setq-local appkit-media-card-fallback-context-function
              #'disco-room--media-card-fallback-context)
  (setq-local disco-room--typing-users (make-hash-table :test #'equal))
  (setq-local disco-room--typing-expire-timer nil)
  (disco-room-poll-reset)
  (disco-room-reaction-reset)
  (disco-room-pin-reset)
  (setq-local disco-room--revealed-spoiler-message-id nil)
  (setq-local disco-room--optimistic-read-ack-seq 0)
  (setq-local disco-room--pending-optimistic-read-ack nil)
  (setq-local disco-room--gateway-handler nil)
  (setq-local disco-room--live-update-handle nil)
  (setq-local disco-room--scroll-observer nil)
  (funcall #'disco-company-setup-room-buffer)
  (when (disco-current-token)
    (disco-sticker-ensure-ready disco-room--guild-id)))

(defun disco-room--media-render-request ()
  "Return a projection frame update for committed media status."
  (disco-room--render-request-create
   :change (appkit-projection-change-create :frame-p t)))

(defun disco-room--media-status-text (model)
  "Return presentation status text from committed room MODEL."
  (pcase (plist-get model :media-phase)
    ('opening "Opening media…") ('playing "Playing media…")
    ('error
     (format "Media failed: %s" (plist-get model :media-message)))
    (_ nil)))

(defun disco-room--surface-init (_context input)
  "Initialize a room Surface from INPUT."
  (let ((channel-id (plist-get input :channel-id))
        (channel-name (plist-get input :channel-name)))
    (disco-room--reset-view-local-state channel-id channel-name)
    (appkit-next
     :model (list :channel-id channel-id
                  :media-phase 'idle
                  :media-message nil)
     :render (disco-room--render-request-create
              :change (appkit-projection-change-create :full-p t :frame-p t)))))

(defun disco-room--surface-update (_context model message)
  "Advance the room Surface MODEL for MESSAGE."
  (pcase message
    ('synchronize (appkit-next :model model :render appkit-render-none))
    ((or 'refresh 'timeline 'frame 'geometry 'position
         `(rows-changed ,_) `(resources-changed ,_) `(gateway-event ,_))
     (appkit-next
      :model model
      :render
      (disco-room--render-request-create
       :change
       (pcase message
         ((or 'refresh 'timeline)
          (appkit-projection-change-create :full-p t :frame-p t))
         ('frame (appkit-projection-change-create :frame-p t))
         ('geometry (appkit-projection-change-create :geometry-p t))
         ('position (appkit-projection-change-create :position 'preserve))
         (`(rows-changed ,keys)
          (appkit-projection-change-create :keys keys))
         (`(resources-changed ,resources)
          (appkit-projection-change-create :resources resources))
         (`(gateway-event ,event)
          (let ((kind (plist-get event :type)))
            (appkit-projection-change-create
             :frame-p (eq kind 'typing-start)
             :keys (when (memq kind '(message-reaction-add
                                      message-reaction-remove
                                      message-reaction-remove-all
                                      message-reaction-remove-emoji
                                      message-poll-vote-add
                                      message-poll-vote-remove))
                     (list (plist-get event :message-id)))))))
       :events (pcase message
                 (`(gateway-event ,event) (list event))))))
    (`(disco-media open ,resource ,kind ,cache-key)
     (let ((next (copy-sequence model)))
       (setf (plist-get next :media-phase)
             (if (eq kind 'video) 'playing 'opening)
             (plist-get next :media-message) nil)
       (appkit-next
        :model next
        :render
        (disco-room--media-render-request)
        :commands
        (list
         (appkit-command-start-effect
          (disco-media-open-effect resource kind cache-key))))))
    (`(disco-media acquired ,file)
     (appkit-next
      :model model
      :render appkit-render-none
      :commands
      (list
       (appkit-command-start-effect
        (disco-media-file-presentation-effect file)))))
    ('(disco-media closed)
     (let ((next (copy-sequence model)))
       (setf (plist-get next :media-phase) 'idle
             (plist-get next :media-message) nil)
       (appkit-next
        :model next
        :render
        (disco-room--media-render-request))))
    (`(disco-media failed ,reason)
     (let ((next (copy-sequence model)))
       (setf (plist-get next :media-phase) 'error
             (plist-get next :media-message) reason)
       (appkit-next
        :model next
        :render
        (disco-room--media-render-request))))
    (_ (appkit-next-reject 'invalid-room-message))))

(defun disco-room--surface-renderer (_surface)
  "Create one Generated Renderer for a room."
  (appkit-generated-renderer-create
   :mount #'disco-runtime-retain-surface-owner
   :merge #'disco-room--merge-render-requests
   :render (lambda (surface _app-read-view model request)
             (let ((disco-room--media-status (disco-room--media-status-text model)))
               (disco-room--render-request surface request))
             nil)
   :recover nil
   :unmount (lambda (_surface)
              (disco-room--detach-live-updates))))

(define-derived-mode disco-room-mode appkit-chatbuf-mode "Disco-Room"
  "Major mode for disco.el room buffers."
  ;; Avoid visible seams between vertically sliced inline images.
  (setq-local line-spacing 0)
  (disco-room--reset-view-local-state)
  (setq-local appkit-chatbuf-input-sync-function
              #'disco-room--sync-draft-from-buffer)
  (add-hook 'text-scale-mode-hook #'disco-room--on-text-scale-change nil t)
  (add-hook 'post-command-hook #'disco-room--post-command t t)
  (appkit-chatbuf-use-timeline-mode #'disco-room-timeline-mode))

(defconst disco-room--surface-type
  (appkit-surface-type-create
   :name 'disco-room
   :mode #'disco-room-mode
   :init #'disco-room--surface-init
   :update #'disco-room--surface-update
   :renderer-factory #'disco-room--surface-renderer)
  "Generated Surface type for Discord rooms.")

(defun disco-room-open (channel-id channel-name)
  "Open CHANNEL-ID as a Generated room Surface."
  (let* ((app (disco-runtime-app))
         (identity (list 'room channel-id))
         (existing (appkit-app-surface app identity))
         (surface
          (cond
           ((appkit-surface-live-p existing)
            (pop-to-buffer (appkit-surface-buffer existing))
            existing)
           (existing
            (error "disco: room Surface is unavailable: %S"
                   (appkit-surface-status existing)))
           (t
            (appkit-open-generated-surface
             disco-room--surface-type
             :app app
             :identity identity
             :input (list :channel-id channel-id :channel-name channel-name)
             :buffer-name (disco-room--buffer-name channel-name channel-id)
             :select t))))
         (buffer (appkit-surface-buffer surface)))
    (with-current-buffer buffer
      (unless existing
        (disco-room--install-scroll-observer surface)
        (disco-room--attach-live-updates)
        (disco-room-refresh))
      (appkit-surface-enable-responsive-geometry
       surface #'disco-room--responsive-geometry-changed)
      (appkit-surface-refresh-responsive-geometry surface)
      (disco-room--queue-update surface 'geometry)

      (disco-room--flush-updates surface))
    buffer))

(provide 'disco-room)

;;; disco-room.el ends here
