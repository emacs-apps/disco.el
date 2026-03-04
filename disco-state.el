;;; disco-state.el --- In-memory state for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Central state container for guilds/channels and room-local message caches.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar disco-state--guilds nil
  "List of guild objects as alists.")

(defvar disco-state--channels-by-guild (make-hash-table :test #'equal)
  "Hash table guild-id -> list of channel objects.")

(defvar disco-state--channels-by-id (make-hash-table :test #'equal)
  "Hash table channel-id -> channel object.")

(defvar disco-state--private-channels nil
  "List of private channels (DM/group DM) as channel objects.")

(defvar disco-state--threads-by-parent (make-hash-table :test #'equal)
  "Hash table parent-channel-id -> list of thread channel objects.")

(defvar disco-state--thread-ids-by-guild (make-hash-table :test #'equal)
  "Hash table guild-id -> list of known thread channel IDs.")

(defvar disco-state--messages-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> list of message objects.")

(defvar disco-state--unread-counts-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> unread message count.")

(defvar disco-state--last-read-message-id-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> last acknowledged message ID.")

(defvar disco-state--last-read-pin-timestamp-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> last acknowledged channel pin timestamp.")

(defvar disco-state--ack-token-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> last read-state ack token.")

(defvar disco-state--thread-member-ids-by-thread (make-hash-table :test #'equal)
  "Hash table thread-id -> list of known member user IDs.")

(defvar disco-state--thread-member-count-by-thread (make-hash-table :test #'equal)
  "Hash table thread-id -> last known thread member count.")

(defconst disco-state-read-state-type-alist
  '((channel . 0)
    (guild-event . 1)
    (notification-center . 2)
    (guild-home . 3)
    (guild-onboarding-question . 4)
    (message-requests . 5))
  "Declarative map of Discord read-state type names to integer values.")

(defconst disco-state-read-state-type-channel
  (alist-get 'channel disco-state-read-state-type-alist)
  "Read-state type value for channel message unreads.")

(defconst disco-state-read-state-flag-alist
  '((is-guild-channel . 1)
    (is-thread . 2)
    (is-mention-low-importance . 4))
  "Declarative map of read-state channel flag names to bit values.")

(defconst disco-state-read-state-flag-is-guild-channel
  (alist-get 'is-guild-channel disco-state-read-state-flag-alist)
  "Flag bit for guild channel read-state.")

(defconst disco-state-read-state-flag-is-thread
  (alist-get 'is-thread disco-state-read-state-flag-alist)
  "Flag bit for thread channel read-state.")

(defconst disco-state-discord-epoch-seconds 1420070400
  "Discord epoch as UNIX seconds (2015-01-01 00:00:00 UTC).")

(defconst disco-state--seconds-per-day 86400
  "Number of seconds in one UTC day.")

(defconst disco-state-message-type-recipient-remove 2
  "Discord message type value for `RECIPIENT_REMOVE'.")

(defconst disco-state-message-type-poll-result 46
  "Discord message type value for `POLL_RESULT'.")

(defun disco-state-reset ()
  "Reset all in-memory state."
  (setq disco-state--guilds nil)
  (setq disco-state--private-channels nil)
  (clrhash disco-state--channels-by-guild)
  (clrhash disco-state--channels-by-id)
  (clrhash disco-state--threads-by-parent)
  (clrhash disco-state--thread-ids-by-guild)
  (clrhash disco-state--messages-by-channel)
  (clrhash disco-state--unread-counts-by-channel)
  (clrhash disco-state--last-read-message-id-by-channel)
  (clrhash disco-state--last-read-pin-timestamp-by-channel)
  (clrhash disco-state--ack-token-by-channel)
  (clrhash disco-state--thread-member-ids-by-thread)
  (clrhash disco-state--thread-member-count-by-thread))

(defun disco-state-channel-thread-p (channel)
  "Return non-nil when CHANNEL is a thread channel."
  (memq (alist-get 'type channel) '(10 11 12)))

(defun disco-state-private-channel-p (channel)
  "Return non-nil when CHANNEL is a DM or group DM channel."
  (memq (alist-get 'type channel) '(1 3)))

(defun disco-state-thread-only-parent-channel-p (channel)
  "Return non-nil when CHANNEL is a thread-only parent channel."
  (memq (alist-get 'type channel) '(15 16)))

(defun disco-state--parent-threads-upsert (parent-id channel)
  "Insert or replace thread CHANNEL under PARENT-ID."
  (let ((channel-id (alist-get 'id channel))
        (existing (or (gethash parent-id disco-state--threads-by-parent) '()))
        (found nil)
        updated)
    (setq updated
          (mapcar
           (lambda (it)
             (if (equal (alist-get 'id it) channel-id)
                 (progn (setq found t) channel)
               it))
           existing))
    (unless found
      (setq updated (append updated (list channel))))
    (puthash parent-id updated disco-state--threads-by-parent)))

(defun disco-state--remove-thread-indexes (channel)
  "Remove thread CHANNEL from parent/guild indexes."
  (let* ((channel-id (alist-get 'id channel))
         (parent-id (alist-get 'parent_id channel))
         (guild-id (alist-get 'guild_id channel)))
    (when parent-id
      (let* ((threads (or (gethash parent-id disco-state--threads-by-parent) '()))
             (updated (cl-remove-if (lambda (it)
                                      (equal (alist-get 'id it) channel-id))
                                    threads)))
        (if updated
            (puthash parent-id updated disco-state--threads-by-parent)
          (remhash parent-id disco-state--threads-by-parent))))
    (when guild-id
      (let* ((ids (or (gethash guild-id disco-state--thread-ids-by-guild) '()))
             (updated (delete channel-id (copy-sequence ids))))
        (if updated
            (puthash guild-id updated disco-state--thread-ids-by-guild)
          (remhash guild-id disco-state--thread-ids-by-guild))))))

(defun disco-state--index-thread-channel (channel)
  "Index thread CHANNEL in parent/guild lookup tables."
  (let* ((channel-id (alist-get 'id channel))
         (parent-id (alist-get 'parent_id channel))
         (guild-id (alist-get 'guild_id channel)))
    (when parent-id
      (disco-state--parent-threads-upsert parent-id channel))
    (when (and guild-id channel-id)
      (let ((ids (or (gethash guild-id disco-state--thread-ids-by-guild) '())))
        (unless (member channel-id ids)
          (puthash guild-id (append ids (list channel-id))
                   disco-state--thread-ids-by-guild))))))

(defun disco-state--thread-member-user-id (thread-member)
  "Extract user ID from THREAD-MEMBER object."
  (or (and (listp thread-member)
           (alist-get 'user_id thread-member))
      (let* ((member (and (listp thread-member)
                          (alist-get 'member thread-member)))
             (user (and (listp member)
                        (alist-get 'user member))))
        (and (listp user)
             (alist-get 'id user)))))

(defun disco-state-thread-member-ids (thread-id)
  "Return known member user IDs for THREAD-ID."
  (copy-sequence (or (gethash thread-id disco-state--thread-member-ids-by-thread) '())))

(defun disco-state-thread-member-count (thread-id)
  "Return known member count for THREAD-ID, or nil."
  (gethash thread-id disco-state--thread-member-count-by-thread))

(defun disco-state-set-thread-member-count (thread-id member-count)
  "Store MEMBER-COUNT for THREAD-ID and return normalized count."
  (if (numberp member-count)
      (let ((normalized (max 0 member-count)))
        (puthash thread-id normalized disco-state--thread-member-count-by-thread)
        normalized)
    (remhash thread-id disco-state--thread-member-count-by-thread)
    nil))

(defun disco-state-upsert-thread-member (thread-id user-id)
  "Add USER-ID to THREAD-ID member index and return updated IDs."
  (let ((normalized-user-id (and user-id (format "%s" user-id))))
    (if (not (and thread-id normalized-user-id))
        (disco-state-thread-member-ids thread-id)
      (let ((existing (or (gethash thread-id disco-state--thread-member-ids-by-thread) '())))
        (unless (member normalized-user-id existing)
          (puthash thread-id
                   (append existing (list normalized-user-id))
                   disco-state--thread-member-ids-by-thread))
        (disco-state-thread-member-ids thread-id)))))

(defun disco-state-delete-thread-member (thread-id user-id)
  "Remove USER-ID from THREAD-ID member index and return updated IDs."
  (let ((normalized-user-id (and user-id (format "%s" user-id))))
    (if (not (and thread-id normalized-user-id))
        (disco-state-thread-member-ids thread-id)
      (let* ((existing (or (gethash thread-id disco-state--thread-member-ids-by-thread) '()))
             (updated (delete normalized-user-id (copy-sequence existing))))
        (if updated
            (puthash thread-id updated disco-state--thread-member-ids-by-thread)
          (remhash thread-id disco-state--thread-member-ids-by-thread))
        updated))))

(defun disco-state-apply-thread-members-update (thread-id added-members removed-member-ids
                                                          &optional member-count)
  "Apply gateway thread membership delta for THREAD-ID.

ADDED-MEMBERS is an array/list of thread member objects.
REMOVED-MEMBER-IDS is an array/list of removed user IDs.
MEMBER-COUNT is optional approximate thread member count."
  (dolist (member (or added-members '()))
    (let ((user-id (disco-state--thread-member-user-id member)))
      (when user-id
        (disco-state-upsert-thread-member thread-id user-id))))
  (dolist (user-id (or removed-member-ids '()))
    (disco-state-delete-thread-member thread-id user-id))
  (when (numberp member-count)
    (disco-state-set-thread-member-count thread-id member-count))
  (disco-state-thread-member-ids thread-id))

(defun disco-state--replace-or-append-channel (channels channel)
  "Return CHANNELS with CHANNEL replaced by ID or appended if absent."
  (let ((channel-id (alist-get 'id channel))
        (found nil)
        updated)
    (setq updated
          (mapcar
           (lambda (it)
             (if (equal (alist-get 'id it) channel-id)
                 (progn (setq found t) channel)
               it))
           channels))
    (if found
        updated
      (append updated (list channel)))))

(defun disco-state--replace-or-append-guild (guilds guild)
  "Return GUILDS with GUILD replaced by ID or appended if absent."
  (let ((guild-id (alist-get 'id guild))
        (found nil)
        updated)
    (setq updated
          (mapcar
           (lambda (it)
             (if (equal (alist-get 'id it) guild-id)
                 (progn (setq found t) guild)
               it))
           guilds))
    (if found
        updated
      (append updated (list guild)))))

(defun disco-state-set-guilds (guilds)
  "Set GUILDS list in memory."
  (setq disco-state--guilds guilds))

(defun disco-state-guilds ()
  "Return guild list from memory."
  disco-state--guilds)

(defun disco-state-private-channels ()
  "Return private channels from memory."
  (or disco-state--private-channels '()))

(defun disco-state-set-private-channels (channels)
  "Set private CHANNELS list in memory and refresh channel indexes."
  (let* ((private-only (seq-filter #'disco-state-private-channel-p (or channels '())))
         (new-ids (delq nil (mapcar (lambda (it) (alist-get 'id it)) private-only))))
    (dolist (old (copy-sequence (or disco-state--private-channels '())))
      (let ((old-id (alist-get 'id old)))
        (when (and old-id (not (member old-id new-ids)))
          (disco-state-delete-channel old-id))))
    (setq disco-state--private-channels private-only)
    (dolist (channel private-only)
      (let ((channel-id (alist-get 'id channel)))
        (when channel-id
          (puthash channel-id channel disco-state--channels-by-id))))
    private-only))

(defun disco-state-upsert-guild (guild)
  "Insert or update one GUILD object in memory by ID."
  (setq disco-state--guilds
        (disco-state--replace-or-append-guild (or disco-state--guilds '()) guild)))

(defun disco-state-delete-guild (guild-id)
  "Delete GUILD-ID and all related channel/thread/message state."
  (setq disco-state--guilds
        (cl-remove-if (lambda (it)
                        (equal (alist-get 'id it) guild-id))
                      (or disco-state--guilds '())))
  (dolist (channel (copy-sequence (or (gethash guild-id disco-state--channels-by-guild) '())))
    (let ((channel-id (alist-get 'id channel)))
      (when channel-id
        (disco-state-delete-channel channel-id))))
  (remhash guild-id disco-state--channels-by-guild)
  (remhash guild-id disco-state--thread-ids-by-guild))

(defun disco-state-put-channels (guild-id channels)
  "Store CHANNELS list for GUILD-ID and index channels by ID."
  ;; Remove previously indexed threads for this guild before replacing list.
  (let ((old-thread-ids (or (gethash guild-id disco-state--thread-ids-by-guild) '())))
    (dolist (thread-id old-thread-ids)
      (let ((old-thread (gethash thread-id disco-state--channels-by-id)))
        (when old-thread
          (disco-state--remove-thread-indexes old-thread)
          (remhash thread-id disco-state--thread-member-ids-by-thread)
          (remhash thread-id disco-state--thread-member-count-by-thread)
          (remhash thread-id disco-state--channels-by-id)))))
  (puthash guild-id channels disco-state--channels-by-guild)
  (dolist (channel channels)
    (let ((channel-id (alist-get 'id channel)))
      (when channel-id
        (puthash channel-id channel disco-state--channels-by-id)
        (when (disco-state-channel-thread-p channel)
          (disco-state--index-thread-channel channel)
          (let ((thread-member (alist-get 'member channel))
                (thread-member-count (alist-get 'member_count channel)))
            (when thread-member
              (let ((user-id (disco-state--thread-member-user-id thread-member)))
                (when user-id
                  (disco-state-upsert-thread-member channel-id user-id))))
            (when (numberp thread-member-count)
              (disco-state-set-thread-member-count channel-id thread-member-count))))))))

(defun disco-state-guild-channels (guild-id)
  "Return channels for GUILD-ID."
  (gethash guild-id disco-state--channels-by-guild))

(defun disco-state-channel (channel-id)
  "Return channel object for CHANNEL-ID."
  (gethash channel-id disco-state--channels-by-id))

(defun disco-state-parent-threads (parent-id)
  "Return list of threads under PARENT-ID."
  (or (gethash parent-id disco-state--threads-by-parent) '()))

(defun disco-state-guild-thread-ids (guild-id)
  "Return known thread channel IDs for GUILD-ID."
  (copy-sequence (or (gethash guild-id disco-state--thread-ids-by-guild) '())))

(defun disco-state-guild-threads (guild-id)
  "Return known thread channel objects for GUILD-ID."
  (let (threads)
    (dolist (thread-id (disco-state-guild-thread-ids guild-id))
      (let ((thread (gethash thread-id disco-state--channels-by-id)))
        (when thread
          (push thread threads))))
    (nreverse threads)))

(defun disco-state-upsert-channel (channel)
  "Insert or update one CHANNEL object in all indexes."
  (let* ((channel-id (alist-get 'id channel))
         (guild-id (alist-get 'guild_id channel))
         (old (and channel-id (gethash channel-id disco-state--channels-by-id))))
    (when (and old
               (disco-state-private-channel-p old)
               (not (disco-state-private-channel-p channel)))
      (setq disco-state--private-channels
            (cl-remove-if (lambda (it)
                            (equal (alist-get 'id it) channel-id))
                          (or disco-state--private-channels '()))))
    (when (and old (disco-state-channel-thread-p old))
      (disco-state--remove-thread-indexes old))
    (when channel-id
      (puthash channel-id channel disco-state--channels-by-id))
    (when guild-id
      (let ((channels (or (gethash guild-id disco-state--channels-by-guild) '())))
        (puthash guild-id
                 (disco-state--replace-or-append-channel channels channel)
                 disco-state--channels-by-guild)))
    (when (disco-state-private-channel-p channel)
      (setq disco-state--private-channels
            (disco-state--replace-or-append-channel
             (or disco-state--private-channels '())
             channel)))
    (when (disco-state-channel-thread-p channel)
      (disco-state--index-thread-channel channel)
      (let ((thread-member (alist-get 'member channel))
            (thread-member-count (alist-get 'member_count channel)))
        (when thread-member
          (let ((user-id (disco-state--thread-member-user-id thread-member)))
            (when user-id
              (disco-state-upsert-thread-member channel-id user-id))))
        (when (numberp thread-member-count)
          (disco-state-set-thread-member-count channel-id thread-member-count))))))

(defun disco-state-delete-channel (channel-id)
  "Delete channel CHANNEL-ID from indexes."
  (let ((channel (gethash channel-id disco-state--channels-by-id)))
    (when channel
      (let ((guild-id (alist-get 'guild_id channel)))
        (when guild-id
          (let* ((channels (or (gethash guild-id disco-state--channels-by-guild) '()))
                 (updated (cl-remove-if (lambda (it)
                                          (equal (alist-get 'id it) channel-id))
                                        channels)))
            (puthash guild-id updated disco-state--channels-by-guild)))
        (when (disco-state-private-channel-p channel)
          (setq disco-state--private-channels
                (cl-remove-if (lambda (it)
                                (equal (alist-get 'id it) channel-id))
                              (or disco-state--private-channels '()))))
        (when (disco-state-channel-thread-p channel)
          (disco-state--remove-thread-indexes channel)))
      (remhash channel-id disco-state--channels-by-id)
      (remhash channel-id disco-state--messages-by-channel)
      (remhash channel-id disco-state--unread-counts-by-channel)
      (remhash channel-id disco-state--last-read-message-id-by-channel)
      (remhash channel-id disco-state--last-read-pin-timestamp-by-channel)
      (remhash channel-id disco-state--ack-token-by-channel)))
  (remhash channel-id disco-state--thread-member-ids-by-thread)
  (remhash channel-id disco-state--thread-member-count-by-thread))

(defun disco-state-sync-threads (guild-id parent-channel-ids threads)
  "Sync active THREADS for GUILD-ID.

If PARENT-CHANNEL-IDS is nil, replace all known threads for that guild.
Otherwise, replace threads only under the provided parent IDs."
  (let* ((target-parents (and parent-channel-ids (copy-sequence parent-channel-ids)))
         (old-thread-ids (or (gethash guild-id disco-state--thread-ids-by-guild) '())))
    (dolist (thread-id old-thread-ids)
      (let* ((old-thread (gethash thread-id disco-state--channels-by-id))
             (parent-id (and old-thread (alist-get 'parent_id old-thread))))
        (when (and old-thread
                   (or (null target-parents)
                       (member parent-id target-parents)))
          (disco-state-delete-channel thread-id))))
    (dolist (thread threads)
      (disco-state-upsert-channel thread))))

(defun disco-state-put-messages (channel-id messages)
  "Store MESSAGES list for CHANNEL-ID."
  (puthash channel-id messages disco-state--messages-by-channel))

(defun disco-state-messages (channel-id)
  "Return messages list for CHANNEL-ID."
  (gethash channel-id disco-state--messages-by-channel))

(defun disco-state-channel-unread-count (channel-id)
  "Return unread count for CHANNEL-ID."
  (gethash channel-id disco-state--unread-counts-by-channel 0))

(defun disco-state-channel-own-unread-count (channel)
  "Return unread count tracked directly on CHANNEL."
  (disco-state-channel-unread-count (alist-get 'id channel)))

(defun disco-state-parent-thread-unread-total (parent-channel-id)
  "Return unread total aggregated from threads under PARENT-CHANNEL-ID."
  (let ((total 0))
    (dolist (thread (disco-state-parent-threads parent-channel-id))
      (setq total (+ total (disco-state-channel-own-unread-count thread))))
    total))

(defun disco-state-channel-effective-unread-count (channel)
  "Return unread count for CHANNEL including child-thread unread."
  (+ (disco-state-channel-own-unread-count channel)
     (disco-state-parent-thread-unread-total (alist-get 'id channel))))

(defun disco-state-channels-unread-total (channels)
  "Return aggregated unread count for CHANNELS."
  (let ((total 0))
    (dolist (channel channels)
      (setq total (+ total (disco-state-channel-own-unread-count channel))))
    total))

(defun disco-state-increment-channel-unread (channel-id &optional delta)
  "Increase unread count for CHANNEL-ID by DELTA (default 1)."
  (let* ((step (max 0 (or delta 1)))
         (next (+ (disco-state-channel-unread-count channel-id) step)))
    (puthash channel-id next disco-state--unread-counts-by-channel)
    next))

(defun disco-state-set-channel-unread (channel-id count)
  "Set unread COUNT for CHANNEL-ID and return normalized count."
  (let ((normalized (max 0 (or count 0))))
    (if (> normalized 0)
        (puthash channel-id normalized disco-state--unread-counts-by-channel)
      (remhash channel-id disco-state--unread-counts-by-channel))
    normalized))

(defun disco-state-clear-channel-unread (channel-id)
  "Reset unread count for CHANNEL-ID to zero."
  (remhash channel-id disco-state--unread-counts-by-channel)
  0)

(defun disco-state-channel-last-read-message-id (channel-id)
  "Return last acknowledged message ID for CHANNEL-ID, or nil."
  (gethash channel-id disco-state--last-read-message-id-by-channel))

(defun disco-state-set-channel-last-read-message-id (channel-id message-id)
  "Set CHANNEL-ID read cursor to MESSAGE-ID and return MESSAGE-ID."
  (puthash channel-id message-id disco-state--last-read-message-id-by-channel)
  message-id)

(defun disco-state-channel-last-read-pin-timestamp (channel-id)
  "Return last acknowledged pin timestamp for CHANNEL-ID, or nil."
  (gethash channel-id disco-state--last-read-pin-timestamp-by-channel))

(defun disco-state-set-channel-last-read-pin-timestamp (channel-id timestamp)
  "Set CHANNEL-ID acknowledged pin TIMESTAMP and return TIMESTAMP."
  (if timestamp
      (puthash channel-id timestamp disco-state--last-read-pin-timestamp-by-channel)
    (remhash channel-id disco-state--last-read-pin-timestamp-by-channel))
  timestamp)

(defun disco-state-channel-has-unread-pins-p (channel)
  "Return non-nil when CHANNEL has unacknowledged pinned-message updates."
  (let* ((channel-id (alist-get 'id channel))
         (channel-pin-timestamp (alist-get 'last_pin_timestamp channel))
         (read-pin-timestamp (and channel-id
                                  (disco-state-channel-last-read-pin-timestamp channel-id))))
    (and (stringp channel-pin-timestamp)
         (or (null read-pin-timestamp)
             (string-lessp read-pin-timestamp channel-pin-timestamp)))))

(defun disco-state-channel-own-has-unread-p (channel)
  "Return non-nil when CHANNEL itself has unread state."
  (let* ((channel-id (alist-get 'id channel))
         (last-message-id (alist-get 'last_message_id channel))
         (last-read-id (and channel-id
                            (disco-state-channel-last-read-message-id channel-id))))
    (or (> (disco-state-channel-own-unread-count channel) 0)
        (and (stringp last-message-id)
             (or (null last-read-id)
                 (disco-state-snowflake< last-read-id last-message-id)))
        (disco-state-channel-has-unread-pins-p channel))))

(defun disco-state-channel-has-unread-p (channel)
  "Return non-nil when CHANNEL has unread state, including child threads."
  (or (disco-state-channel-own-has-unread-p channel)
      (seq-some #'disco-state-channel-own-has-unread-p
                (disco-state-parent-threads (alist-get 'id channel)))))

(defun disco-state-channel-read-state-flags (channel-id)
  "Return calculated read-state flags integer for CHANNEL-ID."
  (let ((channel (disco-state-channel channel-id))
        (flags 0))
    (when (alist-get 'guild_id channel)
      (setq flags (logior flags disco-state-read-state-flag-is-guild-channel)))
    (when (disco-state-channel-thread-p channel)
      (setq flags (logior flags disco-state-read-state-flag-is-thread)))
    flags))

(defun disco-state-current-last-viewed-day ()
  "Return current `last_viewed' day value for Discord read-state ACK.

The value is the number of UTC days since the Discord epoch."
  (floor (/ (- (float-time) disco-state-discord-epoch-seconds)
            disco-state--seconds-per-day)))

(defun disco-state-channel-ack-fields (channel-id)
  "Return keyword plist for channel ACK payload fields.

Result contains `:flags' and `:last-viewed'."
  (list :flags (disco-state-channel-read-state-flags channel-id)
        :last-viewed (disco-state-current-last-viewed-day)))

(defun disco-state-apply-message-ack (channel-id message-id &optional mention-count)
  "Apply channel MESSAGE_ACK semantics to local state.

CHANNEL-ID identifies the channel read-state.
When MESSAGE-ID is non-nil, update the channel read cursor.
When MENTION-COUNT is an integer, update unread count; when omitted,
preserve current unread count per Discord read-state docs."
  (when channel-id
    (when message-id
      (disco-state-set-channel-last-read-message-id channel-id message-id))
    (when (numberp mention-count)
      (disco-state-set-channel-unread channel-id mention-count))))

(defun disco-state-apply-channel-pins-update (channel-id last-pin-timestamp)
  "Apply CHANNEL_PINS_UPDATE payload fields to local channel state."
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (when channel
      (let ((updated (copy-tree channel)))
        (setf (alist-get 'last_pin_timestamp updated) last-pin-timestamp)
        (disco-state-upsert-channel updated)
        t))))

(defun disco-state-apply-channel-pins-ack (channel-id timestamp)
  "Apply CHANNEL_PINS_ACK payload fields to local read-state."
  (when channel-id
    (disco-state-set-channel-last-read-pin-timestamp channel-id timestamp)))

(defun disco-state--normalize-id (value)
  "Return VALUE normalized as string ID."
  (format "%s" value))

(defun disco-state--message-author-id (message)
  "Return author ID from MESSAGE object."
  (let ((author (alist-get 'author message)))
    (alist-get 'id author)))

(defun disco-state--message-mentions-user-p (message current-user-id)
  "Return non-nil when MESSAGE mentions CURRENT-USER-ID."
  (let* ((normalized-user-id (disco-state--normalize-id current-user-id))
         (mentions (alist-get 'mentions message))
         (mention-everyone (eq t (alist-get 'mention_everyone message)))
         (mention-roles (alist-get 'mention_roles message))
         (member (alist-get 'member message))
         (member-roles (alist-get 'roles member))
         (member-role-id-set (mapcar #'disco-state--normalize-id member-roles))
         (direct-mention
          (seq-some
           (lambda (user)
             (equal normalized-user-id
                    (disco-state--normalize-id (alist-get 'id user))))
           mentions))
         (role-mention
          (seq-some
           (lambda (role-id)
             (member (disco-state--normalize-id role-id)
                     member-role-id-set))
           mention-roles)))
    (or direct-mention mention-everyone role-mention)))

(defun disco-state--channel-muted-p (channel)
  "Return non-nil when CHANNEL is muted."
  (eq t (alist-get 'muted channel)))

(defun disco-state--message-create-should-increment-unread-p (channel message current-user-id)
  "Return non-nil when MESSAGE_CREATE should increment unread for CHANNEL."
  (let ((message-type (alist-get 'type message)))
    (cond
     ((disco-state-private-channel-p channel)
      (and (/= message-type disco-state-message-type-recipient-remove)
           (or (not (disco-state--channel-muted-p channel))
               (disco-state--message-mentions-user-p message current-user-id))))
     ((alist-get 'guild_id channel)
      (disco-state--message-mentions-user-p message current-user-id))
     (t nil))))

(defun disco-state-apply-message-create (channel-id message current-user-id watched)
  "Apply MESSAGE_CREATE read-state effects for CHANNEL-ID.

MESSAGE is the gateway message object.
CURRENT-USER-ID identifies the active account user.
WATCHED means a room buffer currently tracks this channel."
  (let* ((channel (disco-state-channel channel-id))
         (message-id (alist-get 'id message))
         (message-type (alist-get 'type message))
         (author-id (disco-state--message-author-id message))
         (own-message (equal (disco-state--normalize-id author-id)
                             (disco-state--normalize-id current-user-id))))
    (when (and channel (not watched))
      (if own-message
          (when (/= message-type disco-state-message-type-poll-result)
            (disco-state-apply-message-ack channel-id message-id 0))
        (when (disco-state--message-create-should-increment-unread-p
               channel message current-user-id)
          (disco-state-increment-channel-unread channel-id 1))))))

(defun disco-state-apply-thread-create (thread current-user-id)
  "Apply THREAD_CREATE read-state effects from THREAD for CURRENT-USER-ID."
  (let* ((parent-id (alist-get 'parent_id thread))
         (thread-id (alist-get 'id thread))
         (owner-id (alist-get 'owner_id thread))
         (parent-channel (and parent-id
                              (disco-state-channel parent-id))))
    (when (and parent-id
               thread-id
               owner-id
               current-user-id
               (disco-state-thread-only-parent-channel-p parent-channel)
               (equal (disco-state--normalize-id owner-id)
                      (disco-state--normalize-id current-user-id)))
      (disco-state-apply-message-ack parent-id thread-id 0))))

(defun disco-state-apply-channel-unread (channel-unread)
  "Apply one gateway CHANNEL-UNREAD structure to local channel state."
  (let* ((channel-id (alist-get 'id channel-unread))
         (channel (and channel-id (disco-state-channel channel-id))))
    (when channel
      (let ((updated (copy-tree channel)))
        (when (assq 'last_message_id channel-unread)
          (setf (alist-get 'last_message_id updated)
                (alist-get 'last_message_id channel-unread)))
        (when (assq 'last_pin_timestamp channel-unread)
          (setf (alist-get 'last_pin_timestamp updated)
                (alist-get 'last_pin_timestamp channel-unread)))
        (disco-state-upsert-channel updated)
        t))))

(defun disco-state-apply-channel-unread-updates (channel-unread-updates)
  "Apply CHANNEL-UNREAD-UPDATES list and return number of applied updates."
  (let ((applied 0))
    (dolist (channel-unread channel-unread-updates)
      (when (disco-state-apply-channel-unread channel-unread)
        (setq applied (1+ applied))))
    applied))

(defun disco-state-apply-ready-read-state-entry (entry)
  "Apply one Ready/read-state ENTRY to local channel read-state.

Only entries of type `CHANNEL' are applied. Returns non-nil when an
entry is applied, else nil."
  (let ((read-state-type (or (alist-get 'read_state_type entry)
                             disco-state-read-state-type-channel))
        (channel-id (alist-get 'id entry))
        (message-id (alist-get 'last_message_id entry))
        (mention-count (alist-get 'mention_count entry)))
    (when (and (numberp read-state-type)
               (= read-state-type disco-state-read-state-type-channel)
               channel-id)
      (disco-state-apply-message-ack channel-id message-id mention-count)
      (when (assq 'last_pin_timestamp entry)
        (disco-state-set-channel-last-read-pin-timestamp
         channel-id
         (alist-get 'last_pin_timestamp entry)))
      t)))

(defun disco-state-channel-ack-token (channel-id)
  "Return read-state ack token for CHANNEL-ID, or nil."
  (gethash channel-id disco-state--ack-token-by-channel))

(defun disco-state-set-channel-ack-token (channel-id token)
  "Store read-state ack TOKEN for CHANNEL-ID and return TOKEN.

If TOKEN is nil, clear any stored token for CHANNEL-ID."
  (if token
      (puthash channel-id token disco-state--ack-token-by-channel)
    (remhash channel-id disco-state--ack-token-by-channel))
  token)

(defun disco-state-channel-ack-request-fields (channel-id)
  "Return keyword plist fields for channel ACK request.

Result contains `:token', `:flags', and `:last-viewed'."
  (let ((fields (disco-state-channel-ack-fields channel-id)))
    (list :token (disco-state-channel-ack-token channel-id)
          :flags (plist-get fields :flags)
          :last-viewed (plist-get fields :last-viewed))))

(defun disco-state-apply-channel-ack-response (channel-id response)
  "Apply read-state ACK RESPONSE payload for CHANNEL-ID.

When RESPONSE includes `token', update cached ack token accordingly."
  (let ((token-pair (assq 'token response)))
    (when token-pair
      (disco-state-set-channel-ack-token channel-id (cdr token-pair)))))

(defun disco-state-reset-ack-tokens ()
  "Clear all stored read-state ack tokens."
  (clrhash disco-state--ack-token-by-channel))

(defun disco-state-apply-user-update ()
  "Apply USER_UPDATE read-state side effects."
  (disco-state-reset-ack-tokens))

(defun disco-state-snowflake< (left right)
  "Return non-nil when snowflake LEFT is strictly less than RIGHT.

Comparison is numeric-safe for decimal snowflake strings."
  (when (and (stringp left) (stringp right))
    (let ((left-len (length left))
          (right-len (length right)))
      (if (= left-len right-len)
          (string-lessp left right)
        (< left-len right-len)))))

(defun disco-state-snowflake>= (left right)
  "Return non-nil when snowflake LEFT is greater than or equal to RIGHT."
  (and (stringp left)
       (stringp right)
       (or (equal left right)
           (disco-state-snowflake< right left))))

(provide 'disco-state)

;;; disco-state.el ends here
