;;; disco-state.el --- In-memory state for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Central state container for guilds/channels and room-local message caches.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'disco-channel-type)
(require 'disco-read-state)
(require 'disco-util)

(defvar disco-state--guilds nil
  "List of guild objects as alists.")

(defvar disco-state--channels-by-guild (make-hash-table :test #'equal)
  "Hash table guild-id -> list of channel objects.")

(defvar disco-state--guild-channels-loaded (make-hash-table :test #'equal)
  "Hash table of guild IDs with complete channel snapshots.")

(defvar disco-state--channels-by-id (make-hash-table :test #'equal)
  "Hash table channel-id -> channel object.")

(defvar disco-state--private-channels nil
  "List of private DM-like channels as channel objects.")

(defvar disco-state--threads-by-parent (make-hash-table :test #'equal)
  "Hash table parent-channel-id -> list of thread channel objects.")

(defvar disco-state--thread-ids-by-guild (make-hash-table :test #'equal)
  "Hash table guild-id -> list of known thread channel IDs.")

(defvar disco-state--messages-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> list of message objects.")

(defvar disco-state--message-revision-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> latest message-cache mutation revision.")

(defvar disco-state--message-revisions-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> hash table of message-id mutation revisions.")

(defvar disco-state--read-states (make-hash-table :test #'equal)
  "Hash table (READ-STATE-TYPE . RESOURCE-ID) -> read-state object.")

(defvar disco-state--user-guild-settings (make-hash-table :test #'equal)
  "Hash table guild-id -> current user's notification settings.")

(defvar disco-state--channel-notification-overrides (make-hash-table :test #'equal)
  "Hash table channel-id -> current user's channel notification override.")

(defvar disco-state--ack-token-by-read-state (make-hash-table :test #'equal)
  "Hash table (READ-STATE-TYPE . RESOURCE-ID) -> last read-state ack token.")

(defvar disco-state--thread-member-ids-by-thread (make-hash-table :test #'equal)
  "Hash table thread-id -> list of known member user IDs.")

(defvar disco-state--thread-member-count-by-thread (make-hash-table :test #'equal)
  "Hash table thread-id -> last known thread member count.")

(defvar disco-state--presences-by-user (make-hash-table :test #'equal)
  "Hash table user-id -> latest user presence object.")

(defvar disco-state--presences-by-guild-user (make-hash-table :test #'equal)
  "Hash table (guild-id . user-id) -> latest guild-scoped presence object.")

(defvar disco-state--guild-members-by-guild-user (make-hash-table :test #'equal)
  "Hash table (guild-id . user-id) -> cached guild member object.")

(defvar disco-state--guild-member-ids-by-guild (make-hash-table :test #'equal)
  "Hash table guild-id -> list of cached guild member user IDs.")

(defvar disco-state--sessions nil
  "List of gateway session objects from SESSIONS_REPLACE.")

(defvar disco-state--voice-states-by-key (make-hash-table :test #'equal)
  "Hash table (user-id . session-id) -> latest voice-state object.")

(defvar disco-state--voice-state-keys-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> list of tracked (user-id . session-id) keys.")

(defvar disco-state--channel-member-counts-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> alist with member/presence counters.")

(defvar disco-state--conversation-summaries-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> list of conversation summary objects.")


(defconst disco-state-discord-epoch-seconds 1420070400
  "Discord epoch as UNIX seconds (2015-01-01 00:00:00 UTC).")

(defconst disco-state--seconds-per-day 86400
  "Number of seconds in one UTC day.")

(defconst disco-state-message-type-recipient-remove 2
  "Discord message type value for `RECIPIENT_REMOVE'.")

(defconst disco-state-message-type-poll-result 46
  "Discord message type value for `POLL_RESULT'.")

(defconst disco-state-message-preview-cache-limit 40
  "Maximum cached preview messages per channel from lightweight gateway responses.")

(defun disco-state--normalize-id (value)
  "Return VALUE normalized as string ID."
  (and value (format "%s" value)))

(defun disco-state--channel-in-guild (channel guild-id)
  "Return a copy of CHANNEL canonically owned by GUILD-ID."
  (when (listp channel)
    (let ((normalized (copy-tree channel)))
      (setf (alist-get 'guild_id normalized) guild-id)
      normalized)))

(defun disco-state--non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (> (length value) 0)))

(defun disco-state--read-state-key (read-state-type resource-id)
  "Return hash key for READ-STATE-TYPE and RESOURCE-ID."
  (when resource-id
    (cons read-state-type (disco-state--normalize-id resource-id))))

(defun disco-state-read-state (read-state-type resource-id)
  "Return read-state object for READ-STATE-TYPE and RESOURCE-ID."
  (let ((key (disco-state--read-state-key read-state-type resource-id)))
    (when key
      (copy-tree (gethash key disco-state--read-states)))))

(defun disco-state-read-state-counter-total (read-state-type)
  "Return aggregated unread counter value for READ-STATE-TYPE."
  (let ((counter-field (disco-read-state-counter-field read-state-type))
        (total 0))
    (when counter-field
      (maphash
       (lambda (key state)
         (when (and (consp key)
                    (= (car key) read-state-type))
           (let ((value (alist-get counter-field state)))
             (when (numberp value)
               (setq total (+ total (max 0 value)))))))
       disco-state--read-states))
    total))

(defun disco-state--upsert-read-state (read-state-type resource-id fields)
  "Upsert read-state for READ-STATE-TYPE/RESOURCE-ID with FIELDS alist."
  (let* ((normalized-resource-id (disco-state--normalize-id resource-id))
         (key (disco-state--read-state-key read-state-type normalized-resource-id))
         (state (and key
                     (or (copy-tree (gethash key disco-state--read-states))
                         `((read_state_type . ,read-state-type)
                           (id . ,normalized-resource-id))))))
    (when key
      (dolist (field fields)
        (setf (alist-get (car field) state nil nil #'eq)
              (cdr field)))
      (puthash key state disco-state--read-states)
      state)))

(defun disco-state--delete-read-state (read-state-type resource-id)
  "Delete read-state and ack token for READ-STATE-TYPE/RESOURCE-ID."
  (let ((key (disco-state--read-state-key read-state-type resource-id)))
    (when key
      (remhash key disco-state--read-states)
      (remhash key disco-state--ack-token-by-read-state))))

(defun disco-state-reset ()
  "Reset all in-memory state."
  (setq disco-state--guilds nil)
  (setq disco-state--private-channels nil)
  (setq disco-state--sessions nil)
  (clrhash disco-state--channels-by-guild)
  (clrhash disco-state--guild-channels-loaded)
  (clrhash disco-state--channels-by-id)
  (clrhash disco-state--threads-by-parent)
  (clrhash disco-state--thread-ids-by-guild)
  (clrhash disco-state--messages-by-channel)
  (clrhash disco-state--message-revision-by-channel)
  (clrhash disco-state--message-revisions-by-channel)
  (clrhash disco-state--read-states)
  (clrhash disco-state--user-guild-settings)
  (clrhash disco-state--channel-notification-overrides)
  (clrhash disco-state--ack-token-by-read-state)
  (clrhash disco-state--thread-member-ids-by-thread)
  (clrhash disco-state--thread-member-count-by-thread)
  (clrhash disco-state--presences-by-user)
  (clrhash disco-state--presences-by-guild-user)
  (clrhash disco-state--guild-members-by-guild-user)
  (clrhash disco-state--guild-member-ids-by-guild)
  (clrhash disco-state--voice-states-by-key)
  (clrhash disco-state--voice-state-keys-by-channel)
  (clrhash disco-state--channel-member-counts-by-channel)
  (clrhash disco-state--conversation-summaries-by-channel))

(defun disco-state-channel-thread-p (channel)
  "Return non-nil when CHANNEL is a thread channel."
  (disco-channel-thread-p channel))

(defun disco-state-private-channel-p (channel)
  "Return non-nil when CHANNEL is a private DM-like channel."
  (disco-channel-private-p channel))

(defun disco-state-thread-only-parent-channel-p (channel)
  "Return non-nil when CHANNEL is a thread-only parent channel."
  (disco-channel-thread-only-parent-p channel))

(defun disco-state--channel-age-restricted-p (channel &optional seen)
  "Return non-nil when CHANNEL or an ancestor is age-restricted.

SEEN is an internal list of visited channel IDs used to avoid recursion loops."
  (when (listp channel)
    (let* ((channel-id (alist-get 'id channel))
           (explicit-nsfw (assoc 'nsfw channel)))
      (cond
       ((and channel-id (member channel-id seen))
        nil)
       ((and explicit-nsfw
             (disco-util-json-true-p (cdr explicit-nsfw)))
        t)
       ((disco-state-channel-thread-p channel)
        (let* ((parent-id (alist-get 'parent_id channel))
               (parent (and parent-id
                            (disco-state-channel parent-id))))
          (when parent
            (disco-state--channel-age-restricted-p
             parent
             (if channel-id
                 (cons channel-id seen)
               seen)))))
       (t nil)))))

(defun disco-state-channel-age-restricted-p (channel)
  "Return non-nil when CHANNEL should be treated as age-restricted.

Thread channels inherit this status from their parent channel when Discord does
not send an explicit `nsfw' field on the thread itself."
  (disco-state--channel-age-restricted-p channel nil))

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
  "Set GUILDS list in memory and remove state for departed guilds."
  (let ((new-ids (delq nil (mapcar (lambda (guild)
                                     (disco-state--normalize-id
                                      (alist-get 'id guild)))
                                   guilds))))
    (dolist (old-guild (copy-sequence disco-state--guilds))
      (let ((old-id (disco-state--normalize-id (alist-get 'id old-guild))))
        (when (and old-id (not (member old-id new-ids)))
          (disco-state-delete-guild old-id)))))
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
  (disco-state-remove-voice-states-for-guild guild-id)
  (remhash guild-id disco-state--channels-by-guild)
  (remhash guild-id disco-state--guild-channels-loaded)
  (remhash guild-id disco-state--thread-ids-by-guild))

(defun disco-state-guild-channels-loaded-p (guild-id)
  "Return non-nil when GUILD-ID has a complete channel snapshot."
  (and (gethash (disco-state--normalize-id guild-id)
                disco-state--guild-channels-loaded)
       t))

(defun disco-state-put-channels (guild-id channels)
  "Store complete non-thread CHANNELS snapshot for GUILD-ID.

Active thread entities are an independent Gateway/API collection and remain
indexed when the guild channel directory is refreshed."
  (setq guild-id (disco-state--normalize-id guild-id))
  (setq channels
        (delq nil
              (mapcar (lambda (channel)
                        (disco-state--channel-in-guild channel guild-id))
                      channels)))
  (let* ((old-channels (copy-sequence
                        (or (gethash guild-id disco-state--channels-by-guild) '())))
         (new-ids (delq nil (mapcar (lambda (channel)
                                     (disco-state--normalize-id
                                      (alist-get 'id channel)))
                                   channels)))
         (preserved-threads
          (seq-filter
           (lambda (channel)
             (and (disco-state-channel-thread-p channel)
                  (not (member (disco-state--normalize-id (alist-get 'id channel))
                               new-ids))))
           old-channels)))
    (dolist (old-channel old-channels)
      (let ((old-id (disco-state--normalize-id (alist-get 'id old-channel))))
        (when (and old-id
                   (not (disco-state-channel-thread-p old-channel))
                   (not (member old-id new-ids)))
          (disco-state-delete-channel old-id))))
    (puthash guild-id (append channels preserved-threads)
             disco-state--channels-by-guild))
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
              (disco-state-set-thread-member-count channel-id thread-member-count)))))))
  (puthash guild-id t disco-state--guild-channels-loaded))

(defun disco-state-guild-channels (guild-id)
  "Return channels for GUILD-ID."
  (gethash (disco-state--normalize-id guild-id)
           disco-state--channels-by-guild))

(defun disco-state-channel (channel-id)
  "Return channel object for CHANNEL-ID."
  (gethash (disco-state--normalize-id channel-id)
           disco-state--channels-by-id))

(defun disco-state--settings-guild-key (guild-id)
  "Return stable settings key for possibly nil GUILD-ID."
  (if guild-id (disco-state--normalize-id guild-id) :private))

(defun disco-state-user-guild-setting (guild-id)
  "Return current user's notification setting for GUILD-ID.

Nil GUILD-ID addresses the private-channel settings entry."
  (copy-tree
   (gethash (disco-state--settings-guild-key guild-id)
            disco-state--user-guild-settings)))

(defun disco-state-channel-notification-override (channel-id)
  "Return current user's notification override for CHANNEL-ID."
  (copy-tree
   (gethash (disco-state--normalize-id channel-id)
            disco-state--channel-notification-overrides)))

(defun disco-state-channel-notification-level (channel)
  "Return effective message notification level for CHANNEL.

Values follow Discord: 0 all messages, 1 mentions only, and 2 none."
  (let* ((channel-id (alist-get 'id channel))
         (guild-id (alist-get 'guild_id channel))
         (override (disco-state-channel-notification-override channel-id))
         (override-level (and override (alist-get 'message_notifications override)))
         (setting (disco-state-user-guild-setting guild-id))
         (guild-level (and setting (alist-get 'message_notifications setting)))
         (guild (and guild-id
                     (seq-find (lambda (it) (equal (alist-get 'id it) guild-id))
                               disco-state--guilds)))
         (default-level (and guild (alist-get 'default_message_notifications guild))))
    (cond
     ((disco-state-private-channel-p channel) 0)
     ((and (integerp override-level) (/= override-level 3)) override-level)
     ((and (integerp guild-level) (/= guild-level 3)) guild-level)
     ((integerp default-level) default-level)
     (t 1))))

(defun disco-state-apply-user-guild-setting (setting)
  "Store one user guild SETTING and replace its channel override index."
  (when (listp setting)
    (let* ((guild-id (alist-get 'guild_id setting))
           (key (disco-state--settings-guild-key guild-id))
           (old (gethash key disco-state--user-guild-settings)))
      (dolist (override (or (alist-get 'channel_overrides old) '()))
        (when-let* ((channel-id (alist-get 'channel_id override)))
          (remhash (disco-state--normalize-id channel-id)
                   disco-state--channel-notification-overrides)))
      (puthash key (copy-tree setting) disco-state--user-guild-settings)
      (dolist (override (or (alist-get 'channel_overrides setting) '()))
        (when-let* ((channel-id (alist-get 'channel_id override)))
          (puthash (disco-state--normalize-id channel-id)
                   (copy-tree override)
                   disco-state--channel-notification-overrides)))
      setting)))

(defun disco-state-set-user-guild-settings (settings)
  "Replace current user's notification SETTINGS and override indexes."
  (clrhash disco-state--user-guild-settings)
  (clrhash disco-state--channel-notification-overrides)
  (dolist (setting (or settings '()))
    (disco-state-apply-user-guild-setting setting))
  settings)

(defun disco-state-channels ()
  "Return all indexed channels without duplicates."
  (let (channels)
    (maphash (lambda (_channel-id channel)
               (push channel channels))
             disco-state--channels-by-id)
    channels))

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
      (disco-state--delete-read-state disco-read-state-type-channel channel-id)))
  (let ((voice-state-keys
         (copy-sequence (or (gethash channel-id disco-state--voice-state-keys-by-channel)
                            '()))))
    (dolist (voice-key voice-state-keys)
      (remhash voice-key disco-state--voice-states-by-key)))
  (remhash channel-id disco-state--thread-member-ids-by-thread)
  (remhash channel-id disco-state--thread-member-count-by-thread)
  (remhash channel-id disco-state--conversation-summaries-by-channel)
  (remhash channel-id disco-state--channel-member-counts-by-channel)
  (remhash channel-id disco-state--voice-state-keys-by-channel))

(defun disco-state-sync-threads (guild-id parent-channel-ids threads)
  "Sync active THREADS for GUILD-ID.

If PARENT-CHANNEL-IDS is nil, replace all known threads for that guild.
Otherwise, replace threads only under the provided parent IDs."
  (setq guild-id (disco-state--normalize-id guild-id))
  (setq threads
        (delq nil
              (mapcar (lambda (thread)
                        (disco-state--channel-in-guild thread guild-id))
                      threads)))
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
  (let* ((normalized-channel-id (disco-state--normalize-id channel-id))
         (old (gethash normalized-channel-id disco-state--messages-by-channel))
         (old-by-id (make-hash-table :test #'equal))
         (new-by-id (make-hash-table :test #'equal))
         changed-ids)
    (dolist (message old)
      (when-let* ((message-id
                   (disco-state--normalize-id (alist-get 'id message))))
        (puthash message-id message old-by-id)))
    (dolist (message messages)
      (when-let* ((message-id
                   (disco-state--normalize-id (alist-get 'id message))))
        (puthash message-id message new-by-id)))
    (maphash
     (lambda (message-id old-message)
       (unless (equal old-message (gethash message-id new-by-id))
         (push message-id changed-ids)))
     old-by-id)
    (maphash
     (lambda (message-id new-message)
       (unless (equal new-message (gethash message-id old-by-id))
         (cl-pushnew message-id changed-ids :test #'equal)))
     new-by-id)
    (when changed-ids
      (let* ((revision (1+ (gethash normalized-channel-id
                                    disco-state--message-revision-by-channel
                                    0)))
             (revisions
              (or (gethash normalized-channel-id
                           disco-state--message-revisions-by-channel)
                  (let ((table (make-hash-table :test #'equal)))
                    (puthash normalized-channel-id table
                             disco-state--message-revisions-by-channel)
                    table))))
        (puthash normalized-channel-id revision
                 disco-state--message-revision-by-channel)
        (dolist (message-id changed-ids)
          (puthash message-id revision revisions))))
    (puthash normalized-channel-id messages disco-state--messages-by-channel)))

(defun disco-state-messages (channel-id)
  "Return messages list for CHANNEL-ID."
  (gethash (disco-state--normalize-id channel-id)
           disco-state--messages-by-channel))

(defun disco-state-message-revision (channel-id)
  "Return the latest message-cache mutation revision for CHANNEL-ID."
  (gethash (disco-state--normalize-id channel-id)
           disco-state--message-revision-by-channel
           0))

(defun disco-state--message-mutated-after-p (channel-id message-id revision)
  "Return non-nil when MESSAGE-ID in CHANNEL-ID changed after REVISION."
  (let ((revisions
         (gethash (disco-state--normalize-id channel-id)
                  disco-state--message-revisions-by-channel)))
    (> (if revisions
           (gethash (disco-state--normalize-id message-id) revisions 0)
         0)
       revision)))

(defun disco-state-merge-message-page (channel-id messages request-revision)
  "Merge REST page MESSAGES into CHANNEL-ID at REQUEST-REVISION.

Gateway and local mutations newer than REQUEST-REVISION take precedence,
including deletions.  Return the resulting newest-first message list."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (message (append (disco-state-messages channel-id)
                             (cl-remove-if
                              (lambda (candidate)
                                (when-let* ((message-id (alist-get 'id candidate)))
                                  (disco-state--message-mutated-after-p
                                   channel-id message-id request-revision)))
                              messages)))
      (when-let* ((message-id
                   (disco-state--normalize-id (alist-get 'id message))))
        (unless (gethash message-id seen)
          (puthash message-id t seen)
          (push message merged))))
    (setq merged
          (sort merged
                (lambda (left right)
                  (disco-state-snowflake< (alist-get 'id right)
                                           (alist-get 'id left)))))
    (disco-state-put-messages channel-id merged)
    merged))

(defun disco-state-upsert-message (channel-id message)
  "Insert or replace MESSAGE in CHANNEL-ID cache, returning new message list."
  (let* ((normalized-channel-id (disco-state--normalize-id channel-id))
         (message-id (disco-state--normalize-id (alist-get 'id message)))
         (nonce (disco-state--normalize-id (alist-get 'nonce message))))
    (when (and normalized-channel-id
               message-id
               (listp message))
      (let* ((existing (copy-sequence
                        (or (gethash normalized-channel-id
                                     disco-state--messages-by-channel)
                            '())))
             (updated (cons message
                            (cl-remove-if
                             (lambda (it)
                               (or (equal (disco-state--normalize-id (alist-get 'id it))
                                          message-id)
                                   (and nonce
                                        (equal (disco-state--normalize-id
                                                (alist-get 'nonce it))
                                               nonce))))
                             existing))))
        (when (and (integerp disco-state-message-preview-cache-limit)
                   (> disco-state-message-preview-cache-limit 0)
                   (> (length updated) disco-state-message-preview-cache-limit))
          (setq updated (seq-take updated disco-state-message-preview-cache-limit)))
        (disco-state-put-messages normalized-channel-id updated)
        updated))))

(defun disco-state-insert-pending-message (channel-id nonce content current-user-id
                                                      &optional reply-to-message-id)
  "Insert exact local pending message identified by NONCE into CHANNEL-ID."
  (let ((message
         `((id . ,(format "%s" nonce))
           (nonce . ,(format "%s" nonce))
           (channel_id . ,(format "%s" channel-id))
           (content . ,(or content ""))
           (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
           (pending . t)
           (author (id . ,(format "%s" current-user-id))
                   (username . "You"))
           ,@(when reply-to-message-id
               `((message_reference
                  (message_id . ,(format "%s" reply-to-message-id))
                  (channel_id . ,(format "%s" channel-id))))))))
    (disco-state-upsert-message channel-id message)
    message))

(defun disco-state-remove-pending-message (channel-id nonce)
  "Remove pending message identified by NONCE from CHANNEL-ID."
  (let* ((normalized (disco-state--normalize-id nonce))
         (messages (disco-state-messages channel-id))
         (updated
          (cl-remove-if
           (lambda (message)
             (and (alist-get 'pending message)
                  (equal normalized
                         (disco-state--normalize-id (alist-get 'nonce message)))))
           messages)))
    (disco-state-put-messages channel-id updated)
    updated))

(defun disco-state-apply-last-messages (messages)
  "Apply LAST_MESSAGES payload MESSAGES and return affected channel IDs."
  (let (channel-ids)
    (dolist (message (or messages '()))
      (let ((channel-id (disco-state--normalize-id (alist-get 'channel_id message))))
        (when (and channel-id (listp message))
          (disco-state-upsert-message channel-id message)
          (cl-pushnew channel-id channel-ids :test #'equal))))
    (nreverse channel-ids)))

(defun disco-state-sessions ()
  "Return copy of tracked gateway sessions list."
  (copy-tree (or disco-state--sessions '())))

(defun disco-state-set-sessions (sessions)
  "Replace tracked gateway SESSIONS list."
  (setq disco-state--sessions (copy-tree (or sessions '()))))

(defun disco-state-overall-session ()
  "Return synthetic `all' session object when present."
  (seq-find
   (lambda (session)
     (equal (disco-state--normalize-id (alist-get 'session_id session))
            "all"))
   (or disco-state--sessions '())))

(defun disco-state--presence-key (guild-id user-id)
  "Return key for GUILD-ID and USER-ID presence entry."
  (cons (disco-state--normalize-id guild-id)
        (disco-state--normalize-id user-id)))

(defun disco-state--guild-member-key (guild-id user-id)
  "Return key for GUILD-ID and USER-ID guild member cache entry."
  (cons (disco-state--normalize-id guild-id)
        (disco-state--normalize-id user-id)))

(defun disco-state--guild-member-user-id (member)
  "Extract normalized user id from guild MEMBER object."
  (let ((user (and (listp member) (alist-get 'user member))))
    (disco-state--normalize-id
     (or (and (listp user) (alist-get 'id user))
         (and (listp member) (alist-get 'user_id member))))))

(defun disco-state-guild-member (guild-id user-id)
  "Return copied cached guild member for GUILD-ID and USER-ID."
  (let ((key (disco-state--guild-member-key guild-id user-id)))
    (when (and (consp key)
               (cdr key))
      (copy-tree (gethash key disco-state--guild-members-by-guild-user)))))

(defun disco-state-guild-members (guild-id)
  "Return copied cached guild member list for GUILD-ID."
  (let ((normalized-guild-id (disco-state--normalize-id guild-id))
        result)
    (when normalized-guild-id
      (dolist (user-id (or (gethash normalized-guild-id disco-state--guild-member-ids-by-guild)
                           '()))
        (when-let* ((member (disco-state-guild-member normalized-guild-id user-id)))
          (push member result))))
    (nreverse result)))

(defun disco-state-upsert-guild-member (guild-id member)
  "Cache guild MEMBER object for GUILD-ID and return copied member."
  (let* ((normalized-guild-id (disco-state--normalize-id guild-id))
         (user-id (disco-state--guild-member-user-id member))
         (key (and normalized-guild-id user-id
                   (disco-state--guild-member-key normalized-guild-id user-id))))
    (when key
      (puthash key (copy-tree member) disco-state--guild-members-by-guild-user)
      (let ((existing (copy-sequence (or (gethash normalized-guild-id
                                                  disco-state--guild-member-ids-by-guild)
                                         '()))))
        (unless (member user-id existing)
          (puthash normalized-guild-id
                   (append existing (list user-id))
                   disco-state--guild-member-ids-by-guild)))
      (copy-tree member))))

(defun disco-state-apply-guild-members-chunk (guild-id members &optional presences)
  "Apply cached guild MEMBERS chunk and optional PRESENCES for GUILD-ID.

Return list of cached member user IDs."
  (let ((normalized-guild-id (disco-state--normalize-id guild-id))
        result)
    (when normalized-guild-id
      (dolist (member (or members '()))
        (when-let* ((cached (disco-state-upsert-guild-member normalized-guild-id member))
                    (user-id (disco-state--guild-member-user-id cached)))
          (push user-id result)))
      (dolist (presence (or presences '()))
        (disco-state-apply-presence-update
         (append `((guild_id . ,normalized-guild-id))
                 (copy-tree (or presence '())))))
      (nreverse result))))

(defun disco-state-presence (user-id &optional guild-id)
  "Return copied presence for USER-ID, optionally scoped to GUILD-ID."
  (let ((normalized-user-id (disco-state--normalize-id user-id))
        (normalized-guild-id (disco-state--normalize-id guild-id)))
    (when normalized-user-id
      (copy-tree
       (if normalized-guild-id
           (gethash (disco-state--presence-key normalized-guild-id normalized-user-id)
                    disco-state--presences-by-guild-user)
         (gethash normalized-user-id disco-state--presences-by-user))))))

(defun disco-state-presences (&optional guild-id)
  "Return copied list of tracked presences, optionally scoped to GUILD-ID."
  (let ((normalized-guild-id (disco-state--normalize-id guild-id))
        result)
    (if normalized-guild-id
        (maphash
         (lambda (key presence)
           (when (and (consp key)
                      (equal (car key) normalized-guild-id))
             (push (copy-tree presence) result)))
         disco-state--presences-by-guild-user)
      (maphash
       (lambda (_user-id presence)
         (push (copy-tree presence) result))
       disco-state--presences-by-user))
    (nreverse result)))

(defun disco-state-apply-presence-update (presence)
  "Apply PRESENCE_UPDATE payload PRESENCE and return non-nil when stored."
  (let* ((user (alist-get 'user presence))
         (user-id (disco-state--normalize-id
                   (or (alist-get 'id presence)
                       (and (listp user) (alist-get 'id user)))))
         (guild-id (disco-state--normalize-id (alist-get 'guild_id presence))))
    (when (and user-id (listp presence))
      (puthash user-id (copy-tree presence) disco-state--presences-by-user)
      (when guild-id
        (puthash (disco-state--presence-key guild-id user-id)
                 (copy-tree presence)
                 disco-state--presences-by-guild-user))
      t)))

(defun disco-state-channel-member-count (channel-id)
  "Return last known member_count for CHANNEL-ID, or nil."
  (let* ((normalized-channel-id (disco-state--normalize-id channel-id))
         (entry (and normalized-channel-id
                     (gethash normalized-channel-id
                              disco-state--channel-member-counts-by-channel))))
    (and (listp entry)
         (numberp (alist-get 'member_count entry))
         (alist-get 'member_count entry))))

(defun disco-state-channel-presence-count (channel-id)
  "Return last known presence_count for CHANNEL-ID, or nil."
  (let* ((normalized-channel-id (disco-state--normalize-id channel-id))
         (entry (and normalized-channel-id
                     (gethash normalized-channel-id
                              disco-state--channel-member-counts-by-channel))))
    (and (listp entry)
         (numberp (alist-get 'presence_count entry))
         (alist-get 'presence_count entry))))

(defun disco-state-apply-channel-member-count-update (channel-id member-count presence-count)
  "Apply CHANNEL_MEMBER_COUNT_UPDATE fields for CHANNEL-ID."
  (let ((normalized-channel-id (disco-state--normalize-id channel-id)))
    (when normalized-channel-id
      (let ((entry (or (copy-tree (gethash normalized-channel-id
                                           disco-state--channel-member-counts-by-channel))
                       `((id . ,normalized-channel-id)))))
        (when (numberp member-count)
          (setf (alist-get 'member_count entry nil nil #'eq)
                (max 0 member-count)))
        (when (numberp presence-count)
          (setf (alist-get 'presence_count entry nil nil #'eq)
                (max 0 presence-count)))
        (puthash normalized-channel-id entry
                 disco-state--channel-member-counts-by-channel)
        entry))))

(defun disco-state--voice-state-key (voice-state)
  "Return stable voice-state key for VOICE-STATE payload."
  (let ((user-id (disco-state--normalize-id (alist-get 'user_id voice-state)))
        (session-id (disco-state--normalize-id (alist-get 'session_id voice-state))))
    (when user-id
      (cons user-id (or session-id "")))))

(defun disco-state--voice-state-channel-id (voice-state)
  "Return normalized channel id from VOICE-STATE payload."
  (disco-state--normalize-id (alist-get 'channel_id voice-state)))

(defun disco-state--voice-state-guild-id (voice-state)
  "Return normalized guild id from VOICE-STATE payload."
  (disco-state--normalize-id (alist-get 'guild_id voice-state)))

(defun disco-state--voice-state-user-id-from-key (voice-key)
  "Return user id component from VOICE-KEY cons cell."
  (cond
   ((consp voice-key)
    (disco-state--normalize-id (car voice-key)))
   ((stringp voice-key)
    voice-key)
   (t nil)))

(defun disco-state--voice-remove-key-from-channel (channel-id voice-key)
  "Remove VOICE-KEY from CHANNEL-ID index."
  (let* ((normalized-channel-id (disco-state--normalize-id channel-id))
         (existing (and normalized-channel-id
                        (copy-sequence
                         (or (gethash normalized-channel-id
                                      disco-state--voice-state-keys-by-channel)
                             '()))))
         (updated (cl-remove voice-key existing :test #'equal)))
    (when normalized-channel-id
      (if updated
          (puthash normalized-channel-id updated
                   disco-state--voice-state-keys-by-channel)
        (remhash normalized-channel-id disco-state--voice-state-keys-by-channel)))))

(defun disco-state--voice-add-key-to-channel (channel-id voice-key)
  "Add VOICE-KEY to CHANNEL-ID index."
  (let ((normalized-channel-id (disco-state--normalize-id channel-id)))
    (when normalized-channel-id
      (let ((existing (copy-sequence
                       (or (gethash normalized-channel-id
                                    disco-state--voice-state-keys-by-channel)
                           '()))))
        (cl-pushnew voice-key existing :test #'equal)
        (puthash normalized-channel-id existing
                 disco-state--voice-state-keys-by-channel)))))

(defun disco-state-channel-voice-user-ids (channel-id)
  "Return unique user IDs currently tracked in voice CHANNEL-ID."
  (let ((keys (copy-sequence
               (or (gethash (disco-state--normalize-id channel-id)
                            disco-state--voice-state-keys-by-channel)
                   '())))
        user-ids)
    (dolist (voice-key keys)
      (let ((user-id (disco-state--voice-state-user-id-from-key voice-key)))
        (when user-id
          (cl-pushnew user-id user-ids :test #'equal))))
    (nreverse user-ids)))

(defun disco-state-channel-voice-member-count (channel-id)
  "Return count of unique users currently tracked in voice CHANNEL-ID."
  (length (disco-state-channel-voice-user-ids channel-id)))

(defun disco-state-voice-active-channel-count ()
  "Return number of channels with tracked voice participants."
  (let ((count 0))
    (maphash
     (lambda (_channel-id voice-keys)
       (when voice-keys
         (setq count (1+ count))))
     disco-state--voice-state-keys-by-channel)
    count))

(defun disco-state-voice-active-user-count ()
  "Return number of unique users with tracked voice states."
  (let (user-ids)
    (maphash
     (lambda (voice-key _voice-state)
       (let ((user-id (disco-state--voice-state-user-id-from-key voice-key)))
         (when user-id
           (cl-pushnew user-id user-ids :test #'equal))))
     disco-state--voice-states-by-key)
    (length user-ids)))

(defun disco-state-apply-voice-state-update (voice-state)
  "Apply VOICE_STATE_UPDATE payload VOICE-STATE.

Returns plist containing :user-id, :session-id, :guild-id, :channel-id,
:previous-channel-id and :channel-ids (affected channel ids)."
  (let* ((voice-key (disco-state--voice-state-key voice-state))
         (old-state (and voice-key
                         (copy-tree (gethash voice-key disco-state--voice-states-by-key))))
         (new-state (and voice-key (listp voice-state) (copy-tree voice-state)))
         (old-channel-id (and old-state
                              (disco-state--voice-state-channel-id old-state)))
         (new-channel-id (and new-state
                              (disco-state--voice-state-channel-id new-state)))
         (guild-id (or (and new-state (disco-state--voice-state-guild-id new-state))
                       (and old-state (disco-state--voice-state-guild-id old-state))))
         (user-id (and voice-key
                       (disco-state--voice-state-user-id-from-key voice-key)))
         (session-id (and (consp voice-key)
                          (disco-state--normalize-id (cdr voice-key))))
         channel-ids)
    (when voice-key
      (when (and old-channel-id
                 (not (equal old-channel-id new-channel-id)))
        (disco-state--voice-remove-key-from-channel old-channel-id voice-key))
      (if new-channel-id
          (progn
            (disco-state--voice-add-key-to-channel new-channel-id voice-key)
            (puthash voice-key new-state disco-state--voice-states-by-key))
        (remhash voice-key disco-state--voice-states-by-key))
      (when old-channel-id
        (cl-pushnew old-channel-id channel-ids :test #'equal))
      (when new-channel-id
        (cl-pushnew new-channel-id channel-ids :test #'equal))
      (list :user-id user-id
            :session-id session-id
            :guild-id guild-id
            :channel-id new-channel-id
            :previous-channel-id old-channel-id
            :channel-ids (nreverse channel-ids)))))

(defun disco-state--remove-voice-states-by-predicate (predicate)
  "Remove tracked voice states matching PREDICATE.

PREDICATE is called with key and voice-state object.
Returns list of affected channel ids."
  (let (keys channel-ids)
    (maphash
     (lambda (voice-key voice-state)
       (when (funcall predicate voice-key voice-state)
         (push voice-key keys)))
     disco-state--voice-states-by-key)
    (dolist (voice-key keys)
      (let* ((voice-state (gethash voice-key disco-state--voice-states-by-key))
             (channel-id (and voice-state
                              (disco-state--voice-state-channel-id voice-state))))
        (when channel-id
          (disco-state--voice-remove-key-from-channel channel-id voice-key)
          (cl-pushnew channel-id channel-ids :test #'equal))
        (remhash voice-key disco-state--voice-states-by-key)))
    (nreverse channel-ids)))

(defun disco-state-remove-voice-states-for-user (user-id)
  "Remove tracked voice states for USER-ID and return affected channels."
  (let ((normalized-user-id (disco-state--normalize-id user-id)))
    (disco-state--remove-voice-states-by-predicate
     (lambda (voice-key _voice-state)
       (equal (disco-state--voice-state-user-id-from-key voice-key)
              normalized-user-id)))))

(defun disco-state-remove-voice-states-for-user-in-guild (user-id guild-id)
  "Remove tracked voice states for USER-ID scoped to GUILD-ID."
  (let ((normalized-user-id (disco-state--normalize-id user-id))
        (normalized-guild-id (disco-state--normalize-id guild-id)))
    (disco-state--remove-voice-states-by-predicate
     (lambda (voice-key voice-state)
       (and (equal (disco-state--voice-state-user-id-from-key voice-key)
                   normalized-user-id)
            (equal (disco-state--voice-state-guild-id voice-state)
                   normalized-guild-id))))))

(defun disco-state-remove-voice-states-for-guild (guild-id)
  "Remove tracked voice states for GUILD-ID and return affected channels."
  (let ((normalized-guild-id (disco-state--normalize-id guild-id)))
    (disco-state--remove-voice-states-by-predicate
     (lambda (_voice-key voice-state)
       (equal (disco-state--voice-state-guild-id voice-state)
              normalized-guild-id)))))

(defun disco-state-apply-passive-voice-state-snapshot (guild-id voice-states)
  "Apply passive voice state snapshot for GUILD-ID.

VOICE-STATES is a full guild-scoped voice-state list.
Returns affected channel IDs."
  (let ((normalized-guild-id (disco-state--normalize-id guild-id))
        channel-ids)
    (setq channel-ids
          (append channel-ids
                  (disco-state-remove-voice-states-for-guild normalized-guild-id)))
    (dolist (voice-state (or voice-states '()))
      (let* ((voice-state-with-guild
              (if (assq 'guild_id voice-state)
                  voice-state
                (cons (cons 'guild_id normalized-guild-id) voice-state)))
             (delta (disco-state-apply-voice-state-update voice-state-with-guild)))
        (setq channel-ids
              (append (plist-get delta :channel-ids)
                      channel-ids))))
    (seq-uniq (nreverse channel-ids) #'equal)))

(defun disco-state-apply-passive-voice-state-updates (guild-id updated removed-user-ids)
  "Apply passive voice deltas for GUILD-ID and return affected channel IDs.

UPDATED is list of updated voice states.
REMOVED-USER-IDS is list of user IDs removed from voice state."
  (let ((normalized-guild-id (disco-state--normalize-id guild-id))
        channel-ids)
    (dolist (voice-state (or updated '()))
      (let* ((voice-state-with-guild
              (if (assq 'guild_id voice-state)
                  voice-state
                (cons (cons 'guild_id normalized-guild-id) voice-state)))
             (delta (disco-state-apply-voice-state-update voice-state-with-guild)))
        (setq channel-ids
              (append (plist-get delta :channel-ids)
                      channel-ids))))
    (dolist (user-id (or removed-user-ids '()))
      (setq channel-ids
            (append (disco-state-remove-voice-states-for-user-in-guild
                     user-id normalized-guild-id)
                    channel-ids)))
    (seq-uniq (nreverse channel-ids) #'equal)))

(defun disco-state--update-channel-fields (channel-id fields)
  "Update CHANNEL-ID object with FIELDS and return non-nil when updated."
  (let* ((normalized-channel-id (disco-state--normalize-id channel-id))
         (channel (and normalized-channel-id
                       (disco-state-channel normalized-channel-id))))
    (when channel
      (let ((updated (copy-tree channel))
            (changed nil))
        (dolist (field fields)
          (let ((key (car field))
                (value (cdr field)))
            (unless (equal (alist-get key updated nil nil #'eq) value)
              (setq changed t))
            (setf (alist-get key updated nil nil #'eq) value)))
        (when changed
          (disco-state-upsert-channel updated))
        changed))))

(defun disco-state-apply-channel-status-update (channel-id status)
  "Apply voice/status field update for CHANNEL-ID."
  (disco-state--update-channel-fields
   channel-id
   `((status . ,status))))

(defun disco-state-apply-channel-voice-start-time-update (channel-id voice-start-time)
  "Apply voice_start_time field update for CHANNEL-ID."
  (disco-state--update-channel-fields
   channel-id
   `((voice_start_time . ,voice-start-time))))

(defun disco-state-apply-channel-statuses (channels)
  "Apply CHANNEL_STATUSES response CHANNELS and return updated channel IDs."
  (let (channel-ids)
    (dolist (channel-status (or channels '()))
      (let ((channel-id (alist-get 'id channel-status)))
        (when (and channel-id
                   (assq 'status channel-status)
                   (disco-state-apply-channel-status-update
                    channel-id
                    (alist-get 'status channel-status)))
          (cl-pushnew (disco-state--normalize-id channel-id)
                      channel-ids
                      :test #'equal))))
    (nreverse channel-ids)))

(defun disco-state-apply-channel-info (channels)
  "Apply CHANNEL_INFO response CHANNELS and return updated channel IDs."
  (let (channel-ids)
    (dolist (channel-info (or channels '()))
      (let ((channel-id (alist-get 'id channel-info))
            fields)
        (when (assq 'status channel-info)
          (push `(status . ,(alist-get 'status channel-info)) fields))
        (when (assq 'voice_start_time channel-info)
          (push `(voice_start_time . ,(alist-get 'voice_start_time channel-info))
                fields))
        (when (and channel-id
                   fields
                   (disco-state--update-channel-fields channel-id (nreverse fields)))
          (cl-pushnew (disco-state--normalize-id channel-id)
                      channel-ids
                      :test #'equal))))
    (nreverse channel-ids)))

(defun disco-state-channel-conversation-summaries (channel-id)
  "Return copied conversation summaries list for CHANNEL-ID."
  (copy-tree
   (or (gethash (disco-state--normalize-id channel-id)
                disco-state--conversation-summaries-by-channel)
       '())))

(defun disco-state--conversation-summary-sort-id (summary)
  "Return snowflake-like sort key for conversation SUMMARY object."
  (or (disco-state--normalize-id (alist-get 'end_id summary))
      (disco-state--normalize-id (alist-get 'id summary))))

(defun disco-state-channel-conversation-summary-preview (channel-id)
  "Return best effort preview text from CHANNEL-ID conversation summaries."
  (let* ((summary (car (disco-state-channel-conversation-summaries channel-id)))
         (short (and (listp summary) (alist-get 'summ_short summary)))
         (topic (and (listp summary) (alist-get 'topic summary))))
    (cond
     ((disco-state--non-empty-string-p short)
      short)
     ((disco-state--non-empty-string-p topic)
      topic)
     (t nil))))

(defun disco-state-apply-conversation-summary-update (channel-id summaries)
  "Apply CONVERSATION_SUMMARY_UPDATE for CHANNEL-ID.

SUMMARIES are merged by summary `id'.
Returns updated summaries list."
  (let ((normalized-channel-id (disco-state--normalize-id channel-id)))
    (when normalized-channel-id
      (let ((existing
             (copy-tree
              (or (gethash normalized-channel-id
                           disco-state--conversation-summaries-by-channel)
                  '()))))
        (dolist (summary (or summaries '()))
          (let ((summary-id (disco-state--normalize-id (alist-get 'id summary))))
            (when summary-id
              (setq existing
                    (cl-remove-if
                     (lambda (it)
                       (equal (disco-state--normalize-id (alist-get 'id it))
                              summary-id))
                     existing)))
            (when (listp summary)
              (push (copy-tree summary) existing))))
        (setq existing
              (sort existing
                    (lambda (left right)
                      (let ((left-id (disco-state--conversation-summary-sort-id left))
                            (right-id (disco-state--conversation-summary-sort-id right)))
                        (cond
                         ((and left-id right-id)
                          (disco-state-snowflake< right-id left-id))
                         (left-id t)
                         (right-id nil)
                         (t nil))))))
        (puthash normalized-channel-id existing
                 disco-state--conversation-summaries-by-channel)
        existing))))

(defun disco-state-channel-unread-count (channel-id)
  "Return unread count for CHANNEL-ID."
  (let* ((state (disco-state-read-state disco-read-state-type-channel channel-id))
         (mention-count (and (listp state)
                             (alist-get 'mention_count state))))
    (if (numberp mention-count)
        (max 0 mention-count)
      0)))

(defun disco-state-channel-unread-mention-count (channel-id)
  "Return high-importance unread mention count for CHANNEL-ID.

Discord may use `mention_count' for ordinary ALL_MESSAGES notifications.  A
read state whose `is-mention-low-importance' flag is set contains no actual
ping and therefore contributes zero here."
  (let* ((state (disco-state-read-state disco-read-state-type-channel channel-id))
         (flags (and (listp state) (alist-get 'flags state)))
         (count (disco-state-channel-unread-count channel-id)))
    (if (and (numberp flags)
             (/= 0 (logand flags
                           disco-read-state-flag-is-mention-low-importance)))
        0
      count)))

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
    (disco-state-set-channel-unread channel-id next)
    next))

(defun disco-state-set-channel-unread (channel-id count)
  "Set unread COUNT for CHANNEL-ID and return normalized count."
  (let ((normalized (max 0 (or count 0))))
    (disco-state--upsert-read-state
     disco-read-state-type-channel
     channel-id
     `((mention_count . ,normalized)))
    normalized))

(defun disco-state-clear-channel-unread (channel-id)
  "Reset unread count for CHANNEL-ID to zero."
  (disco-state-set-channel-unread channel-id 0)
  0)

(defun disco-state-channel-last-read-message-id (channel-id)
  "Return last acknowledged message ID for CHANNEL-ID, or nil."
  (let ((state (disco-state-read-state disco-read-state-type-channel channel-id)))
    (and (listp state)
         (alist-get 'last_message_id state))))

(defun disco-state-set-channel-last-read-message-id (channel-id message-id)
  "Set CHANNEL-ID read cursor to MESSAGE-ID and return MESSAGE-ID."
  (disco-state--upsert-read-state
   disco-read-state-type-channel
   channel-id
   `((last_message_id . ,message-id)))
  message-id)

(defun disco-state-channel-last-read-pin-timestamp (channel-id)
  "Return last acknowledged pin timestamp for CHANNEL-ID, or nil."
  (let ((state (disco-state-read-state disco-read-state-type-channel channel-id)))
    (and (listp state)
         (alist-get 'last_pin_timestamp state))))

(defun disco-state-set-channel-last-read-pin-timestamp (channel-id timestamp)
  "Set CHANNEL-ID acknowledged pin TIMESTAMP and return TIMESTAMP."
  (disco-state--upsert-read-state
   disco-read-state-type-channel
   channel-id
   `((last_pin_timestamp . ,timestamp)))
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

(defun disco-state-channel-known-unread-message-count (channel)
  "Return the locally known unread message count for CHANNEL.

Count cached messages newer than the read cursor.  Like ningen's
`ChannelCountUnreads', return one when the channel is known to be unread but
the local cache cannot establish a larger count.  The result is therefore a
useful lower bound, not a protocol-provided exact total."
  (let* ((channel-id (alist-get 'id channel))
         (last-read-id (and channel-id
                            (disco-state-channel-last-read-message-id channel-id)))
         (messages (and channel-id (disco-state-messages channel-id)))
         (known
          (if (stringp last-read-id)
              (cl-count-if
               (lambda (message)
                 (when-let* ((message-id
                              (disco-state--normalize-id (alist-get 'id message))))
                   (disco-state-snowflake< last-read-id message-id)))
               messages)
            0)))
    (if (> known 0)
        known
      (if (disco-state-channel-own-has-unread-p channel) 1 0))))

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
      (setq flags (logior flags disco-read-state-flag-is-guild-channel)))
    (when (disco-state-channel-thread-p channel)
      (setq flags (logior flags disco-read-state-flag-is-thread)))
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

(defun disco-state-apply-message-ack (channel-id message-id
                                                 &optional mention-count flags
                                                 last-viewed version)
  "Apply channel MESSAGE_ACK semantics to local state.

CHANNEL-ID identifies the channel read-state.
When MESSAGE-ID is non-nil, update the channel read cursor.
When MENTION-COUNT is an integer, update unread count; when omitted,
preserve current unread count per Discord read-state docs.
FLAGS, LAST-VIEWED and VERSION are applied when provided."
  (when channel-id
    (let (fields)
      (when message-id
        (push `(last_message_id . ,message-id) fields))
      (when (numberp mention-count)
        (push `(mention_count . ,(max 0 mention-count)) fields))
      (when (numberp flags)
        (push `(flags . ,flags) fields))
      (when (numberp last-viewed)
        (push `(last_viewed . ,last-viewed) fields))
      (when (numberp version)
        (push `(version . ,version) fields))
      (when fields
        (disco-state--upsert-read-state
         disco-read-state-type-channel
         channel-id
         (nreverse fields))))))

(defun disco-state-apply-channel-pins-update (channel-id last-pin-timestamp)
  "Apply CHANNEL_PINS_UPDATE payload fields to local channel state."
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (when channel
      (let ((updated (copy-tree channel)))
        (setf (alist-get 'last_pin_timestamp updated) last-pin-timestamp)
        (disco-state-upsert-channel updated)
        t))))

(defun disco-state-apply-channel-pins-ack (channel-id timestamp &optional version)
  "Apply CHANNEL_PINS_ACK payload fields to local read-state."
  (when channel-id
    (let ((fields `((last_pin_timestamp . ,timestamp))))
      (when (numberp version)
        (setq fields (append fields `((version . ,version)))))
      (disco-state--upsert-read-state
       disco-read-state-type-channel
       channel-id
       fields))))

(defun disco-state-apply-feature-ack (read-state-type resource-id entity-id &optional version)
  "Apply feature ACK semantics for READ-STATE-TYPE/RESOURCE-ID.

ENTITY-ID is stored as the latest acknowledged entity cursor and unread badge
counter is reset to zero. VERSION is stored when provided."
  (let ((entity-field (disco-read-state-entity-field read-state-type))
        (counter-field (disco-read-state-counter-field read-state-type)))
    (when (and (numberp read-state-type)
               resource-id
               entity-id
               entity-field)
      (let ((fields `((,entity-field . ,(disco-state--normalize-id entity-id)))))
        (when counter-field
          (setq fields (append fields `((,counter-field . 0)))))
        (when (numberp version)
          (setq fields (append fields `((version . ,version)))))
        (disco-state--upsert-read-state
         read-state-type
         resource-id
         fields)
        t))))

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

(defun disco-state--mute-active-p (object)
  "Return non-nil when OBJECT describes an active mute."
  (when (and (listp object)
             (disco-util-json-true-p (alist-get 'muted object)))
    (let* ((config (alist-get 'mute_config object))
           (window (and (listp config) (alist-get 'selected_time_window config)))
           (end-time (and (listp config) (alist-get 'end_time config))))
      (or (not (listp config))
          (equal window -1)
          (not (stringp end-time))
          (condition-case nil
              (time-less-p (current-time) (date-to-time end-time))
            (error t))))))

(defun disco-state--channel-muted-p (channel &optional seen)
  "Return non-nil when CHANNEL is effectively muted.

Honor direct channel overrides, parent/category inheritance, guild or private
settings, and temporary mute expiration.  SEEN prevents malformed parent
cycles."
  (let* ((channel-id (disco-state--normalize-id (alist-get 'id channel)))
         (guild-id (alist-get 'guild_id channel))
         (parent-id (disco-state--normalize-id (alist-get 'parent_id channel)))
         (override (and channel-id
                        (gethash channel-id
                                 disco-state--channel-notification-overrides)))
         (settings (gethash (disco-state--settings-guild-key guild-id)
                            disco-state--user-guild-settings)))
    (and (not (member channel-id seen))
         (or (disco-state--mute-active-p override)
             (disco-state--mute-active-p channel)
             (disco-state--mute-active-p settings)
             (when (and parent-id (not (member parent-id seen)))
               (when-let* ((parent (disco-state-channel parent-id)))
                 (disco-state--channel-muted-p
                  parent (cons channel-id seen))))))))

(defun disco-state-channel-muted-p (channel)
  "Return non-nil when CHANNEL is muted for the current user."
  (disco-state--channel-muted-p channel))

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
         (message-id (disco-state--normalize-id (alist-get 'id message)))
         (message-type (alist-get 'type message))
         (author-id (disco-state--message-author-id message))
         (own-message (equal (disco-state--normalize-id author-id)
                             (disco-state--normalize-id current-user-id))))
    (when channel
      (let ((channel-last-message-id
             (disco-state--normalize-id (alist-get 'last_message_id channel))))
        (when (and message-id
                   (or (null channel-last-message-id)
                       (disco-state-snowflake< channel-last-message-id message-id)))
          (let ((updated (copy-tree channel)))
            (setf (alist-get 'last_message_id updated) message-id)
            (disco-state-upsert-channel updated))))
      (unless watched
        (if own-message
            (when (/= message-type disco-state-message-type-poll-result)
              (disco-state-apply-message-ack channel-id message-id 0))
          (when (disco-state--message-create-should-increment-unread-p
                 channel message current-user-id)
            (let* ((state (disco-state-read-state
                           disco-read-state-type-channel channel-id))
                   (old-count (disco-state-channel-unread-count channel-id))
                   (old-flags (or (and state (alist-get 'flags state))
                                  (disco-state-channel-read-state-flags channel-id)))
                   (ping-p (disco-state--message-mentions-user-p
                            message current-user-id))
                   (flags
                    (if ping-p
                        (logand old-flags
                                (lognot disco-read-state-flag-is-mention-low-importance))
                      (if (or (= old-count 0)
                              (/= 0 (logand old-flags
                                            disco-read-state-flag-is-mention-low-importance)))
                          (logior old-flags
                                  disco-read-state-flag-is-mention-low-importance)
                        old-flags))))
              (disco-state--upsert-read-state
               disco-read-state-type-channel channel-id
               `((mention_count . ,(1+ old-count))
                 (flags . ,flags))))))))))

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
  "Apply one Ready/read-state ENTRY to local read-state store.

Returns non-nil when ENTRY can be normalized and applied."
  (let ((read-state-type (or (alist-get 'read_state_type entry)
                             disco-read-state-type-channel))
        (resource-id (alist-get 'id entry))
        (state-fields
         '(last_message_id
           last_acked_id
           mention_count
           badge_count
           last_pin_timestamp
           flags
           last_viewed
           version))
        fields)
    (when (and (numberp read-state-type)
               resource-id)
      (dolist (field state-fields)
        (when (assq field entry)
          (push (cons field (alist-get field entry)) fields)))
      (disco-state--upsert-read-state
       read-state-type
       resource-id
       (nreverse fields))
      t)))

(defun disco-state-read-state-ack-token (read-state-type resource-id)
  "Return read-state ack token for READ-STATE-TYPE/RESOURCE-ID."
  (let ((key (disco-state--read-state-key read-state-type resource-id)))
    (when key
      (gethash key disco-state--ack-token-by-read-state))))

(defun disco-state-set-read-state-ack-token (read-state-type resource-id token)
  "Store read-state ack TOKEN for READ-STATE-TYPE/RESOURCE-ID.

If TOKEN is nil, clear any stored token for the read-state."
  (let ((key (disco-state--read-state-key read-state-type resource-id)))
    (when key
      (if token
          (puthash key token disco-state--ack-token-by-read-state)
        (remhash key disco-state--ack-token-by-read-state)))
    token))

(defun disco-state-channel-ack-token (channel-id)
  "Return read-state ack token for CHANNEL-ID, or nil."
  (disco-state-read-state-ack-token disco-read-state-type-channel channel-id))

(defun disco-state-set-channel-ack-token (channel-id token)
  "Store read-state ack TOKEN for CHANNEL-ID and return TOKEN."
  (disco-state-set-read-state-ack-token
   disco-read-state-type-channel
   channel-id
   token))

(defun disco-state-channel-ack-request-fields (channel-id)
  "Return keyword plist fields for channel ACK request.

Result contains `:token', `:flags', and `:last-viewed'."
  (let ((fields (disco-state-channel-ack-fields channel-id)))
    (list :token (disco-state-channel-ack-token channel-id)
          :flags (plist-get fields :flags)
          :last-viewed (plist-get fields :last-viewed))))

(defun disco-state-apply-read-state-ack-response (read-state-type resource-id response)
  "Apply read-state ACK RESPONSE payload for READ-STATE-TYPE/RESOURCE-ID.

When RESPONSE includes `token', update cached ack token accordingly."
  (let ((token-pair (assq 'token response)))
    (when token-pair
      (disco-state-set-read-state-ack-token
       read-state-type
       resource-id
       (cdr token-pair)))))

(defun disco-state-apply-channel-ack-response (channel-id response)
  "Apply channel read-state ACK RESPONSE payload for CHANNEL-ID."
  (disco-state-apply-read-state-ack-response
   disco-read-state-type-channel
   channel-id
   response))

(defun disco-state-reset-ack-tokens ()
  "Clear all stored read-state ack tokens."
  (clrhash disco-state--ack-token-by-read-state))

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
