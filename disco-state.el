;;; disco-state.el --- In-memory state for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Central state container for guilds/channels and room-local message caches.

;;; Code:

(require 'cl-lib)

(defvar disco-state--guilds nil
  "List of guild objects as alists.")

(defvar disco-state--channels-by-guild (make-hash-table :test #'equal)
  "Hash table guild-id -> list of channel objects.")

(defvar disco-state--channels-by-id (make-hash-table :test #'equal)
  "Hash table channel-id -> channel object.")

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

(defvar disco-state--ack-token-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> last read-state ack token.")

(defun disco-state-reset ()
  "Reset all in-memory state."
  (setq disco-state--guilds nil)
  (clrhash disco-state--channels-by-guild)
  (clrhash disco-state--channels-by-id)
  (clrhash disco-state--threads-by-parent)
  (clrhash disco-state--thread-ids-by-guild)
  (clrhash disco-state--messages-by-channel)
  (clrhash disco-state--unread-counts-by-channel)
  (clrhash disco-state--last-read-message-id-by-channel)
  (clrhash disco-state--ack-token-by-channel))

(defun disco-state-channel-thread-p (channel)
  "Return non-nil when CHANNEL is a thread channel."
  (memq (alist-get 'type channel) '(10 11 12)))

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
          (remhash thread-id disco-state--channels-by-id)))))
  (puthash guild-id channels disco-state--channels-by-guild)
  (dolist (channel channels)
    (let ((channel-id (alist-get 'id channel)))
      (when channel-id
        (puthash channel-id channel disco-state--channels-by-id)
        (when (disco-state-channel-thread-p channel)
          (disco-state--index-thread-channel channel))))))

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
    (when (and old (disco-state-channel-thread-p old))
      (disco-state--remove-thread-indexes old))
    (when channel-id
      (puthash channel-id channel disco-state--channels-by-id))
    (when guild-id
      (let ((channels (or (gethash guild-id disco-state--channels-by-guild) '())))
        (puthash guild-id
                 (disco-state--replace-or-append-channel channels channel)
                 disco-state--channels-by-guild)))
    (when (disco-state-channel-thread-p channel)
      (disco-state--index-thread-channel channel))))

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
        (when (disco-state-channel-thread-p channel)
          (disco-state--remove-thread-indexes channel)))
      (remhash channel-id disco-state--channels-by-id)
      (remhash channel-id disco-state--messages-by-channel)
      (remhash channel-id disco-state--unread-counts-by-channel)
      (remhash channel-id disco-state--last-read-message-id-by-channel)
      (remhash channel-id disco-state--ack-token-by-channel))))

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

(defun disco-state-reset-ack-tokens ()
  "Clear all stored read-state ack tokens."
  (clrhash disco-state--ack-token-by-channel))

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
