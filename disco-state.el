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

(defvar disco-state--messages-by-channel (make-hash-table :test #'equal)
  "Hash table channel-id -> list of message objects.")

(defun disco-state-reset ()
  "Reset all in-memory state."
  (setq disco-state--guilds nil)
  (clrhash disco-state--channels-by-guild)
  (clrhash disco-state--channels-by-id)
  (clrhash disco-state--messages-by-channel))

(defun disco-state-set-guilds (guilds)
  "Set GUILDS list in memory."
  (setq disco-state--guilds guilds))

(defun disco-state-guilds ()
  "Return guild list from memory."
  disco-state--guilds)

(defun disco-state-put-channels (guild-id channels)
  "Store CHANNELS list for GUILD-ID and index channels by ID."
  (puthash guild-id channels disco-state--channels-by-guild)
  (dolist (channel channels)
    (let ((channel-id (alist-get 'id channel)))
      (when channel-id
        (puthash channel-id channel disco-state--channels-by-id)))))

(defun disco-state-guild-channels (guild-id)
  "Return channels for GUILD-ID."
  (gethash guild-id disco-state--channels-by-guild))

(defun disco-state-channel (channel-id)
  "Return channel object for CHANNEL-ID."
  (gethash channel-id disco-state--channels-by-id))

(defun disco-state-put-messages (channel-id messages)
  "Store MESSAGES list for CHANNEL-ID."
  (puthash channel-id messages disco-state--messages-by-channel))

(defun disco-state-messages (channel-id)
  "Return messages list for CHANNEL-ID."
  (gethash channel-id disco-state--messages-by-channel))

(provide 'disco-state)

;;; disco-state.el ends here
