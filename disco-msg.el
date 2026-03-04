;;; disco-msg.el --- Message model helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared helpers for Discord message identity/reference access.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'disco-state)

(defconst disco-msg--reference-field-map
  '((id . message_id)
    (channel-id . channel_id)
    (guild-id . guild_id))
  "Declarative mapping from reference field role to payload key.")

(defun disco-msg-normalize-id (value)
  "Return normalized snowflake-like ID string from VALUE, or nil.

String IDs are trimmed; integer IDs are stringified. Empty values are
rejected."
  (let ((normalized
         (cond
          ((stringp value) (string-trim value))
          ((integerp value) (number-to-string value))
          (t nil))))
    (when (and (stringp normalized)
               (not (string-empty-p normalized)))
      normalized)))

(defun disco-msg-id (message)
  "Return normalized message ID for MESSAGE, or nil."
  (disco-msg-normalize-id (and (listp message) (alist-get 'id message))))

(defun disco-msg-reference (message)
  "Return message_reference object from MESSAGE, or nil."
  (let ((reference (and (listp message) (alist-get 'message_reference message))))
    (and (listp reference) reference)))

(defun disco-msg-reference-field (message field-role)
  "Return normalized reference field FIELD-ROLE from MESSAGE.

FIELD-ROLE is one of `id', `channel-id', or `guild-id'."
  (let* ((reference (disco-msg-reference message))
         (field-key (alist-get field-role disco-msg--reference-field-map)))
    (disco-msg-normalize-id
     (and reference field-key (alist-get field-key reference)))))

(defun disco-msg-reference-id (message)
  "Return referenced message ID from MESSAGE, or nil."
  (disco-msg-reference-field message 'id))

(defun disco-msg-reference-channel-id (message)
  "Return referenced channel ID from MESSAGE, or nil."
  (disco-msg-reference-field message 'channel-id))

(defun disco-msg-reference-guild-id (message)
  "Return referenced guild ID from MESSAGE, or nil."
  (disco-msg-reference-field message 'guild-id))

(defun disco-msg-find-in-messages (messages message-id)
  "Return message object with MESSAGE-ID from MESSAGES, or nil.

ID comparison is normalized for snowflake strings."
  (let ((normalized-target-id (disco-msg-normalize-id message-id)))
    (when normalized-target-id
      (seq-find
       (lambda (message)
         (equal (disco-msg-id message) normalized-target-id))
       (or messages '())))))

(defun disco-msg-find-in-channel (channel-id message-id)
  "Return cached MESSAGE-ID from CHANNEL-ID, or nil."
  (let ((normalized-channel-id (disco-msg-normalize-id channel-id)))
    (when normalized-channel-id
      (disco-msg-find-in-messages
       (disco-state-messages normalized-channel-id)
       message-id))))

(provide 'disco-msg)

;;; disco-msg.el ends here
