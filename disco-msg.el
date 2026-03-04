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

(defun disco-msg-author-display-name (message)
  "Return best-effort author display name for MESSAGE."
  (let* ((author (and (listp message) (alist-get 'author message)))
         (member (and (listp message) (alist-get 'member message))))
    (or (and (listp member)
             (stringp (alist-get 'nick member))
             (not (string-empty-p (alist-get 'nick member)))
             (alist-get 'nick member))
        (and (listp author)
             (stringp (alist-get 'global_name author))
             (not (string-empty-p (alist-get 'global_name author)))
             (alist-get 'global_name author))
        (and (listp author)
             (stringp (alist-get 'username author))
             (not (string-empty-p (alist-get 'username author)))
             (alist-get 'username author))
        (and (listp author)
             (disco-msg-normalize-id (alist-get 'id author))))))

(defun disco-msg-preview-content (message)
  "Return compact one-line content preview for MESSAGE."
  (let* ((content (and (listp message)
                       (stringp (alist-get 'content message))
                       (string-trim (alist-get 'content message))))
         (attachments (or (and (listp message)
                               (alist-get 'attachments message))
                          '()))
         (embeds (or (and (listp message)
                          (alist-get 'embeds message))
                     '()))
         (poll (and (listp message)
                    (alist-get 'poll message)))
         (sticker-items (or (and (listp message)
                                 (alist-get 'sticker_items message))
                            '())))
    (cond
     ((and (stringp content)
           (not (string-empty-p content)))
      (replace-regexp-in-string "[\n\r\t ]+" " " content))
     ((and (listp poll) poll)
      "(poll)")
     ((> (length attachments) 0)
      (format "(%d attachment%s)"
              (length attachments)
              (if (= (length attachments) 1) "" "s")))
     ((> (length embeds) 0)
      "(embed)")
     ((> (length sticker-items) 0)
      "(sticker)")
     (t
      "(message)"))))

(defun disco-msg-preview-line (message)
  "Return compact single-line preview label for MESSAGE."
  (let ((author (disco-msg-author-display-name message))
        (content (disco-msg-preview-content message)))
    (if (and author (not (string-empty-p author)))
        (format "%s> %s" author content)
      content)))

(defun disco-msg-channel-last-cached-message (channel)
  "Return last cached message object for CHANNEL, or nil."
  (let* ((channel-id (and (listp channel)
                          (disco-msg-normalize-id (alist-get 'id channel))))
         (last-message-id (and (listp channel)
                               (disco-msg-normalize-id
                                (alist-get 'last_message_id channel))))
         (messages (and channel-id
                        (disco-state-messages channel-id))))
    (or (and last-message-id
             (disco-msg-find-in-messages messages last-message-id))
        (car messages))))

(defun disco-msg-channel-preview-line (channel)
  "Return best-effort cached preview line for CHANNEL, or nil."
  (when-let* ((message (disco-msg-channel-last-cached-message channel)))
    (disco-msg-preview-line message)))

(provide 'disco-msg)

;;; disco-msg.el ends here
