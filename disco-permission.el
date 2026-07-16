;;; disco-permission.el --- Permission helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared permission parsing and checks for Discord channel permission bitfields.
;;
;; Permission names and values follow Discord's permission table:
;; https://discord.com/developers/docs/topics/permissions#permissions-bitwise-permission-flags

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-channel-type)

(defconst disco-permission-bits
  `((create-instant-invite               . ,(ash 1 0))
    (kick-members                        . ,(ash 1 1))
    (ban-members                         . ,(ash 1 2))
    (administrator                       . ,(ash 1 3))
    (manage-channels                     . ,(ash 1 4))
    (manage-guild                        . ,(ash 1 5))
    (add-reactions                       . ,(ash 1 6))
    (view-audit-log                      . ,(ash 1 7))
    (priority-speaker                    . ,(ash 1 8))
    (stream                              . ,(ash 1 9))
    (view-channel                        . ,(ash 1 10))
    (send-messages                       . ,(ash 1 11))
    (send-tts-messages                   . ,(ash 1 12))
    (manage-messages                     . ,(ash 1 13))
    (embed-links                         . ,(ash 1 14))
    (attach-files                        . ,(ash 1 15))
    (read-message-history                . ,(ash 1 16))
    (mention-everyone                    . ,(ash 1 17))
    (use-external-emojis                 . ,(ash 1 18))
    (use-external-emoji                  . ,(ash 1 18))
    (view-guild-insights                 . ,(ash 1 19))
    (connect                             . ,(ash 1 20))
    (speak                               . ,(ash 1 21))
    (mute-members                        . ,(ash 1 22))
    (deafen-members                      . ,(ash 1 23))
    (move-members                        . ,(ash 1 24))
    (use-vad                             . ,(ash 1 25))
    (change-nickname                     . ,(ash 1 26))
    (manage-nicknames                    . ,(ash 1 27))
    (manage-roles                        . ,(ash 1 28))
    (manage-webhooks                     . ,(ash 1 29))
    (manage-guild-expressions            . ,(ash 1 30))
    ;; Backward-compat aliases used by older docs/clients.
    (manage-emojis-and-stickers          . ,(ash 1 30))
    (manage-expressions                  . ,(ash 1 30))
    (use-application-commands            . ,(ash 1 31))
    (use-slash-commands                  . ,(ash 1 31))
    (request-to-speak                    . ,(ash 1 32))
    (manage-events                       . ,(ash 1 33))
    (manage-threads                      . ,(ash 1 34))
    (create-public-threads               . ,(ash 1 35))
    (create-private-threads              . ,(ash 1 36))
    (use-external-stickers               . ,(ash 1 37))
    (use-external-sticker                . ,(ash 1 37))
    (send-messages-in-threads            . ,(ash 1 38))
    (use-embedded-activities             . ,(ash 1 39))
    (moderate-members                    . ,(ash 1 40))
    (view-creator-monetization-analytics . ,(ash 1 41))
    (use-soundboard                      . ,(ash 1 42))
    (create-guild-expressions            . ,(ash 1 43))
    ;; 
    (create-expressions                  . ,(ash 1 43))
    (create-events                       . ,(ash 1 44))
    (use-external-sounds                 . ,(ash 1 45))
    (send-voice-messages                 . ,(ash 1 46))
    ;; 
    (use-clyde-ai                        . ,(ash 1 47))
    (set-voice-channel-status            . ,(ash 1 48))
    (send-polls                          . ,(ash 1 49))
    (use-external-apps                   . ,(ash 1 50))
    (pin-messages                        . ,(ash 1 51))
    (bypass-slowmode                     . ,(ash 1 52)))
  "Known Discord permission bits keyed by normalized symbols.")

(defconst disco-permission-administrator
  (or (alist-get 'administrator disco-permission-bits) 0)
  "ADMINISTRATOR permission bit.")

(defun disco-permission--normalize-name (permission)
  "Normalize PERMISSION designator to a canonical symbol.

PERMISSION may be a symbol, keyword, or string like VIEW_CHANNEL."
  (let* ((raw
          (cond
           ((keywordp permission) (substring (symbol-name permission) 1))
           ((symbolp permission) (symbol-name permission))
           ((stringp permission) permission)
           (t nil))))
    (when raw
      (let ((trimmed (replace-regexp-in-string "\\`:+" "" (string-trim raw))))
        (unless (string-empty-p trimmed)
          (intern
           (replace-regexp-in-string
            "[[:space:]_]+"
            "-"
            (downcase trimmed))))))))

(defun disco-permission-bit (permission)
  "Return integer bit mask for PERMISSION.

PERMISSION accepts:
- integer bit mask
- symbol/keyword (for example `view-channel' or `:view-channel')
- string (for example VIEW_CHANNEL).

Return nil when PERMISSION cannot be resolved."
  (cond
   ((and (integerp permission) (>= permission 0))
    permission)
   (t
    (let ((name (disco-permission--normalize-name permission)))
      (and name
           (alist-get name disco-permission-bits))))))

(defun disco-permission-parse-bitfield (value)
  "Parse permission bitfield VALUE into an integer or nil.

VALUE may be an integer or decimal string."
  (cond
   ((and (integerp value) (>= value 0))
    value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))
   (t nil)))

(defun disco-permission-display-name (permission)
  "Return human-readable display name for PERMISSION designator."
  (let* ((raw (cond
               ((keywordp permission) (substring (symbol-name permission) 1))
               ((symbolp permission) (symbol-name permission))
               ((stringp permission) permission)
               ((integerp permission) (format "0x%X" permission))
               (t (format "%s" permission))))
         (trimmed (replace-regexp-in-string "\\`:+" "" raw))
         (snake (replace-regexp-in-string "[[:space:]-]+" "_" trimmed)))
    (upcase snake)))

(defun disco-permission-channel-known-p (channel)
  "Return non-nil when CHANNEL has a parseable computed permissions field."
  (let ((raw (and (listp channel) (alist-get 'permissions channel))))
    (not (null (disco-permission-parse-bitfield raw)))))

(cl-defun disco-permission-has-p (bitfield permission &optional (unknown-value t))
  "Return non-nil when BITFIELD includes PERMISSION.

BITFIELD is a permission bitfield integer or decimal string.
PERMISSION is any designator accepted by `disco-permission-bit'.
When BITFIELD is missing/unparseable, return UNKNOWN-VALUE.

`ADMINISTRATOR' is treated as an override for all other permissions."
  (let ((bits (disco-permission-parse-bitfield bitfield))
        (required (disco-permission-bit permission)))
    (unless required
      (error "disco: unknown permission %S" permission))
    (if (null bits)
        unknown-value
      (or (not (zerop (logand bits required)))
          (and (/= required disco-permission-administrator)
               (not (zerop (logand bits disco-permission-administrator))))))))

(cl-defun disco-permission-has-all-p (bitfield permissions &optional (unknown-value t))
  "Return non-nil when BITFIELD includes every item in PERMISSIONS.

PERMISSIONS is a list of permission designators accepted by
`disco-permission-bit'."
  (let ((ok t))
    (dolist (permission (or permissions '()) ok)
      (unless (disco-permission-has-p bitfield permission unknown-value)
        (setq ok nil)))))

(cl-defun disco-permission-channel-has-p (channel permission &optional (unknown-value t))
  "Return non-nil when CHANNEL's computed permissions include PERMISSION.

When CHANNEL has no `permissions' field, return UNKNOWN-VALUE."
  (disco-permission-has-p
   (and (listp channel) (alist-get 'permissions channel))
   permission
   unknown-value))

(cl-defun disco-permission-channel-has-all-p (channel permissions &optional (unknown-value t))
  "Return non-nil when CHANNEL includes every item in PERMISSIONS.

When CHANNEL has no `permissions' field, UNKNOWN-VALUE controls fallback."
  (disco-permission-has-all-p
   (and (listp channel) (alist-get 'permissions channel))
   permissions
   unknown-value))

(cl-defun disco-permission-channel-missing (channel permissions &optional (unknown-value t))
  "Return missing PERMISSIONS for CHANNEL as normalized symbols.

When UNKNOWN-VALUE is non-nil and channel permissions are unavailable,
permissions are treated as satisfied."
  (let (missing)
    (dolist (permission (or permissions '()) (nreverse missing))
      (unless (disco-permission-channel-has-p channel permission unknown-value)
        (push (or (disco-permission--normalize-name permission)
                  permission)
              missing)))))

(cl-defun disco-permission-ensure-channel (channel permissions &key action (unknown-value t))
  "Signal `user-error' when CHANNEL misses PERMISSIONS.

ACTION is optional human-readable text appended as \\='for ACTION\\='.
When UNKNOWN-VALUE is non-nil, missing or unparsable channel permissions are
considered satisfied. Return t when the permission check succeeds."
  (let ((missing (disco-permission-channel-missing channel permissions unknown-value)))
    (when missing
      (user-error
       "disco: missing permission%s %s%s"
       (if (> (length missing) 1) "s" "")
       (mapconcat #'disco-permission-display-name missing ", ")
       (if (and (stringp action) (not (string-empty-p action)))
           (format " for %s" action)
         ""))))
  t)

(defun disco-permission-error-missing-access-p (err)
  "Return non-nil when ERR is a Discord missing-access response.

This matches common REST failures such as HTTP 403 with Discord error code
`50001' or message text \\='Missing Access\\='."
  (and (consp err)
       (eq (car err) 'disco-api-error)
       (let* ((data (cdr err))
              (status (nth 1 data))
              (body (nth 2 data))
              (code (and (listp body) (alist-get 'code body)))
              (message (and (listp body) (alist-get 'message body))))
         (and (equal status 403)
              (or (equal code 50001)
                  (equal message "Missing Access"))))))

(provide 'disco-permission)

;;; disco-permission.el ends here
