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

(defconst disco-permission-bits
  '((create-instant-invite . #x0000000000000001)
    (kick-members . #x0000000000000002)
    (ban-members . #x0000000000000004)
    (administrator . #x0000000000000008)
    (manage-channels . #x0000000000000010)
    (manage-guild . #x0000000000000020)
    (add-reactions . #x0000000000000040)
    (view-audit-log . #x0000000000000080)
    (priority-speaker . #x0000000000000100)
    (stream . #x0000000000000200)
    (view-channel . #x0000000000000400)
    (send-messages . #x0000000000000800)
    (send-tts-messages . #x0000000000001000)
    (manage-messages . #x0000000000002000)
    (embed-links . #x0000000000004000)
    (attach-files . #x0000000000008000)
    (read-message-history . #x0000000000010000)
    (mention-everyone . #x0000000000020000)
    (use-external-emojis . #x0000000000040000)
    (use-external-emoji . #x0000000000040000)
    (view-guild-insights . #x0000000000080000)
    (connect . #x0000000000100000)
    (speak . #x0000000000200000)
    (mute-members . #x0000000000400000)
    (deafen-members . #x0000000000800000)
    (move-members . #x0000000001000000)
    (use-vad . #x0000000002000000)
    (change-nickname . #x0000000004000000)
    (manage-nicknames . #x0000000008000000)
    (manage-roles . #x0000000010000000)
    (manage-webhooks . #x0000000020000000)
    (manage-guild-expressions . #x0000000040000000)
    ;; Backward-compat aliases used by older docs/clients.
    (manage-emojis-and-stickers . #x0000000040000000)
    (manage-expressions . #x0000000040000000)
    (use-application-commands . #x0000000080000000)
    (use-slash-commands . #x0000000080000000)
    (request-to-speak . #x0000000100000000)
    (manage-events . #x0000000200000000)
    (manage-threads . #x0000000400000000)
    (create-public-threads . #x0000000800000000)
    (create-private-threads . #x0000001000000000)
    (use-external-stickers . #x0000002000000000)
    (use-external-sticker . #x0000002000000000)
    (send-messages-in-threads . #x0000004000000000)
    (use-embedded-activities . #x0000008000000000)
    (moderate-members . #x0000010000000000)
    (view-creator-monetization-analytics . #x0000020000000000)
    (use-soundboard . #x0000040000000000)
    (create-guild-expressions . #x0000080000000000)
    ;; Backward-compat alias used by older unofficial docs.
    (create-expressions . #x0000080000000000)
    (create-events . #x0000100000000000)
    (use-external-sounds . #x0000200000000000)
    (send-voice-messages . #x0000400000000000)
    ;; Deprecated in public docs, retained here for compatibility.
    (use-clyde-ai . #x0000800000000000)
    (set-voice-channel-status . #x0001000000000000)
    (send-polls . #x0002000000000000)
    (use-external-apps . #x0004000000000000)
    (pin-messages . #x0008000000000000)
    (bypass-slowmode . #x0010000000000000))
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

(cl-defun disco-permission-channel-viewable-p (channel &optional (unknown-value t))
  "Return non-nil when CHANNEL should be treated as visible.

For guild channels this checks `view-channel'. DM/group-DM and channels
without guild context are treated as visible. UNKNOWN-VALUE is used when
computed permissions are missing or unparsable."
  (let ((channel-type (and (listp channel) (alist-get 'type channel)))
        (guild-id (and (listp channel) (alist-get 'guild_id channel))))
    (cond
     ((memq channel-type '(1 3))
      t)
     ((null guild-id)
      t)
     (t
      (disco-permission-channel-has-p channel 'view-channel unknown-value)))))

(provide 'disco-permission)

;;; disco-permission.el ends here
