;;; disco-markdown.el --- Markdown rendering helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared Markdown rendering entrypoint used by room/embed modules.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'disco-util)

(defcustom disco-markdown-backend 'auto
  "Backend used for Markdown rendering in disco.

`auto' prefers markdown-mode when available, then falls back to legacy
punctuation unescaping.
`legacy' always uses `disco-util-unescape-markdown-punctuation'.
`markdown-mode' enforces markdown-mode backend when available; otherwise it
falls back to `legacy'."
  :type '(choice
          (const :tag "Auto" auto)
          (const :tag "Legacy punctuation unescape" legacy)
          (const :tag "markdown-mode" markdown-mode))
  :group 'disco)

(defcustom disco-markdown-enable-discord-tokens t
  "When non-nil, render Discord-specific tokens after Markdown pass.

This includes mentions, channel references, role mentions, command mentions,
custom emoji markers, and Discord timestamps."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-cache-enabled t
  "When non-nil, cache Markdown render results by backend/context/text."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-cache-limit 2048
  "Maximum number of entries in Markdown render cache.

When exceeded, cache is cleared to keep runtime behavior simple and stable."
  :type 'integer
  :group 'disco)

(defface disco-markdown-mention-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for rendered Discord mentions."
  :group 'disco)

(defface disco-markdown-command-face
  '((t :inherit font-lock-keyword-face))
  "Face used for rendered Discord slash-command mentions."
  :group 'disco)

(defface disco-markdown-timestamp-face
  '((t :inherit font-lock-constant-face))
  "Face used for rendered Discord timestamps."
  :group 'disco)

(defface disco-markdown-emoji-face
  '((t :inherit font-lock-builtin-face))
  "Face used for rendered custom emoji markers."
  :group 'disco)

(defconst disco-markdown--regexp-user-mention "<@!?\\([0-9]+\\)>"
  "Regexp matching user mention tokens.")

(defconst disco-markdown--regexp-role-mention "<@&\\([0-9]+\\)>"
  "Regexp matching role mention tokens.")

(defconst disco-markdown--regexp-channel-mention "<#\\([0-9]+\\)>"
  "Regexp matching channel mention tokens.")

(defconst disco-markdown--regexp-command-mention "</\\([^:>]+\\):\\([0-9]+\\)>"
  "Regexp matching slash command mention tokens.")

(defconst disco-markdown--regexp-custom-emoji "<a?:\\([^:>]+\\):\\([0-9]+\\)>"
  "Regexp matching custom emoji mention tokens.")

(defconst disco-markdown--regexp-timestamp "<t:\\([0-9]+\\)\\(?::\\([tTdDfFRsS]\\)\\)?>"
  "Regexp matching Discord timestamp tokens.")

(defvar disco-markdown--cache (make-hash-table :test #'equal)
  "Cache table for rendered Markdown strings.")

(defun disco-markdown-clear-cache ()
  "Clear Markdown render cache."
  (interactive)
  (clrhash disco-markdown--cache)
  (message "disco: markdown render cache cleared"))

(defun disco-markdown--string-present-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (not (string-empty-p value))))

(defun disco-markdown--normalize-id (value)
  "Return VALUE as normalized snowflake string, or nil."
  (cond
   ((null value) nil)
   ((stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
        trimmed)))
   ((integerp value)
    (number-to-string value))
   ((numberp value)
    (format "%.0f" value))
   (t
    (let ((str (string-trim (format "%s" value))))
      (unless (string-empty-p str)
        str)))))

(defun disco-markdown--cache-key (backend context text &optional message-context-key)
  "Build cache key from BACKEND, CONTEXT, TEXT and MESSAGE-CONTEXT-KEY."
  (list backend context message-context-key text))

(defun disco-markdown--cache-get (key)
  "Return cached render value for KEY.

A copied string is returned so callers cannot mutate cache entries."
  (when key
    (let ((value (gethash key disco-markdown--cache)))
      (and (stringp value)
           (copy-sequence value)))))

(defun disco-markdown--cache-put (key value)
  "Store VALUE in cache for KEY.

VALUE is copied to isolate cache entries from caller mutation."
  (when (and key (stringp value))
    (puthash key (copy-sequence value) disco-markdown--cache)
    (when (> (hash-table-count disco-markdown--cache)
             (max 1 disco-markdown-cache-limit))
      (clrhash disco-markdown--cache))))

(defun disco-markdown--markdown-mode-available-p ()
  "Return non-nil when markdown-mode view backend is available."
  (and (require 'markdown-mode nil t)
       (or (fboundp 'gfm-view-mode)
           (fboundp 'markdown-view-mode))))

(defun disco-markdown--resolve-backend ()
  "Return resolved backend symbol for current settings."
  (pcase disco-markdown-backend
    ('legacy 'legacy)
    ('markdown-mode
     (if (disco-markdown--markdown-mode-available-p)
         'markdown-mode
       'legacy))
    (_
     (if (disco-markdown--markdown-mode-available-p)
         'markdown-mode
       'legacy))))

(defun disco-markdown--next-face-change (text pos len)
  "Return next position where face-related properties change.

TEXT is source string, POS is current index and LEN is text length."
  (let ((next-face (or (next-single-char-property-change pos 'face text) len))
        (next-font-lock
         (or (next-single-char-property-change pos 'font-lock-face text) len)))
    (min next-face next-font-lock)))

(defun disco-markdown--sanitize-face-properties (text)
  "Return TEXT stripped to `face' properties only.

This drops markdown-mode specific `keymap', `help-echo', and other interaction
properties that should not leak into room buffers."
  (let* ((source (or text ""))
         (copy (substring-no-properties source))
         (len (length source))
         (pos 0))
    (while (< pos len)
      (let* ((face (or (get-text-property pos 'face source)
                       (get-text-property pos 'font-lock-face source)))
             (next (disco-markdown--next-face-change source pos len)))
        (when face
          (add-text-properties pos next (list 'face face) copy))
        (setq pos next)))
    copy))

(defun disco-markdown--render-with-markdown-mode (text)
  "Render TEXT through markdown-mode and return visible propertized string."
  (with-temp-buffer
    (insert (or text ""))
    ;; Use markdown view mode so hidden-markup copy path strips formatting tokens.
    (funcall (if (fboundp 'gfm-view-mode)
                 'gfm-view-mode
               'markdown-view-mode))
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure (point-min) (point-max)))
    (disco-markdown--sanitize-face-properties
     (filter-buffer-substring (point-min) (point-max) nil))))

(defun disco-markdown--render-legacy (text)
  "Render TEXT with legacy markdown punctuation unescape behavior."
  (if (stringp text)
      (disco-util-unescape-markdown-punctuation text)
    ""))

(defun disco-markdown--sequence-list (value)
  "Return VALUE converted to a list sequence."
  (cond
   ((null value) nil)
   ((listp value) value)
   ((vectorp value) (append value nil))
   (t nil)))

(defun disco-markdown--message-context-key (message)
  "Return stable cache context key for MESSAGE, or nil."
  (when (listp message)
    (let* ((id (disco-markdown--normalize-id (disco-util-object-get message 'id)))
           (mentions (disco-util-object-get message 'mentions))
           (mention-roles (disco-util-object-get message 'mention_roles 'mentionRoles))
           (mention-channels (disco-util-object-get message 'mention_channels 'mentionChannels))
           (resolved (disco-util-object-get message 'resolved))
           (snapshot-key (list id mentions mention-roles mention-channels resolved)))
      (md5 (prin1-to-string snapshot-key)))))

(defun disco-markdown--user-display-name (user)
  "Return best display name for USER object."
  (let* ((global-name (disco-util-object-get user 'global_name 'globalName))
         (username (disco-util-object-get user 'username 'name))
         (user-id (disco-markdown--normalize-id (disco-util-object-get user 'id))))
    (or (and (disco-markdown--string-present-p global-name) global-name)
        (and (disco-markdown--string-present-p username) username)
        (and user-id (format "user:%s" user-id))
        "user")))

(defun disco-markdown--entry-object (entry)
  "Return object payload represented by ENTRY.

ENTRY may be either an object alist or a map pair of (ID . OBJECT)."
  (if (and (consp entry)
           (atom (car entry))
           (listp (cdr entry)))
      (cdr entry)
    entry))

(defun disco-markdown--hash-put-id-name (table id name)
  "Store ID->NAME in TABLE when both are present strings."
  (when (and (hash-table-p table)
             (disco-markdown--string-present-p id)
             (disco-markdown--string-present-p name))
    (puthash id name table)))

(defun disco-markdown--build-user-name-map (message)
  "Return hash map of mention user-id -> display name from MESSAGE."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (user (disco-markdown--sequence-list
                   (disco-util-object-get message 'mentions)))
      (let ((id (disco-markdown--normalize-id (disco-util-object-get user 'id))))
        (disco-markdown--hash-put-id-name
         table
         id
         (disco-markdown--user-display-name user))))
    table))

(defun disco-markdown--state-channel-name (channel-id)
  "Try to resolve CHANNEL-ID name from in-memory state and return nil on miss."
  (when (and (disco-markdown--string-present-p channel-id)
             (fboundp 'disco-state-channel))
    (let ((channel (ignore-errors (funcall 'disco-state-channel channel-id))))
      (when (listp channel)
        (let ((name (disco-util-object-get channel 'name)))
          (if (disco-markdown--string-present-p name)
              name
            (format "channel:%s" channel-id)))))))

(defun disco-markdown--build-channel-name-map (message)
  "Return hash map of mentioned channel-id -> display name from MESSAGE."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (channel (disco-markdown--sequence-list
                      (disco-util-object-get message 'mention_channels
                                             'mentionChannels)))
      (let ((id (disco-markdown--normalize-id (disco-util-object-get channel 'id)))
            (name (disco-util-object-get channel 'name)))
        (disco-markdown--hash-put-id-name table id name)))
    (let* ((resolved (disco-util-object-get message 'resolved))
           (resolved-channels (and (listp resolved)
                                   (disco-util-object-get resolved 'channels))))
      (dolist (entry (disco-markdown--sequence-list resolved-channels))
        (let* ((object (disco-markdown--entry-object entry))
               (id (or (disco-markdown--normalize-id (disco-util-object-get object 'id))
                       (disco-markdown--normalize-id (car-safe entry))))
               (name (disco-util-object-get object 'name)))
          (disco-markdown--hash-put-id-name table id name))))
    table))

(defun disco-markdown--build-role-name-map (message)
  "Return hash map of role-id -> role name from MESSAGE."
  (let ((table (make-hash-table :test #'equal))
        (resolved (disco-util-object-get message 'resolved)))
    (when (listp resolved)
      (let ((resolved-roles (disco-util-object-get resolved 'roles)))
        (dolist (entry (disco-markdown--sequence-list resolved-roles))
          (let* ((object (disco-markdown--entry-object entry))
                 (id (or (disco-markdown--normalize-id (disco-util-object-get object 'id))
                         (disco-markdown--normalize-id (car-safe entry))))
                 (name (disco-util-object-get object 'name)))
            (disco-markdown--hash-put-id-name table id name)))))
    table))

(defun disco-markdown--relative-time-label (epoch-seconds)
  "Return human-readable relative label for EPOCH-SECONDS."
  (let* ((now (float-time (current-time)))
         (delta (- now epoch-seconds))
         (future (< delta 0))
         (seconds (abs delta))
         (unit (cond
                ((< seconds 60) "second")
                ((< seconds 3600) "minute")
                ((< seconds 86400) "hour")
                ((< seconds 2592000) "day")
                ((< seconds 31536000) "month")
                (t "year")))
         (value (cond
                 ((< seconds 60) (round seconds))
                 ((< seconds 3600) (floor (/ seconds 60)))
                 ((< seconds 86400) (floor (/ seconds 3600)))
                 ((< seconds 2592000) (floor (/ seconds 86400)))
                 ((< seconds 31536000) (floor (/ seconds 2592000)))
                 (t (floor (/ seconds 31536000)))))
         (plural (if (= value 1) "" "s")))
    (if future
        (format "in %d %s%s" value unit plural)
      (format "%d %s%s ago" value unit plural))))

(defun disco-markdown--format-discord-timestamp (raw-seconds style)
  "Return display label for RAW-SECONDS with Discord timestamp STYLE."
  (if (not (and (stringp raw-seconds)
                (string-match-p "\\`[0-9]+\\'" raw-seconds)))
      (format "<t:%s%s>"
              (or raw-seconds "")
              (if (disco-markdown--string-present-p style)
                  (format ":%s" style)
                ""))
    (let* ((seconds (string-to-number raw-seconds))
           (time (seconds-to-time seconds))
           (style-char (if (disco-markdown--string-present-p style)
                           (aref style 0)
                         ?f)))
      (pcase style-char
        (?t (format-time-string "%H:%M" time))
        (?T (format-time-string "%H:%M:%S" time))
        (?d (format-time-string "%Y-%m-%d" time))
        (?D (format-time-string "%B %d, %Y" time))
        (?f (format-time-string "%B %d, %Y %H:%M" time))
        (?F (format-time-string "%A, %B %d, %Y %H:%M" time))
        (?s (format-time-string "%Y-%m-%d %H:%M" time))
        (?S (format-time-string "%Y-%m-%d %H:%M:%S" time))
        (?R (disco-markdown--relative-time-label seconds))
        (_ (format-time-string "%B %d, %Y %H:%M" time))))))

(defun disco-markdown--code-face-p (face)
  "Return non-nil when FACE denotes markdown code/preformatted text."
  (when face
    (or (eq face 'markdown-inline-code-face)
        (eq face 'markdown-code-face)
        (eq face 'markdown-pre-face)
        (and (symbolp face)
             (string-match-p "\\`markdown-.*\\(code\\|pre\\)" (symbol-name face))))))

(defun disco-markdown--position-in-code-face-p (position)
  "Return non-nil when POSITION in current buffer belongs to a code face."
  (let ((face (get-text-property position 'face)))
    (if (listp face)
        (seq-some #'disco-markdown--code-face-p face)
      (disco-markdown--code-face-p face))))

(defun disco-markdown--apply-regexp-replacements (text backend replacements)
  "Apply REPLACEMENTS over TEXT and return transformed string.

BACKEND controls whether code-face regions are protected from replacements."
  (with-temp-buffer
    (insert text)
    (dolist (entry replacements)
      (goto-char (point-min))
      (let ((regexp (car entry))
            (replacer (cdr entry)))
        (while (re-search-forward regexp nil t)
          (let ((beg (match-beginning 0)))
            (unless (and (eq backend 'markdown-mode)
                         (disco-markdown--position-in-code-face-p beg))
              (let ((replacement (funcall replacer)))
                (when (stringp replacement)
                  (replace-match replacement t t))))))))
    (buffer-string)))

(defun disco-markdown--render-discord-tokens (text backend message)
  "Render Discord-specific token syntax in TEXT.

BACKEND controls code-region behavior. MESSAGE carries mention/channel context."
  (if (not (and (eq backend 'markdown-mode)
                disco-markdown-enable-discord-tokens
                (disco-markdown--string-present-p text)))
      text
    (let* ((user-map (disco-markdown--build-user-name-map message))
           (channel-map (disco-markdown--build-channel-name-map message))
           (role-map (disco-markdown--build-role-name-map message))
           (replacements
            (list
             (cons disco-markdown--regexp-user-mention
                   (lambda ()
                     (let* ((id (match-string-no-properties 1))
                            (name (or (gethash id user-map)
                                      (format "user:%s" id))))
                       (propertize (concat "@" name)
                                   'face 'disco-markdown-mention-face))))
             (cons disco-markdown--regexp-role-mention
                   (lambda ()
                     (let* ((id (match-string-no-properties 1))
                            (name (or (gethash id role-map)
                                      (format "role:%s" id))))
                       (propertize (concat "@" name)
                                   'face 'disco-markdown-mention-face))))
             (cons disco-markdown--regexp-channel-mention
                   (lambda ()
                     (let* ((id (match-string-no-properties 1))
                            (name (or (gethash id channel-map)
                                      (disco-markdown--state-channel-name id)
                                      (format "channel:%s" id))))
                       (propertize (concat "#" name)
                                   'face 'disco-markdown-mention-face))))
             (cons disco-markdown--regexp-command-mention
                   (lambda ()
                     (let* ((name (string-trim (match-string-no-properties 1)))
                            (command (if (string-prefix-p "/" name)
                                         name
                                       (concat "/" name))))
                       (propertize command 'face 'disco-markdown-command-face))))
             (cons disco-markdown--regexp-custom-emoji
                   (lambda ()
                     (let ((name (match-string-no-properties 1)))
                       (propertize (format ":%s:" name)
                                   'face 'disco-markdown-emoji-face))))
             (cons disco-markdown--regexp-timestamp
                   (lambda ()
                     (let ((seconds (match-string-no-properties 1))
                           (style (match-string-no-properties 2)))
                       (propertize
                        (disco-markdown--format-discord-timestamp seconds style)
                        'face 'disco-markdown-timestamp-face)))))))
      (disco-markdown--apply-regexp-replacements text backend replacements))))

(cl-defun disco-markdown-render (text &key context message)
  "Render Markdown TEXT for display.

CONTEXT is an optional symbol used for cache key partitioning.
MESSAGE is optional message data used to resolve mention-like tokens."
  (let* ((source (if (stringp text) text ""))
         (backend (disco-markdown--resolve-backend))
         (message-context-key (disco-markdown--message-context-key message))
         (cache-key (and disco-markdown-cache-enabled
                         (disco-markdown--cache-key backend context
                                                    source message-context-key)))
         (cached (disco-markdown--cache-get cache-key)))
    (or cached
        (let* ((markdown-rendered
                (condition-case _
                    (pcase backend
                      ('markdown-mode
                       (disco-markdown--render-with-markdown-mode source))
                      (_
                       (disco-markdown--render-legacy source)))
                  (error
                   (disco-markdown--render-legacy source))))
               (rendered (disco-markdown--render-discord-tokens
                          markdown-rendered backend message)))
          (disco-markdown--cache-put cache-key rendered)
          rendered))))

(provide 'disco-markdown)

;;; disco-markdown.el ends here
