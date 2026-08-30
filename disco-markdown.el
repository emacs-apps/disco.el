;;; disco-markdown.el --- Discord Markdown adapter for Appkit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 disco.el contributors

;;; Commentary:

;; Discord message content is provider-owned Markdown source.  This module
;; protects Discord-only delimiters, delegates CommonMark structure to Appkit's
;; bounded Tree-sitter codec, adapts provider tokens into semantic objects, and
;; renders the resulting immutable Document through Appkit's native renderer.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chatbuf)
(require 'appkit-markup)
(require 'appkit-markup-codec)
(require 'appkit-markup-codecs)
(require 'appkit-markup-ui)
(require 'appkit-ui)
(require 'disco-emoji-image)

(defcustom disco-markdown-enable-discord-tokens t
  "When non-nil, adapt Discord-specific tokens into semantic objects."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-enable-spoiler-render t
  "When non-nil, adapt Discord `||spoiler||' spans into semantic objects."
  :type 'boolean
  :group 'disco)

(defface disco-markdown-mention-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for Discord mention objects."
  :group 'disco)

(defface disco-markdown-command-face
  '((t :inherit font-lock-keyword-face))
  "Face used for Discord command objects."
  :group 'disco)

(defface disco-markdown-timestamp-face
  '((t :inherit font-lock-constant-face))
  "Face used for Discord timestamp objects."
  :group 'disco)

(defface disco-markdown-emoji-face
  '((t :inherit font-lock-builtin-face))
  "Face used for Discord custom emoji objects."
  :group 'disco)

(defface disco-markdown-navigation-face
  '((t :inherit font-lock-function-name-face))
  "Face used for Discord guild navigation objects."
  :group 'disco)

(defface disco-markdown-spoiler-face
  '((t :inherit default))
  "Face used for Discord spoiler contents."
  :group 'disco)

(defface disco-markdown-subtitle-face
  '((t :inherit shadow :height 0.9))
  "Face used for Discord `-#' subtitle lines."
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
  "Regexp matching custom emoji tokens.")
(defconst disco-markdown--regexp-timestamp
  "<t:\\([0-9]+\\)\\(?::\\([tTdDfFRsS]\\)\\)?>"
  "Regexp matching Discord timestamp tokens.")
(defconst disco-markdown--regexp-guild-navigation "<id:\\([^>]+\\)>"
  "Regexp matching Discord guild navigation tokens.")
(defconst disco-markdown--regexp-guild-navigation-bare
  "\\_<id:[[:alnum:]:_-]+\\_>"
  "Regexp matching bare Discord guild navigation tokens.")
(defconst disco-markdown--regexp-everyone-mention "@\\(?:everyone\\|here\\)"
  "Regexp matching @everyone and @here tokens.")

(defconst disco-markdown--regexp-provider-token
  (concat "\\(?:" disco-markdown--regexp-user-mention
          "\\|" disco-markdown--regexp-role-mention
          "\\|" disco-markdown--regexp-channel-mention
          "\\|" disco-markdown--regexp-command-mention
          "\\|" disco-markdown--regexp-custom-emoji
          "\\|" disco-markdown--regexp-timestamp
          "\\|" disco-markdown--regexp-guild-navigation
          "\\|" disco-markdown--regexp-guild-navigation-bare
          "\\|" disco-markdown--regexp-everyone-mention "\\)")
  "Regexp matching one Discord provider token.")

(defconst disco-markdown--spoiler-translation-table
  (let ((table (make-char-table 'translation-table ?█)))
    (set-char-table-range table ?\n ?\n)
    table)
  "Translation table used to mask hidden spoiler contents.")

(cl-defstruct (disco-markdown-object
               (:constructor disco-markdown-object--create)
               (:copier nil))
  (kind nil :read-only t)
  (raw nil :read-only t)
  (id nil :read-only t)
  (name nil :read-only t)
  (style nil :read-only t)
  (animated nil :read-only t))

(cl-defstruct (disco-markdown--marker
               (:constructor disco-markdown--marker-create)
               (:copier nil))
  (kind nil :read-only t))

(defvar disco-markdown--sentinels nil)
(defvar disco-markdown--user-map nil)
(defvar disco-markdown--channel-map nil)
(defvar disco-markdown--role-map nil)


(defun disco-markdown--string-present-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun disco-markdown--normalize-id (value)
  "Return VALUE as a normalized Discord identifier string, or nil."
  (cond
   ((null value) nil)
   ((stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed) trimmed)))
   ((integerp value) (number-to-string value))
   ((numberp value) (format "%.0f" value))
   (t
    (let ((text (string-trim (format "%s" value))))
      (unless (string-empty-p text) text)))))

(defun disco-markdown--sequence-list (value)
  "Return VALUE converted to a list sequence."
  (cond ((null value) nil)
        ((listp value) value)
        ((vectorp value) (append value nil))
        (t nil)))

(defun disco-markdown--entry-object (entry)
  "Return the object payload represented by ENTRY."
  (if (and (consp entry) (atom (car entry)) (listp (cdr entry)))
      (cdr entry)
    entry))

(defun disco-markdown--hash-put-id-name (table id name)
  "Store ID mapped to NAME in TABLE when both are non-empty strings."
  (when (and (disco-markdown--string-present-p id)
             (disco-markdown--string-present-p name))
    (puthash id name table)))

(defun disco-markdown--user-display-name (user)
  "Return the best display name for USER."
  (let ((global-name (alist-get 'global_name user))
        (username (alist-get 'username user))
        (id (disco-markdown--normalize-id (alist-get 'id user))))
    (or (and (disco-markdown--string-present-p global-name) global-name)
        (and (disco-markdown--string-present-p username) username)
        (and id (format "user:%s" id))
        "user")))

(defun disco-markdown--build-user-name-map (message)
  "Return a user identifier to display-name map from MESSAGE."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (user (disco-markdown--sequence-list (alist-get 'mentions message)))
      (disco-markdown--hash-put-id-name
       table
       (disco-markdown--normalize-id (alist-get 'id user))
       (disco-markdown--user-display-name user)))
    table))

(defun disco-markdown--state-channel-name (channel-id)
  "Resolve CHANNEL-ID from current in-memory state, or return nil."
  (when (and (disco-markdown--string-present-p channel-id)
             (fboundp 'disco-state-channel))
    (let ((channel (ignore-errors (funcall 'disco-state-channel channel-id))))
      (when (listp channel)
        (let ((name (alist-get 'name channel)))
          (if (disco-markdown--string-present-p name)
              name
            (format "channel:%s" channel-id)))))))

(defun disco-markdown--build-channel-name-map (message)
  "Return a channel identifier to display-name map from MESSAGE."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (channel
             (disco-markdown--sequence-list (alist-get 'mention_channels message)))
      (disco-markdown--hash-put-id-name
       table
       (disco-markdown--normalize-id (alist-get 'id channel))
       (alist-get 'name channel)))
    (let* ((resolved (alist-get 'resolved message))
           (channels (and (listp resolved) (alist-get 'channels resolved))))
      (dolist (entry (disco-markdown--sequence-list channels))
        (let ((object (disco-markdown--entry-object entry)))
          (disco-markdown--hash-put-id-name
           table
           (or (disco-markdown--normalize-id (alist-get 'id object))
               (disco-markdown--normalize-id (car-safe entry)))
           (alist-get 'name object)))))
    table))

(defun disco-markdown--build-role-name-map (message)
  "Return a role identifier to display-name map from MESSAGE."
  (let ((table (make-hash-table :test #'equal))
        (resolved (alist-get 'resolved message)))
    (when (listp resolved)
      (dolist (entry
               (disco-markdown--sequence-list (alist-get 'roles resolved)))
        (let ((object (disco-markdown--entry-object entry)))
          (disco-markdown--hash-put-id-name
           table
           (or (disco-markdown--normalize-id (alist-get 'id object))
               (disco-markdown--normalize-id (car-safe entry)))
           (alist-get 'name object)))))
    table))

(defun disco-markdown--relative-time-label (epoch-seconds)
  "Return a human-readable relative label for EPOCH-SECONDS."
  (let* ((delta (- (float-time (current-time)) epoch-seconds))
         (future (< delta 0))
         (seconds (abs delta))
         (unit (cond ((< seconds 60) "second")
                     ((< seconds 3600) "minute")
                     ((< seconds 86400) "hour")
                     ((< seconds 2592000) "day")
                     ((< seconds 31536000) "month")
                     (t "year")))
         (value (cond ((< seconds 60) (round seconds))
                      ((< seconds 3600) (floor (/ seconds 60)))
                      ((< seconds 86400) (floor (/ seconds 3600)))
                      ((< seconds 2592000) (floor (/ seconds 86400)))
                      ((< seconds 31536000) (floor (/ seconds 2592000)))
                      (t (floor (/ seconds 31536000)))))
         (plural (if (= value 1) "" "s")))
    (if future
        (format "in %d %s%s" value unit plural)
      (format "%d %s%s ago" value unit plural))))

(defun disco-markdown--format-timestamp (seconds style)
  "Return the display label for Discord timestamp SECONDS and STYLE."
  (if (not (and (stringp seconds) (string-match-p "\\`[0-9]+\\'" seconds)))
      (or seconds "")
    (let* ((epoch (string-to-number seconds))
           (time (seconds-to-time epoch))
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
        (?R (disco-markdown--relative-time-label epoch))
        (_ (format-time-string "%B %d, %Y %H:%M" time))))))

(defun disco-markdown-custom-emoji-identities (text)
  "Return ordered custom emoji identities parsed from Discord TEXT."
  (let ((position 0) result)
    (when (stringp text)
      (while (string-match disco-markdown--regexp-custom-emoji text position)
        (let ((token (match-string-no-properties 0 text)))
          (push (list :name (match-string-no-properties 1 text)
                      :id (match-string-no-properties 2 text)
                      :animated (and (string-prefix-p "<a:" token) t))
                result))
        (setq position (match-end 0))))
    (nreverse result)))

(defun disco-markdown--unused-characters (source count)
  "Return COUNT private-use characters absent from SOURCE."
  (let ((code #xe000) result)
    (while (< (length result) count)
      (when (> code #xf8ff)
        (signal 'appkit-markup-codec-error '(discord-sentinel-exhausted)))
      (let ((character (string code)))
        (unless (string-match-p (regexp-quote character) source)
          (push character result)))
      (setq code (1+ code)))
    (nreverse result)))

(defun disco-markdown--make-sentinels (source)
  "Return a private sentinel plist for SOURCE."
  (let ((characters (disco-markdown--unused-characters source 11)))
    (list :escape-less (pop characters)
          :escape-at (pop characters)
          :escape-pipe (pop characters)
          :escape-underscore (pop characters)
          :escape-dash (pop characters)
          :escape-hash (pop characters)
          :underline-open (pop characters)
          :underline-close (pop characters)
          :spoiler-open (pop characters)
          :spoiler-close (pop characters)
          :subtitle (pop characters)
          :provider-table (make-hash-table :test #'eql)
          :provider-next #xe100)))

(defun disco-markdown--escaped-p (text position)
  "Return non-nil when TEXT character at POSITION is backslash escaped."
  (let ((cursor (1- position)) (count 0))
    (while (and (>= cursor 0) (= (aref text cursor) ?\\))
      (setq count (1+ count)
            cursor (1- cursor)))
    (= (% count 2) 1)))

(defun disco-markdown--protect-escapes (source sentinels)
  "Replace provider-significant escapes in SOURCE using SENTINELS."
  (let ((result source)
        (mapping `((?< . ,(plist-get sentinels :escape-less))
                   (?@ . ,(plist-get sentinels :escape-at))
                   (?| . ,(plist-get sentinels :escape-pipe))
                   (?_ . ,(plist-get sentinels :escape-underscore))
                   (?- . ,(plist-get sentinels :escape-dash))
                   (?# . ,(plist-get sentinels :escape-hash))))
        replacements)
    (cl-loop for position from 0 below (length source)
             for character = (aref source position)
             for sentinel = (cdr (assq character mapping))
             when (and sentinel (disco-markdown--escaped-p source position))
             do (push (list (1- position) (1+ position) sentinel) replacements))
    (dolist (replacement replacements result)
      (setq result
            (concat (substring result 0 (nth 0 replacement))
                    (nth 2 replacement)
                    (substring result (nth 1 replacement)))))))

(defun disco-markdown--word-character-p (character)
  "Return non-nil when CHARACTER is a word constituent."
  (and character (memq (char-syntax character) '(?w ?_))))

(defun disco-markdown--replace-delimiter-pairs
    (source delimiter open-sentinel close-sentinel &optional underline-p)
  "Replace paired DELIMITER in SOURCE with OPEN-SENTINEL and CLOSE-SENTINEL.

Pairs never cross a line.  UNDERLINE-P applies Discord's word-boundary and
non-whitespace rules for `__underline__'."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (let ((width (length delimiter)))
      (while (search-forward delimiter nil t)
        (let* ((open-start (- (point) width))
               (open-end (point))
               (limit (line-end-position))
               close-start)
          (save-excursion
            (while (and (not close-start) (search-forward delimiter limit t))
              (let* ((candidate-start (- (point) width))
                     (first (char-after open-end))
                     (last (char-before candidate-start))
                     (outside-left (char-before open-start))
                     (outside-right (char-after (point))))
                (when (and (< open-end candidate-start)
                           (or (not underline-p)
                               (and (not (memq first '(?\s ?\t ?\n)))
                                    (not (memq last '(?\s ?\t ?\n)))
                                    (not (disco-markdown--word-character-p outside-left))
                                    (not (disco-markdown--word-character-p outside-right)))))
                  (setq close-start candidate-start)))))
          (if (not close-start)
              (goto-char open-end)
            (goto-char close-start)
            (delete-region close-start (+ close-start width))
            (insert close-sentinel)
            (delete-region open-start open-end)
            (goto-char open-start)
            (insert open-sentinel)))))
    (buffer-string)))

(defun disco-markdown--provider-sentinel (source sentinels)
  "Return one unused provider sentinel character for SOURCE and SENTINELS."
  (let ((code (plist-get sentinels :provider-next))
        (table (plist-get sentinels :provider-table))
        character)
    (while (progn
             (when (> code #xf8ff)
               (signal 'appkit-markup-codec-error
                       '(discord-sentinel-exhausted)))
             (setq character (string code))
             (or (gethash code table)
                 (string-match-p (regexp-quote character) source)
                 (cl-loop for (key value) on sentinels by #'cddr
                          thereis
                          (and (not (memq key
                                          '(:provider-table :provider-next)))
                               (stringp value)
                               (equal value character)))))
      (setq code (1+ code)))
    (setf (plist-get sentinels :provider-next) (1+ code))
    character))

(defun disco-markdown--protect-provider-tokens (source sentinels)
  "Protect provider tokens in SOURCE as occurrence-specific SENTINELS."
  (let ((position 0)
        (count 0)
        replacements)
    (while (string-match disco-markdown--regexp-provider-token source position)
      (when (>= count appkit-markup-codec-object-limit)
        (signal 'appkit-markup-codec-error '(too-many-objects)))
      (let* ((begin (match-beginning 0))
             (end (match-end 0))
             (raw (match-string-no-properties 0 source))
             (sentinel (disco-markdown--provider-sentinel source sentinels)))
        (puthash (aref sentinel 0) raw
                 (plist-get sentinels :provider-table))
        (push (list begin end sentinel) replacements)
        (setq position end
              count (1+ count))))
    (dolist (replacement replacements source)
      (setq source
            (concat (substring source 0 (nth 0 replacement))
                    (nth 2 replacement)
                    (substring source (nth 1 replacement)))))))

(defun disco-markdown--protect-extensions (source sentinels)
  "Protect Discord extension delimiters in SOURCE using SENTINELS."
  (let ((protected (disco-markdown--protect-escapes source sentinels)))
    (setq protected
          (disco-markdown--replace-delimiter-pairs
           protected "__"
           (plist-get sentinels :underline-open)
           (plist-get sentinels :underline-close) t))
    (when disco-markdown-enable-spoiler-render
      (setq protected
            (disco-markdown--replace-delimiter-pairs
             protected "||"
             (plist-get sentinels :spoiler-open)
             (plist-get sentinels :spoiler-close))))
    (with-temp-buffer
      (insert protected)
      (goto-char (point-min))
      (while (re-search-forward "^\\([ \t]*\\)-#\\(?:[ \t]+\\|$\\)" nil t)
        (replace-match
         (concat (match-string-no-properties 1)
                 (plist-get sentinels :subtitle))
         t t))
      (disco-markdown--protect-provider-tokens
       (buffer-string) sentinels))))

(defun disco-markdown--sentinel-kind (character)
  "Return delimiter marker kind for CHARACTER, or nil."
  (cond
   ((= character (aref (plist-get disco-markdown--sentinels :underline-open) 0))
    'underline-open)
   ((= character (aref (plist-get disco-markdown--sentinels :underline-close) 0))
    'underline-close)
   ((= character (aref (plist-get disco-markdown--sentinels :spoiler-open) 0))
    'spoiler-open)
   ((= character (aref (plist-get disco-markdown--sentinels :spoiler-close) 0))
    'spoiler-close)))

(defun disco-markdown--split-delimiter-events (node)
  "Split text NODE into ordinary text and transient delimiter events."
  (if (not (appkit-markup-text-p node))
      (list node)
    (let* ((text (appkit-markup-text-text node))
           (styles (appkit-markup-text-styles node))
           (start 0) result)
      (cl-loop for position from 0 below (length text)
               for kind = (disco-markdown--sentinel-kind (aref text position))
               when kind do
               (when (< start position)
                 (push (appkit-markup-text (substring text start position) styles)
                       result))
               (push (disco-markdown--marker-create :kind kind) result)
               (setq start (1+ position)))
      (when (< start (length text))
        (push (appkit-markup-text (substring text start) styles) result))
      (nreverse result))))

(defun disco-markdown--marker-literal (kind)
  "Return the source delimiter represented by marker KIND."
  (pcase kind
    ((or 'underline-open 'underline-close) "__")
    ((or 'spoiler-open 'spoiler-close) "||")
    (_ "")))

(defun disco-markdown--add-style-inline (node style)
  "Return inline NODE with STYLE applied semantically."
  (cond
   ((appkit-markup-text-p node)
    (appkit-markup-text
     (appkit-markup-text-text node)
     (cons style (appkit-markup-text-styles node))))
   ((appkit-markup-link-p node)
    (appkit-markup-link
     (appkit-markup-link-url node)
     (mapcar (lambda (child) (disco-markdown--add-style-inline child style))
             (appkit-markup-link-children node))))
   ((appkit-markup-object-p node)
    (appkit-markup-object
     (appkit-markup-object-value node)
     (appkit-markup-object-fallback node)
     (cons style (appkit-markup-object-styles node))))
   (t node)))

(defun disco-markdown--parse-delimiter-events (events &optional closing-kind)
  "Parse transient delimiter EVENTS until CLOSING-KIND.

Return (NODES REST CLOSED-P)."
  (let (result closed)
    (while (and events (not closed))
      (let ((event (pop events)))
        (if (not (disco-markdown--marker-p event))
            (push event result)
          (let ((kind (disco-markdown--marker-kind event)))
            (cond
             ((eq kind closing-kind) (setq closed t))
             ((memq kind '(underline-open spoiler-open))
              (let* ((expected (if (eq kind 'underline-open)
                                   'underline-close
                                 'spoiler-close))
                     (parsed (disco-markdown--parse-delimiter-events events expected))
                     (children (nth 0 parsed))
                     (remaining (nth 1 parsed))
                     (matched (nth 2 parsed)))
                (setq events remaining)
                (if (not matched)
                    (progn
                      (push (appkit-markup-text
                             (disco-markdown--marker-literal kind)) result)
                      (dolist (child children) (push child result)))
                  (if (eq kind 'underline-open)
                      (dolist (child children)
                        (push (disco-markdown--add-style-inline child 'underline)
                              result))
                    (push
                     (appkit-markup-object
                      (disco-markdown-object--create
                       :kind 'spoiler :raw nil)
                      children)
                     result)))))
             (t
              (push (appkit-markup-text
                     (disco-markdown--marker-literal kind)) result)))))))
    (list (nreverse result) events closed)))

(defun disco-markdown--token-object (token styles)
  "Return semantic provider object for TOKEN carrying STYLES."
  (let (kind id name style animated display)
    (cond
     ((string-match (concat "\\`" disco-markdown--regexp-role-mention "\\'") token)
      (setq kind 'role id (match-string 1 token)
            name (or (gethash id disco-markdown--role-map) (format "role:%s" id))
            display (concat "@" name)))
     ((string-match (concat "\\`" disco-markdown--regexp-user-mention "\\'") token)
      (setq kind 'user id (match-string 1 token)
            name (or (gethash id disco-markdown--user-map) (format "user:%s" id))
            display (concat "@" name)))
     ((string-match (concat "\\`" disco-markdown--regexp-channel-mention "\\'") token)
      (setq kind 'channel id (match-string 1 token)
            name (or (gethash id disco-markdown--channel-map)
                     (disco-markdown--state-channel-name id)
                     (format "channel:%s" id))
            display (concat "#" name)))
     ((string-match (concat "\\`" disco-markdown--regexp-command-mention "\\'") token)
      (setq kind 'command name (string-trim (match-string 1 token))
            id (match-string 2 token)
            display (if (string-prefix-p "/" name) name (concat "/" name))))
     ((string-match (concat "\\`" disco-markdown--regexp-custom-emoji "\\'") token)
      (setq kind 'emoji name (match-string 1 token) id (match-string 2 token)
            animated (string-prefix-p "<a:" token)
            display (format ":%s:" name)))
     ((string-match (concat "\\`" disco-markdown--regexp-timestamp "\\'") token)
      (setq kind 'timestamp id (match-string 1 token) style (match-string 2 token)
            display (disco-markdown--format-timestamp id style)))
     ((string-match (concat "\\`" disco-markdown--regexp-guild-navigation "\\'") token)
      (setq kind 'navigation id (string-trim (match-string 1 token))
            display (format "id:%s" (if (string-empty-p id) "unknown" id))))
     ((string-match-p (concat "\\`" disco-markdown--regexp-everyone-mention "\\'") token)
      (setq kind 'everyone name token display token))
     (t
      (setq kind 'navigation id (string-remove-prefix "id:" token)
            display token)))
    (appkit-markup-object
     (disco-markdown-object--create
      :kind kind :raw token :id id :name name :style style :animated animated)
     (list (appkit-markup-text display))
     styles)))

(defun disco-markdown--split-provider-tokens (node)
  "Split protected provider occurrences in NODE into semantic objects."
  (cond
   ((appkit-markup-object-p node)
    (list
     (appkit-markup-object
      (appkit-markup-object-value node)
      (apply #'append
             (mapcar #'disco-markdown--split-provider-tokens
                     (appkit-markup-object-fallback node)))
      (appkit-markup-object-styles node))))
   ((or (not (appkit-markup-text-p node))
        (memq 'code (appkit-markup-text-styles node)))
    (list node))
   (t
    (let* ((text (appkit-markup-text-text node))
           (styles (appkit-markup-text-styles node))
           (table (plist-get disco-markdown--sentinels :provider-table))
           (start 0)
           result)
      (cl-loop for position from 0 below (length text)
               for raw = (gethash (aref text position) table)
               when raw do
               (when (< start position)
                 (push (appkit-markup-text
                        (substring text start position) styles)
                       result))
               (push
                (if disco-markdown-enable-discord-tokens
                    (disco-markdown--token-object raw styles)
                  (appkit-markup-text raw styles))
                result)
               (setq start (1+ position)))
      (when (< start (length text))
        (push (appkit-markup-text (substring text start) styles) result))
      (nreverse result)))))

(defun disco-markdown--subtitle-line (nodes)
  "Wrap Discord subtitle line NODES in one provider object when marked."
  (let ((first (car nodes))
        (sentinel (plist-get disco-markdown--sentinels :subtitle)))
    (if (not (and (appkit-markup-text-p first)
                  (string-match
                   (concat "\\`\\([ \t]*\\)" (regexp-quote sentinel))
                   (appkit-markup-text-text first))))
        nodes
      (let* ((text (appkit-markup-text-text first))
             (styles (appkit-markup-text-styles first))
             (indent (match-string 1 text))
             (rest (substring text (match-end 0)))
             (children (append
                        (and (not (string-empty-p rest))
                             (list (appkit-markup-text rest styles)))
                        (cdr nodes)))
             result)
        (when (not (string-empty-p indent))
          (push (appkit-markup-text indent) result))
        (push (appkit-markup-object
               (disco-markdown-object--create :kind 'subtitle :raw nil)
               children)
              result)
        (nreverse result)))))

(defun disco-markdown--apply-subtitles (nodes)
  "Apply Discord subtitle semantics to inline NODES line by line."
  (let (line result)
    (dolist (node nodes)
      (if (appkit-markup-line-break-p node)
          (progn
            (setq result (nconc result (disco-markdown--subtitle-line line)))
            (setq result (nconc result (list node))
                  line nil))
        (setq line (nconc line (list node)))))
    (nconc result (disco-markdown--subtitle-line line))))

(defun disco-markdown--restore-text-sentinels (text code-p)
  "Restore private sentinel characters in TEXT according to CODE-P."
  (let ((result text)
        (escapes `((:escape-less . ?<) (:escape-at . ?@)
                   (:escape-pipe . ?|) (:escape-underscore . ?_)
                   (:escape-dash . ?-) (:escape-hash . ?#))))
    (dolist (entry escapes)
      (setq result
            (replace-regexp-in-string
             (regexp-quote (plist-get disco-markdown--sentinels (car entry)))
             (if code-p
                 (concat "\\" (char-to-string (cdr entry)))
               (char-to-string (cdr entry)))
             result t t)))
    (dolist (entry `((:underline-open . "__") (:underline-close . "__")
                     (:spoiler-open . "||") (:spoiler-close . "||")
                     (:subtitle . "-# ")))
      (setq result
            (replace-regexp-in-string
             (regexp-quote (plist-get disco-markdown--sentinels (car entry)))
             (cdr entry) result t t)))
    (maphash
     (lambda (character raw)
       (setq result
             (replace-regexp-in-string
              (regexp-quote (char-to-string character)) raw result t t)))
     (plist-get disco-markdown--sentinels :provider-table))
    result))

(defun disco-markdown--restore-inline (node &optional code-p)
  "Restore private sentinels recursively in inline NODE."
  (cond
   ((appkit-markup-text-p node)
    (appkit-markup-text
     (disco-markdown--restore-text-sentinels
      (appkit-markup-text-text node)
      (or code-p (memq 'code (appkit-markup-text-styles node))))
     (appkit-markup-text-styles node)))
   ((appkit-markup-link-p node)
    (appkit-markup-link
     (appkit-markup-link-url node)
     (mapcar (lambda (child) (disco-markdown--restore-inline child code-p))
             (appkit-markup-link-children node))))
   ((appkit-markup-object-p node)
    (appkit-markup-object
     (appkit-markup-object-value node)
     (mapcar (lambda (child) (disco-markdown--restore-inline child code-p))
             (appkit-markup-object-fallback node))
     (appkit-markup-object-styles node)))
   (t node)))

(defun disco-markdown--adapt-inlines (children)
  "Adapt Discord extensions and provider tokens in inline CHILDREN."
  (let (events parsed tokens)
    (dolist (node children)
      (if (or (appkit-markup-link-p node)
              (and (appkit-markup-text-p node)
                   (memq 'code (appkit-markup-text-styles node))))
          (setq events
                (nconc events
                       (list (disco-markdown--restore-inline
                              node
                              (and (appkit-markup-text-p node) t)))))
        (setq events
              (nconc events (disco-markdown--split-delimiter-events node)))))
    (setq parsed (car (disco-markdown--parse-delimiter-events events)))
    (dolist (node parsed)
      (setq tokens (nconc tokens (disco-markdown--split-provider-tokens node))))
    (mapcar #'disco-markdown--restore-inline
            (disco-markdown--apply-subtitles tokens))))

(defun disco-markdown--adapt-block (block)
  "Adapt provider semantics recursively in Appkit BLOCK."
  (cond
   ((appkit-markup-paragraph-p block)
    (appkit-markup-paragraph
     (disco-markdown--adapt-inlines (appkit-markup-paragraph-children block))))
   ((appkit-markup-heading-p block)
    (appkit-markup-heading
     (appkit-markup-heading-level block)
     (disco-markdown--adapt-inlines (appkit-markup-heading-children block))))
   ((appkit-markup-quote-p block)
    (appkit-markup-quote
     (mapcar #'disco-markdown--adapt-block (appkit-markup-quote-blocks block))))
   ((appkit-markup-list-p block)
    (appkit-markup-list
     (appkit-markup-list-style block)
     (mapcar
      (lambda (item)
        (appkit-markup-list-item
         (mapcar #'disco-markdown--adapt-block
                 (appkit-markup-list-item-blocks item))))
      (appkit-markup-list-items block))
     :start (appkit-markup-list-start block)))
   ((appkit-markup-preformatted-p block)
    (appkit-markup-preformatted
     (disco-markdown--restore-text-sentinels
      (appkit-markup-preformatted-text block) t)
     (appkit-markup-preformatted-language block)))
   ((appkit-markup-object-block-p block)
    (appkit-markup-object-block
     (appkit-markup-object-block-value block)
     (mapcar #'disco-markdown--adapt-block
             (appkit-markup-object-block-fallback block))))
   (t block)))

(cl-defun disco-markdown-parse (text &key context message spoiler-message-id)
  "Adapt Discord Markdown TEXT into one immutable Appkit parse result."
  (ignore context spoiler-message-id)
  (let* ((source
          (let ((source
                 (if (stringp text) (substring-no-properties text) "")))
            (when (> (length source) appkit-markup-codec-source-limit)
              (signal 'appkit-markup-codec-error '(source-too-long)))
            source))
         (disco-markdown--sentinels (disco-markdown--make-sentinels source))
         (disco-markdown--user-map (disco-markdown--build-user-name-map message))
         (disco-markdown--channel-map
          (disco-markdown--build-channel-name-map message))
         (disco-markdown--role-map (disco-markdown--build-role-name-map message))
         (protected (disco-markdown--protect-extensions
                     source disco-markdown--sentinels))
         (result (appkit-markup-parse 'markdown protected))
         (document
          (appkit-markup-document
           (mapcar #'disco-markdown--adapt-block
                   (appkit-markup-document-blocks
                    (appkit-markup-parse-result-document result))))))
    (appkit-markup-parse-result
     document :diagnostics (appkit-markup-parse-result-diagnostics result))))

(cl-defun disco-markdown-document (text &key context message spoiler-message-id)
  "Return the semantic Appkit Document adapted from Discord Markdown TEXT."
  (appkit-markup-parse-result-document
   (disco-markdown-parse
    text :context context :message message
    :spoiler-message-id spoiler-message-id)))

(defun disco-markdown--link-action (url)
  "Return a native action opening URL."
  (and (disco-markdown--string-present-p url)
       (lambda () (browse-url url t))))

(defun disco-markdown--hide-region (start end)
  "Mask visible characters between START and END without changing text."
  (let ((position start))
    (while (< position end)
      (let ((character (char-after position)))
        (unless (eq character ?\n)
          (add-text-properties
           position (1+ position)
           (list 'display
                 (char-to-string
                  (char-table-range
                   disco-markdown--spoiler-translation-table character))
                 'rear-nonsticky '(display))))
      (setq position (1+ position))))))

(defun disco-markdown--insert-object-fallback (node object-inserter)
  "Insert semantic fallback of object NODE using OBJECT-INSERTER recursively."
  (appkit-markup-ui-insert-document
   (appkit-markup-document
    (list (appkit-markup-paragraph (appkit-markup-object-fallback node))))
   :final-newline-p nil
   :interactive-p t
   :link-action #'disco-markdown--link-action
   :object-inserter object-inserter))

(defun disco-markdown--object-inserter (context spoiler-message-id reveal-spoilers)
  "Return native object inserter for CONTEXT and spoiler rendering state."
  (letrec
      ((inserter
        (lambda (node)
          (let* ((value (appkit-markup-object-value node))
                 (kind (and (disco-markdown-object-p value)
                            (disco-markdown-object-kind value)))
                 (start (point)))
            (pcase kind
              ('emoji
               (let* ((name (disco-markdown-object-name value))
                      (fallback (format ":%s:" name))
                      (display
                       (if (eq context 'room-message)
                           (disco-emoji-image-display-string
                            (disco-markdown-object-id value)
                            (disco-markdown-object-animated value)
                            fallback)
                         fallback)))
                 (insert display)
                 (add-text-properties
                  start (point)
                  (list 'face 'disco-markdown-emoji-face
                        'disco-emoji-id (disco-markdown-object-id value)
                        'disco-emoji-name name
                        'disco-emoji-animated
                        (disco-markdown-object-animated value)))))
              ('spoiler
               (disco-markdown--insert-object-fallback node inserter)
               (add-face-text-property
                start (point) 'disco-markdown-spoiler-face 'append)
               (unless reveal-spoilers
                 (disco-markdown--hide-region start (point)))
               (when (< start (point))
                 (let ((message-id spoiler-message-id))
                   (appkit-ui-add-action
                    start (point)
                    (and (disco-markdown--string-present-p message-id)
                         (lambda ()
                           (when (fboundp 'disco-room-toggle-message-spoilers)
                             (funcall 'disco-room-toggle-message-spoilers message-id))))
                    :help-echo (if reveal-spoilers "Hide spoiler" "Reveal spoiler")
                    :face 'disco-markdown-spoiler-face)
                   (add-text-properties
                    start (point)
                    (list 'disco-markdown-spoiler-message-id message-id
                          'disco-markdown-spoiler-hidden (not reveal-spoilers))))))
              ('subtitle
               (disco-markdown--insert-object-fallback node inserter)
               (add-face-text-property
                start (point) 'disco-markdown-subtitle-face 'append))
              (_
               (disco-markdown--insert-object-fallback node inserter)
               (add-face-text-property
                start (point)
                (pcase kind
                  ((or 'user 'role 'channel 'everyone)
                   'disco-markdown-mention-face)
                  ('command 'disco-markdown-command-face)
                  ('timestamp 'disco-markdown-timestamp-face)
                  ('navigation 'disco-markdown-navigation-face)
                  (_ 'appkit-markup-object-fallback-face))
                'append)))))))
    inserter))

(defun disco-markdown--native-face-p (face expected)
  "Return non-nil when FACE includes EXPECTED."
  (or (eq face expected)
      (and (listp face)
           (or (memq expected face)
               (seq-some
                (lambda (entry)
                  (and (listp entry)
                       (disco-markdown--native-face-p entry expected)))
                face)))))

(defun disco-markdown--annotate-native-span (start end)
  "Add Discord copy semantics to native markup between START and END."
  (let ((position start))
    (while (< position end)
      (let* ((face (get-text-property position 'face))
             (action (get-text-property
                      position appkit-ui-action-property))
             (help (get-text-property position 'help-echo))
             (next
              (min
               (or (next-single-property-change position 'face nil end) end)
               (or (next-single-property-change
                    position appkit-ui-action-property nil end)
                   end)
               (or (next-single-property-change
                    position 'help-echo nil end)
                   end))))
        (cond
         ((and (functionp action)
               (stringp help)
               (disco-markdown--native-face-p
                face 'appkit-markup-link-face))
          (add-text-properties
           position next (list 'disco-markdown-url help)))
         ((disco-markdown--native-face-p
           face 'appkit-markup-code-face)
          (add-text-properties
           position next
           '(disco-markdown-code t disco-markdown-code-kind inline)))
         ((disco-markdown--native-face-p
           face 'appkit-markup-preformatted-face)
          (add-text-properties
           position next
           '(disco-markdown-code t disco-markdown-code-kind block))))
        (setq position next)))))

(cl-defun disco-markdown-insert-document
    (document &key context spoiler-message-id reveal-spoilers prefix properties
              (final-newline-p t) (interactive-p t))
  "Insert semantic Discord DOCUMENT natively at point."
  (let* ((object-inserter
          (disco-markdown--object-inserter
           context spoiler-message-id reveal-spoilers))
         (span
          (appkit-markup-ui-insert-document
           document
           :prefix prefix
           :properties properties
           :final-newline-p final-newline-p
           :interactive-p interactive-p
           :link-action #'disco-markdown--link-action
           :object-inserter object-inserter)))
    (disco-markdown--annotate-native-span (car span) (cdr span))
    span))

(cl-defun disco-markdown-insert
    (text &key context message spoiler-message-id reveal-spoilers prefix properties
          (final-newline-p t) (interactive-p t))
  "Adapt Discord Markdown TEXT and insert its semantic Document natively."
  (disco-markdown-insert-document
   (disco-markdown-document
    text :context context :message message
    :spoiler-message-id spoiler-message-id)
   :context context
   :spoiler-message-id spoiler-message-id
   :reveal-spoilers reveal-spoilers
   :prefix prefix
   :properties properties
   :final-newline-p final-newline-p
   :interactive-p interactive-p))

(cl-defun disco-markdown-render
    (text &key context message spoiler-message-id reveal-spoilers)
  "Return native-rendered Discord Markdown TEXT as a propertized string."
  (with-temp-buffer
    (disco-markdown-insert
     text :context context :message message
     :spoiler-message-id spoiler-message-id
     :reveal-spoilers reveal-spoilers
     :final-newline-p nil)
    (buffer-string)))

(cl-defun disco-markdown-copy-export
    (text &key context message spoiler-message-id reveal-spoilers)
  "Return a property-free semantic copy export for Discord Markdown TEXT."
  (ignore reveal-spoilers)
  (appkit-markup-plain-text
   (disco-markdown-document
    text :context (or context 'copy-export) :message message
    :spoiler-message-id spoiler-message-id)))

(defun disco-markdown--printer-escape (text)
  "Escape literal Discord Markdown TEXT."
  (replace-regexp-in-string
   "[][\\`*_~|]" (lambda (match) (concat "\\" match)) text t t))

(defun disco-markdown--printer-escape-block-start (text)
  "Escape a Discord block marker at the start of literal TEXT."
  (let ((position
         (cond
          ((string-match-p
            "\\`\\(?:#\\{1,6\\}[ \t]+\\|>[ \t]?\\|```\\|-#[ \t]+\\)"
            text)
           0)
          ((string-match "\\`\\( *\\)[-+*][ \t]+" text)
           (length (match-string 1 text)))
          ((string-match "\\` *[0-9]+\\([.)]\\)[ \t]+" text)
           (match-beginning 1)))))
    (if position
        (concat (substring text 0 position) "\\"
                (substring text position))
      text)))

(defun disco-markdown--printer-escape-url (url)
  "Escape Discord Markdown destination delimiters in URL."
  (let ((position 0) (start 0) pieces)
    (while (< position (length url))
      (when (memq (aref url position) '(?\\ ?\( ?\)))
        (when (< start position)
          (push (substring url start position) pieces))
        (push (string ?\\ (aref url position)) pieces)
        (setq start (1+ position)))
      (setq position (1+ position)))
    (when (< start (length url))
      (push (substring url start) pieces))
    (if pieces (apply #'concat (nreverse pieces)) url)))

(defun disco-markdown--printer-style-delimiter (style)
  "Return Discord delimiter for semantic STYLE."
  (alist-get style
             '((bold . "**") (italic . "*") (underline . "__")
               (strike . "~~") (code . "`"))))

(defun disco-markdown--printer-prefix-lines (text first rest)
  "Prefix TEXT first with FIRST and subsequently with REST."
  (let ((first-line-p t))
    (mapconcat
     (lambda (line)
       (prog1 (concat (if first-line-p first rest) line)
         (setq first-line-p nil)))
     (split-string text "\n" nil) "\n")))

(defun disco-markdown--printer-inlines (children path losses)
  "Print inline CHILDREN at PATH, mutating LOSSES list cell."
  (let ((line-start-p t) active result)
    (cl-labels
        ((emit (text)
           (unless (string-empty-p text) (push text result)))
         (desired-styles
          (node)
          (let ((text (appkit-markup-text-text node)) desired)
            (dolist (style (appkit-markup-text-styles node))
              (if-let* ((delimiter
                         (disco-markdown--printer-style-delimiter style)))
                  (if (and (eq style 'code)
                           (string-match-p (regexp-quote delimiter) text))
                      (push (appkit-markup-loss style path) (car losses))
                    (push style desired))
                (push (appkit-markup-loss style path) (car losses))))
            (setq desired (nreverse desired))
            ;; Keep already-open delimiters outermost when a run adds styles.
            ;; This avoids closing and reopening underline merely because the
            ;; normalized style order places a newly added bold first.
            (append
             (seq-filter (lambda (style) (memq style desired)) active)
             (seq-remove (lambda (style) (memq style active)) desired))))
         (transition
          (desired)
          (let ((common 0) (left active) (right desired))
            (while (and left right (eq (car left) (car right)))
              (setq common (1+ common)
                    left (cdr left)
                    right (cdr right)))
            (dolist (style (reverse (nthcdr common active)))
              (emit (disco-markdown--printer-style-delimiter style)))
            (dolist (style (nthcdr common desired))
              (emit (disco-markdown--printer-style-delimiter style)))
            (setq active desired)))
         (special
          (node)
          (cond
           ((appkit-markup-link-p node)
            (format
             "[%s](%s)"
             (disco-markdown--printer-inlines
              (appkit-markup-link-children node)
              (append path '(label)) losses)
             (disco-markdown--printer-escape-url
              (appkit-markup-link-url node))))
           ((appkit-markup-object-p node)
            (let* ((value (appkit-markup-object-value node))
                   (kind (and (disco-markdown-object-p value)
                              (disco-markdown-object-kind value)))
                   (fallback
                    (disco-markdown--printer-inlines
                     (appkit-markup-object-fallback node)
                     (append path '(fallback)) losses)))
              (pcase kind
                ('spoiler (concat "||" fallback "||"))
                ('subtitle (concat "-# " fallback))
                ((or 'user 'role 'channel 'command 'emoji
                     'timestamp 'navigation 'everyone)
                 (or (disco-markdown-object-raw value) fallback))
                (_
                 (push (appkit-markup-loss 'object path) (car losses))
                 fallback))))
           ((appkit-markup-line-break-p node) "  \n")
           (t ""))))
      (dolist (node children)
        (if (appkit-markup-text-p node)
            (let* ((desired (desired-styles node))
                   (text (appkit-markup-text-text node))
                   (encoded
                    (if (memq 'code desired)
                        text
                      (disco-markdown--printer-escape text))))
              (transition desired)
              (when (and line-start-p (null desired))
                (setq encoded
                      (disco-markdown--printer-escape-block-start encoded)))
              (emit encoded)
              (setq line-start-p nil))
          (transition nil)
          (let ((encoded (special node)))
            (emit encoded)
            (setq line-start-p
                  (and (not (string-empty-p encoded))
                       (= (aref encoded (1- (length encoded))) ?\n))))))
      (transition nil))
    (apply #'concat (nreverse result))))

(defun disco-markdown--printer-blocks (blocks path losses)
  "Print semantic BLOCKS at PATH, mutating LOSSES list cell."
  (let (result)
    (cl-loop
     for block in blocks
     for index from 0
     for here = (append path (list index))
     do
     (push
      (cond
       ((appkit-markup-paragraph-p block)
        (disco-markdown--printer-inlines
         (appkit-markup-paragraph-children block)
         (append here '(children)) losses))
       ((appkit-markup-heading-p block)
        (concat
         (make-string (appkit-markup-heading-level block) ?#)
         " "
         (disco-markdown--printer-inlines
          (appkit-markup-heading-children block)
          (append here '(children)) losses)))
       ((appkit-markup-quote-p block)
        (disco-markdown--printer-prefix-lines
         (disco-markdown--printer-blocks
          (appkit-markup-quote-blocks block)
          (append here '(blocks)) losses)
         "> " "> "))
       ((appkit-markup-list-p block)
        (let ((number (or (appkit-markup-list-start block) 1))
              items)
          (cl-loop
           for item in (appkit-markup-list-items block)
           for item-index from 0
           do
           (let* ((marker
                   (if (eq (appkit-markup-list-style block) 'ordered)
                       (prog1 (format "%d. " number)
                         (setq number (1+ number)))
                     "- "))
                  (body
                   (disco-markdown--printer-blocks
                    (appkit-markup-list-item-blocks item)
                    (append here (list 'items item-index 'blocks))
                    losses)))
             (push
              (disco-markdown--printer-prefix-lines
               body marker (make-string (length marker) ?\s))
              items)))
          (mapconcat #'identity (nreverse items) "\n")))
       ((appkit-markup-preformatted-p block)
        (let* ((text (appkit-markup-preformatted-text block))
               (runs (mapcar #'length (split-string text "[^`]+" t)))
               (width (max 3 (1+ (if runs (apply #'max runs) 0))))
               (fence (make-string width ?`)))
          (concat fence
                  (or (appkit-markup-preformatted-language block) "")
                  "\n" text "\n" fence)))
       ((appkit-markup-object-block-p block)
        (push (appkit-markup-loss 'object-block here) (car losses))
        (disco-markdown--printer-blocks
         (appkit-markup-object-block-fallback block)
         (append here '(fallback)) losses))
       (t ""))
      result))
    (mapconcat #'identity (nreverse result) "\n\n")))

(defun disco-markdown--codec-print (document _context)
  "Print DOCUMENT as canonical Discord Markdown."
  (let ((losses (list nil)))
    (appkit-markup-print-result
     (disco-markdown--printer-blocks
      (appkit-markup-document-blocks document) '(blocks) losses)
     (nreverse (car losses)))))

(defun disco-markdown--codec-parse (source context)
  "Parse Discord Markdown SOURCE into provider-aware semantics."
  (disco-markdown-parse source :context context))

(defun disco-markdown--codec-edit (source operation start end data context)
  "Delegate Discord source edit geometry to Appkit Markdown."
  (funcall
   (appkit-markup-codec-edit-function (appkit-markup-codec 'markdown))
   source operation start end data context))

(appkit-markup-register-codec
 'discord-markdown
 :label "Discord Markdown"
 :parse #'disco-markdown--codec-parse
 :print #'disco-markdown--codec-print
 :edit #'disco-markdown--codec-edit
 :capabilities
 '(heading bold italic underline strike code link quote list preformatted))

(provide 'disco-markdown)

;;; disco-markdown.el ends here
