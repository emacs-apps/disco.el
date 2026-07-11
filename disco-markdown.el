;;; disco-markdown.el --- Markdown rendering helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared Markdown rendering entrypoint used by room/embed modules.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)

(defcustom disco-markdown-enable-discord-tokens t
  "When non-nil, render Discord-specific tokens after Markdown pass.

This includes mentions, channel references, role mentions, command mentions,
custom emoji markers, and Discord timestamps."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-enable-spoiler-render t
  "When non-nil, render Discord spoiler syntax (`||spoiler||`) in rich
Markdown text."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-cache-enabled t
  "When non-nil, cache Markdown render results by context and text."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-cache-limit 2048
  "Maximum number of entries in Markdown render cache.

When exceeded, cache is cleared to keep runtime behavior simple and stable."
  :type 'integer
  :group 'disco)

(defcustom disco-markdown-fontify-code-blocks-natively t
  "When non-nil, fontify fenced code blocks using the hinted language mode.

This mirrors Discord's language-tagged code block behavior."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-code-block-default-mode nil
  "Fallback major mode used to fontify fenced code blocks without a language."
  :type '(choice (const :tag "None" nil)
          (symbol :tag "Major mode"))
  :group 'disco)

(defcustom disco-markdown-code-lang-modes
  '(("bash" . sh-mode)
    ("c" . c-mode)
    ("cpp" . c++-mode)
    ("c++" . c++-mode)
    ("elisp" . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("go" . go-mode)
    ("javascript" . js-mode)
    ("js" . js-mode)
    ("json" . js-json-mode)
    ("python" . python-mode)
    ("rust" . rust-mode)
    ("sh" . sh-mode)
    ("shell" . sh-mode)
    ("typescript" . typescript-mode)
    ("ts" . typescript-mode))
  "Extra language name to major mode mappings for fenced code blocks."
  :type '(repeat (cons (string :tag "Language")
                       (symbol :tag "Major mode")))
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

(defface disco-markdown-navigation-face
  '((t :inherit font-lock-function-name-face))
  "Face used for rendered Discord guild navigation tokens."
  :group 'disco)

(defface disco-markdown-spoiler-face
  '((t :inherit default))
  "Face used for rendered Discord spoiler contents."
  :group 'disco)

(defface disco-markdown-subtitle-face
  '((t :inherit shadow :height 0.9))
  "Face used for Discord `-#` subtitle lines."
  :group 'disco)

(defface disco-markdown-link-face
  '((t :inherit link))
  "Face used for internal rendered links."
  :group 'disco)

(defface disco-markdown-strong-face
  '((t :inherit bold))
  "Face used for internal strong emphasis."
  :group 'disco)

(defface disco-markdown-emphasis-face
  '((t :inherit italic))
  "Face used for internal emphasis."
  :group 'disco)

(defface disco-markdown-underline-face
  '((t :underline t))
  "Face used for internal underline emphasis."
  :group 'disco)

(defface disco-markdown-strikethrough-face
  '((t :strike-through t))
  "Face used for internal strikethrough emphasis."
  :group 'disco)

(defface disco-markdown-code-face
  '((t :inherit fixed-pitch))
  "Face used for internal inline and fenced code."
  :group 'disco)

(defface disco-markdown-blockquote-face
  '((t :inherit font-lock-comment-face))
  "Face used for internal blockquote sections."
  :group 'disco)

(defface disco-markdown-heading-1-face
  '((t :inherit default :weight bold :height 1.30))
  "Face used for first-level headings."
  :group 'disco)

(defface disco-markdown-heading-2-face
  '((t :inherit default :weight bold :height 1.18))
  "Face used for second-level headings."
  :group 'disco)

(defface disco-markdown-heading-3-face
  '((t :inherit default :weight bold :height 1.08))
  "Face used for third-level headings."
  :group 'disco)

(defface disco-markdown-heading-4-face
  '((t :inherit default :weight bold))
  "Face used for fourth-level headings."
  :group 'disco)

(defface disco-markdown-heading-5-face
  '((t :inherit default :weight bold))
  "Face used for fifth-level headings."
  :group 'disco)

(defface disco-markdown-heading-6-face
  '((t :inherit default :weight bold))
  "Face used for sixth-level headings."
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

(defconst disco-markdown--regexp-guild-navigation "<id:\\([^>]+\\)>"
  "Regexp matching Discord guild navigation tokens.")

(defconst disco-markdown--regexp-guild-navigation-bare "\\_<id:[[:alnum:]:_-]+\\_>"
  "Regexp matching guild navigation tokens after markdown angle-bracket stripping.")

(defconst disco-markdown--regexp-everyone-mention "@\\(?:everyone\\|here\\)"
  "Regexp matching @everyone/@here mention tokens.")

(defconst disco-markdown--regexp-spoiler "||\\([^|\n][^|\n]*?\\)||"
  "Regexp matching single-line Discord spoiler tokens.")

(defconst disco-markdown--regexp-subtitle-line "^\\([ \t]*\\)-#\\(?:[ \t]+\\|$\\)"
  "Regexp matching Discord `-#` subtitle lines.")

(defconst disco-markdown--regexp-heading-marker "^\\([ \t]*\\)\\(#+\\)\\(?:[ \t]+\\|$\\)"
  "Regexp matching visible ATX heading markers in rendered output.")

(defconst disco-markdown--regexp-inline-link
  "\\[\\([^][\n]+\\)\\](\\([^()\n]+\\))"
  "Regexp matching simple inline Markdown links.")

(defconst disco-markdown--regexp-angle-autolink
  "<\\([[:alpha:]][[:alnum:]+.-]*://[^<>[:space:]]+\\)>"
  "Regexp matching angle-bracket autolinks.")

(defconst disco-markdown--regexp-fenced-code-block-start
  "^```.*$"
  "Regexp matching the opening line of a fenced code block.")

(defconst disco-markdown--regexp-fenced-code-block-end
  "^```[ \t]*$"
  "Regexp matching the closing line of a fenced code block.")

(defconst disco-markdown--regexp-inline-code
  "`\\([^`\n]+\\)`"
  "Regexp matching a simple inline code span.")

(defconst disco-markdown--regexp-blockquote-line
  "^\\([ \t]*\\)>\\(?:[ \t]+\\|$\\)"
  "Regexp matching a single-line blockquote marker.")

(defconst disco-markdown--regexp-blockquote-rest-line
  "^\\([ \t]*\\)>>>\\(?:[ \t]+\\|$\\)"
  "Regexp matching a blockquote marker that quotes the rest of the message.")

(defconst disco-markdown--spoiler-translation-table
  (let ((table (make-char-table 'translation-table))
        (mask-char (decode-char 'ucs #x2588)))
    (set-char-table-range table t mask-char)
    (aset table ?\n ?\n)
    table)
  "Translation table used to mask hidden spoiler contents.")

(defvar disco-markdown--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'disco-markdown-open-at-point)
    (define-key map (kbd "RET") #'disco-markdown-open-at-point)
    map)
  "Keymap used for rendered Markdown links.")

(defvar disco-markdown--spoiler-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'disco-markdown-toggle-spoiler-at-point)
    (define-key map (kbd "RET") #'disco-markdown-toggle-spoiler-at-point)
    map)
  "Keymap used for rendered spoiler regions.")

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

(defun disco-markdown-open-at-point (&optional event)
  "Open rendered Markdown link at point or EVENT."
  (interactive (list last-input-event))
  (when (eventp event)
    (posn-set-point (event-start event)))
  (let ((url (or (get-text-property (point) 'disco-markdown-url)
                 (get-text-property (line-beginning-position)
                                    'disco-markdown-url))))
    (unless (disco-markdown--string-present-p url)
      (user-error "disco: no Markdown link at point"))
    (browse-url url t)))

(defun disco-markdown-toggle-spoiler-at-point (&optional event)
  "Toggle rendered spoiler at point or EVENT."
  (interactive (list last-input-event))
  (when (eventp event)
    (posn-set-point (event-start event)))
  (let ((message-id (or (get-text-property (point) 'disco-markdown-spoiler-message-id)
                        (get-text-property (line-beginning-position)
                                           'disco-markdown-spoiler-message-id))))
    (unless (and (disco-markdown--string-present-p message-id)
                 (fboundp 'disco-room-toggle-message-spoilers))
      (user-error "disco: spoiler is not interactive here"))
    (funcall 'disco-room-toggle-message-spoilers message-id)))

(defun disco-markdown--cache-key (context text &optional message-context-key)
  "Build cache key from CONTEXT, TEXT, and MESSAGE-CONTEXT-KEY."
  (list context message-context-key text))

(defun disco-markdown--render-policy-key ()
  "Return a stable cache key for settings that affect rendered output."
  (md5
   (prin1-to-string
    (list disco-markdown-fontify-code-blocks-natively
          disco-markdown-code-block-default-mode
          disco-markdown-code-lang-modes))))

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

(defun disco-markdown--next-style-change (text pos len)
  "Return next position where display-relevant properties change.

TEXT is the source string, POS is the current index, and LEN is its length."
  (let ((next-face (or (next-single-char-property-change pos 'face text) len))
        (next-font-lock
         (or (next-single-char-property-change pos 'font-lock-face text) len))
        (next-help-echo
         (or (next-single-char-property-change pos 'help-echo text) len)))
    (min next-face next-font-lock next-help-echo)))

(defun disco-markdown--sanitize-face-properties (text)
  "Return TEXT stripped to stable display-relevant properties."
  (let* ((source (or text ""))
         (copy (substring-no-properties source))
         (len (length source))
         (pos 0))
    (while (< pos len)
      (let* ((face (or (get-text-property pos 'face source)
                       (get-text-property pos 'font-lock-face source)))
             (help-echo (get-text-property pos 'help-echo source))
             (next (disco-markdown--next-style-change source pos len))
             props)
        (when face
          (setq props (list 'face face)))
        (when (stringp help-echo)
          (setq props (append props (list 'help-echo help-echo))))
        (when props
          (add-text-properties pos next props copy))
        (setq pos next)))
    copy))

(defun disco-markdown--face-match-p (face target)
  "Return non-nil when FACE or one of its entries matches TARGET."
  (if (listp face)
      (memq target face)
    (eq face target)))

(defun disco-markdown--heading-face-for-level (level)
  "Return disco heading face symbol for heading LEVEL."
  (pcase level
    (1 'disco-markdown-heading-1-face)
    (2 'disco-markdown-heading-2-face)
    (3 'disco-markdown-heading-3-face)
    (4 'disco-markdown-heading-4-face)
    (5 'disco-markdown-heading-5-face)
    (_ 'disco-markdown-heading-6-face)))

(defun disco-markdown--link-face-p (face)
  "Return non-nil when FACE already denotes a link-like span."
  (or (disco-markdown--face-match-p face 'disco-markdown-link-face)
      (disco-markdown--face-match-p face 'link)))

(defun disco-markdown--add-link-face (object start end)
  "Add `disco-markdown-link-face' to OBJECT between START and END."
  (when (< start end)
    (add-face-text-property start end 'disco-markdown-link-face 'append object)))

(defun disco-markdown--make-link-string (label url)
  "Return LABEL propertized as an openable link to URL."
  (let ((payload (copy-sequence (or label ""))))
    (disco-markdown--add-link-face payload 0 (length payload))
    (disco-markdown--add-open-url-properties payload 0 (length payload) url)
    payload))

(defun disco-markdown--add-action-properties (object start end keymap help-echo properties)
  "Add KEYMAP/HELP-ECHO/PROPERTIES to OBJECT between START and END."
  (when (< start end)
    (add-text-properties
     start end
     (append (list 'keymap keymap
                   'mouse-face 'highlight
                   'follow-link t
                   'help-echo help-echo)
             properties)
     object)))

(defun disco-markdown--add-open-url-properties (object start end url)
  "Add Markdown URL open properties to OBJECT between START and END for URL."
  (when (disco-markdown--string-present-p url)
    (disco-markdown--add-action-properties
     object start end disco-markdown--link-keymap
     (format "Open link: %s" url)
     (list 'disco-markdown-url url))))

(defun disco-markdown--hide-spoiler-text (text)
  "Return TEXT with display masking applied for hidden spoilers.

The underlying text stays intact so reveal and copy behavior can preserve the
original spoiler contents, including leading and trailing spaces."
  (let* ((payload (copy-sequence (or text "")))
         (len (length payload))
         (pos 0))
    (while (< pos len)
      (let* ((char (aref payload pos))
             (masked (char-table-range disco-markdown--spoiler-translation-table
                                       char)))
        (unless (eq char ?\n)
          (add-text-properties pos (1+ pos)
                               (list 'display (string masked)
                                     'rear-nonsticky '(display))
                               payload))
        (setq pos (1+ pos))))
    payload))

(defun disco-markdown--make-spoiler-string (text spoiler-message-id reveal-spoilers)
  "Return TEXT rendered as a Discord spoiler string.

SPOILER-MESSAGE-ID enables room interaction. When REVEAL-SPOILERS is non-nil,
return visible spoiler contents; otherwise return masked text."
  (let* ((payload (if reveal-spoilers
                      (copy-sequence (or text ""))
                    (disco-markdown--hide-spoiler-text text)))
         (help-echo (if reveal-spoilers
                        "Hide spoiler"
                      "Reveal spoiler")))
    (add-face-text-property 0 (length payload)
                            'disco-markdown-spoiler-face 'append payload)
    (if (disco-markdown--string-present-p spoiler-message-id)
        (disco-markdown--add-action-properties
         payload 0 (length payload) disco-markdown--spoiler-keymap help-echo
         (list 'disco-markdown-spoiler-message-id spoiler-message-id
               'disco-markdown-spoiler-hidden (not reveal-spoilers)))
      (when (< 0 (length payload))
        (add-text-properties 0 (length payload)
                             (list 'help-echo help-echo)
                             payload)))
    payload))

(defun disco-markdown--copy-materialize-line-prefixes (text)
  "Return TEXT with `line-prefix' regions turned into literal text."
  (let* ((source (or text ""))
         (len (length source))
         (pos 0)
         parts)
    (while (< pos len)
      (let* ((newline (or (string-match "\n" source pos) len))
             (line-end (if (< newline len) (1+ newline) newline))
             (prefix (or (get-text-property pos 'line-prefix source)
                         (and (> line-end pos)
                              (get-text-property (1- line-end)
                                                 'line-prefix source))))
             (line (substring source pos line-end)))
        (when (stringp prefix)
          (push (copy-sequence prefix) parts))
        (push line parts)
        (setq pos line-end)))
    (apply #'concat (nreverse parts))))

(defun disco-markdown--sanitize-copy-properties (text)
  "Return TEXT with interactive/display-only properties removed."
  (let ((copy (copy-sequence (or text ""))))
    (remove-list-of-text-properties
     0 (length copy)
     '(keymap mouse-face follow-link help-echo
       line-prefix wrap-prefix rear-nonsticky display
       read-only front-sticky
       disco-markdown-url
       disco-markdown-spoiler-message-id
       disco-markdown-spoiler-hidden
       disco-markdown-code
       disco-markdown-code-kind
       disco-markdown-protected)
     copy)
    copy))

(cl-defun disco-markdown-copy-export (text &key context message spoiler-message-id
                                           reveal-spoilers)
  "Return TEXT exported for copy/yank operations.

This materializes line prefixes such as blockquote markers and drops
interactive display properties that should not leak into the kill ring."
  (disco-markdown--sanitize-copy-properties
   (disco-markdown--copy-materialize-line-prefixes
    (disco-markdown-render text
                           :context (or context 'copy-export)
                           :message message
                           :spoiler-message-id spoiler-message-id
                           :reveal-spoilers reveal-spoilers))))

(defun disco-markdown--url-inside-literal-markdown-link-p (text start end)
  "Return non-nil when URL span START END in TEXT is part of literal link syntax."
  (or (and (< 1 start)
           (< end (length text))
           (eq (aref text (1- start)) ?\()
           (eq (aref text (- start 2)) ?\])
           (eq (aref text end) ?\)))
      (and (< 0 start)
           (< end (length text))
           (eq (aref text (1- start)) ?<)
           (eq (aref text end) ?>))))

(defun disco-markdown--apply-visible-url-properties (text)
  "Attach open actions to visible URL substrings outside protected spans."
  (let ((copy (copy-sequence text)))
    (with-temp-buffer
      (insert copy)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((bounds (bounds-of-thing-at-point 'url))
               (beg (and bounds (car bounds)))
               (end (and bounds (cdr bounds)))
               (url (and bounds (thing-at-point-url-at-point))))
          (if (and bounds
                   (<= (point) beg)
                   (disco-markdown--string-present-p url))
              (progn
                (unless (or (disco-markdown--position-protected-p
                             (1- beg) copy)
                            (get-text-property (1- beg)
                                               'disco-markdown-spoiler-message-id
                                               copy)
                            (get-text-property (1- beg)
                                               'disco-markdown-url
                                               copy)
                            (disco-markdown--url-inside-literal-markdown-link-p
                             copy (1- beg) (1- end)))
                  (unless (disco-markdown--link-face-p
                           (get-text-property (1- beg) 'face copy))
                    (disco-markdown--add-link-face copy (1- beg) (1- end)))
                  (disco-markdown--add-open-url-properties
                   copy (1- beg) (1- end) url))
                (goto-char end))
            (forward-char 1)))))
    copy))

(defun disco-markdown--apply-line-prefix-properties (line-start line-end prefix face)
  "Apply PREFIX and FACE display properties to a line.

LINE-START and LINE-END delimit the line contents in the current buffer."
  (let ((prop-end (if (< line-start line-end)
                      line-end
                    (and (< line-end (point-max))
                         (1+ line-end)))))
    (when prop-end
      (add-text-properties line-start prop-end
                           (list 'line-prefix prefix
                                 'wrap-prefix prefix)
                           (current-buffer)))
    (when (< line-start line-end)
      (add-face-text-property line-start line-end face 'append))))

(defun disco-markdown--apply-blockquote-lines (text)
  "Render blockquote markers in plain TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((quote-rest nil)
          (prefix (propertize "| " 'face 'disco-markdown-blockquote-face)))
      (while (< (point) (point-max))
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (goto-char line-start)
          (cond
           (quote-rest
            (disco-markdown--apply-line-prefix-properties
             line-start line-end prefix 'disco-markdown-blockquote-face))
           ((not (get-text-property line-start 'disco-markdown-protected))
            (cond
             ((and (looking-at disco-markdown--regexp-blockquote-rest-line)
                   (not (get-text-property (match-end 1)
                                           'disco-markdown-protected)))
              (delete-region (match-end 1) (match-end 0))
              (setq line-start (line-beginning-position)
                    line-end (line-end-position)
                    quote-rest t)
              (disco-markdown--apply-line-prefix-properties
               line-start line-end prefix 'disco-markdown-blockquote-face))
             ((and (looking-at disco-markdown--regexp-blockquote-line)
                   (not (get-text-property (match-end 1)
                                           'disco-markdown-protected)))
              (delete-region (match-end 1) (match-end 0))
              (setq line-start (line-beginning-position)
                    line-end (line-end-position))
              (disco-markdown--apply-line-prefix-properties
               line-start line-end prefix 'disco-markdown-blockquote-face))))))
        (forward-line 1)))
    (buffer-string)))

(defun disco-markdown--apply-heading-lines (text)
  "Apply disco heading faces to ATX heading lines in plain TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (goto-char line-start)
        (when (and (not (get-text-property line-start 'disco-markdown-protected))
                   (looking-at disco-markdown--regexp-heading-marker)
                   (not (get-text-property (match-beginning 2)
                                           'disco-markdown-protected)))
          (let ((level (min 6 (length (match-string-no-properties 2)))))
            (delete-region (match-beginning 2) (match-end 0))
            (setq line-start (line-beginning-position)
                  line-end (line-end-position))
            (when (< line-start line-end)
              (add-face-text-property
               line-start line-end
               (disco-markdown--heading-face-for-level level)
               'append))))
        (forward-line 1)))
    (buffer-string)))

(defun disco-markdown--find-next-unprotected-char (char limit)
  "Return next unprotected CHAR before LIMIT in current buffer, or nil."
  (let ((found nil)
        (needle (char-to-string char)))
    (while (and (not found)
                (search-forward needle limit t))
      (let ((pos (1- (point))))
        (unless (disco-markdown--position-protected-p pos)
          (setq found pos))))
    found))

(defun disco-markdown--find-inline-link-url-end (limit)
  "Return URL closing paren position before LIMIT in current buffer, or nil."
  (let ((depth 1)
        (close-pos nil))
    (while (and (not close-pos)
                (< (point) limit))
      (let ((pos (point))
            (char (char-after)))
        (cond
         ((or (null char)
              (eq char ?\n))
          (setq close-pos :invalid))
         ((disco-markdown--position-protected-p pos)
          (forward-char 1))
         ((eq char ?\()
          (setq depth (1+ depth))
          (forward-char 1))
         ((eq char ?\))
          (setq depth (1- depth))
          (if (= depth 0)
              (setq close-pos pos)
            (forward-char 1)))
         (t
          (forward-char 1)))))
    (unless (eq close-pos :invalid)
      close-pos)))

(defun disco-markdown--apply-inline-link-replacements (text)
  "Render inline Markdown links in plain TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "[" nil t)
      (let* ((open-start (1- (point)))
             (line-limit (min (1+ (line-end-position)) (point-max)))
             (continue-pos (point)))
        (unless (disco-markdown--position-protected-p open-start)
          (let ((label-start (point))
                (close-bracket nil)
                (close-end nil)
                (payload nil))
            (save-excursion
              (setq close-bracket
                    (disco-markdown--find-next-unprotected-char ?\] line-limit))
              (when close-bracket
                (goto-char (1+ close-bracket))
                (when (and (< (point) line-limit)
                           (eq (char-after) ?\()
                           (not (disco-markdown--position-protected-p (point))))
                  (forward-char 1)
                  (let* ((url-start (point))
                         (url-end (disco-markdown--find-inline-link-url-end line-limit)))
                    (when url-end
                      (let ((url (buffer-substring-no-properties url-start url-end)))
                        (when (disco-markdown--string-present-p url)
                          (setq close-end (1+ url-end)
                                payload
                                (disco-markdown--make-link-string
                                 (buffer-substring label-start close-bracket)
                                 url)))))))))
            (when (and payload close-end)
              (delete-region open-start close-end)
              (goto-char open-start)
              (insert payload)
              (setq continue-pos (+ open-start (length payload))))))
        (goto-char continue-pos)))
    (buffer-string)))

(defun disco-markdown--apply-angle-autolink-replacements (text)
  "Render angle-bracket autolinks in plain TEXT."
  (disco-markdown--apply-regexp-replacements
   text
   (list
    (cons disco-markdown--regexp-angle-autolink
          (lambda ()
            (let ((url (match-string-no-properties 1)))
              (disco-markdown--make-link-string url url)))))))

(defun disco-markdown--apply-internal-link-replacements (text)
  "Render inline links and autolinks in plain TEXT."
  (disco-markdown--apply-angle-autolink-replacements
   (disco-markdown--apply-inline-link-replacements text)))

(defun disco-markdown--render-internal (text)
  "Render TEXT with disco's internal Discord-focused renderer."
  (let ((rendered (if (stringp text) text "")))
    (setq rendered (disco-markdown--apply-fenced-code-blocks rendered))
    (setq rendered (disco-markdown--apply-inline-code-spans rendered))
    (setq rendered (disco-markdown--apply-escapes rendered))
    (setq rendered (disco-markdown--apply-blockquote-lines rendered))
    (setq rendered (disco-markdown--apply-internal-link-replacements rendered))
    (setq rendered (disco-markdown--apply-inline-emphasis rendered))
    (setq rendered (disco-markdown--apply-heading-lines rendered))
    (disco-markdown--apply-visible-url-properties rendered)))

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
    (let* ((id (disco-markdown--normalize-id (alist-get 'id message)))
           (mentions (alist-get 'mentions message))
           (mention-roles (alist-get 'mention_roles message))
           (mention-channels (alist-get 'mention_channels message))
           (resolved (alist-get 'resolved message))
           (snapshot-key (list id mentions mention-roles mention-channels resolved)))
      (md5 (prin1-to-string snapshot-key)))))

(defun disco-markdown--user-display-name (user)
  "Return best display name for USER object."
  (let* ((global-name (alist-get 'global_name user))
         (username (alist-get 'username user))
         (user-id (disco-markdown--normalize-id (alist-get 'id user))))
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
                   (alist-get 'mentions message)))
      (let ((id (disco-markdown--normalize-id (alist-get 'id user))))
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
        (let ((name (alist-get 'name channel)))
          (if (disco-markdown--string-present-p name)
              name
            (format "channel:%s" channel-id)))))))

(defun disco-markdown--build-channel-name-map (message)
  "Return hash map of mentioned channel-id -> display name from MESSAGE."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (channel (disco-markdown--sequence-list
                      (alist-get 'mention_channels message)))
      (let ((id (disco-markdown--normalize-id (alist-get 'id channel)))
            (name (alist-get 'name channel)))
        (disco-markdown--hash-put-id-name table id name)))
    (let* ((resolved (alist-get 'resolved message))
           (resolved-channels (and (listp resolved)
                                   (alist-get 'channels resolved))))
      (dolist (entry (disco-markdown--sequence-list resolved-channels))
        (let* ((object (disco-markdown--entry-object entry))
               (id (or (disco-markdown--normalize-id (alist-get 'id object))
                       (disco-markdown--normalize-id (car-safe entry))))
               (name (alist-get 'name object)))
          (disco-markdown--hash-put-id-name table id name))))
    table))

(defun disco-markdown--channel-context-key (text message)
  "Return channel labels from external state that can affect rendering TEXT.

MESSAGE-provided channel names take precedence, matching the token renderer."
  (when (and disco-markdown-enable-discord-tokens
             (disco-markdown--string-present-p text))
    (let ((message-names (disco-markdown--build-channel-name-map message))
          (seen (make-hash-table :test #'equal))
          (start 0)
          labels)
      (while (string-match disco-markdown--regexp-channel-mention text start)
        (let* ((id (match-string-no-properties 1 text))
               (name (or (gethash id message-names)
                         (disco-markdown--state-channel-name id)
                         (format "channel:%s" id))))
          (unless (gethash id seen)
            (puthash id t seen)
            (push (cons id name) labels)))
        (setq start (match-end 0)))
      (nreverse labels))))

(defun disco-markdown--build-role-name-map (message)
  "Return hash map of role-id -> role name from MESSAGE."
  (let ((table (make-hash-table :test #'equal))
        (resolved (alist-get 'resolved message)))
    (when (listp resolved)
      (let ((resolved-roles (alist-get 'roles resolved)))
        (dolist (entry (disco-markdown--sequence-list resolved-roles))
          (let* ((object (disco-markdown--entry-object entry))
                 (id (or (disco-markdown--normalize-id (alist-get 'id object))
                         (disco-markdown--normalize-id (car-safe entry))))
                 (name (alist-get 'name object)))
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

(defun disco-markdown--format-guild-navigation (raw)
  "Return display label for guild navigation token RAW."
  (let ((token (string-trim (or raw ""))))
    (if (string-empty-p token)
        "id:unknown"
      (format "id:%s" token))))

(defun disco-markdown--contains-relative-timestamp-p (text)
  "Return non-nil when TEXT contains Discord relative timestamp tokens."
  (and (stringp text)
       (string-match-p "<t:[0-9]+:R>" text)))

(defun disco-markdown--position-protected-p (position &optional object)
  "Return non-nil when POSITION in OBJECT belongs to protected Markdown text.

OBJECT defaults to the current buffer and may also be a string."
  (get-text-property position 'disco-markdown-protected object))

(defun disco-markdown--word-constituent-char-p (char)
  "Return non-nil when CHAR behaves like a word constituent."
  (and char
       (memq (char-syntax char) '(?w ?_))))

(defun disco-markdown--markdown-escapable-char-p (char)
  "Return non-nil when CHAR can be escaped in Discord markdown text."
  (and (characterp char)
       (string-match-p "[[:punct:]]" (char-to-string char))))

(defun disco-markdown--lang-mode-predicate (mode)
  "Return non-nil when MODE is a usable major mode for code fontification."
  (and mode
       (fboundp mode)
       (or (not (string-match-p "ts-mode\\'" (symbol-name mode)))
           (cl-loop for pair in (bound-and-true-p major-mode-remap-alist)
                    for func = (cdr pair)
                    thereis (and (atom func) (eq mode func)))
           (cl-loop for pair in auto-mode-alist
                    for func = (cdr pair)
                    thereis (and (atom func) (eq mode func))))))

(defun disco-markdown--get-lang-mode (lang)
  "Return the major mode that should be used for LANG."
  (let ((name (string-trim (or lang ""))))
    (when (disco-markdown--string-present-p name)
      (cl-find-if
       #'disco-markdown--lang-mode-predicate
       (nconc
        (list (cdr (assoc name disco-markdown-code-lang-modes))
              (cdr (assoc (downcase name) disco-markdown-code-lang-modes)))
        (and (fboundp 'treesit-language-available-p)
             (list (and (treesit-language-available-p (intern name))
                        (intern (concat name "-ts-mode")))
                   (and (treesit-language-available-p (intern (downcase name)))
                        (intern (concat (downcase name) "-ts-mode")))))
        (list (intern (concat name "-mode"))
              (intern (concat (downcase name) "-mode"))))))))

(defun disco-markdown--code-fence-language (line)
  "Return the language tag parsed from fenced code LINE, or nil."
  (when (and (stringp line)
             (string-match "\\`[ \\t]*```[ \\t]*\\([^ \\t`\\r\\n]+\\)" line))
    (match-string 1 line)))

(defun disco-markdown--make-code-string (text &optional lang kind)
  "Return TEXT propertized as code, optionally fontified for LANG.

KIND is an optional symbol describing the code span, typically `inline' or
`block'."
  (let* ((source (or text ""))
         (payload (copy-sequence source))
         (lang-mode (and disco-markdown-fontify-code-blocks-natively
                         (or (and (disco-markdown--string-present-p lang)
                                  (disco-markdown--get-lang-mode lang))
                             disco-markdown-code-block-default-mode))))
    (when (and (< 0 (length payload))
               (symbolp lang-mode)
               (fboundp lang-mode))
      (with-current-buffer
          (get-buffer-create
           (format " *disco-markdown-code-fontification:%s*"
                   (symbol-name lang-mode)))
        (let ((inhibit-modification-hooks nil))
          (erase-buffer)
          (insert payload " "))
        (unless (eq major-mode lang-mode)
          (funcall lang-mode))
        (when (fboundp 'font-lock-ensure)
          (font-lock-ensure (point-min) (point-max)))
        (setq payload
              (disco-markdown--sanitize-face-properties
               (buffer-substring (point-min) (1- (point-max)))))))
    (when (< 0 (length payload))
      (add-face-text-property 0 (length payload)
                              'disco-markdown-code-face 'append payload)
      (add-text-properties 0 (length payload)
                           (append '(disco-markdown-protected t
                                     disco-markdown-code t)
                                   (when kind
                                     (list 'disco-markdown-code-kind kind)))
                           payload))
    payload))

(defun disco-markdown--apply-escapes (text)
  "Remove Markdown escape backslashes from TEXT and protect escaped chars."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (let ((slash-pos (1- (point))))
        (unless (disco-markdown--position-protected-p slash-pos)
          (let ((next-char (char-after)))
            (when (and next-char
                       (disco-markdown--markdown-escapable-char-p next-char)
                       (not (disco-markdown--position-protected-p (point))))
              (delete-region slash-pos (point))
              (add-text-properties slash-pos (1+ slash-pos)
                                   '(disco-markdown-protected t)
                                   (current-buffer))
              (goto-char (1+ slash-pos)))))))
    (buffer-string)))

(defun disco-markdown--inline-delimiter-content-valid-p (open-end close-start)
  "Return non-nil when delimiter span between OPEN-END and CLOSE-START is usable."
  (and (< open-end close-start)
       (let ((first (char-after open-end))
             (last (char-before close-start)))
         (and first last
              (not (memq first '(?\s ?\t ?\n)))
              (not (memq last '(?\s ?\t ?\n)))))))

(defun disco-markdown--apply-inline-delimiter-face (text delimiter face &optional protect predicate)
  "Strip DELIMITER pairs from TEXT and apply FACE to enclosed text.

When PROTECT is non-nil, enclosed text is marked as internal protected text.
When PREDICATE is non-nil it is called with OPEN-START, OPEN-END, CLOSE-START,
and CLOSE-END and must return non-nil for the span to be transformed."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((delimiter-len (length delimiter)))
      (while (search-forward delimiter nil t)
        (let* ((open-start (- (point) delimiter-len))
               (open-end (point))
               (line-limit (line-end-position))
               (close-start nil))
          (unless (disco-markdown--position-protected-p open-start)
            (save-excursion
              (while (and (not close-start)
                          (search-forward delimiter line-limit t))
                (let ((candidate-start (- (point) delimiter-len))
                      (candidate-end (point)))
                  (when (and (not (disco-markdown--position-protected-p
                                   candidate-start))
                             (disco-markdown--inline-delimiter-content-valid-p
                              open-end candidate-start)
                             (or (null predicate)
                                 (funcall predicate open-start open-end
                                          candidate-start candidate-end)))
                    (setq close-start candidate-start)))))
            (when close-start
              (delete-region close-start (+ close-start delimiter-len))
              (delete-region open-start (+ open-start delimiter-len))
              (let ((beg open-start)
                    (end (- close-start delimiter-len)))
                (when (< beg end)
                  (add-face-text-property beg end face 'append)
                  (when protect
                    (add-text-properties beg end
                                         '(disco-markdown-protected t))))
                (goto-char (max beg end))))))))
    (buffer-string)))

(defun disco-markdown--apply-fenced-code-blocks (text)
  "Strip fenced code block markers from TEXT and protect the contents."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward disco-markdown--regexp-fenced-code-block-start nil t)
      (let* ((open-start (match-beginning 0))
             (open-line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
             (lang (disco-markdown--code-fence-language open-line)))
        (forward-line 1)
        (let ((content-start (point)))
          (if (re-search-forward disco-markdown--regexp-fenced-code-block-end nil t)
              (let* ((close-start (match-beginning 0))
                     (close-end (match-end 0))
                     (payload (disco-markdown--make-code-string
                               (buffer-substring content-start close-start)
                               lang 'block)))
                (delete-region open-start close-end)
                (goto-char open-start)
                (insert payload)
                (goto-char (+ open-start (length payload))))
            (goto-char (point-max))))))
    (buffer-string)))

(defun disco-markdown--apply-inline-code-spans (text)
  "Strip inline code span markers from TEXT and protect the contents."
  (disco-markdown--apply-regexp-replacements
   text
   (list
    (cons disco-markdown--regexp-inline-code
          (lambda ()
            (disco-markdown--make-code-string
             (buffer-substring (match-beginning 1) (match-end 1))
             nil 'inline))))))

(defun disco-markdown--apply-inline-emphasis (text)
  "Apply internal emphasis styling to TEXT."
  (let ((rendered text))
    (setq rendered
          (disco-markdown--apply-inline-delimiter-face
           rendered "**" 'disco-markdown-strong-face))
    (setq rendered
          (disco-markdown--apply-inline-delimiter-face
           rendered "__" 'disco-markdown-underline-face nil
           (lambda (open-start _open-end _close-start close-end)
             (and (not (disco-markdown--word-constituent-char-p
                        (char-before open-start)))
                  (not (disco-markdown--word-constituent-char-p
                        (char-after close-end)))))))
    (setq rendered
          (disco-markdown--apply-inline-delimiter-face
           rendered "~~" 'disco-markdown-strikethrough-face))
    (setq rendered
          (disco-markdown--apply-inline-delimiter-face
           rendered "*" 'disco-markdown-emphasis-face))
    (disco-markdown--apply-inline-delimiter-face
     rendered "_" 'disco-markdown-emphasis-face nil
     (lambda (open-start _open-end _close-start close-end)
       (and (not (disco-markdown--word-constituent-char-p
                  (char-before open-start)))
            (not (disco-markdown--word-constituent-char-p
                  (char-after close-end))))))))

(defun disco-markdown--apply-regexp-replacements (text replacements)
  "Apply REPLACEMENTS over TEXT, skipping protected regions."
  (with-temp-buffer
    (insert text)
    (dolist (entry replacements)
      (goto-char (point-min))
      (let ((regexp (car entry))
            (replacer (cdr entry)))
        (while (re-search-forward regexp nil t)
          (let ((beg (match-beginning 0)))
            (unless (disco-markdown--position-protected-p beg)
              (let ((replacement (funcall replacer)))
                (when (stringp replacement)
                  (replace-match replacement t t))))))))
    (buffer-string)))

(defun disco-markdown--apply-subtitle-lines (text)
  "Render Discord `-#` subtitle lines in unprotected regions of TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward disco-markdown--regexp-subtitle-line nil t)
      (let ((marker-start (match-end 1))
            (marker-end (match-end 0)))
        (unless (disco-markdown--position-protected-p marker-start)
          (delete-region marker-start marker-end)
          (let ((line-end (line-end-position)))
            (when (< (point) line-end)
              (add-face-text-property
               (point) line-end 'disco-markdown-subtitle-face 'append))))))
    (buffer-string)))

(defun disco-markdown--render-discord-tokens (text message spoiler-message-id
                                                   reveal-spoilers)
  "Render Discord-specific token syntax in TEXT.

MESSAGE carries mention/channel context.  SPOILER-MESSAGE-ID identifies the
message used for spoiler interaction.
REVEAL-SPOILERS controls whether spoiler contents are visible."
  (if (not (disco-markdown--string-present-p text))
      text
    (let* ((user-map (disco-markdown--build-user-name-map message))
           (channel-map (disco-markdown--build-channel-name-map message))
           (role-map (disco-markdown--build-role-name-map message))
           (replacements nil))
      (when disco-markdown-enable-discord-tokens
        (setq replacements
              (append
               replacements
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
                           'face 'disco-markdown-timestamp-face))))
                (cons disco-markdown--regexp-guild-navigation
                      (lambda ()
                        (let ((raw (match-string-no-properties 1)))
                          (propertize
                           (disco-markdown--format-guild-navigation raw)
                           'face 'disco-markdown-navigation-face))))
                (cons disco-markdown--regexp-guild-navigation-bare
                      (lambda ()
                        (let* ((token (match-string-no-properties 0))
                               (raw (if (string-prefix-p "id:" token)
                                        (substring token 3)
                                      token)))
                          (propertize
                           (disco-markdown--format-guild-navigation raw)
                           'face 'disco-markdown-navigation-face))))
                (cons disco-markdown--regexp-everyone-mention
                      (lambda ()
                        (propertize (match-string-no-properties 0)
                                    'face 'disco-markdown-mention-face)))))))
      (when disco-markdown-enable-spoiler-render
        (setq replacements
              (append
               replacements
               (list
                (cons disco-markdown--regexp-spoiler
                      (lambda ()
                        (disco-markdown--make-spoiler-string
                         (buffer-substring (match-beginning 1) (match-end 1))
                         spoiler-message-id
                         reveal-spoilers)))))))
      (let ((rendered (if replacements
                          (disco-markdown--apply-regexp-replacements
                           text replacements)
                        text)))
        (disco-markdown--apply-subtitle-lines rendered)))))

(cl-defun disco-markdown-render (text &key context message spoiler-message-id
                                      reveal-spoilers)
  "Render Markdown TEXT for display.

CONTEXT is an optional symbol used for cache key partitioning.
MESSAGE is optional message data used to resolve mention-like tokens.
SPOILER-MESSAGE-ID enables spoiler toggling inside room buffers.
When REVEAL-SPOILERS is non-nil, spoiler contents are shown instead of masked."
  (let* ((source (if (stringp text) text ""))
         (message-context-key (disco-markdown--message-context-key message))
         (cache-context (list context
                              disco-markdown-enable-discord-tokens
                              disco-markdown-enable-spoiler-render
                              (disco-markdown--render-policy-key)
                              (disco-markdown--channel-context-key source message)
                              spoiler-message-id
                              (and reveal-spoilers t)))
         (cacheable-p (and disco-markdown-cache-enabled
                           (not (disco-markdown--contains-relative-timestamp-p source))))
         (cache-key (and cacheable-p
                         (disco-markdown--cache-key cache-context source
                                                    message-context-key)))
         (cached (disco-markdown--cache-get cache-key)))
    (or cached
        (let* ((markdown-rendered (disco-markdown--render-internal source))
               (rendered (disco-markdown--render-discord-tokens
                          markdown-rendered message
                          spoiler-message-id reveal-spoilers)))
          (disco-markdown--cache-put cache-key rendered)
          rendered))))

(provide 'disco-markdown)

;;; disco-markdown.el ends here
