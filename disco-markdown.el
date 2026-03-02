;;; disco-markdown.el --- Markdown rendering helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared Markdown rendering entrypoint used by room/embed modules.

;;; Code:

(require 'cl-lib)
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

(defcustom disco-markdown-cache-enabled t
  "When non-nil, cache Markdown render results by backend/context/text."
  :type 'boolean
  :group 'disco)

(defcustom disco-markdown-cache-limit 2048
  "Maximum number of entries in Markdown render cache.

When exceeded, cache is cleared to keep runtime behavior simple and stable."
  :type 'integer
  :group 'disco)

(defvar disco-markdown--cache (make-hash-table :test #'equal)
  "Cache table for rendered Markdown strings.")

(defun disco-markdown-clear-cache ()
  "Clear Markdown render cache."
  (interactive)
  (clrhash disco-markdown--cache)
  (message "disco: markdown render cache cleared"))

(defun disco-markdown--cache-key (backend context text)
  "Build cache key from BACKEND, CONTEXT and TEXT."
  (list backend context text))

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

(cl-defun disco-markdown-render (text &key context)
  "Render Markdown TEXT for display.

CONTEXT is an optional symbol used for cache key partitioning."
  (let* ((source (if (stringp text) text ""))
         (backend (disco-markdown--resolve-backend))
         (cache-key (and disco-markdown-cache-enabled
                         (disco-markdown--cache-key backend context source)))
         (cached (disco-markdown--cache-get cache-key)))
    (or cached
        (let ((rendered
               (condition-case _
                   (pcase backend
                     ('markdown-mode
                      (disco-markdown--render-with-markdown-mode source))
                     (_
                      (disco-markdown--render-legacy source)))
                 (error
                  (disco-markdown--render-legacy source)))))
          (disco-markdown--cache-put cache-key rendered)
          rendered))))

(provide 'disco-markdown)

;;; disco-markdown.el ends here
