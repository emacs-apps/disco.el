;;; disco-ui.el --- Shared UI rendering primitives for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Small shared insertion helpers used by room/root renderers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)

;;; ── Vertical bar (SVG in GUI, Unicode in terminal) ──────────────────

(defvar disco-ui--vbar-image-cache (make-hash-table :test #'equal)
  "Cache of SVG vertical-bar images keyed by (COLOR XH CHAR-W).")

(defun disco-ui--face-foreground-color (face)
  "Extract foreground colour string from FACE.
FACE may be a symbol, a plist (:foreground ...) or a list of faces."
  (cond
   ((and (listp face) (plist-get face :foreground))
    (plist-get face :foreground))
   ((symbolp face)
    (ignore-errors (face-foreground face nil t)))
   ((and (listp face) (symbolp (car face)))
    (ignore-errors (face-foreground (car face) nil t)))))

(defun disco-ui--create-vbar-svg (face)
  "Create an SVG vertical-bar image coloured with FACE.
The image fills the full line height so consecutive lines join seamlessly.
Returns nil in terminal frames or when SVG is unavailable."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-rectangle))
    (let* ((xh (or (ignore-errors (default-line-height))
                   (frame-char-height) 16))
           (char-w (max 1 (frame-char-width)))
           (bar-w (max 3 (round (* char-w 0.18))))
           (color (or (disco-ui--face-foreground-color face)
                      (face-foreground 'default nil t)
                      "gray"))
           (key (list color xh char-w)))
      (or (gethash key disco-ui--vbar-image-cache)
          (let* ((svg (svg-create char-w xh))
                 (_ (svg-rectangle svg 0 0 bar-w xh
                                   :fill-opacity 1 :fill color))
                 (data (with-temp-buffer (svg-print svg) (buffer-string)))
                 (image (create-image data 'svg t
                                      :scale 1.0
                                      :width char-w :height xh
                                      :ascent 'center)))
            (when image
              (puthash key image disco-ui--vbar-image-cache))
            image)))))

(defun disco-ui--vbar-string (face)
  "One-column vertical bar string coloured with FACE.
GUI frames get an SVG image; terminal frames get a plain ▏ character."
  (let ((image (disco-ui--create-vbar-svg face)))
    (if image
        (propertize " " 'display image 'rear-nonsticky '(display))
      (if face (propertize "▏" 'face face) "▏"))))

;;; ── Buttons & styled lines ──────────────────────────────────────────

(cl-defun disco-ui-insert-action-button (label action
                                               &key face help-echo properties)
  "Insert clickable button LABEL calling ACTION.

ACTION is a no-arg function. FACE, HELP-ECHO and PROPERTIES customize button
text properties passed to `insert-text-button'."
  (let ((button-props
         (append
          (list 'follow-link t
                'action (lambda (_button)
                          (funcall action)))
          (when help-echo
            (list 'help-echo help-echo))
          (when face
            (list 'face face))
          properties)))
    (apply #'insert-text-button label button-props)))

(cl-defun disco-ui-insert-styled-line (text &key face properties)
  "Insert one TEXT line and apply FACE/PROPERTIES to the inserted span.

Return inserted span as (START . END)."
  (let ((start (point)))
    (insert (or text "") "\n")
    (when (or face properties)
      (add-text-properties
       start (point)
       (append properties
               (when face (list 'face face)))))
    (cons start (point))))

;;; ── Prefix state machinery ─────────────────────────────────────────

(defconst disco-ui--prefix-state-tag 'disco-ui-prefix-state
  "Internal marker used to identify line-prefix state objects.")

(defun disco-ui-prefix-state-p (value)
  "Return non-nil when VALUE is a `disco-ui' line-prefix state object."
  (and (vectorp value)
       (= (length value) 4)
       (eq (aref value 0) disco-ui--prefix-state-tag)))

(defun disco-ui-make-prefix-state (first-prefix rest-prefix)
  "Return mutable line-prefix state with FIRST-PREFIX and REST-PREFIX."
  (vector disco-ui--prefix-state-tag first-prefix rest-prefix nil))

(defun disco-ui-prefix-state-current (state)
  "Return current prefix string for prefix STATE without consuming it."
  (when (disco-ui-prefix-state-p state)
    (if (aref state 3)
        (aref state 2)
      (or (aref state 1) (aref state 2)))))

(defun disco-ui-prefix-state-rest (state)
  "Return rest-prefix string from prefix STATE."
  (when (disco-ui-prefix-state-p state)
    (aref state 2)))

(defun disco-ui-prefix-state-consume (state)
  "Return current prefix from STATE and mark first-prefix as consumed."
  (when (disco-ui-prefix-state-p state)
    (let ((prefix (disco-ui-prefix-state-current state)))
      (aset state 3 t)
      prefix)))

(defun disco-ui-prefix-string (prefix &optional consume default)
  "Return normalized prefix string from PREFIX source.

When PREFIX is state and CONSUME is non-nil, consume its first-prefix.
DEFAULT is used when PREFIX yields nil."
  (or (cond
       ((disco-ui-prefix-state-p prefix)
        (if consume
            (disco-ui-prefix-state-consume prefix)
          (disco-ui-prefix-state-current prefix)))
       ((stringp prefix) prefix)
       (t nil))
      (or default "")))

;;; ── Card line-prefix helpers ───────────────────────────────────────

(defun disco-ui-combine-faces (&rest faces)
  "Return one face value from FACES, dropping nil entries."
  (let ((values (delq nil faces)))
    (cond
     ((null values) nil)
     ((null (cdr values)) (car values))
     (t values))))

(defvar disco-ui-card-indent-prefix "    "
  "Dynamic base indent prefix used by `disco-ui-card-line-prefix'.")

(defvar disco-ui-card-indent-prefix-state nil
  "Dynamic line-prefix state used by card renderers in current insertion scope.")

(cl-defun disco-ui-card-line-prefix (&key face (indent disco-ui-card-indent-prefix))
  "Return a display-only card prefix string.

FACE colours the vertical bar marker; INDENT is kept plain.  In GUI frames
the marker is an SVG image that fills the full line height so consecutive
lines produce a seamless vertical bar.  The marker replaces INDENT's last
column so card content stays column-aligned with normal lines."
  (let* ((base (or indent ""))
         (mark (disco-ui--vbar-string face))
         (base-len (length base)))
    (if (> base-len 0)
        (concat (substring base 0 (1- base-len)) mark)
      mark)))

(cl-defun disco-ui-card-prefix-state (&key face indent)
  "Return card line-prefix state for current insertion scope.

When `disco-ui-card-indent-prefix-state' is bound to a prefix-state, this
function consumes its first prefix for the card's first row and uses its rest
prefix for subsequent card rows."
  (let* ((line-state disco-ui-card-indent-prefix-state)
         (default-indent (or indent disco-ui-card-indent-prefix))
         (first-indent (disco-ui-prefix-string line-state t default-indent))
         (rest-indent (disco-ui-prefix-string line-state nil default-indent)))
    (disco-ui-make-prefix-state
     (disco-ui-card-line-prefix :face face :indent first-indent)
     (disco-ui-card-line-prefix :face face :indent rest-indent))))

;;; ── Line prefix application ────────────────────────────────────────

(defun disco-ui--apply-line-prefix-span (start end line-prefix-str &optional wrap-prefix-str)
  "Apply line/wrap prefix strings to START..END span.

LINE-PREFIX-STR is prepended to existing `line-prefix'. WRAP-PREFIX-STR
defaults to LINE-PREFIX-STR and is prepended to existing `wrap-prefix'."
  (when (< start end)
    (let* ((line (or line-prefix-str ""))
           (wrap (or wrap-prefix-str line))
           (existing-line (get-text-property start 'line-prefix))
           (existing-wrap (get-text-property start 'wrap-prefix)))
      (add-text-properties
       start end
       (list 'line-prefix (concat line (if (stringp existing-line) existing-line ""))
             'wrap-prefix (concat wrap (if (stringp existing-wrap) existing-wrap "")))))))

(defun disco-ui-apply-line-prefix (start end prefix)
  "Apply PREFIX as display prefix for region START..END.

PREFIX can be a string or a mutable prefix-state created by
`disco-ui-make-prefix-state'."
  (when (< start end)
    (if (disco-ui-prefix-state-p prefix)
        (let* ((pos start)
               (first-prefix (disco-ui-prefix-state-consume prefix))
               (rest-prefix (or (disco-ui-prefix-state-rest prefix)
                                first-prefix))
               (line-prefix first-prefix))
          (save-excursion
            (goto-char start)
            (while (< pos end)
              (goto-char pos)
              (let* ((line-end (line-end-position))
                     (next-pos (if (< line-end end)
                                   (1+ line-end)
                                 end)))
                ;; Telega-like behavior: wrapped continuations use rest-prefix,
                ;; so avatar/image prefix is not repeated on visual wraps.
                (disco-ui--apply-line-prefix-span pos next-pos line-prefix rest-prefix)
                (setq line-prefix rest-prefix)
                (setq pos next-pos)))))
      (disco-ui--apply-line-prefix-span
       start end (disco-ui-prefix-string prefix nil "")))))

;;; ── High-level inserters ───────────────────────────────────────────

(cl-defun disco-ui-insert-prefixed-lines (prefix text &key face properties)
  "Insert TEXT as newline-separated lines using display-only PREFIX.

PREFIX can be a prefix string or prefix-state. FACE and PROPERTIES are applied
per inserted line so copied text stays clean."
  (dolist (line (split-string (or text "") "\n" nil))
    (let ((start (point)))
      (insert line "\n")
      (when (or face properties)
        (add-text-properties
         start
         (point)
         (append properties
                 (when face
                   (list 'face face)))))
      (disco-ui-apply-line-prefix start (point) prefix))))

(defun disco-ui-append-face (start end face)
  "Append FACE to region START..END."
  (when (and face (< start end))
    (add-face-text-property start end face 'append)))

(cl-defun disco-ui-render-list-view (&key title key-hints summary loading-note
                                          items item-inserter empty-text
                                          footer-lines)
  "Render a simple list view block in current buffer.

TITLE, KEY-HINTS, SUMMARY and LOADING-NOTE are optional header lines.
ITEMS are rendered by ITEM-INSERTER when present; otherwise EMPTY-TEXT is
inserted (defaults to `(empty)`). FOOTER-LINES is an optional list of lines
printed after the list with an extra separating blank line."
  (when title
    (insert title "\n"))
  (when key-hints
    (insert key-hints "\n"))
  (when summary
    (insert summary "\n"))
  (when loading-note
    (insert loading-note "\n"))
  (insert "\n")
  (if (and items (functionp item-inserter))
      (dolist (item items)
        (funcall item-inserter item))
    (insert (or empty-text "(empty)") "\n"))
  (when footer-lines
    (insert "\n")
    (dolist (line footer-lines)
      (insert line "\n"))))

(provide 'disco-ui)

;;; disco-ui.el ends here
