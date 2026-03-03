;;; disco-ui.el --- Shared UI rendering primitives for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Small shared insertion helpers used by room/root renderers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)

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
       start
       (point)
       (append properties
               (when face
                 (list 'face face)))))
    (cons start (point))))

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

(cl-defun disco-ui-card-line-prefix (&key face (indent disco-ui-card-indent-prefix)
                                          (marker "▏"))
  "Return a display-only card prefix string.

FACE is applied to MARKER while INDENT is kept plain. MARKER replaces
INDENT's last column so card content stays column-aligned with normal lines."
  (let* ((base (or indent ""))
         (mark (if face
                   (propertize marker 'face face)
                 marker))
         (base-len (length base)))
    (if (> base-len 0)
        (concat (substring base 0 (1- base-len)) mark)
      mark)))

(cl-defun disco-ui-card-prefix-state (&key face (marker "▏") indent)
  "Return card line-prefix state for current insertion scope.

When `disco-ui-card-indent-prefix-state' is bound to a prefix-state, this
function consumes its first prefix for the card's first row and uses its rest
prefix for subsequent card rows."
  (let* ((line-state disco-ui-card-indent-prefix-state)
         (default-indent (or indent disco-ui-card-indent-prefix))
         (first-indent (disco-ui-prefix-string line-state t default-indent))
         (rest-indent (disco-ui-prefix-string line-state nil default-indent)))
    (disco-ui-make-prefix-state
     (disco-ui-card-line-prefix :face face :indent first-indent :marker marker)
     (disco-ui-card-line-prefix :face face :indent rest-indent :marker marker))))

(defun disco-ui--apply-line-prefix-span (start end line-prefix-str &optional wrap-prefix-str)
  "Apply line/wrap prefix strings to START..END span.

LINE-PREFIX-STR is used for `line-prefix'. WRAP-PREFIX-STR defaults to
LINE-PREFIX-STR when omitted."
  (when (< start end)
    (add-text-properties
     start end
     (list 'line-prefix line-prefix-str
           'wrap-prefix (or wrap-prefix-str line-prefix-str)))))

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
