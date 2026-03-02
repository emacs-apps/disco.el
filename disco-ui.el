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

(cl-defun disco-ui-insert-prefixed-lines (prefix text &key face properties)
  "Insert TEXT as newline-separated lines using display-only PREFIX.

FACE and PROPERTIES are applied to each inserted line span. PREFIX is set via
`line-prefix' and `wrap-prefix' properties so copied text stays clean."
  (dolist (line (split-string (or text "") "\n" nil))
    (let ((start (point)))
      (insert line "\n")
      (add-text-properties
       start
       (point)
       (append properties
               (when face
                 (list 'face face))
               (list 'line-prefix prefix
                     'wrap-prefix prefix))))))

(defun disco-ui-combine-faces (&rest faces)
  "Return one face value from FACES, dropping nil entries."
  (let ((values (delq nil faces)))
    (cond
     ((null values) nil)
     ((null (cdr values)) (car values))
     (t values))))

(defvar disco-ui-card-indent-prefix "    "
  "Dynamic base indent prefix used by `disco-ui-card-line-prefix'.")

(cl-defun disco-ui-card-line-prefix (&key face (indent disco-ui-card-indent-prefix)
                                          (marker "▏"))
  "Return a display-only card prefix string.

FACE is applied to MARKER while INDENT is kept plain."
  (concat (or indent "")
          (if face
              (propertize marker 'face face)
            marker)))

(defun disco-ui-apply-line-prefix (start end prefix-str)
  "Apply PREFIX-STR as display prefix for region START..END."
  (when (< start end)
    (add-text-properties start end
                         (list 'line-prefix prefix-str
                               'wrap-prefix prefix-str))))

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
