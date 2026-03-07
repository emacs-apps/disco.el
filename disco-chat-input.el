;;; disco-chat-input.el --- Shared chat input helpers for disco-like buffers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Generic editable-footer helpers shared by disco-room and other chat-like
;; buffers such as emacs-qq.  This mirrors the separation telega uses between
;; prompt/input management and the rest of chat rendering logic.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defun disco-chat-input-configure-map (map bindings &key parent)
  "Reset MAP and apply BINDINGS over PARENT.

BINDINGS is a list of cons cells in the form (KEY . COMMAND), where KEY is a
string accepted by `kbd'.  PARENT defaults to `current-global-map'."
  (setcdr map nil)
  (set-keymap-parent map (or parent (current-global-map)))
  (dolist (binding bindings)
    (define-key map (kbd (car binding)) (cdr binding)))
  map)

(cl-defun disco-chat-input-compose-text
    (&key visible-p draft context-text typing-text typing-face
          (prompt ">>> ") prompt-property input-property leading-newline)
  "Return prompt/footer text for a chat composer.

When VISIBLE-P is nil, return an empty string.  DRAFT is the current draft
text.  CONTEXT-TEXT and TYPING-TEXT are inserted above the prompt when
non-empty.  PROMPT-PROPERTY and INPUT-PROPERTY are the marker properties used
later by `disco-chat-input-bind-region-from-properties'.  When LEADING-NEWLINE
is non-nil, insert one newline before the footer block."
  (if (not visible-p)
      ""
    (let ((input (if (string-empty-p (or draft ""))
                     "\n"
                   draft)))
      (concat
       (if leading-newline "\n" "")
       (if (string-empty-p (or context-text ""))
           ""
         (propertize context-text
                     'read-only t
                     'front-sticky '(read-only)
                     'rear-nonsticky '(read-only)))
       (if (string-empty-p (or typing-text ""))
           ""
         (propertize typing-text
                     'read-only t
                     'front-sticky '(read-only)
                     'rear-nonsticky '(read-only)
                     'face typing-face))
       (propertize prompt
                   'read-only t
                   'field prompt-property
                   'cursor-intangible t
                   prompt-property t
                   'front-sticky '(read-only field cursor-intangible)
                   'rear-nonsticky
                   (list 'read-only 'field 'cursor-intangible input-property))
       (propertize input
                   input-property t
                   'read-only nil)))))

(defun disco-chat-input-region-bounds (input-marker)
  "Return writable draft bounds described by INPUT-MARKER, or nil."
  (when (and (markerp input-marker)
             (eq (marker-buffer input-marker) (current-buffer)))
    (let ((start (marker-position input-marker)))
      (when (<= start (point-max))
        (cons start (point-max))))))

(defun disco-chat-input-start-position (input-marker)
  "Return draft input start position for INPUT-MARKER, or nil."
  (car-safe (disco-chat-input-region-bounds input-marker)))

(defun disco-chat-input-prompt-start-position (prompt-marker)
  "Return prompt start position for PROMPT-MARKER, or nil."
  (and (markerp prompt-marker)
       (eq (marker-buffer prompt-marker) (current-buffer))
       (marker-position prompt-marker)))

(defun disco-chat-input-logical-end-position (input-marker)
  "Return logical draft end position for INPUT-MARKER.

A synthetic trailing newline used to keep empty inputs editable is excluded."
  (let ((bounds (disco-chat-input-region-bounds input-marker)))
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (cond
         ((<= end start) start)
         ((eq (char-before end) ?\n) (max start (1- end)))
         (t end))))))

(defun disco-chat-input-point-in-input-p (input-marker &optional position)
  "Return non-nil when POSITION or point is inside INPUT-MARKER region."
  (let* ((bounds (disco-chat-input-region-bounds input-marker))
         (pos (or position (point))))
    (and bounds
         (<= (car bounds) pos)
         (<= pos (cdr bounds)))))

(defun disco-chat-input-point-in-prompt-p (prompt-marker input-marker &optional position)
  "Return non-nil when POSITION or point is inside prompt glyph span."
  (let ((prompt-start (disco-chat-input-prompt-start-position prompt-marker))
        (input-start (disco-chat-input-start-position input-marker))
        (pos (or position (point))))
    (and (number-or-marker-p prompt-start)
         (number-or-marker-p input-start)
         (>= pos prompt-start)
         (< pos input-start))))

(defun disco-chat-input-apply-text-properties (input-marker input-map)
  "Ensure INPUT-MARKER region stays editable and uses INPUT-MAP."
  (when-let* ((bounds (disco-chat-input-region-bounds input-marker)))
    (with-silent-modifications
      (add-text-properties
       (car bounds) (cdr bounds)
       (list 'read-only nil
             'local-map input-map
             'rear-nonsticky '(read-only local-map))))))

(defun disco-chat-input-bind-region-from-properties
    (input-property prompt-property input-marker prompt-marker)
  "Locate composer region using INPUT-PROPERTY and PROMPT-PROPERTY.

INPUT-MARKER and PROMPT-MARKER are updated in the current buffer.  Return
non-nil when an input region is found."
  (let ((input-start (text-property-any (point-min) (point-max)
                                        input-property t)))
    (when input-start
      (let* ((probe-start (max (point-min) (- input-start 32)))
             (prompt-start (or (text-property-any probe-start input-start
                                                  prompt-property t)
                               (max (point-min) (1- input-start)))))
        (set-marker prompt-marker prompt-start (current-buffer))
        (set-marker input-marker input-start (current-buffer))
        t))))

(defun disco-chat-input-current-string (input-marker)
  "Return current draft text from INPUT-MARKER region, or nil."
  (when-let* ((bounds (disco-chat-input-region-bounds input-marker)))
    (replace-regexp-in-string
     "[\n\r]+\\'"
     ""
     (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(cl-defun disco-chat-input-after-change
    (beg end &key rendering-p input-marker input-map sync-function)
  "Handle editable-region changes between BEG and END.

When RENDERING-P is non-nil, do nothing.  Otherwise re-apply editable text
properties to INPUT-MARKER region and call SYNC-FUNCTION when change overlaps
that region."
  (unless rendering-p
    (when-let* ((bounds (disco-chat-input-region-bounds input-marker)))
      (when (and (< beg (cdr bounds))
                 (> end (car bounds)))
        (disco-chat-input-apply-text-properties input-marker input-map)
        (when (functionp sync-function)
          (funcall sync-function))))))

(defun disco-chat-input-post-command-clamp-point (input-marker prompt-marker)
  "Keep point out of prompt glyphs and synthetic newline area."
  (when (disco-chat-input-point-in-prompt-p prompt-marker input-marker)
    (when-let* ((input-start (disco-chat-input-start-position input-marker)))
      (goto-char input-start)))
  (when-let* ((logical-end (disco-chat-input-logical-end-position input-marker)))
    (when (and (disco-chat-input-point-in-input-p input-marker)
               (> (point) logical-end))
      (goto-char logical-end))))

(provide 'disco-chat-input)

;;; disco-chat-input.el ends here
