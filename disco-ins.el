;;; disco-ins.el --- Shared insert/render leaf helpers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared insertion helpers for chat-like renderers.  This module is the
;; owner for small render leaves and formatting primitives; room/root EWOC and
;; timeline orchestration should stay with their UI facades.

;;; Code:

(require 'cl-lib)
(require 'disco-msg)
(require 'disco-ui)

(defun disco-ins-prefix-string (prefix &optional consume default)
  "Return normalized prefix string from PREFIX.

When PREFIX is a mutable prefix-state and CONSUME is non-nil, consume its
first-prefix.  DEFAULT falls back to the empty string when omitted."
  (disco-ui-prefix-string prefix consume (or default "")))

(defun disco-ins-insert-prefixed-lines (prefix text &optional face)
  "Insert TEXT as newline-separated lines, each prefixed by PREFIX.

When FACE is non-nil, apply FACE to each inserted line."
  (disco-ui-insert-prefixed-lines prefix text :face face))

(defun disco-ins-insert-full-width-divider (label face target-width
                                                  &optional properties)
  "Insert a centered divider for LABEL spanning TARGET-WIDTH columns.

FACE is applied to the entire inserted span.  PROPERTIES is an optional plist
of additional text properties.  Return the inserted span as (START . END)."
  (let* ((open "( ")
         (close " )")
         (inner-width (+ (string-width open)
                         (string-width label)
                         (string-width close)))
         (fill-col (max 0 (or target-width 0)))
         (total-bar (max 4 (- fill-col inner-width)))
         (left-bars (/ total-bar 2))
         (right-bars (- total-bar left-bars))
         (start (point)))
    (insert (make-string left-bars ?─)
            open label close
            (make-string right-bars ?─)
            "\n")
    (add-face-text-property start (point) face t)
    (when properties
      (add-text-properties start (point) properties))
    (cons start (point))))

(defun disco-ins-insert-divider-row (text face target-width &optional properties)
  "Insert read-only divider row TEXT using FACE across TARGET-WIDTH.

PROPERTIES is appended before the standard read-only divider properties.
Return the inserted span as (START . END)."
  (disco-ins-insert-full-width-divider
   text face target-width
   (append properties
           '(read-only t
             front-sticky (read-only)
             rear-nonsticky (read-only)))))

(cl-defun disco-ins-insert-reference-line (body &key prefix face button-label
                                                     button-action button-face
                                                     properties)
  "Insert one prefixed reference line with optional BODY and action button.

PREFIX is applied with `disco-ui-apply-line-prefix'.  FACE and PROPERTIES are
applied to the inserted span.  When BUTTON-LABEL is non-nil, BUTTON-ACTION is
inserted using `disco-ui-insert-action-button'.  Return the inserted span as
(START . END)."
  (let ((line-start (point))
        (text (and (stringp body) (not (string-empty-p body)) body)))
    (insert "↪")
    (when (or text button-label)
      (insert " "))
    (when text
      (insert text))
    (when button-label
      (when text
        (insert " "))
      (if (functionp button-action)
          (disco-ui-insert-action-button
           button-label button-action :face button-face)
        (insert (if button-face
                    (propertize button-label 'face button-face)
                  button-label))))
    (insert "\n")
    (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
    (when (or face properties)
      (add-text-properties
       line-start (point)
       (append properties
               (when face (list 'face face)))))
    (cons line-start (point))))

(cl-defun disco-ins-insert-reaction-line (reactions &key prefix selected-face
                                                    unselected-face line-face)
  "Insert one reaction chip line for REACTIONS.

PREFIX is applied with `disco-ui-apply-line-prefix'.  SELECTED-FACE and
UNSELECTED-FACE style each reaction chip.  LINE-FACE is applied to the whole
inserted span.  Return the inserted span as (START . END), or nil when
REACTIONS is empty."
  (when reactions
    (let ((line-start (point))
          (first t))
      (dolist (reaction reactions)
        (unless first
          (insert " "))
        (setq first nil)
        (let ((chip (format "[%s %s]"
                            (disco-msg-reaction-emoji reaction)
                            (disco-msg-reaction-count reaction))))
          (insert (propertize chip
                              'face (if (disco-msg-reaction-selected-p reaction)
                                        selected-face
                                      unselected-face)))))
      (insert "\n")
      (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
      (when line-face
        (add-face-text-property line-start (point) line-face nil))
      (cons line-start (point)))))

(cl-defun disco-ins-insert-attachment-lines (summary &key prefix url
                                                      summary-face url-face)
  "Insert plain attachment SUMMARY and optional URL lines.

PREFIX is applied with `disco-ui-apply-line-prefix'.  SUMMARY-FACE styles the
summary line, and URL-FACE styles the optional URL line.  Return the inserted
span as (START . END), or nil when SUMMARY is empty."
  (when (and (stringp summary) (not (string-empty-p summary)))
    (let ((start (point)))
      (let ((summary-start (point)))
        (insert summary "\n")
        (disco-ui-apply-line-prefix summary-start (point) (or prefix "    "))
        (when summary-face
          (add-text-properties summary-start (point) (list 'face summary-face))))
      (when (and (stringp url) (not (string-empty-p url)))
        (let ((url-start (point)))
          (insert "  " url "\n")
          (disco-ui-apply-line-prefix url-start (point) (or prefix "    "))
          (when url-face
            (add-text-properties url-start (point) (list 'face url-face)))))
      (cons start (point)))))

(cl-defun disco-ins-insert-forward-card (&key source-text sent-at content
                                               insert-source-icon title-label
                                               jump-label jump-action
                                               jump-face jump-help-echo
                                               border-face title-face meta-face)
  "Insert one forwarded-message card.

SOURCE-TEXT is the rendered source label line body.  SENT-AT and CONTENT are
optional metadata/body strings.  INSERT-SOURCE-ICON, when non-nil, is called to
insert an inline source icon before SOURCE-TEXT.  JUMP-LABEL and JUMP-ACTION
configure an optional action button row.  BORDER-FACE, TITLE-FACE, and
META-FACE control the card styling.  Return the inserted span as (START . END)."
  (let ((card-start (point))
        (prefix-state (disco-ui-card-prefix-state :face border-face)))
    (let ((title-start (point)))
      (insert (or title-label "[forwarded message]") "\n")
      (disco-ui-apply-line-prefix title-start (point) prefix-state)
      (when title-face
        (disco-ui-append-face title-start (point) title-face)))
    (let ((source-start (point)))
      (insert "source: ")
      (when (functionp insert-source-icon)
        (funcall insert-source-icon)
        (insert " "))
      (insert (or source-text "unknown"))
      (insert "\n")
      (disco-ui-apply-line-prefix source-start (point) prefix-state)
      (when meta-face
        (disco-ui-append-face source-start (point) meta-face)))
    (when (and (stringp sent-at) (not (string-empty-p sent-at)))
      (let ((time-start (point)))
        (insert "sent: " sent-at "\n")
        (disco-ui-apply-line-prefix time-start (point) prefix-state)
        (when meta-face
          (disco-ui-append-face time-start (point) meta-face))))
    (when (and (stringp content) (not (string-empty-p content)))
      (let ((content-start (point)))
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        (disco-ui-apply-line-prefix content-start (point) prefix-state)))
    (when jump-label
      (let ((action-start (point)))
        (if (functionp jump-action)
            (disco-ui-insert-action-button
             jump-label jump-action
             :face jump-face
             :help-echo jump-help-echo)
          (insert (if jump-face
                      (propertize jump-label 'face jump-face)
                    jump-label)))
        (insert "\n")
        (disco-ui-apply-line-prefix action-start (point) prefix-state)
        (when meta-face
          (disco-ui-append-face action-start (point) meta-face))))
    (cons card-start (point))))

(provide 'disco-ins)

;;; disco-ins.el ends here
