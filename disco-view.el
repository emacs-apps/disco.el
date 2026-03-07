;;; disco-view.el --- Cursor/view preservation helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared utilities for preserving cursor/viewport state around full buffer
;; rerenders, plus reusable one-line row rendering helpers used by root-style
;; list views. This keeps passive timeline/root updates from unexpectedly
;; snapping point to the input/footer while giving multiple views a shared
;; layout component.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-ui)

(cl-defstruct (disco-view--snapshot
               (:constructor disco-view--snapshot-create))
  line
  column
  anchor-property
  anchor-value
  anchor-line-offset
  window-start-line)

(cl-defun disco-view-capture-position (&key anchor-property preserve-window-start)
  "Capture current position context for later restoration.

When ANCHOR-PROPERTY is non-nil, also capture its value at point (or line
beginning), so restore can anchor by semantic row identity.

When PRESERVE-WINDOW-START is non-nil, capture window-start as a 1-based line
index for the current buffer window."
  (let* ((anchor-value (and anchor-property
                            (or (get-text-property (point) anchor-property)
                                (get-text-property (line-beginning-position)
                                                   anchor-property))))
         (anchor-target (and anchor-property
                             anchor-value
                             (text-property-any
                              (point-min)
                              (point-max)
                              anchor-property
                              anchor-value)))
         (anchor-line-offset (and anchor-target
                                  (max 0 (- (line-number-at-pos)
                                            (save-excursion
                                              (goto-char anchor-target)
                                              (line-number-at-pos))))))
         (win (and preserve-window-start
                   (get-buffer-window (current-buffer))))
         (window-start-line (and win
                                 (save-excursion
                                   (goto-char (window-start win))
                                   (line-number-at-pos)))))
    (disco-view--snapshot-create
     :line (line-number-at-pos)
     :column (current-column)
     :anchor-property anchor-property
     :anchor-value anchor-value
     :anchor-line-offset anchor-line-offset
     :window-start-line window-start-line)))

(defun disco-view--anchor-value-at (pos property)
  "Return PROPERTY value at POS, probing previous char when needed."
  (or (get-text-property pos property)
      (and (> pos (point-min))
           (get-text-property (1- pos) property))))

(defun disco-view--move-to-anchor-line-offset (anchor-property anchor-value offset)
  "Move forward up to OFFSET lines while staying within ANCHOR-VALUE row."
  (let ((remaining (max 0 (or offset 0))))
    (while (> remaining 0)
      (let ((next-pos (save-excursion
                        (forward-line 1)
                        (point))))
        (if (or (= next-pos (point))
                (not (equal (disco-view--anchor-value-at next-pos anchor-property)
                            anchor-value)))
            (setq remaining 0)
          (goto-char next-pos)
          (setq remaining (1- remaining)))))))

(defun disco-view-restore-position (snapshot)
  "Restore point/window state from SNAPSHOT.

If SNAPSHOT carries an anchor property/value and the anchor is still present,
restore by anchor first. Otherwise restore by line/column fallback."
  (let* ((anchor-property (disco-view--snapshot-anchor-property snapshot))
         (anchor-value (disco-view--snapshot-anchor-value snapshot))
         (anchor-line-offset (disco-view--snapshot-anchor-line-offset snapshot))
         (line (max 1 (or (disco-view--snapshot-line snapshot) 1)))
         (column (max 0 (or (disco-view--snapshot-column snapshot) 0)))
         (target (and anchor-property
                      anchor-value
                      (text-property-any
                       (point-min)
                       (point-max)
                       anchor-property
                       anchor-value))))
    (if target
        (progn
          (goto-char target)
          (disco-view--move-to-anchor-line-offset
           anchor-property
           anchor-value
           anchor-line-offset))
      (goto-char (point-min))
      (forward-line (1- line)))
    (move-to-column column)
    (let* ((window-start-line (disco-view--snapshot-window-start-line snapshot))
           (win (and (integerp window-start-line)
                     (get-buffer-window (current-buffer)))))
      (when win
        (save-excursion
          (goto-char (point-min))
          (forward-line (max 0 (1- window-start-line)))
          (set-window-start win (point) 'noforce))))))

(cl-defun disco-view-render-preserving-position
    (render-fn &key anchor-property preserve-window-start after-restore)
  "Call RENDER-FN, then restore cursor/viewport context.

RENDER-FN must redraw current buffer. ANCHOR-PROPERTY and
PRESERVE-WINDOW-START are forwarded to `disco-view-capture-position'.
AFTER-RESTORE, when non-nil, is called after point/window restoration."
  (let ((snapshot (disco-view-capture-position
                   :anchor-property anchor-property
                   :preserve-window-start preserve-window-start)))
    (funcall render-fn)
    (when snapshot
      (disco-view-restore-position snapshot))
    (when (functionp after-restore)
      (funcall after-restore))))

(cl-defstruct (disco-view-list-spec
               (:constructor disco-view-list-spec-create))
  title
  key-hints
  summary
  loading-note
  items
  item-inserter
  empty-text
  footer-lines)

(defun disco-view-render-list-spec (spec)
  "Render list SPEC in current buffer using `disco-ui-render-list-view'."
  (disco-ui-render-list-view
   :title (disco-view-list-spec-title spec)
   :key-hints (disco-view-list-spec-key-hints spec)
   :summary (disco-view-list-spec-summary spec)
   :loading-note (disco-view-list-spec-loading-note spec)
   :items (disco-view-list-spec-items spec)
   :item-inserter (disco-view-list-spec-item-inserter spec)
   :empty-text (disco-view-list-spec-empty-text spec)
   :footer-lines (disco-view-list-spec-footer-lines spec)))

(cl-defun disco-view-render-list-spec-preserving-position
    (spec &key anchor-property preserve-window-start after-restore)
  "Render list SPEC and restore cursor/viewport context."
  (disco-view-render-preserving-position
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (disco-view-render-list-spec spec)
       (goto-char (point-min))))
   :anchor-property anchor-property
   :preserve-window-start preserve-window-start
   :after-restore after-restore))

(cl-defun disco-view-insert-heading-line
    (text &key face line-properties help-echo mouse-face)
  "Insert heading TEXT as one styled line."
  (let ((start (point)))
    (insert (or text "") "\n")
    (add-text-properties
     start
     (point)
     (append (or line-properties '())
             (when face
               (list 'face face))
             (when help-echo
               (list 'help-echo help-echo))
             (when mouse-face
               (list 'mouse-face mouse-face))))))

(cl-defun disco-view-insert-note-line
    (text &key face line-properties help-echo mouse-face)
  "Insert note TEXT as one styled line."
  (disco-view-insert-heading-line
   text
   :face (or face 'shadow)
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defun disco-view-insert-action-line
    (label &key prefix suffix face line-properties help-echo mouse-face)
  "Insert clickable action LABEL as one styled line."
  (disco-view-insert-heading-line
   (format "%s%s%s"
           (or prefix "  [")
           (or label "")
           (or suffix "]"))
   :face (or face 'link)
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face (or mouse-face 'highlight)))

(cl-defstruct (disco-view-one-line-row
               (:constructor disco-view-one-line-row-create))
  icon-inserter
  context
  preview
  preview-leading-length
  preview-leading-face
  time
  time-face
  time-tail-face
  line-properties
  help-echo)

(defun disco-view-canonicalize-number (spec base)
  "Resolve SPEC against BASE columns.

SPEC can be an integer, float ratio, or list (VALUE MIN MAX)."
  (let* ((raw (if (consp spec) (car spec) spec))
         (min-value (when (consp spec) (nth 1 spec)))
         (max-value (when (consp spec) (nth 2 spec)))
         (value (cond
                 ((integerp raw) raw)
                 ((floatp raw) (round (* raw base)))
                 ((numberp raw) (round raw))
                 (t base))))
    (when (numberp min-value)
      (setq value (max value min-value)))
    (when (numberp max-value)
      (setq value (min value max-value)))
    value))

(defun disco-view-truncate-fill (text width &optional right-align)
  "Return TEXT truncated and padded to WIDTH.

When RIGHT-ALIGN is non-nil, pad on the left instead of right."
  (let* ((target (max 0 (or width 0)))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (padding (max 0 (- target (string-width trimmed)))))
    (if right-align
        (concat (make-string padding ?\s) trimmed)
      (concat trimmed (make-string padding ?\s)))))

(defun disco-view-elide-string (str max &optional face)
  "Return STR visually elided to MAX columns using display properties."
  (let* ((text (or str ""))
         (str-width (string-width text))
         (limit (max 0 (or max 0))))
    (if (<= str-width limit)
        text
      (let* ((elide-str "…")
             (elide-width (string-width elide-str))
             (elide-pos 1)
             (str-len (length text))
             (elide-trail (floor (* limit (- 1 elide-pos))))
             (trail-width
              (progn
                (while (and (> elide-trail 0)
                            (> (string-width text (- str-len elide-trail))
                               (floor (* limit (- 1 elide-pos)))))
                  (setq elide-trail (1- elide-trail)))
                (string-width text (- str-len elide-trail))))
             (elide-lead (- (min limit str-len) elide-width trail-width))
             (result (copy-sequence text)))
        (when (< elide-lead 0)
          (setq elide-lead 0))
        (while (and (> elide-lead 0)
                    (> (+ (string-width result 0 elide-lead)
                          elide-width trail-width)
                       limit))
          (setq elide-lead (1- elide-lead)))
        (add-text-properties
         elide-lead
         (- str-len elide-trail)
         (list 'display elide-str
               'rear-nonsticky '(display)
               'face face)
         result)
        result))))

(defun disco-view--chars-xwidth (columns &optional window)
  "Return pixel width for COLUMNS using WINDOW metrics."
  (let* ((win (or window (get-buffer-window (current-buffer) t)))
         (frame (and (window-live-p win)
                     (window-frame win)))
         (char-width
          (or (and (frame-live-p frame)
                   (let* ((font (ignore-errors (face-font 'default frame)))
                          (info (and font (ignore-errors (font-info font frame)))))
                     (when info
                       (let ((width (aref info 11)))
                         (if (> width 0)
                             width
                           (aref info 10))))))
              (and (frame-live-p frame)
                   (frame-char-width frame))
              (frame-char-width)
              1)))
    (* (max 0 columns) (max 1 char-width))))

(defun disco-view-current-column ()
  "Like `current-column', but account for prior `:align-to' spacers."
  (let* ((bol (line-beginning-position))
         (point-now (point))
         (scan point-now)
         align-column)
    (while (and (not align-column)
                (> scan bol)
                (setq scan (previous-single-char-property-change
                            scan 'display nil bol)))
      (let ((display (get-text-property scan 'display)))
        (when (and (listp display)
                   (> (length display) 2)
                   (eq (nth 0 display) 'space)
                   (eq (nth 1 display) :align-to))
          (let ((align-val (nth 2 display)))
            (setq align-column
                  (+ (if (listp align-val)
                         (ceiling (/ (or (car align-val) 0)
                                     (float (max 1 (disco-view--chars-xwidth 1)))))
                       (or align-val 0))
                     (string-width (buffer-substring scan point-now))))))))
    (or align-column (current-column))))

(defun disco-view-move-to-column (column)
  "Insert one forward-only align-to spacer for COLUMN."
  (let* ((target (max 0 (or column 0)))
         (current (disco-view-current-column)))
    (when (>= target current)
      (let ((align-to (if (display-graphic-p)
                          (list (disco-view--chars-xwidth target))
                        target)))
        (insert (propertize " " 'display `(space :align-to ,align-to)))))))

(defun disco-view-one-line-column-widths (content-width context-width-spec)
  "Return one-line context/preview width split for CONTENT-WIDTH."
  (let* ((max-context-inner (max 8 (- content-width 3)))
         (context-inner-width
          (max 8
               (min max-context-inner
                    (disco-view-canonicalize-number context-width-spec
                                                   content-width))))
         (preview-width (max 0 (- content-width context-inner-width 3))))
    (list :context-inner-width context-inner-width
          :preview-width preview-width
          :separator-width (if (> preview-width 0) 1 0))))

(cl-defun disco-view-insert-one-line-row (row &key indent width
                                               icon-slot-width context-width-spec)
  "Insert ROW using one-line activity-style layout.

ROW is a `disco-view-one-line-row' object. INDENT is left padding in spaces.
WIDTH sets the total row width. ICON-SLOT-WIDTH reserves columns for the
icon slot. CONTEXT-WIDTH-SPEC controls context width using
`disco-view-canonicalize-number' semantics."
  (let* ((padding (make-string (max 0 (or indent 0)) ?\s))
         (context-text (or (disco-view-one-line-row-context row) ""))
         (preview-text (or (disco-view-one-line-row-preview row) ""))
         (time-text (or (disco-view-one-line-row-time row) ""))
         (time-width (if (string-empty-p time-text)
                         0
                       (max 6 (string-width time-text))))
         (line-start (point)))
    (insert padding)
    (let* ((icon-start (disco-view-current-column))
           (slot-width (max 2 (or icon-slot-width 2)))
           (slot-target (max icon-start
                             (1- (+ icon-start slot-width)))))
      (when-let* ((icon-inserter (disco-view-one-line-row-icon-inserter row)))
        (funcall icon-inserter))
      (disco-view-move-to-column slot-target)
      (insert " "))
    (let* ((content-start (disco-view-current-column))
           (time-gap (if (> time-width 0) 1 0))
           (content-width (max 20 (- (max 20 (or width 20))
                                     content-start
                                     time-width
                                     time-gap)))
           (widths (disco-view-one-line-column-widths
                    content-width
                    (or context-width-spec '(0.45 20))))
           (context-inner-width (or (plist-get widths :context-inner-width) 8))
           (preview-width (or (plist-get widths :preview-width) 0))
           (separator-width (or (plist-get widths :separator-width) 0)))
      (let ((context-start (disco-view-current-column)))
        (insert "[")
        (insert (disco-view-elide-string context-text context-inner-width 'default))
        (disco-view-move-to-column (+ context-start 1 context-inner-width))
        (insert "]"))
      (when (> preview-width 0)
        (when (> separator-width 0)
          (insert " "))
        (let ((preview-start (point)))
          (insert (disco-view-elide-string preview-text preview-width 'shadow))
          (add-text-properties preview-start (point) (list 'face 'shadow))
          (let ((leading-length (disco-view-one-line-row-preview-leading-length row))
                (leading-face (disco-view-one-line-row-preview-leading-face row)))
            (when (and (integerp leading-length)
                       (> leading-length 0)
                       leading-face)
              (add-text-properties preview-start
                                   (min (point) (+ preview-start leading-length))
                                   (list 'face leading-face))))))
      (when (> time-width 0)
        (let ((target-time-col (- (max 20 (or width 20)) time-width)))
          (disco-view-move-to-column target-time-col)
          (let* ((time-start (point))
                 (time-face (or (disco-view-one-line-row-time-face row) 'shadow))
                 (tail-face (disco-view-one-line-row-time-tail-face row)))
            (insert (disco-view-truncate-fill time-text time-width t))
            (if (and tail-face (> (point) time-start))
                (let ((tail-start (max time-start (1- (point)))))
                  (when (< time-start tail-start)
                    (add-text-properties time-start tail-start (list 'face time-face)))
                  (add-text-properties tail-start (point) (list 'face tail-face)))
              (add-text-properties time-start (point) (list 'face time-face))))))
    (insert "\n")
    (add-text-properties
     line-start
     (point)
     (append (or (disco-view-one-line-row-line-properties row) '())
             (when-let* ((help-echo (disco-view-one-line-row-help-echo row)))
               (list 'mouse-face 'highlight
                     'help-echo help-echo)))))))

(provide 'disco-view)

;;; disco-view.el ends here
