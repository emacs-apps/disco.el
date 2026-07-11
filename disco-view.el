;;; disco-view.el --- Cursor/view preservation helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared utilities for preserving cursor/viewport state, reconciling
;; stable-keyed EWOC lists, and rendering one-line rows used by root-style
;; views.  This keeps passive updates from snapping point or rebuilding
;; unchanged rows while giving multiple clients a shared layout component.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'disco-ui)

(defun disco-view--key-set (keys)
  "Return an equal-tested set containing KEYS."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (key keys set)
      (puthash key t set))))

(defun disco-view--keyed-ewoc-nodes (ewoc key-function)
  "Return a stable-key to node table for EWOC using KEY-FUNCTION.

Every entry must have a non-nil, unique key.  Violations are programming
errors because incremental reconciliation cannot identify such rows."
  (let ((nodes (make-hash-table :test #'equal))
        (node (ewoc-nth ewoc 0)))
    (while node
      (let ((key (funcall key-function (ewoc-data node))))
        (unless key
          (error "Disco view: EWOC entry has no stable key"))
        (when (gethash key nodes)
          (error "Disco view: duplicate EWOC key %S" key))
        (puthash key node nodes))
      (setq node (ewoc-next ewoc node)))
    nodes))

(defun disco-view--validate-keyed-entries (entries key-function)
  "Require KEY-FUNCTION to return one unique stable key for each of ENTRIES."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((key (funcall key-function entry)))
        (unless key
          (error "Disco view: projected entry has no stable key"))
        (when (gethash key seen)
          (error "Disco view: duplicate projected key %S" key))
        (puthash key t seen)))))

(cl-defun disco-view-reconcile-keyed-ewoc
    (ewoc entries key-function &key force-keys)
  "Reconcile keyed EWOC with ENTRIES and return its new node table.

KEY-FUNCTION returns a non-nil stable key for every entry.  Existing nodes in
the right position retain their identity; changed data is invalidated in
place, moved rows are reinserted at the requested position, vanished rows are
deleted, and new rows are inserted.  FORCE-KEYS invalidates matching rows even
when their entry data compares equal, for presentation-only changes such as a
newly cached image."
  (disco-view--validate-keyed-entries entries key-function)
  (let* ((available (disco-view--keyed-ewoc-nodes ewoc key-function))
         (target-keys (disco-view--key-set
                       (mapcar key-function entries)))
         stale-keys
         (next-node nil)
        (new-table (make-hash-table :test #'equal))
        (forced (disco-view--key-set force-keys)))
    ;; Remove vanished rows before positional reconciliation.  Otherwise a
    ;; surviving row after a deleted prefix appears to be a move and loses its
    ;; EWOC node identity unnecessarily.
    (maphash (lambda (key _node)
               (unless (gethash key target-keys)
                 (push key stale-keys)))
             available)
    (dolist (key stale-keys)
      (ewoc-delete ewoc (gethash key available))
      (remhash key available))
    (setq next-node (ewoc-nth ewoc 0))
    (dolist (entry entries)
      (let* ((key (funcall key-function entry))
             (existing (and key (gethash key available)))
             node)
        (cond
         ((and existing (eq existing next-node))
          (setq node existing
                next-node (ewoc-next ewoc next-node))
          (when (or (gethash key forced)
                    (not (equal (ewoc-data node) entry)))
            (ewoc-set-data node entry)
            (ewoc-invalidate ewoc node)))
         (existing
          (ewoc-delete ewoc existing)
          (setq node (if next-node
                         (ewoc-enter-before ewoc next-node entry)
                       (ewoc-enter-last ewoc entry))))
         (t
          (setq node (if next-node
                         (ewoc-enter-before ewoc next-node entry)
                       (ewoc-enter-last ewoc entry)))))
        (remhash key available)
        (puthash key node new-table)))
    (maphash (lambda (_key node) (ewoc-delete ewoc node)) available)
    new-table))

(defun disco-view-invalidate-keyed-ewoc-node (ewoc node-table key)
  "Invalidate KEY in EWOC using NODE-TABLE.

Return non-nil when the keyed node exists."
  (when-let* ((node (and (hash-table-p node-table)
                         (gethash key node-table))))
    (ewoc-invalidate ewoc node)
    t))

(cl-defstruct (disco-view--snapshot
               (:constructor disco-view--snapshot-create))
  line
  column
  anchor-property
  anchor-value
  anchor-line-offset
  window-start-line)

(defun disco-view-find-property-value (start end property value)
  "Return first position from START to END whose PROPERTY equals VALUE.

Unlike `text-property-any', comparison uses `equal'.  Chat message identifiers
are commonly distinct string objects with the same contents, so identity
comparison is not a valid lookup rule."
  (let ((position start)
        found)
    (while (and (< position end) (not found))
      (if (equal (get-text-property position property) value)
          (setq found position)
        (setq position
              (next-single-property-change position property nil end))))
    found))

(cl-defun disco-view-capture-position (&key anchor-property preserve-window-start)
  "Capture current position context for later restoration.

When ANCHOR-PROPERTY is non-nil, also capture its value at point (or line
beginning), so restore can anchor by semantic row identity.

When PRESERVE-WINDOW-START is non-nil, capture `window-start' as a 1-based line
index for the current buffer window."
  (let* ((anchor-value (and anchor-property
                            (or (get-text-property (point) anchor-property)
                                (get-text-property (line-beginning-position)
                                                   anchor-property))))
         (anchor-target (and anchor-property
                             anchor-value
                             (disco-view-find-property-value
                              (point-min) (point-max)
                              anchor-property anchor-value)))
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
  "Move OFFSET lines within the ANCHOR-PROPERTY row named ANCHOR-VALUE."
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

(defun disco-view-restore-position (snapshot &optional anchor-value-map)
  "Restore point/window state from SNAPSHOT.

If SNAPSHOT carries an anchor property/value and the anchor is still present,
restore by anchor first.  Otherwise restore by line/column fallback.
ANCHOR-VALUE-MAP is an alist mapping old anchor values to their new identities
after an explicit keyed-row rekey."
  (let* ((anchor-property (disco-view--snapshot-anchor-property snapshot))
         (captured-anchor-value
          (disco-view--snapshot-anchor-value snapshot))
         (anchor-value
          (if-let* ((mapping (assoc captured-anchor-value anchor-value-map)))
              (cdr mapping)
            captured-anchor-value))
         (anchor-line-offset (disco-view--snapshot-anchor-line-offset snapshot))
         (line (max 1 (or (disco-view--snapshot-line snapshot) 1)))
         (column (max 0 (or (disco-view--snapshot-column snapshot) 0)))
         (target (and anchor-property
                      anchor-value
                      (disco-view-find-property-value
                       (point-min) (point-max)
                       anchor-property anchor-value))))
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

RENDER-FN must redraw current buffer.  ANCHOR-PROPERTY and
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
   :summary (disco-view-list-spec-summary spec)
   :loading-note (disco-view-list-spec-loading-note spec)
   :items (disco-view-list-spec-items spec)
   :item-inserter (disco-view-list-spec-item-inserter spec)
   :empty-text (disco-view-list-spec-empty-text spec)
   :footer-lines (disco-view-list-spec-footer-lines spec)))

(cl-defun disco-view-render-list-spec-preserving-position
    (spec &key anchor-property preserve-window-start after-restore)
  "Render list SPEC and restore cursor/viewport context.

ANCHOR-PROPERTY, PRESERVE-WINDOW-START, and AFTER-RESTORE are forwarded to
`disco-view-render-preserving-position'."
  (disco-view-render-preserving-position
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (disco-view-render-list-spec spec)
       (goto-char (point-min))))
   :anchor-property anchor-property
   :preserve-window-start preserve-window-start
   :after-restore after-restore))

(cl-defstruct (disco-view-label-row
               (:constructor disco-view-label-row-create))
  label
  prefix
  suffix
  icon-inserter
  icon-separator
  face
  line-properties
  help-echo
  mouse-face)

(defun disco-view-insert-label-row (row)
  "Insert one simple label ROW."
  (let ((start (point)))
    (when-let* ((prefix (disco-view-label-row-prefix row)))
      (insert prefix))
    (when-let* ((icon-inserter (disco-view-label-row-icon-inserter row)))
      (funcall icon-inserter)
      (when-let* ((icon-separator (disco-view-label-row-icon-separator row)))
        (insert icon-separator)))
    (insert (or (disco-view-label-row-label row) ""))
    (when-let* ((suffix (disco-view-label-row-suffix row)))
      (insert suffix))
    (insert "\n")
    (add-text-properties
     start
     (point)
     (append (or (disco-view-label-row-line-properties row) '())
             (when-let* ((face (disco-view-label-row-face row)))
               (list 'face face))
             (when-let* ((help-echo (disco-view-label-row-help-echo row)))
               (list 'help-echo help-echo))
             (when-let* ((mouse-face (disco-view-label-row-mouse-face row)))
               (list 'mouse-face mouse-face))))))

(cl-defun disco-view-insert-label-line
    (label &key prefix suffix icon-inserter icon-separator
           face line-properties help-echo mouse-face)
  "Insert LABEL as one styled line.

PREFIX, SUFFIX, ICON-INSERTER, ICON-SEPARATOR, FACE, LINE-PROPERTIES,
HELP-ECHO, and MOUSE-FACE customize its presentation and interaction."
  (disco-view-insert-label-row
   (disco-view-label-row-create
    :label label
    :prefix prefix
    :suffix suffix
    :icon-inserter icon-inserter
    :icon-separator icon-separator
    :face face
    :line-properties line-properties
    :help-echo help-echo
    :mouse-face mouse-face)))

(cl-defun disco-view-insert-heading-line
    (text &key face line-properties help-echo mouse-face)
  "Insert heading TEXT using FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE."
  (disco-view-insert-label-line
   text
   :face face
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defun disco-view-insert-note-line
    (text &key face line-properties help-echo mouse-face)
  "Insert note TEXT using FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE."
  (disco-view-insert-heading-line
   text
   :face (or face 'shadow)
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defun disco-view-insert-action-line
    (label &key prefix suffix face line-properties help-echo mouse-face)
  "Insert a clickable action line for LABEL.

PREFIX, SUFFIX, FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE customize it."
  (disco-view-insert-label-line
   label
   :prefix (or prefix "  [")
   :suffix (or suffix "]")
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
  help-echo
  mouse-face)

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
  "Return STR elided to MAX columns using display properties and FACE."
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

(defun disco-view--string-pixel-width (text &optional face)
  "Return graphical pixel width of TEXT rendered with FACE.

Return nil when the current buffer has no graphical display window."
  (when-let* ((window (get-buffer-window (current-buffer) t))
              ((window-live-p window))
              (frame (window-frame window))
              ((display-graphic-p frame))
              ((fboundp 'string-pixel-width)))
    (let ((measured (copy-sequence (or text ""))))
      (when (and face (> (length measured) 0))
        (add-face-text-property 0 (length measured) face t measured))
      (string-pixel-width measured (current-buffer)))))

(defun disco-view--pixel-continuation-char-p (character)
  "Return non-nil when CHARACTER continues the preceding display cluster."
  (and character
       (or (memq (get-char-code-property character 'general-category)
                 '(Mn Mc Me))
           (<= #xFE00 character #xFE0F)
           (<= #xE0100 character #xE01EF)
           (<= #x1F3FB character #x1F3FF)
           (= character #x20E3))))

(defun disco-view--regional-indicator-p (character)
  "Return non-nil when CHARACTER is a regional-indicator symbol."
  (and character (<= #x1F1E6 character #x1F1FF)))

(defun disco-view--safe-elide-boundary (text boundary)
  "Move BOUNDARY left to a safe display-cluster edge in TEXT."
  (let ((position (max 0 (min (length text) boundary)))
        changed)
    (while (and (> position 0) (< position (length text))
                (progn
                  (setq changed nil)
                  (cond
                   ((disco-view--pixel-continuation-char-p
                     (aref text position))
                    (setq position (1- position)
                          changed t))
                   ((= (aref text (1- position)) #x200D)
                    (setq position (1- position)
                          changed t))
                   ((and (disco-view--regional-indicator-p
                          (aref text (1- position)))
                         (disco-view--regional-indicator-p
                          (aref text position)))
                    (setq position (1- position)
                          changed t)))
                  changed)))
    position))

(defun disco-view--elide-string-to-pixels (text pixel-limit face)
  "Return TEXT right-elided within PIXEL-LIMIT using FACE metrics."
  (let* ((ellipsis "…")
         (text-length (length text))
         (low 0)
         (high (max 0 (1- text-length)))
         (best 0))
    (while (<= low high)
      (let* ((middle (/ (+ low high) 2))
             (candidate (concat (substring text 0 middle) ellipsis))
             (width (disco-view--string-pixel-width candidate face)))
        (if (and (numberp width) (<= width pixel-limit))
            (setq best middle
                  low (1+ middle))
          (setq high (1- middle)))))
    (setq best (disco-view--safe-elide-boundary text best))
    (let ((result (copy-sequence text)))
      (add-text-properties
       best text-length
       (list 'display ellipsis
             'rear-nonsticky '(display)
             'face face)
       result)
      result)))

(defun disco-view-elide-string-for-columns (str max &optional face)
  "Return STR visually elided to MAX display columns.

Graphical buffers use actual font pixels so emoji and variable-width glyphs
cannot push following aligned columns to the right.  Terminals use ordinary
column widths.  FACE supplies the font metrics used for measurement."
  (let* ((text (or str ""))
         (limit (max 0 (or max 0)))
         (pixel-width (disco-view--string-pixel-width text face)))
    (if (numberp pixel-width)
        (let ((pixel-limit (disco-view--chars-xwidth limit)))
          (if (<= pixel-width pixel-limit)
              text
            (disco-view--elide-string-to-pixels text pixel-limit face)))
      (disco-view-elide-string text limit face))))

(defun disco-view--chars-xwidth (columns &optional window)
  "Return pixel width for COLUMNS using WINDOW metrics."
  (let* ((win (or window (get-buffer-window (current-buffer) t)))
         (frame (and (window-live-p win)
                     (window-frame win)))
         (buffer (and (window-live-p win) (window-buffer win)))
         (char-width
          (or (and (frame-live-p frame)
                   (display-graphic-p frame)
                   (fboundp 'string-pixel-width)
                   ;; STRING-PIXEL-WIDTH already accepts the buffer whose face
                   ;; remapping should be used.  Selecting WIN here would sync
                   ;; buffer point with its window-point during row insertion.
                   (ignore-errors
                     (string-pixel-width
                      (propertize "0" 'face 'default)
                      buffer)))
              (and (frame-live-p frame)
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
    (* (max 0 columns)
       (if (and (frame-live-p frame) (display-graphic-p frame))
           (max 1 char-width)
         1))))

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
  "Insert one absolute align-to spacer for COLUMN."
  (let* ((target (max 0 (or column 0)))
         (win (get-buffer-window (current-buffer) t))
         (frame (and (window-live-p win) (window-frame win))))
    (let ((align-to (if (and (frame-live-p frame)
                             (display-graphic-p frame))
                        (list (disco-view--chars-xwidth target win))
                      target)))
      (insert (propertize " " 'display `(space :align-to ,align-to))))))

(defun disco-view-window-fill-column (&optional window margin-columns)
  "Return telega-style usable columns for WINDOW.

The result follows face remapping/text scaling, includes window margins,
subtracts display line-number width, and reserves MARGIN-COLUMNS at the right
edge.  Return nil when WINDOW is not live."
  (let ((win (or window (get-buffer-window (current-buffer) t))))
    (when (window-live-p win)
      (let* ((margins (window-margins win))
             (width (+ (window-width win 'remap)
                       (or (car margins) 0)
                       (or (cdr margins) 0)))
             (line-numbers-p
              (with-current-buffer (window-buffer win)
                (bound-and-true-p display-line-numbers-mode)))
             (line-number-pixels
              (if line-numbers-p
                  (with-selected-window win
                    (line-number-display-width 'pixels))
                0))
             (char-pixels (max 1 (disco-view--chars-xwidth 1 win)))
             (line-number-columns
              (if (and (numberp line-number-pixels)
                       (> line-number-pixels 0))
                  (ceiling (/ line-number-pixels (float char-pixels)))
                0)))
        (max 1 (- width
                  (max 0 (or margin-columns 0))
                  line-number-columns))))))

(defun disco-view-one-line-column-widths (content-width context-width-spec)
  "Split CONTENT-WIDTH using CONTEXT-WIDTH-SPEC for the context column."
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

(defun disco-view--one-line-text (text)
  "Return TEXT with physical line-breaking whitespace collapsed."
  (string-trim
   (replace-regexp-in-string "[\t\n\r ]+" " " (or text "") nil t)))

(cl-defun disco-view-insert-one-line-row
    (row &key indent width icon-slot-width context-width-spec time-slot-width)
  "Insert ROW using one-line activity-style layout.

ROW is a `disco-view-one-line-row' object.  INDENT is left padding in spaces.
WIDTH sets the total row width.  ICON-SLOT-WIDTH reserves columns for the
icon slot.  CONTEXT-WIDTH-SPEC controls context width using
`disco-view-canonicalize-number' semantics.  TIME-SLOT-WIDTH reserves a stable
right-aligned timestamp column."
  (let* ((padding (make-string (max 0 (or indent 0)) ?\s))
         (context-text
          (disco-view--one-line-text (disco-view-one-line-row-context row)))
         (preview-text
          (disco-view--one-line-text (disco-view-one-line-row-preview row)))
         (time-text
          (disco-view--one-line-text (disco-view-one-line-row-time row)))
         (time-width
          (max (max 0 (or time-slot-width 0))
               (if (string-empty-p time-text)
                   0
                 (max 6 (string-width time-text)))))
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
        (insert (disco-view-elide-string-for-columns
                 context-text context-inner-width 'default))
        (disco-view-move-to-column (+ context-start 1 context-inner-width))
        (insert "]"))
      (when (> preview-width 0)
        (when (> separator-width 0)
          (insert " "))
        (let ((preview-start (point)))
          (insert (disco-view-elide-string-for-columns
                   preview-text preview-width 'shadow))
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
               (list 'help-echo help-echo))
             (when-let* ((mouse-face (disco-view-one-line-row-mouse-face row)))
               (list 'mouse-face mouse-face)))))))

(provide 'disco-view)

;;; disco-view.el ends here
