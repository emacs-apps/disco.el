;;; disco-chat-timeline.el --- Shared projected chat timeline -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Persistent chat timeline controller shared by disco-room and other clients.
;; Clients project protocol messages into `disco-chat-timeline-row' values.
;; Every state change uses one keyed reconciliation path: the projection may be
;; rebuilt in full, while EWOC only redraws rows whose payload, render context,
;; or dependency state changed.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'disco-chatbuf)
(require 'disco-view)

(cl-defstruct (disco-chat-timeline-row
               (:constructor disco-chat-timeline-row-create))
  "One protocol-independent projected chat timeline row."
  key
  payload
  context
  dependencies)

(cl-defstruct (disco-chat-timeline--state
               (:constructor disco-chat-timeline--state-create))
  ewoc
  node-table
  row-table
  keys
  dependency-index
  anchor-property
  printer
  after-mutation-function
  mutation-depth
  deferred-keys)

(defvar-local disco-chat-timeline--current nil
  "Shared projected timeline state owned by the current chat buffer.")

(defun disco-chat-timeline-reset ()
  "Discard projected timeline state in the current buffer."
  (setq-local disco-chat-timeline--current nil))

(defun disco-chat-timeline-live-p ()
  "Return non-nil when the current buffer owns a live timeline."
  (and (disco-chat-timeline--state-p disco-chat-timeline--current)
       (disco-chat-timeline--state-ewoc disco-chat-timeline--current)))

(defun disco-chat-timeline-ewoc ()
  "Return the current shared EWOC, or nil before timeline initialization."
  (and (disco-chat-timeline--state-p disco-chat-timeline--current)
       (disco-chat-timeline--state-ewoc disco-chat-timeline--current)))

(defun disco-chat-timeline-keys ()
  "Return projected row keys in display order."
  (copy-sequence
   (or (and (disco-chat-timeline--state-p disco-chat-timeline--current)
            (disco-chat-timeline--state-keys disco-chat-timeline--current))
       '())))

(defun disco-chat-timeline-node (key)
  "Return current EWOC node identified by KEY, or nil."
  (and (disco-chat-timeline--state-p disco-chat-timeline--current)
       (hash-table-p
        (disco-chat-timeline--state-node-table disco-chat-timeline--current))
       (gethash key
                (disco-chat-timeline--state-node-table
                 disco-chat-timeline--current))))

(defun disco-chat-timeline-row (key)
  "Return current projected row identified by KEY, or nil."
  (and (disco-chat-timeline--state-p disco-chat-timeline--current)
       (hash-table-p
        (disco-chat-timeline--state-row-table disco-chat-timeline--current))
       (gethash key
                (disco-chat-timeline--state-row-table
                 disco-chat-timeline--current))))

(defun disco-chat-timeline-context (key)
  "Return render context belonging to projected row KEY."
  (when-let* ((row (disco-chat-timeline-row key)))
    (disco-chat-timeline-row-context row)))

(defun disco-chat-timeline--require-state ()
  "Return current projected timeline state or signal an invariant error."
  (or (and (disco-chat-timeline--state-p disco-chat-timeline--current)
           disco-chat-timeline--current)
      (error "Disco chat timeline has not been initialized")))

(defun disco-chat-timeline--print-row (row)
  "Render projected ROW through the current client printer."
  (let* ((state (disco-chat-timeline--require-state))
         (printer (disco-chat-timeline--state-printer state)))
    (unless (functionp printer)
      (error "Disco chat timeline has no row printer"))
    (funcall printer row)))

(cl-defun disco-chat-timeline-ensure
    (&key printer anchor-property header footer after-mutation-function)
  "Ensure the current buffer owns one projected timeline.

PRINTER renders one `disco-chat-timeline-row'.  ANCHOR-PROPERTY is the text
property used to restore semantic message position.  HEADER and FOOTER seed a
new EWOC.  AFTER-MUTATION-FUNCTION runs after outer structural transactions."
  (if (disco-chat-timeline--state-p disco-chat-timeline--current)
      (progn
        (when printer
          (setf (disco-chat-timeline--state-printer
                 disco-chat-timeline--current)
                printer))
        (when anchor-property
          (setf (disco-chat-timeline--state-anchor-property
                 disco-chat-timeline--current)
                anchor-property))
        (setf (disco-chat-timeline--state-after-mutation-function
               disco-chat-timeline--current)
              after-mutation-function))
    (unless (functionp printer)
      (error "Disco chat timeline requires a row printer"))
    (let ((state
           (disco-chat-timeline--state-create
            :node-table (make-hash-table :test #'equal)
            :row-table (make-hash-table :test #'equal)
            :keys nil
            :dependency-index (make-hash-table :test #'equal)
            :anchor-property anchor-property
            :printer printer
            :after-mutation-function after-mutation-function
            :mutation-depth 0
            :deferred-keys nil)))
      (setq-local disco-chat-timeline--current state)
      (disco-chatbuf-with-structural-update
        (erase-buffer)
        (setf (disco-chat-timeline--state-ewoc state)
              (ewoc-create #'disco-chat-timeline--print-row
                           header footer t)))))
  (disco-chat-timeline--state-ewoc disco-chat-timeline--current))

(cl-defun disco-chat-timeline-project
    (entries key-function &key context-function dependencies-function)
  "Project ENTRIES into protocol-independent timeline rows.

KEY-FUNCTION receives one entry.  CONTEXT-FUNCTION, when non-nil, receives the
previous entry and current entry.  DEPENDENCIES-FUNCTION receives the current
entry and returns opaque resource keys whose changes should redraw the row."
  (let ((previous nil)
        rows)
    (dolist (entry entries (nreverse rows))
      (push (disco-chat-timeline-row-create
             :key (funcall key-function entry)
             :payload entry
             :context (and context-function
                           (funcall context-function previous entry))
             :dependencies (and dependencies-function
                                (delete-dups
                                 (delq nil
                                       (copy-sequence
                                        (or (funcall dependencies-function entry)
                                            '()))))))
            rows)
      (setq previous entry))))

(defun disco-chat-timeline--validate-rows (rows)
  "Require ROWS to have unique, non-nil stable keys."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (row rows)
      (unless (disco-chat-timeline-row-p row)
        (error "Disco chat timeline projection contains a non-row: %S" row))
      (let ((key (disco-chat-timeline-row-key row)))
        (unless key
          (error "Disco chat timeline row has no stable key"))
        (when (gethash key seen)
          (error "Disco chat timeline has duplicate row key %S" key))
        (puthash key t seen)))))

(defun disco-chat-timeline--row-table (rows)
  "Return equal-tested key to row table for validated ROWS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row rows table)
      (puthash (disco-chat-timeline-row-key row) row table))))

(defun disco-chat-timeline--dependency-index (rows)
  "Build resource key to projected row key index for ROWS."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (row rows index)
      (let ((row-key (disco-chat-timeline-row-key row)))
        (dolist (resource-key (disco-chat-timeline-row-dependencies row))
          (puthash resource-key
                   (cons row-key
                         (delete row-key (gethash resource-key index)))
                   index))))))

(defun disco-chat-timeline--dependent-keys-in-index (index resource-keys)
  "Return row keys in INDEX depending on RESOURCE-KEYS."
  (let (keys)
    (when (hash-table-p index)
      (dolist (resource-key resource-keys)
        (setq keys (nconc (copy-sequence (gethash resource-key index)) keys))))
    (delete-dups (delq nil keys))))

(defun disco-chat-timeline-dependent-keys (resource-keys)
  "Return current row keys depending on any of RESOURCE-KEYS."
  (let ((state (disco-chat-timeline--require-state)))
    (disco-chat-timeline--dependent-keys-in-index
     (disco-chat-timeline--state-dependency-index state)
     resource-keys)))

(defun disco-chat-timeline--footer-region-bounds ()
  "Return current EWOC footer bounds before the prompt, or nil."
  (when-let* ((ewoc (disco-chat-timeline-ewoc))
              (start (ewoc-location (ewoc--footer ewoc))))
    (let ((end (or (disco-chatbuf-prompt-start-position)
                   (disco-chatbuf-input-start-position)
                   (point-max))))
      (when (<= start end)
        (cons start end)))))

(defun disco-chat-timeline-footer-start-position ()
  "Return the current EWOC footer start position, or nil."
  (car-safe (disco-chat-timeline--footer-region-bounds)))

(defun disco-chat-timeline--position-zone-state (position preserve-window-start)
  "Capture semantic state for POSITION in the current timeline.

PRESERVE-WINDOW-START is forwarded for message-zone snapshots."
  (let ((position (min (point-max) (max (point-min) position))))
    (cond
     ((disco-chatbuf-point-in-input-p position)
      (list :zone 'input
            :offset (- position (disco-chatbuf-input-start-position))))
     ((disco-chatbuf-point-in-prompt-p position)
      (list :zone 'prompt
            :offset (- position (disco-chatbuf-prompt-start-position))))
     ((when-let* ((bounds (disco-chat-timeline--footer-region-bounds)))
        (and (<= (car bounds) position)
             (<= position (cdr bounds))))
      (list :zone 'footer
            :offset (- position
                       (car (disco-chat-timeline--footer-region-bounds)))))
     (t
      (save-excursion
        (goto-char position)
        (list :zone 'message
              :snapshot
              (disco-view-capture-position
               :anchor-property
               (disco-chat-timeline--state-anchor-property
                (disco-chat-timeline--require-state))
               :preserve-window-start preserve-window-start)))))))

(defun disco-chat-timeline--restore-zone-state (position-state rekeys)
  "Restore POSITION-STATE, remapping semantic anchors through REKEYS."
  (pcase (plist-get position-state :zone)
    ('input
     (when-let* ((start (disco-chatbuf-input-start-position))
                 (end (disco-chatbuf-input-logical-end-position)))
       (goto-char (min end
                       (max start
                            (+ start (or (plist-get position-state :offset) 0)))))))
    ('prompt
     (when-let* ((start (disco-chatbuf-prompt-start-position))
                 (end (disco-chatbuf-input-start-position)))
       (goto-char (min (max start (1- end))
                       (+ start (or (plist-get position-state :offset) 0))))))
    ('footer
     (when-let* ((bounds (disco-chat-timeline--footer-region-bounds)))
       (goto-char (min (cdr bounds)
                       (+ (car bounds)
                          (or (plist-get position-state :offset) 0))))))
    (_
     (when-let* ((snapshot (plist-get position-state :snapshot)))
       (disco-view-restore-position snapshot rekeys))))
  (point))

(cl-defun disco-chat-timeline-run-preserving-position (mutator &key rekeys)
  "Run MUTATOR as one undo-free chat timeline transaction.

Point, active mark, viewport, footer position, composer position, and window
points inside the composer are restored afterwards.  REKEYS is an alist mapping
old semantic row keys to new keys."
  (let ((state (disco-chat-timeline--require-state)))
    (if (> (or (disco-chat-timeline--state-mutation-depth state) 0) 0)
        (funcall mutator)
      (let* ((window-input-offsets
              (disco-chatbuf-capture-window-input-offsets))
             (point-state
              (disco-chat-timeline--position-zone-state (point) t))
             (mark-state
              (and mark-active
                   (disco-chat-timeline--position-zone-state (mark t) nil))))
        (setf (disco-chat-timeline--state-mutation-depth state) 1)
        (unwind-protect
            (disco-chatbuf-with-structural-update
              (funcall mutator))
          (setf (disco-chat-timeline--state-mutation-depth state) 0)
          (disco-chat-timeline--restore-zone-state point-state rekeys)
          (disco-chatbuf-restore-window-input-offsets window-input-offsets)
          (if mark-state
              (let ((mark-position
                     (save-excursion
                       (disco-chat-timeline--restore-zone-state mark-state rekeys)
                       (point))))
                (set-marker (mark-marker) mark-position)
                (setq mark-active t
                      deactivate-mark nil))
            (setq mark-active nil
                  deactivate-mark t))
          (when-let* ((after-mutation
                       (disco-chat-timeline--state-after-mutation-function state)))
            (funcall after-mutation)))))))

(cl-defun disco-chat-timeline-set-frame
    (header footer &key bind-input-function)
  "Set timeline HEADER and FOOTER and then call BIND-INPUT-FUNCTION."
  (let* ((state (disco-chat-timeline--require-state))
         (ewoc (disco-chat-timeline--state-ewoc state)))
    (disco-chat-timeline-run-preserving-position
     (lambda ()
       (disco-chatbuf-clear-prompt-and-input)
       (ewoc-set-hf ewoc header footer)
       (when (functionp bind-input-function)
         (funcall bind-input-function))))))

(defun disco-chat-timeline--validate-rekeys (state row-table rekeys)
  "Validate REKEYS against STATE and projected ROW-TABLE."
  (let ((nodes (disco-chat-timeline--state-node-table state))
        (targets (make-hash-table :test #'equal)))
    (dolist (mapping rekeys)
      (let ((old-key (car mapping))
            (new-key (cdr mapping)))
        (unless (and old-key new-key (not (equal old-key new-key)))
          (error "Disco chat timeline has invalid rekey %S" mapping))
        (unless (gethash new-key row-table)
          (error "Disco chat timeline rekey target %S is not projected" new-key))
        (when (gethash new-key targets)
          (error "Disco chat timeline has duplicate rekey target %S" new-key))
        (puthash new-key t targets)
        (when (and (gethash old-key nodes)
                   (gethash new-key nodes)
                   (not (eq (gethash old-key nodes) (gethash new-key nodes))))
          (error "Disco chat timeline rekey target %S already exists" new-key))))))

(defun disco-chat-timeline--apply-rekeys (state row-table rekeys)
  "Apply validated REKEYS to live nodes in STATE using projected ROW-TABLE."
  (let ((nodes (disco-chat-timeline--state-node-table state)))
    (dolist (mapping rekeys)
      (let* ((old-key (car mapping))
             (new-key (cdr mapping))
             (node (gethash old-key nodes))
             (row (gethash new-key row-table)))
        (when node
          (ewoc-set-data node row))))))

(cl-defun disco-chat-timeline-sync
    (rows &key force-keys changed-resources rekeys)
  "Synchronize the live timeline with projected ROWS.

FORCE-KEYS redraws presentation-only changes.  CHANGED-RESOURCES redraws rows
whose dependency lists mention those opaque resource keys.  REKEYS maps old
row keys to newly projected keys while preserving node and cursor identity."
  (disco-chat-timeline--validate-rows rows)
  (let* ((state (disco-chat-timeline--require-state))
         (ewoc (disco-chat-timeline--state-ewoc state))
         (row-table (disco-chat-timeline--row-table rows))
         (new-dependency-index
          (disco-chat-timeline--dependency-index rows))
         (dependency-force-keys
          (delete-dups
           (append
            (disco-chat-timeline--dependent-keys-in-index
             (disco-chat-timeline--state-dependency-index state)
             changed-resources)
            (disco-chat-timeline--dependent-keys-in-index
             new-dependency-index changed-resources))))
         (rekey-targets (mapcar #'cdr rekeys))
         (effective-force-keys
          (delete-dups
           (delq nil
                 (append (copy-sequence force-keys)
                         dependency-force-keys
                         rekey-targets)))))
    (disco-chat-timeline--validate-rekeys state row-table rekeys)
    (disco-chat-timeline-run-preserving-position
     (lambda ()
       (disco-chat-timeline--apply-rekeys state row-table rekeys)
       (setf (disco-chat-timeline--state-node-table state)
             (disco-view-reconcile-keyed-ewoc
              ewoc rows #'disco-chat-timeline-row-key
              :force-keys effective-force-keys)
             (disco-chat-timeline--state-row-table state) row-table
             (disco-chat-timeline--state-keys state)
             (mapcar #'disco-chat-timeline-row-key rows)
             (disco-chat-timeline--state-dependency-index state)
             new-dependency-index))
     :rekeys rekeys)
    (disco-chat-timeline-keys)))

(cl-defun disco-chat-timeline-invalidate (keys &key defer-while-mark-active)
  "Redraw existing rows identified by KEYS.

When DEFER-WHILE-MARK-ACTIVE is non-nil, queue keys until
`disco-chat-timeline-flush-deferred' is called with no active region."
  (let* ((state (disco-chat-timeline--require-state))
         (keys (delete-dups (delq nil (copy-sequence keys)))))
    (cond
     ((null keys) nil)
     ((and defer-while-mark-active mark-active)
      (setf (disco-chat-timeline--state-deferred-keys state)
            (delete-dups
             (append keys
                     (disco-chat-timeline--state-deferred-keys state))))
      'deferred)
     (t
      (disco-chat-timeline-run-preserving-position
       (lambda ()
         (dolist (key keys)
           (disco-view-invalidate-keyed-ewoc-node
            (disco-chat-timeline--state-ewoc state)
            (disco-chat-timeline--state-node-table state)
            key))))
      t))))

(defun disco-chat-timeline-flush-deferred ()
  "Redraw deferred keys when no region is active."
  (let ((state (disco-chat-timeline--require-state)))
    (when (and (not mark-active)
               (disco-chat-timeline--state-deferred-keys state))
      (let ((keys (prog1 (disco-chat-timeline--state-deferred-keys state)
                    (setf (disco-chat-timeline--state-deferred-keys state) nil))))
        (disco-chat-timeline-invalidate keys)))))

(defun disco-chat-timeline-refresh ()
  "Refresh all projected rows while preserving chat position."
  (let ((state (disco-chat-timeline--require-state)))
    (disco-chat-timeline-run-preserving-position
     (lambda ()
       (ewoc-refresh (disco-chat-timeline--state-ewoc state))))))

(defun disco-chat-timeline-key-at-point (&optional position)
  "Return semantic timeline key at POSITION or point."
  (let* ((position (or position (point)))
         (property
          (disco-chat-timeline--state-anchor-property
           (disco-chat-timeline--require-state))))
    (and property
         (or (get-text-property position property)
             (save-excursion
               (goto-char position)
               (get-text-property (line-beginning-position) property))))))

(defun disco-chat-timeline-key-position (key)
  "Return first buffer position carrying semantic row KEY, or nil."
  (let ((property
         (disco-chat-timeline--state-anchor-property
          (disco-chat-timeline--require-state))))
    (and property
         (disco-view-find-property-value
          (point-min) (point-max) property key))))

(provide 'disco-chat-timeline)

;;; disco-chat-timeline.el ends here
