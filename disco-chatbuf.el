;;; disco-chatbuf.el --- Shared chat buffer core helpers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared chat buffer state and prompt/input helpers used to move disco-like
;; buffers toward a telega-style chatbuf model.  This module intentionally
;; focuses on stable prompt/input state and structured input objects; timeline
;; rendering remains client-specific.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ring)
(require 'subr-x)

(defcustom disco-chatbuf-input-ring-size 50
  "Default size for shared chat buffer input history rings."
  :type 'integer
  :group 'disco)

(defface disco-chatbuf-input-object
  '((t :inherit shadow))
  "Face used for structured input objects inserted into chatbuf input."
  :group 'disco)

(defconst disco-chatbuf-input-object-property 'disco-chatbuf-input-object
  "Text property storing the semantic object represented in chatbuf input.")

(defconst disco-chatbuf-input-object-start-property 'disco-chatbuf-input-object-start
  "Text property marking the first character of a structured input object.")

(defconst disco-chatbuf-input-object-end-property 'disco-chatbuf-input-object-end
  "Text property marking the last character of a structured input object.")

(defvar-local disco-chatbuf--input-marker nil
  "Marker pointing to the start of the editable input region.")

(defvar-local disco-chatbuf--prompt-marker nil
  "Marker pointing to the start of the current prompt button.")

(defvar-local disco-chatbuf--prompt-button nil
  "Button object used for the visible chat prompt.")

(defvar-local disco-chatbuf--input-ring nil
  "Ring containing shared chatbuf input history entries.")

(defvar-local disco-chatbuf--input-idx nil
  "Current absolute position inside `disco-chatbuf--input-ring'.")

(defvar-local disco-chatbuf--input-pending nil
  "Input remembered before entering history navigation.")

(defvar-local disco-chatbuf--aux-plist nil
  "Current aux state plist for the active chat buffer.")

(defvar-local disco-chatbuf--input-options-plist nil
  "Current input options plist for the active chat buffer.")

(defvar-local disco-chatbuf--rendering nil
  "Non-nil while the owning chat buffer is performing a bulk redraw.")

(define-button-type 'disco-chatbuf-prompt
  :supertype 'button
  'face 'default
  'read-only t
  'front-sticky t
  'rear-nonsticky t
  'cursor-intangible t
  'inactive t
  'field 'disco-chatbuf-prompt)

(defun disco-chatbuf-init-state (&optional ring-size)
  "Initialize shared chat buffer state in the current buffer.

RING-SIZE overrides `disco-chatbuf-input-ring-size' when non-nil.  Existing
markers and rings are reused when already present."
  (unless (markerp disco-chatbuf--input-marker)
    (setq-local disco-chatbuf--input-marker (make-marker)))
  (unless (markerp disco-chatbuf--prompt-marker)
    (setq-local disco-chatbuf--prompt-marker (make-marker)))
  (unless (ring-p disco-chatbuf--input-ring)
    (setq-local disco-chatbuf--input-ring
                (make-ring (max 1 (or ring-size disco-chatbuf-input-ring-size)))))
  (unless (local-variable-p 'disco-chatbuf--input-idx)
    (setq-local disco-chatbuf--input-idx nil))
  (unless (local-variable-p 'disco-chatbuf--input-pending)
    (setq-local disco-chatbuf--input-pending nil))
  (unless (local-variable-p 'disco-chatbuf--aux-plist)
    (setq-local disco-chatbuf--aux-plist nil))
  (unless (local-variable-p 'disco-chatbuf--input-options-plist)
    (setq-local disco-chatbuf--input-options-plist nil))
  (unless (local-variable-p 'disco-chatbuf--rendering)
    (setq-local disco-chatbuf--rendering nil)))

(defun disco-chatbuf-prompt-start-position ()
  "Return current prompt start position, or nil when prompt is unavailable."
  (and (markerp disco-chatbuf--prompt-marker)
       (eq (marker-buffer disco-chatbuf--prompt-marker) (current-buffer))
       (marker-position disco-chatbuf--prompt-marker)))

(defun disco-chatbuf-input-start-position ()
  "Return current editable input start position, or nil when unavailable."
  (and (markerp disco-chatbuf--input-marker)
       (eq (marker-buffer disco-chatbuf--input-marker) (current-buffer))
       (marker-position disco-chatbuf--input-marker)))

(defun disco-chatbuf-input-region-bounds ()
  "Return current editable input bounds as (START . END), or nil."
  (when-let* ((start (disco-chatbuf-input-start-position)))
    (when (<= start (point-max))
      (cons start (point-max)))))

(defun disco-chatbuf-input-logical-end-position ()
  "Return logical end position of the current editable input region."
  (when (disco-chatbuf-input-start-position)
    (point-max)))

(defun disco-chatbuf-prompt-button-live-p ()
  "Return non-nil when the current prompt button is live in this buffer."
  (let ((prompt-start (disco-chatbuf-prompt-start-position))
        (input-start (disco-chatbuf-input-start-position)))
    (and prompt-start
         input-start
         (< prompt-start input-start)
         (let ((button (button-at prompt-start)))
           (and button
                (eq (button-get button 'field) 'disco-chatbuf-prompt))))))

(defun disco-chatbuf-point-in-input-p (&optional position)
  "Return non-nil when POSITION or point is inside the editable input region."
  (let* ((bounds (disco-chatbuf-input-region-bounds))
         (pos (or position (point))))
    (and bounds
         (<= (car bounds) pos)
         (<= pos (cdr bounds)))))

(defun disco-chatbuf-point-in-prompt-p (&optional position)
  "Return non-nil when POSITION or point is inside the prompt glyph span."
  (let ((prompt-start (disco-chatbuf-prompt-start-position))
        (input-start (disco-chatbuf-input-start-position))
        (pos (or position (point))))
    (and prompt-start
         input-start
         (>= pos prompt-start)
         (< pos input-start))))

(defun disco-chatbuf-post-command-clamp-point ()
  "Keep point out of the prompt glyph span."
  (when (disco-chatbuf-point-in-prompt-p)
    (when-let* ((input-start (disco-chatbuf-input-start-position)))
      (goto-char input-start))))

(defun disco-chatbuf--ensure-point-after-input-start ()
  "Move point to the end of buffer when it is before input start."
  (when-let* ((input-start (disco-chatbuf-input-start-position)))
    (when (< (point) input-start)
      (goto-char (point-max)))))

(defun disco-chatbuf--restore-input-point (input-offset)
  "Restore point inside the input region using INPUT-OFFSET when possible."
  (when (numberp input-offset)
    (when-let* ((input-start (disco-chatbuf-input-start-position)))
      (goto-char (min (point-max)
                      (max input-start (+ input-start input-offset)))))))

(defun disco-chatbuf-install-prompt (&optional prompt)
  "Ensure a prompt button exists at the end of the current buffer.

PROMPT defaults to `>>> '.  If a prompt already exists, update it in place."
  (interactive)
  (disco-chatbuf-init-state)
  (if (disco-chatbuf-prompt-button-live-p)
      (disco-chatbuf-prompt-update prompt)
    (let ((prompt-text (or prompt ">>> ")))
      (goto-char (point-max))
      (set-marker disco-chatbuf--prompt-marker (point) (current-buffer))
      (setq disco-chatbuf--prompt-button
            (insert-text-button prompt-text 'type 'disco-chatbuf-prompt))
      (set-marker disco-chatbuf--input-marker (point) (current-buffer))
      disco-chatbuf--prompt-button)))

(defun disco-chatbuf-prompt-update (&optional prompt)
  "Replace the visible prompt text with PROMPT.

PROMPT defaults to `>>> '.  Any existing input contents stay in place and
point is restored relative to the input start when it was inside the input."
  (interactive)
  (disco-chatbuf-init-state)
  (let* ((prompt-text (or prompt ">>> "))
         (prompt-start (or (disco-chatbuf-prompt-start-position) (point-max)))
         (input-start (or (disco-chatbuf-input-start-position) prompt-start))
         (in-input (disco-chatbuf-point-in-input-p))
         (input-offset (and in-input (- (point) input-start)))
         (inhibit-read-only t))
    (save-excursion
      (delete-region prompt-start input-start)
      (goto-char prompt-start)
      (set-marker disco-chatbuf--prompt-marker (point) (current-buffer))
      (setq disco-chatbuf--prompt-button
            (insert-text-button prompt-text 'type 'disco-chatbuf-prompt))
      (set-marker disco-chatbuf--input-marker (point) (current-buffer)))
    (disco-chatbuf--restore-input-point input-offset)
    disco-chatbuf--prompt-button))

(defun disco-chatbuf-has-input-p ()
  "Return non-nil when the current chat buffer has some input text."
  (when-let* ((input-start (disco-chatbuf-input-start-position)))
    (< input-start (point-max))))

(defun disco-chatbuf-input-string ()
  "Return the current editable input string, preserving text properties."
  (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
    (buffer-substring (car bounds) (cdr bounds))))

(defun disco-chatbuf-input-delete ()
  "Delete all current input contents."
  (interactive)
  (when-let* ((input-start (disco-chatbuf-input-start-position)))
    (let ((inhibit-read-only t))
      (delete-region input-start (point-max)))))

(defun disco-chatbuf-input-set-text (text)
  "Replace the current input contents with TEXT."
  (disco-chatbuf-init-state)
  (disco-chatbuf-input-delete)
  (when-let* ((input-start (disco-chatbuf-input-start-position)))
    (save-excursion
      (goto-char input-start)
      (insert (or text ""))))
  (goto-char (point-max)))

(defun disco-chatbuf-input-apply-text-properties (&optional input-map)
  "Apply editable-region properties to current input text.

When INPUT-MAP is non-nil, install it as `local-map' for the full input region."
  (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
    (with-silent-modifications
      (add-text-properties
       (car bounds) (cdr bounds)
       (append
        '(read-only nil
          rear-nonsticky (read-only local-map))
        (when input-map
          (list 'local-map input-map)))))))

(cl-defun disco-chatbuf-after-change
    (beg end &key rendering-p input-map sync-function prune-broken-objects)
  "Maintain shared input-region invariants after a buffer change.

BEG and END describe the changed region.  When RENDERING-P is non-nil, do
nothing.  Otherwise, if the change overlaps the current input region, re-apply
input properties, optionally prune broken structured objects, and then call
SYNC-FUNCTION when non-nil."
  (unless rendering-p
    (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
      (when (and (< beg (cdr bounds))
                 (> end (car bounds)))
        (disco-chatbuf-input-apply-text-properties input-map)
        (when prune-broken-objects
          (disco-chatbuf-input-prune-broken-objects))
        (when (functionp sync-function)
          (funcall sync-function))))))

(cl-defun disco-chatbuf-input-insert (content &key object properties)
  "Insert CONTENT into the current input region.

CONTENT must be a string.  When OBJECT is non-nil, tag the inserted text as a
structured input object using `disco-chatbuf-input-object-property'.  Extra
PROPERTIES are appended to the inserted text properties."
  (unless (stringp content)
    (user-error "disco-chatbuf: input content must be a string"))
  (when (and object (string-empty-p content))
    (user-error "disco-chatbuf: structured input objects need visible text"))
  (disco-chatbuf-init-state)
  (disco-chatbuf--ensure-point-after-input-start)
  (when-let* ((input-start (disco-chatbuf-input-start-position)))
    (when (< (point) input-start)
      (goto-char (point-max))))
  (let ((start (point)))
    (insert content)
    (when (< start (point))
      (let ((end (point))
            (text-properties properties))
        (when object
          (setq text-properties
                (append
                 (list disco-chatbuf-input-object-property object
                       'face 'disco-chatbuf-input-object)
                 text-properties))
          (add-text-properties start (1+ start)
                               (list disco-chatbuf-input-object-start-property t))
          (add-text-properties (1- end) end
                               (list disco-chatbuf-input-object-end-property t)))
        (when text-properties
          (add-text-properties start end text-properties))))))

(defun disco-chatbuf-input-object-at-point (&optional position)
  "Return structured input object at POSITION or point, or nil."
  (get-text-property (or position (point)) disco-chatbuf-input-object-property))

(defun disco-chatbuf-input-has-objects-p ()
  "Return non-nil when current input contains structured objects."
  (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
    (text-property-not-all (car bounds) (cdr bounds)
                           disco-chatbuf-input-object-property nil)))

(defun disco-chatbuf--input-object-region-start (position)
  "Return start position of object region containing POSITION, or nil."
  (when (disco-chatbuf-input-object-at-point position)
    (or (previous-single-property-change
         (1+ position) disco-chatbuf-input-object-property nil
         (or (disco-chatbuf-input-start-position) (point-min)))
        (disco-chatbuf-input-start-position)
        (point-min))))

(defun disco-chatbuf-input-object-bounds-at-point (&optional position)
  "Return bounds of structured input object at POSITION or point, or nil."
  (let ((pos (or position (point))))
    (when (disco-chatbuf-input-object-at-point pos)
      (let* ((start (disco-chatbuf--input-object-region-start pos))
             (end (or (next-single-property-change
                       pos disco-chatbuf-input-object-property nil (point-max))
                      (point-max))))
        (cons start end)))))

(defun disco-chatbuf-input-prune-broken-objects ()
  "Delete structured input objects whose boundary markers became invalid."
  (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
    (let ((pos (car bounds))
          (limit (cdr bounds))
          (inhibit-read-only t))
      (while (< pos limit)
        (let ((object (get-text-property pos disco-chatbuf-input-object-property)))
          (if object
              (let* ((start (or (disco-chatbuf--input-object-region-start pos) pos))
                     (end (or (next-single-property-change
                               pos disco-chatbuf-input-object-property nil limit)
                              limit))
                     (valid-start (get-text-property start
                                                     disco-chatbuf-input-object-start-property))
                     (valid-end (get-text-property (1- end)
                                                   disco-chatbuf-input-object-end-property)))
                (if (and valid-start valid-end)
                    (setq pos end)
                  (delete-region start end)
                  (setq limit (point-max))
                  (setq pos start)))
            (setq pos (or (next-single-property-change
                           pos disco-chatbuf-input-object-property nil limit)
                          limit))))))))

(defun disco-chatbuf-aux-set (aux-plist)
  "Replace current aux state with AUX-PLIST and return it."
  (setq disco-chatbuf--aux-plist aux-plist))

(defun disco-chatbuf-aux-reset ()
  "Clear current aux state and return nil."
  (setq disco-chatbuf--aux-plist nil))

(defun disco-chatbuf-aux-active-p ()
  "Return non-nil when a shared aux state is currently active."
  (not (null disco-chatbuf--aux-plist)))

(defun disco-chatbuf-input-options-set (options-plist)
  "Replace current input options state with OPTIONS-PLIST and return it."
  (setq disco-chatbuf--input-options-plist options-plist))

(defun disco-chatbuf-input-options-reset ()
  "Clear current input options state and return nil."
  (setq disco-chatbuf--input-options-plist nil))

(defun disco-chatbuf-input-history-push (&optional input)
  "Push INPUT into shared input history when it is plain text and non-empty.

When INPUT is nil, use current input contents.  Structured-object input is not
stored yet because history semantics for mixed object/text entries are not
finalized."
  (let ((value (or input (disco-chatbuf-input-string))))
    (unless (or (null value)
                (disco-chatbuf-input-has-objects-p)
                (string-empty-p (string-trim-right (substring-no-properties value)))
                (and (ring-p disco-chatbuf--input-ring)
                     (> (ring-length disco-chatbuf--input-ring) 0)
                     (equal value (ring-ref disco-chatbuf--input-ring 0))))
      (ring-insert disco-chatbuf--input-ring value)))
  (setq disco-chatbuf--input-idx nil)
  (setq disco-chatbuf--input-pending nil))

(defun disco-chatbuf-input-history-goto (index)
  "Replace current input with history entry INDEX.

When INDEX is nil, restore pending input remembered before history navigation."
  (unless (ring-p disco-chatbuf--input-ring)
    (user-error "disco-chatbuf: input history is unavailable"))
  (unless disco-chatbuf--input-idx
    (setq disco-chatbuf--input-pending (or (disco-chatbuf-input-string) "")))
  (setq disco-chatbuf--input-idx index)
  (cond
   ((null index)
    (disco-chatbuf-input-set-text (or disco-chatbuf--input-pending "")))
   ((or (< index 0) (>= index (ring-length disco-chatbuf--input-ring)))
    (user-error "disco-chatbuf: history index %s is out of range" index))
   (t
    (disco-chatbuf-input-set-text (ring-ref disco-chatbuf--input-ring index)))))

(defun disco-chatbuf-input-history-prev (&optional n)
  "Replace input with N previous entries from input history."
  (interactive "p")
  (let* ((step (max 1 (or n 1)))
         (ring-size (and (ring-p disco-chatbuf--input-ring)
                         (ring-length disco-chatbuf--input-ring))))
    (cond
     ((or (null ring-size) (= ring-size 0))
      (user-error "disco-chatbuf: input history is empty"))
     ((null disco-chatbuf--input-idx)
      (disco-chatbuf-input-history-goto (1- step)))
     (t
      (disco-chatbuf-input-history-goto
       (min (1- ring-size) (+ disco-chatbuf--input-idx step)))))))

(defun disco-chatbuf-input-history-next (&optional n)
  "Replace input with N newer entries from input history."
  (interactive "p")
  (let ((step (max 1 (or n 1))))
    (cond
     ((null disco-chatbuf--input-idx)
      (user-error "disco-chatbuf: already at latest input"))
     ((<= disco-chatbuf--input-idx (1- step))
      (setq disco-chatbuf--input-idx nil)
      (disco-chatbuf-input-set-text (or disco-chatbuf--input-pending ""))
      (setq disco-chatbuf--input-pending nil))
     (t
      (disco-chatbuf-input-history-goto (- disco-chatbuf--input-idx step))))))

(provide 'disco-chatbuf)

;;; disco-chatbuf.el ends here
