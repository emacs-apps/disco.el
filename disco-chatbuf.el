;;; disco-chatbuf.el --- Shared chat buffer core helpers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared canonical composer state and prompt/input helpers for telega-style
;; chat buffers.  Persistent projected timelines live in
;; `disco-chat-timeline'; protocol-specific message rendering remains in each
;; client.

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

(defvar disco-chatbuf--input-cache)

(defun disco-chatbuf-copy-string (value)
  "Return VALUE copied, preserving text properties when it is a string."
  (if (stringp value)
      (copy-sequence value)
    ""))

(defun disco-chatbuf-string-plain-text (value)
  "Return VALUE without text properties, or an empty string."
  (substring-no-properties (or value "")))

(defun disco-chatbuf-string-has-objects-p (value)
  "Return non-nil when VALUE contains structured input objects."
  (and (stringp value)
       (text-property-not-all 0 (length value)
                              disco-chatbuf-input-object-property nil
                              value)))

(defun disco-chatbuf-input-state ()
  "Return canonical chat composer input, preserving text properties."
  (disco-chatbuf-copy-string disco-chatbuf--input-cache))

(cl-defun disco-chatbuf-input-state-set (value &key reset-history-p)
  "Set canonical composer input to VALUE, preserving text properties.

When RESET-HISTORY-P is non-nil, clear shared history navigation state."
  (setq disco-chatbuf--input-cache (disco-chatbuf-copy-string value))
  (when reset-history-p
    (disco-chatbuf-input-history-reset))
  (disco-chatbuf-input-state))

(cl-defun disco-chatbuf-input-state-clear (&key reset-history-p)
  "Clear canonical composer input.

When RESET-HISTORY-P is non-nil, clear shared history navigation state."
  (disco-chatbuf-input-state-set "" :reset-history-p reset-history-p))

(cl-defun disco-chatbuf-input-state-sync (&key (reset-history-p t))
  "Synchronize canonical composer input from the editable tail region.

Return a plist with keys `:value' and `:changed-p'.  When the live input text
changes including text properties, canonical state is updated and shared
history navigation state is reset when RESET-HISTORY-P is non-nil."
  (let* ((text (disco-chatbuf-copy-string (or (disco-chatbuf-input-string) "")))
         (current disco-chatbuf--input-cache)
         (changed-p (not (equal-including-properties text current))))
    (when changed-p
      (setq disco-chatbuf--input-cache text)
      (when reset-history-p
        (disco-chatbuf-input-history-reset)))
    (list :value (disco-chatbuf-copy-string disco-chatbuf--input-cache)
          :changed-p changed-p)))

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

(defvar-local disco-chatbuf--input-cache ""
  "Canonical chat composer input, preserving structured text properties.")

(defvar-local disco-chatbuf--aux-plist nil
  "Current aux state plist for the active chat buffer.")

(defvar-local disco-chatbuf--input-options-plist nil
  "Current input options plist for the active chat buffer.")

(defvar-local disco-chatbuf--rendering nil
  "Non-nil while the owning chat buffer is performing a bulk redraw.")

(defvar disco-chatbuf--mutating-input nil
  "Dynamically non-nil while a compound composer mutation is in progress.")

(defun disco-chatbuf-rendering-p ()
  "Return non-nil while shared chat buffer structure is being updated."
  disco-chatbuf--rendering)

(defmacro disco-chatbuf-with-structural-update (&rest body)
  "Run BODY as one generated chat buffer structural update.

Structural updates are read-only-buffer mutations produced from application
state.  They must not enter buffer undo history; the editable composer remains
independently undoable outside this scope."
  (declare (indent 0) (debug t))
  `(let ((disco-chatbuf--rendering t)
         (inhibit-read-only t)
         (buffer-undo-list t))
     ,@body))

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
  (unless (local-variable-p 'disco-chatbuf--input-cache)
    (setq-local disco-chatbuf--input-cache ""))
  (unless (local-variable-p 'disco-chatbuf--aux-plist)
    (setq-local disco-chatbuf--aux-plist nil))
  (unless (local-variable-p 'disco-chatbuf--input-options-plist)
    (setq-local disco-chatbuf--input-options-plist nil))
  (unless (local-variable-p 'disco-chatbuf--rendering)
    (setq-local disco-chatbuf--rendering nil)))

(defun disco-chatbuf-reset-state (&optional ring-size)
  "Reset shared chat buffer state in the current buffer.

This recreates prompt/input markers, clears prompt button state, allocates a
fresh input history ring, and resets aux/input-options/rendering state.
RING-SIZE overrides `disco-chatbuf-input-ring-size' when non-nil."
  (when (markerp disco-chatbuf--input-marker)
    (set-marker disco-chatbuf--input-marker nil))
  (when (markerp disco-chatbuf--prompt-marker)
    (set-marker disco-chatbuf--prompt-marker nil))
  (setq-local disco-chatbuf--input-marker (make-marker))
  (setq-local disco-chatbuf--prompt-marker (make-marker))
  (setq-local disco-chatbuf--prompt-button nil)
  (setq-local disco-chatbuf--input-ring
              (make-ring (max 1 (or ring-size disco-chatbuf-input-ring-size))))
  (setq-local disco-chatbuf--input-idx nil)
  (setq-local disco-chatbuf--input-pending nil)
  (setq-local disco-chatbuf--input-cache "")
  (setq-local disco-chatbuf--aux-plist nil)
  (setq-local disco-chatbuf--input-options-plist nil)
  (setq-local disco-chatbuf--rendering nil))

(defun disco-chatbuf-mode-setup ()
  "Apply telega-like chat buffer defaults in the current buffer."
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local window-point-insertion-type t)
  (setq-local next-line-add-newlines nil)
  (setq-local next-screen-context-lines 0)
  (when (fboundp 'cursor-intangible-mode)
    (cursor-intangible-mode 1))
  (when (fboundp 'cursor-sensor-mode)
    (cursor-sensor-mode 1)))

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

(defun disco-chatbuf-capture-window-input-offsets ()
  "Return (WINDOW . OFFSET) pairs for windows whose point is in composer input."
  (let ((bounds (disco-chatbuf-input-region-bounds))
        offsets)
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (let ((window-point (window-point window)))
            (when (and (<= start window-point)
                       (<= window-point end))
              (push (cons window (- window-point start)) offsets))))))
    offsets))

(defun disco-chatbuf-restore-window-input-offsets (offsets)
  "Restore window points in OFFSETS relative to current composer input start."
  (let ((start (disco-chatbuf-input-start-position))
        (logical-end (disco-chatbuf-input-logical-end-position)))
    (when (and (number-or-marker-p start)
               (number-or-marker-p logical-end))
      (dolist (entry offsets)
        (let ((window (car entry))
              (offset (cdr entry)))
          (when (and (window-live-p window)
                     (eq (window-buffer window) (current-buffer)))
            (set-window-point
             window
             (min logical-end
                  (max start (+ start offset))))))))))

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

(defun disco-chatbuf-clear-prompt-and-input ()
  "Remove the current prompt and input region from the buffer."
  (interactive)
  (let ((prompt-start (disco-chatbuf-prompt-start-position))
        (inhibit-read-only t))
    (when prompt-start
      (delete-region prompt-start (point-max))))
  (setq disco-chatbuf--prompt-button nil)
  (when (markerp disco-chatbuf--prompt-marker)
    (set-marker disco-chatbuf--prompt-marker nil))
  (when (markerp disco-chatbuf--input-marker)
    (set-marker disco-chatbuf--input-marker nil)))

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
      (delete-region input-start (point-max))))
  (unless (or disco-chatbuf--mutating-input
              (disco-chatbuf-rendering-p))
    (disco-chatbuf-input-state-sync)))

(cl-defun disco-chatbuf-input-set-text (text &key preserve-history-navigation-p)
  "Replace current input with TEXT.

When PRESERVE-HISTORY-NAVIGATION-P is non-nil, retain the current history
cursor and remembered pending input."
  (disco-chatbuf-init-state)
  (let ((disco-chatbuf--mutating-input t))
    (disco-chatbuf-input-delete)
    (when-let* ((input-start (disco-chatbuf-input-start-position)))
      (save-excursion
        (goto-char input-start)
        (insert (or text ""))))
    (goto-char (point-max)))
  (unless (disco-chatbuf-rendering-p)
    (disco-chatbuf-input-state-sync
     :reset-history-p (not preserve-history-navigation-p))))

(defun disco-chatbuf-input-replace (text)
  "Replace current input contents with TEXT, preserving point inside input.

If point was inside the input region before replacement, restore its relative
offset from the input start when possible."
  (let* ((input-start (disco-chatbuf-input-start-position))
         (in-input (and input-start (disco-chatbuf-point-in-input-p)))
         (input-offset (and in-input (- (point) input-start))))
    (disco-chatbuf-input-set-text text)
    (disco-chatbuf--restore-input-point input-offset)))

(cl-defun disco-chatbuf-bind-input-region (&key visible-p prompt input-text post-bind-function)
  "Ensure the tail input region matches VISIBLE-P, PROMPT and INPUT-TEXT.

When VISIBLE-P is nil, remove the current prompt and input region.  Otherwise,
install or update PROMPT, replace the input contents with INPUT-TEXT, and call
POST-BIND-FUNCTION when non-nil.  This is a shared chatbuf primitive; callers
can use POST-BIND-FUNCTION for owner-specific text properties or local repair."
  (disco-chatbuf-init-state)
  (if (not visible-p)
      (disco-chatbuf-clear-prompt-and-input)
    (save-excursion
      (goto-char (point-max))
      (if (disco-chatbuf-prompt-button-live-p)
          (disco-chatbuf-prompt-update prompt)
        (disco-chatbuf-install-prompt prompt)))
    (disco-chatbuf-input-set-text input-text)
    (when (functionp post-bind-function)
      (funcall post-bind-function))))

(defun disco-chatbuf-input-apply-text-properties ()
  "Normalize current input region after redraws and edits."
  (disco-chatbuf-init-state)
  (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
    (with-silent-modifications
      (add-text-properties (car bounds) (cdr bounds)
                           '(read-only nil)))))

(cl-defun disco-chatbuf-after-change
    (beg end &key rendering-p sync-function prune-broken-objects)
  "Maintain shared input-region invariants after a buffer change.

BEG and END describe the changed region.  When RENDERING-P is non-nil, do
nothing.  Otherwise, if the change overlaps the current input region, normalize
input text properties, optionally prune broken structured objects, and then call
SYNC-FUNCTION when non-nil."
  (unless rendering-p
    (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
      (when (and (< beg (cdr bounds))
                 (> end (car bounds)))
        (disco-chatbuf-input-apply-text-properties)
        (when prune-broken-objects
          (disco-chatbuf-input-prune-broken-objects))
        (when (functionp sync-function)
          (funcall sync-function))))))

(defun disco-chatbuf-split-by-text-property (string property)
  "Split STRING by changes of text PROPERTY.

Mirrors `telega--split-by-text-prop' (used by telega-chatbuf input→IMC)."
  (let ((finish (length string))
        (start 0)
        (pos 0)
        result)
    (while (and (> finish pos)
                (setq pos
                      (next-single-char-property-change pos property string)))
      (push (substring string start pos) result)
      (setq start pos))
    (nreverse result)))

(cl-defun disco-chatbuf-input-insert (content &key object properties)
  "Insert CONTENT into the current input region.

CONTENT must be a string.  When OBJECT is non-nil, tag the inserted text as a
structured input object using `disco-chatbuf-input-object-property'.  Extra
PROPERTIES are appended to the inserted text properties.

Attachment layout follows telega's `telega-chatbuf-input-insert':

1. If point is already on an attachment, insert a separating space first.
2. Object body carries the object property (+ optional face).
3. First char gets `disco-chatbuf-input-object-start-property' (telega
   `attach-open-bracket').
4. A trailing space after the body carries
   `disco-chatbuf-input-object-end-property' and `rear-nonsticky t'
   (telega `attach-close-bracket' + `rear-nonsticky t' on the spacer).

The trailing spacer is what keeps following typed text (e.g. Chinese after an
image) from inheriting the object property — default Emacs rear-stickiness
would otherwise glue it into the attachment."
  (unless (stringp content)
    (user-error "disco-chatbuf: input content must be a string"))
  (when (and object (string-empty-p content))
    (user-error "disco-chatbuf: structured input objects need visible text"))
  (disco-chatbuf-init-state)
  (disco-chatbuf--ensure-point-after-input-start)
  (when-let* ((input-start (disco-chatbuf-input-start-position)))
    (when (< (point) input-start)
      (goto-char (point-max))))
  ;; telega: when point sits on an existing attach, separate with a space.
  (when (and object
             (disco-chatbuf-input-start-position)
             (>= (point) (disco-chatbuf-input-start-position))
             (disco-chatbuf-input-object-at-point (point)))
    (insert " "))
  (if (not object)
      (let ((start (point)))
        (insert content)
        (when (and properties (< start (point)))
          (add-text-properties start (point) properties)))
    ;; Structured object (telega `telega-chatbuf-input-insert' pattern).
    ;;
    ;; Inhibit modification hooks while the object is half-built: after-change
    ;; prune would otherwise see start-without-end and delete the body (image
    ;; labels with `display' properties hit this immediately).  telega applies
    ;; open/close brackets before post-command validation runs.
    (let ((inhibit-modification-hooks t)
          (body-start (point)))
      (insert content)
      (insert " ")
      (let ((body-end (1- (point)))
            (gap-end (point)))
        (when (< body-start body-end)
          (add-text-properties
           body-start gap-end
           (append
            (list disco-chatbuf-input-object-property object
                  'face 'disco-chatbuf-input-object
                  'cursor-intangible t)
            properties))
          (add-text-properties body-start (1+ body-start)
                               (list disco-chatbuf-input-object-start-property t))
          ;; Trailing spacer: telega attach-close-bracket + rear-nonsticky t.
          (add-text-properties body-end gap-end
                               (list disco-chatbuf-input-object-end-property t
                                     'rear-nonsticky t
                                     ;; Spacer itself should not be intangible so
                                     ;; the user can place point after the attach.
                                     'cursor-intangible nil))))))
  (unless (disco-chatbuf-rendering-p)
    (disco-chatbuf-input-state-sync)))

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

(defun disco-chatbuf-aux-state ()
  "Return current shared aux state plist, or nil."
  disco-chatbuf--aux-plist)

(defun disco-chatbuf-aux-type ()
  "Return current shared aux type, or nil."
  (plist-get disco-chatbuf--aux-plist :aux-type))

(defun disco-chatbuf-aux-message-id ()
  "Return current shared aux message id, or nil."
  (plist-get disco-chatbuf--aux-plist :message-id))

(defun disco-chatbuf-aux-active-p ()
  "Return non-nil when a shared aux state is currently active."
  (not (null disco-chatbuf--aux-plist)))

(defun disco-chatbuf-input-options-set (options-plist)
  "Replace current input options state with OPTIONS-PLIST and return it."
  (setq disco-chatbuf--input-options-plist options-plist))

(defun disco-chatbuf-input-options-reset ()
  "Clear current input options state and return nil."
  (setq disco-chatbuf--input-options-plist nil))

(defun disco-chatbuf-input-options-state ()
  "Return current shared input options plist, or nil."
  disco-chatbuf--input-options-plist)

(defun disco-chatbuf-input-option (key &optional default)
  "Return input option KEY from shared state, or DEFAULT when missing."
  (if (plist-member disco-chatbuf--input-options-plist key)
      (plist-get disco-chatbuf--input-options-plist key)
    default))

(defun disco-chatbuf-input-history-push (&optional input)
  "Push INPUT into shared input history when it is plain text and non-empty.

When INPUT is nil, use current input contents.  Structured-object input is not
stored yet because history semantics for mixed object/text entries are not
finalized.  When INPUT is non-nil, object detection uses INPUT's text
properties instead of inspecting the live buffer contents."
  (let* ((value (or input (disco-chatbuf-input-string)))
         (has-objects (and value
                           (if input
                               (text-property-not-all
                                0 (length value)
                                disco-chatbuf-input-object-property nil
                                value)
                             (disco-chatbuf-input-has-objects-p)))))
    (unless (or (null value)
                has-objects
                (string-empty-p (string-trim-right (substring-no-properties value)))
                (and (ring-p disco-chatbuf--input-ring)
                     (> (ring-length disco-chatbuf--input-ring) 0)
                     (equal value (ring-ref disco-chatbuf--input-ring 0))))
      (ring-insert disco-chatbuf--input-ring value)))
  (disco-chatbuf-input-history-reset))

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
    (disco-chatbuf-input-set-text
     (or disco-chatbuf--input-pending "")
     :preserve-history-navigation-p t))
   ((or (< index 0) (>= index (ring-length disco-chatbuf--input-ring)))
    (user-error "disco-chatbuf: history index %s is out of range" index))
   (t
    (disco-chatbuf-input-set-text
     (ring-ref disco-chatbuf--input-ring index)
     :preserve-history-navigation-p t))))

(defun disco-chatbuf-input-history-elements ()
  "Return current input history entries from newest to oldest."
  (let (items)
    (when (ring-p disco-chatbuf--input-ring)
      (dotimes (idx (ring-length disco-chatbuf--input-ring))
        (push (ring-ref disco-chatbuf--input-ring idx) items)))
    (nreverse items)))

(defun disco-chatbuf-input-history-reset ()
  "Clear shared input-history navigation state without altering history items."
  (setq disco-chatbuf--input-idx nil)
  (setq disco-chatbuf--input-pending nil))

(defun disco-chatbuf-input-history-prev-value (current-input &optional n)
  "Return N older history entries for cached CURRENT-INPUT.

CURRENT-INPUT is remembered as the pending latest value the first time history
navigation moves away from it.  The return value is a plist with `:status' set
to either `ok' or `empty'.  When the status is `ok', `:value' contains the
selected history string, preserving text properties of CURRENT-INPUT when it is
restored later via `disco-chatbuf-input-history-next-value'."
  (let* ((step (max 1 (or n 1)))
         (ring-size (and (ring-p disco-chatbuf--input-ring)
                         (ring-length disco-chatbuf--input-ring))))
    (if (or (null ring-size) (= ring-size 0))
        (list :status 'empty)
      (unless (integerp disco-chatbuf--input-idx)
        (setq disco-chatbuf--input-pending
              (disco-chatbuf-copy-string current-input))
        (setq disco-chatbuf--input-idx -1))
      (setq disco-chatbuf--input-idx
            (min (1- ring-size) (+ disco-chatbuf--input-idx step)))
      (list :status 'ok
            :value (disco-chatbuf-copy-string
                    (ring-ref disco-chatbuf--input-ring
                              disco-chatbuf--input-idx))))))

(defun disco-chatbuf-input-history-next-value (&optional n)
  "Return N newer history entries for cached chatbuf input state.

The return value is a plist with `:status' set to either `ok' or `latest'.
When the status is `ok', `:value' contains the selected history string or the
remembered pending latest input when navigation returns to the newest state."
  (let ((step (max 1 (or n 1))))
    (cond
     ((null disco-chatbuf--input-idx)
      (list :status 'latest))
     ((<= disco-chatbuf--input-idx (1- step))
      (let ((value (disco-chatbuf-copy-string
                    (or disco-chatbuf--input-pending ""))))
        (setq disco-chatbuf--input-idx nil)
        (setq disco-chatbuf--input-pending nil)
        (list :status 'ok :value value)))
     (t
      (setq disco-chatbuf--input-idx (- disco-chatbuf--input-idx step))
      (list :status 'ok
            :value (disco-chatbuf-copy-string
                    (ring-ref disco-chatbuf--input-ring
                              disco-chatbuf--input-idx)))))))

(defun disco-chatbuf-input-history-prev (&optional n)
  "Replace input with N previous entries from input history."
  (interactive "p")
  (let ((result (disco-chatbuf-input-history-prev-value
                 (disco-chatbuf-input-string)
                 n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-chatbuf-input-set-text
        (plist-get result :value)
        :preserve-history-navigation-p t))
      (_
       (user-error "disco-chatbuf: input history is empty")))))

(defun disco-chatbuf-input-history-next (&optional n)
  "Replace input with N newer entries from input history."
  (interactive "p")
  (let ((result (disco-chatbuf-input-history-next-value n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-chatbuf-input-set-text
        (plist-get result :value)
        :preserve-history-navigation-p t))
      (_
       (user-error "disco-chatbuf: already at latest input")))))

(provide 'disco-chatbuf)

;;; disco-chatbuf.el ends here
