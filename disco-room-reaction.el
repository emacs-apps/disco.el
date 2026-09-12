;;; disco-room-reaction.el --- Reaction interaction for Disco rooms -*- lexical-binding: t; -*-

;;; Commentary:

;; Room-local reaction state and mutations.  The room controller retains
;; Appkit view lifecycle, Gateway dispatch, and timeline projection.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-completion)
(require 'disco-api)
(require 'disco-company)
(require 'disco-customize)
(require 'disco-ins)
(require 'disco-msg)
(require 'disco-room-compose)

(declare-function disco-room--async-error-message "disco-room" (err))
(declare-function disco-room--channel-buffer-p "disco-room" (buffer channel-id view))
(declare-function disco-room--ensure-surface "disco-room" ())
(declare-function disco-room--event-self-p "disco-room" (event))
(declare-function disco-room--message-at-point "disco-room" ())
(declare-function disco-room--message-by-id "disco-room" (message-id))
(declare-function disco-room--message-id-at-point "disco-room" ())
(declare-function disco-room--request-render "disco-room" (view))
(declare-function disco-room--update-message-locally "disco-room" (message-id function))

(defvar disco-room--channel-id)

(defun disco-room-reaction-insert (message prefix)
  "Insert MESSAGE reaction chips using timeline PREFIX."
  (when disco-room-show-reactions
    (let ((message-id (alist-get 'id message)))
      (disco-ins-insert-reaction-line
       (disco-msg-reactions message)
       :prefix prefix
       :selected-face 'disco-room-reaction-selected
       :unselected-face 'disco-room-reaction
       :line-face 'disco-room-message-meta
       :action-function
       (lambda (reaction)
         (disco-room-toggle-reaction
          (disco-room--event-emoji->input
           (or (alist-get 'emoji reaction)
               (alist-get 'emoji_name reaction)))
          message-id))
       :help-echo-function
       (lambda (reaction)
         (if (disco-msg-reaction-selected-p reaction)
             "Remove your reaction"
           "Add your reaction"))))))

(defvar-local disco-room--reaction-op-seq 0
  "Monotonic owner token for reaction requests in this room view.")
(defvar-local disco-room--reaction-ops nil
  "Current reaction operation keyed by message id and emoji identity.")

(defun disco-room-reaction-reset ()
  "Reset reaction-local state in the current room buffer."
  (setq-local disco-room--reaction-op-seq 0
              disco-room--reaction-ops (make-hash-table :test #'equal)))

(defun disco-room-reaction-forget-message (message-id)
  "Discard reaction-local state belonging to deleted MESSAGE-ID."
  (disco-room--reaction-ops-clear-message message-id))

(defun disco-room--reaction-unavailable-reason (&optional _msg)
  "Return reason reaction actions are unavailable, or nil."
  (disco-room--room-send-restriction-reason '(add-reactions)))

(defun disco-room--parse-reaction-input (emoji)
  "Parse user EMOJI input into plist with :id/:name.

Accepted forms: Unicode emoji, `name:id`, or `<:name:id>`/`<a:name:id>`."
  (let ((raw (string-trim (or emoji ""))))
    (cond
     ((string-match "^<a?:\\([^:>]+\\):\\([0-9]+\\)>$" raw)
      (list :name (match-string 1 raw)
            :id (match-string 2 raw)))
     ((string-match "^\\([^:]+\\):\\([0-9]+\\)$" raw)
      (list :name (match-string 1 raw)
            :id (match-string 2 raw)))
     (t
      (list :name raw :id nil)))))

(defun disco-room--reaction-op-key (message-id emoji)
  "Return stable operation key for MESSAGE-ID and EMOJI.

Custom emoji identity is its id, independent of a later name change."
  (let* ((spec (disco-room--parse-reaction-input emoji))
         (emoji-id (plist-get spec :id))
         (emoji-name (plist-get spec :name)))
    (list (format "%s" message-id)
          (if emoji-id
              (cons 'id (format "%s" emoji-id))
            (cons 'name emoji-name)))))

(defun disco-room--reaction-op-begin (message-id emoji addp)
  "Begin and return an owner token for MESSAGE-ID/EMOJI reaction ADDP."
  (unless (hash-table-p disco-room--reaction-ops)
    (setq disco-room--reaction-ops (make-hash-table :test #'equal)))
  (let* ((key (disco-room--reaction-op-key message-id emoji))
         (token (cl-incf disco-room--reaction-op-seq)))
    (puthash key (list :token token :addp (and addp t))
             disco-room--reaction-ops)
    token))

(defun disco-room--reaction-op-current-p (message-id emoji token)
  "Return non-nil when TOKEN still owns MESSAGE-ID/EMOJI reaction."
  (let ((operation
         (and (hash-table-p disco-room--reaction-ops)
              (gethash (disco-room--reaction-op-key message-id emoji)
                       disco-room--reaction-ops))))
    (and (listp operation)
         (= (or (plist-get operation :token) -1) token))))

(defun disco-room--reaction-op-finish (message-id emoji token)
  "Finish MESSAGE-ID/EMOJI reaction when TOKEN still owns it."
  (when (disco-room--reaction-op-current-p message-id emoji token)
    (remhash (disco-room--reaction-op-key message-id emoji)
             disco-room--reaction-ops)
    t))

(defun disco-room--reaction-op-confirm-gateway (message-id emoji addp)
  "Finish current MESSAGE-ID/EMOJI operation confirmed by Gateway ADDP."
  (let* ((key (disco-room--reaction-op-key message-id emoji))
         (operation (and (hash-table-p disco-room--reaction-ops)
                         (gethash key disco-room--reaction-ops))))
    (when (and (listp operation)
               (eq (and (plist-get operation :addp) t) (and addp t)))
      (remhash key disco-room--reaction-ops)
      t)))

(defun disco-room--reaction-ops-clear-message (message-id)
  "Invalidate all pending reaction operations for MESSAGE-ID."
  (when (hash-table-p disco-room--reaction-ops)
    (let (keys)
      (maphash (lambda (key _operation)
                 (when (equal (car key) (format "%s" message-id))
                   (push key keys)))
               disco-room--reaction-ops)
      (dolist (key keys)
        (remhash key disco-room--reaction-ops)))))

(defun disco-room--reaction-op-clear-emoji (message-id emoji)
  "Invalidate the pending MESSAGE-ID/EMOJI reaction operation."
  (when (hash-table-p disco-room--reaction-ops)
    (remhash (disco-room--reaction-op-key message-id emoji)
             disco-room--reaction-ops)))

(defun disco-room--reaction-matches-input-p (reaction emoji)
  "Return non-nil when REACTION matches EMOJI input string."
  (let* ((spec (disco-room--parse-reaction-input emoji))
         (target-id (plist-get spec :id))
         (target-name (plist-get spec :name))
         (emoji-obj (alist-get 'emoji reaction))
         (reaction-id (and (listp emoji-obj) (alist-get 'id emoji-obj)))
         (reaction-name (disco-msg-reaction-emoji reaction)))
    (if target-id
        (and reaction-id (equal (format "%s" reaction-id) (format "%s" target-id)))
      (equal reaction-name target-name))))

(defun disco-room--message-has-own-reaction-p (msg emoji)
  "Return non-nil when MSG has current-user reaction EMOJI."
  (let ((found nil))
    (dolist (reaction (disco-msg-reactions msg))
      (when (and (disco-room--reaction-matches-input-p reaction emoji)
                 (disco-msg-reaction-selected-p reaction))
        (setq found t)))
    found))

(defun disco-room--message-with-reaction-delta
    (msg emoji addp update-own-selection-p)
  "Return MSG copy after applying one reaction delta for EMOJI.

When ADDP is non-nil, add one reaction; otherwise remove one.  When
UPDATE-OWN-SELECTION-P is non-nil, update current-user selection and make the
transition idempotent against REST/Gateway echoes.  Otherwise preserve current
user selection while applying another user's count delta."
  (let* ((updated (copy-tree msg))
         (reactions (copy-tree (disco-msg-reactions msg)))
         (spec (disco-room--parse-reaction-input emoji))
         (target-id (plist-get spec :id))
         (target-name (plist-get spec :name))
         (found nil)
         (next '()))
    (dolist (reaction reactions)
      (if (disco-room--reaction-matches-input-p reaction emoji)
          (let* ((count (max 0 (or (disco-msg-reaction-count reaction) 0)))
                 (was-selected (disco-msg-reaction-selected-p reaction))
                 (change-count-p
                  (or (not update-own-selection-p)
                      (not (eq (and was-selected t) (and addp t)))))
                 (next-count
                  (if (not change-count-p)
                      count
                    (if addp
                        (1+ count)
                      ;; Preserve the invariant that an own selection implies
                      ;; at least one aggregate reaction when another user
                      ;; removes theirs.
                      (max (if (and (not update-own-selection-p)
                                    was-selected)
                               1
                             0)
                           (1- count)))))
                 (item (copy-tree reaction)))
            (setq found t)
            (setf (alist-get 'count item nil 'remove) next-count)
            (when (assq 'total_count item)
              (setf (alist-get 'total_count item nil 'remove) next-count))
            (when update-own-selection-p
              (setf (alist-get 'me item nil 'remove) (if addp t :false))
              (when (assq 'is_chosen item)
                (setf (alist-get 'is_chosen item nil 'remove)
                      (if addp t :false))))
            (when (> next-count 0)
              (push item next)))
        (push reaction next)))
    (unless (or found (not addp))
      (push `((count . 1)
              (me . ,(if update-own-selection-p t :false))
              (emoji . ((name . ,target-name)
                        (id . ,target-id))))
            next))
    (setf (alist-get 'reactions updated nil 'remove) (nreverse next))
    updated))

(defun disco-room--event-emoji->input (emoji)
  "Normalize gateway EMOJI payload into reaction input string."
  (cond
   ((and (listp emoji) (alist-get 'id emoji))
    (format "%s:%s"
            (or (alist-get 'name emoji) "_")
            (alist-get 'id emoji)))
   ((and (listp emoji) (alist-get 'name emoji))
    (alist-get 'name emoji))
   ((stringp emoji)
    emoji)
   (t nil)))

(defun disco-room--message-cleared-reactions (msg)
  "Return MSG copy with all reactions removed."
  (let ((updated (copy-tree msg)))
    (setf (alist-get 'reactions updated nil 'remove) '())
    (setf (alist-get 'reaction_counts updated nil 'remove) '())
    updated))

(defun disco-room--message-removed-reaction-emoji (msg emoji)
  "Return MSG copy with reaction EMOJI removed completely."
  (let* ((updated (copy-tree msg))
         (reactions (copy-tree (disco-msg-reactions msg)))
         (next '()))
    (dolist (reaction reactions)
      (unless (disco-room--reaction-matches-input-p reaction emoji)
        (push reaction next)))
    (setf (alist-get 'reactions updated nil 'remove) (nreverse next))
    updated))

(defun disco-room--apply-live-reaction-event (event)
  "Apply reaction EVENT to local room state and projected timeline.

Return non-nil when a local message update was applied."
  (let* ((event-type (plist-get event :type))
         (message-id (plist-get event :message-id))
         (emoji-input (disco-room--event-emoji->input (plist-get event :emoji)))
         (is-self (disco-room--event-self-p event)))
    (pcase event-type
      ('message-reaction-add
       (let ((applied
              (and (stringp emoji-input)
                   (disco-room--update-message-locally
                    message-id
                    (lambda (msg)
                      (disco-room--message-with-reaction-delta
                       msg emoji-input t is-self))))))
         (when (and applied is-self)
           (disco-room--reaction-op-confirm-gateway
            message-id emoji-input t))
         applied))
      ('message-reaction-remove
       (let ((applied
              (and (stringp emoji-input)
                   (disco-room--update-message-locally
                    message-id
                    (lambda (msg)
                      (disco-room--message-with-reaction-delta
                       msg emoji-input nil is-self))))))
         (when (and applied is-self)
           (disco-room--reaction-op-confirm-gateway
            message-id emoji-input nil))
         applied))
      ('message-reaction-remove-all
       (prog1
           (disco-room--update-message-locally
            message-id
            #'disco-room--message-cleared-reactions)
         (disco-room--reaction-ops-clear-message message-id)))
      ('message-reaction-remove-emoji
       (prog1
           (and (stringp emoji-input)
                (disco-room--update-message-locally
                 message-id
                 (lambda (msg)
                   (disco-room--message-removed-reaction-emoji
                    msg emoji-input))))
         (when (stringp emoji-input)
           (disco-room--reaction-op-clear-emoji message-id emoji-input))))
      (_ nil))))

(defun disco-room--default-reaction-emoji (msg)
  "Return best default reaction emoji suggestion from MSG."
  (let* ((reactions (disco-msg-reactions msg))
         (selected (seq-find #'disco-msg-reaction-selected-p reactions))
         (candidate (or selected (car reactions))))
    (or (and candidate (disco-msg-reaction-emoji candidate))
        "👍")))

(defvar disco-room--reaction-emoji-history nil
  "Minibuffer history for visual Discord reaction readers.")

(defun disco-room--reaction-default-candidate (candidates default)
  "Return the CANDIDATES entry matching reaction DEFAULT, or nil."
  (when (and (stringp default)
             (not (string-empty-p (string-trim default))))
    (let ((target (downcase (string-trim default))))
      (seq-find
       (lambda (candidate)
         (let ((terms
                (appkit-chat-completion-candidate-search-terms candidate)))
           (seq-some
            (lambda (value)
              (and (stringp value)
                   (string-equal target (downcase (string-trim value)))))
            (cons
             (appkit-chat-completion-candidate-insert candidate)
             (cond
              ((stringp terms) (list terms))
              ((listp terms) terms))))))
       candidates))))

(defun disco-room--read-reaction-emoji
    (prompt &optional default message own-only)
  "Read a Discord reaction with PROMPT and optional DEFAULT.
MESSAGE supplies aggregate reactions and custom emoji occurring in its content.
When OWN-ONLY is non-nil, offer only reactions selected by the current user.
Use the bounded guild and Unicode visual catalog when available.  Older Emacs
versions or rooms without a catalog retain the unrestricted text fallback."
  (let ((candidates
         (disco-company-reaction-candidates message own-only)))
    (if (null candidates)
        (let ((emoji
               (string-trim
                (read-string (format-prompt prompt default)
                             nil nil default))))
          (if (string-empty-p emoji)
              (or default (user-error "disco: emoji cannot be empty"))
            emoji))
      (let* ((default-candidate
              (disco-room--reaction-default-candidate candidates default))
             (default-candidate
              (or
               default-candidate
               (when (and (stringp default)
                          (not (string-empty-p (string-trim default))))
                 (appkit-chat-completion-candidate-create
                  :label (string-trim default)
                  :insert (string-trim default)
                  :search-terms (list (string-trim default))
                  :value '(:kind default-reaction)))))
             (candidates
              (if (and default-candidate
                       (not (memq default-candidate candidates)))
                  (cons default-candidate candidates)
                candidates))
             (candidate
              (appkit-chat-completion-read-visual
               (format-prompt prompt default)
               candidates
               :history 'disco-room--reaction-emoji-history
               :default-candidate default-candidate))
             (emoji
              (appkit-chat-completion-candidate-insert candidate)))
        (unless (and (stringp emoji)
                     (not (string-empty-p (string-trim emoji))))
          (error "disco: reaction candidate has no insertion identity"))
        emoji))))

(defun disco-room--add-reaction-to-msg (msg)
  "Prompt for and add a reaction to MSG."
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason msg)
   "add reactions")
  (let* ((default (disco-room--default-reaction-emoji msg))
         (picked
          (disco-room--read-reaction-emoji
           "Add reaction" default msg)))
    (disco-room-add-reaction picked (alist-get 'id msg))))

(defun disco-room-add-reaction (&optional emoji message-id)
  "Add EMOJI reaction to MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message"))))
     (disco-room--ensure-action-available
      (disco-room--reaction-unavailable-reason msg)
      "add reactions")
     (let* ((default (disco-room--default-reaction-emoji msg))
            (picked
             (disco-room--read-reaction-emoji
              "Add reaction" default msg)))
       (list picked (alist-get 'id msg)))))
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason)
   "add reactions")
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         (emoji-text emoji))
    (let ((op-token
           (disco-room--reaction-op-begin target-id emoji-text t)))
      (disco-api-add-reaction-async
       channel-id
       target-id
       emoji-text
       :on-success
       (lambda (_response)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--reaction-op-current-p
                    target-id emoji-text op-token)
               (disco-room--update-message-locally
                target-id
                (lambda (msg)
                  (disco-room--message-with-reaction-delta
                   msg emoji-text t t)))
               (disco-room--reaction-op-finish
                target-id emoji-text op-token)
               (disco-room--queue-update view (list 'rows-changed (list target-id)))

               (message "disco: reaction added (%s)" emoji-text)))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--reaction-op-finish
                    target-id emoji-text op-token)
               (message "disco: add reaction failed: %s"
                        (disco-room--async-error-message err))))))))))

(defun disco-room--remove-reaction-from-msg (msg)
  "Prompt for and remove a reaction from MSG."
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason msg)
   "remove reactions")
  (let* ((reactions
          (seq-filter
           #'disco-msg-reaction-selected-p
           (disco-msg-reactions msg))))
    (unless reactions
      (user-error "disco: this message has no reaction from you"))
    (let* ((default (disco-msg-reaction-emoji (car reactions)))
           (picked
            (disco-room--read-reaction-emoji
             "Remove reaction" default msg t)))
      (disco-room-remove-reaction picked (alist-get 'id msg)))))

(defun disco-room-remove-reaction (&optional emoji message-id)
  "Remove current user's EMOJI reaction from MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message"))))
     (disco-room--ensure-action-available
      (disco-room--reaction-unavailable-reason msg)
      "remove reactions")
     (let* ((reactions
             (seq-filter
              #'disco-msg-reaction-selected-p
              (disco-msg-reactions msg))))
       (unless reactions
         (user-error "disco: this message has no reaction from you"))
       (let* ((default (disco-msg-reaction-emoji (car reactions)))
              (picked
               (disco-room--read-reaction-emoji
                "Remove reaction" default msg t)))
         (list picked (alist-get 'id msg))))))
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason)
   "remove reactions")
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         (emoji-text emoji))
    (let ((op-token
           (disco-room--reaction-op-begin target-id emoji-text nil)))
      (disco-api-remove-own-reaction-async
       channel-id
       target-id
       emoji-text
       :on-success
       (lambda (_response)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--reaction-op-current-p
                    target-id emoji-text op-token)
               (disco-room--update-message-locally
                target-id
                (lambda (msg)
                  (disco-room--message-with-reaction-delta
                   msg emoji-text nil t)))
               (disco-room--reaction-op-finish
                target-id emoji-text op-token)
               (disco-room--queue-update view (list 'rows-changed (list target-id)))

               (message "disco: reaction removed (%s)" emoji-text)))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--reaction-op-finish
                    target-id emoji-text op-token)
               (message "disco: remove reaction failed: %s"
                        (disco-room--async-error-message err))))))))))

(defun disco-room--toggle-reaction-on-msg (msg)
  "Prompt for and toggle a reaction on MSG."
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason msg)
   "toggle reactions")
  (let* ((default (disco-room--default-reaction-emoji msg))
         (picked
          (disco-room--read-reaction-emoji
           "Toggle reaction" default msg)))
    (disco-room-toggle-reaction picked (alist-get 'id msg))))

(defun disco-room-toggle-reaction (&optional emoji message-id)
  "Toggle current user's EMOJI reaction on MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message"))))
     (disco-room--ensure-action-available
      (disco-room--reaction-unavailable-reason msg)
      "toggle reactions")
     (let* ((default (disco-room--default-reaction-emoji msg))
            (picked
             (disco-room--read-reaction-emoji
              "Toggle reaction" default msg)))
       (list picked (alist-get 'id msg)))))
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason)
   "toggle reactions")
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (msg (or (disco-room--message-by-id target-id)
                  (and (null message-id)
                       (disco-room--message-at-point))
                  (user-error "disco: message not found in room state"))))
    (if (disco-room--message-has-own-reaction-p msg emoji)
        (disco-room-remove-reaction emoji target-id)
      (disco-room-add-reaction emoji target-id))))

(provide 'disco-room-reaction)

;;; disco-room-reaction.el ends here
