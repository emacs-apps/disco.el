;;; disco-room-poll.el --- Poll interaction for Disco rooms -*- lexical-binding: t; -*-

;;; Commentary:

;; Poll-local state, rendering, actions, and the nested poll transient.  The
;; room controller remains responsible for room lifecycle, Gateway dispatch,
;; and projection transactions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'appkit-ui)
(require 'disco-api)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-permission)
(require 'disco-state)
(require 'disco-room-compose)

(declare-function disco-room--async-error-message "disco-room" (err))
(declare-function disco-room--channel-buffer-p "disco-room" (buffer channel-id view))
(declare-function disco-room--channel-object "disco-room" ())
(declare-function disco-room--ensure-surface "disco-room" ())
(declare-function disco-room--event-self-p "disco-room" (event))
(declare-function disco-room--message-at-point "disco-room" ())
(declare-function disco-room--message-author-id "disco-room-render" (message))
(declare-function disco-room--message-by-id "disco-room" (message-id))
(declare-function disco-room--request-render "disco-room" (view))
(declare-function disco-room--update-message-locally "disco-room" (message-id function))
(declare-function disco-room-menu--message-at-point "disco-room" ())
(declare-function disco-room-refresh "disco-room" ())
(declare-function disco-room-render "disco-room" ())
(declare-function disco-api--validate-message-content-length
                  "disco-api-normalize" (content field-name))
(declare-function disco-room--channel-message-by-id
                  "disco-room" (channel-id message-id))
(declare-function disco-room--observe-live-create "disco-room" (message-id))

(defvar disco-room--channel-id)
(defvar disco-room--send-in-flight)

(defvar-local disco-room--poll-selection-drafts nil)
(defvar-local disco-room--poll-vote-op-seq 0
  "Monotonic owner token for poll vote requests in this room view.")
(defvar-local disco-room--poll-vote-ops nil
  "Current poll vote operation keyed by message id.")

(defun disco-room-poll-reset ()
  "Reset poll-local state in the current room buffer."
  (setq-local disco-room--poll-selection-drafts (make-hash-table :test #'equal)
              disco-room--poll-vote-op-seq 0
              disco-room--poll-vote-ops (make-hash-table :test #'equal)))

(defun disco-room-poll-forget-message (message-id)
  "Discard poll-local state belonging to deleted MESSAGE-ID."
  (disco-room--poll-clear-draft-selection message-id)
  (when (hash-table-p disco-room--poll-vote-ops)
    (remhash message-id disco-room--poll-vote-ops)))

(defun disco-room-poll-actionable-at-point-p ()
  "Return non-nil when point is on a poll with an available action."
  (let ((message (disco-room-menu--message-at-point)))
    (and (disco-msg-poll message)
         (or (not (disco-room--poll-vote-unavailable-reason message))
             (not (disco-room--poll-expire-unavailable-reason message))))))

(defun disco-room--poll-unavailable-reason ()
  "Return reason send-poll action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason '(send-polls))
      (when-let* ((aux (disco-room--composer-aux-context-name)))
        (format "cancel %s before sending a poll" aux))))

(defun disco-room--poll-vote-required-permissions (&optional channel)
  "Return required permissions for poll vote actions in CHANNEL."
  (disco-room--required-send-permissions channel))

(defun disco-room--poll-expire-required-permissions (&optional channel)
  "Return required permissions for poll expire action in CHANNEL."
  (append (disco-room--required-send-permissions channel)
          '(send-polls)))

(defun disco-room--poll-vote-unavailable-reason (&optional msg)
  "Return reason poll voting actions are unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (poll (and (listp msg) (disco-msg-poll msg))))
    (cond
     ((null msg)
      "point is not on a message")
     ((null poll)
      "point is not on a poll")
     ((disco-msg-poll-expired-p poll)
      "poll is closed")
     (t
      (disco-room--room-send-restriction-reason)))))

(defun disco-room--poll-submit-unavailable-reason (&optional msg)
  "Return reason staged poll submit is unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (base-reason (disco-room--poll-vote-unavailable-reason msg)))
    (or base-reason
        (let* ((target-id (alist-get 'id msg))
               (poll (disco-msg-poll msg))
               (staged (disco-room--poll-effective-selection target-id poll))
               (committed (disco-msg-poll-voted-answer-ids poll)))
          (cond
           ((null staged)
            "no staged poll selection")
           ((equal (disco-room--poll-selection-key staged)
                   (disco-room--poll-selection-key committed))
            "no pending poll vote changes"))))))

(defun disco-room--poll-clear-unavailable-reason (&optional msg)
  "Return reason clear-poll-votes is unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (base-reason (disco-room--poll-vote-unavailable-reason msg)))
    (or base-reason
        (let* ((poll (and (listp msg) (disco-msg-poll msg)))
               (committed (and poll (disco-msg-poll-voted-answer-ids poll))))
          (unless committed
            "no existing poll vote to remove")))))

(defun disco-room--poll-expire-unavailable-reason (&optional msg)
  "Return reason end-poll is unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (poll (and (listp msg) (disco-msg-poll msg))))
    (cond
     ((null msg)
      "point is not on a message")
     ((null poll)
      "point is not on a poll")
     ((disco-msg-poll-expired-p poll)
      "poll is already closed")
     ((not (disco-room--poll-owned-by-current-user-p msg nil))
      "only poll author can end this poll")
     (t
      (disco-room--room-send-restriction-reason '(send-polls))))))

(cl-defun disco-room--poll-owned-by-current-user-p (msg &optional (unknown-value t))
  "Return non-nil when poll in MSG is owned by current user.

If current user identity is unknown, return UNKNOWN-VALUE."
  (let* ((author-id (and (listp msg) (disco-room--message-author-id msg)))
         (self-id (disco-gateway-current-user-id)))
    (if (or (null author-id) (null self-id))
        unknown-value
      (equal (format "%s" author-id) (format "%s" self-id)))))

(defun disco-room--poll-can-vote-p (msg)
  "Return non-nil when current user can vote in poll message MSG."
  (let ((poll (disco-msg-poll msg)))
    (and poll
         (not (disco-msg-poll-expired-p poll))
         (disco-permission-channel-has-all-p
          (disco-room--channel-object)
          (disco-room--poll-vote-required-permissions)))))

(defun disco-room--poll-can-expire-p (msg)
  "Return non-nil when current user can end poll message MSG."
  (let ((poll (disco-msg-poll msg)))
    (and poll
         (not (disco-msg-poll-expired-p poll))
         (disco-room--poll-owned-by-current-user-p msg t)
         (disco-permission-channel-has-all-p
          (disco-room--channel-object)
          (disco-room--poll-expire-required-permissions)))))

(defun disco-room--poll-draft-selection (message-id)
  "Return staged poll selection list for MESSAGE-ID.

This may return nil when a staged empty selection exists."
  (when (and (hash-table-p disco-room--poll-selection-drafts)
             message-id)
    (let ((value (gethash message-id disco-room--poll-selection-drafts :disco--missing)))
      (unless (eq value :disco--missing)
        value))))

(defun disco-room--poll-draft-selection-present-p (message-id)
  "Return non-nil when MESSAGE-ID has a staged poll selection entry."
  (and (hash-table-p disco-room--poll-selection-drafts)
       message-id
       (not (eq (gethash message-id disco-room--poll-selection-drafts :disco--missing)
                :disco--missing))))

(defun disco-room--poll-set-draft-selection (message-id answer-ids)
  "Store staged poll ANSWER-IDS for MESSAGE-ID and return normalized list."
  (let ((normalized (disco-msg-poll-normalize-answer-id-list answer-ids)))
    (unless (hash-table-p disco-room--poll-selection-drafts)
      (setq disco-room--poll-selection-drafts (make-hash-table :test #'equal)))
    (if normalized
        (puthash message-id normalized disco-room--poll-selection-drafts)
      (puthash message-id '() disco-room--poll-selection-drafts))
    normalized))

(defun disco-room--poll-clear-draft-selection (message-id)
  "Clear staged poll selection for MESSAGE-ID."
  (when (and (hash-table-p disco-room--poll-selection-drafts)
             message-id)
    (remhash message-id disco-room--poll-selection-drafts)))

(defun disco-room--poll-selection-key (answer-ids)
  "Return canonical set-like key for poll ANSWER-IDS."
  (sort (copy-sequence
         (disco-msg-poll-normalize-answer-id-list answer-ids))
        #'<))

(defun disco-room--poll-vote-op-begin (message-id selected-answer-ids)
  "Begin and return an owner token for MESSAGE-ID vote selection."
  (unless (hash-table-p disco-room--poll-vote-ops)
    (setq disco-room--poll-vote-ops (make-hash-table :test #'equal)))
  (let ((token (cl-incf disco-room--poll-vote-op-seq)))
    (puthash message-id
             (list :token token
                   :target (disco-room--poll-selection-key
                            selected-answer-ids))
             disco-room--poll-vote-ops)
    token))

(defun disco-room--poll-vote-op-current-p (message-id token)
  "Return non-nil when TOKEN still owns MESSAGE-ID's vote operation."
  (let ((operation (and (hash-table-p disco-room--poll-vote-ops)
                        (gethash message-id disco-room--poll-vote-ops))))
    (and (listp operation)
         (= (or (plist-get operation :token) -1) token))))

(defun disco-room--poll-vote-op-finish (message-id token)
  "Finish MESSAGE-ID vote operation when TOKEN still owns it."
  (when (disco-room--poll-vote-op-current-p message-id token)
    (remhash message-id disco-room--poll-vote-ops)
    t))

(defun disco-room--poll-draft-matches-p (message-id selected-answer-ids)
  "Return non-nil when MESSAGE-ID's staged vote equals SELECTED-ANSWER-IDS."
  (and (disco-room--poll-draft-selection-present-p message-id)
       (equal (disco-room--poll-selection-key
               (disco-room--poll-draft-selection message-id))
              (disco-room--poll-selection-key selected-answer-ids))))

(defun disco-room--poll-vote-op-confirm-convergence (message-id)
  "Finish MESSAGE-ID's vote operation if Gateway state reached its target."
  (let* ((operation (and (hash-table-p disco-room--poll-vote-ops)
                         (gethash message-id disco-room--poll-vote-ops)))
         (target (and (listp operation) (plist-get operation :target)))
         (message (and operation (disco-room--message-by-id message-id)))
         (poll (and message (disco-msg-poll message)))
         (committed (and poll (disco-msg-poll-voted-answer-ids poll))))
    (when (and operation
               (equal (disco-room--poll-selection-key committed)
                      (disco-room--poll-selection-key target)))
      ;; Do not erase a newer, unsent draft that was staged while this request
      ;; was in flight.
      (when (disco-room--poll-draft-matches-p message-id target)
        (disco-room--poll-clear-draft-selection message-id))
      (remhash message-id disco-room--poll-vote-ops)
      t)))

(defun disco-room--poll-effective-selection (message-id poll)
  "Return effective UI selection for MESSAGE-ID in POLL.

Staged selection takes precedence over committed vote state."
  (if (disco-room--poll-draft-selection-present-p message-id)
      (disco-msg-poll-normalize-answer-id-list
       (disco-room--poll-draft-selection message-id))
    (disco-msg-poll-voted-answer-ids poll)))

(defun disco-room--poll-add-selection (message-id poll answer-id)
  "Return staged selection with ANSWER-ID added for MESSAGE-ID/POLL."
  (let ((current (copy-sequence (disco-room--poll-effective-selection message-id poll))))
    (if (disco-msg-poll-multiselect-p poll)
        (if (member answer-id current)
            current
          (append current (list answer-id)))
      (list answer-id))))

(defun disco-room--poll-toggle-draft-selection (message-id poll answer-id)
  "Return staged selection after toggling ANSWER-ID for MESSAGE-ID/POLL."
  (let* ((current (copy-sequence (disco-room--poll-effective-selection message-id poll)))
         (has (member answer-id current)))
    (if (disco-msg-poll-multiselect-p poll)
        (if has
            (delete answer-id current)
          (append current (list answer-id)))
      (if has
          '()
        (list answer-id)))))

(defun disco-room--poll-draft-differs-p (message-id poll)
  "Return non-nil when staged selection differs from committed vote state."
  (let ((draft (disco-room--poll-draft-selection message-id))
        (committed (disco-msg-poll-voted-answer-ids poll)))
    (and (disco-room--poll-draft-selection-present-p message-id)
         (not (equal (disco-room--poll-selection-key draft)
                     (disco-room--poll-selection-key committed))))))

(defun disco-room--poll-answer-selected-p (message-id poll answer-id)
  "Return non-nil when ANSWER-ID is selected in effective poll UI state."
  (member answer-id (disco-room--poll-effective-selection message-id poll)))

(defun disco-room--message-with-poll-vote-selection (msg selected-answer-ids)
  "Return MSG copy with current-user poll votes set to SELECTED-ANSWER-IDS."
  (let* ((updated (copy-tree msg))
         (poll (copy-tree (disco-msg-poll msg))))
    (if (not (listp poll))
        updated
      (let* ((results (copy-tree (or (disco-msg-poll-results poll) '())))
             (counts (copy-tree (or (alist-get 'answer_counts results) '())))
             (selected (delete-dups (copy-sequence (or selected-answer-ids '()))))
             (previous (disco-msg-poll-voted-answer-ids poll)))
        (dolist (answer-id (delete-dups (append previous selected)))
          (let* ((existing (seq-find (lambda (it)
                                       (equal (alist-get 'id it) answer-id))
                                     counts))
                 (entry (or (copy-tree existing)
                            `((id . ,answer-id)
                              (count . 0)
                              (me_voted . :false))))
                 (count (max 0 (or (alist-get 'count entry) 0)))
                 (was-voted (member answer-id previous))
                 (now-voted (member answer-id selected)))
            (when (and (not was-voted) now-voted)
              (setq count (1+ count)))
            (when (and was-voted (not now-voted))
              (setq count (max 0 (1- count))))
            (setf (alist-get 'count entry nil 'remove) count)
            (setf (alist-get 'me_voted entry nil 'remove) (if now-voted t :false))
            (if existing
                (setq counts (mapcar (lambda (it)
                                       (if (equal (alist-get 'id it) answer-id)
                                           entry
                                         it))
                                     counts))
              (setq counts (append counts (list entry))))))
        (setf (alist-get 'answer_counts results nil 'remove) counts)
        (setf (alist-get 'results poll nil 'remove) results)
        (setf (alist-get 'poll updated nil 'remove) poll)
        updated))))

(defun disco-room--message-with-poll-vote-delta (msg answer-id addp self-p)
  "Return MSG copy updated with one poll vote delta.

ANSWER-ID is the poll answer receiving update.  ADDP non-nil means add vote;
otherwise remove.  SELF-P non-nil makes the own-vote transition idempotent."
  (let* ((updated (copy-tree msg))
         (poll (copy-tree (disco-msg-poll msg))))
    (if (not (and (listp poll) (integerp answer-id)))
        updated
      (let* ((results (copy-tree (or (disco-msg-poll-results poll) '())))
             (counts (copy-tree (or (alist-get 'answer_counts results) '())))
             (is-self (and self-p t))
             (existing (seq-find (lambda (it)
                                   (equal (alist-get 'id it) answer-id))
                                 counts))
             (entry (or (copy-tree existing)
                        `((id . ,answer-id)
                          (count . 0)
                          (me_voted . :false))))
             (count (max 0 (or (alist-get 'count entry) 0)))
             (was-voted (eq (alist-get 'me_voted entry) t))
             ;; A self Gateway event and its REST completion describe the same
             ;; transition.  Own-selection state makes that transition
             ;; idempotent; votes from other users remain ordinary deltas.
             (change-count-p (or (not is-self)
                                 (not (eq (and was-voted t)
                                          (and addp t))))))
        (when change-count-p
          (setq count (if addp
                          (1+ count)
                        ;; A cached own vote is itself one vote.  An event for
                        ;; another user cannot reduce the aggregate below it.
                        (max (if (and (not is-self) was-voted) 1 0)
                             (1- count)))))
        (setf (alist-get 'count entry nil 'remove) count)
        (when is-self
          (setf (alist-get 'me_voted entry nil 'remove)
                (if addp t :false)))
        (if existing
            (setq counts (mapcar (lambda (it)
                                   (if (equal (alist-get 'id it) answer-id)
                                       entry
                                     it))
                                 counts))
          (setq counts (append counts (list entry))))
        (setf (alist-get 'answer_counts results nil 'remove) counts)
        (setf (alist-get 'results poll nil 'remove) results)
        (setf (alist-get 'poll updated nil 'remove) poll)
        updated))))

(defun disco-room--apply-live-poll-vote-event (event)
  "Apply poll vote EVENT to local room state and projected timeline."
  (let* ((event-type (plist-get event :type))
         (message-id (plist-get event :message-id))
         (raw-answer-id (plist-get event :answer-id))
         (answer-id (cond
                     ((integerp raw-answer-id) raw-answer-id)
                     ((and (stringp raw-answer-id)
                           (string-match-p "\\`[0-9]+\\'" raw-answer-id))
                      (string-to-number raw-answer-id))
                     (t nil)))
         (is-self (disco-room--event-self-p event))
         (applied
          (and (integerp answer-id)
               (pcase event-type
                 ('message-poll-vote-add
                  (disco-room--update-message-locally
                   message-id
                   (lambda (msg)
                     (disco-room--message-with-poll-vote-delta
                      msg answer-id t is-self))))
                 ('message-poll-vote-remove
                  (disco-room--update-message-locally
                   message-id
                   (lambda (msg)
                     (disco-room--message-with-poll-vote-delta
                      msg answer-id nil is-self))))
                 (_ nil)))))
    (when (and applied is-self)
      ;; A multi-select request can produce several Gateway events.  Keep the
      ;; staged selection until canonical own-vote state has fully converged,
      ;; and never clear a newer draft merely because an older echo arrived.
      (disco-room--poll-vote-op-confirm-convergence message-id))
    applied))

(defun disco-room--insert-message-poll (msg)
  "Insert poll detail block for MSG when present."
  (when disco-room-show-polls
    (let* ((poll (disco-msg-poll msg))
           (message-id (alist-get 'id msg))
           (question (and poll (disco-msg-poll-question-text poll)))
           (state (and poll (disco-msg-poll-state-label poll)))
           (expiry-label (and poll
                              (disco-msg-poll-expiry-label
                               poll disco-room-poll-date-format)))
           (answers (and poll (or (alist-get 'answers poll) '())))
           (committed-selection (and poll (disco-msg-poll-voted-answer-ids poll)))
           (effective-selection (and poll (disco-room--poll-effective-selection message-id poll)))
           (draft-differs (and poll (disco-room--poll-draft-differs-p message-id poll)))
           (can-vote (and poll (disco-room--poll-can-vote-p msg)))
           (can-expire (and poll (disco-room--poll-can-expire-p msg))))
      (when poll
        (let ((prefix-state (appkit-ui-card-prefix-state :face 'disco-room-attachment-card-border)))
          (let ((title-start (point)))
            (insert "[poll] " question "\n")
            (appkit-ui-apply-line-prefix title-start (point) prefix-state)
            (add-text-properties title-start (point)
                                 `(disco-message-id ,message-id))
            (appkit-ui-append-face
             title-start (point) disco-room-poll-title-face))
          (let ((meta-start (point))
                (parts (list (format "status=%s" state))))
            (when (disco-msg-poll-multiselect-p poll)
              (setq parts (append parts '("multi"))))
            (when (and disco-room-poll-show-total-votes
                       (disco-msg-poll-results poll))
              (setq parts
                    (append parts
                            (list (format "votes=%d"
                                          (disco-msg-poll-total-votes poll))))))
            (when expiry-label
              (setq parts (append parts (list (format "ends=%s" expiry-label)))))
            (insert (mapconcat #'identity parts "   ") "\n")
            (appkit-ui-apply-line-prefix meta-start (point) prefix-state)
            (add-text-properties meta-start (point)
                                 `(disco-message-id ,message-id))
            (appkit-ui-append-face
             meta-start (point) disco-room-poll-meta-face))
          (dolist (answer answers)
            (let* ((answer-id (disco-msg-poll-answer-id answer))
                   (selected (and answer-id
                                  (member answer-id effective-selection)))
                   (count (and answer-id
                               (disco-msg-poll-answer-count poll answer-id)))
                   (emoji (disco-msg-poll-answer-emoji answer))
                   (label (disco-msg-poll-answer-text answer))
                   (line-start (point)))
              (if (and can-vote answer-id disco-room-poll-auto-toggle-vote)
                  (appkit-ui-insert-action-button
                   (format "%s %s%s"
                           (if selected "[x]" "[ ]")
                           (if emoji (concat emoji " ") "")
                           label)
                   (lambda ()
                     (disco-room-toggle-poll-answer answer-id message-id))
                   :face (if selected
                             disco-room-poll-voted-face
                           disco-room-poll-option-face)
                   :help-echo "Toggle staged selection for this answer")
                (insert (propertize
                         (format "%s %s%s"
                                 (if selected "[x]" "[ ]")
                                 (if emoji (concat emoji " ") "")
                                 label)
                         'face (if selected
                                   disco-room-poll-voted-face
                                 disco-room-poll-option-face))))
              (when (and disco-room-poll-show-voter-counts (integerp count))
                (insert (propertize (format "  (%d)" count)
                                    'face disco-room-poll-meta-face)))
              (insert "\n")
              (appkit-ui-apply-line-prefix line-start (point) prefix-state)
              (add-text-properties line-start (point)
                                   `(disco-message-id ,message-id
                                     disco-poll-answer-id ,answer-id))))
          (let ((actions-start (point))
                (inserted nil))
            (when (and can-vote draft-differs effective-selection)
              (appkit-ui-insert-action-button
               "[Vote]"
               (lambda ()
                 (disco-room-submit-poll-vote message-id))
               :face disco-room-poll-button-face
               :help-echo "Submit selected poll answers")
              (insert " ")
              (setq inserted t))
            (when (and can-vote
                       committed-selection
                       (or (not draft-differs)
                           (null effective-selection)))
              (appkit-ui-insert-action-button
               "[Remove vote]"
               (lambda ()
                 (disco-room-clear-poll-votes message-id))
               :face disco-room-poll-button-face
               :help-echo "Remove all my poll votes")
              (insert " ")
              (setq inserted t))
            (when can-expire
              (appkit-ui-insert-action-button
               "[End poll]"
               (lambda ()
                 (disco-room-expire-poll message-id))
               :face disco-room-poll-button-face
               :help-echo "End this poll now")
              (setq inserted t))
            (unless inserted
              (insert (propertize "[no poll actions available]" 'face 'shadow)))
            (insert "\n")
            (appkit-ui-apply-line-prefix actions-start (point) prefix-state)
            (add-text-properties actions-start (point)
                                 `(disco-message-id ,message-id))
            (appkit-ui-append-face
             actions-start (point) disco-room-poll-meta-face)))))))

(defun disco-room--poll-message-required (&optional message-id)
  "Return poll message object by MESSAGE-ID or point, or raise user error."
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (msg (or (disco-room--message-by-id target-id)
                  (user-error "disco: message not found in room state")))
         (poll (disco-msg-poll msg)))
    (unless poll
      (user-error "disco: message %s has no poll" target-id))
    msg))

(defun disco-room--poll-answer-id-at-point ()
  "Return poll answer id text property at point, or nil."
  (let ((raw (or (get-text-property (point) 'disco-poll-answer-id)
                 (save-excursion
                   (beginning-of-line)
                   (get-text-property (point) 'disco-poll-answer-id)))))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

(defun disco-room--poll-answer-choices (msg)
  "Return completion choices for poll answers in MSG.

Each item is (LABEL . ANSWER-ID)."
  (let* ((poll (or (disco-msg-poll msg) '()))
         (answers (or (alist-get 'answers poll) '()))
         out)
    (dolist (answer answers (nreverse out))
      (let ((answer-id (disco-msg-poll-answer-id answer)))
        (when answer-id
          (let* ((emoji (disco-msg-poll-answer-emoji answer))
                 (text (disco-msg-poll-answer-text answer))
                 (label (format "%d: %s%s"
                                answer-id
                                (if emoji (concat emoji " ") "")
                                text)))
            (push (cons label answer-id) out)))))))

(defun disco-room--read-poll-answer-id (msg &optional default)
  "Prompt poll answer id for MSG, using DEFAULT answer id when provided."
  (let* ((choices (disco-room--poll-answer-choices msg))
         (labels (mapcar #'car choices))
         (default-label (and default
                             (car (rassoc default choices))))
         (picked (completing-read
                  (if default-label
                      (format "Poll answer (default %s): " default-label)
                    "Poll answer: ")
                  labels
                  nil
                  t
                  nil
                  nil
                  default-label)))
    (or (cdr (assoc picked choices))
        default
        (user-error "disco: invalid poll answer"))))

(defun disco-room-send-poll (question options &optional duration allow-multiselect content)
  "Create and send a poll with QUESTION and OPTIONS in current room.

DURATION is in hours. ALLOW-MULTISELECT toggles multi-select behavior.
CONTENT is optional extra text sent alongside the poll."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--poll-unavailable-reason)
      "send polls")
     (let* ((question-input (string-trim (read-string "Poll question: ")))
            (duration-input (read-number "Poll duration (hours): "
                                         disco-room-poll-default-duration-hours))
            (allow-multi (y-or-n-p "Allow multiple answers? "))
            (content-input (string-trim (read-string "Optional message content: ")))
            (max-options (max 2 disco-room-poll-max-options))
            (idx 1)
            (options nil)
            opt)
       (while (and (<= idx max-options)
                   (not (string-empty-p
                         (setq opt (string-trim
                                    (read-string
                                     (format "Option %d (empty to finish): " idx)))))))
         (push opt options)
         (setq idx (1+ idx)))
       (unless (and (stringp question-input)
                    (not (string-empty-p question-input)))
         (user-error "disco: poll question cannot be empty"))
       (unless (>= (length options) 2)
         (user-error "disco: poll requires at least 2 options"))
       (list question-input
             (nreverse options)
             duration-input
             allow-multi
             (unless (string-empty-p content-input)
               content-input)))))
  (disco-room--ensure-action-available
   (disco-room--poll-unavailable-reason)
   "send polls")
  (let* ((poll `((question . ((text . ,question)))
                 (answers . ,(mapcar (lambda (option)
                                       `((poll_media . ((text . ,option)))))
                                     options))
                 (duration . ,duration)
                 (allow_multiselect . ,(if allow-multiselect t :false))))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         request-revision
         (required-permissions
          (append (disco-room--required-send-permissions)
                  '(send-polls))))
    (disco-api--validate-message-content-length content "content")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     required-permissions
     :action "sending poll")
    (setq request-revision
          (disco-state-message-revision channel-id))
    (setq disco-room--send-in-flight t)
    (disco-room--queue-update view 'frame)

    (disco-api-create-message-async
     channel-id
     :content content
     :poll poll
     :allowed-mentions (disco-room--send-allowed-mentions)
     :on-success
     (lambda (response)
       (when (and (listp response) (alist-get 'id response))
         (disco-state-merge-message-response
          channel-id response request-revision))
       (when (disco-room--channel-buffer-p room-buffer channel-id view)
         (with-current-buffer room-buffer
           (setq disco-room--send-in-flight nil)
           (cond
            ((and (listp response)
                  (alist-get 'id response)
                  (disco-room--channel-message-by-id
                   channel-id (alist-get 'id response)))
             (disco-room--observe-live-create (alist-get 'id response)))
            ((not (and (listp response) (alist-get 'id response)))
             (disco-room-refresh)))
           (disco-room--request-render view)
           (message "disco: poll sent"))))
     :on-error
     (lambda (err)
       (when (disco-room--channel-buffer-p room-buffer channel-id view)
         (with-current-buffer room-buffer
           (setq disco-room--send-in-flight nil)
           (disco-room--request-render view)
           (message "disco: send poll failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room--submit-poll-vote (message-id selected-answer-ids)
  "Submit SELECTED-ANSWER-IDS for poll MESSAGE-ID asynchronously."
  (let* ((msg (disco-room--poll-message-required message-id))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         (target-id (alist-get 'id msg))
         (normalized (disco-msg-poll-normalize-answer-id-list selected-answer-ids)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "vote in polls")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     (disco-room--poll-vote-required-permissions)
     :action "poll voting")
    (let ((op-token (disco-room--poll-vote-op-begin target-id normalized)))
      (disco-api-create-poll-vote-async
       channel-id
       target-id
       normalized
       :on-success
       (lambda (_response)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             ;; Only the newest request for this poll may replace the complete
             ;; own-selection snapshot.  Gateway deltas remain authoritative
             ;; and the selection transform itself is echo-idempotent.
             (when (disco-room--poll-vote-op-current-p target-id op-token)
               (disco-room--update-message-locally
                target-id
                (lambda (message)
                  (disco-room--message-with-poll-vote-selection
                   message normalized)))
               (when (disco-room--poll-draft-matches-p target-id normalized)
                 (disco-room--poll-clear-draft-selection target-id))
               (disco-room--poll-vote-op-finish target-id op-token)
               (disco-room--queue-update view (list 'rows-changed (list target-id)))

               (message "disco: poll vote updated")))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--poll-vote-op-finish target-id op-token)
               (message "disco: poll vote failed: %s"
                        (disco-room--async-error-message err))))))))))

(defun disco-room--pick-poll-answer-id (msg &optional explicit-answer-id)
  "Return poll answer id from EXPLICIT-ANSWER-ID, point, or prompt for MSG."
  (or explicit-answer-id
      (disco-room--poll-answer-id-at-point)
      (disco-room--read-poll-answer-id msg nil)))

(defun disco-room--stage-poll-selection (message-id selection)
  "Stage poll SELECTION for MESSAGE-ID and rerender room buffer."
  (disco-room--poll-set-draft-selection message-id selection)
  (disco-room-render))

(defun disco-room-vote-poll-answer (&optional answer-id message-id)
  "Stage ANSWER-ID as selected for poll MESSAGE-ID.

In single-select polls, this replaces the staged selection."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "stage poll votes")
    (let ((picked (disco-room--pick-poll-answer-id msg answer-id)))
      (disco-room--stage-poll-selection
       target-id
       (disco-room--poll-add-selection target-id poll picked)))))

(defun disco-room-remove-poll-vote (&optional answer-id message-id)
  "Stage removal of ANSWER-ID vote from poll MESSAGE-ID."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg))
         (current (disco-room--poll-effective-selection target-id poll)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "stage poll vote removals")
    (let ((picked (disco-room--pick-poll-answer-id msg answer-id)))
      (unless (member picked current)
        (user-error "disco: answer %s is not selected" picked))
      (disco-room--stage-poll-selection
       target-id
       (delete picked (copy-sequence current))))))

(defun disco-room-toggle-poll-answer (&optional answer-id message-id)
  "Toggle staged poll ANSWER-ID in MESSAGE-ID.

This only updates local staged selection. Use `disco-room-submit-poll-vote' to
send votes to Discord."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "toggle staged poll votes")
    (let ((picked (disco-room--pick-poll-answer-id msg answer-id)))
      (disco-room--stage-poll-selection
       target-id
       (disco-room--poll-toggle-draft-selection target-id poll picked)))))

(defun disco-room-submit-poll-vote (&optional message-id)
  "Submit staged poll selection for MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg))
         (staged (disco-room--poll-effective-selection target-id poll))
         (committed (disco-msg-poll-voted-answer-ids poll)))
    (disco-room--ensure-action-available
     (disco-room--poll-submit-unavailable-reason msg)
     "submit poll votes")
    (when (null staged)
      (user-error "disco: select at least one answer before voting"))
    (when (equal (disco-room--poll-selection-key staged)
                 (disco-room--poll-selection-key committed))
      (user-error "disco: no pending poll vote changes"))
    (disco-room--submit-poll-vote target-id staged)))

(defun disco-room-clear-poll-votes (&optional message-id)
  "Remove all current-user votes for poll MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg))
         (committed (disco-msg-poll-voted-answer-ids poll)))
    (disco-room--ensure-action-available
     (disco-room--poll-clear-unavailable-reason msg)
     "clear poll votes")
    (unless committed
      (user-error "disco: no existing poll vote to remove"))
    (disco-room--submit-poll-vote target-id '())))

(defun disco-room-expire-poll (&optional message-id)
  "End poll in MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         request-revision)
    (disco-room--ensure-action-available
     (disco-room--poll-expire-unavailable-reason msg)
     "end polls")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     (disco-room--poll-expire-required-permissions)
     :action "ending polls")
    (when (or (not disco-room-poll-confirm-expire)
              (y-or-n-p (format "End poll %s now? " target-id)))
      (setq request-revision
            (disco-state-message-revision channel-id))
      (disco-api-expire-poll-async
       channel-id
       target-id
       :on-success
       (lambda (response)
         (if (and (listp response) (alist-get 'id response))
             (disco-state-merge-message-response
              channel-id response request-revision)
           ;; Discord normally returns the expired message.  If a proxy strips
           ;; it, refresh only the still-current presentation owner.
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (disco-room-refresh))))
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (disco-room--request-render view)
             (message "disco: poll ended"))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (message "disco: end poll failed: %s"
                    (disco-room--async-error-message err))))))))

(transient-define-prefix disco-room-poll-transient ()
  "Transient for the poll at point."
  :refresh-suffixes t
  [["Vote"
    :if-not disco-room--poll-vote-unavailable-reason
    ("t" "Toggle answer" disco-room-toggle-poll-answer :transient t)
    ("s" "Submit staged vote" disco-room-submit-poll-vote
     :if-not disco-room--poll-submit-unavailable-reason)]
   ["Manage"
    ("c" "Remove my vote" disco-room-clear-poll-votes
     :if-not disco-room--poll-clear-unavailable-reason)
    ("x" "End poll" disco-room-expire-poll
     :if-not disco-room--poll-expire-unavailable-reason)]]
  (interactive)
  (unless (disco-msg-poll (disco-room-menu--message-at-point))
    (user-error "disco: point is not on a poll"))
  (transient-setup 'disco-room-poll-transient))

(provide 'disco-room-poll)

;;; disco-room-poll.el ends here
