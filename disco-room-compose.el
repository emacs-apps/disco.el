;;; disco-room-compose.el --- Composer and send flows for Disco rooms -*- lexical-binding: t; -*-

;;; Commentary:

;; Room-local composer state, draft and attachment editing, send/edit/reply
;; operations, and their asynchronous reconciliation.  The room facade retains
;; lifecycle, history, Gateway dispatch, and timeline projection ownership.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)

(require 'appkit-core)
(require 'appkit-media)
(require 'appkit-chatbuf)
(require 'appkit-compose)
(require 'appkit-markup-compose)
(require 'disco-api)
(require 'disco-company)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-ins)
(require 'disco-media)
(require 'disco-markdown)
(require 'disco-msg)
(require 'disco-permission)
(require 'disco-state)
(require 'disco-sticker)
(require 'disco-thread)

(declare-function disco-api--validate-message-content-length "disco-api-normalize" (content field-name))
(declare-function disco-room--async-error-message "disco-room" (err))
(declare-function disco-room--author-face "disco-room-render" (message))
(declare-function disco-room--channel-buffer-p "disco-room" (buffer channel-id view))
(declare-function disco-room--channel-message-by-id "disco-room" (channel-id message-id))
(declare-function disco-room--channel-object "disco-room" ())
(declare-function disco-room--ensure-jump-permissions "disco-room" (channel-id &optional channel))
(declare-function disco-room--ensure-surface "disco-room" ())
(declare-function disco-room--latest-message-id "disco-room" ())
(declare-function disco-room--line-fill-column "disco-room-render" ())
(declare-function disco-room--lottie-sticker-at-point "disco-room" ())
(declare-function disco-room--message-at-point "disco-room" ())
(declare-function disco-room--message-author "disco-room-render" (message))
(declare-function disco-room--message-author-id "disco-room-render" (message))
(declare-function disco-room--message-by-id "disco-room" (message-id))
(declare-function disco-room--message-effective-attachments "disco-room-render" (message))
(declare-function disco-room--message-id-at-point "disco-room" ())
(declare-function disco-room--observe-live-create "disco-room" (message-id))
(declare-function disco-room--request-render "disco-room" (view))
(declare-function disco-room--resolve-target-channel "disco-room" (channel-id))
(declare-function disco-room--typing-indicator-text "disco-room" ())
(declare-function disco-room--update-frame "disco-room" (&optional channel draft))
(declare-function disco-room-complete-mention "disco-company" ())

(defvar disco-api--message-content-limit)
(defvar disco-room--channel-id)
(defvar disco-room--channel-name)
(defvar disco-room--guild-id)

;;; Composer state

(defvar-local disco-room--pending-reply-to nil)
(defvar-local disco-room--pending-edit nil)

(defvar-local disco-room--send-in-flight nil)
(defvar-local disco-room--sticker-picker-pending nil
  "Non-nil while this room is loading catalogs for an explicit Sticker pick.")

(defvar disco-room--send-nonce-counter 0
  "Monotonic low bits for client-generated Discord message nonces.")

(defun disco-room--next-send-nonce ()
  "Return a unique snowflake-shaped nonce for exact send reconciliation."
  (setq disco-room--send-nonce-counter
        (logand (1+ disco-room--send-nonce-counter) (1- (ash 1 22))))
  (number-to-string
   (+ (ash (- (truncate (* 1000 (float-time)))
              (* disco-state-discord-epoch-seconds 1000))
           22)
      disco-room--send-nonce-counter)))

(defvar-local disco-room--pending-attachments nil)
(defvar-local disco-room--attachment-token-table nil)
(defvar-local disco-room--attachment-token-seq 0)

(defvar-local disco-room--preview-buffer-owner-p nil
  "Non-nil when this buffer is a Disco composer-preview projection.")

(put 'disco-room--preview-buffer-owner-p 'permanent-local t)

(defconst disco-room--preview-buffer-name "*disco-room-preview*"
  "Preferred display name for the composer preview buffer.")

(defvar disco-room--preview-buffer nil
  "Live explicitly owned composer preview buffer, including after a rename.")

(defconst disco-room--attachment-token-regexp "\\[file:\\([0-9]+\\)\\]"
  "Regexp used to match attachment tokens in room draft input.")

(defconst disco-room--input-object-kind-attachment 'attachment
  "Structured input object kind used for queued file attachments.")

(defvar disco-room-draft-history-search-history nil
  "Minibuffer history for room draft-history searches.")

(defun disco-room-compose--clear-session-cache-memory ()
  "Clear account-scoped composer cache bookkeeping."
  (setq disco-room--preview-buffer nil
        disco-room-draft-history-search-history nil))

;;; Composer availability

(defun disco-room--compose-snapshot ()
  "Return a property-preserving snapshot of the current room source."
  (if-let* ((bounds (appkit-chatbuf-input-region-bounds)))
      (appkit-chatbuf-copy-string
       (buffer-substring (car bounds) (cdr bounds)))
    (appkit-chatbuf-copy-string (disco-room--current-draft))))

(defun disco-room--compose-context ()
  "Return non-secret provider context for one room capture."
  (list :channel-id disco-room--channel-id
        :guild-id disco-room--guild-id))

(defun disco-room--compose-object-classifier (value _text)
  "Classify structured compose VALUE for Discord output."
  (if (disco-room--attachment-input-object-p value)
      '(side-channel . attachments)
    '(reject . unsupported-discord-compose-object)))

(defun disco-room--compose-object-printer (node)
  "Print structured input NODE or leave Discord semantic objects to the codec."
  (let ((value
         (cond
          ((appkit-markup-object-p node) (appkit-markup-object-value node))
          ((appkit-markup-object-block-p node)
           (appkit-markup-object-block-value node)))))
    (unless (disco-markdown-object-p value)
      (signal 'appkit-markup-object-rejected
              '(unsupported-discord-compose-object))))
  nil)

(defun disco-room--setup-markup-compose ()
  "Configure Appkit capture and source codecs for the current room."
  (let ((active
         (and (boundp 'appkit-markup-compose-active-codec)
              (memq appkit-markup-compose-active-codec
                    disco-room-compose-codecs)
              appkit-markup-compose-active-codec)))
    (appkit-compose-setup
     :snapshot-function #'disco-room--compose-snapshot
     :source-bounds-function #'appkit-chatbuf-input-region-bounds)
    (appkit-markup-compose-setup
     :codecs disco-room-compose-codecs
     :active-codec (or active (car disco-room-compose-codecs))
     :context-function #'disco-room--compose-context
     :object-classifier #'disco-room--compose-object-classifier
     :object-printer #'disco-room--compose-object-printer)))

(defun disco-room--capture-attachments (capture)
  "Return upload attachments frozen in markup CAPTURE."
  (let* ((parse-result
          (appkit-markup-compose-capture-parse-result capture))
         (occurrences
          (alist-get
           'attachments
           (appkit-markup-parse-result-side-channels parse-result))))
    (delq
     nil
     (mapcar
      (lambda (occurrence)
        (disco-room--attachment-input-object-to-attachment
         (copy-tree
          (appkit-markup-object-occurrence-value occurrence))))
      occurrences))))

(defun disco-room-compose-reset ()
  "Reset composer-local state in the current room buffer."
  (disco-room--set-composer-aux-state nil nil)
  (disco-room--sync-shared-input-options-state)
  (setq-local disco-room--send-in-flight nil
              disco-room--sticker-picker-pending nil
              disco-room--pending-attachments nil
              disco-room--attachment-token-table
              (make-hash-table :test #'equal)
              disco-room--attachment-token-seq 0)
  (disco-room--setup-markup-compose))

(defun disco-room--required-send-permissions (&optional channel)
  "Return permission list required to send message in CHANNEL.

When CHANNEL is nil, use current room channel."
  (if (disco-thread-channel-p (or channel (disco-room--channel-object)))
      '(send-messages-in-threads)
    '(send-messages)))

(defun disco-room--composer-missing-permissions (&optional channel)
  "Return missing send permissions that should hide room composer for CHANNEL.

When computed permissions are unavailable, return nil to avoid false
negatives."
  (let ((channel (or channel (disco-room--channel-object))))
    (and channel
         (disco-permission-channel-known-p channel)
         (disco-permission-channel-missing
          channel
          (disco-room--required-send-permissions channel)
          nil))))

(defun disco-room--system-user-dm-restriction-reason (&optional channel)
  "Return read-only reason for official system-user DM CHANNEL, or nil."
  (let* ((channel (or channel (disco-room--channel-object)))
         (channel-type (and (listp channel) (alist-get 'type channel)))
         (recipients (and (listp channel) (alist-get 'recipients channel)))
         (recipient (and (equal channel-type 1) (car recipients))))
    (when (and (listp recipient)
               (alist-get 'system recipient))
      "official Discord system DMs are read-only")))

(defun disco-room--thread-send-restriction-reason (&optional channel)
  "Return thread-local send restriction reason for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (when (disco-thread-channel-p channel)
      (let ((tags (delq nil (list (and (disco-thread-archived-p channel) "archived")
                                  (and (disco-thread-locked-p channel) "locked")))))
        (when tags
          (format "current thread is %s" (mapconcat #'identity tags ", ")))))))

(defun disco-room--room-send-restriction-reason (&optional extra-permissions channel)
  "Return send restriction reason for current room CHANNEL, or nil.

EXTRA-PERMISSIONS augments the base send permission set for this room."
  (let* ((channel (or channel (disco-room--channel-object)))
         (system-dm-reason (disco-room--system-user-dm-restriction-reason channel))
         (thread-reason (disco-room--thread-send-restriction-reason channel))
         (permissions (append (disco-room--required-send-permissions channel)
                              (or extra-permissions '())))
         (missing (and channel
                       (disco-permission-channel-known-p channel)
                       (disco-permission-channel-missing channel permissions nil))))
    (or system-dm-reason
        thread-reason
        (when missing
          (format "missing %s"
                  (mapconcat #'disco-permission-display-name missing ", "))))))

(defun disco-room--composer-visible-p (&optional channel)
  "Return non-nil when room composer should be shown for CHANNEL."
  (not (disco-room--room-send-restriction-reason nil channel)))

(defun disco-room--composer-hidden-status-line (&optional channel)
  "Return read-only status line when room composer is hidden for CHANNEL."
  (when-let* ((reason (disco-room--room-send-restriction-reason nil channel)))
    (format "(read-only room; composer hidden: %s)" reason)))

(defun disco-room--current-composer-aux-state ()
  "Return current room-local composer aux plist, or nil."
  (cond
   ((and (listp disco-room--pending-edit)
         (eq (plist-get disco-room--pending-edit :type) 'edit))
    (let ((message-id (plist-get disco-room--pending-edit :message-id)))
      (list :aux-type 'edit
            :aux-msg (disco-room--composer-context-message message-id)
            :message-id message-id)))
   (disco-room--pending-reply-to
    (list :aux-type 'reply
          :aux-msg (disco-room--composer-context-message disco-room--pending-reply-to)
          :message-id disco-room--pending-reply-to))
   (t nil)))

(defun disco-room--composer-reply-message-id ()
  "Return target message id for active composer reply, or nil."
  (when (eq (plist-get (appkit-chatbuf-aux-state) :aux-type) 'reply)
    (plist-get (appkit-chatbuf-aux-state) :message-id)))

(defun disco-room--composer-edit-active-p ()
  "Return non-nil when room composer is editing an existing message."
  (eq (plist-get (appkit-chatbuf-aux-state) :aux-type) 'edit))

(defun disco-room--composer-edit-message-id ()
  "Return target message id for active composer edit, or nil."
  (and (disco-room--composer-edit-active-p)
       (plist-get (appkit-chatbuf-aux-state) :message-id)))

(defun disco-room--composer-aux-active-p ()
  "Return non-nil when room composer currently has reply/edit context."
  (not (null (appkit-chatbuf-aux-state))))

(defun disco-room--composer-aux-context-name ()
  "Return human-readable name for active composer aux context, or nil."
  (pcase (plist-get (appkit-chatbuf-aux-state) :aux-type)
    ('edit "editing a message")
    ('reply "replying to a message")
    (_ nil)))

(defun disco-room--message-owned-by-current-user-p (msg &optional unknown-value)
  "Return non-nil when MSG belongs to current user.

If ownership cannot be determined, return UNKNOWN-VALUE."
  (let* ((author-id (and (listp msg) (disco-room--message-author-id msg)))
         (self-id (disco-gateway-current-user-id)))
    (if (or (null author-id) (null self-id))
        unknown-value
      (equal (format "%s" author-id) (format "%s" self-id)))))

(defun disco-room--edit-permission-reason (&optional msg)
  "Return edit permission/restriction reason for MSG, or nil."
  (or (disco-room--room-send-restriction-reason
       nil (disco-room--channel-object))
      (cond
       ((not (consp msg))
        "message ownership is unavailable")
       ((not (disco-room--message-owned-by-current-user-p msg nil))
        "only your own messages can be edited"))))

(defun disco-room--attach-unavailable-reason ()
  "Return reason attach-file action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason '(attach-files))
      (when (disco-room--composer-edit-active-p)
        "attachments are unavailable while editing a message")))

(defun disco-room--reply-unavailable-reason ()
  "Return reason reply action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason '(read-message-history))
      (when (disco-room--composer-edit-active-p)
        "cancel the active edit before starting a reply")))

(defun disco-room--forward-unavailable-reason ()
  "Return reason forward action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason)
      (when-let* ((aux (disco-room--composer-aux-context-name)))
        (format "cancel %s before forwarding" aux))))

(defun disco-room--edit-start-unavailable-reason (&optional msg)
  "Return reason entering composer edit mode for MSG is unavailable, or nil."
  (or (when (disco-room--composer-edit-active-p)
        "already editing a message")
      (when (disco-room--composer-reply-message-id)
        "cancel the active reply before editing a message")
      (disco-room--edit-permission-reason msg)))

(defun disco-room--attachment-token-count ()
  "Return number of queued attachment refs in current draft."
  (length (disco-room--attachments-from-draft (disco-room--current-draft))))

(defun disco-room--attachment-token-action-unavailable-reason (&optional min-count)
  "Return reason attachment actions are unavailable, or nil.

MIN-COUNT optionally requires at least that many queued attachments."
  (or (when (disco-room--composer-edit-active-p)
        "attachments are unavailable while editing a message")
      (let ((count (disco-room--attachment-token-count)))
        (cond
         ((<= (or min-count 1) 0)
          nil)
         ((zerop count)
          "no queued attachments")
         ((and (integerp min-count) (< count min-count))
          (format "need at least %d queued attachments" min-count))))))

(defun disco-room--send-message-unavailable-reason ()
  "Return reason send-message action is unavailable, or nil."
  (let* ((draft (disco-room--current-draft))
         (has-attachments (not (null (disco-room--attachments-from-draft draft))))
         (reply-to (disco-room--composer-reply-message-id))
         (edit-message-id (disco-room--composer-edit-message-id))
         (edit-message (and edit-message-id
                            (disco-room--composer-context-message edit-message-id))))
    (if edit-message-id
        (or (disco-room--edit-permission-reason edit-message)
            (when has-attachments
              "attachments are unavailable while editing a message"))
      (disco-room--room-send-restriction-reason
       (append (when has-attachments '(attach-files))
               (when reply-to '(read-message-history)))))))

(defun disco-room--channel-permission-reason (permissions &optional channel)
  "Return missing-permission reason for PERMISSIONS on CHANNEL, or nil."
  (let* ((channel (or channel (disco-room--channel-object)))
         (missing (and channel
                       (disco-permission-channel-known-p channel)
                       (disco-permission-channel-missing channel permissions nil))))
    (when missing
      (format "missing %s"
              (mapconcat #'disco-permission-display-name missing ", ")))))

(defun disco-room--sticker-unavailable-reason ()
  "Return reason sending a Sticker is unavailable, or nil."
  (disco-room--room-send-restriction-reason))

(defun disco-room--delete-message-unavailable-reason (&optional msg)
  "Return reason delete-message action is unavailable for MSG, or nil."
  (when (listp msg)
    (let* ((channel (disco-room--channel-object))
           (missing-manage
            (and channel
                 (disco-permission-channel-known-p channel)
                 (not (disco-room--message-owned-by-current-user-p msg t))
                 (disco-permission-channel-missing channel '(manage-messages) nil))))
      (when missing-manage
        (format "missing %s"
                (mapconcat #'disco-permission-display-name missing-manage ", "))))))

(defun disco-room--ensure-action-available (reason action)
  "Signal `user-error' when ACTION is unavailable for REASON."
  (when reason
    (user-error "disco: cannot %s: %s" action reason)))

(defun disco-room--copy-attachment-token-table ()
  "Return deep copy of current attachment token table as alist entries."
  (let (entries)
    (when (hash-table-p disco-room--attachment-token-table)
      (maphash (lambda (token-id entry)
                 (push (cons token-id (copy-tree entry)) entries))
               disco-room--attachment-token-table))
    (nreverse entries)))

(defun disco-room--restore-attachment-token-table (entries)
  "Replace current attachment token table with ENTRIES alist copy."
  (unless (hash-table-p disco-room--attachment-token-table)
    (setq disco-room--attachment-token-table (make-hash-table :test #'equal)))
  (clrhash disco-room--attachment-token-table)
  (dolist (entry entries)
    (puthash (car entry) (copy-tree (cdr entry)) disco-room--attachment-token-table)))

;;; Composer operation ownership

(defun disco-room--composer-edit-saved-state ()
  "Capture composer state to be restored after edit cancel or success."
  (list :draft (appkit-chatbuf-copy-string (disco-room--current-draft))
        :reply-to (disco-room--composer-reply-message-id)
        :active-codec appkit-markup-compose-active-codec
        :attachment-token-seq disco-room--attachment-token-seq
        :attachment-token-entries (disco-room--copy-attachment-token-table)))

(defun disco-room--composer-edit-restore-state (state &optional defer-live-update-p)
  "Restore composer STATE captured by `disco-room--composer-edit-saved-state'.

When DEFER-LIVE-UPDATE-P is non-nil, update controller state only.  The owning
Appkit view will project the restored composer during its next sync."
  (let ((draft (appkit-chatbuf-copy-string (plist-get state :draft))))
    (disco-room--set-composer-aux-state nil (plist-get state :reply-to))
    (setq disco-room--attachment-token-seq
          (or (plist-get state :attachment-token-seq) 0))
    (disco-room--restore-attachment-token-table
     (plist-get state :attachment-token-entries))
    (when-let* ((codec (plist-get state :active-codec)))
      (appkit-markup-compose-set-active-codec codec))
    (disco-room--apply-draft-state
     draft
     :reset-history-p t
     :defer-live-update-p defer-live-update-p)))

(defun disco-room--composer-edit-clear (&optional restore-state defer-live-update-p)
  "Clear active composer edit.

When RESTORE-STATE is non-nil, also restore the saved draft/reply/attachment
state that was present before edit mode was entered.  DEFER-LIVE-UPDATE-P is
forwarded to the controller-only restore path used by asynchronous callbacks."
  (when (disco-room--composer-edit-active-p)
    (let ((saved-state (plist-get disco-room--pending-edit :saved-state)))
      (disco-room--set-composer-aux-state nil nil)
      (when restore-state
        (disco-room--composer-edit-restore-state
         saved-state defer-live-update-p)))
    t))

(defun disco-room--composer-operation-slot ()
  "Capture the complete client-owned composer slot for one mutation."
  (list :draft (appkit-chatbuf-copy-string (disco-room--current-draft))
        :pending-edit (copy-tree disco-room--pending-edit)
        :pending-reply-to disco-room--pending-reply-to
        :active-codec appkit-markup-compose-active-codec
        :message-revision
        (disco-state-message-revision disco-room--channel-id)
        :attachment-token-seq disco-room--attachment-token-seq
        :attachment-token-entries (disco-room--copy-attachment-token-table)))

(defun disco-room--clear-composer-operation-slot ()
  "Clear input and aux as one mutation boundary and return its revision."
  (disco-room--clear-draft)
  (disco-room--set-composer-aux-state nil nil)
  (appkit-chatbuf-composer-revision))

(defun disco-room--restore-composer-operation-slot
    (revision slot &optional defer-live-update-p)
  "Restore SLOT only while Appkit composer REVISION remains pristine.

When DEFER-LIVE-UPDATE-P is non-nil, update controller state only.  Return
non-nil exactly when restoration wins.  Reply/edit targets deleted after SLOT
was captured are not restored, but their draft and attachments remain
recoverable."
  (when (= revision (appkit-chatbuf-composer-revision))
    (let* ((pending-edit (copy-tree (plist-get slot :pending-edit)))
           (pending-reply-to (plist-get slot :pending-reply-to))
           (message-revision (plist-get slot :message-revision)))
      (cl-labels
          ((deleted-after-capture-p
             (message-id)
             (and message-id
                  (disco-state-message-deleted-after-p
                   disco-room--channel-id message-id message-revision))))
        (when (deleted-after-capture-p
               (plist-get pending-edit :message-id))
          (setq pending-edit nil))
        (when (deleted-after-capture-p pending-reply-to)
          (setq pending-reply-to nil))
        (when-let* ((saved-state (plist-get pending-edit :saved-state))
                    (saved-reply-to (plist-get saved-state :reply-to)))
          (when (deleted-after-capture-p saved-reply-to)
            (setf (plist-get saved-state :reply-to) nil))))
      (setq disco-room--attachment-token-seq
            (or (plist-get slot :attachment-token-seq) 0))
      (disco-room--restore-attachment-token-table
       (plist-get slot :attachment-token-entries))
      (disco-room--set-composer-aux-state pending-edit pending-reply-to)
      (when-let* ((codec (plist-get slot :active-codec)))
        (appkit-markup-compose-set-active-codec codec))
      (disco-room--apply-draft-state
       (appkit-chatbuf-copy-string (plist-get slot :draft))
       :reset-history-p t
       :defer-live-update-p defer-live-update-p)
      t)))

(defun disco-room--composer-context-message (message-id)
  "Return cached local message object for MESSAGE-ID, or nil."
  (and message-id (disco-room--message-by-id message-id)))

(defun disco-room--composer-context-text (aux-state)
  "Return the unified composer context card for AUX-STATE."
  (when aux-state
    (let* ((aux-type (plist-get aux-state :aux-type))
           (message-id (plist-get aux-state :message-id))
           (msg (or (disco-room--composer-context-message message-id)
                    (plist-get aux-state :aux-msg)))
           (author (and msg (disco-room--message-author msg)))
           (author-face (and msg (disco-room--author-face msg)))
           (subject
            (if (and (stringp author) (not (string-empty-p author)))
                (propertize author 'face author-face)
              "message"))
           (title
            (pcase aux-type
              ('edit "Editing message")
              ('reply (concat "Reply to " subject))
              (_ "")))
           (preview (string-trim
                     (or (and msg (disco-msg-preview-content msg)) ""))))
      (appkit-chatbuf-aux-render
       :title title
       :preview
       (disco-media-message-one-line-preview
        msg
        (if (string-empty-p preview)
            "Message preview unavailable"
          preview)
        :attachments
        (and msg (disco-room--message-effective-attachments msg)))
       :cancel-action #'disco-room-cancel-reply
       :cancel-help
       (format "Cancel %s (C-c C-k)"
               (if (eq aux-type 'edit) "edit" "reply"))
       :accent-face author-face
       :width (disco-room--line-fill-column)))))

(defun disco-room--composer-enter-edit (msg)
  "Enter composer edit mode for MSG."
  (let* ((message-id (alist-get 'id msg))
         (old-content (or (alist-get 'content msg) ""))
         (saved-state (disco-room--composer-edit-saved-state)))
    (unless (and message-id (not (string-empty-p (format "%s" message-id))))
      (user-error "disco: message id is unavailable for edit"))
    (when (disco-room--composer-edit-active-p)
      (disco-room--composer-edit-clear t))
    (disco-room--set-composer-aux-state
     (list :type 'edit
           :message-id message-id
           :saved-state saved-state)
     nil)
    (appkit-markup-compose-set-active-codec 'discord-markdown)
    (disco-room--apply-draft-state old-content :reset-history-p t)
    (setq disco-room--attachment-token-seq 0)
    (when (hash-table-p disco-room--attachment-token-table)
      (clrhash disco-room--attachment-token-table))
    (disco-room--update-frame)
    (appkit-chatbuf-focus-input)
    (message "disco: editing message %s in composer" message-id)))

;;; Draft and structured attachments

(defun disco-room--current-draft ()
  "Return current room draft string, preserving text properties."
  (appkit-chatbuf-input-state))

(defun disco-room--attachment-input-object-p (object)
  "Return non-nil when OBJECT is a queued attachment input object."
  (and (listp object)
       (eq (plist-get object :kind) disco-room--input-object-kind-attachment)))

(cl-defun disco-room--make-attachment-input-object
    (path &key description filename content-type spoiler)
  "Build one structured attachment input object for PATH."
  (unless (and (stringp path) (not (string-empty-p path)))
    (user-error "disco: attachment object requires a file path"))
  (let* ((resolved-filename (or filename (file-name-nondirectory path)))
         (trimmed-description (and (stringp description)
                                   (not (string-empty-p (string-trim description)))
                                   (string-trim description))))
    (list :kind disco-room--input-object-kind-attachment
          :path path
          :filename resolved-filename
          :description trimmed-description
          :content-type content-type
          :spoiler (and spoiler t))))

(defun disco-room--attachment-input-object-display-text (attachment)
  "Return visible composer text for ATTACHMENT input object."
  (let* ((path (plist-get attachment :path))
         (filename
          (or (plist-get attachment :filename)
              (and path (file-name-nondirectory path))
              "unnamed"))
         (content-type (plist-get attachment :content-type))
         (image-p
          (or (and (stringp content-type)
                   (string-prefix-p "image/" content-type))
              (appkit-media-image-file-name-p filename)))
         (image
          (and image-p
               (appkit-media-file-present-p path)
               (appkit-media-one-line-preview-image-from-file path)))
         (preview
          (and image
               (appkit-media-one-line-image-display-string image "▧")))
         (size
          (and (appkit-media-file-present-p path)
               (file-size-human-readable
                (file-attribute-size (file-attributes path))))))
    (concat (if (plist-get attachment :spoiler) "[spoiler] " "")
            (if image-p "[image] " "[file] ")
            (if preview (concat preview " ") "")
            (propertize filename 'help-echo path)
            (if size (format " (%s)" size) ""))))

(defun disco-room--attachment-input-object-string (attachment)
  "Return one propertized draft string representing ATTACHMENT."
  (let* ((object (copy-tree attachment))
         (text (disco-room--attachment-input-object-display-text object)))
    (appkit-chatbuf-input-object-string text object)))

(defun disco-room--insert-attachment-input-object (attachment)
  "Insert ATTACHMENT as one structured composer object at point."
  (let ((object (copy-tree attachment)))
    ;; Do not insert the leading separator into an existing atomic object when
    ;; point was placed unexpectedly inside its intangible display span.
    (when-let* ((bounds (appkit-chatbuf-input-object-bounds-at-point)))
      (unless (= (point) (car bounds))
        (goto-char (cdr bounds))))
    (when (and (appkit-chatbuf-point-in-input-p)
               (> (point) (or (appkit-chatbuf-input-start-position) (point-min)))
               (let ((before (char-before)))
                 (and before (not (memq before '(?\s ?\t ?\n))))))
      (appkit-chatbuf-input-insert " "))
    (appkit-chatbuf-input-insert
     (disco-room--attachment-input-object-display-text object)
     :object object)
    (when (let ((after (char-after)))
            (and after (not (memq after '(?\s ?\t ?\n)))))
      (appkit-chatbuf-input-insert " "))
    object))

(defun disco-room--attachment-input-object-to-attachment (object)
  "Convert structured attachment OBJECT into upload plist."
  (when (disco-room--attachment-input-object-p object)
    (let ((attachment
           (list :path (plist-get object :path)
                 :filename (or (plist-get object :filename)
                               (file-name-nondirectory
                                (or (plist-get object :path) ""))))))
      (when-let* ((description (plist-get object :description)))
        (setq attachment (plist-put attachment :description description)))
      (when-let* ((content-type (plist-get object :content-type)))
        (setq attachment (plist-put attachment :content-type content-type)))
      (when (plist-get object :spoiler)
        (setq attachment (plist-put attachment :is-spoiler t)))
      attachment)))

(defun disco-room--attachment-label (attachment prefix)
  "Return one user-facing label for ATTACHMENT using PREFIX."
  (let* ((path (or (plist-get attachment :path) ""))
         (filename (or (plist-get attachment :filename)
                       (and (not (string-empty-p path))
                            (file-name-nondirectory path))
                       "missing"))
         (description (or (plist-get attachment :description) "")))
    (if (string-empty-p description)
        (format "%s %s" prefix filename)
      (format "%s %s - %s" prefix filename description))))

(defun disco-room--draft-substring-delete (draft start end)
  "Return DRAFT with region START..END removed, preserving properties."
  (concat (substring draft 0 start)
          (substring draft end)))

(defun disco-room--draft-substring-replace (draft start end replacement)
  "Return DRAFT with region START..END replaced by REPLACEMENT."
  (concat (substring draft 0 start)
          replacement
          (substring draft end)))

(defun disco-room--attachment-refs (&optional draft)
  "Return ordered attachment refs found in DRAFT."
  (let* ((text (or draft (disco-room--current-draft)))
         (len (length text))
         (pos 0)
         (refs '()))
    (while (< pos len)
      (let ((object (get-text-property pos appkit-chatbuf-input-object-property text)))
        (if (disco-room--attachment-input-object-p object)
            (let* ((end (appkit-chatbuf-next-input-object-change
                         pos text len))
                   (object-copy (copy-tree object))
                   (attachment (disco-room--attachment-input-object-to-attachment object-copy)))
              (push (list :type 'object
                          :start pos
                          :end end
                          :object object-copy
                          :attachment attachment
                          :label (disco-room--attachment-label attachment "[file]"))
                    refs)
              (setq pos end))
          (let* ((next-object
                  (appkit-chatbuf-next-input-object-change pos text len))
                 (chunk (substring-no-properties text pos next-object))
                 (chunk-pos 0))
            (while (string-match disco-room--attachment-token-regexp chunk chunk-pos)
              (let* ((token-id (match-string 1 chunk))
                     (attachment (copy-tree (or (disco-room--attachment-by-token-id token-id)
                                                (list :token-id token-id))))
                     (start (+ pos (match-beginning 0)))
                     (end (+ pos (match-end 0))))
                (push (list :type 'token
                            :start start
                            :end end
                            :token-id token-id
                            :attachment attachment
                            :label (disco-room--attachment-label
                                    attachment
                                    (disco-room--attachment-token-text token-id)))
                      refs))
              (setq chunk-pos (match-end 0)))
            (setq pos next-object)))))
    (nreverse refs)))

(defun disco-room--choose-attachment-ref (prompt)
  "Prompt for one queued attachment ref using PROMPT."
  (let* ((refs (disco-room--attachment-refs))
         (labels (mapcar (lambda (ref) (plist-get ref :label)) refs))
         (picked (completing-read prompt labels nil t)))
    (or (seq-find (lambda (ref)
                    (equal (plist-get ref :label) picked))
                  refs)
        (user-error "disco: invalid attachment selection"))))

(defun disco-room--attachment-ref-string (ref)
  "Return serialized draft text for attachment REF."
  (pcase (plist-get ref :type)
    ('object
     (disco-room--attachment-input-object-string
      (or (plist-get ref :object)
          (disco-room--make-attachment-input-object
           (plist-get (plist-get ref :attachment) :path)
           :filename (plist-get (plist-get ref :attachment) :filename)
           :description (plist-get (plist-get ref :attachment) :description)
           :spoiler (plist-get (plist-get ref :attachment) :is-spoiler)
           :content-type (plist-get (plist-get ref :attachment) :content-type)))))
    (_
     (disco-room--attachment-token-text (plist-get ref :token-id)))))

(defun disco-room--draft-input-objects (&optional draft)
  "Return ordered structured input objects found in DRAFT."
  (let* ((text (or draft (disco-room--current-draft)))
         (len (length text))
         (pos 0)
         (objects '()))
    (while (< pos len)
      (let ((object (get-text-property pos appkit-chatbuf-input-object-property text)))
        (if object
            (let ((end (appkit-chatbuf-next-input-object-change
                        pos text len)))
              (push (copy-tree object) objects)
              (setq pos end))
          (setq pos (appkit-chatbuf-next-input-object-change
                     pos text len)))))
    (nreverse objects)))

(defun disco-room--next-attachment-token-id ()
  "Return next unique attachment token id for current room buffer."
  (setq disco-room--attachment-token-seq (1+ (or disco-room--attachment-token-seq 0)))
  (number-to-string disco-room--attachment-token-seq))

(defun disco-room--attachment-token-text (token-id)
  "Return textual draft token representation for TOKEN-ID."
  (format "[file:%s]" token-id))

(defun disco-room--attachment-token-ids-in-text (text)
  "Return attachment token ids found in TEXT, preserving first-seen order."
  (let ((pos 0)
        (ids '())
        (seen (make-hash-table :test #'equal)))
    (while (and (stringp text)
                (< pos (length text))
                (string-match disco-room--attachment-token-regexp text pos))
      (let ((token-id (match-string 1 text)))
        (unless (gethash token-id seen)
          (puthash token-id t seen)
          (push token-id ids)))
      (setq pos (match-end 0)))
    (nreverse ids)))

(defun disco-room--attachment-by-token-id (token-id)
  "Return attachment plist by TOKEN-ID from current room token table."
  (and disco-room--attachment-token-table
       (gethash token-id disco-room--attachment-token-table)))

(defun disco-room--parse-draft-input (&optional draft)
  "Parse DRAFT into plain content, structured objects, and attachment uploads."
  (let* ((text (or draft (disco-room--current-draft)))
         (len (length text))
         (pos 0)
         (content-parts '())
         (objects '())
         (attachments '())
         (token-ids '())
         (seen-token-ids (make-hash-table :test #'equal)))
    (while (< pos len)
      (let ((object (get-text-property pos appkit-chatbuf-input-object-property text)))
        (if object
            (let* ((end (appkit-chatbuf-next-input-object-change
                         pos text len))
                   (object-copy (copy-tree object)))
              (push object-copy objects)
              (when-let* ((attachment
                           (disco-room--attachment-input-object-to-attachment object-copy)))
                (push attachment attachments))
              (setq pos end))
          (let* ((end (appkit-chatbuf-next-input-object-change
                       pos text len))
                 (chunk (substring-no-properties text pos end))
                 (content-chunk
                  (replace-regexp-in-string disco-room--attachment-token-regexp "" chunk)))
            (push content-chunk content-parts)
            (dolist (token-id (disco-room--attachment-token-ids-in-text chunk))
              (unless (gethash token-id seen-token-ids)
                (puthash token-id t seen-token-ids)
                (push token-id token-ids)
                (when-let* ((attachment (disco-room--attachment-by-token-id token-id)))
                  (push (copy-tree attachment) attachments))))
            (setq pos end)))))
    (list :content (mapconcat #'identity (nreverse content-parts) "")
          :objects (nreverse objects)
          :attachments (nreverse attachments)
          :token-ids (nreverse token-ids))))

(defun disco-room--attachments-from-draft (&optional draft)
  "Return ordered attachment list referenced by DRAFT."
  (plist-get (disco-room--parse-draft-input draft) :attachments))

(defun disco-room--draft-without-attachment-tokens (&optional draft)
  "Return DRAFT plain content with attachment placeholders removed."
  (plist-get (disco-room--parse-draft-input draft) :content))

(defun disco-room--sync-pending-attachments-from-draft (&optional draft)
  "Refresh `disco-room--pending-attachments' using parsed DRAFT references."
  (setq disco-room--pending-attachments
        (disco-room--attachments-from-draft draft)))

(defun disco-room--prune-unused-attachment-tokens (&optional draft)
  "Remove token table entries that are not referenced in DRAFT."
  (let ((alive (make-hash-table :test #'equal)))
    (dolist (token-id (plist-get (disco-room--parse-draft-input draft) :token-ids))
      (puthash token-id t alive))
    (when disco-room--attachment-token-table
      (maphash
       (lambda (token-id _attachment)
         (unless (gethash token-id alive)
           (remhash token-id disco-room--attachment-token-table)))
       disco-room--attachment-token-table))))

(defun disco-room--apply-input-text-properties ()
  "Normalize current draft text properties after redraws and edits."
  (appkit-chatbuf-input-apply-text-properties)
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (with-silent-modifications
      (add-text-properties
       (car bounds) (cdr bounds)
       '(disco-room-input t)))))

(defun disco-room--sync-draft-from-buffer ()
  "Sync shared chatbuf draft cache from editable input region."
  (let ((text (plist-get (appkit-chatbuf-input-state-sync)
                         :value)))
    (disco-room--prune-unused-attachment-tokens text)
    (disco-room--sync-pending-attachments-from-draft text)))

(cl-defun disco-room--apply-draft-state
    (text &key reset-history-p defer-live-update-p)
  "Apply draft TEXT to cache/live input and return update metadata.

When a visible tail input exists, update it directly.  Attachment-derived
footer changes are projected separately without rebuilding that input.
Callers use the returned metadata to decide whether a frame refresh is
needed.  When DEFER-LIVE-UPDATE-P is non-nil, only controller state changes;
an Appkit sync must project the resulting composer."
  (let ((old-attachments (copy-tree disco-room--pending-attachments))
        (live-input-p (and (not defer-live-update-p)
                           (not (appkit-chatbuf-rendering-p))
                           (appkit-chatbuf-input-start-position))))
    (let ((draft
           (appkit-chatbuf-input-state-set
            text
            :reset-history-p reset-history-p)))
      (disco-room--prune-unused-attachment-tokens draft)
      (disco-room--sync-pending-attachments-from-draft draft)
      (let ((attachments-changed-p
             (not (equal old-attachments disco-room--pending-attachments))))
        (when live-input-p
          (appkit-chatbuf-with-generated-update
            (appkit-chatbuf-input-replace draft)
            (disco-room--apply-input-text-properties)))
        (list :draft (appkit-chatbuf-copy-string draft)
              :attachments-changed-p attachments-changed-p
              :live-input-updated-p (and live-input-p t))))))

(defun disco-room--set-draft (text)
  "Set room draft TEXT and refresh composer surfaces as needed."
  (let ((result (disco-room--apply-draft-state text)))
    (when (plist-get result :attachments-changed-p)
      (disco-room--update-frame))))

(defun disco-room--clear-draft ()
  "Clear room draft and reset draft history navigation state."
  (let ((result (disco-room--apply-draft-state "" :reset-history-p t)))
    (when (plist-get result :attachments-changed-p)
      (disco-room--update-frame))))

(defun disco-room-draft-prev (&optional n)
  "Replace draft with N previous entries from draft history."
  (interactive "p")
  (let ((result (appkit-chatbuf-input-history-prev-value
                 (disco-room--current-draft)
                 n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-room--set-draft (plist-get result :value)))
      (_
       (message "disco: draft history is empty")))))

(defun disco-room-draft-next (&optional n)
  "Replace draft with N newer entries from draft history."
  (interactive "p")
  (let ((result (appkit-chatbuf-input-history-next-value n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-room--set-draft (plist-get result :value)))
      (_
       (message "disco: already at latest draft")))))

(defun disco-room-edit-draft ()
  "Edit current room draft in minibuffer and re-render room."
  (interactive)
  (when (appkit-chatbuf-string-has-objects-p (disco-room--current-draft))
    (user-error "disco: minibuffer draft editing is unavailable for structured input objects"))
  (let ((updated (read-from-minibuffer
                  "Draft: "
                  (appkit-chatbuf-string-plain-text (disco-room--current-draft)))))
    (appkit-chatbuf-input-history-reset)
    (disco-room--set-draft updated)))

;;; Composer footer and input binding

(defun disco-room--input-footer-context-text ()
  "Return extra context lines shown above the room composer."
  (concat
   (or (disco-room--composer-context-text
        (appkit-chatbuf-aux-state))
       "")
   (if disco-room--pending-attachments
       (format "Queued attachments: %s\n"
               (mapconcat #'identity
                          (disco-room--pending-attachment-labels)
                          ", "))
     "")))

(defun disco-room--input-footer-text ()
  "Build read-only EWOC footer text shown above the room prompt."
  (let ((context-text (disco-room--input-footer-context-text))
        (typing-text (disco-room--typing-indicator-text)))
    (if (not (disco-room--composer-visible-p))
        ""
      (let ((text
             (concat
              "\n"
              (if (string-empty-p context-text)
                  ""
                context-text)
              (if (and (stringp typing-text)
                       (not (string-empty-p typing-text)))
                  (propertize (concat typing-text "\n")
                              'face 'disco-room-typing-indicator)
                ""))))
        (add-text-properties
         0 (length text)
         '(read-only t
           front-sticky (read-only)
           rear-nonsticky (read-only))
         text)
        text))))

(defun disco-room--prompt-text ()
  "Return visible prompt text for the current room buffer."
  ">>> ")

(defun disco-room--sync-shared-aux-state ()
  "Mirror room reply/edit context into shared chatbuf aux state."
  (if-let* ((aux-state (disco-room--current-composer-aux-state)))
      (appkit-chatbuf-aux-set aux-state)
    (appkit-chatbuf-aux-reset)))

(defun disco-room--set-composer-aux-state (pending-edit pending-reply-to)
  "Set room composer PENDING-EDIT and PENDING-REPLY-TO, then sync aux state."
  (setq disco-room--pending-edit pending-edit
        disco-room--pending-reply-to pending-reply-to)
  (disco-room--sync-shared-aux-state))

(defun disco-room--current-input-options-state ()
  "Return current room-local input-options plist for shared chatbuf state."
  (list :send-on-return disco-room-send-on-return
        :long-message-action disco-room-long-message-action
        :allowed-mentions (copy-tree disco-room-allowed-mentions)
        :reply-mention-replied-user disco-room-reply-mention-replied-user))

(defun disco-room--input-options-state ()
  "Return composer input-options plist from shared chatbuf state, or nil."
  (appkit-chatbuf-input-options-state))

(defun disco-room--input-option-send-on-return ()
  "Return effective send-on-return option from shared chatbuf state."
  (eq t (plist-get (disco-room--input-options-state) :send-on-return)))

(defun disco-room--input-option-long-message-action ()
  "Return effective long-message action from shared chatbuf state."
  (plist-get (disco-room--input-options-state) :long-message-action))

(defun disco-room--input-option-allowed-mentions ()
  "Return effective allowed-mentions option from shared chatbuf state."
  (let ((state (disco-room--input-options-state)))
    (when (plist-member state :allowed-mentions)
      (copy-tree (plist-get state :allowed-mentions)))))

(defun disco-room--input-option-reply-mention-replied-user ()
  "Return effective reply-mention option from shared chatbuf state."
  (eq t (plist-get (disco-room--input-options-state)
                   :reply-mention-replied-user)))

(defun disco-room--sync-shared-input-options-state ()
  "Mirror room-local input option state into shared chatbuf state."
  (appkit-chatbuf-input-options-set
   (disco-room--current-input-options-state)))

(defun disco-room--set-input-options-state (options)
  "Set room-local input OPTIONS and sync shared chatbuf state.

OPTIONS should be a plist using the same keys as
`disco-room--current-input-options-state'.  Missing keys fall back to the
current effective input-options state.  Return the normalized state plist."
  (let* ((current (or (disco-room--input-options-state)
                      (disco-room--current-input-options-state)))
         (send-on-return (if (plist-member options :send-on-return)
                             (plist-get options :send-on-return)
                           (plist-get current :send-on-return)))
         (long-message-action (if (plist-member options :long-message-action)
                                  (plist-get options :long-message-action)
                                (plist-get current :long-message-action)))
         (allowed-mentions (if (plist-member options :allowed-mentions)
                               (copy-tree (plist-get options :allowed-mentions))
                             (copy-tree (plist-get current :allowed-mentions))))
         (reply-mention-replied-user
          (if (plist-member options :reply-mention-replied-user)
              (plist-get options :reply-mention-replied-user)
            (plist-get current :reply-mention-replied-user))))
    (setq-local disco-room-send-on-return send-on-return
                disco-room-long-message-action long-message-action
                disco-room-allowed-mentions allowed-mentions
                disco-room-reply-mention-replied-user reply-mention-replied-user)
    (disco-room--sync-shared-input-options-state)
    (disco-room--current-input-options-state)))

(defun disco-room--update-input-options-state (updater)
  "Apply UPDATER to current effective input-options state and sync the result."
  (disco-room--set-input-options-state
   (funcall updater (or (disco-room--input-options-state)
                        (disco-room--current-input-options-state)))))

(defun disco-room--bind-input-region-from-footer ()
  "Ensure the persistent tail input region exists and matches current draft."
  (appkit-chatbuf-init-state disco-room-input-history-size)
  (disco-room--sync-shared-aux-state)
  (disco-room--sync-shared-input-options-state)
  (appkit-chatbuf-bind-input-region
   :visible-p (disco-room--composer-visible-p)
   :prompt (disco-room--prompt-text)
   :input-text (disco-room--current-draft)
   :post-bind-function #'disco-room--apply-input-text-properties))

(defun disco-room--pending-attachment-labels ()
  "Return compact filename labels for pending composer attachments."
  (let ((labels
         (mapcar
          (lambda (item)
            (let* ((token-id (plist-get item :token-id))
                   (path (or (plist-get item :path) ""))
                   (filename (if (string-empty-p path)
                                 "missing"
                               (file-name-nondirectory path)))
                   (description (or (plist-get item :description) ""))
                   (token-label (cond
                                 (token-id
                                  (disco-room--attachment-token-text token-id))
                                 ((disco-room--attachment-input-object-p item)
                                  "[file]")
                                 (t "[file:?]"))))
              (if (string-empty-p description)
                  (format "%s %s" token-label filename)
                (format "%s %s - %s" token-label filename description))))
          (or disco-room--pending-attachments '()))))
    (if (> (length labels) 3)
        (append (seq-take labels 3)
                (list (format "+%d more" (- (length labels) 3))))
      labels)))

(defun disco-room--append-attachment-token-to-draft (token-id)
  "Append attachment TOKEN-ID marker to current draft input."
  (let* ((token-text (disco-room--attachment-token-text token-id))
         (draft (disco-room--current-draft))
         (separator (if (or (string-empty-p draft)
                            (string-match-p "[ \t\n]\\'" draft))
                        ""
                      " ")))
    (disco-room--set-draft (concat draft separator token-text))))

(defun disco-room--remove-first-token-from-draft (draft token-id)
  "Return DRAFT with first TOKEN-ID marker removed."
  (let* ((token-text (disco-room--attachment-token-text token-id))
         (regexp (regexp-quote token-text)))
    (if (string-match regexp draft)
        (concat (substring draft 0 (match-beginning 0))
                (substring draft (match-end 0)))
      draft)))

(defun disco-room--attachment-token-bounds-at-point ()
  "Return bounds of attachment token around point in input region, or nil."
  (let ((bounds (appkit-chatbuf-input-region-bounds))
        (pos (point))
        found)
    (when (and bounds (appkit-chatbuf-point-in-input-p pos))
      (save-excursion
        (goto-char (car bounds))
        (while (and (not found)
                    (re-search-forward disco-room--attachment-token-regexp (cdr bounds) t))
          (when (and (<= (match-beginning 0) pos)
                     (<= pos (match-end 0)))
            (setq found (cons (match-beginning 0) (match-end 0))))))
      found)))

(defun disco-room--attachment-object-bounds-at-point ()
  "Return bounds of attachment input object around point, or nil."
  (let ((object (appkit-chatbuf-input-object-at-point)))
    (when (disco-room--attachment-input-object-p object)
      (appkit-chatbuf-input-object-bounds-at-point))))

(defun disco-room--rewrite-draft-attachment-order (ordered-refs)
  "Rewrite current draft so ORDERED-REFS becomes the attachment sequence."
  (let* ((text-only (string-trim-right
                     (disco-room--draft-without-attachment-tokens
                      (disco-room--current-draft))))
         (parts (if (string-empty-p text-only)
                    nil
                  (list text-only))))
    (dolist (ref ordered-refs)
      (when parts
        (setq parts (append parts '(" "))))
      (setq parts (append parts (list (disco-room--attachment-ref-string ref)))))
    (disco-room--set-draft (if parts (apply #'concat parts) ""))))

(defun disco-room-remove-attachment-token-at-point ()
  "Remove queued attachment at point, or prompt for one when needed."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 1)
   "remove attachments")
  (let ((object-bounds (disco-room--attachment-object-bounds-at-point))
        (token-bounds (disco-room--attachment-token-bounds-at-point)))
    (cond
     (object-bounds
      (delete-region (car object-bounds) (cdr object-bounds))
      (disco-room--sync-draft-from-buffer)
      (disco-room--update-frame)
      (message "disco: removed attachment"))
     (token-bounds
      (let* ((token-text (buffer-substring-no-properties (car token-bounds) (cdr token-bounds)))
             (token-id (and (string-match disco-room--attachment-token-regexp token-text)
                            (match-string 1 token-text))))
        (delete-region (car token-bounds) (cdr token-bounds))
        (disco-room--sync-draft-from-buffer)
        (when token-id
          (remhash token-id disco-room--attachment-token-table))
        (disco-room--update-frame)
        (message "disco: removed attachment %s" (or token-id ""))))
     (t
      (let* ((ref (disco-room--choose-attachment-ref "Remove attachment: "))
             (type (plist-get ref :type))
             (start (plist-get ref :start))
             (end (plist-get ref :end))
             (draft (disco-room--current-draft))
             (updated (disco-room--draft-substring-delete draft start end)))
        (when (eq type 'token)
          (remhash (plist-get ref :token-id) disco-room--attachment-token-table))
        (disco-room--set-draft updated)
        (message "disco: removed %s" (plist-get ref :label)))))))

(defun disco-room-list-attachments ()
  "List queued attachments for current draft."
  (interactive)
  (disco-room--ensure-action-available
   (when (disco-room--composer-edit-active-p)
     "attachments are unavailable while editing a message")
   "list attachments")
  (let ((refs (disco-room--attachment-refs)))
    (if (null refs)
        (message "disco: no queued attachments")
      (message "disco: %s"
               (mapconcat (lambda (ref) (plist-get ref :label)) refs " | ")))))

(defun disco-room--replace-attachment-ref (ref attachment)
  "Replace queued attachment REF with normalized ATTACHMENT."
  (pcase (plist-get ref :type)
    ('token
     (puthash (plist-get ref :token-id)
              (plist-put attachment :token-id (plist-get ref :token-id))
              disco-room--attachment-token-table)
     (disco-room--sync-pending-attachments-from-draft))
    ('object
     (let ((replacement
            (disco-room--attachment-input-object-string
             (disco-room--make-attachment-input-object
              (plist-get attachment :path)
              :filename (plist-get attachment :filename)
              :description (plist-get attachment :description)
              :content-type (plist-get attachment :content-type)
              :spoiler (plist-get attachment :is-spoiler)))))
       (disco-room--set-draft
        (disco-room--draft-substring-replace
         (disco-room--current-draft)
         (plist-get ref :start)
         (plist-get ref :end)
         replacement))))))

(defun disco-room-edit-attachment-description ()
  "Edit description of one queued attachment."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 1)
   "edit attachment descriptions")
  (let* ((ref (disco-room--choose-attachment-ref "Edit attachment: "))
         (attachment
          (copy-tree
           (or (plist-get ref :attachment)
               (user-error "disco: attachment not found"))))
         (current (or (plist-get attachment :description) ""))
         (next-input
          (read-string
           (format "Description for %s (empty clears): "
                   (plist-get ref :label))
           current))
         (next (string-trim next-input)))
    (setq attachment
          (plist-put attachment :description
                     (unless (string-empty-p next) next)))
    (disco-room--replace-attachment-ref ref attachment)
    (disco-room--update-frame)
    (if (string-empty-p next)
        (message "disco: cleared description for %s" (plist-get ref :label))
      (message "disco: updated description for %s" (plist-get ref :label)))))

(defun disco-room-toggle-attachment-spoiler ()
  "Toggle spoiler status for one queued attachment."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 1)
   "toggle attachment spoilers")
  (let* ((ref (disco-room--choose-attachment-ref "Toggle spoiler: "))
         (attachment
          (copy-tree
           (or (plist-get ref :attachment)
               (user-error "disco: attachment not found"))))
         (spoiler (not (plist-get attachment :is-spoiler))))
    (setq attachment (plist-put attachment :is-spoiler spoiler))
    (disco-room--replace-attachment-ref ref attachment)
    (disco-room--update-frame)
    (message "disco: attachment spoiler %s for %s"
             (if spoiler "enabled" "disabled")
             (plist-get ref :label))))

(defun disco-room-reorder-attachments ()
  "Reorder one queued attachment in the current draft."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 2)
   "reorder attachments")
  (let* ((refs (disco-room--attachment-refs))
         (count (length refs)))
    (when (< count 2)
      (user-error "disco: need at least two attachments to reorder"))
    (let* ((ref (disco-room--choose-attachment-ref "Move attachment: "))
           (current-index (or (cl-position ref refs :test #'equal)
                              (user-error "disco: attachment not found in draft")))
           (target-index-input
            (read-number
             (format "Move %s from %d to position (1-%d): "
                     (plist-get ref :label)
                     (1+ current-index)
                     count)
             (1+ current-index)))
           (target-index (max 0 (min (1- count) (1- target-index-input))))
           (without-ref (seq-remove (lambda (it) (equal it ref)) refs))
           (prefix (seq-take without-ref target-index))
           (suffix (seq-drop without-ref target-index))
           (next-order (append prefix (list ref) suffix)))
      (disco-room--rewrite-draft-attachment-order next-order)
      (message "disco: moved %s to position %d"
               (plist-get ref :label)
               (1+ target-index)))))

(defun disco-room--message-id-required-at-point ()
  "Return message ID at point, or signal user error."
  (or (disco-room--message-id-at-point)
      (user-error "disco: point is not on a message")))

(defun disco-room--forward-source-message (source-channel-id message-id)
  "Resolve SOURCE-CHANNEL-ID/MESSAGE-ID to a message object, or nil."
  (let ((channel-id (disco-msg-normalize-id source-channel-id))
        (target-id (disco-msg-normalize-id message-id)))
    (when (and channel-id target-id)
      (or (seq-find (lambda (msg)
                      (equal (disco-msg-normalize-id (alist-get 'id msg))
                             target-id))
                    (or (disco-state-messages channel-id) '()))
          (disco-api-channel-message channel-id target-id)))))

(defun disco-room--forward-only-select-values (prompt choices)
  "Read one or more values from CHOICES with PROMPT.

CHOICES is an alist of (LABEL . VALUE). Empty input means no selection."
  (if (null choices)
      nil
    (let* ((labels (mapcar #'car choices))
           (picked (completing-read-multiple
                    (format "%s (RET to skip)" prompt)
                    labels
                    nil
                    t
                    nil
                    nil
                    ""))
           (normalized
            (delq nil
                  (mapcar (lambda (label)
                            (let ((text (string-trim (or label ""))))
                              (unless (string-empty-p text)
                                text)))
                          picked)))
           values)
      (dolist (label normalized)
        (let ((entry (assoc label choices)))
          (unless entry
            (user-error "disco: invalid forward-only selection `%s'" label))
          (push (cdr entry) values)))
      (delete-dups (nreverse values)))))

(defun disco-room--read-forward-only-from-message (source-message)
  "Read `forward_only' payload by selecting embeds/attachments from SOURCE-MESSAGE."
  (let* ((embeds (or (alist-get 'embeds source-message) '()))
         (attachments (or (alist-get 'attachments source-message) '()))
         (embed-choices nil)
         (attachment-choices nil)
         (idx 0)
         payload)
    (dolist (embed embeds)
      (let* ((kind (or (alist-get 'type embed) "embed"))
             (title (string-trim (or (alist-get 'title embed)
                                     (alist-get 'description embed)
                                     (alist-get 'url embed)
                                     "(no title)")))
             (label (format "#%d [%s] %s" idx kind title)))
        (push (cons label idx) embed-choices)
        (setq idx (1+ idx))))
    (setq embed-choices (nreverse embed-choices))
    (dolist (attachment attachments)
      (let* ((attachment-id (disco-msg-normalize-id (alist-get 'id attachment)))
             (filename (or (alist-get 'filename attachment) "(unnamed)"))
             (label (and attachment-id
                         (format "%s %s" attachment-id filename))))
        (when (and label attachment-id)
          (push (cons label attachment-id) attachment-choices))))
    (setq attachment-choices (nreverse attachment-choices))
    (unless (or embed-choices attachment-choices)
      (user-error "disco: source message has no embeds or attachments to subset"))
    (let ((picked-embed-indices
           (disco-room--forward-only-select-values
            "Pick embeds to forward (comma list): "
            embed-choices))
          (picked-attachment-ids
           (disco-room--forward-only-select-values
            "Pick attachments to forward (comma list): "
            attachment-choices)))
      (when picked-embed-indices
        (push `(embed_indices . ,(vconcat picked-embed-indices)) payload))
      (when picked-attachment-ids
        (push `(attachment_ids . ,(vconcat picked-attachment-ids)) payload))
      (unless payload
        (user-error "disco: forward-only selection is empty"))
      (nreverse payload))))

(defun disco-room--read-forward-only (&optional source-channel-id message-id)
  "Read optional forward_only selection from minibuffer prompts."
  (when (y-or-n-p "Forward only selected embeds/attachments? ")
    (let ((source-message (disco-room--forward-source-message
                           source-channel-id
                           message-id)))
      (unless source-message
        (user-error "disco: source message unavailable for forward-only"))
      (disco-room--read-forward-only-from-message source-message))))

(defun disco-room--send-allowed-mentions (&optional replying-p)
  "Return normalized allowed_mentions payload for outgoing message send/edit.

When REPLYING-P is non-nil and reply-mention is enabled, include
`replied_user'."
  (let* ((allowed-mentions (disco-room--input-option-allowed-mentions))
         (base
          (cond
           ((null allowed-mentions) nil)
           ((eq allowed-mentions 'none) '((parse . [])))
           ((eq allowed-mentions 'all)
            '((parse . ["users" "roles" "everyone"])))
           ((listp allowed-mentions) (copy-tree allowed-mentions))
           (t (error "disco: invalid allowed mentions policy")))))
    (when (and replying-p
               (disco-room--input-option-reply-mention-replied-user))
      (let ((value t))
        (if (listp base)
            (let ((cell (assq 'replied_user base)))
              (if cell
                  (setcdr cell value)
                (setq base (append base `((replied_user . ,value))))))
          (setq base `((replied_user . ,value))))))
    base))

(defun disco-room-draft-history-search (regexp)
  "Load one draft-history entry matching REGEXP."
  (interactive
   (list (read-regexp "Draft history search (regexp): "
                      nil
                      'disco-room-draft-history-search-history)))
  (let* ((entries (cl-delete-duplicates
                   (appkit-chatbuf-input-history-elements)
                   :test #'equal))
         (matches (seq-filter (lambda (entry)
                                (and (stringp entry)
                                     (string-match-p regexp entry)))
                              entries)))
    (cond
     ((null matches)
      (message "disco: no draft history entry matches %s" regexp))
     (t
      (let ((picked (if (= 1 (length matches))
                        (car matches)
                      (completing-read "Matching draft: " matches nil t nil nil
                                       (car matches)))))
        (appkit-chatbuf-input-history-reset)
        (disco-room--set-draft picked)
        (message "disco: loaded draft history match"))))))

(defun disco-room--owned-preview-buffer ()
  "Return the explicitly owned composer preview buffer, creating it if needed."
  (or (and (buffer-live-p disco-room--preview-buffer)
           (buffer-local-value 'disco-room--preview-buffer-owner-p
                               disco-room--preview-buffer)
           disco-room--preview-buffer)
      (let* ((named (get-buffer disco-room--preview-buffer-name))
             (buffer
              (if (and (buffer-live-p named)
                       (buffer-local-value
                        'disco-room--preview-buffer-owner-p named))
                  named
                ;; A display-name collision never transfers ownership of an
                ;; ordinary user buffer to Disco.
                (generate-new-buffer disco-room--preview-buffer-name))))
        (with-current-buffer buffer
          (setq-local disco-room--preview-buffer-owner-p t))
        (setq disco-room--preview-buffer buffer))))

;;; Input options and attachment commands

(defun disco-room-input-preview (&optional prefix)
  "Show an immutable semantic preview using PREFIX source-codec selection."
  (interactive "P")
  (let* ((capture (appkit-markup-compose-capture prefix))
         (document (appkit-markup-compose-document capture))
         (attachments (disco-room--capture-attachments capture))
         (buf (disco-room--owned-preview-buffer))
         (mode-label (pcase (plist-get (appkit-chatbuf-aux-state) :aux-type)
                       ('edit "edit")
                       ('reply "reply")
                       (_ "message")))
         (codec-label
          (appkit-markup-compose-codec-label
           (appkit-markup-compose-capture-codec capture))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format "Room: %s\n"
                 (or disco-room--channel-name
                     disco-room--channel-id "(unknown)")))
        (insert (format "Composer mode: %s\n" mode-label))
        (insert (format "Source format: %s\n" codec-label))
        (insert (format "Attachments: %d\n\n" (length attachments)))
        (insert "Content:\n")
        (if (string-empty-p (appkit-markup-plain-text document))
            (insert "(empty)\n")
          (appkit-markup-compose-preview capture))
        (when attachments
          (insert "\nAttachments:\n")
          (dolist (attachment attachments)
            (insert
             (format "- %s\n"
                     (disco-room--attachment-label
                      attachment "[file]")))))
        (special-mode)
        (setq-local disco-room--preview-buffer-owner-p t)))
    (display-buffer buf)
    (message "disco: opened %s composer preview" codec-label)))

(defun disco-room-attach (attach-type)
  "Choose ATTACH-TYPE and invoke its configured attachment command."
  (interactive
   (list
    (completing-read
     "Attachment type: "
     (mapcar #'car disco-room-attach-commands)
     nil t)))
  (let ((command (cadr (assoc attach-type disco-room-attach-commands))))
    (unless (commandp command)
      (user-error "disco: invalid attachment command for %s" attach-type))
    (call-interactively command)))

(defun disco-room-toggle-send-on-return ()
  "Toggle whether `RET' sends the current room draft."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :send-on-return
                             (not (plist-get current :send-on-return)))))))
    (message "disco: RET now %s"
             (if (plist-get state :send-on-return)
                 "sends messages"
               "opens draft editor"))))

(defun disco-room-cycle-long-message-action ()
  "Cycle long-message send behavior for current room buffer."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :long-message-action
                             (pcase (plist-get current :long-message-action)
                               ('split 'file)
                               (_ 'split)))))))
    (message "disco: long messages now %s"
             (pcase (plist-get state :long-message-action)
               ('file "send as file")
               (_ "split across messages")))))

(defun disco-room-cycle-allowed-mentions ()
  "Cycle allowed-mentions policy for current room buffer."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :allowed-mentions
                             (pcase (plist-get current :allowed-mentions)
                               ('none 'all)
                               ('all nil)
                               (_ 'none)))))))
    (message "disco: allowed mentions now %s"
             (pcase (plist-get state :allowed-mentions)
               ('none "disabled")
               ('all "explicitly enabled")
               (_ "Discord defaults")))))

(defun disco-room-toggle-reply-mention-replied-user ()
  "Toggle whether replies mention the replied user in current room."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :reply-mention-replied-user
                             (not (plist-get current :reply-mention-replied-user)))))))
    (message "disco: reply mention of replied user %s"
             (if (plist-get state :reply-mention-replied-user)
                 "enabled"
               "disabled"))))

(defun disco-room-reset-input-options ()
  "Reset room-local input option overrides back to global defaults."
  (interactive)
  (dolist (var '(disco-room-send-on-return
                 disco-room-long-message-action
                 disco-room-allowed-mentions
                 disco-room-reply-mention-replied-user))
    (kill-local-variable var))
  (disco-room--set-input-options-state (disco-room--current-input-options-state))
  (message "disco: room input options reset to global defaults"))

(transient-define-prefix disco-room-input-options-transient ()
  "Transient for telega-like room input options."
  [["Input Options"
    ("RET" "Toggle RET send/editor" disco-room-toggle-send-on-return)
    ("l" "Cycle long-message action" disco-room-cycle-long-message-action)
    ("m" "Cycle allowed mentions" disco-room-cycle-allowed-mentions)
    ("r" "Toggle reply mention" disco-room-toggle-reply-mention-replied-user)
    ("0" "Reset room-local options" disco-room-reset-input-options)]])

(defun disco-room-attach-file (path &optional description spoiler)
  "Queue attachment PATH for next room send.

DESCRIPTION is optional per-file description.  With interactive prefix, mark
the attachment as a spoiler."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--attach-unavailable-reason)
      "attach files")
     (let* ((path (read-file-name "Attach file: " nil nil t))
            (description-input
             (string-trim
              (read-string "Attachment description (optional): ")))
            (description
             (unless (string-empty-p description-input) description-input)))
       (list path description (and current-prefix-arg t)))))
  (disco-room--ensure-action-available
   (disco-room--attach-unavailable-reason)
   "attach files")
  (unless (file-readable-p path)
    (user-error "disco: file is not readable: %s" path))
  (let ((attachment
         (disco-room--make-attachment-input-object
          path :description description :spoiler spoiler)))
    (if (appkit-chatbuf-input-start-position)
        (progn
          (unless (appkit-chatbuf-point-in-input-p)
            (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
          (disco-room--insert-attachment-input-object attachment)
          (disco-room--sync-draft-from-buffer))
      (let* ((draft (disco-room--current-draft))
             (separator (if (or (string-empty-p (appkit-chatbuf-string-plain-text draft))
                                (string-match-p "[ \t\n]\\'"
                                                (appkit-chatbuf-string-plain-text draft)))
                            ""
                          " ")))
        (disco-room--set-draft
         (concat draft
                 separator
                 (disco-room--attachment-input-object-string attachment)))))
    (message "disco: queued attachment %s"
             (file-name-nondirectory path))))

(defun disco-room-clear-attachments ()
  "Clear queued attachments for next send in current room."
  (interactive)
  (disco-room--ensure-action-available
   (when (disco-room--composer-edit-active-p)
     "attachments are unavailable while editing a message")
   "clear attachments")
  (setq disco-room--pending-attachments nil)
  (when disco-room--attachment-token-table
    (clrhash disco-room--attachment-token-table))
  (disco-room--set-draft
   (string-trim-right (disco-room--draft-without-attachment-tokens)))
  (message "disco: cleared queued attachments"))

(defun disco-room--clear-pending-attachment-state ()
  "Clear queued attachment state without touching the current draft text."
  (setq disco-room--pending-attachments nil)
  (when disco-room--attachment-token-table
    (clrhash disco-room--attachment-token-table)))

(defun disco-room--message-content-over-limit-p (content)
  "Return non-nil when CONTENT exceeds Discord's single-message limit."
  (and (stringp content)
       (> (length content) disco-api--message-content-limit)))

;;; Send pipeline

(defun disco-room--write-long-message-temp-attachment (content)
  "Write CONTENT to a temporary text file attachment plist."
  (let ((path (make-temp-file "disco-message-" nil ".txt"))
        (coding-system-for-write 'utf-8))
    (with-temp-file path
      (insert (or content "")))
    (list :path path
          :filename disco-room-long-message-file-name
          :content-type "text/plain; charset=utf-8")))

(defun disco-room--send-sticker-object (sticker)
  "Send one normalized STICKER in the current room."
  (let ((sticker-id (disco-sticker-id sticker)))
    (unless sticker-id
      (user-error "disco: selected Sticker has no valid ID"))
    (disco-room--ensure-action-available
     (disco-room--sticker-unavailable-reason)
     "send stickers")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     (disco-room--required-send-permissions)
     :action "sending stickers")
    (if disco-room--send-in-flight
        (message "disco: send already in progress")
      (let ((room-buffer (current-buffer))
            (channel-id disco-room--channel-id)
            (view (disco-room--ensure-surface))
            (request-revision
             (disco-state-message-revision disco-room--channel-id)))
        (setq disco-room--send-in-flight t)
        (disco-room--queue-update view 'frame)

        (disco-api-send-message-async
         channel-id nil
         :sticker-ids (list sticker-id)
         :on-success
         (lambda (response)
           (when (and (listp response) (alist-get 'id response))
             (disco-state-merge-message-response
              channel-id response request-revision))
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room--request-render view)
               (message
                (if (and (listp response) (alist-get 'id response))
                    "disco: sticker sent"
                  "disco: sticker send failed: Discord returned no message")))))
         :on-error
         (lambda (err)
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room--request-render view)
               (message "disco: sticker send failed: %s"
                        (disco-room--async-error-message err))))))))))

(defun disco-room--read-and-send-sticker (&optional ranked-only)
  "Read and send a sticker, optionally restricting to account rankings."
  (when-let* ((sticker
               (disco-sticker-read disco-room--guild-id ranked-only)))
    (disco-room--send-sticker-object sticker)))

(defun disco-room-send-sticker (&optional ranked-only)
  "Select and send a sticker.

With prefix RANKED-ONLY, offer only Favorite and Frequently Used stickers."
  (interactive "P")
  (disco-room--ensure-action-available
   (disco-room--sticker-unavailable-reason)
   "send stickers")
  (cond
   (disco-room--send-in-flight
    (message "disco: send already in progress"))
   ((disco-sticker-ready-p disco-room--guild-id)
    (disco-sticker-ensure-ready disco-room--guild-id)
    (disco-room--read-and-send-sticker ranked-only))
   (disco-room--sticker-picker-pending
    (message "disco: sticker catalog is still loading"))
   (t
    (let ((room-buffer (current-buffer))
          (channel-id disco-room--channel-id)
          (guild-id disco-room--guild-id)
          (view (disco-room--ensure-surface)))
      (setq disco-room--sticker-picker-pending t)
      (message "disco: loading sticker catalog…")
      (disco-sticker-ensure-ready
       guild-id
       :on-success
       (lambda ()
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (setq disco-room--sticker-picker-pending nil)
             (run-at-time
              0 nil
              (lambda ()
                (when (disco-room--channel-buffer-p
                       room-buffer channel-id view)
                  (with-current-buffer room-buffer
                    (disco-room--read-and-send-sticker ranked-only))))))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (setq disco-room--sticker-picker-pending nil)
             (message "disco: sticker catalog load failed: %s"
                      (disco-room--async-error-message err))))))))))

;;;; Send operation model

(cl-defstruct
    (disco-room--send-plan
     (:constructor disco-room--send-plan-create))
  "Immutable capture and provider policy for one composer submission."
  draft
  document
  content
  attachments
  edit-message-id
  edit-message
  reply-to
  allowed-mentions
  long-message-action
  needs-attach-files-p
  required-permissions)

(cl-defstruct
    (disco-room--send-operation
     (:constructor disco-room--send-operation-create))
  "Mutable exactly-once lifecycle for one send or edit operation."
  plan
  room-buffer
  channel-id
  view
  slot
  recovery-slot
  cleared-revision
  settled-p)

(cl-defstruct
    (disco-room--send-leg
     (:constructor disco-room--send-leg-create))
  "Exactly-once transport lifecycle for one Discord create request."
  operation
  nonce
  request-revision
  text
  on-success
  on-error
  settled-p)

(defun disco-room--capture-send-plan (&optional prefix)
  "Capture one immutable composer send plan selected by PREFIX."
  (let* ((draft
          (appkit-chatbuf-copy-string (disco-room--current-draft)))
         (capture (appkit-markup-compose-capture prefix))
         (document (appkit-markup-compose-document capture))
         (output
          (appkit-markup-compose-output capture 'discord-markdown))
         (losses (appkit-markup-compose-output-losses output))
         (attachments (disco-room--capture-attachments capture))
         (content
          (string-trim-right
           (appkit-markup-compose-output-source output)))
         (edit-message-id (disco-room--composer-edit-message-id)))
    (when losses
      (user-error
       "disco: selected source format cannot be encoded without semantic loss: %S"
       (mapcar #'appkit-markup-loss-kind losses)))
    (let* ((reply-to (disco-room--composer-reply-message-id))
           (long-message-action
            (and (disco-room--message-content-over-limit-p content)
                 (disco-room--input-option-long-message-action)))
           (needs-attach-files-p
            (or attachments (eq long-message-action 'file))))
      (disco-room--send-plan-create
       :draft draft
       :document document
       :content content
       :attachments (copy-tree attachments)
       :edit-message-id edit-message-id
       :edit-message
       (and edit-message-id
            (disco-room--composer-context-message edit-message-id))
       :reply-to reply-to
       :allowed-mentions
       (disco-room--send-allowed-mentions (not (null reply-to)))
       :long-message-action long-message-action
       :needs-attach-files-p needs-attach-files-p
       :required-permissions
       (append
        (disco-room--required-send-permissions)
        (when needs-attach-files-p '(attach-files))
        (when reply-to '(read-message-history)))))))

(defun disco-room--send-plan-empty-p (plan)
  "Return non-nil when PLAN has no create or edit payload."
  (and (string-empty-p (disco-room--send-plan-content plan))
       (null (disco-room--send-plan-attachments plan))
       (null (disco-room--send-plan-edit-message-id plan))))

(defun disco-room--new-send-operation (plan)
  "Return an unstarted send operation owning PLAN's composer slot."
  (let ((slot (disco-room--composer-operation-slot)))
    (setq slot
          (plist-put
           slot :draft
           (appkit-chatbuf-copy-string
            (disco-room--send-plan-draft plan))))
    (disco-room--send-operation-create
     :plan plan
     :room-buffer (current-buffer)
     :channel-id disco-room--channel-id
     :view (disco-room--ensure-surface)
     :slot slot
     :recovery-slot (copy-tree slot))))

(defun disco-room--send-operation-active-p (operation)
  "Return non-nil when OPERATION still owns its captured room view."
  (disco-room--channel-buffer-p
   (disco-room--send-operation-room-buffer operation)
   (disco-room--send-operation-channel-id operation)
   (disco-room--send-operation-view operation)))

(defun disco-room--send-operation-claim-settlement (operation)
  "Claim OPERATION's terminal transition exactly once."
  (unless (disco-room--send-operation-settled-p operation)
    (setf (disco-room--send-operation-settled-p operation) t)
    t))

(defun disco-room--begin-send-operation (operation)
  "Clear OPERATION's composer slot and mark its room in flight."
  (let ((buffer (disco-room--send-operation-room-buffer operation))
        (view (disco-room--send-operation-view operation)))
    (with-current-buffer buffer
      (setf (disco-room--send-operation-cleared-revision operation)
            (disco-room--clear-composer-operation-slot))
      (setq disco-room--send-in-flight t)
      (disco-room--queue-update view 'frame)))
  operation)

(defun disco-room--abort-send-operation (operation)
  "Restore OPERATION after a synchronous dispatch failure."
  (when (disco-room--send-operation-claim-settlement operation)
    (when (disco-room--send-operation-active-p operation)
      (with-current-buffer
          (disco-room--send-operation-room-buffer operation)
        (setq disco-room--send-in-flight nil)
        (when (integerp
               (disco-room--send-operation-cleared-revision operation))
          (disco-room--restore-composer-operation-slot
           (disco-room--send-operation-cleared-revision operation)
           (disco-room--send-operation-recovery-slot operation)
           t))
        (disco-room--request-render
         (disco-room--send-operation-view operation))))))

(defun disco-room--settle-send-success (operation text)
  "Settle OPERATION successfully and display TEXT."
  (when (disco-room--send-operation-claim-settlement operation)
    (when (disco-room--send-operation-active-p operation)
      (with-current-buffer
          (disco-room--send-operation-room-buffer operation)
        (setq disco-room--send-in-flight nil)
        (disco-room--request-render
         (disco-room--send-operation-view operation))
        (message "%s" text)))))

(defun disco-room--settle-send-failure (operation error-data text)
  "Settle failed OPERATION, restore its remainder, and display TEXT."
  (when (disco-room--send-operation-claim-settlement operation)
    (when (disco-room--send-operation-active-p operation)
      (with-current-buffer
          (disco-room--send-operation-room-buffer operation)
        (setq disco-room--send-in-flight nil)
        (let ((restored
               (disco-room--restore-composer-operation-slot
                (disco-room--send-operation-cleared-revision operation)
                (disco-room--send-operation-recovery-slot operation)
                t)))
          (disco-room--request-render
           (disco-room--send-operation-view operation))
          (message
           "%s%s: %s"
           text
           (if restored " (draft restored)" "")
           (disco-room--async-error-message error-data)))))))

;;;; Edit operation

(defun disco-room--settle-edit-failure
    (operation edit-message-id error-data)
  "Settle failed edit OPERATION for EDIT-MESSAGE-ID."
  (when (disco-room--send-operation-claim-settlement operation)
    (when (disco-room--send-operation-active-p operation)
      (with-current-buffer
          (disco-room--send-operation-room-buffer operation)
        (setq disco-room--send-in-flight nil)
        (disco-room--restore-composer-operation-slot
         (disco-room--send-operation-cleared-revision operation)
         (disco-room--send-operation-slot operation)
         t)
        (disco-room--request-render
         (disco-room--send-operation-view operation))
        (message
         "disco: edit failed for %s: %s"
         edit-message-id
         (disco-room--async-error-message error-data))))))

(defun disco-room--settle-edit-success
    (operation edit-message-id saved-state request-revision response)
  "Settle successful edit OPERATION from RESPONSE."
  (if (not (and (listp response) (alist-get 'id response)))
      (disco-room--settle-edit-failure
       operation edit-message-id
       (list 'error "Discord edit-message returned no message"))
    (when (disco-room--send-operation-claim-settlement operation)
      (disco-state-merge-message-response
       (disco-room--send-operation-channel-id operation)
       response request-revision)
      (when (disco-room--send-operation-active-p operation)
        (with-current-buffer
            (disco-room--send-operation-room-buffer operation)
          (setq disco-room--send-in-flight nil)
          (when
              (= (disco-room--send-operation-cleared-revision operation)
                 (appkit-chatbuf-composer-revision))
            (disco-room--composer-edit-restore-state saved-state t))
          (disco-room--request-render
           (disco-room--send-operation-view operation))
          (message "disco: edited message %s" edit-message-id))))))

(defun disco-room--send-edit-operation (plan)
  "Validate and dispatch the edit described by PLAN."
  (let* ((content (disco-room--send-plan-content plan))
         (edit-message-id
          (disco-room--send-plan-edit-message-id plan))
         (operation (disco-room--new-send-operation plan))
         (channel-id
          (disco-room--send-operation-channel-id operation))
         (request-revision
          (disco-state-message-revision channel-id))
         (saved-state
          (copy-tree
           (plist-get
            (plist-get
             (disco-room--send-operation-slot operation)
             :pending-edit)
            :saved-state))))
    (disco-api--validate-message-content-length content "content")
    (disco-room--ensure-action-available
     (disco-room--edit-permission-reason
      (disco-room--send-plan-edit-message plan))
     "edit messages")
    (when (disco-room--send-plan-attachments plan)
      (user-error
       "disco: editing via composer does not support attachments yet"))
    (condition-case err
        (progn
          (disco-room--begin-send-operation operation)
          (disco-api-edit-message-async
           channel-id edit-message-id content
           :allowed-mentions
           (disco-room--send-plan-allowed-mentions plan)
           :on-success
           (lambda (response)
             (disco-room--settle-edit-success
              operation edit-message-id saved-state
              request-revision response))
           :on-error
           (lambda (error-data)
             (disco-room--settle-edit-failure
              operation edit-message-id error-data))))
      (error
       (disco-room--abort-send-operation operation)
       (signal (car err) (cdr err))))))

;;;; Create-message legs

(defun disco-room--send-leg-claim-settlement (leg)
  "Claim LEG's terminal transport transition exactly once."
  (unless (disco-room--send-leg-settled-p leg)
    (setf (disco-room--send-leg-settled-p leg) t)
    t))

(defun disco-room--send-leg-failure (leg error-data)
  "Settle failed LEG with ERROR-DATA."
  (when (disco-room--send-leg-claim-settlement leg)
    (let ((operation (disco-room--send-leg-operation leg)))
      (disco-state-remove-pending-message
       (disco-room--send-operation-channel-id operation)
       (disco-room--send-leg-nonce leg))
      (funcall (disco-room--send-leg-on-error leg) error-data))))

(defun disco-room--send-leg-success (leg response)
  "Settle successful LEG from Discord RESPONSE."
  (if (not (and (listp response) (alist-get 'id response)))
      (disco-room--send-leg-failure
       leg (list 'error "Discord create-message returned no message"))
    (when (disco-room--send-leg-claim-settlement leg)
      (let* ((operation (disco-room--send-leg-operation leg))
             (channel-id
              (disco-room--send-operation-channel-id operation))
             (accepted-response (copy-tree response)))
        ;; Discord normally returns a complete Message object.  Preserve the
        ;; exact sent wire content if a transport omits it.
        (unless (stringp (alist-get 'content accepted-response))
          (setf (alist-get 'content accepted-response)
                (or (disco-room--send-leg-text leg) "")))
        (disco-state-merge-message-response
         channel-id accepted-response
         (disco-room--send-leg-request-revision leg)
         (disco-room--send-leg-nonce leg))
        (when (disco-room--send-operation-active-p operation)
          (with-current-buffer
              (disco-room--send-operation-room-buffer operation)
            (when-let* ((message
                         (disco-room--channel-message-by-id
                          channel-id
                          (alist-get 'id accepted-response))))
              (disco-room--enqueue-local-create-response
               (disco-room--send-operation-view operation)
               channel-id message))))
        (funcall
         (disco-room--send-leg-on-success leg)
         accepted-response)))))

(defun disco-room--dispatch-message-leg
    (operation text semantic-document reply attachments
               on-success on-error)
  "Dispatch one create-message leg owned by OPERATION."
  (let* ((plan (disco-room--send-operation-plan operation))
         (channel-id
          (disco-room--send-operation-channel-id operation))
         (view (disco-room--send-operation-view operation))
         (request-revision
          (disco-state-message-revision channel-id))
         (nonce (disco-room--next-send-nonce))
         (pending-content
          (if (and attachments
                   (or (not (stringp text)) (string-empty-p text)))
              (format
               "Uploading %d attachment%s…"
               (length attachments)
               (if (= (length attachments) 1) "" "s"))
            text))
         (pending-document
          (or semantic-document
              (and
               (equal text (disco-room--send-plan-content plan))
               (disco-room--send-plan-document plan))))
         (leg
          (disco-room--send-leg-create
           :operation operation
           :nonce nonce
           :request-revision request-revision
           :text text
           :on-success on-success
           :on-error on-error)))
    (disco-state-insert-pending-message
     channel-id nonce pending-content
     (disco-gateway-current-user-id)
     reply pending-document)
    (disco-room--request-render view)
    (condition-case dispatch-error
        (if attachments
            (disco-api-send-message-with-attachments-async
             channel-id
             :content
             (and (stringp text)
                  (not (string-empty-p text))
                  text)
             :reply-to-message-id reply
             :allowed-mentions
             (and (stringp text)
                  (not (string-empty-p text))
                  (disco-room--send-plan-allowed-mentions plan))
             :attachments attachments
             :nonce nonce
             :on-success
             (lambda (response)
               (disco-room--send-leg-success leg response))
             :on-error
             (lambda (error-data)
               (disco-room--send-leg-failure leg error-data)))
          (disco-api-send-message-async
           channel-id text
           :reply-to-message-id reply
           :allowed-mentions
           (and (stringp text)
                (not (string-empty-p text))
                (disco-room--send-plan-allowed-mentions plan))
           :nonce nonce
           :on-success
           (lambda (response)
             (disco-room--send-leg-success leg response))
           :on-error
           (lambda (error-data)
             (disco-room--send-leg-failure leg error-data))))
      (error
       (disco-room--send-leg-failure leg dispatch-error)
       (signal (car dispatch-error) (cdr dispatch-error))))))

;;;; Create-message strategies

(defun disco-room--split-recovery-slot (slot remaining)
  "Return SLOT prepared to recover unsent REMAINING chunks."
  (let ((recovery (copy-tree slot)))
    (setq recovery
          (plist-put
           recovery :draft
           (mapconcat
            (lambda (chunk) (plist-get chunk :source))
            remaining "\n\n"))
          recovery (plist-put recovery :active-codec 'discord-markdown)
          recovery (plist-put recovery :pending-edit nil)
          recovery (plist-put recovery :pending-reply-to nil)
          recovery (plist-put recovery :attachment-token-seq 0)
          recovery (plist-put recovery :attachment-token-entries nil))
    recovery))

(defun disco-room--send-split-next
    (operation remaining sent-count total)
  "Send OPERATION's REMAINING chunks after SENT-COUNT of TOTAL."
  (let* ((plan (disco-room--send-operation-plan operation))
         (chunk (car remaining))
         (rest (cdr remaining))
         (first-p (= sent-count 0)))
    (disco-room--dispatch-message-leg
     operation
     (plist-get chunk :source)
     (plist-get chunk :document)
     (and first-p (disco-room--send-plan-reply-to plan))
     (and first-p (disco-room--send-plan-attachments plan))
     (lambda (_response)
       (if rest
           (progn
             (setf
              (disco-room--send-operation-recovery-slot operation)
              (disco-room--split-recovery-slot
               (disco-room--send-operation-recovery-slot operation)
               rest))
             (disco-room--send-split-next
              operation rest (1+ sent-count) total))
         (disco-room--settle-send-success
          operation
          (format "disco: sent %d split messages" total))))
     (lambda (error-data)
       (disco-room--settle-send-failure
        operation error-data
        (format
         "disco: sent %d/%d split messages"
         sent-count total))))))

(defun disco-room--send-split-operation (operation)
  "Dispatch OPERATION as semantic provider chunks."
  (let* ((document
          (disco-room--send-plan-document
           (disco-room--send-operation-plan operation)))
         (chunks
          (condition-case nil
              (disco-markdown-print-chunks
               document disco-api--message-content-limit)
            (appkit-markup-codec-error
             (user-error
              "disco: message cannot be split without breaking semantic structure; choose file mode")))))
    (disco-room--send-split-next
     operation chunks 0 (length chunks))))

(defun disco-room--send-file-operation (operation)
  "Dispatch OPERATION's text as a temporary attachment."
  (let* ((plan (disco-room--send-operation-plan operation))
         (text-attachment
          (disco-room--write-long-message-temp-attachment
           (disco-room--send-plan-content plan)))
         (attachments
          (append
           (disco-room--send-plan-attachments plan)
           (list text-attachment))))
    (unwind-protect
        (disco-room--dispatch-message-leg
         operation nil nil
         (disco-room--send-plan-reply-to plan)
         attachments
         (lambda (_response)
           (disco-room--settle-send-success
            operation
            (format
             "disco: long message sent as %s"
             disco-room-long-message-file-name)))
         (lambda (error-data)
           (disco-room--settle-send-failure
            operation error-data "disco: send failed")))
      (ignore-errors
        (delete-file (plist-get text-attachment :path))))))

(defun disco-room--send-single-operation (operation)
  "Dispatch OPERATION as one Discord message."
  (let ((plan (disco-room--send-operation-plan operation)))
    (disco-room--dispatch-message-leg
     operation
     (disco-room--send-plan-content plan)
     (disco-room--send-plan-document plan)
     (disco-room--send-plan-reply-to plan)
     (disco-room--send-plan-attachments plan)
     (lambda (_response)
       (disco-room--settle-send-success
        operation
        (if (disco-room--send-plan-attachments plan)
            "disco: message with attachment(s) sent"
          "disco: message sent")))
     (lambda (error-data)
       (disco-room--settle-send-failure
        operation error-data "disco: send failed")))))

(defun disco-room--send-create-operation (plan)
  "Validate and dispatch the create operation described by PLAN."
  (disco-room--ensure-action-available
   (disco-room--room-send-restriction-reason
    (append
     (when (disco-room--send-plan-needs-attach-files-p plan)
       '(attach-files))
     (when (disco-room--send-plan-reply-to plan)
       '(read-message-history))))
   "send messages")
  (disco-permission-ensure-channel
   (disco-room--channel-object)
   (disco-room--send-plan-required-permissions plan)
   :action "sending messages")
  (unless (string-empty-p (disco-room--send-plan-content plan))
    (appkit-chatbuf-input-history-push
     (disco-room--send-plan-content plan)))
  (let ((operation (disco-room--new-send-operation plan)))
    (condition-case err
        (progn
          (disco-room--begin-send-operation operation)
          (pcase (disco-room--send-plan-long-message-action plan)
            ('split (disco-room--send-split-operation operation))
            ('file (disco-room--send-file-operation operation))
            (_ (disco-room--send-single-operation operation))))
      (error
       (disco-room--abort-send-operation operation)
       (signal (car err) (cdr err))))))

;;;; Codec-specific send entry points

(defun disco-room-send-message-with-codec (codec)
  "Send the current draft once using CODEC without changing active codec."
  (interactive
   (progn
     (unless appkit-markup-compose-codecs
       (user-error "disco: room composer has no configured source codecs"))
     (list
      (intern
       (completing-read
        "Send with codec: "
        (mapcar #'symbol-name appkit-markup-compose-codecs)
        nil t nil nil
        (and appkit-markup-compose-active-codec
             (symbol-name appkit-markup-compose-active-codec)))))))
  (unless (memq codec appkit-markup-compose-codecs)
    (user-error "disco: codec is not configured: %s" codec))
  (let ((appkit-markup-compose-active-codec codec))
    (disco-room-send-message)))

;;;; Interactive entry

(defun disco-room-send-message (&optional prefix)
  "Capture and send the current semantic draft using PREFIX source format."
  (interactive "P")
  (when (appkit-chatbuf-input-start-position)
    (appkit-chatbuf-input-prune-broken-objects)
    (disco-room--sync-draft-from-buffer))
  (disco-room--ensure-action-available
   (disco-room--send-message-unavailable-reason)
   (if (disco-room--composer-edit-active-p)
       "save edits"
     "send messages"))
  (if disco-room--send-in-flight
      (message "disco: send already in progress")
    (let ((plan (disco-room--capture-send-plan prefix)))
      (cond
       ((disco-room--send-plan-empty-p plan)
        (message "disco: draft is empty"))
       ((disco-room--send-plan-edit-message-id plan)
        (disco-room--send-edit-operation plan))
       (t
        (disco-room--send-create-operation plan))))))

;;; Reply, forward, and edit commands

(defun disco-room--reply-to-msg (msg)
  "Set pending reply target to MSG for the next send."
  (let ((message-id (alist-get 'id msg)))
    (unless (and (stringp message-id) (not (string-empty-p message-id)))
      (user-error "disco: message has no id to reply to"))
    (disco-room-reply-to-message message-id)))

(defun disco-room-reply-to-message (&optional message-id)
  "Set pending reply target MESSAGE-ID for next send.

When called interactively, defaults to message under point."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--reply-unavailable-reason)
      "start replies")
     (let* ((at-point (ignore-errors (disco-room--message-id-at-point)))
            (fallback (or at-point (disco-room--latest-message-id)))
            (raw (read-string
                  (if fallback
                      (format "Reply to message ID (default %s): " fallback)
                    "Reply to message ID: "))))
       (list (if (string-empty-p raw)
                 (or fallback
                     (user-error "disco: no target message available"))
               raw)))))
  (disco-room--ensure-action-available
   (disco-room--reply-unavailable-reason)
   "start replies")
  (when (disco-room--composer-edit-active-p)
    (disco-room--composer-edit-clear t))
  (disco-room--set-composer-aux-state nil message-id)
  (disco-room--update-frame)
  (appkit-chatbuf-focus-input)
  (message "disco: next message will reply to %s" message-id))

(defun disco-room--forward-msg (msg)
  "Forward MSG into the current room, prompting only for optional extras."
  (let* ((message-id (alist-get 'id msg))
         (source-channel-id (or (alist-get 'channel_id msg) disco-room--channel-id))
         (content-raw (string-trim (read-string "Optional forward comment: ")))
         (forward-only (disco-room--read-forward-only source-channel-id message-id)))
    (unless (and (stringp message-id) (not (string-empty-p message-id)))
      (user-error "disco: message has no id to forward"))
    (unless (and (stringp source-channel-id) (not (string-empty-p source-channel-id)))
      (user-error "disco: message has no source channel id to forward"))
    (disco-room-forward-message
     message-id
     source-channel-id
     (unless (string-empty-p content-raw)
       content-raw)
     forward-only)))

(defun disco-room-forward-message (&optional message-id source-channel-id content forward-only)
  "Forward MESSAGE-ID from SOURCE-CHANNEL-ID into current room.

CONTENT is optional text sent alongside the forwarded reference.
FORWARD-ONLY optionally narrows embeds/attachments included in the forward."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--forward-unavailable-reason)
      "forward messages")
     (let* ((at-point (ignore-errors (disco-room--message-id-at-point)))
            (fallback-message (or at-point (disco-room--latest-message-id)))
            (message-raw (read-string
                          (if fallback-message
                              (format "Forward message ID (default %s): " fallback-message)
                            "Forward message ID: ")))
            (message-id (if (string-empty-p message-raw)
                            (or fallback-message
                                (user-error "disco: no message id provided"))
                          message-raw))
            (fallback-channel (or disco-room--channel-id ""))
            (channel-raw (read-string
                          (if (string-empty-p fallback-channel)
                              "Source channel ID: "
                            (format "Source channel ID (default %s): " fallback-channel))))
            (source-channel-id (if (string-empty-p channel-raw)
                                   (or fallback-channel
                                       (user-error "disco: no source channel id provided"))
                                 channel-raw))
            (content-raw (string-trim (read-string "Optional forward comment: ")))
            (forward-only (disco-room--read-forward-only source-channel-id message-id)))
       (list message-id
             source-channel-id
             (unless (string-empty-p content-raw)
               content-raw)
             forward-only))))
  (disco-room--ensure-action-available
   (disco-room--forward-unavailable-reason)
   "forward messages")
  (let* ((target-channel-id disco-room--channel-id)
         (source-channel-id (or source-channel-id disco-room--channel-id))
         (source-channel
          (and source-channel-id
               (disco-room--resolve-target-channel source-channel-id)))
         (normalized-content
          (and (stringp content)
               (let ((trimmed (string-trim content)))
                 (unless (string-empty-p trimmed)
                   trimmed))))
         (room-buffer (current-buffer))
         (view (disco-room--ensure-surface))
         request-revision
         (allowed-mentions
          (and normalized-content (disco-room--send-allowed-mentions)))
         settled-p)
    (disco-api--validate-message-content-length normalized-content "content")
    (unless (and message-id (not (string-empty-p (format "%s" message-id))))
      (user-error "disco: message id cannot be empty"))
    (unless (and source-channel-id
                 (not (string-empty-p (format "%s" source-channel-id))))
      (user-error "disco: source channel id cannot be empty"))
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     (disco-room--required-send-permissions)
     :action "forwarding messages")
    (disco-room--ensure-jump-permissions source-channel-id source-channel)
    (setq request-revision
          (disco-state-message-revision target-channel-id))
    (setq disco-room--send-in-flight t)
    (disco-room--queue-update view 'frame)

    (cl-labels
        ((room-active-p
           ()
           (disco-room--channel-buffer-p room-buffer target-channel-id view))
         (finish-error
           (error-data)
           (unless settled-p
             (setq settled-p t)
             (when (room-active-p)
               (with-current-buffer room-buffer
                 (setq disco-room--send-in-flight nil)
                 (disco-room--request-render view)
                 (message "disco: forward failed: %s"
                          (disco-room--async-error-message error-data))))))
         (finish-success
           (response)
           (if (not (and (listp response) (alist-get 'id response)))
               (finish-error
                (list 'error "disco: forward response has no message id"))
             (unless settled-p
               (setq settled-p t)
               (disco-state-merge-message-response
                target-channel-id response request-revision)
               (when (room-active-p)
                 (with-current-buffer room-buffer
                   (setq disco-room--send-in-flight nil)
                   (disco-room--request-render view)
                   (message "disco: forwarded message %s from channel %s"
                            message-id source-channel-id)))))))
      (condition-case err
          (disco-api-forward-message-async
           target-channel-id
           message-id
           source-channel-id
           :content normalized-content
           :forward-only forward-only
           :allowed-mentions (and normalized-content allowed-mentions)
           :on-success #'finish-success
           :on-error #'finish-error)
        (error
         (finish-error err)
         (signal (car err) (cdr err)))))))

(defun disco-room-cancel-reply ()
  "Cancel pending composer reply/edit context."
  (interactive)
  (cond
   ((disco-room--composer-edit-active-p)
    (disco-room--composer-edit-clear t)
    (disco-room--update-frame)
    (message "disco: edit target cleared"))
   ((disco-room--composer-reply-message-id)
    (disco-room--set-composer-aux-state nil nil)
    (disco-room--update-frame)
    (message "disco: reply target cleared"))
   (t
    (message "disco: no composer context to cancel"))))

(defun disco-room-return-dwim ()
  "RET behavior for room buffer.

An unresolved composer token owns RET so completion cannot fall through into
an accidental send.  On a projected Lottie Sticker, RET streams its native
frames; elsewhere outside the composer, RET returns point to the draft.
Inside the composer, send-on-return sends the draft and the disabled setting
opens the draft editor."
  (interactive)
  (cond
   ((not (appkit-chatbuf-point-in-input-p))
    (if-let* ((sticker (disco-room--lottie-sticker-at-point)))
        (disco-sticker-play sticker)
      (goto-char
       (or (appkit-chatbuf-input-logical-end-position) (point-max)))))
   ((disco-company-completion-token-at-point)
    (disco-room-complete-mention))
   ((disco-room--input-option-send-on-return)
    (disco-room-send-message))
   (t
    (disco-room-edit-draft))))

(defun disco-room--edit-msg (msg)
  "Enter composer edit mode for MSG in current room."
  (disco-room--ensure-action-available
   (disco-room--edit-start-unavailable-reason msg)
   "edit messages")
  (disco-room--composer-enter-edit msg))

(defun disco-room-edit-message ()
  "Enter composer edit mode for message at point in current room."
  (interactive)
  (disco-room--edit-msg (disco-room--message-at-point)))

(provide 'disco-room-compose)

;;; disco-room-compose.el ends here
