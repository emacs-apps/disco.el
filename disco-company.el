;;; disco-company.el --- Composer completion backends for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Completion helpers for room composer tokens (`@', `#', and `:').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-chat-completion)
(require 'appkit-chat-emoji)
(require 'disco-msg)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-util)
(require 'svg nil t)

(defvar company-mode)
(defvar company-backends)
(defvar completion-at-point-functions)
(defvar disco-room-enable-company-backend)
(defvar disco-room-show-avatar-images)

(declare-function disco-gateway-request-guild-members "disco-gateway")
(declare-function disco-gateway-running-p "disco-gateway")

(defcustom disco-company-show-user-avatars t
  "When non-nil, show user avatars in company completion annotations."
  :type 'boolean
  :group 'disco)

(defcustom disco-company-capf-avatar-size 'auto
  "Avatar pixel size for completion annotations.

`auto' keeps avatars slightly below current frame character height so
completion row height stays stable across CAPF/Corfu and company popups."
  :type '(choice
          (const :tag "Auto (fit to row height)" auto)
          integer)
  :group 'disco)

(defcustom disco-company-member-search-limit 100
  "Maximum guild members requested for one composer prefix search."
  :type 'integer
  :group 'disco)

(defcustom disco-company-member-search-retry-seconds 30
  "Seconds before the same guild member prefix may be requested again."
  :type 'number
  :group 'disco)

(defcustom disco-company-member-search-debounce-seconds 0.2
  "Idle delay before an automatic guild member prefix search is sent."
  :type 'number
  :group 'disco)

(defcustom disco-company-member-search-timeout-seconds 10
  "Seconds before an unanswered guild member search can be retried."
  :type 'number
  :group 'disco)

(defvar disco-company--rounded-avatar-cache (make-hash-table :test #'equal)
  "Cache of rounded completion avatar images keyed by file/size/mtime.")

(defun disco-company-reset-session-cache-state ()
  "Clear account-scoped completion avatar images without redrawing."
  (clrhash disco-company--rounded-avatar-cache))

(defvar disco-company--member-search-nonce-counter 0
  "Monotonic counter used to own guild member search responses.")

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--typing-users nil)

(defvar-local disco-company--member-search-requests nil
  "Hash table of member search keys to request metadata in this room.")

(defvar-local disco-company--pending-member-search nil
  "Explicit member search waiting to refresh completion in this room.")

(defvar-local disco-company--member-search-debounce-timer nil
  "Timer for the latest automatic member prefix search in this room.")

(defvar-local disco-company--member-search-debounce-query nil
  "Raw query owned by `disco-company--member-search-debounce-timer'.")

(defvar-local disco-company--gateway-handler nil
  "Buffer-owned Gateway handler for member completion responses.")

(defvar-local disco-company--gateway-handle nil
  "Appkit lifecycle handle owning the room completion Gateway hook.")

(defvar-local disco-company--owner-token nil
  "Opaque lifecycle token captured by room completion callbacks and timers.")

(declare-function disco-room--sync-draft-from-buffer "disco-room")

(defun disco-company--ensure-owner-token ()
  "Return the current room completion lifecycle token."
  (or disco-company--owner-token
      (setq-local disco-company--owner-token
                  (list 'disco-company-owner (current-buffer)))))

(defun disco-company--callback-active-p (buffer view owner-token)
  "Return non-nil when BUFFER still belongs to VIEW and OWNER-TOKEN.

Detached test/initialization buffers use nil VIEW, but become inactive as soon
as any Appkit view attaches, so nil can never degrade into replacement access."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq owner-token disco-company--owner-token)
              (if view
                  (and (appkit-view-live-p view)
                       (eq view (appkit-current-view))
                       (eq buffer (appkit-view-buffer view)))
                (null (appkit-current-view)))))))

(defun disco-company--install-gateway-handler (view owner-token)
  "Install a VIEW-owned completion Gateway hook for OWNER-TOKEN."
  (let ((buffer (current-buffer))
        handler
        handle)
    (setq handler
          (lambda (event)
            (when (disco-company--callback-active-p
                   buffer view owner-token)
              (with-current-buffer buffer
                (disco-company--handle-gateway-event event)))))
    (add-hook 'disco-gateway-event-hook handler)
    (setq handle
          (appkit-register-handle
           view 'function
           (lambda ()
             (remove-hook 'disco-gateway-event-hook handler)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq handler disco-company--gateway-handler)
                   (setq disco-company--gateway-handler nil))
                 (when (eq handle disco-company--gateway-handle)
                   (setq disco-company--gateway-handle nil))
                 (when (eq owner-token disco-company--owner-token)
                   (disco-company--clear-member-search-state)
                   (setq disco-company--owner-token nil)))))))
    (setq-local disco-company--gateway-handler handler
                disco-company--gateway-handle handle)))

(defun disco-company--as-list (value)
  "Return VALUE normalized as a proper list."
  (cond
   ((null value) nil)
   ((listp value) value)
   (t (list value))))

(defun disco-company-setup-room-buffer ()
  "Install CAPF/company completion hooks for current room buffer."
  (unless (hash-table-p disco-company--member-search-requests)
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal)))
  (let ((view (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p view)
           (functionp disco-company--gateway-handler)
           (appkit-handle-p disco-company--gateway-handle)
           (appkit-handle-alive-p disco-company--gateway-handle)
           (eq view (appkit-handle-owner disco-company--gateway-handle))))
     ((appkit-view-live-p view)
      (disco-company--teardown-room-buffer)
      (let ((owner-token (disco-company--ensure-owner-token)))
        (disco-company--install-gateway-handler view owner-token)))
     (t
      ;; Major-mode initialization precedes Appkit attachment.  The room setup
      ;; callback runs again after attach and installs the exact-view handler.
      (when (or disco-company--gateway-handler
                disco-company--gateway-handle)
        (disco-company--teardown-room-buffer))
      (disco-company--ensure-owner-token))))
  (add-hook 'kill-buffer-hook #'disco-company--teardown-room-buffer nil t)
  (let* ((existing-capf
          (disco-company--as-list completion-at-point-functions))
         (existing-dispatch
          (disco-company--as-list appkit-chat-completion-functions)))
    ;; Keep setup idempotent while preserving completion installed by other
    ;; room features.  The shared setup installs the ordinary CAPF adapter
    ;; immediately after Disco's company-first dispatcher.
    (setq-local completion-at-point-functions
                (cl-remove #'disco-room-complete-at-point
                           existing-capf
                           :test #'eq))
    (setq-local appkit-chat-completion-functions
                (cl-remove-if
                 (lambda (function)
                   (memq function
                         '(disco-company--complete-with-company
                           appkit-chat-completion-at-point)))
                 existing-dispatch))
    (appkit-chat-completion-setup
     :capf-functions '(disco-room-complete-at-point)
     :dispatch-functions '(disco-company--complete-with-company)
     :append t))
  (when (and disco-room-enable-company-backend
             (featurep 'company)
             (boundp 'company-backends))
    (let* ((existing-backends (disco-company--as-list company-backends))
           (filtered-backends (cl-remove 'disco-room-company-completion
                                         existing-backends
                                         :test #'equal)))
      (setq-local company-backends
                  (cons 'disco-room-company-completion filtered-backends)))))

(defun disco-company--teardown-room-buffer ()
  "Remove current room's global Gateway completion handler."
  (disco-company--clear-member-search-state)
  (let ((handler disco-company--gateway-handler)
        (handle disco-company--gateway-handle))
    (setq disco-company--gateway-handler nil
          disco-company--gateway-handle nil
          disco-company--owner-token nil)
    (when handler
      (remove-hook 'disco-gateway-event-hook handler))
    (when (and (appkit-handle-p handle)
               (appkit-handle-alive-p handle))
      (appkit-cancel-handle handle))))

(defun disco-company--normalize-id (value)
  "Return normalized snowflake-like ID string from VALUE, or nil."
  (disco-msg-normalize-id value))

(defun disco-company--normalize-list-sequence (value)
  "Normalize VALUE into a list, preserving list/vector elements."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun disco-company--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-company--thread-channel-p (&optional channel)
  "Return non-nil when CHANNEL (or current room channel) is a thread."
  (disco-thread-channel-p (or channel (disco-company--channel-object))))

(defun disco-company--sync-room-draft ()
  "Sync room draft state after completion insertion."
  (disco-room--sync-draft-from-buffer))

(defun disco-company--completion-token-bounds ()
  "Return completion metadata for `@', `#', or `:' token at point."
  (or (appkit-chat-completion-delimited-token-bounds ?:)
      (appkit-chat-completion-token-bounds '(?@ ?#))))

(defun disco-company-completion-token-at-point ()
  "Return unresolved room completion token at point, or nil.

This public query lets the room RET dispatcher guard against an active
`@'/`#'/`:' token without depending on a particular completion frontend."
  (disco-company--completion-token-bounds))

(defun disco-company--member-search-query (raw-query)
  "Normalize RAW-QUERY for a remote guild member search."
  (let ((query (string-trim (or raw-query ""))))
    (while (string-prefix-p "@" query)
      (setq query (substring query 1)))
    (string-trim query)))

(defun disco-company--member-search-nonce ()
  "Return a new nonce for a room-owned guild member search."
  (format "disco-company-%x-%x"
          (logand (sxhash (current-buffer)) #xfffffff)
          (cl-incf disco-company--member-search-nonce-counter)))

(defun disco-company--member-search-entry-active-p (entry now)
  "Return non-nil when request ENTRY remains reusable at time NOW."
  (and entry
       (< (- now (or (plist-get entry :time) 0))
          (max 1 disco-company-member-search-retry-seconds))))

(defun disco-company--cancel-member-search-timer (timer)
  "Cancel TIMER when it is a live timer object."
  (when (timerp timer)
    (cancel-timer timer)))

(defun disco-company--cancel-member-search-debounce ()
  "Cancel the current room's automatic member search debounce."
  (disco-company--cancel-member-search-timer
   disco-company--member-search-debounce-timer)
  (setq disco-company--member-search-debounce-timer nil
        disco-company--member-search-debounce-query nil))

(defun disco-company--clear-member-search-state ()
  "Cancel and forget all room-owned member search state."
  (disco-company--cancel-member-search-debounce)
  (when (hash-table-p disco-company--member-search-requests)
    (maphash
     (lambda (_key entry)
       (disco-company--cancel-member-search-timer
        (plist-get entry :timer)))
     disco-company--member-search-requests)
    (clrhash disco-company--member-search-requests))
  (setq disco-company--pending-member-search nil))

(defun disco-company--prune-member-search-requests (now)
  "Remove expired room member search entries at time NOW."
  (when (hash-table-p disco-company--member-search-requests)
    (let (expired)
      (maphash
       (lambda (key entry)
         (unless (disco-company--member-search-entry-active-p entry now)
           (push (cons key entry) expired)))
       disco-company--member-search-requests)
      (dolist (item expired)
        (disco-company--cancel-member-search-timer
         (plist-get (cdr item) :timer))
        (remhash (car item) disco-company--member-search-requests)))))

(defun disco-company--member-search-timeout
    (buffer view owner-token key nonce)
  "Expire BUFFER's VIEW-owned search for KEY when NONCE still owns it."
  (when (disco-company--callback-active-p buffer view owner-token)
    (with-current-buffer buffer
      (when (hash-table-p disco-company--member-search-requests)
        (let ((entry (gethash key disco-company--member-search-requests)))
          (when (and entry (equal nonce (plist-get entry :nonce)))
            (remhash key disco-company--member-search-requests)
            (when (and disco-company--pending-member-search
                       (equal nonce
                              (plist-get disco-company--pending-member-search
                                         :nonce)))
              (setq disco-company--pending-member-search nil))))))))

(defun disco-company--run-debounced-member-search
    (buffer view owner-token guild-id raw-query)
  "Run BUFFER's debounced RAW-QUERY when VIEW and GUILD-ID still own it."
  (when (disco-company--callback-active-p buffer view owner-token)
    (with-current-buffer buffer
      (when (equal raw-query disco-company--member-search-debounce-query)
        (let ((token (disco-company--completion-token-bounds)))
          (setq disco-company--member-search-debounce-timer nil
                disco-company--member-search-debounce-query nil)
          (when (and token
                     (eq (plist-get token :trigger) ?@)
                     (equal guild-id
                            (disco-company--normalize-id disco-room--guild-id))
                     (equal (downcase
                             (disco-company--member-search-query raw-query))
                            (downcase
                             (disco-company--member-search-query
                              (plist-get token :query)))))
            (disco-company--maybe-request-guild-members raw-query)))))))

(defun disco-company--schedule-member-search (raw-query)
  "Debounce an automatic guild member search for RAW-QUERY."
  (let ((guild-id (disco-company--normalize-id disco-room--guild-id))
        (query (disco-company--member-search-query raw-query)))
    (cond
     ((or (not guild-id) (string-empty-p query))
      (disco-company--cancel-member-search-debounce))
     ((and disco-company--member-search-debounce-timer
           (equal raw-query disco-company--member-search-debounce-query)))
     (t
      (disco-company--cancel-member-search-debounce)
      (setq disco-company--member-search-debounce-query raw-query)
      (setq disco-company--member-search-debounce-timer
            (run-at-time
             (max 0 disco-company-member-search-debounce-seconds)
             nil
             #'disco-company--run-debounced-member-search
             (current-buffer)
             (appkit-current-view)
             (disco-company--ensure-owner-token)
             guild-id raw-query))))))

(cl-defun disco-company--maybe-request-guild-members
    (raw-query &key explicit)
  "Request guild members matching RAW-QUERY when the local request is stale.

When EXPLICIT is non-nil, remember an in-flight request so its matching
Gateway chunk can advance the same room token's completion model."
  (when explicit
    (disco-company--cancel-member-search-debounce))
  (unless (hash-table-p disco-company--member-search-requests)
    (setq-local disco-company--member-search-requests
                (make-hash-table :test #'equal)))
  (let* ((guild-id (disco-company--normalize-id disco-room--guild-id))
         (query (disco-company--member-search-query raw-query))
         (query-key (downcase query))
         (key (and guild-id (cons guild-id query-key)))
         (now (float-time))
         (buffer (current-buffer))
         (view (appkit-current-view))
         (owner-token (disco-company--ensure-owner-token))
         entry)
    (disco-company--prune-member-search-requests now)
    (setq entry (and key
                     (gethash key disco-company--member-search-requests)))
    (when (and key
               (not (string-empty-p query))
               (fboundp 'disco-gateway-running-p)
               (disco-gateway-running-p)
               (fboundp 'disco-gateway-request-guild-members))
      (cond
       ((disco-company--member-search-entry-active-p entry now)
        (when (and explicit (eq (plist-get entry :status) 'in-flight))
          (setq disco-company--pending-member-search entry)))
       (t
        (let* ((nonce (disco-company--member-search-nonce))
               (request (list :guild-id guild-id
                              :query query
                              :key key
                              :nonce nonce
                              :view view
                              :owner-token owner-token
                              :status 'in-flight
                              :time now))
               sent
               timeout-timer)
          (puthash key request disco-company--member-search-requests)
          (when explicit
            (setq disco-company--pending-member-search request))
          (condition-case err
              (setq sent
                    (if (string-match-p "\\`[0-9]\\{15,20\\}\\'" query)
                        (disco-gateway-request-guild-members
                         guild-id :user-ids (list query) :nonce nonce)
                      (disco-gateway-request-guild-members
                       guild-id
                       :query query
                       :limit (max 1 (min 100 disco-company-member-search-limit))
                       :nonce nonce)))
            (error
             (when explicit
               (message "disco: member search failed: %s"
                        (error-message-string err)))))
          (unless sent
            (remhash key disco-company--member-search-requests)
            (when (equal nonce
                         (plist-get disco-company--pending-member-search :nonce))
              (setq disco-company--pending-member-search nil)))
          (when sent
            (setq timeout-timer
                  (run-at-time
                   (max 0.1 disco-company-member-search-timeout-seconds)
                   nil #'disco-company--member-search-timeout
                   buffer view owner-token key nonce))
            (setq request (plist-put request :timer timeout-timer))
            (puthash key request disco-company--member-search-requests)
            (when explicit
              (setq disco-company--pending-member-search request)))
          sent))))))

(defun disco-company--member-search-entry-by-nonce (nonce)
  "Return request entry owned by NONCE in the current room."
  (let (found)
    (when (and (stringp nonce)
               (hash-table-p disco-company--member-search-requests))
      (maphash
       (lambda (_key entry)
         (when (and (not found)
                    (equal nonce (plist-get entry :nonce)))
           (setq found entry)))
       disco-company--member-search-requests))
    found))

(defun disco-company--handle-gateway-event (event)
  "Handle member completion response EVENT for the current room."
  (pcase (plist-get event :type)
    ('ready
     (disco-company--clear-member-search-state))
    ('guild-members-chunk
     (let* ((nonce (plist-get event :nonce))
            (entry (disco-company--member-search-entry-by-nonce nonce))
            (event-guild-id
             (disco-company--normalize-id (plist-get event :guild-id))))
       (when (and entry
                  (equal event-guild-id (plist-get entry :guild-id))
                  (disco-company--callback-active-p
                   (current-buffer)
                   (plist-get entry :view)
                   (plist-get entry :owner-token)))
         (disco-company--cancel-member-search-timer
          (plist-get entry :timer))
         (setq entry (plist-put (copy-sequence entry) :status 'done))
         (setq entry (plist-put entry :timer nil))
         (setq entry (plist-put entry :time (float-time)))
         (puthash (plist-get entry :key)
                  entry
                  disco-company--member-search-requests)
         (when (and disco-company--pending-member-search
                    (equal nonce
                           (plist-get disco-company--pending-member-search
                                      :nonce)))
           (setq disco-company--pending-member-search nil))
         ;; Candidate tables read guild-member state lazily.  The gateway
         ;; callback only advances this request model; completion presentation
         ;; remains owned by the next explicit frontend action.
         entry)))))

(defun disco-company--completion-string-present-p (value)
  "Return non-nil when VALUE is a non-empty trimmed string."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun disco-company--completion-first-present (&rest values)
  "Return first non-empty trimmed string from VALUES, or nil."
  (seq-find #'disco-company--completion-string-present-p
            (mapcar (lambda (value)
                      (and (stringp value)
                           (string-trim value)))
                    values)))

(defun disco-company--completion-short-id (id)
  "Return short suffix label for snowflake-like ID."
  (let ((text (or (disco-company--normalize-id id) "????")))
    (if (> (length text) 4)
        (substring text (- (length text) 4))
      text)))

(defun disco-company--completion-disambiguate-label (base hint seen-labels)
  "Return unique completion label from BASE using HINT against SEEN-LABELS."
  (let* ((seed (or base ""))
         (suffix (if (disco-company--completion-string-present-p hint)
                     (format " (%s)" hint)
                   ""))
         (index 0)
         candidate)
    (setq candidate seed)
    (while (gethash candidate seen-labels)
      (setq index (1+ index))
      (setq candidate
            (if (= index 1)
                (format "%s%s" seed suffix)
              (format "%s%s#%d" seed suffix index))))
    (puthash candidate t seen-labels)
    candidate))

(defun disco-company--completion-make-candidate (label insert annotation sort-key &rest props)
  "Build one completion candidate plist.

PROPS is appended as additional plist metadata."
  (append (list :label label :insert insert :annotation annotation :sort-key sort-key)
          props))

(cl-defun disco-company--completion--merge-user
    (table user-id &key username global-name nick avatar display-name)
  "Merge one user record into TABLE by USER-ID."
  (let ((id (disco-company--normalize-id user-id)))
    (when id
      (let ((entry (copy-sequence (or (gethash id table)
                                      (list :id id)))))
        (dolist (field-value (list (cons :username username)
                                   (cons :global-name global-name)
                                   (cons :nick nick)
                                   (cons :avatar avatar)))
          (let* ((field (car field-value))
                 (raw-value (cdr field-value))
                 (value (and (stringp raw-value) (string-trim raw-value))))
            (when (disco-company--completion-string-present-p value)
              (setq entry (plist-put entry field value)))))
        (let ((display-rank (or (plist-get entry :display-rank) 0))
              (display (plist-get entry :display-name)))
          (dolist (choice `((40 . ,nick)
                            (30 . ,global-name)
                            (20 . ,display-name)
                            (10 . ,username)))
            (let* ((rank (car choice))
                   (raw-value (cdr choice))
                   (value (and (stringp raw-value) (string-trim raw-value))))
              (when (and (disco-company--completion-string-present-p value)
                         (> rank display-rank))
                (setq display-rank rank)
                (setq display value))))
          (setq entry (plist-put entry :display-rank display-rank))
          (when (disco-company--completion-string-present-p display)
            (setq entry (plist-put entry :display-name display))))
        (puthash id entry table)))))

(defun disco-company--completion-user-annotation-text (username user-id)
  "Return annotation text for a user completion row."
  (format " user %s | id:%s"
          (if (disco-company--completion-string-present-p username)
              (format "@%s" username)
            "@unknown")
          user-id))

(defun disco-company--completion-collect-user-name-map ()
  "Return hash table of user-id -> metadata plist for current room context."
  (let ((table (make-hash-table :test #'equal)))
    (let* ((channel (disco-company--channel-object))
           (recipients (and (listp channel)
                            (disco-company--normalize-list-sequence
                             (alist-get 'recipients channel)))))
      (dolist (recipient recipients)
        (when (listp recipient)
          (disco-company--completion--merge-user
           table
           (alist-get 'id recipient)
           :username (alist-get 'username recipient)
           :global-name (alist-get 'global_name recipient)
           :avatar (alist-get 'avatar recipient)))))
    (when (hash-table-p disco-room--typing-users)
      (maphash
       (lambda (user-id entry)
         (disco-company--completion--merge-user
          table
          user-id
          :display-name (and (listp entry)
                             (plist-get entry :display-name))))
       disco-room--typing-users))
    ;; Guild member chunks are the broadest member source available locally.
    ;; Their Discord shape is ((nick . ...) (user (id . ...) ...)).
    (when-let* ((guild-id
                 (disco-company--normalize-id disco-room--guild-id)))
      (dolist (member (disco-state-guild-members guild-id))
        (when (listp member)
          (let ((user (alist-get 'user member)))
            (disco-company--completion--merge-user
             table
             (or (and (listp user) (alist-get 'id user))
                 (alist-get 'user_id member))
             :username (and (listp user) (alist-get 'username user))
             :global-name (and (listp user) (alist-get 'global_name user))
             :nick (alist-get 'nick member)
             :avatar (and (listp user) (alist-get 'avatar user)))))))
    (dolist (msg (or (disco-state-messages disco-room--channel-id) '()))
      (let* ((author (alist-get 'author msg))
             (member (alist-get 'member msg)))
        (when (listp author)
          (disco-company--completion--merge-user
           table
           (alist-get 'id author)
           :username (alist-get 'username author)
           :global-name (alist-get 'global_name author)
           :nick (and (listp member) (alist-get 'nick member))
           :avatar (alist-get 'avatar author)))))
    (when (disco-company--thread-channel-p)
      (dolist (user-id (or (disco-state-thread-member-ids disco-room--channel-id) '()))
        (disco-company--completion--merge-user table user-id)))
    table))

(defun disco-company--completion-user-candidates ()
  "Return `@' user mention candidates for current room context."
  (let ((user-map (disco-company--completion-collect-user-name-map))
        (label-counts (make-hash-table :test #'equal))
        (seen-labels (make-hash-table :test #'equal))
        records
        out)
    (maphash
     (lambda (user-id entry)
       (let* ((display-name (or (and (listp entry) (plist-get entry :display-name))
                                (format "user-%s" (disco-company--completion-short-id user-id))))
               (base-label (format "@%s" display-name)))
          (puthash base-label
                   (1+ (gethash base-label label-counts 0))
                   label-counts)
          (push (list :user-id user-id
                      :entry entry
                      :display-name display-name
                      :base-label base-label)
                records)))
     user-map)
    (setq records
          (sort records
                (lambda (left right)
                  (let ((left-label (plist-get left :base-label))
                        (right-label (plist-get right :base-label)))
                    (if (equal left-label right-label)
                        (string-lessp (plist-get left :user-id)
                                      (plist-get right :user-id))
                      (string-lessp left-label right-label))))))
    (dolist (record records)
      (let* ((user-id (plist-get record :user-id))
             (entry (plist-get record :entry))
             (display-name (plist-get record :display-name))
             (base-label (plist-get record :base-label))
             (username (and (listp entry) (plist-get entry :username)))
             (global-name (and (listp entry) (plist-get entry :global-name)))
             (nick (and (listp entry) (plist-get entry :nick)))
             (avatar-hash (and (listp entry) (plist-get entry :avatar)))
             (collision-p (> (gethash base-label label-counts 0) 1))
             (label-seed
              (if collision-p
                  (format "%s (%s)" base-label
                          (disco-company--completion-short-id user-id))
                base-label))
             (label (disco-company--completion-disambiguate-label
                     label-seed user-id seen-labels)))
        (push (disco-company--completion-make-candidate
               label
               (format "<@%s>" user-id)
               (disco-company--completion-user-annotation-text username user-id)
               (downcase display-name)
               :kind 'user
               :user-id user-id
               :display-name display-name
               :username username
               :global-name global-name
               :nick nick
               :avatar-hash avatar-hash)
              out)))
    (sort out (lambda (left right)
                (let ((left-key (or (plist-get left :sort-key) ""))
                      (right-key (or (plist-get right :sort-key) "")))
                  (if (equal left-key right-key)
                      (string-lessp (or (plist-get left :user-id) "")
                                    (or (plist-get right :user-id) ""))
                    (string-lessp left-key right-key)))))))

(defun disco-company--completion-role-candidates ()
  "Return `@' role mention candidates from current guild state."
  (let* ((guild-id (disco-company--normalize-id disco-room--guild-id))
         (roles (and guild-id (disco-state-guild-roles guild-id)))
         (seen-labels (make-hash-table :test #'equal))
         out)
    (dolist (role roles)
      (let* ((role-id (disco-company--normalize-id (and (listp role) (alist-get 'id role))))
             (role-name (and (listp role) (alist-get 'name role)))
             (name (and (stringp role-name) (string-trim role-name))))
        (when (and role-id
                   (disco-company--completion-string-present-p name)
                   (not (equal role-id guild-id)))
          (let ((label (disco-company--completion-disambiguate-label
                        (format "@%s" name)
                        (disco-company--completion-short-id role-id)
                        seen-labels)))
            (push (disco-company--completion-make-candidate
                   label
                   (format "<@&%s>" role-id)
                   " role"
                   (downcase name)
                   :kind 'role
                   :role-id role-id)
                  out)))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-company--completion-special-candidates ()
  "Return special `@everyone' and `@here' candidates for guild channels."
  (when (disco-company--normalize-id disco-room--guild-id)
    (list (disco-company--completion-make-candidate
           "@everyone" "@everyone" " special" "@everyone"
           :kind 'special)
          (disco-company--completion-make-candidate
           "@here" "@here" " special" "@here"
           :kind 'special))))

(defun disco-company--completion-channel-type-label (channel)
  "Return human-readable channel type label for CHANNEL."
  (pcase (and (listp channel) (alist-get 'type channel))
    (0 " text")
    (2 " voice")
    (5 " announcement")
    ((or 10 11 12) " thread")
    (13 " stage")
    (14 " directory")
    (15 " forum")
    (16 " media")
    (17 " lobby")
    (_ " channel")))

(defun disco-company--completion-mentionable-channel-p (channel)
  "Return non-nil when CHANNEL should be offered for `#' completion."
  (let ((type (and (listp channel) (alist-get 'type channel))))
    (and (listp channel)
         (not (eq type 4))
         (disco-company--completion-string-present-p (alist-get 'name channel))
         (disco-company--normalize-id (alist-get 'id channel)))))

(defun disco-company--completion-channel-candidates ()
  "Return `#' channel mention candidates from current guild state."
  (let* ((guild-id (disco-company--normalize-id disco-room--guild-id))
         (channels (and guild-id
                        (disco-state-guild-channels guild-id)))
         (seen-ids (make-hash-table :test #'equal))
         (seen-labels (make-hash-table :test #'equal))
         out)
    (dolist (channel (or channels '()))
      (when (disco-company--completion-mentionable-channel-p channel)
        (let* ((channel-id (disco-company--normalize-id (alist-get 'id channel)))
               (name (string-trim (alist-get 'name channel))))
          (unless (gethash channel-id seen-ids)
            (puthash channel-id t seen-ids)
            (let ((label (disco-company--completion-disambiguate-label
                          (format "#%s" name)
                          (disco-company--completion-short-id channel-id)
                          seen-labels)))
              (push (disco-company--completion-make-candidate
                     label
                     (format "<#%s>" channel-id)
                     (disco-company--completion-channel-type-label channel)
                     (downcase name)
                     :kind 'channel
                     :channel-id channel-id)
                    out))))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-company--completion-emoji-available-p (emoji)
  "Return non-nil when custom EMOJI is available for insertion."
  (let ((available (and (listp emoji) (assq 'available emoji))))
    (or (null available)
        (disco-util-json-true-p (cdr available)))))

(defun disco-company--completion-emoji-candidates ()
  "Return `:' custom emoji candidates from current guild state."
  (let ((guild-id (disco-company--normalize-id disco-room--guild-id))
        (seen-labels (make-hash-table :test #'equal))
        out)
    (dolist (emoji (and guild-id (disco-state-guild-emojis guild-id)))
      (let* ((emoji-id (disco-company--normalize-id
                        (and (listp emoji) (alist-get 'id emoji))))
             (raw-name (and (listp emoji) (alist-get 'name emoji)))
             (name (and (stringp raw-name) (string-trim raw-name)))
             (animated (and (listp emoji)
                            (disco-util-json-true-p
                             (alist-get 'animated emoji)))))
        (when (and emoji-id
                   (disco-company--completion-string-present-p name)
                   (disco-company--completion-emoji-available-p emoji))
          (let ((label
                 (disco-company--completion-disambiguate-label
                  (format ":%s:" name)
                  (disco-company--completion-short-id emoji-id)
                  seen-labels)))
            (push (disco-company--completion-make-candidate
                   label
                   (format "<%s:%s:%s>" (if animated "a" "") name emoji-id)
                   (if animated " animated emoji" " emoji")
                   (downcase name)
                   :kind 'emoji
                   :emoji-id emoji-id
                   :emoji-name name
                   :animated animated)
                  out)))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-company--completion-unicode-emoji-candidates ()
  "Return protocol-neutral Unicode emoji candidates from Appkit."
  (mapcar
   (lambda (candidate)
     (let* ((value (appkit-chat-completion-candidate-value candidate))
            (name (plist-get value :name))
            (glyph (plist-get value :emoji)))
       (disco-company--completion-make-candidate
        (appkit-chat-completion-candidate-label candidate)
        (appkit-chat-completion-candidate-insert candidate)
        " unicode emoji"
        (downcase (or name ""))
        :kind 'unicode-emoji
        :emoji-name name
        :emoji-glyph glyph
        :prefix (appkit-chat-completion-candidate-prefix candidate))))
   (or (appkit-chat-emoji-candidates) '())))

(defun disco-company--completion-uniquify-candidates (candidates)
  "Return CANDIDATES with globally unique display labels."
  (let ((seen-labels (make-hash-table :test #'equal))
        out)
    (dolist (candidate candidates)
      (let* ((copy (copy-sequence candidate))
             (label (or (plist-get copy :label) ""))
             (hint (or (and (plist-get copy :user-id)
                            (disco-company--completion-short-id
                             (plist-get copy :user-id)))
                       (and (plist-get copy :role-id)
                            (disco-company--completion-short-id
                             (plist-get copy :role-id)))
                       (and (plist-get copy :channel-id)
                            (disco-company--completion-short-id
                             (plist-get copy :channel-id)))
                       (and (plist-get copy :emoji-id)
                            (disco-company--completion-short-id
                             (plist-get copy :emoji-id)))
                       (string-trim (or (plist-get copy :annotation) ""))))
             (unique (disco-company--completion-disambiguate-label
                      label
                      hint
                      seen-labels)))
        (setq copy (plist-put copy :label unique))
        (push copy out)))
    (nreverse out)))

(defun disco-company--completion-candidates-for-trigger (trigger)
  "Return completion candidates for TRIGGER in current room context."
  (disco-company--completion-uniquify-candidates
   (pcase trigger
     (?@ (append (or (disco-company--completion-special-candidates) '())
                 (disco-company--completion-role-candidates)
                 (disco-company--completion-user-candidates)))
     (?# (disco-company--completion-channel-candidates))
     (?: (append (disco-company--completion-emoji-candidates)
                 (disco-company--completion-unicode-emoji-candidates)))
     (_ nil))))

(defun disco-company--completion-filter-candidates (candidates prefix)
  "Return CANDIDATES matching PREFIX or an alias case-insensitively."
  (let* ((needle (downcase (or prefix "")))
         (without-trigger
          (replace-regexp-in-string "\\`[@#:/]+" "" needle))
         (needles (delete-dups (list needle without-trigger))))
    (seq-filter
     (lambda (candidate)
       (seq-some
        (lambda (value)
          (let ((haystack (downcase value)))
            (seq-some
             (lambda (term)
               (or (string-empty-p term)
                   (string-match-p (regexp-quote term) haystack)))
             needles)))
        (cons (or (plist-get candidate :label) "")
              (disco-company--completion-search-terms candidate))))
     candidates)))

(defun disco-company--completion-search-terms (candidate)
  "Return alternate searchable strings for Disco plist CANDIDATE."
  (seq-filter
   #'disco-company--completion-string-present-p
   (list (plist-get candidate :display-name)
         (plist-get candidate :nick)
         (plist-get candidate :global-name)
         (plist-get candidate :username)
         (plist-get candidate :user-id)
         (plist-get candidate :role-id)
         (plist-get candidate :channel-id)
         (plist-get candidate :emoji-name)
         (plist-get candidate :emoji-id)
         (plist-get candidate :emoji-glyph))))

(defun disco-company--completion-appkit-candidate (candidate)
  "Wrap Disco plist CANDIDATE for the shared completion layer.

The original protocol-specific plist remains available as the shared
candidate's opaque value."
  (appkit-chat-completion-candidate-create
   :label (plist-get candidate :label)
   :insert (plist-get candidate :insert)
   :search-terms (disco-company--completion-search-terms candidate)
   :prefix (plist-get candidate :prefix)
   :annotation
   (lambda (appkit-candidate)
     (disco-company--completion-capf-annotation
      (appkit-chat-completion-candidate-value appkit-candidate)))
   :value candidate))

(defun disco-company--completion-apply-candidate (completed candidate)
  "Replace COMPLETED with protocol insertion from plist CANDIDATE."
  (appkit-chat-completion-apply-candidate
   completed
   (disco-company--completion-appkit-candidate candidate)
   :suffix " "
   :sync-function #'disco-company--sync-room-draft))

(defun disco-company--completion-user-initials (candidate)
  "Return text avatar initials for user CANDIDATE."
  (let* ((name (or (plist-get candidate :display-name)
                   (plist-get candidate :username)
                   "?"))
         (parts (split-string (or name "") "[^[:alnum:]]+" t))
         (first (if parts (substring (car parts) 0 1) "?"))
         (second (if (> (length parts) 1)
                     (substring (cadr parts) 0 1)
                   "")))
    (upcase (concat first second))))

(defun disco-company--completion-avatar-size ()
  "Return normalized avatar size for completion annotations.

`auto' mode follows current line pixel height, so avatar size tracks text-scale."
  (let* ((line-height (or (and (fboundp 'line-pixel-height)
                               (ignore-errors (line-pixel-height)))
                          (frame-char-height)
                          16))
         (auto-size (max 8 (- line-height 2))))
    (pcase disco-company-capf-avatar-size
      ('auto auto-size)
      ((pred integerp) (max 8 disco-company-capf-avatar-size))
      (_ auto-size))))

(defun disco-company--completion-image-with-size (image pixel-size)
  "Return IMAGE spec resized to PIXEL-SIZE, or IMAGE when not applicable."
  (if (and (consp image)
           (eq (car image) 'image)
           (integerp pixel-size)
           (> pixel-size 0))
      (let* ((type (car image))
             (props (copy-sequence (cdr image))))
        (setq props (plist-put props :width pixel-size))
        (setq props (plist-put props :height pixel-size))
        (setq props (plist-put props :ascent 'center))
        (cons type props))
    image))

(defun disco-company--completion-avatar-message (candidate)
  "Build minimal fake room message carrying avatar fields for CANDIDATE."
  (let ((user-id (plist-get candidate :user-id))
        (avatar-hash (plist-get candidate :avatar-hash)))
    (when user-id
      (let ((author `((id . ,user-id))))
        (when (disco-company--completion-string-present-p avatar-hash)
          (setq author (append author `((avatar . ,avatar-hash)))))
        `((author . ,author))))))

(defun disco-company--completion-avatar-cache-file (avatar-msg)
  "Return cached avatar file path for AVATAR-MSG, or nil."
  (when (and avatar-msg
             (fboundp 'disco-room--avatar-cache-key)
             (fboundp 'disco-room--avatar-cache-existing-file))
    (let ((cache-key (ignore-errors
                       (funcall #'disco-room--avatar-cache-key avatar-msg))))
      (when cache-key
        (ignore-errors
          (funcall #'disco-room--avatar-cache-existing-file cache-key))))))

(defun disco-company--completion-avatar-mime-type (file)
  "Return MIME type string for avatar FILE extension, or nil."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (pcase ext
      ("png" "image/png")
      ((or "jpg" "jpeg") "image/jpeg")
      ("gif" "image/gif")
      ("webp" "image/webp")
      (_ nil))))

(defun disco-company--completion-rounded-avatar-image (file pixel-size)
  "Return rounded SVG avatar image from FILE at PIXEL-SIZE, or nil."
  (when (and (stringp file)
             (file-readable-p file)
             (integerp pixel-size)
             (> pixel-size 0)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-circle)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (let* ((attrs (file-attributes file))
           (mtime (and attrs (file-attribute-modification-time attrs)))
           (cache-key (format "%s:%s:%s" file pixel-size mtime))
           (cached (gethash cache-key disco-company--rounded-avatar-cache)))
      (or cached
          (let* ((mime (disco-company--completion-avatar-mime-type file))
                 (svg (and mime (svg-create pixel-size pixel-size))))
            (when (and mime svg)
              (let* ((radius (/ (float pixel-size) 2.0))
                     (clip (svg-clip-path svg :id "clip")))
                (svg-circle clip radius radius radius)
                (svg-embed svg
                           file
                           mime
                           nil
                           :x 0
                           :y 0
                           :width pixel-size
                           :height pixel-size
                           :clip-path "url(#clip)")
                (let ((image (svg-image svg
                                        :ascent 'center
                                        :width pixel-size
                                        :height pixel-size)))
                  (puthash cache-key image disco-company--rounded-avatar-cache)
                  image))))))))

(defun disco-company--completion-user-avatar-image (candidate &optional pixel-size)
  "Return avatar image object for user CANDIDATE when available.

When PIXEL-SIZE is non-nil, resize image to that size."
  (when (and disco-company-show-user-avatars
             (boundp 'disco-room-show-avatar-images)
             disco-room-show-avatar-images
             (fboundp 'disco-room--avatar-image))
    (let* ((avatar-msg (disco-company--completion-avatar-message candidate))
           ;; Trigger room avatar cache/fetch path first.
           (raw-image (and avatar-msg
                           (ignore-errors
                             (funcall #'disco-room--avatar-image avatar-msg))))
           (cache-file (disco-company--completion-avatar-cache-file avatar-msg))
           (rounded-image (and cache-file pixel-size
                               (disco-company--completion-rounded-avatar-image
                                cache-file
                                pixel-size))))
      (or rounded-image
          (if pixel-size
              (disco-company--completion-image-with-size raw-image pixel-size)
            raw-image)))))

(defun disco-company--completion-user-annotation (candidate)
  "Return user annotation string for CANDIDATE."
  (let* ((username (plist-get candidate :username))
         (user-id (plist-get candidate :user-id))
         (image (disco-company--completion-user-avatar-image
                 candidate
                 (disco-company--completion-avatar-size)))
         (icon (if image
                   (propertize " " 'display image)
                 (format "[%s]" (disco-company--completion-user-initials candidate)))))
    (format "  %s %s | id:%s"
            icon
            (if (disco-company--completion-string-present-p username)
                (format "@%s" username)
              "@unknown")
            (or user-id "?"))))

(defun disco-company--completion-company-annotation (candidate)
  "Return company annotation string for CANDIDATE plist."
  (if (eq (plist-get candidate :kind) 'user)
      (disco-company--completion-user-annotation candidate)
    (or (plist-get candidate :annotation) "")))

(defun disco-company--completion-capf-annotation (candidate)
  "Return CAPF/Corfu annotation string for CANDIDATE plist."
  (if (eq (plist-get candidate :kind) 'user)
      (disco-company--completion-user-annotation candidate)
    (or (plist-get candidate :annotation) "")))

(defun disco-company--completion-company-meta (candidate)
  "Return company metadata string for CANDIDATE plist."
  (if (eq (plist-get candidate :kind) 'user)
      (format "display:%s username:%s id:%s"
              (or (plist-get candidate :display-name) "")
              (or (plist-get candidate :username) "")
              (or (plist-get candidate :user-id) ""))
    (or (plist-get candidate :label) "")))

(defun disco-room-complete-at-point ()
  "Return completion data for `@', `#', and `:' tokens at point.

This function is suitable for `completion-at-point-functions'."
  (let ((token (disco-company--completion-token-bounds)))
    (when token
      (let* ((trigger (plist-get token :trigger))
             (start (plist-get token :start))
             (end (plist-get token :end))
             (candidates
              (mapcar
               #'disco-company--completion-appkit-candidate
               (disco-company--completion-candidates-for-trigger trigger))))
        (when (eq trigger ?@)
          (disco-company--schedule-member-search
           (plist-get token :query)))
        (appkit-chat-completion-capf
         start end candidates
         :suffix " "
         :sync-function #'disco-company--sync-room-draft)))))

(defun disco-company--company-prefix ()
  "Return current completion token for company backend, or nil."
  (let ((token (disco-company--completion-token-bounds)))
    (when token
      (plist-get token :raw))))

(defun disco-company--company-candidates (prefix)
  "Return company candidate strings for PREFIX."
  (when (and (stringp prefix) (> (length prefix) 0))
    (let* ((trigger (aref prefix 0))
           (matches (disco-company--completion-filter-candidates
                     (disco-company--completion-candidates-for-trigger trigger)
                     prefix)))
      (when (eq trigger ?@)
        (disco-company--schedule-member-search prefix))
      (mapcar (lambda (candidate)
                (propertize (plist-get candidate :label)
                            'disco-room-completion-candidate candidate
                            'disco-room-completion-insert
                            (plist-get candidate :insert)
                            'disco-room-completion-annotation
                            (plist-get candidate :annotation)))
              matches))))

(defun disco-room-company-completion (command &optional arg &rest _ignored)
  "Company backend for room `@', `#', and `:' token completion."
  (interactive (list 'interactive))
  (pcase command
    ('interactive
     (when (fboundp 'company-begin-backend)
       (funcall 'company-begin-backend 'disco-room-company-completion)))
    ('prefix
     (when (and (derived-mode-p 'disco-room-mode)
                (appkit-chatbuf-point-in-input-p))
       (disco-company--company-prefix)))
    ('sorted t)
    ('require-match 'never)
    ('candidates
     (disco-company--company-candidates arg))
    ('annotation
     (let ((candidate (and (stringp arg)
                           (get-text-property 0 'disco-room-completion-candidate arg))))
       (if candidate
           (disco-company--completion-company-annotation candidate)
         (or (and (stringp arg)
                  (get-text-property 0 'disco-room-completion-annotation arg))
             ""))))
    ('meta
     (let ((candidate (and (stringp arg)
                           (get-text-property 0 'disco-room-completion-candidate arg))))
       (if candidate
           (disco-company--completion-company-meta candidate)
         (or arg ""))))
    ('post-completion
     (let ((candidate (and (stringp arg)
                           (get-text-property
                            0 'disco-room-completion-candidate arg))))
       (when candidate
         (disco-company--completion-apply-candidate arg candidate))))))

(defun disco-company--complete-with-capf ()
  "Try CAPF completion for mention/channel token at point."
  (appkit-chat-completion-at-point))

(defun disco-company--complete-with-company ()
  "Try company backend completion for mention/channel token at point."
  (when (and (bound-and-true-p company-mode)
             (fboundp 'company-begin-backend)
             (fboundp 'company-complete)
             (disco-company--completion-token-bounds))
    (funcall 'company-begin-backend 'disco-room-company-completion)
    (funcall 'company-complete)))

(defun disco-room-complete-mention ()
  "Complete `@' mention, `#' channel, or `:' custom emoji at point.

This compatibility command runs the shared company-first completion order."
  (interactive)
  (let ((token (disco-company--completion-token-bounds)))
    (if (not token)
        (message "disco: point is not on a mention, channel, or emoji token")
      (when (eq (plist-get token :trigger) ?@)
        (disco-company--maybe-request-guild-members
         (plist-get token :query)
         :explicit t))
      (or (appkit-chat-completion-complete)
          (unless disco-company--pending-member-search
            (message "disco: no completion candidates for %s"
                     (plist-get token :raw)))))))

(provide 'disco-company)

;;; disco-company.el ends here
