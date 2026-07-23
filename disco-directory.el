;;; disco-directory.el --- Lazy Discord guild directory owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns REST request lifecycles for the guild/DM index, per-guild channels, and
;; parent-scoped thread pages.  Surface controllers start requests; projectors
;; only consume state and directory events.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-permission)
(require 'disco-preview)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-util)

(defvar disco-directory-event-hook nil
  "Hook run with one directory lifecycle event plist.")

(defvar disco-directory--index-generation 0)
(defvar disco-directory--index-request nil
  "Current index request ownership record, or nil.")
(defvar disco-directory--guild-generation (make-hash-table :test #'equal))
(defvar disco-directory--guild-status (make-hash-table :test #'equal))
(defvar disco-directory--request-epoch 0
  "Monotonic owner of all outstanding directory requests.")
(defvar disco-directory--gateway-session-generation
  (disco-gateway-session-generation)
  "Gateway session generation owning current directory request state.")

(defvar disco-directory--parent-thread-request-sequence 0
  "Monotonic request token for parent-scoped active-thread searches.")

(defvar disco-directory--parent-thread-state (make-hash-table :test #'equal)
  "Hash table parent-channel-id -> active-thread lifecycle state.")

(defconst disco-directory--parent-thread-page-limit 25
  "Maximum page size accepted by Discord's thread-search endpoint.")

(defconst disco-directory--parent-thread-index-retries 3
  "Number of delayed retries for an unavailable Discord search index.")

(defun disco-directory--normalize-id (value)
  "Return VALUE as a string ID, or nil."
  (and value (format "%s" value)))

(defun disco-directory--settle-index-request (request status errors)
  "Settle REQUEST once with STATUS and endpoint ERRORS.

STATUS is one of `completed', `superseded', or `cancelled'.  The completion
callback receives the same explicit result shape for every terminal state."
  (unless (plist-get request :settled)
    (setf (plist-get request :settled) t)
    (when (eq request disco-directory--index-request)
      (setq disco-directory--index-request nil))
    (let ((result
           (list :status status
                 :generation (plist-get request :generation)
                 :session-generation
                 (plist-get request :session-generation)
                 :errors (nreverse errors))))
      (disco-directory--emit
       (pcase status
         ('completed 'index-loaded)
         ('superseded 'index-superseded)
         ('cancelled 'index-cancelled))
       :generation (plist-get request :generation)
       :status status
       :errors (plist-get result :errors)
       :result result)
      (when-let* ((on-complete (plist-get request :on-complete)))
        (funcall on-complete result)))))

(defun disco-directory--retire-requests ()
  "Cancel every outstanding request and clear its lifecycle state."
  (let ((index-request disco-directory--index-request))
    ;; Invalidate and detach first: a completion callback may synchronously
    ;; start a fresh request, which must belong to the post-reset epoch.
    (setq disco-directory--index-request nil)
    (cl-incf disco-directory--request-epoch)
    (cl-incf disco-directory--index-generation)
    (clrhash disco-directory--guild-generation)
    (clrhash disco-directory--guild-status)
    (clrhash disco-directory--parent-thread-state)
    (when index-request
      (disco-directory--settle-index-request
       index-request 'cancelled nil))))

(defun disco-directory-reset ()
  "Reset directory request ownership state."
  (setq disco-directory--gateway-session-generation
        (disco-gateway-session-generation))
  (disco-directory--retire-requests))

(defun disco-directory--sync-gateway-session ()
  "Retire requests that belong to a superseded Gateway READY session."
  (let ((generation (disco-gateway-session-generation)))
    (unless (= generation disco-directory--gateway-session-generation)
      (setq disco-directory--gateway-session-generation generation)
      (disco-directory--retire-requests))))

(defun disco-directory--emit (type &rest properties)
  "Emit directory event TYPE with PROPERTIES."
  (run-hook-with-args 'disco-directory-event-hook
                      (append (list :type type) properties)))

(defun disco-directory-guild-status (guild-id)
  "Return request status for GUILD-ID.

The result is `loading', `error', `loaded', or `unloaded'."
  (disco-directory--sync-gateway-session)
  (let ((request-status
         (gethash (format "%s" guild-id) disco-directory--guild-status)))
    (cond
     ((eq request-status 'loading) 'loading)
     ((disco-state-guild-channels-loaded-p guild-id) 'loaded)
     ((eq request-status 'loaded) 'unloaded)
     (request-status request-status)
     (t 'unloaded))))

(defun disco-directory--guild-channel-snapshot-resolved-p (channels)
  "Return non-nil when every guild channel in CHANNELS has permissions."
  (cl-every #'disco-permission-channel-known-p channels))

(defun disco-directory--unresolved-channel-snapshot-error (guild-id channels)
  "Return protocol error data for unresolved GUILD-ID CHANNELS."
  (let ((unresolved
         (delq nil
               (mapcar
                (lambda (channel)
                  (unless (disco-permission-channel-known-p channel)
                    (and (alist-get 'id channel)
                         (format "%s" (alist-get 'id channel)))))
                channels))))
    (list :kind 'unresolved-channel-permissions
          :guild-id guild-id
          :channel-ids unresolved
          :message
          (format "Discord omitted computed permissions for %d channel%s"
                  (length unresolved)
                  (if (= (length unresolved) 1) "" "s")))))

(defun disco-directory-parent-threads-state (parent-channel-id)
  "Return active-thread lifecycle state for PARENT-CHANNEL-ID.

The committed `:thread-ids' and `:next-cursor' remain present while a refresh
or append request is loading or has failed.  No table entry means `unloaded'."
  (disco-directory--sync-gateway-session)
  (or (gethash (format "%s" parent-channel-id)
               disco-directory--parent-thread-state)
      '(:status unloaded)))

(defun disco-directory-parent-threads (parent-channel-id)
  "Return PARENT-CHANNEL-ID's ordered active-thread snapshot.

Only IDs in the directory request snapshot are considered.  In particular,
stale threads in the global parent index cannot turn an authoritative empty
snapshot into a non-empty one."
  (delq nil
        (mapcar #'disco-state-channel
                (plist-get
                 (disco-directory-parent-threads-state parent-channel-id)
                 :thread-ids))))

(defun disco-directory-parent-thread-viewable-p
    (parent-channel-id thread-or-id)
  "Return non-nil when THREAD-OR-ID is authorized by PARENT-CHANNEL-ID's page.

Endpoint inclusion is scoped to the committed parent snapshot.  It supplies
the unknown-thread decision only; canonical Gateway hidden evidence and an
inaccessible or mismatched parent still deny the thread."
  (disco-directory--sync-gateway-session)
  (let* ((parent-id (disco-directory--normalize-id parent-channel-id))
         (thread-id
          (disco-directory--normalize-id
           (if (listp thread-or-id)
               (alist-get 'id thread-or-id)
             thread-or-id)))
         (state
          (and parent-id
               (gethash parent-id disco-directory--parent-thread-state)))
         (thread (and thread-id (disco-state-channel thread-id)))
         (parent (and parent-id (disco-state-channel parent-id))))
    (and parent-id thread-id
         (member thread-id (plist-get state :thread-ids))
         thread parent
         (disco-state-channel-thread-p thread)
         (equal parent-id
                (disco-directory--normalize-id
                 (alist-get 'parent_id thread)))
         (equal (disco-directory--normalize-id (alist-get 'guild_id parent))
                (disco-directory--normalize-id (alist-get 'guild_id thread)))
         (disco-state-channel-viewable-p parent nil)
         (disco-state-channel-viewable-p thread t))))

(defun disco-directory--parent-thread-request-state (parent-id token)
  "Return TOKEN's current, valid request state for PARENT-ID.

If TOKEN still owns the request but its guild or parent context changed, drop
the state.  Gateway and index events already own the corresponding view
reprojection; the stale REST response must only stop being `loading'."
  (disco-directory--sync-gateway-session)
  (let* ((state (gethash parent-id disco-directory--parent-thread-state))
         (guild-id (plist-get state :guild-id))
         (parent (disco-state-channel parent-id))
         (current-p
          (and (eq (plist-get state :status) 'loading)
               (= (or (plist-get state :token) -1) token))))
    (when current-p
      (if (and parent
               (disco-channel-thread-parent-p parent)
               (equal (disco-directory--normalize-id
                       (alist-get 'guild_id parent))
                      guild-id)
               (disco-directory--guild-known-p guild-id)
               (disco-state-channel-viewable-p parent nil))
          state
        (remhash parent-id disco-directory--parent-thread-state)
        nil))))

(defun disco-directory--emit-parent-thread-state (parent-id event-type)
  "Emit EVENT-TYPE for PARENT-ID's current directory lifecycle."
  (let ((state (disco-directory-parent-threads-state parent-id)))
    (disco-directory--emit
     event-type
     :guild-id (plist-get state :guild-id)
     :parent-id parent-id)))

(defun disco-directory--merge-thread-ids (existing new-threads)
  "Append NEW-THREADS' IDs to EXISTING while preserving response order."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (thread-id existing)
      (setq thread-id (format "%s" thread-id))
      (unless (gethash thread-id seen)
        (puthash thread-id t seen)
        (push thread-id merged)))
    (dolist (thread new-threads)
      (when-let* ((thread-id (alist-get 'id thread)))
        (setq thread-id (format "%s" thread-id))
        (unless (gethash thread-id seen)
          (puthash thread-id t seen)
          (push thread-id merged))))
    (nreverse merged)))

(defun disco-directory--canonical-thread-search-page
    (parent-id guild-id result)
  "Validate and canonicalize RESULT for PARENT-ID in GUILD-ID.

Return a plist containing `:threads', `:first-messages', `:has-more', and
`:total'.  No state is mutated.  A cross-parent, cross-guild, malformed, or
non-thread entity is a protocol error rather than permission evidence."
  (unless (and (listp result) (assq 'threads result))
    (error "Disco: thread-search response has no thread collection"))
  (let ((raw-threads (alist-get 'threads result))
        (raw-messages (or (alist-get 'first_messages result) '()))
        (total (alist-get 'total_results result))
        (thread-id-set (make-hash-table :test #'equal))
        threads messages)
    (unless (listp raw-threads)
      (error "Disco: thread-search thread collection is malformed"))
    (unless (listp raw-messages)
      (error "Disco: thread-search starter collection is malformed"))
    (when (and total (not (and (integerp total) (>= total 0))))
      (error "Disco: thread-search total is malformed"))
    (dolist (thread raw-threads)
      (let* ((thread-id
              (disco-directory--normalize-id
               (and (listp thread) (alist-get 'id thread))))
             (thread-parent-id
              (disco-directory--normalize-id
               (and (listp thread) (alist-get 'parent_id thread))))
             (thread-guild-id
              (disco-directory--normalize-id
               (and (listp thread) (alist-get 'guild_id thread)))))
        (unless (and thread-id
                     (equal parent-id thread-parent-id)
                     (or (null thread-guild-id)
                         (equal guild-id thread-guild-id))
                     (disco-state-channel-thread-p thread))
          (error "Disco: thread-search returned an invalid thread identity"))
        (unless (gethash thread-id thread-id-set)
          (puthash thread-id t thread-id-set)
          (let ((canonical (copy-tree thread)))
            (setf (alist-get 'guild_id canonical) guild-id)
            (push canonical threads)))))
    (dolist (message raw-messages)
      (let* ((message-id
              (disco-directory--normalize-id
               (and (listp message) (alist-get 'id message))))
             (channel-id
              (disco-directory--normalize-id
               (and (listp message) (alist-get 'channel_id message))))
             (message-guild-id
              (disco-directory--normalize-id
               (and (listp message) (alist-get 'guild_id message)))))
        (unless (and message-id channel-id (gethash channel-id thread-id-set)
                     (or (null message-guild-id)
                         (equal guild-id message-guild-id)))
          (error "Disco: thread-search returned an invalid starter identity"))
        (push (copy-tree message) messages)))
    (list :threads (nreverse threads)
          :first-messages (nreverse messages)
          :has-more (and (disco-util-json-true-p
                          (alist-get 'has_more result))
                         t)
          :total total)))

(defun disco-directory--ingest-thread-search-page (threads first-messages)
  "Store one fully validated page of THREADS and FIRST-MESSAGES."
  (dolist (thread threads)
    (disco-state-upsert-channel thread))
  (dolist (message first-messages)
    (disco-state-upsert-message (alist-get 'channel_id message) message)))

(defun disco-directory--thread-search-retry-delay (result)
  "Return protocol retry delay in seconds for index-pending RESULT."
  (let ((value (alist-get 'retry_after result)))
    (cond
     ((numberp value) (max 0.1 value))
     ((and (stringp value)
           (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
      (max 0.1 (string-to-number value)))
     (t 1.0))))

(defun disco-directory--thread-search-index-pending-p (result)
  "Return non-nil when RESULT reports an unavailable search index."
  (and (listp result)
       (= (or (alist-get 'code result) 0) 110000)))

(defun disco-directory--fail-parent-thread-request (parent-id token error-value)
  "Finish TOKEN for PARENT-ID with ERROR-VALUE without losing its snapshot."
  (when-let* ((state
               (disco-directory--parent-thread-request-state parent-id token)))
    (setq state (copy-sequence state))
    (setf (plist-get state :status) 'error
          (plist-get state :phase) nil
          (plist-get state :token) nil
          (plist-get state :error) error-value)
    (puthash parent-id state disco-directory--parent-thread-state)
    (disco-directory--emit-parent-thread-state
     parent-id 'parent-threads-error)))

(defun disco-directory--load-parent-thread-page
    (parent-id guild-id token cursor index-retries)
  "Load one active-thread page for PARENT-ID owned by TOKEN.

GUILD-ID owns the parent.  CURSOR is the stable creation-time cursor and
INDEX-RETRIES counts delayed index retries already attempted for this page.
Successful pages are committed independently; this function never drains the
next page automatically."
  (disco-api-channel-search-threads-async
   parent-id
   :archived :false
   :sort-by 'creation-time
   :sort-order 'desc
   :limit disco-directory--parent-thread-page-limit
   :max-id cursor
   :on-success
   (lambda (result)
     (when-let* ((state
                  (disco-directory--parent-thread-request-state
                   parent-id token)))
       (if (disco-directory--thread-search-index-pending-p result)
           (if (< index-retries disco-directory--parent-thread-index-retries)
               (let* ((delay
                       (disco-directory--thread-search-retry-delay result))
                      (next-state (copy-sequence state)))
                 (setf (plist-get next-state :phase) 'indexing)
                 (puthash parent-id next-state
                          disco-directory--parent-thread-state)
                 (disco-directory--emit-parent-thread-state
                  parent-id 'parent-threads-loading)
                 (run-at-time
                  delay nil
                  (lambda ()
                    (when (disco-directory--parent-thread-request-state
                           parent-id token)
                      (disco-directory--load-parent-thread-page
                       parent-id guild-id token cursor (1+ index-retries))))))
             (disco-directory--fail-parent-thread-request
              parent-id token
              (list :status 202
                    :body result
                    :message (or (alist-get 'message result)
                                 "Thread search index is not available"))))
         (condition-case err
             (let* ((mode (plist-get state :mode))
                    (validated
                     (disco-directory--canonical-thread-search-page
                      parent-id guild-id result))
                    (page (plist-get validated :threads))
                    (first-messages
                     (plist-get validated :first-messages))
                    (base-ids
                     (and (eq mode 'append)
                          (plist-get state :thread-ids)))
                    (merged-ids
                     (disco-directory--merge-thread-ids base-ids page))
                    (next-total
                     (if (eq mode 'append)
                         (or (plist-get state :total)
                             (plist-get validated :total))
                       (plist-get validated :total)))
                    (has-more (plist-get validated :has-more))
                    (next-cursor
                     (disco-directory--normalize-id
                      (and page (alist-get 'id (car (last page)))))))
               (if (and has-more
                        (or (null next-cursor)
                            (and cursor
                                 (not (disco-state-snowflake<
                                       next-cursor cursor)))))
                   (disco-directory--fail-parent-thread-request
                    parent-id token
                    '(:kind invalid-thread-search-cursor
                      :message
                      "Thread search returned no next creation-time cursor"))
                 ;; Validation above covers the entire page before either the
                 ;; identity index or message cache is changed.
                 (disco-directory--ingest-thread-search-page
                  page first-messages)
                 ;; Commit only after the complete page has passed identity
                 ;; and cursor validation and has been ingested.  A replace
                 ;; request therefore leaves the previous snapshot visible
                 ;; until this point.
                 (puthash
                  parent-id
                  (list :guild-id guild-id
                        :status 'loaded
                        :thread-ids merged-ids
                        :next-cursor (and has-more next-cursor)
                        :total next-total)
                  disco-directory--parent-thread-state)
                 (unless (disco-channel-thread-only-parent-p
                          (disco-state-channel parent-id))
                   (disco-preview-request-thread-page guild-id page))
                 (disco-directory--emit-parent-thread-state
                  parent-id 'parent-threads-loaded)))
           (error
            (disco-directory--fail-parent-thread-request
             parent-id token
             (list :kind 'invalid-thread-search-response
                   :message (error-message-string err))))))))
   :on-error
   (lambda (error-value)
     (disco-directory--fail-parent-thread-request
      parent-id token error-value))))

(defun disco-directory--validate-parent-thread-load (parent-id)
  "Return PARENT-ID's guild ID or signal why it cannot be searched."
  (let* ((parent (disco-state-channel parent-id))
         (guild-id (and parent (alist-get 'guild_id parent))))
    (unless parent
      (error "Disco: cannot load threads for unknown parent %s" parent-id))
    (unless (disco-channel-thread-parent-p parent)
      (error "Disco: channel %s cannot contain threads" parent-id))
    (unless guild-id
      (error "Disco: parent channel %s has no guild context" parent-id))
    (setq guild-id (format "%s" guild-id))
    (unless (disco-directory--guild-known-p guild-id)
      (error "Disco: cannot load threads for departed guild %s" guild-id))
    (unless (disco-state-channel-viewable-p parent nil)
      (error "Disco: cannot load threads for inaccessible parent %s" parent-id))
    guild-id))

(defun disco-directory--start-parent-thread-page
    (parent-id guild-id cursor mode)
  "Start one PARENT-ID page in GUILD-ID at CURSOR.

MODE is `replace' or `append'.  Committed snapshot fields remain in the same
state plist until a successful response replaces them."
  (unless (memq mode '(replace append))
    (error "Disco: invalid parent-thread request mode %S" mode))
  (let* ((token (cl-incf disco-directory--parent-thread-request-sequence))
         (state
          (copy-sequence
           (or (gethash parent-id disco-directory--parent-thread-state)
               (list :guild-id guild-id)))))
    (setf (plist-get state :status) 'loading
          (plist-get state :guild-id) guild-id
          (plist-get state :token) token
          (plist-get state :mode) mode
          (plist-get state :phase) nil
          (plist-get state :error) nil)
    (puthash parent-id state disco-directory--parent-thread-state)
    (disco-directory--emit-parent-thread-state
     parent-id 'parent-threads-loading)
    (disco-directory--load-parent-thread-page
     parent-id guild-id token cursor 0)
    'loading))

(cl-defun disco-directory-load-parent-threads-async
    (parent-channel-id &key force)
  "Load the first active-thread page under PARENT-CHANNEL-ID asynchronously.

Concurrent loads are coalesced.  FORCE supersedes an in-flight request and
refreshes from the first page while retaining the committed snapshot until the
replacement succeeds.  Use
`disco-directory-load-more-parent-threads-async' for subsequent pages and
`disco-directory-retry-parent-threads-async' after an error."
  (disco-directory--sync-gateway-session)
  (let* ((parent-id (format "%s" parent-channel-id))
         (guild-id (disco-directory--validate-parent-thread-load parent-id))
         (state (disco-directory-parent-threads-state parent-id))
         (status (plist-get state :status)))
    (cond
     ((and (eq status 'loading) (not force))
      'loading)
     ((and (eq status 'loaded) (not force))
      'loaded)
     ((and (eq status 'error) (not force))
      'error)
     (t
      (disco-directory--start-parent-thread-page
       parent-id guild-id nil 'replace)))))

(defun disco-directory-load-more-parent-threads-async (parent-channel-id)
  "Load PARENT-CHANNEL-ID's next active-thread page explicitly."
  (disco-directory--sync-gateway-session)
  (let* ((parent-id (format "%s" parent-channel-id))
         (guild-id (disco-directory--validate-parent-thread-load parent-id))
         (state (disco-directory-parent-threads-state parent-id))
         (status (plist-get state :status))
         (cursor (plist-get state :next-cursor)))
    (cond
     ((eq status 'loading) 'loading)
     ((eq status 'error) 'error)
     ((not (eq status 'loaded)) 'unloaded)
     ((null cursor) 'loaded)
     (t
      (disco-directory--start-parent-thread-page
       parent-id guild-id cursor 'append)))))

(defun disco-directory-retry-parent-threads-async (parent-channel-id)
  "Retry PARENT-CHANNEL-ID's failed page from its exact saved cursor."
  (disco-directory--sync-gateway-session)
  (let* ((parent-id (format "%s" parent-channel-id))
         (guild-id (disco-directory--validate-parent-thread-load parent-id))
         (state (disco-directory-parent-threads-state parent-id)))
    (if (not (eq (plist-get state :status) 'error))
        (plist-get state :status)
      (let* ((mode (plist-get state :mode))
             (cursor (and (eq mode 'append)
                          (plist-get state :next-cursor))))
        (unless (memq mode '(replace append))
          (error "Disco: failed parent-thread request has no retry mode for %s"
                 parent-id))
        (when (and (eq mode 'append) (null cursor))
          (error "Disco: failed append request has no retry cursor for %s"
                 parent-id))
        (disco-directory--start-parent-thread-page
         parent-id guild-id cursor mode)))))

(defun disco-directory--guild-known-p (guild-id)
  "Return non-nil when GUILD-ID remains in the guild index."
  (seq-some (lambda (guild)
              (equal (format "%s" (alist-get 'id guild))
                     (format "%s" guild-id)))
            (disco-state-guilds)))

(defun disco-directory--prune-guild-requests ()
  "Remove request metadata for guilds absent from the current index."
  (let (departed departed-parents)
    (maphash
     (lambda (guild-id _status)
       (unless (disco-directory--guild-known-p guild-id)
         (push guild-id departed)))
     disco-directory--guild-status)
    (dolist (guild-id departed)
      (remhash guild-id disco-directory--guild-status)
      (remhash guild-id disco-directory--guild-generation))
    (maphash
     (lambda (parent-id state)
       (unless (disco-directory--guild-known-p (plist-get state :guild-id))
         (push parent-id departed-parents)))
     disco-directory--parent-thread-state)
    (dolist (parent-id departed-parents)
      (remhash parent-id disco-directory--parent-thread-state))))

(cl-defun disco-directory-refresh-index-async (&key on-complete)
  "Refresh guild and private-channel indexes asynchronously.

Call ON-COMPLETE exactly once with a result plist.  Its `:status' is
`completed', `superseded', or `cancelled'; `:errors' contains endpoint/error
pairs collected by a completed request.  Starting another index refresh
immediately supersedes the current one.  A directory reset or a new Gateway
READY session cancels it.  Superseded and cancelled responses never mutate
directory state."
  (disco-directory--sync-gateway-session)
  (let* ((superseded-request disco-directory--index-request)
         (generation (cl-incf disco-directory--index-generation))
         (session-generation (disco-gateway-session-generation))
         (request (list :generation generation
                        :session-generation session-generation
                        :on-complete on-complete
                        :settled nil))
         (pending-endpoints (make-hash-table :test #'eq))
         (pending 2)
         errors)
    (puthash 'guilds t pending-endpoints)
    (puthash 'private-channels t pending-endpoints)
    (setq disco-directory--index-request request)
    (disco-directory--emit 'index-loading :generation generation)
    (cl-labels
        ((active-p ()
           (and (not (plist-get request :settled))
                (eq request disco-directory--index-request)
                (= generation disco-directory--index-generation)
                (= session-generation
                   (disco-gateway-session-generation))))
         (settle-stale ()
           (unless (plist-get request :settled)
             (disco-directory--settle-index-request
              request
              (if (= session-generation
                     (disco-gateway-session-generation))
                  'superseded
                'cancelled)
              errors)))
         (endpoint-pending-p (endpoint)
           (gethash endpoint pending-endpoints))
         (finish-one (endpoint)
           (when (endpoint-pending-p endpoint)
             (remhash endpoint pending-endpoints)
             (if (active-p)
                 (progn
                   (cl-decf pending)
                   (when (zerop pending)
                     (disco-directory--settle-index-request
                      request 'completed errors)))
               (settle-stale))))
         (fail (endpoint error-value)
           (when (and (endpoint-pending-p endpoint) (active-p))
             (push (cons endpoint error-value) errors))
           (finish-one endpoint)))
      ;; Install the new owner before notifying the old request.  If an old
      ;; completion callback starts another refresh synchronously, that newer
      ;; refresh supersedes this one and `active-p' prevents duplicate I/O.
      (when superseded-request
        (disco-directory--settle-index-request
         superseded-request 'superseded nil))
      (when (active-p)
        (disco-api-user-guilds-async
         :on-success
         (lambda (guilds)
           (when (and (endpoint-pending-p 'guilds) (active-p))
             (disco-state-set-guilds guilds)
             (disco-directory--prune-guild-requests))
           (finish-one 'guilds))
         :on-error (lambda (error-value) (fail 'guilds error-value))))
      (when (active-p)
        (disco-api-user-private-channels-async
         :on-success
         (lambda (channels)
           (when (and (endpoint-pending-p 'private-channels) (active-p))
             (disco-state-set-private-channels channels))
           (finish-one 'private-channels))
         :on-error (lambda (error-value)
                     (fail 'private-channels error-value))))
      generation)))

(defun disco-directory--load-active-threads-async
    (guild-id generation request-epoch session-generation)
  "Load active threads for GUILD-ID under exact request ownership.

GENERATION, REQUEST-EPOCH, and SESSION-GENERATION must all remain current."
  (when disco-fetch-guild-active-threads
    (disco-api-guild-active-threads-async
     guild-id
     :on-success
     (lambda (active)
       (when (and (= request-epoch disco-directory--request-epoch)
                  (= session-generation
                     (disco-gateway-session-generation))
                  (= generation
                     (gethash guild-id disco-directory--guild-generation 0))
                  (disco-directory--guild-known-p guild-id))
         (dolist (thread (or (alist-get 'threads active) '()))
           (let ((stored (copy-tree thread)))
             (setf (alist-get 'guild_id stored) guild-id)
             (disco-state-upsert-channel stored)))
         (disco-directory--emit 'guild-enriched :guild-id guild-id)))
     :on-error
     (lambda (error-value)
       (when (and (= request-epoch disco-directory--request-epoch)
                  (= session-generation
                     (disco-gateway-session-generation))
                  (= generation
                     (gethash guild-id disco-directory--guild-generation 0)))
         (disco-directory--emit
          'guild-enrichment-error
          :guild-id guild-id :error error-value))))))

(cl-defun disco-directory-load-guild-async (guild-id &key force)
  "Ensure GUILD-ID has a complete channel snapshot.

When FORCE is non-nil, refresh an already loaded snapshot.  Concurrent loads
for the same guild are coalesced."
  (setq guild-id (format "%s" guild-id))
  (let ((status (disco-directory-guild-status guild-id)))
    (cond
     ((eq status 'loading)
      'loading)
     ((and (eq status 'loaded) (not force))
      'loaded)
     ((not (disco-directory--guild-known-p guild-id))
      (error "Disco: cannot load unknown guild %s" guild-id))
     (t
      (let ((generation
             (1+ (gethash guild-id disco-directory--guild-generation 0)))
            (request-epoch disco-directory--request-epoch)
            (session-generation (disco-gateway-session-generation)))
        (puthash guild-id generation disco-directory--guild-generation)
        (puthash guild-id 'loading disco-directory--guild-status)
        (disco-directory--emit
         'guild-loading :guild-id guild-id :generation generation)
        (disco-api-guild-channels-async
         guild-id
         :on-success
         (lambda (channels)
           (when (and (= request-epoch disco-directory--request-epoch)
                      (= session-generation
                         (disco-gateway-session-generation))
                      (= generation
                         (gethash guild-id disco-directory--guild-generation 0))
                      (disco-directory--guild-known-p guild-id))
             (if (disco-directory--guild-channel-snapshot-resolved-p channels)
                 (progn
                   (disco-state-put-channels guild-id channels)
                   (puthash guild-id 'loaded disco-directory--guild-status)
                   (disco-directory--emit
                    'guild-loaded :guild-id guild-id :generation generation)
                   (disco-directory--load-active-threads-async
                    guild-id generation request-epoch session-generation))
               (let ((error-value
                      (disco-directory--unresolved-channel-snapshot-error
                       guild-id channels)))
                 (puthash guild-id 'error disco-directory--guild-status)
                 (disco-directory--emit
                  'guild-error :guild-id guild-id :generation generation
                  :error error-value)))))
         :on-error
         (lambda (error-value)
           (when (and (= request-epoch disco-directory--request-epoch)
                      (= session-generation
                         (disco-gateway-session-generation))
                      (= generation
                         (gethash guild-id disco-directory--guild-generation 0)))
             (puthash guild-id 'error disco-directory--guild-status)
             (disco-directory--emit
              'guild-error :guild-id guild-id :generation generation
              :error error-value))))
        'loading)))))

(cl-defun disco-directory-refresh-all-async ()
  "Refresh the index, then explicitly hydrate every known guild."
  (disco-directory-refresh-index-async
   :on-complete
   (lambda (result)
     (when (and (eq (plist-get result :status) 'completed)
                (not (assq 'guilds (plist-get result :errors))))
       (dolist (guild (disco-state-guilds))
         (when-let* ((guild-id (alist-get 'id guild)))
           (disco-directory-load-guild-async guild-id :force t)))))))

(provide 'disco-directory)

;;; disco-directory.el ends here
