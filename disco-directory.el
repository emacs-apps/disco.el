;;; disco-directory.el --- Lazy Discord guild directory owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns REST request lifecycles for the guild/DM index and per-guild channel
;; snapshots.  Views consume state and directory events; they never issue these
;; requests directly.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-customize)
(require 'disco-permission)
(require 'disco-state)
(require 'disco-thread)
(require 'disco-util)

(defvar disco-directory-event-hook nil
  "Hook run with one directory lifecycle event plist.")

(defvar disco-directory--index-generation 0)
(defvar disco-directory--guild-generation (make-hash-table :test #'equal))
(defvar disco-directory--guild-status (make-hash-table :test #'equal))

(defvar disco-directory--parent-thread-request-sequence 0
  "Monotonic request token for parent-scoped active-thread searches.")

(defvar disco-directory--parent-thread-state (make-hash-table :test #'equal)
  "Hash table parent-channel-id -> active-thread request state plist.")

(defconst disco-directory--parent-thread-page-limit 25
  "Maximum page size accepted by Discord's thread-search endpoint.")

(defconst disco-directory--parent-thread-index-retries 3
  "Number of delayed retries for an unavailable Discord search index.")

(defun disco-directory-reset ()
  "Reset directory request ownership state."
  (cl-incf disco-directory--index-generation)
  (clrhash disco-directory--guild-generation)
  (clrhash disco-directory--guild-status)
  (clrhash disco-directory--parent-thread-state))

(defun disco-directory--emit (type &rest properties)
  "Emit directory event TYPE with PROPERTIES."
  (run-hook-with-args 'disco-directory-event-hook
                      (append (list :type type) properties)))

(defun disco-directory-guild-status (guild-id)
  "Return request status for GUILD-ID.

The result is `loading', `error', `loaded', or `unloaded'."
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
  "Return active-thread request state for PARENT-CHANNEL-ID.

The returned plist has a `:status' value of `unloaded', `loading', `loaded',
or `error'.  Loading state may also expose `:loaded-count', `:total', and
`:phase'; error state exposes the original `:error' payload."
  (or (gethash (format "%s" parent-channel-id)
               disco-directory--parent-thread-state)
      '(:status unloaded :loaded-count 0)))

(defun disco-directory-parent-threads-status (parent-channel-id)
  "Return active-thread request status for PARENT-CHANNEL-ID."
  (plist-get (disco-directory-parent-threads-state parent-channel-id) :status))

(defun disco-directory--parent-thread-request-active-p (parent-id token)
  "Return non-nil when TOKEN still owns PARENT-ID's active request."
  (let ((state (gethash parent-id disco-directory--parent-thread-state)))
    (and (eq (plist-get state :status) 'loading)
         (= (or (plist-get state :token) -1) token)
         (disco-state-channel parent-id)
         (disco-directory--guild-known-p (plist-get state :guild-id)))))

(defun disco-directory--put-parent-thread-state (parent-id state event-type)
  "Store PARENT-ID request STATE and emit EVENT-TYPE."
  (puthash parent-id state disco-directory--parent-thread-state)
  (disco-directory--emit
   event-type
   :guild-id (plist-get state :guild-id)
   :parent-id parent-id
   :state state))

(defun disco-directory--merge-thread-results (existing new-threads)
  "Append NEW-THREADS to EXISTING while deduplicating channel IDs."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (thread (append existing new-threads))
      (when-let* ((thread-id (alist-get 'id thread)))
        (setq thread-id (format "%s" thread-id))
        (unless (gethash thread-id seen)
          (puthash thread-id t seen)
          (push thread merged))))
    (nreverse merged)))

(defun disco-directory--ingest-thread-search-page
    (guild-id threads first-messages)
  "Store one GUILD-ID thread-search page of THREADS and FIRST-MESSAGES."
  (dolist (thread threads)
    (let ((stored (copy-tree thread)))
      (setf (alist-get 'guild_id stored) guild-id)
      (disco-state-upsert-channel stored)))
  (dolist (message first-messages)
    (when-let* ((channel-id (alist-get 'channel_id message)))
      (disco-state-upsert-message channel-id message))))

(defun disco-directory--missing-thread-starter-ids (threads)
  "Return IDs of THREADS whose starter messages are not cached."
  (delq nil
        (mapcar
         (lambda (thread)
           (unless (disco-thread-starter-message thread)
             (and (alist-get 'id thread)
                  (format "%s" (alist-get 'id thread)))))
         threads)))

(defun disco-directory--parent-thread-previews-complete-p (parent-id)
  "Return non-nil when every active thread below PARENT-ID has a starter."
  (seq-every-p
   (lambda (thread)
     (or (disco-thread-archived-p thread)
         (disco-thread-starter-message thread)))
   (disco-state-parent-threads parent-id)))

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

(defun disco-directory--fail-parent-thread-request
    (parent-id guild-id token error-value)
  "Finish TOKEN for PARENT-ID in GUILD-ID with ERROR-VALUE."
  (when (disco-directory--parent-thread-request-active-p parent-id token)
    (disco-directory--put-parent-thread-state
     parent-id
     (list :status 'error
           :guild-id guild-id
           :token token
           :error error-value)
     'parent-threads-error)))

(defun disco-directory--load-parent-thread-page
    (parent-id guild-id token max-id accumulated total index-retries)
  "Load one active-thread page for PARENT-ID owned by TOKEN.

GUILD-ID owns the parent.  MAX-ID is the stable creation-time cursor,
ACCUMULATED contains prior unique results, TOTAL is Discord's latest total
count, and INDEX-RETRIES counts delayed index retries already attempted for
this page."
  (disco-api-channel-search-threads-async
   parent-id
   :archived :false
   :sort-by 'creation-time
   :sort-order 'desc
   :limit disco-directory--parent-thread-page-limit
   :max-id max-id
   :on-success
   (lambda (result)
     (when (disco-directory--parent-thread-request-active-p parent-id token)
       (if (disco-directory--thread-search-index-pending-p result)
           (if (< index-retries disco-directory--parent-thread-index-retries)
               (let* ((delay
                       (disco-directory--thread-search-retry-delay result))
                      (state
                       (list :status 'loading
                             :phase 'indexing
                             :guild-id guild-id
                             :token token
                             :loaded-count (length accumulated)
                             :total total
                             :retry-after delay)))
                 (disco-directory--put-parent-thread-state
                  parent-id state 'parent-threads-loading)
                 (run-at-time
                  delay nil
                  (lambda ()
                    (when (disco-directory--parent-thread-request-active-p
                           parent-id token)
                      (disco-directory--load-parent-thread-page
                       parent-id guild-id token max-id accumulated total
                       (1+ index-retries))))))
             (disco-directory--fail-parent-thread-request
              parent-id guild-id token
              (list :status 202
                    :body result
                    :message (or (alist-get 'message result)
                                 "Thread search index is not available"))))
         (let* ((page (or (alist-get 'threads result) '()))
                (merged
                 (disco-directory--merge-thread-results accumulated page))
                (next-total (or total (alist-get 'total_results result)))
                (has-more (disco-util-json-true-p
                           (alist-get 'has_more result)))
                (next-max-id
                 (and page (alist-get 'id (car (last page))))))
           (disco-directory--ingest-thread-search-page
            guild-id page (or (alist-get 'first_messages result) '()))
           (let ((missing-starter-ids
                  (disco-directory--missing-thread-starter-ids page)))
             (cond
              (missing-starter-ids
               (disco-directory--fail-parent-thread-request
                parent-id guild-id token
                (list :message "Thread search omitted starter messages"
                      :thread-ids missing-starter-ids)))
              ((and has-more
                    (or (null next-max-id)
                        (equal (format "%s" next-max-id) max-id)))
               (disco-directory--fail-parent-thread-request
                parent-id guild-id token
                '(:message "Thread search returned no next creation-time cursor")))
              (t
               (disco-directory--put-parent-thread-state
                parent-id
                (list :status (if has-more 'loading 'loaded)
                      :phase (and has-more 'fetching)
                      :guild-id guild-id
                      :token token
                      :loaded-count (length merged)
                      :total next-total)
                (if has-more 'parent-threads-page 'parent-threads-loaded))
               (when has-more
                 (disco-directory--load-parent-thread-page
                  parent-id guild-id token (format "%s" next-max-id)
                  merged next-total 0)))))))))
   :on-error
   (lambda (error-value)
     (disco-directory--fail-parent-thread-request
      parent-id guild-id token error-value))))

(cl-defun disco-directory-load-parent-threads-async
    (parent-channel-id &key force)
  "Load every active thread under PARENT-CHANNEL-ID asynchronously.

Search pages are fetched lazily only for the selected parent.  Concurrent
loads are coalesced; FORCE supersedes an in-flight or completed request."
  (let* ((parent-id (format "%s" parent-channel-id))
         (parent (disco-state-channel parent-id))
         (guild-id (and parent (alist-get 'guild_id parent)))
         (status (disco-directory-parent-threads-status parent-id)))
    (unless parent
      (error "Disco: cannot load threads for unknown parent %s" parent-id))
    (unless (disco-channel-thread-parent-p parent)
      (error "Disco: channel %s cannot contain threads" parent-id))
    (unless guild-id
      (error "Disco: parent channel %s has no guild context" parent-id))
    (setq guild-id (format "%s" guild-id))
    (cond
     ((and (eq status 'loading) (not force))
      'loading)
     ((and (eq status 'loaded)
           (not force)
           (disco-directory--parent-thread-previews-complete-p parent-id))
      'loaded)
     (t
      (let* ((token (cl-incf disco-directory--parent-thread-request-sequence))
             (state (list :status 'loading
                          :phase 'fetching
                          :guild-id guild-id
                          :token token
                          :loaded-count 0
                          :total nil)))
        (disco-directory--put-parent-thread-state
         parent-id state 'parent-threads-loading)
        (disco-directory--load-parent-thread-page
         parent-id guild-id token nil nil nil 0)
        'loading)))))

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

Call ON-COMPLETE with a list of endpoint errors after both requests settle."
  (let ((generation (cl-incf disco-directory--index-generation))
        (pending 2)
        errors)
    (disco-directory--emit 'index-loading :generation generation)
    (cl-labels
        ((active-p () (= generation disco-directory--index-generation))
         (finish-one ()
           (cl-decf pending)
           (when (and (zerop pending) (active-p))
             (setq errors (nreverse errors))
             (disco-directory--emit
              'index-loaded :generation generation :errors errors)
             (when on-complete
               (funcall on-complete errors))))
         (fail (endpoint error-value)
           (when (active-p)
             (push (cons endpoint error-value) errors))
           (finish-one)))
      (disco-api-user-guilds-async
       :on-success
       (lambda (guilds)
         (when (active-p)
           (disco-state-set-guilds guilds)
           (disco-directory--prune-guild-requests))
         (finish-one))
       :on-error (lambda (error-value) (fail 'guilds error-value)))
      (disco-api-user-private-channels-async
       :on-success
       (lambda (channels)
         (when (active-p)
           (disco-state-set-private-channels channels))
         (finish-one))
       :on-error (lambda (error-value) (fail 'private-channels error-value)))
      generation)))

(defun disco-directory--load-active-threads-async (guild-id generation)
  "Load active threads for GUILD-ID under request GENERATION."
  (when disco-fetch-guild-active-threads
    (disco-api-guild-active-threads-async
     guild-id
     :on-success
     (lambda (active)
       (when (and (= generation
                     (gethash guild-id disco-directory--guild-generation 0))
                  (disco-directory--guild-known-p guild-id))
         (dolist (thread (or (alist-get 'threads active) '()))
           (disco-state-upsert-channel thread))
         (disco-directory--emit 'guild-enriched :guild-id guild-id)))
     :on-error
     (lambda (error-value)
       (when (= generation
                (gethash guild-id disco-directory--guild-generation 0))
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
      (error "disco: cannot load unknown guild %s" guild-id))
     (t
      (let ((generation
             (1+ (gethash guild-id disco-directory--guild-generation 0))))
        (puthash guild-id generation disco-directory--guild-generation)
        (puthash guild-id 'loading disco-directory--guild-status)
        (disco-directory--emit
         'guild-loading :guild-id guild-id :generation generation)
        (disco-api-guild-channels-async
         guild-id
         :on-success
         (lambda (channels)
           (when (and (= generation
                         (gethash guild-id disco-directory--guild-generation 0))
                      (disco-directory--guild-known-p guild-id))
             (if (disco-directory--guild-channel-snapshot-resolved-p channels)
                 (progn
                   (disco-state-put-channels guild-id channels)
                   (puthash guild-id 'loaded disco-directory--guild-status)
                   (disco-directory--emit
                    'guild-loaded :guild-id guild-id :generation generation)
                   (disco-directory--load-active-threads-async guild-id generation))
               (let ((error-value
                      (disco-directory--unresolved-channel-snapshot-error
                       guild-id channels)))
                 (puthash guild-id 'error disco-directory--guild-status)
                 (disco-directory--emit
                  'guild-error :guild-id guild-id :generation generation
                  :error error-value)))))
         :on-error
         (lambda (error-value)
           (when (= generation
                    (gethash guild-id disco-directory--guild-generation 0))
             (puthash guild-id 'error disco-directory--guild-status)
             (disco-directory--emit
              'guild-error :guild-id guild-id :generation generation
              :error error-value))))
        'loading)))))

(cl-defun disco-directory-refresh-all-async ()
  "Refresh the index, then explicitly hydrate every known guild."
  (disco-directory-refresh-index-async
   :on-complete
   (lambda (errors)
     (unless (assq 'guilds errors)
       (dolist (guild (disco-state-guilds))
         (when-let* ((guild-id (alist-get 'id guild)))
           (disco-directory-load-guild-async guild-id :force t)))))))

(provide 'disco-directory)

;;; disco-directory.el ends here
