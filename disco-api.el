;;; disco-api.el --- Discord REST API wrapper for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Synchronous HTTP wrapper for MVP workflows:
;; current user, guild list, guild channels, channel messages, send message.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'disco-customize)
(require 'disco-http)
(require 'disco-api-normalize)

(define-error 'disco-api-error "Disco API error")

(defvar disco-api--global-rate-limit-until 0.0
  "Unix timestamp until which global requests must wait.")

(defvar disco-api--route-rate-limit-until (make-hash-table :test #'equal)
  "Hash route-key -> unix timestamp deadline.")

(defvar disco-api--route-bucket-map (make-hash-table :test #'equal)
  "Hash route-key -> bucket-id.")

(defvar disco-api--bucket-rate-limit-until (make-hash-table :test #'equal)
  "Hash bucket-id -> unix timestamp deadline.")

(defun disco-api--blocked-entries (table)
  "Return sorted list of active blocked entries in TABLE.

Each element is (KEY . remaining-seconds)."
  (let ((now (disco-api--now))
        entries)
    (maphash
     (lambda (key deadline)
       (let ((remaining (- deadline now)))
         (when (> remaining 0)
           (push (cons key remaining) entries))))
     table)
    (sort entries (lambda (a b) (> (cdr a) (cdr b))))))

(defun disco-api-rate-limit-snapshot ()
  "Return current rate-limit snapshot as plist."
  (let* ((now (disco-api--now))
         (global-block (max 0 (- disco-api--global-rate-limit-until now)))
         (route-blocks (disco-api--blocked-entries disco-api--route-rate-limit-until))
         (bucket-blocks (disco-api--blocked-entries disco-api--bucket-rate-limit-until)))
    (list :global-block global-block
          :route-block-count (length route-blocks)
          :bucket-block-count (length bucket-blocks)
          :route-bucket-map-count (hash-table-count disco-api--route-bucket-map)
          :route-blocks route-blocks
          :bucket-blocks bucket-blocks)))

(defun disco-api-describe-rate-limits ()
  "Show current rate-limit state in a dedicated buffer."
  (interactive)
  (let* ((snapshot (disco-api-rate-limit-snapshot))
         (global-block (plist-get snapshot :global-block))
         (route-blocks (plist-get snapshot :route-blocks))
         (bucket-blocks (plist-get snapshot :bucket-blocks))
         (buf (get-buffer-create "*disco-rate-limit*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Global block: %.3fs\n" global-block))
        (insert (format "Blocked routes: %d\n" (length route-blocks)))
        (insert (format "Blocked buckets: %d\n" (length bucket-blocks)))
        (insert (format "Route->bucket mappings: %d\n\n"
                        (plist-get snapshot :route-bucket-map-count)))
        (insert "Top route blocks:\n")
        (if route-blocks
            (dolist (entry (seq-take route-blocks 20))
              (insert (format "  %s => %.3fs\n" (car entry) (cdr entry))))
          (insert "  (none)\n"))
        (insert "\nTop bucket blocks:\n")
        (if bucket-blocks
            (dolist (entry (seq-take bucket-blocks 20))
              (insert (format "  %s => %.3fs\n" (car entry) (cdr entry))))
          (insert "  (none)\n"))
        (special-mode)))
    (pop-to-buffer buf)))

(defun disco-api-reset-rate-limit-state ()
  "Reset cached rate-limit tracking state."
  (setq disco-api--global-rate-limit-until 0.0)
  (clrhash disco-api--route-rate-limit-until)
  (clrhash disco-api--route-bucket-map)
  (clrhash disco-api--bucket-rate-limit-until))

(defun disco-api--now ()
  "Return current unix timestamp as float."
  (float-time))

(defun disco-api--route-key (method endpoint)
  "Build stable route key from METHOD and ENDPOINT."
  (format "%s %s" method endpoint))

(defun disco-api--header (headers key)
  "Return header KEY value from HEADERS, supporting symbol/string forms."
  (let ((sym (if (symbolp key)
                 key
               (intern (downcase key))))
        (str (if (symbolp key)
                 (symbol-name key)
               key)))
    (or (cdr (assq sym headers))
        (cdr (assoc str headers)))))

(defun disco-api--to-number (value)
  "Convert VALUE to number when possible, else nil."
  (cond
   ((numberp value)
    (float value))
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
    (string-to-number value))
   (t nil)))

(defun disco-api--remaining-zero-p (value)
  "Return non-nil if VALUE represents 0 remaining requests."
  (or (equal value "0")
      (equal value 0)
      (equal value 0.0)))

(defun disco-api--extract-retry-after (headers body)
  "Extract retry-after seconds from HEADERS or BODY.

Return a number (seconds) or nil."
  (or (disco-api--to-number (disco-api--header headers 'retry-after))
      (and (listp body)
           (disco-api--to-number (alist-get 'retry_after body)))))

(defun disco-api--route-deadline (route-key)
  "Return current effective deadline for ROUTE-KEY."
  (let* ((route-deadline (or (gethash route-key disco-api--route-rate-limit-until) 0.0))
         (bucket-id (gethash route-key disco-api--route-bucket-map))
         (bucket-deadline (if bucket-id
                              (or (gethash bucket-id disco-api--bucket-rate-limit-until) 0.0)
                            0.0)))
    (max route-deadline bucket-deadline)))

(defun disco-api--wait-for-rate-limit (route-key)
  "Sleep until ROUTE-KEY is no longer blocked by rate-limit state."
  (let* ((deadline (max disco-api--global-rate-limit-until
                        (disco-api--route-deadline route-key)))
         (sleep-time (- deadline (disco-api--now))))
    (when (> sleep-time 0)
      (sleep-for sleep-time))))

(defun disco-api--set-route-deadline (route-key deadline &optional bucket-id)
  "Set DEADLINE for ROUTE-KEY, optionally under BUCKET-ID."
  (if bucket-id
      (puthash bucket-id deadline disco-api--bucket-rate-limit-until)
    (puthash route-key deadline disco-api--route-rate-limit-until)))

(defun disco-api--update-rate-limit-state (route-key status headers body)
  "Update in-memory rate-limit state from one response.

ROUTE-KEY identifies the API route.
STATUS, HEADERS, BODY come from transport layer."
  (let* ((now (disco-api--now))
         (bucket-id (disco-api--header headers 'x-ratelimit-bucket))
         (remaining (disco-api--header headers 'x-ratelimit-remaining))
         (reset-after (disco-api--to-number
                       (disco-api--header headers 'x-ratelimit-reset-after)))
         (retry-after (disco-api--extract-retry-after headers body))
         (global-header (disco-api--header headers 'x-ratelimit-global))
         (global-body (and (listp body) (alist-get 'global body))))
    (when bucket-id
      (puthash route-key bucket-id disco-api--route-bucket-map))

    ;; Proactive cooldown when bucket is exhausted.
    (when (and (disco-api--remaining-zero-p remaining)
               reset-after
               (> reset-after 0))
      (disco-api--set-route-deadline
       route-key
       (+ now reset-after disco-rate-limit-safety-margin)
       bucket-id))

    ;; Authoritative cooldown on 429 responses.
    (when (and (= status 429) retry-after)
      (let ((deadline (+ now retry-after disco-rate-limit-safety-margin)))
        (if (or (disco-api--json-true-p global-body)
                (equal global-header "true"))
            (setq disco-api--global-rate-limit-until deadline)
          (disco-api--set-route-deadline route-key deadline bucket-id))))))

(defun disco-api--auth-header ()
  "Return Authorization header value from active token source."
  (let ((token (disco-current-token)))
    (unless token
      (user-error "disco: token is not set; use M-x disco-set-token or DISCO_TOKEN"))
    token))

(defun disco-api--normalize-query-entry (entry)
  "Normalize one query ENTRY for `url-build-query-string'.

Supported entry shapes are (KEY . VALUE) and (KEY VALUE)."
  (cond
   ((and (consp entry)
         (consp (cdr entry))
         (null (cddr entry)))
    (let ((key (car entry))
          (value (cadr entry)))
      (when (and key value)
        (list (format "%s" key)
              (format "%s" value)))))
   ((and (consp entry) (not (listp (cdr entry))))
    (let* ((raw-key (car entry))
           (raw-value (cdr entry))
           (key (if (symbolp raw-key)
                    (symbol-name raw-key)
                  raw-key)))
      (when (and key raw-value)
        (list (format "%s" key)
              (format "%s" raw-value)))))
   (t nil)))

(defun disco-api--normalize-query (query)
  "Normalize QUERY entries for `url-build-query-string'."
  (delq nil (mapcar #'disco-api--normalize-query-entry query)))

(defun disco-api--build-url (endpoint &optional query)
  "Build full URL using ENDPOINT and optional QUERY alist."
  (let ((normalized-query
         (and (consp query)
              (disco-api--normalize-query query))))
    (concat
     (replace-regexp-in-string "/$" "" disco-api-base-url)
     endpoint
     (if normalized-query
         (concat "?" (url-build-query-string normalized-query))
       ""))))

(defun disco-api--json-encode (payload)
  "JSON encode PAYLOAD, preserving UTF-8."
  (let ((json-encoding-pretty-print nil)
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-encode payload)))

(defun disco-api--decode-json (body-text)
  "Decode BODY-TEXT into alist/list JSON value.

Return nil for empty or non-JSON body."
  (if (or (null body-text) (string-empty-p body-text))
      nil
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false :false))
      (condition-case _
          (json-read-from-string body-text)
        (error nil)))))

(defun disco-api--http-error-message (status raw-body body)
  "Return user-facing message for STATUS/RAW-BODY/BODY response tuple."
  (format "HTTP %d %s"
          status
          (or (and (listp body) (alist-get 'message body))
              (and (not (string-empty-p (or raw-body ""))) raw-body)
              "request failed")))

(defun disco-api--error-plist (status body message)
  "Build normalized async error plist."
  (list :status status
        :body body
        :message message))

(defun disco-api--request (method endpoint &optional payload query unauthenticated
                                  raw-body extra-headers body-type)
  "Execute METHOD request to ENDPOINT.

PAYLOAD is an alist encoded as JSON for request body.
QUERY is an alist for query parameters.
If UNAUTHENTICATED is non-nil, omit Authorization header.
RAW-BODY and EXTRA-HEADERS enable non-JSON requests (for example multipart).
BODY-TYPE is forwarded to transport layer."
  (when (and payload raw-body)
    (error "disco: payload and raw-body cannot be combined"))
  (let* ((headers
          (append
           (unless raw-body
             '(("Content-Type" . "application/json")))
           `(("Accept" . "application/json")
             ("User-Agent" . ,disco-user-agent)
             ("X-Discord-Locale" . ,disco-locale)
             ("Accept-Language" . ,disco-locale))
           (or extra-headers '())
           (unless unauthenticated
             `(("Authorization" . ,(disco-api--auth-header))))))
         (data (cond
                ((not (null raw-body))
                 raw-body)
                ((eq payload :empty-object)
                 "{}")
                (payload
                 (disco-api--json-encode payload))
                (t nil)))
         (effective-body-type (or body-type
                                  (when raw-body 'binary)))
         (url (disco-api--build-url endpoint query))
         (route-key (disco-api--route-key method endpoint))
         (attempt 0))
    (catch 'disco-api-return
      (while t
        (disco-api--wait-for-rate-limit route-key)
        (let* ((response (disco-http-request
                          :method method
                          :url url
                          :headers headers
                          :body data
                          :body-type effective-body-type
                          :timeout disco-http-timeout))
               (status (or (plist-get response :status) 0))
               (raw-body (or (plist-get response :body) ""))
               (response-headers (or (plist-get response :headers) nil))
               (body (disco-api--decode-json raw-body))
               (retry-after (disco-api--extract-retry-after response-headers body)))
          (disco-api--update-rate-limit-state route-key status response-headers body)
          (cond
           ((and (>= status 200) (< status 300))
            (throw 'disco-api-return body))
           ((= status 429)
            (if (>= attempt disco-rate-limit-max-retries)
                (signal
                 'disco-api-error
                 (list (format "rate limited (429), retries exhausted, retry-after=%s"
                               (or retry-after "unknown"))
                       status
                       body))
              (setq attempt (1+ attempt))
              (sleep-for (or retry-after 1.0))))
           (t
            (signal
             'disco-api-error
             (list (disco-api--http-error-message status raw-body body)
                   status
                   body)))))))))

(cl-defun disco-api--request-async (method endpoint &key payload query unauthenticated on-success on-error
                                           raw-body extra-headers body-type)
  "Execute METHOD request to ENDPOINT asynchronously.

ON-SUCCESS receives decoded JSON body.
ON-ERROR receives plist `(:status :body :message)'.
RAW-BODY and EXTRA-HEADERS enable non-JSON requests (for example multipart).
BODY-TYPE is forwarded to transport layer."
  (when (and payload raw-body)
    (error "disco: payload and raw-body cannot be combined"))
  (let* ((headers
          (append
           (unless raw-body
             '(("Content-Type" . "application/json")))
           `(("Accept" . "application/json")
             ("User-Agent" . ,disco-user-agent)
             ("X-Discord-Locale" . ,disco-locale)
             ("Accept-Language" . ,disco-locale))
           (or extra-headers '())
           (unless unauthenticated
             `(("Authorization" . ,(disco-api--auth-header))))))
         (data (cond
                ((not (null raw-body))
                 raw-body)
                ((eq payload :empty-object)
                 "{}")
                (payload
                 (disco-api--json-encode payload))
                (t nil)))
         (effective-body-type (or body-type
                                  (when raw-body 'binary)))
         (url (disco-api--build-url endpoint query))
         (route-key (disco-api--route-key method endpoint))
         (attempt 0))
    (cl-labels
        ((emit-error (status body message)
           (when on-error
             (funcall on-error (disco-api--error-plist status body message))))
         (schedule-next (delay)
           (run-at-time (max 0 (or delay 0.0)) nil
                        (lambda ()
                          (dispatch-request))))
         (handle-response (response)
           (let* ((status (or (plist-get response :status) 0))
                  (raw-body (or (plist-get response :body) ""))
                  (response-headers (or (plist-get response :headers) nil))
                  (body (disco-api--decode-json raw-body))
                  (retry-after (disco-api--extract-retry-after response-headers body)))
             (disco-api--update-rate-limit-state route-key status response-headers body)
             (cond
              ((and (>= status 200) (< status 300))
               (when on-success
                 (funcall on-success body)))
              ((= status 429)
               (if (>= attempt disco-rate-limit-max-retries)
                   (emit-error
                    status
                    body
                    (format "rate limited (429), retries exhausted, retry-after=%s"
                            (or retry-after "unknown")))
                 (setq attempt (1+ attempt))
                 (schedule-next (or retry-after 1.0))))
              (t
               (emit-error status body
                           (disco-api--http-error-message status raw-body body))))))
         (dispatch-request ()
           (let* ((deadline (max disco-api--global-rate-limit-until
                                 (disco-api--route-deadline route-key)))
                  (wait-time (- deadline (disco-api--now))))
             (if (> wait-time 0)
                 (schedule-next wait-time)
               (disco-http-request-async
                :method method
                :url url
                :headers headers
                :body data
                :body-type effective-body-type
                :timeout disco-http-timeout
                :on-success #'handle-response
                :on-error #'handle-response)))))
      (dispatch-request))))

(defun disco-api-current-user ()
  "Fetch current user object."
  (disco-api--request "GET" "/users/@me" nil nil nil))

(defun disco-api-gateway ()
  "Fetch gateway connection object containing websocket URL."
  (disco-api--request "GET" "/gateway" nil nil t))

(defun disco-api-user-guilds ()
  "Fetch current user's guilds list."
  (disco-api--request "GET" "/users/@me/guilds" nil '(("limit" . "200")) nil))

(cl-defun disco-api-user-guilds-async (&key on-success on-error)
  "Fetch current user's guild list asynchronously."
  (disco-api--request-async
   "GET"
   "/users/@me/guilds"
   :query '(("limit" . "200"))
   :on-success on-success
   :on-error on-error))

(defun disco-api-user-private-channels ()
  "Fetch current user's private channels (DM/group DM) list."
  (disco-api--request "GET" "/users/@me/channels" nil nil nil))

(cl-defun disco-api-user-private-channels-async (&key on-success on-error)
  "Fetch current user's private channels asynchronously."
  (disco-api--request-async
   "GET"
   "/users/@me/channels"
   :on-success on-success
   :on-error on-error))

(defun disco-api-guild-channels (guild-id)
  "Fetch channels in GUILD-ID."
  (disco-api--request
   "GET"
   (format "/guilds/%s/channels" guild-id)
   nil
   '(("permissions" . "true"))
   nil))

(cl-defun disco-api-guild-channels-async (guild-id &key on-success on-error)
  "Fetch channels in GUILD-ID asynchronously."
  (disco-api--request-async
   "GET"
   (format "/guilds/%s/channels" guild-id)
   :query '(("permissions" . "true"))
   :on-success on-success
   :on-error on-error))

(defun disco-api-channel (channel-id)
  "Fetch channel object for CHANNEL-ID."
  (disco-api--request
   "GET"
   (format "/channels/%s" channel-id)
   nil
   nil
   nil))

(cl-defun disco-api-channel-async (channel-id &key on-success on-error)
  "Fetch channel object for CHANNEL-ID asynchronously."
  (disco-api--request-async
   "GET"
   (format "/channels/%s" channel-id)
   :on-success on-success
   :on-error on-error))

(defun disco-api-guild-active-threads (guild-id)
  "Fetch active threads object for GUILD-ID.

Response is an alist with keys including `threads' and `members'."
  (disco-api--request "GET" (format "/guilds/%s/threads/active" guild-id) nil nil nil))

(cl-defun disco-api-guild-active-threads-async (guild-id &key on-success on-error)
  "Fetch active threads object for GUILD-ID asynchronously."
  (disco-api--request-async
   "GET"
   (format "/guilds/%s/threads/active" guild-id)
   :on-success on-success
   :on-error on-error))

(cl-defun disco-api-channel-search-threads
    (channel-id &key name slop tags tag-setting archived sort-by sort-order
                limit offset max-id min-id)
  "Search threads under CHANNEL-ID using `/threads/search'."
  (disco-api--request
   "GET"
   (format "/channels/%s/threads/search" channel-id)
   nil
   (disco-api--thread-search-query
    :name name
    :slop slop
    :tags tags
    :tag-setting tag-setting
    :archived archived
    :sort-by sort-by
    :sort-order sort-order
    :limit limit
    :offset offset
    :max-id max-id
    :min-id min-id)
   nil))

(cl-defun disco-api-channel-search-threads-async
    (channel-id &key name slop tags tag-setting archived sort-by sort-order
                limit offset max-id min-id on-success on-error)
  "Search threads under CHANNEL-ID asynchronously using `/threads/search'."
  (disco-api--request-async
   "GET"
   (format "/channels/%s/threads/search" channel-id)
   :query (disco-api--thread-search-query
           :name name
           :slop slop
           :tags tags
           :tag-setting tag-setting
           :archived archived
           :sort-by sort-by
           :sort-order sort-order
           :limit limit
           :offset offset
           :max-id max-id
           :min-id min-id)
   :on-success on-success
   :on-error on-error))

(defun disco-api-channel-search-active-threads (channel-id &optional limit offset)
  "Search active threads under CHANNEL-ID.

Uses `/channels/{channel.id}/threads/search' with `archived=false'."
  (disco-api-channel-search-threads
   channel-id
   :archived :false
   :sort-by 'last-message-time
   :sort-order 'desc
   :limit limit
   :offset offset))

(cl-defun disco-api-channel-search-active-threads-async
    (channel-id &key limit offset on-success on-error)
  "Search active threads under CHANNEL-ID asynchronously.

Uses `/channels/{channel.id}/threads/search' with `archived=false'."
  (disco-api-channel-search-threads-async
   channel-id
   :archived :false
   :sort-by 'last-message-time
   :sort-order 'desc
   :limit limit
   :offset offset
   :on-success on-success
   :on-error on-error))

(defun disco-api-channel-archived-public-threads (channel-id &optional before limit)
  "Fetch archived public threads under CHANNEL-ID.

BEFORE is an ISO8601 timestamp. LIMIT defaults to 50."
  (disco-api--request
   "GET"
   (format "/channels/%s/threads/archived/public" channel-id)
   nil
   (disco-api--thread-archive-query before limit)
   nil))

(defun disco-api-channel-archived-private-threads (channel-id &optional before limit)
  "Fetch archived private threads under CHANNEL-ID.

BEFORE is an ISO8601 timestamp. LIMIT defaults to 50."
  (disco-api--request
   "GET"
   (format "/channels/%s/threads/archived/private" channel-id)
   nil
   (disco-api--thread-archive-query before limit)
   nil))

(defun disco-api-channel-joined-private-archived-threads (channel-id &optional before limit)
  "Fetch archived private threads joined by current user under CHANNEL-ID.

BEFORE is a thread snowflake ID. LIMIT defaults to 50."
  (disco-api--request
   "GET"
   (format "/channels/%s/users/@me/threads/archived/private" channel-id)
   nil
   (disco-api--thread-archive-query before limit)
   nil))

(cl-defun disco-api-thread-members (thread-id &key with-member after limit)
  "Fetch thread member objects for THREAD-ID."
  (disco-api--request
   "GET"
   (format "/channels/%s/thread-members" thread-id)
   nil
   (disco-api--thread-members-query
    :with-member with-member
    :after after
    :limit limit)
   nil))

(cl-defun disco-api-thread-member (thread-id user-id &key with-member)
  "Fetch thread member object for USER-ID in THREAD-ID."
  (disco-api--request
   "GET"
   (format "/channels/%s/thread-members/%s" thread-id user-id)
   nil
   (disco-api--thread-members-query
    :with-member with-member)
   nil))

(defun disco-api-add-thread-member (thread-id user-id)
  "Add USER-ID to THREAD-ID."
  (disco-api--request
   "PUT"
   (format "/channels/%s/thread-members/%s" thread-id user-id)
   nil
   nil
   nil))

(defun disco-api-remove-thread-member (thread-id user-id)
  "Remove USER-ID from THREAD-ID."
  (disco-api--request
   "DELETE"
   (format "/channels/%s/thread-members/%s" thread-id user-id)
   nil
   nil
   nil))

(defun disco-api-join-thread (thread-id)
  "Join thread THREAD-ID as current user."
  (disco-api--request "PUT" (format "/channels/%s/thread-members/@me" thread-id) nil nil nil))

(defun disco-api-leave-thread (thread-id)
  "Leave thread THREAD-ID as current user."
  (disco-api--request "DELETE" (format "/channels/%s/thread-members/@me" thread-id) nil nil nil))

(cl-defun disco-api-update-thread-member-settings (thread-id &key flags muted mute-config)
  "Update current user's thread settings for THREAD-ID.

FLAGS updates thread member flags. MUTED toggles thread mute state.
MUTE-CONFIG is forwarded as the `mute_config' object."
  (let (payload)
    (when (not (null flags))
      (push `(flags . ,flags) payload))
    (when (not (null muted))
      (push `(muted . ,(if muted t :false)) payload))
    (when mute-config
      (push `(mute_config . ,mute-config) payload))
    (setq payload (nreverse payload))
    (unless payload
      (user-error "disco: no thread settings fields provided"))
    (disco-api--request
     "PATCH"
     (format "/channels/%s/thread-members/@me/settings" thread-id)
     payload
     nil
     nil)))

(cl-defun disco-api-update-thread (thread-id &key name archived locked auto-archive-duration
                                             rate-limit-per-user invitable applied-tags)
  "Patch mutable thread fields for THREAD-ID."
  (let ((payload (disco-api--thread-update-payload
                  :name name
                  :archived archived
                  :locked locked
                  :auto-archive-duration auto-archive-duration
                  :rate-limit-per-user rate-limit-per-user
                  :invitable invitable
                  :applied-tags applied-tags)))
    (unless payload
      (user-error "disco: no thread fields provided"))
    (disco-api--request
     "PATCH"
     (format "/channels/%s" thread-id)
     payload
     nil
     nil)))

(defun disco-api-set-thread-archived (thread-id archived &optional locked)
  "Set THREAD-ID archived state to ARCHIVED.

If LOCKED is non-nil, set lock state in the same request."
  (disco-api-update-thread
   thread-id
   :archived archived
   :locked locked))

(defun disco-api-create-thread-from-message (channel-id message-id name
                                                        &optional auto-archive-duration
                                                        rate-limit-per-user)
  "Create a thread named NAME from MESSAGE-ID under CHANNEL-ID.

AUTO-ARCHIVE-DURATION is minutes (60/1440/4320/10080).
RATE-LIMIT-PER-USER is per-user slowmode seconds."
  (let ((payload `((name . ,name))))
    (when auto-archive-duration
      (setq payload (append payload `((auto_archive_duration . ,auto-archive-duration)))))
    (when rate-limit-per-user
      (setq payload (append payload `((rate_limit_per_user . ,rate-limit-per-user)))))
    (disco-api--request
     "POST"
     (format "/channels/%s/messages/%s/threads" channel-id message-id)
     payload
     nil
     nil)))

(defun disco-api-create-thread (channel-id name
                                           &optional type auto-archive-duration invitable
                                           rate-limit-per-user)
  "Create a detached thread named NAME under CHANNEL-ID.

TYPE is thread channel type (e.g., 11 public, 12 private).
AUTO-ARCHIVE-DURATION is minutes (60/1440/4320/10080).
INVITABLE controls non-moderator invites in private threads.
RATE-LIMIT-PER-USER is per-user slowmode seconds."
  (let ((payload `((name . ,name))))
    (when type
      (setq payload (append payload `((type . ,type)))))
    (when auto-archive-duration
      (setq payload (append payload `((auto_archive_duration . ,auto-archive-duration)))))
    (when (not (null invitable))
      (setq payload (append payload `((invitable . ,(if invitable t :false))))))
    (when rate-limit-per-user
      (setq payload (append payload `((rate_limit_per_user . ,rate-limit-per-user)))))
    (disco-api--request
     "POST"
     (format "/channels/%s/threads" channel-id)
     payload
     nil
     nil)))

(defun disco-api-channel-messages (channel-id &optional before limit)
  "Fetch messages in CHANNEL-ID.

If BEFORE is non-nil, paginate before that message id.
LIMIT defaults to `disco-message-fetch-limit'."
  (let ((query `(("limit" . ,(number-to-string (or limit disco-message-fetch-limit))))))
    (when before
      (setq query (append query `(("before" . ,before)))))
    (disco-api--request "GET" (format "/channels/%s/messages" channel-id) nil query nil)))

(cl-defun disco-api-channel-messages-async (channel-id &key before limit on-success on-error)
  "Fetch messages in CHANNEL-ID asynchronously."
  (let ((query `(("limit" . ,(number-to-string (or limit disco-message-fetch-limit))))))
    (when before
      (setq query (append query `(("before" . ,before)))))
    (disco-api--request-async
     "GET"
     (format "/channels/%s/messages" channel-id)
     :query query
     :on-success on-success
     :on-error on-error)))

(defun disco-api-channel-message (channel-id message-id)
  "Fetch one MESSAGE-ID from CHANNEL-ID."
  (disco-api--request
   "GET"
   (format "/channels/%s/messages/%s" channel-id message-id)
   nil
   nil
   nil))

(cl-defun disco-api-channel-message-async (channel-id message-id &key on-success on-error)
  "Fetch one MESSAGE-ID from CHANNEL-ID asynchronously."
  (disco-api--request-async
   "GET"
   (format "/channels/%s/messages/%s" channel-id message-id)
   :on-success on-success
   :on-error on-error))

(defun disco-api-ack-message (channel-id message-id
                                         &optional token manual mention-count
                                         flags last-viewed)
  "Acknowledge MESSAGE-ID in CHANNEL-ID.

TOKEN is optional read-state ack token from prior responses.
When MANUAL is non-nil, send manual mode to set read cursor explicitly.
MENTION-COUNT implies MANUAL mode and is forwarded as mention_count.
FLAGS and LAST-VIEWED are forwarded when non-nil.

Response may include a refreshed ack token."
  (disco-api--request
   "POST"
   (format "/channels/%s/messages/%s/ack" channel-id message-id)
   (disco-api--ack-message-payload token manual mention-count flags last-viewed)
   nil
   nil))

(cl-defun disco-api-ack-message-async (channel-id message-id
                                                  &key token manual mention-count
                                                  flags last-viewed on-success on-error)
  "Acknowledge MESSAGE-ID in CHANNEL-ID asynchronously."
  (disco-api--request-async
   "POST"
   (format "/channels/%s/messages/%s/ack" channel-id message-id)
   :payload (disco-api--ack-message-payload token manual mention-count flags last-viewed)
   :on-success on-success
   :on-error on-error))


(cl-defun disco-api-create-message (channel-id &key content reply-to-message-id message-reference allowed-mentions attachments poll)
  "Create one message in CHANNEL-ID.

CONTENT is optional text content. REPLY-TO-MESSAGE-ID is a reply shorthand.
MESSAGE-REFERENCE can be used for explicit reply/forward references.
ALLOWED-MENTIONS controls mention parsing for the message.
ATTACHMENTS is an optional list of upload descriptors.
POLL is an optional poll create payload."
  (let* ((normalized-attachments
          (mapcar #'disco-api--normalize-send-attachment (or attachments '())))
         (normalized-poll (disco-api--normalize-poll-request poll))
         (payload (disco-api--message-send-payload
                   content
                   reply-to-message-id
                   message-reference
                   normalized-attachments
                   normalized-poll
                   allowed-mentions)))
    (unless (or (alist-get 'content payload)
                (alist-get 'message_reference payload)
                normalized-attachments
                normalized-poll)
      (user-error "disco: message content, poll, attachments, and message_reference are all empty"))
    (if normalized-attachments
        (let* ((multipart (disco-api--build-message-multipart-body payload normalized-attachments))
               (boundary (car multipart))
               (body (cdr multipart)))
          (disco-api--request
           "POST"
           (format "/channels/%s/messages" channel-id)
           nil
           nil
           nil
           body
           `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary)))
           'binary))
      (disco-api--request
       "POST"
       (format "/channels/%s/messages" channel-id)
       payload
       nil
       nil))))

(cl-defun disco-api-create-message-async (channel-id &key content reply-to-message-id message-reference allowed-mentions attachments poll on-success on-error)
  "Asynchronously create one message in CHANNEL-ID.

Keyword arguments are the same as `disco-api-create-message'."
  (let* ((normalized-attachments
          (mapcar #'disco-api--normalize-send-attachment (or attachments '())))
         (normalized-poll (disco-api--normalize-poll-request poll))
         (payload (disco-api--message-send-payload
                   content
                   reply-to-message-id
                   message-reference
                   normalized-attachments
                   normalized-poll
                   allowed-mentions)))
    (unless (or (alist-get 'content payload)
                (alist-get 'message_reference payload)
                normalized-attachments
                normalized-poll)
      (user-error "disco: message content, poll, attachments, and message_reference are all empty"))
    (if normalized-attachments
        (let* ((multipart (disco-api--build-message-multipart-body payload normalized-attachments))
               (boundary (car multipart))
               (body (cdr multipart)))
          (disco-api--request-async
           "POST"
           (format "/channels/%s/messages" channel-id)
           :raw-body body
           :extra-headers `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary)))
           :body-type 'binary
           :on-success on-success
           :on-error on-error))
      (disco-api--request-async
       "POST"
       (format "/channels/%s/messages" channel-id)
       :payload payload
       :on-success on-success
       :on-error on-error))))

(cl-defun disco-api-send-message-with-attachments (channel-id &key content reply-to-message-id message-reference allowed-mentions attachments)
  "Send message to CHANNEL-ID with ATTACHMENTS.

ATTACHMENTS is a list of file path strings or plists containing :path and
optional :description/:filename/:content-type."
  (disco-api-create-message
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :message-reference message-reference
   :allowed-mentions allowed-mentions
   :attachments attachments))

(cl-defun disco-api-send-message-with-attachments-async (channel-id &key content reply-to-message-id message-reference allowed-mentions attachments on-success on-error)
  "Asynchronously send message to CHANNEL-ID with ATTACHMENTS.

ATTACHMENTS is a list of file path strings or plists containing :path and
optional :description/:filename/:content-type."
  (disco-api-create-message-async
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :message-reference message-reference
   :allowed-mentions allowed-mentions
   :attachments attachments
   :on-success on-success
   :on-error on-error))

(defun disco-api-add-reaction (channel-id message-id emoji)
  "Add EMOJI reaction to MESSAGE-ID in CHANNEL-ID for current user."
  (disco-api--request
   "PUT"
   (format "/channels/%s/messages/%s/reactions/%s/@me"
           channel-id message-id (disco-api--encode-reaction-emoji emoji))
   nil nil nil))

(cl-defun disco-api-add-reaction-async (channel-id message-id emoji &key on-success on-error)
  "Asynchronously add EMOJI reaction to MESSAGE-ID in CHANNEL-ID for current user."
  (disco-api--request-async
   "PUT"
   (format "/channels/%s/messages/%s/reactions/%s/@me"
           channel-id message-id (disco-api--encode-reaction-emoji emoji))
   :on-success on-success
   :on-error on-error))

(defun disco-api-remove-own-reaction (channel-id message-id emoji)
  "Remove current user's EMOJI reaction from MESSAGE-ID in CHANNEL-ID."
  (disco-api--request
   "DELETE"
   (format "/channels/%s/messages/%s/reactions/%s/@me"
           channel-id message-id (disco-api--encode-reaction-emoji emoji))
   nil nil nil))

(cl-defun disco-api-remove-own-reaction-async (channel-id message-id emoji &key on-success on-error)
  "Asynchronously remove current user's EMOJI reaction from MESSAGE-ID."
  (disco-api--request-async
   "DELETE"
   (format "/channels/%s/messages/%s/reactions/%s/@me"
           channel-id message-id (disco-api--encode-reaction-emoji emoji))
   :on-success on-success
   :on-error on-error))

(defun disco-api-create-poll-vote (channel-id message-id answer-ids)
  "Submit poll vote ANSWER-IDS for MESSAGE-ID in CHANNEL-ID."
  (disco-api--request
   "PUT"
   (format "/channels/%s/polls/%s/answers/@me" channel-id message-id)
   `((answer_ids . ,(disco-api--normalize-poll-answer-ids answer-ids)))
   nil
   nil))

(cl-defun disco-api-create-poll-vote-async (channel-id message-id answer-ids &key on-success on-error)
  "Asynchronously submit poll vote ANSWER-IDS for MESSAGE-ID in CHANNEL-ID."
  (disco-api--request-async
   "PUT"
   (format "/channels/%s/polls/%s/answers/@me" channel-id message-id)
   :payload `((answer_ids . ,(disco-api--normalize-poll-answer-ids answer-ids)))
   :on-success on-success
   :on-error on-error))

(defun disco-api-expire-poll (channel-id message-id)
  "End poll for MESSAGE-ID in CHANNEL-ID immediately."
  (disco-api--request
   "POST"
   (format "/channels/%s/polls/%s/expire" channel-id message-id)
   nil
   nil
   nil))

(cl-defun disco-api-expire-poll-async (channel-id message-id &key on-success on-error)
  "Asynchronously end poll for MESSAGE-ID in CHANNEL-ID."
  (disco-api--request-async
   "POST"
   (format "/channels/%s/polls/%s/expire" channel-id message-id)
   :on-success on-success
   :on-error on-error))

(defun disco-api-send-message (channel-id content
                                          &optional reply-to-message-id poll
                                          allowed-mentions)
  "Send CONTENT into CHANNEL-ID.

When REPLY-TO-MESSAGE-ID is non-nil, send as a reply to that message.
When POLL is non-nil, include poll create payload.
When ALLOWED-MENTIONS is non-nil, send explicit allowed_mentions payload."
  (disco-api-create-message
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :allowed-mentions allowed-mentions
   :poll poll))

(cl-defun disco-api-send-message-async (channel-id content
                                                   &key reply-to-message-id
                                                   message-reference
                                                   allowed-mentions
                                                   poll
                                                   on-success on-error)
  "Send CONTENT into CHANNEL-ID asynchronously.

MESSAGE-REFERENCE may be used for explicit reply/forward references.
When POLL is non-nil, include poll create payload."
  (disco-api-create-message-async
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :message-reference message-reference
   :allowed-mentions allowed-mentions
   :poll poll
   :on-success on-success
   :on-error on-error))

(cl-defun disco-api-forward-message (channel-id source-message-id source-channel-id
                                                &key content forward-only allowed-mentions)
  "Forward SOURCE-MESSAGE-ID from SOURCE-CHANNEL-ID into CHANNEL-ID.

CONTENT is optional text sent alongside the forward.
FORWARD-ONLY optionally selects attachments/embeds to include.
ALLOWED-MENTIONS controls mention parsing for optional CONTENT."
  (let ((message-reference
         (append
          (list `(type . 1)
                `(message_id . ,source-message-id)
                `(channel_id . ,source-channel-id))
          (when forward-only
            (list `(forward_only . ,forward-only))))))
    (disco-api-create-message
     channel-id
     :content content
     :message-reference message-reference
     :allowed-mentions allowed-mentions)))

(cl-defun disco-api-forward-message-async (channel-id source-message-id source-channel-id
                                                      &key content forward-only allowed-mentions
                                                      on-success on-error)
  "Asynchronously forward SOURCE-MESSAGE-ID from SOURCE-CHANNEL-ID to CHANNEL-ID."
  (let ((message-reference
         (append
          (list `(type . 1)
                `(message_id . ,source-message-id)
                `(channel_id . ,source-channel-id))
          (when forward-only
            (list `(forward_only . ,forward-only))))))
    (disco-api-create-message-async
     channel-id
     :content content
     :message-reference message-reference
     :allowed-mentions allowed-mentions
     :on-success on-success
     :on-error on-error)))

(defun disco-api-edit-message (channel-id message-id content &optional allowed-mentions)
  "Edit MESSAGE-ID in CHANNEL-ID with new CONTENT.

When ALLOWED-MENTIONS is non-nil, include it in the edit payload."
  (disco-api--request
   "PATCH"
   (format "/channels/%s/messages/%s" channel-id message-id)
   (disco-api--message-edit-payload content allowed-mentions)
   nil
   nil))

(cl-defun disco-api-edit-message-async (channel-id message-id content &key allowed-mentions on-success on-error)
  "Edit MESSAGE-ID in CHANNEL-ID asynchronously with new CONTENT."
  (disco-api--request-async
   "PATCH"
   (format "/channels/%s/messages/%s" channel-id message-id)
   :payload (disco-api--message-edit-payload content allowed-mentions)
   :on-success on-success
   :on-error on-error))

(defun disco-api-delete-message (channel-id message-id)
  "Delete MESSAGE-ID from CHANNEL-ID."
  (disco-api--request
   "DELETE"
   (format "/channels/%s/messages/%s" channel-id message-id)
   nil
   nil
   nil))

(cl-defun disco-api-delete-message-async (channel-id message-id &key on-success on-error)
  "Delete MESSAGE-ID from CHANNEL-ID asynchronously."
  (disco-api--request-async
   "DELETE"
   (format "/channels/%s/messages/%s" channel-id message-id)
   :on-success on-success
   :on-error on-error))

(provide 'disco-api)

;;; disco-api.el ends here
