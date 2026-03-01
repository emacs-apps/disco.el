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

(defun disco-api--json-true-p (value)
  "Return non-nil if VALUE semantically represents JSON true."
  (or (eq value t)
      (equal value "true")
      (eq value 'true)))

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

(defun disco-api--query-bool-string (value)
  "Return VALUE as API query boolean string.

JSON-like true values map to the true string and everything else maps to
the false string."
  (if (disco-api--json-true-p value) "true" "false"))

(defun disco-api--thread-search-tag-setting-value (value)
  "Normalize thread search tag setting VALUE to API representation."
  (pcase value
    ((or 'match-some 'match_some) "match_some")
    ((or 'match-all 'match_all) "match_all")
    ((pred stringp) value)
    (_ nil)))

(defun disco-api--thread-search-sort-by-value (value)
  "Normalize thread search sort-by VALUE to API representation."
  (pcase value
    ((or 'last-message-time 'last_message_time) "last_message_time")
    ((or 'archive-time 'archive_time) "archive_time")
    ('relevance "relevance")
    ((or 'creation-time 'creation_time) "creation_time")
    ((pred stringp) value)
    (_ nil)))

(defun disco-api--thread-search-sort-order-value (value)
  "Normalize thread search sort-order VALUE to API representation."
  (pcase value
    ((or 'asc "asc") "asc")
    ((or 'desc "desc") "desc")
    (_ nil)))

(cl-defun disco-api--thread-search-query (&key name slop tags tag-setting archived
                                               sort-by sort-order limit offset
                                               max-id min-id)
  "Build query alist for `/channels/{channel.id}/threads/search'."
  (let ((query nil))
    (when (and (stringp name) (not (string-empty-p name)))
      (push `("name" . ,name) query))
    (when (numberp slop)
      (push `("slop" . ,(number-to-string (max 0 (min 100 slop)))) query))
    (when (listp tags)
      (dolist (tag tags)
        (when tag
          (push `("tag" . ,(format "%s" tag)) query))))
    (let ((tag-setting-value (disco-api--thread-search-tag-setting-value tag-setting)))
      (when tag-setting-value
        (push `("tag_setting" . ,tag-setting-value) query)))
    (when (not (null archived))
      (push `("archived" . ,(disco-api--query-bool-string archived)) query))
    (let ((sort-by-value (disco-api--thread-search-sort-by-value sort-by)))
      (when sort-by-value
        (push `("sort_by" . ,sort-by-value) query)))
    (let ((sort-order-value (disco-api--thread-search-sort-order-value sort-order)))
      (when sort-order-value
        (push `("sort_order" . ,sort-order-value) query)))
    (when (numberp limit)
      ;; Discord thread search endpoint accepts 1-25.
      (push `("limit" . ,(number-to-string (max 1 (min 25 limit)))) query))
    (when (numberp offset)
      ;; Discord thread search endpoint accepts 0-9975.
      (push `("offset" . ,(number-to-string (max 0 (min 9975 offset)))) query))
    (when max-id
      (push `("max_id" . ,(format "%s" max-id)) query))
    (when min-id
      (push `("min_id" . ,(format "%s" min-id)) query))
    (nreverse query)))

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

(defun disco-api--thread-archive-query (before limit)
  "Build query alist for thread archive endpoints."
  (let* ((raw-limit (or limit 50))
         ;; Discord archived thread endpoints accept 2-100.
         (normalized-limit (max 2 (min 100 raw-limit)))
         (query `(("limit" . ,(number-to-string normalized-limit)))))
    (when before
      (setq query (append query `(("before" . ,before)))))
    query))

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

(cl-defun disco-api--thread-members-query (&key with-member after limit)
  "Build query alist for thread member listing endpoints."
  (let (query)
    (when (not (null with-member))
      (push `("with_member" . ,(disco-api--query-bool-string with-member)) query))
    (when after
      (push `("after" . ,(format "%s" after)) query))
    (when (numberp limit)
      ;; Discord thread member listing endpoint accepts 1-100.
      (push `("limit" . ,(number-to-string (max 1 (min 100 limit)))) query))
    (nreverse query)))

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

(cl-defun disco-api--thread-update-payload (&key name archived locked auto-archive-duration
                                                 rate-limit-per-user invitable applied-tags)
  "Build thread channel PATCH payload from keyword arguments."
  (let (payload)
    (when (and (stringp name) (not (string-empty-p name)))
      (push `(name . ,name) payload))
    (when (not (null archived))
      (push `(archived . ,(if (disco-api--json-true-p archived) t :false)) payload))
    (when (not (null locked))
      (push `(locked . ,(if (disco-api--json-true-p locked) t :false)) payload))
    (when auto-archive-duration
      (push `(auto_archive_duration . ,auto-archive-duration) payload))
    (when (not (null rate-limit-per-user))
      (push `(rate_limit_per_user . ,rate-limit-per-user) payload))
    (when (not (null invitable))
      (push `(invitable . ,(if (disco-api--json-true-p invitable) t :false)) payload))
    (when (listp applied-tags)
      (push `(applied_tags . ,applied-tags) payload))
    (nreverse payload)))

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

(defun disco-api-ack-message (channel-id message-id
                                         &optional token manual mention-count
                                         flags last-viewed)
  "Acknowledge MESSAGE-ID in CHANNEL-ID.

TOKEN is optional read-state ack token from prior responses.
When MANUAL is non-nil, send manual mode to set read cursor explicitly.
MENTION-COUNT implies MANUAL mode and is forwarded as mention_count.
FLAGS and LAST-VIEWED are forwarded when non-nil.

Response may include a refreshed ack token."
  (let* ((manual-value (or manual (not (null mention-count))))
         payload)
    (when token
      (push `(token . ,token) payload))
    (when manual-value
      (push '(manual . t) payload))
    (when mention-count
      (push `(mention_count . ,mention-count) payload))
    (when flags
      (push `(flags . ,flags) payload))
    (when last-viewed
      (push `(last_viewed . ,last-viewed) payload))
    (disco-api--request
     "POST"
     (format "/channels/%s/messages/%s/ack" channel-id message-id)
     (let ((body (nreverse payload)))
       (if body body :empty-object))
     nil
     nil)))

(cl-defun disco-api-ack-message-async (channel-id message-id
                                                  &key token manual mention-count
                                                  flags last-viewed on-success on-error)
  "Acknowledge MESSAGE-ID in CHANNEL-ID asynchronously."
  (let* ((manual-value (or manual (not (null mention-count))))
         payload)
    (when token
      (push `(token . ,token) payload))
    (when manual-value
      (push '(manual . t) payload))
    (when mention-count
      (push `(mention_count . ,mention-count) payload))
    (when flags
      (push `(flags . ,flags) payload))
    (when last-viewed
      (push `(last_viewed . ,last-viewed) payload))
    (disco-api--request-async
     "POST"
     (format "/channels/%s/messages/%s/ack" channel-id message-id)
     :payload (let ((body (nreverse payload)))
                (if body body :empty-object))
     :on-success on-success
     :on-error on-error)))

(defun disco-api--guess-content-type (filename)
  "Best-effort MIME type for FILENAME."
  (let ((ext (downcase (or (file-name-extension (or filename "")) ""))))
    (cond
     ((member ext '("png")) "image/png")
     ((member ext '("jpg" "jpeg")) "image/jpeg")
     ((member ext '("gif")) "image/gif")
     ((member ext '("webp")) "image/webp")
     ((member ext '("bmp")) "image/bmp")
     ((member ext '("svg")) "image/svg+xml")
     ((member ext '("mp4")) "video/mp4")
     ((member ext '("mov")) "video/quicktime")
     ((member ext '("webm")) "video/webm")
     ((member ext '("mkv")) "video/x-matroska")
     ((member ext '("mp3")) "audio/mpeg")
     ((member ext '("wav")) "audio/wav")
     ((member ext '("ogg")) "audio/ogg")
     ((member ext '("pdf")) "application/pdf")
     ((member ext '("zip")) "application/zip")
     ((member ext '("json")) "application/json")
     ((member ext '("txt" "md" "log")) "text/plain")
     (t "application/octet-stream"))))

(defun disco-api--read-file-bytes (path)
  "Return file contents of PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun disco-api--multipart-boundary ()
  "Generate multipart boundary token for one request."
  (format "----disco-%s-%06d"
          (format-time-string "%Y%m%d%H%M%S" (current-time) t)
          (random 1000000)))

(defun disco-api--multipart-write-string (string)
  "Insert STRING into current buffer as UTF-8 unibyte bytes."
  (insert (encode-coding-string (or string "") 'utf-8 t)))

(defun disco-api--multipart-write-payload-json (boundary payload)
  "Insert PAYLOAD as multipart payload_json part using BOUNDARY."
  (disco-api--multipart-write-string (format "--%s\r\n" boundary))
  (disco-api--multipart-write-string
   "Content-Disposition: form-data; name=\"payload_json\"\r\n")
  (disco-api--multipart-write-string "Content-Type: application/json\r\n\r\n")
  (disco-api--multipart-write-string (disco-api--json-encode payload))
  (disco-api--multipart-write-string "\r\n"))

(defun disco-api--multipart-write-file (boundary index attachment)
  "Insert one ATTACHMENT part under multipart BOUNDARY with INDEX."
  (let* ((path (plist-get attachment :path))
         (filename (plist-get attachment :filename))
         (content-type (plist-get attachment :content-type))
         (bytes (disco-api--read-file-bytes path)))
    (disco-api--multipart-write-string (format "--%s\r\n" boundary))
    (disco-api--multipart-write-string
     (format
      "Content-Disposition: form-data; name=\"files[%d]\"; filename=\"%s\"\r\n"
      index
      (replace-regexp-in-string "\"" "_" filename)))
    (disco-api--multipart-write-string
     (format "Content-Type: %s\r\n\r\n" content-type))
    (insert bytes)
    (disco-api--multipart-write-string "\r\n")))

(defun disco-api--normalize-send-attachment (attachment)
  "Normalize ATTACHMENT into plist with :path/:filename/:description/:content-type.

ATTACHMENT may be a file path string or a plist containing :path."
  (let* ((path (cond
                ((stringp attachment) attachment)
                ((and (listp attachment) (plist-get attachment :path))
                 (plist-get attachment :path))
                (t nil)))
         (description (and (listp attachment) (plist-get attachment :description)))
         (filename (and (listp attachment) (plist-get attachment :filename)))
         (content-type (and (listp attachment) (plist-get attachment :content-type))))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "disco: attachment must include a file path"))
    (unless (file-readable-p path)
      (user-error "disco: attachment file is not readable: %s" path))
    (let ((resolved-filename (or filename (file-name-nondirectory path))))
      (list :path path
            :filename resolved-filename
            :description (and (stringp description)
                              (not (string-empty-p (string-trim description)))
                              (string-trim description))
            :content-type (or content-type
                              (disco-api--guess-content-type resolved-filename))))))

(defun disco-api--build-message-multipart-body (payload attachments)
  "Build multipart body for message PAYLOAD and ATTACHMENTS.

Return cons cell (BOUNDARY . BODY)."
  (let ((boundary (disco-api--multipart-boundary)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (disco-api--multipart-write-payload-json boundary payload)
      (cl-loop for attachment in attachments
               for idx from 0
               do (disco-api--multipart-write-file boundary idx attachment))
      (disco-api--multipart-write-string (format "--%s--\r\n" boundary))
      (cons boundary (buffer-string)))))

(defun disco-api--normalize-poll-answer-media (answer index)
  "Normalize one poll ANSWER into create-request answer object.

INDEX is used for user-facing validation messages."
  (let* ((source-media
          (cond
           ((stringp answer)
            `((text . ,answer)))
           ((listp answer)
            (or (alist-get 'poll_media answer)
                (alist-get 'poll-media answer)
                (and (assq 'text answer) answer)))
           (t nil)))
         (text (and (listp source-media)
                    (alist-get 'text source-media)))
         (normalized-text (and (stringp text)
                               (string-trim text)))
         (emoji (and (listp source-media)
                     (alist-get 'emoji source-media)))
         (emoji-id (and (listp emoji) (alist-get 'id emoji)))
         (emoji-name (and (listp emoji) (alist-get 'name emoji)))
         (media nil))
    (unless (and (stringp normalized-text)
                 (not (string-empty-p normalized-text)))
      (user-error "disco: poll answer %d text cannot be empty" (1+ index)))
    (push `(text . ,normalized-text) media)
    (when (or emoji-id emoji-name)
      (push `(emoji . ((id . ,emoji-id)
                       (name . ,emoji-name)))
            media))
    `((poll_media . ,(nreverse media)))))

(defun disco-api--normalize-poll-request (poll)
  "Normalize POLL payload into Discord poll create-request object.

When POLL is nil, return nil."
  (when poll
    (unless (listp poll)
      (user-error "disco: poll payload must be an alist"))
    (let* ((raw-question (alist-get 'question poll))
           (question-text
            (cond
             ((stringp raw-question) (string-trim raw-question))
             ((listp raw-question)
              (let ((text (alist-get 'text raw-question)))
                (and (stringp text) (string-trim text))))
             (t nil)))
           (raw-answers (or (alist-get 'answers poll) '()))
           (normalized-answers
            (cl-loop for answer in raw-answers
                     for idx from 0
                     collect (disco-api--normalize-poll-answer-media answer idx)))
           (duration (alist-get 'duration poll))
           (layout-type (alist-get 'layout_type poll))
           (allow-multiselect-pair
            (or (assq 'allow_multiselect poll)
                (assq 'allow-multiselect poll)))
           payload)
      (unless (and (stringp question-text)
                   (not (string-empty-p question-text)))
        (user-error "disco: poll question cannot be empty"))
      (when (or (< (length normalized-answers) 2)
                (> (length normalized-answers) 10))
        (user-error "disco: poll must have between 2 and 10 answers"))
      (push `(question . ((text . ,question-text))) payload)
      (push `(answers . ,normalized-answers) payload)
      (when duration
        (unless (and (numberp duration)
                     (>= duration 1)
                     (<= duration (* 32 24)))
          (user-error "disco: poll duration must be 1..768 hours"))
        (push `(duration . ,(truncate duration)) payload))
      (when allow-multiselect-pair
        (push `(allow_multiselect . ,(if (disco-api--json-true-p (cdr allow-multiselect-pair))
                                         t
                                       :false))
              payload))
      (when layout-type
        (unless (integerp layout-type)
          (user-error "disco: poll layout_type must be an integer"))
        (push `(layout_type . ,layout-type) payload))
      (nreverse payload))))

(defun disco-api--message-send-payload (content reply-to-message-id attachments poll)
  "Build message create payload.

CONTENT is optional message text, REPLY-TO-MESSAGE-ID optional reply target,
ATTACHMENTS is normalized attachment plist list, POLL is optional poll object."
  (let (payload)
    (when (and (stringp content)
               (not (string-empty-p (string-trim-right content))))
      (push `(content . ,(string-trim-right content)) payload))
    (when reply-to-message-id
      (push `(message_reference . ((message_id . ,reply-to-message-id))) payload))
    (when attachments
      (let ((attachment-objects nil))
        (cl-loop for attachment in attachments
                 for idx from 0
                 do (let ((entry `((id . ,idx)
                                   (filename . ,(plist-get attachment :filename)))))
                      (let ((description (plist-get attachment :description)))
                        (when description
                          (setq entry (append entry `((description . ,description))))))
                      (push entry attachment-objects)))
        (push `(attachments . ,(nreverse attachment-objects)) payload)))
    (when poll
      (push `(poll . ,poll) payload))
    (nreverse payload)))

(cl-defun disco-api-create-message (channel-id &key content reply-to-message-id attachments poll)
  "Create one message in CHANNEL-ID.

CONTENT is optional text content. REPLY-TO-MESSAGE-ID adds message_reference.
ATTACHMENTS is an optional list of upload descriptors.
POLL is an optional poll create payload."
  (let* ((normalized-attachments
          (mapcar #'disco-api--normalize-send-attachment (or attachments '())))
         (normalized-poll (disco-api--normalize-poll-request poll))
         (payload (disco-api--message-send-payload
                   content
                   reply-to-message-id
                   normalized-attachments
                   normalized-poll)))
    (unless (or (alist-get 'content payload)
                normalized-attachments
                normalized-poll)
      (user-error "disco: message content, poll, and attachments are all empty"))
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

(cl-defun disco-api-create-message-async (channel-id &key content reply-to-message-id attachments poll on-success on-error)
  "Asynchronously create one message in CHANNEL-ID.

Keyword arguments are the same as `disco-api-create-message'."
  (let* ((normalized-attachments
          (mapcar #'disco-api--normalize-send-attachment (or attachments '())))
         (normalized-poll (disco-api--normalize-poll-request poll))
         (payload (disco-api--message-send-payload
                   content
                   reply-to-message-id
                   normalized-attachments
                   normalized-poll)))
    (unless (or (alist-get 'content payload)
                normalized-attachments
                normalized-poll)
      (user-error "disco: message content, poll, and attachments are all empty"))
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

(cl-defun disco-api-send-message-with-attachments (channel-id &key content reply-to-message-id attachments)
  "Send message to CHANNEL-ID with ATTACHMENTS.

ATTACHMENTS is a list of file path strings or plists containing :path and
optional :description/:filename/:content-type."
  (disco-api-create-message
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :attachments attachments))

(cl-defun disco-api-send-message-with-attachments-async (channel-id &key content reply-to-message-id attachments on-success on-error)
  "Asynchronously send message to CHANNEL-ID with ATTACHMENTS.

ATTACHMENTS is a list of file path strings or plists containing :path and
optional :description/:filename/:content-type."
  (disco-api-create-message-async
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :attachments attachments
   :on-success on-success
   :on-error on-error))

(defun disco-api--normalize-reaction-emoji (emoji)
  "Normalize user-provided EMOJI string for Discord reaction endpoints."
  (let* ((raw (or emoji ""))
         (trimmed (string-trim raw))
         (custom
          (cond
           ((string-match "^<a?:\\([^:>]+\\):\\([0-9]+\\)>$" trimmed)
            (format "%s:%s" (match-string 1 trimmed) (match-string 2 trimmed)))
           ((string-match "^[^:]+:[0-9]+$" trimmed)
            trimmed)
           (t trimmed))))
    (unless (and (stringp custom) (not (string-empty-p custom)))
      (user-error "disco: emoji cannot be empty"))
    custom))

(defun disco-api--encode-reaction-emoji (emoji)
  "Return URL path component for reaction EMOJI."
  (url-hexify-string (disco-api--normalize-reaction-emoji emoji)))

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

(defun disco-api--normalize-poll-answer-id (answer-id)
  "Normalize poll ANSWER-ID to an integer."
  (let ((value
         (cond
          ((integerp answer-id) answer-id)
          ((and (stringp answer-id)
                (string-match-p "\\`[0-9]+\\'" answer-id))
           (string-to-number answer-id))
          (t nil))))
    (unless (and (integerp value) (> value 0))
      (user-error "disco: poll answer id must be a positive integer"))
    value))

(defun disco-api--normalize-poll-answer-ids (answer-ids)
  "Normalize ANSWER-IDS into a deduped integer list."
  (let* ((source
          (cond
           ((null answer-ids) '())
           ((vectorp answer-ids) (append answer-ids nil))
           ((listp answer-ids) answer-ids)
           (t (list answer-ids))))
         (normalized (mapcar #'disco-api--normalize-poll-answer-id source)))
    (delete-dups normalized)))

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

(defun disco-api-send-message (channel-id content &optional reply-to-message-id poll)
  "Send CONTENT into CHANNEL-ID.

When REPLY-TO-MESSAGE-ID is non-nil, send as a reply to that message.
When POLL is non-nil, include poll create payload."
  (disco-api-create-message
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :poll poll))

(cl-defun disco-api-send-message-async (channel-id content
                                                   &key reply-to-message-id
                                                   poll
                                                   on-success on-error)
  "Send CONTENT into CHANNEL-ID asynchronously.

When POLL is non-nil, include poll create payload."
  (disco-api-create-message-async
   channel-id
   :content content
   :reply-to-message-id reply-to-message-id
   :poll poll
   :on-success on-success
   :on-error on-error))

(defun disco-api-edit-message (channel-id message-id content)
  "Edit MESSAGE-ID in CHANNEL-ID with new CONTENT."
  (disco-api--request
   "PATCH"
   (format "/channels/%s/messages/%s" channel-id message-id)
   `((content . ,content))
   nil
   nil))

(cl-defun disco-api-edit-message-async (channel-id message-id content &key on-success on-error)
  "Edit MESSAGE-ID in CHANNEL-ID asynchronously with new CONTENT."
  (disco-api--request-async
   "PATCH"
   (format "/channels/%s/messages/%s" channel-id message-id)
   :payload `((content . ,content))
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
