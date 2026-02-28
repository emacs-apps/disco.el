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

(defun disco-api--request (method endpoint &optional payload query unauthenticated)
  "Execute METHOD request to ENDPOINT.

PAYLOAD is an alist encoded as JSON for request body.
QUERY is an alist for query parameters.
If UNAUTHENTICATED is non-nil, omit Authorization header."
  (let* ((headers
          (append
           `(("Content-Type" . "application/json")
             ("Accept" . "application/json")
             ("User-Agent" . ,disco-user-agent)
             ("X-Discord-Locale" . ,disco-locale)
             ("Accept-Language" . ,disco-locale))
           (unless unauthenticated
             `(("Authorization" . ,(disco-api--auth-header))))))
         (data (when payload (disco-api--json-encode payload)))
         (url (disco-api--build-url endpoint query))
         (route-key (disco-api--route-key method endpoint))
         (attempt 0))
    (catch 'disco-api-return
      (while t
        (disco-api--wait-for-rate-limit route-key)
        (let* ((response (disco-http-request :method method :url url :headers headers :body data :timeout disco-http-timeout))
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
             (list (format "HTTP %d %s"
                           status
                           (or (and (listp body) (alist-get 'message body))
                               (and (not (string-empty-p raw-body)) raw-body)
                               "request failed"))
                   status
                   body)))))))))

(defun disco-api-current-user ()
  "Fetch current user object."
  (disco-api--request "GET" "/users/@me" nil nil nil))

(defun disco-api-gateway ()
  "Fetch gateway connection object containing websocket URL."
  (disco-api--request "GET" "/gateway" nil nil t))

(defun disco-api-user-guilds ()
  "Fetch current user's guilds list."
  (disco-api--request "GET" "/users/@me/guilds" nil '(("limit" . "200")) nil))

(defun disco-api-user-private-channels ()
  "Fetch current user's private channels (DM/group DM) list."
  (disco-api--request "GET" "/users/@me/channels" nil nil nil))

(defun disco-api-guild-channels (guild-id)
  "Fetch channels in GUILD-ID."
  (disco-api--request "GET" (format "/guilds/%s/channels" guild-id) nil nil nil))

(defun disco-api-guild-active-threads (guild-id)
  "Fetch active threads object for GUILD-ID.

Response is an alist with keys including `threads' and `members'."
  (disco-api--request "GET" (format "/guilds/%s/threads/active" guild-id) nil nil nil))

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

(defun disco-api-join-thread (thread-id)
  "Join thread THREAD-ID as current user."
  (disco-api--request "PUT" (format "/channels/%s/thread-members/@me" thread-id) nil nil nil))

(defun disco-api-leave-thread (thread-id)
  "Leave thread THREAD-ID as current user."
  (disco-api--request "DELETE" (format "/channels/%s/thread-members/@me" thread-id) nil nil nil))

(defun disco-api-set-thread-archived (thread-id archived &optional locked)
  "Set THREAD-ID archived state to ARCHIVED.

If LOCKED is non-nil, set lock state in the same request."
  (let ((payload `((archived . ,(if archived t :false)))))
    (when (not (null locked))
      (setq payload (append payload `((locked . ,(if locked t :false))))))
    (disco-api--request "PATCH" (format "/channels/%s" thread-id) payload nil nil)))

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
     (nreverse payload)
     nil
     nil)))

(defun disco-api-send-message (channel-id content &optional reply-to-message-id)
  "Send CONTENT into CHANNEL-ID.

When REPLY-TO-MESSAGE-ID is non-nil, send as a reply to that message."
  (let ((payload `((content . ,content))))
    (when reply-to-message-id
      (setq payload
            (append payload
                    `((message_reference . ((message_id . ,reply-to-message-id)))))))
    (disco-api--request
     "POST"
     (format "/channels/%s/messages" channel-id)
     payload
     nil
     nil)))

(defun disco-api-edit-message (channel-id message-id content)
  "Edit MESSAGE-ID in CHANNEL-ID with new CONTENT."
  (disco-api--request
   "PATCH"
   (format "/channels/%s/messages/%s" channel-id message-id)
   `((content . ,content))
   nil
   nil))

(defun disco-api-delete-message (channel-id message-id)
  "Delete MESSAGE-ID from CHANNEL-ID."
  (disco-api--request
   "DELETE"
   (format "/channels/%s/messages/%s" channel-id message-id)
   nil
   nil
   nil))

(provide 'disco-api)

;;; disco-api.el ends here
