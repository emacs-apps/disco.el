;;; disco-api.el --- Discord REST API wrapper for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Synchronous HTTP wrapper for MVP workflows:
;; current user, guild list, guild channels, channel messages, send message.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'disco-customize)
(require 'disco-http)

(define-error 'disco-api-error "Disco API error")

(defun disco-api--auth-header ()
  "Return Authorization header value from `disco-token'."
  (unless (and disco-token (not (string-empty-p disco-token)))
    (user-error "disco: token is not set; run M-x disco-set-token"))
  disco-token)

(defun disco-api--build-url (endpoint &optional query)
  "Build full URL using ENDPOINT and optional QUERY alist."
  (concat
   (replace-regexp-in-string "/$" "" disco-api-base-url)
   endpoint
   (if (and query (consp query))
       (concat "?" (url-build-query-string query))
     "")))

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

(defun disco-api--extract-retry-after (headers body)
  "Extract retry-after from HEADERS or BODY.

HEADERS is lower-case alist produced by `disco-http-request'."
  (or (cdr (assoc "retry-after" headers))
      (and (listp body) (alist-get 'retry_after body))))

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
         (response (disco-http-request :method method :url url :headers headers :body data :timeout disco-http-timeout))
         (status (or (plist-get response :status) 0))
         (raw-body (or (plist-get response :body) ""))
         (response-headers (or (plist-get response :headers) nil))
         (body (disco-api--decode-json raw-body)))
    (cond
     ((and (>= status 200) (< status 300))
      body)
     ((= status 429)
      (signal
       'disco-api-error
       (list (format "rate limited (429), retry-after=%s"
                     (or (disco-api--extract-retry-after response-headers body)
                         "unknown"))
             status
             body)))
     (t
      (signal
       'disco-api-error
       (list (format "HTTP %d %s"
                     status
                     (or (and (listp body) (alist-get 'message body))
                         (and (not (string-empty-p raw-body)) raw-body)
                         "request failed"))
             status
             body))))))

(defun disco-api-current-user ()
  "Fetch current user object."
  (disco-api--request "GET" "/users/@me" nil nil nil))

(defun disco-api-user-guilds ()
  "Fetch current user's guilds list."
  (disco-api--request "GET" "/users/@me/guilds" nil '(("limit" . "200")) nil))

(defun disco-api-guild-channels (guild-id)
  "Fetch channels in GUILD-ID."
  (disco-api--request "GET" (format "/guilds/%s/channels" guild-id) nil nil nil))

(defun disco-api-channel-messages (channel-id &optional before limit)
  "Fetch messages in CHANNEL-ID.

If BEFORE is non-nil, paginate before that message id.
LIMIT defaults to `disco-message-fetch-limit'."
  (let ((query `(("limit" . ,(number-to-string (or limit disco-message-fetch-limit))))))
    (when before
      (setq query (append query `(("before" . ,before)))))
    (disco-api--request "GET" (format "/channels/%s/messages" channel-id) nil query nil)))

(defun disco-api-send-message (channel-id content)
  "Send CONTENT into CHANNEL-ID."
  (disco-api--request
   "POST"
   (format "/channels/%s/messages" channel-id)
   `((content . ,content))
   nil
   nil))

(provide 'disco-api)

;;; disco-api.el ends here
