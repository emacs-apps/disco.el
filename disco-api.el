;;; disco-api.el --- Discord REST API wrapper for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Synchronous HTTP wrapper for MVP workflows:
;; current user, guild list, guild channels, channel messages, send message.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'disco-customize)

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

(defun disco-api--extract-retry-after ()
  "Extract Retry-After header from current response buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Retry-After: \\([0-9.]+\\)$" nil t)
      (match-string 1))))

(defun disco-api--request (method endpoint &optional payload query unauthenticated)
  "Execute METHOD request to ENDPOINT.

PAYLOAD is an alist encoded as JSON for request body.
QUERY is an alist for query parameters.
If UNAUTHENTICATED is non-nil, omit Authorization header."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (append
           `(("Content-Type" . "application/json")
             ("Accept" . "application/json")
             ("User-Agent" . ,disco-user-agent)
             ("X-Discord-Locale" . ,disco-locale)
             ("Accept-Language" . ,disco-locale))
           (unless unauthenticated
             `(("Authorization" . ,(disco-api--auth-header))))))
         (url-request-data (when payload (disco-api--json-encode payload)))
         (url (disco-api--build-url endpoint query))
         (buffer (url-retrieve-synchronously url t t disco-http-timeout)))
    (unless buffer
      (signal 'disco-api-error (list "request failed: empty response" method endpoint)))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
              (signal 'disco-api-error (list "request failed: invalid HTTP response" method endpoint)))
            (let ((status (string-to-number (match-string 1))))
              (goto-char (point-min))
              (re-search-forward "^$" nil 'move)
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'symbol)
                     (json-false :false)
                     (body
                      (condition-case _
                          (json-read)
                        (error nil))))
                (cond
                 ((and (>= status 200) (< status 300))
                  body)
                 ((= status 429)
                  (signal
                   'disco-api-error
                   (list (format "rate limited (429), retry-after=%s"
                                 (or (disco-api--extract-retry-after)
                                     (and (listp body) (alist-get 'retry_after body))
                                     "unknown"))
                         status
                         body)))
                 (t
                  (signal
                   'disco-api-error
                   (list (format "HTTP %d %s"
                                 status
                                 (or (and (listp body) (alist-get 'message body))
                                     "request failed"))
                         status
                         body)))))))
        (kill-buffer buffer)))))

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
