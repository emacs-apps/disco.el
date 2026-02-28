;;; disco-http.el --- HTTP transport for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; This module provides a synchronous HTTP transport wrapper built on plz.
;;
;; Return value format is a plist with:
;; - :status  integer HTTP status (0 for transport-level failures)
;; - :body    response body string
;; - :headers header alist, key as lower-cased symbols
;; - :error   raw Lisp error object (only on transport failures)

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'subr-x)

(defgroup disco-http nil
  "HTTP transport options for disco.el."
  :group 'disco)

(defcustom disco-http-serialize-requests t
  "If non-nil, execute HTTP requests through a serialized in-process queue.

This avoids concurrent burst traffic from timers and interactive commands."
  :type 'boolean
  :group 'disco-http)

(defcustom disco-http-queue-limit 1
  "Maximum number of concurrent requests in disco's plz queue.

Value 1 enforces strict serialization."
  :type 'integer
  :group 'disco-http)

(defcustom disco-http-queue-poll-interval 0.02
  "Seconds between queue turn checks while waiting."
  :type 'number
  :group 'disco-http)

(defcustom disco-http-queue-wait-timeout 120
  "Max seconds a request waits in local queue before signaling timeout."
  :type 'integer
  :group 'disco-http)

(defvar disco-http--plz-queue nil
  "Shared plz queue used when request serialization is enabled.")

(defvar disco-http--plz-queue-limit nil
  "Last applied `disco-http-queue-limit' for queue reinitialization.")

(defun disco-http--method-symbol (method)
  "Convert METHOD string into plz method symbol."
  (intern (downcase method)))

(defun disco-http--normalize-headers (headers)
  "Normalize HEADERS alist to lower-cased symbol keys."
  (mapcar
   (lambda (header)
     (let ((key (car header))
           (value (cdr header)))
       (cons (if (symbolp key)
                 (intern (downcase (symbol-name key)))
               key)
             value)))
   (or headers '())))

(defun disco-http--response->plist (response)
  "Convert plz RESPONSE into disco transport plist."
  (list :status (or (plz-response-status response) 0)
        :body (or (plz-response-body response) "")
        :headers (disco-http--normalize-headers (plz-response-headers response))))

(defun disco-http--extract-plz-error (err)
  "Return plz error object from ERR, or nil.

ERR may be a raw `plz-error' object or a condition-case tuple like
`(plz-http-error PLZ-ERROR)'."
  (cond
   ((and (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p err)))
    err)
   ((and (consp err)
         (symbolp (car err)))
    (let ((payload (cadr err)))
      (when (and (fboundp 'plz-error-p)
                 (ignore-errors (plz-error-p payload)))
        payload)))
   (t nil)))

(defun disco-http--error-message (err plz-err response)
  "Return stable human-readable error message for transport ERR."
  (let* ((plz-message (and plz-err
                           (fboundp 'plz-error-message)
                           (ignore-errors (plz-error-message plz-err))))
         (response-body (and response
                             (ignore-errors (plz-response-body response)))))
    (or plz-message
        (and (stringp response-body)
             (not (string-empty-p response-body))
             response-body)
        (and (consp err)
             (ignore-errors (error-message-string err)))
        (and (stringp err) err)
        (format "%S" err))))

(defun disco-http--error->plist (err)
  "Convert transport ERR into disco transport plist."
  (let* ((plz-err (disco-http--extract-plz-error err))
         (response (and plz-err
                        (fboundp 'plz-error-response)
                        (ignore-errors (plz-error-response plz-err))))
         (status (and response (ignore-errors (plz-response-status response))))
         (body (and response (ignore-errors (plz-response-body response))))
         (headers (and response (ignore-errors (plz-response-headers response)))))
    (list :status (or status 0)
          :body (or body "")
          :headers (disco-http--normalize-headers headers)
          :error err
          :error-message (disco-http--error-message err plz-err response))))

(defun disco-http--request-plz (method url headers body timeout)
  "Execute one synchronous HTTP request with plz."
  (condition-case err
      (disco-http--response->plist
       (plz (disco-http--method-symbol method) url
            :headers headers
            :body body
            :body-type 'text
            :as 'response
            :then 'sync
            :timeout timeout
            :connect-timeout timeout))
    (error
     (disco-http--error->plist err))))

(defun disco-http--ensure-queue ()
  "Return active plz queue, creating or reinitializing when needed."
  (when (or (null disco-http--plz-queue)
            (not (equal disco-http--plz-queue-limit disco-http-queue-limit)))
    (setq disco-http--plz-queue (make-plz-queue :limit (max 1 disco-http-queue-limit)))
    (setq disco-http--plz-queue-limit (max 1 disco-http-queue-limit)))
  disco-http--plz-queue)

(defun disco-http--request-plz-queued (method url headers body timeout)
  "Execute one request using the shared asynchronous plz queue.

This function blocks until request completion to preserve disco's
synchronous API contract."
  (let* ((queue (disco-http--ensure-queue))
         (done nil)
         (result nil)
         (start-time (float-time)))
    (plz-queue
     queue
     (disco-http--method-symbol method)
     url
     :headers headers
     :body body
     :body-type 'text
     :as 'response
     :timeout timeout
     :connect-timeout timeout
     :then (lambda (response)
             (setq result (disco-http--response->plist response)
                   done t))
     :else (lambda (err)
             (setq result (disco-http--error->plist err)
                   done t)))
    (plz-run queue)
    (while (not done)
      (when (> (- (float-time) start-time) disco-http-queue-wait-timeout)
        (setq result (list :status 0
                           :body ""
                           :headers nil
                           :error-message (format "disco: HTTP queue wait timeout after %.1fs"
                                                  disco-http-queue-wait-timeout)))
        (setq done t))
      (accept-process-output nil disco-http-queue-poll-interval))
    result))

(defun disco-http-queue-stats ()
  "Return current queue stats as plist."
  (let* ((queue (and disco-http--plz-queue disco-http--plz-queue))
         (active (if queue (length (plz-queue-active queue)) 0))
         (pending (if queue (length (plz-queue-requests queue)) 0)))
    (list :enabled disco-http-serialize-requests
          :queue-created (and queue t)
          :limit (if queue (plz-queue-limit queue) (max 1 disco-http-queue-limit))
          :active active
          :pending pending
          :outstanding (+ active pending))))

(defun disco-http-describe-queue ()
  "Display queue runtime status in minibuffer."
  (interactive)
  (let* ((stats (disco-http-queue-stats))
         (enabled (plist-get stats :enabled))
         (limit (plist-get stats :limit))
         (active (plist-get stats :active))
         (pending (plist-get stats :pending))
         (outstanding (plist-get stats :outstanding)))
    (message "disco-http queue: enabled=%s limit=%s active=%s pending=%s outstanding=%s"
             enabled limit active pending outstanding)))

(defun disco-http-reset-queue-state ()
  "Reset local HTTP queue bookkeeping state." 
  (when disco-http--plz-queue
    (ignore-errors (plz-clear disco-http--plz-queue)))
  (setq disco-http--plz-queue nil)
  (setq disco-http--plz-queue-limit nil))

(cl-defun disco-http-request (&key method url headers body timeout)
  "Execute HTTP request and return plist with :status :body :headers.

METHOD is uppercase string (for example: GET)."
  (if (not disco-http-serialize-requests)
      (disco-http--request-plz method url headers body timeout)
    (disco-http--request-plz-queued method url headers body timeout)))

(provide 'disco-http)

;;; disco-http.el ends here
