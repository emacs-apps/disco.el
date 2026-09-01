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

(defcustom disco-http-queue-limit 4
  "Maximum number of concurrent requests in disco's plz queue.

Value 1 enforces strict serialization. Higher values improve startup
hydration throughput while still honoring rate-limit backoff."
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

(defvar disco-http--generation 0
  "Generation revoking callbacks from an earlier account session.")

(defvar disco-http--reset-in-progress nil
  "Non-nil while account-owned HTTP work is being destructively reset.")

(defvar disco-http--direct-request-owners nil
  "Exact owners for asynchronous requests made outside the shared queue.")

(defun disco-http--assert-session-available ()
  "Reject new HTTP work while destructive session reset is active."
  (when disco-http--reset-in-progress
    (error "Disco HTTP session is unavailable during reset")))

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

(defun disco-http--request-plz (method url headers body timeout &optional body-type)
  "Execute one synchronous HTTP request with plz."
  (condition-case err
      (disco-http--response->plist
       (plz (disco-http--method-symbol method) url
         :headers headers
         :body body
         :body-type (or body-type 'text)
         :as 'response
         :then 'sync
         :timeout timeout
         :connect-timeout timeout))
    (error
     (disco-http--error->plist err))))

(defun disco-http--ensure-queue ()
  "Return active plz queue, creating or reinitializing when needed."
  (disco-http--assert-session-available)
  (when (or (null disco-http--plz-queue)
            (not (equal disco-http--plz-queue-limit disco-http-queue-limit)))
    (setq disco-http--plz-queue (make-plz-queue :limit (max 1 disco-http-queue-limit)))
    (setq disco-http--plz-queue-limit (max 1 disco-http-queue-limit)))
  disco-http--plz-queue)

(defun disco-http--request-plz-queued (method url headers body timeout &optional body-type)
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
      :body-type (or body-type 'text)
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

(defun disco-http--call-callback (callback payload)
  "Invoke CALLBACK with PAYLOAD and guard callback-side errors."
  (when callback
    (condition-case err
        (funcall callback payload)
      (error
       (message "disco-http: callback failed: %s"
                (error-message-string err))))))

(defun disco-http--direct-owner-current-p (owner)
  "Return non-nil when direct request OWNER belongs to this session."
  (and (not disco-http--reset-in-progress)
       (= (plist-get owner :generation) disco-http--generation)
       (memq owner disco-http--direct-request-owners)))

(defun disco-http--retire-direct-owner (owner)
  "Forget exact direct request OWNER."
  (setq disco-http--direct-request-owners
        (delq owner disco-http--direct-request-owners)))

(defun disco-http--cancel-direct-process (process)
  "Cancel direct plz PROCESS without publishing lifecycle errors."
  (when (and (processp process) (process-live-p process))
    (condition-case nil
        (delete-process process)
      ((error quit) nil))))

(defun disco-http--request-plz-async (method url headers body timeout on-success on-error &optional body-type)
  "Execute one asynchronous HTTP request with plz."
  (let ((owner (list :generation disco-http--generation :process nil)))
    ;; Reserve exact ownership before `plz': a test double or process sentinel
    ;; may synchronously reset the account before the constructor returns.
    (push owner disco-http--direct-request-owners)
    (condition-case err
        (let ((process
               (plz (disco-http--method-symbol method) url
                 :headers headers
                 :body body
                 :body-type (or body-type 'text)
                 :as 'response
                 :timeout timeout
                 :connect-timeout timeout
                 :then
                 (lambda (response)
                   (when (disco-http--direct-owner-current-p owner)
                     (disco-http--retire-direct-owner owner)
                     (disco-http--call-callback
                      on-success
                      (disco-http--response->plist response))))
                 :else
                 (lambda (request-error)
                   (when (disco-http--direct-owner-current-p owner)
                     (disco-http--retire-direct-owner owner)
                     (disco-http--call-callback
                      on-error
                      (disco-http--error->plist request-error)))))))
          (if (disco-http--direct-owner-current-p owner)
              (setf (plist-get owner :process) process)
            (disco-http--cancel-direct-process process))
          process)
      (error
       (when (disco-http--direct-owner-current-p owner)
         (disco-http--retire-direct-owner owner)
         (disco-http--call-callback
          on-error (disco-http--error->plist err)))))))

(defun disco-http--request-plz-queued-async (method url headers body timeout on-success on-error &optional body-type)
  "Execute one asynchronous request via the shared plz queue."
  (let ((queue (disco-http--ensure-queue))
        (generation disco-http--generation))
    (plz-queue
      queue
      (disco-http--method-symbol method)
      url
      :headers headers
      :body body
      :body-type (or body-type 'text)
      :as 'response
      :timeout timeout
      :connect-timeout timeout
      :then (lambda (response)
              (when (and (not disco-http--reset-in-progress)
                         (= generation disco-http--generation)
                         (eq queue disco-http--plz-queue))
                (disco-http--call-callback
                 on-success
                 (disco-http--response->plist response))))
      :else (lambda (request-error)
              (when (and (not disco-http--reset-in-progress)
                         (= generation disco-http--generation)
                         (eq queue disco-http--plz-queue))
                (disco-http--call-callback
                 on-error
                 (disco-http--error->plist request-error)))))
    (plz-run queue)))

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

(defun disco-http--clear-queue-state ()
  "Forget HTTP queue bookkeeping without invoking queue cancellation."
  (setq disco-http--plz-queue nil
        disco-http--plz-queue-limit nil
        disco-http--direct-request-owners nil))

(defun disco-http-reset-queue-state ()
  "Revoke and cancel HTTP work owned by the retired account session."
  (let ((queue disco-http--plz-queue)
        (owners disco-http--direct-request-owners)
        (disco-http--reset-in-progress t))
    ;; Revoke exact ownership before cancellation because `plz-clear' invokes
    ;; pending error callbacks synchronously.
    (cl-incf disco-http--generation)
    (disco-http--clear-queue-state)
    (unwind-protect
        (progn
          (when queue
            (condition-case nil
                (plz-clear queue)
              ((error quit) nil)))
          (dolist (owner owners)
            (disco-http--cancel-direct-process
             (plist-get owner :process))))
      (disco-http--clear-queue-state))))

(cl-defun disco-http-request (&key method url headers body timeout body-type)
  "Execute HTTP request and return plist with :status :body :headers.

METHOD is uppercase string (for example: GET)."
  (disco-http--assert-session-available)
  (if (not disco-http-serialize-requests)
      (disco-http--request-plz method url headers body timeout body-type)
    (disco-http--request-plz-queued method url headers body timeout body-type)))

(cl-defun disco-http-request-async (&key method url headers body timeout on-success on-error body-type)
  "Execute HTTP request asynchronously and invoke callbacks.

ON-SUCCESS and ON-ERROR are called with a normalized response plist
containing keys `:status', `:body', and `:headers'."
  (disco-http--assert-session-available)
  (if (not disco-http-serialize-requests)
      (disco-http--request-plz-async method url headers body timeout on-success on-error body-type)
    (disco-http--request-plz-queued-async method url headers body timeout on-success on-error body-type)))

(provide 'disco-http)

;;; disco-http.el ends here
