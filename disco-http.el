;;; disco-http.el --- HTTP backend abstraction for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; HTTP wrapper built on top of plz/curl.
;; Return shape is stable across requests: plist with
;; :status, :body, :headers and optional :error.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'plz)

(declare-function plz "ext:plz")
(declare-function plz-response-status "ext:plz")
(declare-function plz-response-body "ext:plz")
(declare-function plz-response-p "ext:plz")
(declare-function plz-error-response "ext:plz")

(defun disco-http--method-symbol (method)
  "Convert METHOD string into plz method symbol."
  (intern (downcase method)))

(defun disco-http--request-plz (method url headers body timeout)
  "Execute HTTP request via plz and return normalized plist."
  (let (done result err)
    (plz (disco-http--method-symbol method) url
      :headers headers
      :body body
      :body-type 'text
      :as 'response
      :timeout timeout
      :connect-timeout timeout
      :then (lambda (response)
              (setq result
                    (list :status (plz-response-status response)
                          :body (or (plz-response-body response) "")
                          :headers nil)
                    done t))
      :else (lambda (plz-error)
              (let* ((response (and (fboundp 'plz-error-response)
                                    (ignore-errors (plz-error-response plz-error))))
                     (status (when (and response (plz-response-p response))
                               (plz-response-status response)))
                     (body-text (when (and response (plz-response-p response))
                                  (or (plz-response-body response) ""))))
                (setq err (list :status status :body body-text :raw plz-error)
                      done t))))
    (while (not done)
      (accept-process-output nil 0.05))
    (if err
        ;; Return an HTTP-like result to let higher layer do unified handling.
        (list :status (or (plist-get err :status) 0)
              :body (or (plist-get err :body) "")
              :headers nil
              :error (plist-get err :raw))
      result)))

(cl-defun disco-http-request (&key method url headers body timeout)
  "Execute an HTTP request and return plist with :status :body :headers.

METHOD is uppercase string (for example: GET)."
  (disco-http--request-plz method url headers body timeout))

(provide 'disco-http)

;;; disco-http.el ends here
