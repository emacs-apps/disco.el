;;; disco-http.el --- HTTP backend abstraction for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; HTTP backend abstraction with a stable return shape:
;; plist with :status, :body, :headers.
;;
;; Strategy:
;; - default backend is `url' for zero extra dependency.
;; - `plz' is optional and can be selected explicitly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)

(declare-function plz "ext:plz")
(declare-function plz-response-status "ext:plz")
(declare-function plz-response-body "ext:plz")
(declare-function plz-response-p "ext:plz")
(declare-function plz-error-response "ext:plz")

(defgroup disco-http nil
  "HTTP options for disco.el."
  :group 'disco)

(defcustom disco-http-backend 'url
  "HTTP backend used by disco.el.

`url' uses Emacs built-in URL stack with no extra dependency.
`plz' uses curl-backed requests when `plz' is available.
`auto' uses `plz' if available, otherwise falls back to `url'."
  :type '(choice (const :tag "Built-in url.el" url)
                 (const :tag "plz (optional)" plz)
                 (const :tag "Auto" auto))
  :group 'disco-http)

(defvar disco-http--warned-about-plz nil)

(defun disco-http-effective-backend ()
  "Resolve effective backend from `disco-http-backend'."
  (pcase disco-http-backend
    ('url 'url)
    ('plz (if (require 'plz nil t)
              'plz
            (unless disco-http--warned-about-plz
              (setq disco-http--warned-about-plz t)
              (message "disco: plz not found, falling back to url backend"))
            'url))
    ('auto (if (require 'plz nil t) 'plz 'url))
    (_ 'url)))

(defun disco-http--header-alist-from-buffer ()
  "Parse HTTP headers in current response buffer into an alist.

Header names are lower-cased strings."
  (let (headers)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\([^: \n]+\):[ \t]*\(.*\)$" nil t)
        (push (cons (downcase (match-string 1)) (string-trim (match-string 2))) headers)))
    headers))

(defun disco-http--request-url (method url headers body timeout)
  "Execute HTTP request via url.el backend."
  (let* ((url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data body)
         (buffer (url-retrieve-synchronously url t t timeout)))
    (unless buffer
      (error "disco: empty response from %s" url))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "^HTTP/[0-9.]+ \([0-9]+\)" nil t)
              (error "disco: malformed HTTP response for %s" url))
            (let ((status (string-to-number (match-string 1)))
                  (headers-alist (disco-http--header-alist-from-buffer)))
              (goto-char (point-min))
              (re-search-forward "^$" nil 'move)
              (let ((body-text (buffer-substring-no-properties (point) (point-max))))
                (list :status status :body body-text :headers headers-alist))))
        (kill-buffer buffer)))))

(defun disco-http--method-symbol (method)
  "Convert METHOD string into plz method symbol."
  (intern (downcase method)))

(defun disco-http--request-plz (method url headers body timeout)
  "Execute HTTP request via plz backend and return normalized plist."
  (unless (require 'plz nil t)
    (error "disco: plz backend requested but package is unavailable"))
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
  (pcase (disco-http-effective-backend)
    ('plz (disco-http--request-plz method url headers body timeout))
    (_ (disco-http--request-url method url headers body timeout))))

(provide 'disco-http)

;;; disco-http.el ends here
