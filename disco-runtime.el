;;; disco-runtime.el --- Appkit session ownership for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Own the one default Discord application session used by disco buffers.
;; Business state remains in `disco-state'; Appkit owns the session lifecycle.

;;; Code:

(require 'appkit-app)
(require 'appkit-command)

(declare-function disco-gateway-stop "disco-gateway")

(defun disco-runtime--shutdown (_app)
  "Stop Discord transport resources owned by the default app session."
  (when (fboundp 'disco-gateway-stop)
    (disco-gateway-stop)))

(defun disco-runtime--init (_context _input)
  "Initialize Disco's lifecycle-only App model."
  (appkit-next :model 'running :render appkit-render-none))

(defun disco-runtime--update (_context _model message)
  "Reject MESSAGE because this App owns lifecycle, not Disco domain state."
  (appkit-next-reject (list 'disco-lifecycle-app-has-no-domain-messages message)))

(defconst disco-runtime--app-type
  (appkit-app-type-create
   :name 'disco
   :init #'disco-runtime--init
   :update #'disco-runtime--update
   :shutdown #'disco-runtime--shutdown)
  "Canonical App type for Disco's default session.")

(defvar disco-runtime--app nil
  "Default live appkit session for disco.el.")

(defun disco-runtime-app ()
  "Return disco.el's live default Appkit session."
  (unless (appkit-app-live-p disco-runtime--app)
    (setq disco-runtime--app
          (appkit-app-start
           disco-runtime--app-type :identity 'default)))
  disco-runtime--app)

(defun disco-runtime-stop ()
  "Stop and forget disco.el's default appkit session."
  (when (appkit-app-p disco-runtime--app)
    (appkit-app-close disco-runtime--app))
  (setq disco-runtime--app nil))

(provide 'disco-runtime)

;;; disco-runtime.el ends here
