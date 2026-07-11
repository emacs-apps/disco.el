;;; disco-runtime.el --- Appkit session ownership for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Own the one default Discord application session used by disco buffers.
;; Business state remains in `disco-state'; appkit owns lifecycle and views.

;;; Code:

(require 'appkit-core)

(declare-function disco-gateway-stop "disco-gateway")

(defun disco-runtime--shutdown (_app)
  "Stop Discord transport resources owned by the default app session."
  (when (fboundp 'disco-gateway-stop)
    (disco-gateway-stop)))

(appkit-define-app-kind disco
  :shutdown #'disco-runtime--shutdown)

(defvar disco-runtime--app nil
  "Default live appkit session for disco.el.")

(defun disco-runtime-app ()
  "Return disco.el's live default appkit session."
  (unless (appkit-app-live-p disco-runtime--app)
    (setq disco-runtime--app
          (appkit-start-app 'disco :id 'default)))
  disco-runtime--app)

(defun disco-runtime-stop ()
  "Stop and forget disco.el's default appkit session."
  (when (appkit-app-p disco-runtime--app)
    (appkit-stop-app disco-runtime--app))
  (setq disco-runtime--app nil))

(provide 'disco-runtime)

;;; disco-runtime.el ends here
