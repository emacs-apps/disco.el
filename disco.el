;;; disco.el --- Discord client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: comm
;; URL: https://github.com/0WD0/disco.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (plz "0.8") (websocket "1.16") (transient "0.3") (taxy-magit-section "0.14.3"))

;;; Commentary:

;; disco.el provides an MVP Discord client experience in Emacs:
;; - open root buffer with guild/channel list
;; - open a channel timeline
;; - send text messages

;;; Code:

(require 'subr-x)
(require 'disco-customize)
(require 'disco-state)
(require 'disco-directory)
(require 'disco-api)
(require 'disco-http)
(require 'disco-gateway)
(require 'disco-room)
(require 'disco-root)
(require 'disco-modes)
(require 'disco-notifications)

;;;###autoload
(defun disco ()
  "Start disco.el and open root buffer.

If neither session token nor `DISCO_TOKEN' is available, prompt once."
  (interactive)
  (unless (disco-current-token)
    (call-interactively #'disco-set-token))
  (when disco-enable-live-updates
    (disco-gateway-start))
  (disco-root-open)
  (disco-root-refresh))

;;;###autoload
(defun disco-reset-session-state ()
  "Clear all in-memory caches used by disco.el."
  (interactive)
  (disco-gateway-stop)
  (disco-directory-reset)
  (disco-state-reset)
  (disco-api-reset-rate-limit-state)
  (disco-http-reset-queue-state)
  (message "disco: in-memory state reset"))

;;;###autoload
(defun disco-describe-http-queue ()
  "Show current HTTP queue runtime status."
  (interactive)
  (disco-http-describe-queue))

;;;###autoload
(defun disco-describe-rate-limits ()
  "Show current in-memory Discord rate-limit windows."
  (interactive)
  (disco-api-describe-rate-limits))

;;;###autoload
(defun disco-describe-gateway ()
  "Show current gateway transport status."
  (interactive)
  (disco-gateway-describe-status))

(provide 'disco)

;;; disco.el ends here
