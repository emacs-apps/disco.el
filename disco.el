;;; disco.el --- Discord client for Emacs -*- lexical-binding: t; -*-

;; Author: disco.el contributors
;; Keywords: comm
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; disco.el provides an MVP Discord client experience in Emacs:
;; - open root buffer with guild/channel list
;; - open a channel timeline
;; - send text messages

;;; Code:

(require 'subr-x)
(require 'disco-customize)
(require 'disco-state)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-room)
(require 'disco-root)

;;;###autoload
(defun disco ()
  "Start disco.el and open root buffer.

If token is missing, prompt for it once in current session."
  (interactive)
  (unless (and disco-token (not (string-empty-p disco-token)))
    (call-interactively #'disco-set-token))
  (when disco-enable-live-updates
    (disco-gateway-start))
  (disco-root-open)
  (disco-root-refresh))

;;;###autoload
(defun disco-reset-session-state ()
  "Clear all in-memory caches used by disco.el."
  (interactive)
  (disco-state-reset)
  (message "disco: in-memory state reset"))

(provide 'disco)

;;; disco.el ends here
