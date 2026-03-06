;;; disco-transient.el --- Transient command menus for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Central command menus inspired by ement's transient-driven room workflow.

;;; Code:

(require 'transient)

(require 'disco-api)
(require 'disco-customize)
(require 'disco-gateway)
(require 'disco-http)
(require 'disco-state)

(declare-function disco-root-refresh "disco-root")
(declare-function disco-root-list-archived-threads "disco-root")
(declare-function disco-root-cycle-layout "disco-root")
(declare-function disco-root-set-layout "disco-root")
(declare-function disco-root-toggle-unread-lens "disco-root")
(declare-function disco-root-search-transient "disco-root")

(defun disco-transient-reset-session-state ()
  "Reset in-memory session state for disco.el."
  (interactive)
  (disco-gateway-stop)
  (disco-state-reset)
  (disco-api-reset-rate-limit-state)
  (disco-http-reset-queue-state)
  (message "disco: in-memory state reset"))

(defun disco-transient-toggle-active-thread-prefetch ()
  "Toggle `disco-fetch-guild-active-threads' and refresh root when relevant."
  (interactive)
  (setq disco-fetch-guild-active-threads (not disco-fetch-guild-active-threads))
  (message "disco: active thread prefetch %s"
           (if disco-fetch-guild-active-threads "enabled" "disabled"))
  (when (derived-mode-p 'disco-root-mode)
    (disco-root-refresh)))

(defun disco-transient-set-thread-archive-fetch-limit (limit)
  "Set archived thread fetch LIMIT in current session."
  (interactive "nArchive thread fetch limit (2-100): ")
  (setq disco-thread-archive-fetch-limit (max 2 (min 100 limit)))
  (message "disco: archive thread fetch limit set to %d"
           disco-thread-archive-fetch-limit))

(transient-define-prefix disco-root-transient ()
  "Root command menu for disco.el."
  [["Refresh"
    ("g" "Refresh root" disco-root-refresh)
    ("A" "Archived threads..." disco-root-list-archived-threads)
    ("t" "Toggle active thread prefetch" disco-transient-toggle-active-thread-prefetch)
    ("L" "Set archive fetch limit" disco-transient-set-thread-archive-fetch-limit)]
   ["View"
    ("l" "Cycle layout" disco-root-cycle-layout)
    ("V" "Set layout..." disco-root-set-layout)
    ("s" "Search..." disco-root-search-transient)
    ("U" "Toggle unread lens" disco-root-toggle-unread-lens)]
   ["Inspect"
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Session"
    ("x" "Reset session state" disco-transient-reset-session-state)
    ("q" "Quit window" quit-window)]])

(provide 'disco-transient)

;;; disco-transient.el ends here
