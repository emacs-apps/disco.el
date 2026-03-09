;;; disco-util.el --- Shared utility helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared formatting helpers used across room/embed renderers.

;;; Code:

(require 'subr-x)
(require 'time-date)

(defun disco-util-json-true-p (value)
  "Return non-nil when VALUE semantically represents JSON true."
  (or (eq value t)
      (eq value 'true)
      (equal value "true")))

(defun disco-util-format-time (iso8601)
  "Format ISO8601 into a compact local string."
  (condition-case _
      (format-time-string "%Y-%m-%d %H:%M"
                          (date-to-time iso8601))
    (error "unknown-time")))

(defun disco-util-format-time-short (iso8601)
  "Format ISO8601 into HH:MM local string."
  (condition-case _
      (format-time-string "%H:%M" (date-to-time iso8601))
    (error "--:--")))

(provide 'disco-util)

;;; disco-util.el ends here
