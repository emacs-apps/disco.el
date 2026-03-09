;;; disco-util.el --- Shared utility helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared low-level helpers used across disco modules.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'time-date)

(defun disco-util-json-true-p (value)
  "Return non-nil when VALUE semantically represents JSON true."
  (or (eq value t)
      (eq value 'true)
      (equal value "true")))

(defun disco-util-normalize-id-list (ids &optional max-items)
  "Normalize IDS into a list of unique string IDs.

Preserve the original order of first appearance.  When MAX-ITEMS is non-nil,
truncate to at most that many IDs."
  (let (result)
    (dolist (it (or ids '()))
      (let ((normalized (and it (format "%s" it))))
        (when normalized
          (cl-pushnew normalized result :test #'equal))))
    (let ((ordered (nreverse result)))
      (if (and (integerp max-items)
               (> (length ordered) max-items))
          (seq-take ordered max-items)
        ordered))))

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
