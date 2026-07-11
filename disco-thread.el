;;; disco-thread.el --- Thread helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared thread predicates, prompts, and update helpers used by room/root views.

;;; Code:

(require 'subr-x)
(require 'disco-util)
(require 'disco-state)

(defconst disco-thread-channel-types '(10 11 12)
  "Discord channel type values representing thread channels.")

(defconst disco-thread-parent-channel-types '(0 5 15 16)
  "Discord channel types that can expose thread lists in UI.")

(defconst disco-thread-forum-or-media-parent-types '(15 16)
  "Discord channel type values for forum/media parent channels.")

(defconst disco-thread-auto-archive-duration-options '(60 1440 4320 10080)
  "Allowed Discord thread auto archive duration values in minutes.")

(defconst disco-thread--detached-type-choice-alist
  '(("" . nil)
    ("public" . 11)
    ("private" . 12))
  "Declarative map of detached thread type prompt choice to channel type.")

(defconst disco-thread--tristate-choice-alist
  '(("keep" . keep)
    ("yes" . t)
    ("no" . :false))
  "Declarative map of tri-state prompt choice to returned value.")

(defun disco-thread-channel-p (channel)
  "Return non-nil when CHANNEL is a Discord thread channel."
  (and (listp channel)
       (memq (alist-get 'type channel) disco-thread-channel-types)))

(defun disco-thread-parent-channel-p (channel)
  "Return non-nil when CHANNEL can contain visible threads in UI."
  (and (listp channel)
       (memq (alist-get 'type channel) disco-thread-parent-channel-types)))

(defun disco-thread-forum-or-media-channel-p (channel)
  "Return non-nil when CHANNEL is a forum/media parent channel."
  (and (listp channel)
       (memq (alist-get 'type channel) disco-thread-forum-or-media-parent-types)))

(defun disco-thread-metadata (channel)
  "Return thread metadata alist for CHANNEL."
  (or (and (listp channel)
           (alist-get 'thread_metadata channel))
      '()))

(defun disco-thread-archived-p (channel)
  "Return non-nil when CHANNEL thread is archived."
  (let ((meta (disco-thread-metadata channel)))
    (or (disco-util-json-true-p (alist-get 'archived meta))
        (disco-util-json-true-p (and (listp channel)
                                     (alist-get 'archived channel))))))

(defun disco-thread-locked-p (channel)
  "Return non-nil when CHANNEL thread is locked."
  (let ((meta (disco-thread-metadata channel)))
    (or (disco-util-json-true-p (alist-get 'locked meta))
        (disco-util-json-true-p (and (listp channel)
                                     (alist-get 'locked channel))))))

(defun disco-thread-private-p (channel)
  "Return non-nil when CHANNEL is a private thread."
  (equal (and (listp channel) (alist-get 'type channel)) 12))

(defun disco-thread-status-tags (channel)
  "Return status tags for CHANNEL as a list of strings."
  (let (tags)
    (when (disco-thread-archived-p channel)
      (push "archived" tags))
    (when (disco-thread-locked-p channel)
      (push "locked" tags))
    (when (disco-thread-private-p channel)
      (push "private" tags))
    (nreverse tags)))

(defun disco-thread-status-string (channel)
  "Return comma-joined status tags string for CHANNEL."
  (mapconcat #'identity (disco-thread-status-tags channel) ", "))

(defun disco-thread-header-suffix (channel)
  "Return human-readable status suffix for thread CHANNEL."
  (if (not (disco-thread-channel-p channel))
      ""
    (let ((tags (disco-thread-status-tags channel)))
      (if tags
          (format " [thread: %s]" (mapconcat #'identity tags ", "))
        " [thread]"))))

(defun disco-thread-read-auto-archive-duration (&optional required default)
  "Prompt for thread auto archive duration in minutes.

When REQUIRED is non-nil, empty input is rejected. DEFAULT is preselected when
provided."
  (let* ((choices (mapcar #'number-to-string
                          disco-thread-auto-archive-duration-options))
         (prompt (if required
                     "Auto archive minutes: "
                   "Auto archive minutes (empty for default): "))
         (initial (and default (number-to-string default)))
         (raw (completing-read prompt
                               (if required choices (cons "" choices))
                               nil t nil nil (or initial ""))))
    (if (and (not required)
             (string-empty-p raw))
        nil
      (string-to-number raw))))

(defun disco-thread-read-tristate-bool (prompt current-value)
  "Read tri-state boolean with PROMPT and CURRENT-VALUE.

Return symbol `keep', t, or :false."
  (let* ((choice (completing-read
                  (format "%s (keep/yes/no, current %s): "
                          prompt
                          (if (disco-util-json-true-p current-value)
                              "yes"
                            "no"))
                  (mapcar #'car disco-thread--tristate-choice-alist)
                  nil t nil nil "keep")))
    (alist-get choice disco-thread--tristate-choice-alist nil nil #'string=)))

(defun disco-thread-read-detached-type ()
  "Prompt for detached thread type and return numeric channel type or nil."
  (let ((choice (completing-read
                 "Thread type (empty/public/private): "
                 (mapcar #'car disco-thread--detached-type-choice-alist)
                 nil t nil nil "")))
    (alist-get choice disco-thread--detached-type-choice-alist nil nil #'string=)))

(defun disco-thread-resolve-update (updated &optional on-update)
  "Store complete UPDATED thread response and call ON-UPDATE.

Signal an error when the API response is not a channel object."
  (unless (and (listp updated) (alist-get 'id updated))
    (error "disco: thread update returned no channel object"))
  (disco-state-upsert-channel updated)
  (when (functionp on-update)
    (funcall on-update updated))
  updated)

(provide 'disco-thread)

;;; disco-thread.el ends here
