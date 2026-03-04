;;; disco-api-normalize.el --- API payload normalizers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared request payload/query normalization helpers for Discord API calls.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun disco-api--json-true-p (value)
  "Return non-nil if VALUE semantically represents JSON true."
  (or (eq value t)
      (equal value "true")
      (eq value 'true)))

(defun disco-api--query-bool-string (value)
  "Return VALUE as API query boolean string.

JSON-like true values map to the true string and everything else maps to
the false string."
  (if (disco-api--json-true-p value) "true" "false"))

(defconst disco-api--thread-search-tag-setting-alist
  '((match-some . "match_some")
    (match_some . "match_some")
    (match-all . "match_all")
    (match_all . "match_all"))
  "Declarative map for thread search tag setting values.")

(defconst disco-api--thread-search-sort-by-alist
  '((last-message-time . "last_message_time")
    (last_message_time . "last_message_time")
    (archive-time . "archive_time")
    (archive_time . "archive_time")
    (relevance . "relevance")
    (creation-time . "creation_time")
    (creation_time . "creation_time"))
  "Declarative map for thread search sort-by values.")

(defconst disco-api--thread-search-sort-order-alist
  '((asc . "asc")
    (desc . "desc"))
  "Declarative map for thread search sort-order values.")

(defun disco-api--thread-search-tag-setting-value (value)
  "Normalize thread search tag setting VALUE to API representation."
  (cond
   ((stringp value) value)
   ((symbolp value)
    (alist-get value disco-api--thread-search-tag-setting-alist))
   (t nil)))

(defun disco-api--thread-search-sort-by-value (value)
  "Normalize thread search sort-by VALUE to API representation."
  (cond
   ((stringp value) value)
   ((symbolp value)
    (alist-get value disco-api--thread-search-sort-by-alist))
   (t nil)))

(defun disco-api--thread-search-sort-order-value (value)
  "Normalize thread search sort-order VALUE to API representation."
  (cond
   ((and (stringp value)
         (member value '("asc" "desc")))
    value)
   ((symbolp value)
    (alist-get value disco-api--thread-search-sort-order-alist))
   (t nil)))

(cl-defun disco-api--thread-search-query (&key name slop tags tag-setting archived
                                               sort-by sort-order limit offset
                                               max-id min-id)
  "Build query alist for `/channels/{channel.id}/threads/search'."
  (let ((query nil))
    (when (and (stringp name) (not (string-empty-p name)))
      (push `("name" . ,name) query))
    (when (numberp slop)
      (push `("slop" . ,(number-to-string (max 0 (min 100 slop)))) query))
    (when (listp tags)
      (dolist (tag tags)
        (when tag
          (push `("tag" . ,(format "%s" tag)) query))))
    (let ((tag-setting-value (disco-api--thread-search-tag-setting-value tag-setting)))
      (when tag-setting-value
        (push `("tag_setting" . ,tag-setting-value) query)))
    (when (not (null archived))
      (push `("archived" . ,(disco-api--query-bool-string archived)) query))
    (let ((sort-by-value (disco-api--thread-search-sort-by-value sort-by)))
      (when sort-by-value
        (push `("sort_by" . ,sort-by-value) query)))
    (let ((sort-order-value (disco-api--thread-search-sort-order-value sort-order)))
      (when sort-order-value
        (push `("sort_order" . ,sort-order-value) query)))
    (when (numberp limit)
      (push `("limit" . ,(number-to-string (max 1 (min 25 limit)))) query))
    (when (numberp offset)
      (push `("offset" . ,(number-to-string (max 0 (min 9975 offset)))) query))
    (when max-id
      (push `("max_id" . ,(format "%s" max-id)) query))
    (when min-id
      (push `("min_id" . ,(format "%s" min-id)) query))
    (nreverse query)))

(defun disco-api--thread-archive-query (before limit)
  "Build query alist for thread archive endpoints."
  (let* ((raw-limit (or limit 50))
         ;; Discord archived thread endpoints accept 2-100.
         (normalized-limit (max 2 (min 100 raw-limit)))
         (query `(("limit" . ,(number-to-string normalized-limit)))))
    (when before
      (setq query (append query `(("before" . ,before)))))
    query))

(cl-defun disco-api--thread-members-query (&key with-member after limit)
  "Build query alist for thread member listing endpoints."
  (let (query)
    (when (not (null with-member))
      (push `("with_member" . ,(disco-api--query-bool-string with-member)) query))
    (when after
      (push `("after" . ,(format "%s" after)) query))
    (when (numberp limit)
      ;; Discord thread member listing endpoint accepts 1-100.
      (push `("limit" . ,(number-to-string (max 1 (min 100 limit)))) query))
    (nreverse query)))

(cl-defun disco-api--thread-update-payload (&key name archived locked auto-archive-duration
                                                 rate-limit-per-user invitable applied-tags)
  "Build thread channel PATCH payload from keyword arguments."
  (let (payload)
    (when (and (stringp name) (not (string-empty-p name)))
      (push `(name . ,name) payload))
    (when (not (null archived))
      (push `(archived . ,(if (disco-api--json-true-p archived) t :false)) payload))
    (when (not (null locked))
      (push `(locked . ,(if (disco-api--json-true-p locked) t :false)) payload))
    (when auto-archive-duration
      (push `(auto_archive_duration . ,auto-archive-duration) payload))
    (when (not (null rate-limit-per-user))
      (push `(rate_limit_per_user . ,rate-limit-per-user) payload))
    (when (not (null invitable))
      (push `(invitable . ,(if (disco-api--json-true-p invitable) t :false)) payload))
    (when (listp applied-tags)
      (push `(applied_tags . ,applied-tags) payload))
    (nreverse payload)))

(provide 'disco-api-normalize)

;;; disco-api-normalize.el ends here
