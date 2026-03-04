;;; disco-typing.el --- Typing indicator state helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared helpers for typing indicator table mutation and rendering.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun disco-typing-timeout-seconds (raw-timeout)
  "Return normalized typing timeout from RAW-TIMEOUT seconds value."
  (max 1 (or raw-timeout 10)))

(defun disco-typing-normalize-user-id (user-id)
  "Return USER-ID as string, or nil when missing."
  (and user-id (format "%s" user-id)))

(defun disco-typing-prune-expired (typing-table &optional now)
  "Drop expired entries from TYPING-TABLE.

Return non-nil when any entry is removed."
  (let ((cutoff (or now (float-time)))
        stale-ids)
    (when (hash-table-p typing-table)
      (maphash
       (lambda (user-id entry)
         (let ((expires-at (plist-get entry :expires-at)))
           (when (or (not (numberp expires-at))
                     (<= expires-at cutoff))
             (push user-id stale-ids))))
       typing-table))
    (dolist (user-id stale-ids)
      (remhash user-id typing-table))
    (and stale-ids t)))

(defun disco-typing-active-entries (typing-table &optional now)
  "Return active typing entries from TYPING-TABLE sorted by recent activity.

Entries are sorted by descending `:updated-at', then case-insensitive
`display-name'."
  (disco-typing-prune-expired typing-table now)
  (let (entries)
    (when (hash-table-p typing-table)
      (maphash
       (lambda (_user-id entry)
         (when (listp entry)
           (push entry entries)))
       typing-table))
    (sort entries
          (lambda (left right)
            (let ((left-updated (or (plist-get left :updated-at) 0))
                  (right-updated (or (plist-get right :updated-at) 0))
                  (left-name (or (plist-get left :display-name) ""))
                  (right-name (or (plist-get right :display-name) "")))
              (if (= left-updated right-updated)
                  (string-lessp (downcase left-name)
                                (downcase right-name))
                (> left-updated right-updated)))))))

(defun disco-typing-indicator-text (entries)
  "Return one-line typing indicator text from active ENTRIES list.

Return nil when ENTRIES is empty."
  (let* ((names (mapcar (lambda (entry)
                          (or (plist-get entry :display-name) "unknown"))
                        entries))
         (count (length names)))
    (pcase count
      (0 nil)
      (1 (format "%s is typing..." (nth 0 names)))
      (2 (format "%s and %s are typing..." (nth 0 names) (nth 1 names)))
      (3 (format "%s, %s and %s are typing..."
                 (nth 0 names) (nth 1 names) (nth 2 names)))
      (_ (format "%s, %s and %d others are typing..."
                 (nth 0 names) (nth 1 names) (- count 2))))))

(defun disco-typing-indicator-text-from-table (typing-table &optional now)
  "Return indicator text from TYPING-TABLE, or nil when idle."
  (disco-typing-indicator-text
   (disco-typing-active-entries typing-table now)))

(defun disco-typing-next-expiry (typing-table)
  "Return nearest typing expiry timestamp from TYPING-TABLE, or nil."
  (let (next-expiry)
    (when (hash-table-p typing-table)
      (maphash
       (lambda (_user-id entry)
         (let ((expires-at (plist-get entry :expires-at)))
           (when (numberp expires-at)
             (setq next-expiry
                   (if (or (null next-expiry)
                           (< expires-at next-expiry))
                       expires-at
                     next-expiry)))))
       typing-table))
    next-expiry))

(provide 'disco-typing)

;;; disco-typing.el ends here
