;;; disco-util.el --- Shared utility helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared data-shape helpers used across room/embed renderers.

;;; Code:

(require 'subr-x)
(require 'time-date)

(defun disco-util--snake-to-camel-case (name)
  "Convert snake_case NAME string into lowerCamelCase string."
  (let* ((parts (split-string (or name "") "_" t))
         (head (downcase (or (car parts) "")))
         (tail (mapconcat (lambda (part)
                            (capitalize (downcase part)))
                          (cdr parts)
                          "")))
    (concat head tail)))

(defun disco-util--object-key-candidates (key)
  "Return key candidate list for KEY across symbol/string/keyword forms."
  (let* ((raw-name (cond
                    ((keywordp key) (substring (symbol-name key) 1))
                    ((symbolp key) (symbol-name key))
                    ((stringp key) key)
                    (t (format "%s" key))))
         (snake (replace-regexp-in-string "-" "_" raw-name))
         (camel (if (string-match-p "_" snake)
                    (disco-util--snake-to-camel-case snake)
                  snake))
         (names (delete-dups (list snake camel (downcase snake) (downcase camel))))
         out)
    (dolist (name names)
      (push name out)
      (push (intern name) out)
      (push (intern (concat ":" name)) out))
    (delete-dups (nreverse out))))

(defun disco-util-object-get (object &rest keys)
  "Return first non-nil value for KEYS found in OBJECT alist/plist."
  (let ((candidates (delete-dups
                     (apply #'append
                            (mapcar #'disco-util--object-key-candidates keys)))))
    (cond
     ((and (listp object) (consp (car object)))
      (catch 'found
        (dolist (candidate candidates)
          (let ((pair (assoc candidate object)))
            (when (and pair (cdr pair))
              (throw 'found (cdr pair)))))
        nil))
     ((listp object)
      (catch 'found
        (dolist (candidate candidates)
          (when (keywordp candidate)
            (let ((tail (plist-member object candidate)))
              (when (and tail (cadr tail))
                (throw 'found (cadr tail))))))
        nil))
     (t nil))))

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

(defun disco-util--char-run-length (text start char)
  "Return contiguous CHAR run length in TEXT from START."
  (let ((len (length text))
        (pos start))
    (while (and (< pos len)
                (eq (aref text pos) char))
      (setq pos (1+ pos)))
    (- pos start)))

(defun disco-util--markdown-punctuation-char-p (char)
  "Return non-nil when CHAR is a Markdown-escapable punctuation char."
  (and (characterp char)
       (string-match-p "[[:punct:]]" (char-to-string char))))

(defun disco-util-unescape-markdown-punctuation (text)
  "Return TEXT with Markdown punctuation escapes removed.

This follows a markdown-aware strategy: escapes are unwrapped only outside
inline/code-fence spans, so code blocks preserve literal backslashes."
  (if (not (stringp text))
      text
    (let ((len (length text))
          (idx 0)
          (parts nil)
          (in-inline nil)
          (inline-ticks 0)
          (in-fence nil)
          (fence-ticks 0))
      (while (< idx len)
        (let ((char (aref text idx)))
          (if (eq char ?`)
              (let* ((ticks (disco-util--char-run-length text idx ?`))
                     (line-start (or (= idx 0)
                                     (memq (aref text (1- idx)) '(?\n ?\r)))))
                (cond
                 (in-fence
                  (when (and line-start (>= ticks fence-ticks))
                    (setq in-fence nil)
                    (setq fence-ticks 0)))
                 (in-inline
                  (when (= ticks inline-ticks)
                    (setq in-inline nil)
                    (setq inline-ticks 0)))
                 ((and line-start (>= ticks 3))
                  (setq in-fence t)
                  (setq fence-ticks ticks))
                 (t
                  (setq in-inline t)
                  (setq inline-ticks ticks)))
                (push (substring text idx (+ idx ticks)) parts)
                (setq idx (+ idx ticks)))
            (if (and (not in-inline)
                     (not in-fence)
                     (eq char ?\\)
                     (< (1+ idx) len)
                     (disco-util--markdown-punctuation-char-p
                      (aref text (1+ idx))))
                (progn
                  (push (string (aref text (1+ idx))) parts)
                  (setq idx (+ idx 2)))
              (push (string char) parts)
              (setq idx (1+ idx))))))
      (apply #'concat (nreverse parts)))))

(provide 'disco-util)

;;; disco-util.el ends here
