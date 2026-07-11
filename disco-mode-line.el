;;; disco-mode-line.el --- Reusable mode-line presentation helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Client-neutral helpers for installing cached mode-line providers and
;; rendering clickable indicators.  Protocol-specific state and hooks stay in
;; the consuming client.

;;; Code:

(require 'cl-lib)

(defun disco-mode-line-install (provider)
  "Install mode-line PROVIDER in `mode-line-misc-info' once."
  (unless (member provider mode-line-misc-info)
    (setq mode-line-misc-info
          (append mode-line-misc-info (list provider)))))

(defun disco-mode-line-uninstall (provider)
  "Remove all occurrences of mode-line PROVIDER."
  (setq mode-line-misc-info (delete provider mode-line-misc-info)))

(defun disco-mode-line-update-cache (cache-symbol format)
  "Set CACHE-SYMBOL to formatted mode-line FORMAT and redraw all mode lines."
  (set cache-symbol (format-mode-line format))
  (force-mode-line-update t)
  (symbol-value cache-symbol))

(cl-defun disco-mode-line-indicator
    (text &key prefix face command help-echo)
  "Return a mode-line indicator displaying TEXT.

PREFIX is prepended only when TEXT is non-nil.  FACE styles the indicator.
COMMAND is invoked by mouse-1 and HELP-ECHO describes that action."
  (when text
    (concat
     (or prefix "")
     (apply #'propertize
            text
            (append
             (when face (list 'face face))
             (when command
               (list 'local-map (make-mode-line-mouse-map 'mouse-1 command)
                     'mouse-face 'mode-line-highlight))
             (when help-echo (list 'help-echo help-echo)))))))

(provide 'disco-mode-line)

;;; disco-mode-line.el ends here
