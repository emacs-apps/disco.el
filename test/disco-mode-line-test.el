;;; disco-mode-line-test.el --- Tests for mode-line helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco-mode-line)

(defvar disco-mode-line-test--cache nil)

(ert-deftest disco-mode-line-install-is-idempotent ()
  (let ((mode-line-misc-info '(existing)))
    (disco-mode-line-install 'provider)
    (disco-mode-line-install 'provider)
    (should (equal '(existing provider) mode-line-misc-info))
    (disco-mode-line-uninstall 'provider)
    (should (equal '(existing) mode-line-misc-info))))

(ert-deftest disco-mode-line-indicator-is-clickable-with-optional-prefix ()
  (let ((text (disco-mode-line-indicator
               "@2" :prefix " " :face 'warning
               :command #'ignore :help-echo "mentions")))
    (should (equal " @2" (substring-no-properties text)))
    (should (eq 'warning (get-text-property 1 'face text)))
    (should (keymapp (get-text-property 1 'local-map text)))
    (should (equal "mentions" (get-text-property 1 'help-echo text))))
  (should-not (disco-mode-line-indicator nil :prefix " ")))

(ert-deftest disco-mode-line-update-cache-formats-and-returns-value ()
  (let ((disco-mode-line-test--cache nil))
    (cl-letf (((symbol-function 'format-mode-line)
               (lambda (format) (should (equal '("ready") format)) "ready"))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (should (equal "ready"
                     (disco-mode-line-update-cache
                      'disco-mode-line-test--cache '("ready"))))
      (should (equal "ready" disco-mode-line-test--cache)))))

(provide 'disco-mode-line-test)

;;; disco-mode-line-test.el ends here
