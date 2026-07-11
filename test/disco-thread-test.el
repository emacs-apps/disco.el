;;; disco-thread-test.el --- Tests for disco-thread helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-state)
(require 'disco-thread)

(ert-deftest disco-thread-status-helpers-describe-thread-channel ()
  (let ((channel '((id . "t1")
                   (type . 12)
                   (thread_metadata . ((archived . true)
                                       (locked . true))))))
    (should (disco-thread-channel-p channel))
    (should (disco-thread-private-p channel))
    (should (equal '("archived" "locked" "private")
                   (disco-thread-status-tags channel)))
    (should (equal " [thread: archived, locked, private]"
                   (disco-thread-header-suffix channel)))))

(ert-deftest disco-thread-read-auto-archive-duration-supports-empty-and-required ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "")))
    (should-not (disco-thread-read-auto-archive-duration)))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "1440")))
    (should (= 1440 (disco-thread-read-auto-archive-duration t 60)))))

(ert-deftest disco-thread-read-tristate-and-detached-type-normalize-choices ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "no")))
    (should (eq :false (disco-thread-read-tristate-bool "Locked" t))))
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "private")))
    (should (= 12 (disco-thread-read-detached-type)))))

(ert-deftest disco-thread-resolve-update-requires-complete-response ()
  (disco-state-reset)
  (let ((updated '((id . "t1") (name . "new")))
        callback-value)
    (should (equal updated
                   (disco-thread-resolve-update
                    updated
                    (lambda (value)
                      (setq callback-value value)))))
    (should (equal updated callback-value))
    (should (equal "new"
                   (alist-get 'name (disco-state-channel "t1"))))
    (should-error (disco-thread-resolve-update nil))))

(provide 'disco-thread-test)

;;; disco-thread-test.el ends here
