;;; disco-chatbuf-test.el --- Tests for disco-chatbuf -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-chatbuf)

(ert-deftest disco-chatbuf-install-prompt-creates-tail-input-region ()
  (with-temp-buffer
    (insert "timeline\n")
    (disco-chatbuf-init-state 8)
    (disco-chatbuf-install-prompt ">>> ")
    (should (disco-chatbuf-prompt-button-live-p))
    (should (= (disco-chatbuf-input-start-position) (point-max)))
    (insert "hello")
    (should (disco-chatbuf-has-input-p))
    (should (equal "hello" (disco-chatbuf-input-string)))))

(ert-deftest disco-chatbuf-prompt-update-preserves-input-and-point-offset ()
  (with-temp-buffer
    (insert "timeline\n")
    (disco-chatbuf-install-prompt ">>> ")
    (insert "hello")
    (goto-char (+ (disco-chatbuf-input-start-position) 2))
    (disco-chatbuf-prompt-update "qq> ")
    (should (disco-chatbuf-prompt-button-live-p))
    (should (equal "hello" (disco-chatbuf-input-string)))
    (should (= 2 (- (point) (disco-chatbuf-input-start-position))))))

(ert-deftest disco-chatbuf-post-command-clamp-point-skips-prompt-glyphs ()
  (with-temp-buffer
    (insert "timeline\n")
    (disco-chatbuf-install-prompt ">>> ")
    (goto-char (disco-chatbuf-prompt-start-position))
    (disco-chatbuf-post-command-clamp-point)
    (should (= (point) (disco-chatbuf-input-start-position)))))

(ert-deftest disco-chatbuf-structured-object-insert-and-prune ()
  (with-temp-buffer
    (disco-chatbuf-install-prompt ">>> ")
    (disco-chatbuf-input-insert "[file:a.txt]"
                                :object '(:type file :path "/tmp/a.txt"))
    (goto-char (disco-chatbuf-input-start-position))
    (should (equal '(:type file :path "/tmp/a.txt")
                   (disco-chatbuf-input-object-at-point)))
    (should (disco-chatbuf-input-has-objects-p))
    (delete-char 1)
    (disco-chatbuf-input-prune-broken-objects)
    (should (equal "" (or (disco-chatbuf-input-string) "")))
    (should-not (disco-chatbuf-input-has-objects-p))))

(ert-deftest disco-chatbuf-input-history-restores-pending-input ()
  (with-temp-buffer
    (disco-chatbuf-init-state 8)
    (disco-chatbuf-install-prompt ">>> ")
    (disco-chatbuf-input-set-text "first")
    (disco-chatbuf-input-history-push)
    (disco-chatbuf-input-set-text "second")
    (disco-chatbuf-input-history-push)
    (disco-chatbuf-input-set-text "pending")
    (disco-chatbuf-input-history-prev)
    (should (equal "second" (disco-chatbuf-input-string)))
    (disco-chatbuf-input-history-prev)
    (should (equal "first" (disco-chatbuf-input-string)))
    (disco-chatbuf-input-history-next)
    (should (equal "second" (disco-chatbuf-input-string)))
    (disco-chatbuf-input-history-next)
    (should (equal "pending" (disco-chatbuf-input-string)))
    (should-not disco-chatbuf--input-idx)
    (should-not disco-chatbuf--input-pending)))

(ert-deftest disco-chatbuf-empty-input-remains-editable-at-point-max ()
  (save-window-excursion
    (let ((buffer (get-buffer-create " *disco-chatbuf-input*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (disco-chatbuf-init-state 8)
            (disco-chatbuf-install-prompt ">>> ")
            (goto-char (or (disco-chatbuf-input-logical-end-position) (point-max)))
            (execute-kbd-macro "qs")
            (should (equal "qs" (disco-chatbuf-input-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest disco-chatbuf-aux-state-roundtrip ()
  (with-temp-buffer
    (disco-chatbuf-init-state)
    (should-not (disco-chatbuf-aux-active-p))
    (disco-chatbuf-aux-set '(:aux-type reply :aux-msg ((id . "m1"))))
    (should (disco-chatbuf-aux-active-p))
    (should (equal 'reply (plist-get disco-chatbuf--aux-plist :aux-type)))
    (disco-chatbuf-aux-reset)
    (should-not (disco-chatbuf-aux-active-p))))

(provide 'disco-chatbuf-test)

;;; disco-chatbuf-test.el ends here
