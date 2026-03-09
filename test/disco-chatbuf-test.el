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

(ert-deftest disco-chatbuf-string-helpers-preserve-and-inspect-properties ()
  (let ((text (copy-sequence "[file] a.txt")))
    (add-text-properties 0 (length text)
                         (list disco-chatbuf-input-object-property
                               '(:kind attachment :path "/tmp/a.txt"))
                         text)
    (should (disco-chatbuf-string-has-objects-p text))
    (should (equal "[file] a.txt"
                   (disco-chatbuf-string-plain-text text)))
    (let ((copy (disco-chatbuf-copy-string text)))
      (should-not (eq copy text))
      (should (disco-chatbuf-string-has-objects-p copy)))))

(ert-deftest disco-chatbuf-reset-state-reinitializes-history-and-markers ()
  (with-temp-buffer
    (disco-chatbuf-init-state 8)
    (let ((old-input-marker disco-chatbuf--input-marker)
          (old-prompt-marker disco-chatbuf--prompt-marker))
      (disco-chatbuf-aux-set '(:aux-type reply))
      (disco-chatbuf-input-options-set '(:send-on-return t))
      (disco-chatbuf-input-history-push "hello")
      (setq-local disco-chatbuf--prompt-button 'dummy)
      (disco-chatbuf-reset-state 3)
      (should (markerp disco-chatbuf--input-marker))
      (should (markerp disco-chatbuf--prompt-marker))
      (should-not (eq old-input-marker disco-chatbuf--input-marker))
      (should-not (eq old-prompt-marker disco-chatbuf--prompt-marker))
      (should (ring-p disco-chatbuf--input-ring))
      (should (= 3 (ring-size disco-chatbuf--input-ring)))
      (should (= 0 (ring-length disco-chatbuf--input-ring)))
      (should-not disco-chatbuf--input-idx)
      (should-not disco-chatbuf--input-pending)
      (should-not disco-chatbuf--aux-plist)
      (should-not disco-chatbuf--input-options-plist)
      (should-not disco-chatbuf--prompt-button))))

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

(ert-deftest disco-chatbuf-bind-input-region-hides-and-restores-tail-input ()
  (with-temp-buffer
    (insert "timeline\n")
    (disco-chatbuf-bind-input-region
     :visible-p t
     :prompt ">>> "
     :input-text "hello"
     :post-bind-function
     (lambda ()
       (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
         (add-text-properties (car bounds) (cdr bounds) '(demo t)))))
    (should (disco-chatbuf-prompt-button-live-p))
    (should (equal "hello" (disco-chatbuf-input-string)))
    (should (eq t (get-text-property (disco-chatbuf-input-start-position) 'demo)))
    (disco-chatbuf-bind-input-region :visible-p nil)
    (should-not (disco-chatbuf-prompt-button-live-p))
    (should-not (disco-chatbuf-input-start-position))
    (disco-chatbuf-bind-input-region
     :visible-p t
     :prompt "qq> "
     :input-text "world")
    (should (disco-chatbuf-prompt-button-live-p))
    (should (equal "world" (disco-chatbuf-input-string)))))

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
    (should (equal '("second" "first")
                   (disco-chatbuf-input-history-elements)))
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

(ert-deftest disco-chatbuf-input-history-push-explicit-text-ignores-live-object-buffer ()
  (with-temp-buffer
    (disco-chatbuf-init-state 8)
    (disco-chatbuf-install-prompt ">>> ")
    (disco-chatbuf-input-insert "[file:a.txt]"
                                :object '(:type file :path "/tmp/a.txt"))
    (disco-chatbuf-input-history-push "plain text")
    (should (equal '("plain text")
                   (disco-chatbuf-input-history-elements)))))

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
    (disco-chatbuf-aux-set '(:aux-type reply :aux-msg ((id . "m1")) :message-id "m1"))
    (should (disco-chatbuf-aux-active-p))
    (should (equal 'reply (disco-chatbuf-aux-type)))
    (should (equal "m1" (disco-chatbuf-aux-message-id)))
    (should (equal '(:aux-type reply :aux-msg ((id . "m1")) :message-id "m1")
                   (disco-chatbuf-aux-state)))
    (disco-chatbuf-aux-reset)
    (should-not (disco-chatbuf-aux-active-p))))

(ert-deftest disco-chatbuf-input-options-state-roundtrip ()
  (with-temp-buffer
    (disco-chatbuf-init-state)
    (disco-chatbuf-input-options-set
     '(:send-on-return t :allowed-mentions none))
    (should (equal '(:send-on-return t :allowed-mentions none)
                   (disco-chatbuf-input-options-state)))
    (should (eq t (disco-chatbuf-input-option :send-on-return)))
    (should (eq 'none (disco-chatbuf-input-option :allowed-mentions)))
    (should (eq 'fallback (disco-chatbuf-input-option :missing 'fallback)))
    (disco-chatbuf-input-options-reset)
    (should-not (disco-chatbuf-input-options-state))))

(provide 'disco-chatbuf-test)

;;; disco-chatbuf-test.el ends here
