;;; disco-msg-test.el --- Tests for disco-msg helpers -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-msg)
(require 'disco-state)

(ert-deftest disco-msg-preview-content-prefers-text ()
  (should (equal "hello world"
                 (disco-msg-preview-content
                  '((content . "hello\nworld")
                    (attachments . (((id . "a1")))))))))

(ert-deftest disco-msg-preview-content-falls-back-to-attachment-summary ()
  (should (equal "(2 attachments)"
                 (disco-msg-preview-content
                  '((content . "")
                    (attachments . (((id . "a1"))
                                    ((id . "a2")))))))))

(ert-deftest disco-msg-channel-last-cached-message-prefers-last-message-id ()
  (disco-state-reset)
  (let* ((channel '((id . "c1")
                    (last_message_id . "m2")))
         (m1 '((id . "m1") (content . "older")))
         (m2 '((id . "m2") (content . "newer"))))
    (disco-state-put-messages "c1" (list m1 m2))
    (should (equal "m2"
                   (alist-get 'id (disco-msg-channel-last-cached-message channel))))))

(provide 'disco-msg-test)

;;; disco-msg-test.el ends here
