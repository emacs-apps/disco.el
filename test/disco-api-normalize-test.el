;;; disco-api-normalize-test.el --- Tests for disco-api-normalize -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-api-normalize)

(ert-deftest disco-api-normalize-ack-message-payload-empty ()
  (should (eq :empty-object
              (disco-api--ack-message-payload nil nil nil nil nil))))

(ert-deftest disco-api-normalize-ack-message-payload-mention-implies-manual ()
  (should (equal '((manual . t) (mention_count . 3))
                 (disco-api--ack-message-payload nil nil 3 nil nil))))

(ert-deftest disco-api-normalize-ack-message-payload-validates-fields ()
  (should-error (disco-api--ack-message-payload nil nil -1 nil nil) :type 'error)
  (should-error (disco-api--ack-message-payload nil nil nil -5 nil) :type 'error)
  (should-error (disco-api--ack-message-payload 42 nil nil nil nil) :type 'error))

(ert-deftest disco-api-normalize-message-edit-payload-allowed-mentions ()
  (should (equal '((content . "hi")
                   (allowed_mentions (parse . ["users"])))
                 (disco-api--message-edit-payload
                  "hi"
                  '((parse . ["users"]))))))

(provide 'disco-api-normalize-test)

;;; disco-api-normalize-test.el ends here
