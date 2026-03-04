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

(ert-deftest disco-api-normalize-token-payload ()
  (should (eq :empty-object (disco-api--token-payload nil)))
  (should (equal '((token . "tok")) (disco-api--token-payload "tok")))
  (should (equal '((token . :null)) (disco-api--token-payload :null)))
  (should-error (disco-api--token-payload 42) :type 'error))

(ert-deftest disco-api-normalize-read-state-type ()
  (should (= 0 (disco-api--normalize-read-state-type nil "read_state_type" 0)))
  (should (= 1 (disco-api--normalize-read-state-type 'guild-event "read_state_type" nil)))
  (should (= 2 (disco-api--normalize-read-state-type "notification_center"
                                                     "read_state_type"
                                                     nil)))
  (should (= 5 (disco-api--normalize-read-state-type "5" "read_state_type" nil)))
  (should-error (disco-api--normalize-read-state-type "unknown" "read_state_type" nil)
                :type 'error))

(ert-deftest disco-api-normalize-read-states-bulk-payload ()
  (should (equal
           '((read_states
              . [((channel_id . "11")
                  (message_id . "22"))
                 ((read_state_type . 1)
                  (channel_id . "33")
                  (message_id . "44"))]))
           (disco-api--read-states-bulk-payload
            '(((channel_id . "11")
               (message_id . "22"))
              ((read_state_type . guild-event)
               (channel_id . "33")
               (message_id . "44"))))))
  (should-error
   (disco-api--read-states-bulk-payload
    '(((channel_id . "11")
       (message_id . "0"))))
   :type 'error))

(ert-deftest disco-api-normalize-delete-read-state-payload ()
  (should (equal nil
                 (disco-api--delete-read-state-payload
                  :read-state-type nil
                  :version nil)))
  (should (equal '((read_state_type . 4)
                   (version . 2))
                 (disco-api--delete-read-state-payload
                  :read-state-type 'guild-onboarding-question
                  :version "2"))))

(provide 'disco-api-normalize-test)

;;; disco-api-normalize-test.el ends here
