;;; disco-api-test.el --- Tests for disco-api read-state wrappers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-api)

(ert-deftest disco-api-ack-guild-feature-builds-endpoint-and-payload ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok (disco-api-ack-guild-feature "123" 'guild-event "456" "tok")))
      (should (equal
               '("POST" "/guilds/123/ack/1/456" ((token . "tok")) nil nil nil nil nil)
               captured)))))

(ert-deftest disco-api-bulk-update-read-states-builds-payload ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok
                  (disco-api-bulk-update-read-states
                   '(((channel_id . "11")
                      (message_id . "22"))
                     ((read_state_type . guild-event)
                      (channel_id . "33")
                      (message_id . "44"))))))
      (should (equal
               '("POST"
                 "/read-states/ack-bulk"
                 ((read_states
                   . [((channel_id . "11")
                       (message_id . "22"))
                      ((read_state_type . 1)
                       (channel_id . "33")
                       (message_id . "44"))]))
                 nil nil nil nil nil)
               captured)))))

(ert-deftest disco-api-delete-read-state-defaults-to-empty-payload ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok (disco-api-delete-read-state "987")))
      (should (equal
               '("DELETE" "/channels/987/messages/ack" nil nil nil nil nil nil)
               captured)))))

(provide 'disco-api-test)

;;; disco-api-test.el ends here
