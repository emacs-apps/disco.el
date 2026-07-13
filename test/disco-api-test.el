;;; disco-api-test.el --- Tests for disco-api read-state wrappers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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

(ert-deftest disco-api-ack-channel-pins-builds-endpoint ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok (disco-api-ack-channel-pins "555")))
      (should (equal
               '("POST" "/channels/555/pins/ack" nil nil nil nil nil nil)
               captured)))))

(ert-deftest disco-api-ack-user-feature-builds-endpoint-and-payload ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok (disco-api-ack-user-feature 'notification-center "456" :null)))
      (should (equal
               '("POST" "/users/@me/2/456/ack" ((token . :null)) nil nil nil nil nil)
               captured)))))

(ert-deftest disco-api-channel-messages-around-builds-query ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok (disco-api-channel-messages-around "123" "456" 50)))
      (should (equal
               '("GET" "/channels/123/messages" nil
                 (("limit" . "50") ("around" . "456"))
                 nil nil nil nil)
               captured)))))

(ert-deftest disco-api-channel-messages-async-builds-after-query ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request-async)
               (lambda (method endpoint &rest args)
                 (setq captured (list method endpoint args))
                 'request)))
      (should
       (eq 'request
           (disco-api-channel-messages-async
            "123"
            :after "456"
            :limit 25
            :on-success #'ignore
            :on-error #'ignore)))
      (should
       (equal
        '("GET" "/channels/123/messages"
          (:query (("limit" . "25") ("after" . "456"))
                  :on-success ignore
                  :on-error ignore))
        captured)))))

(ert-deftest disco-api-channel-messages-async-rejects-conflicting-cursors ()
  (should-error
   (disco-api-channel-messages-async "123" :before "100" :after "200")
   :type 'error))

(ert-deftest disco-api-preload-channel-messages-async-builds-payload ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request-async)
               (lambda (method endpoint &rest args)
                 (setq captured (list method endpoint args))
                 'request)))
      (should
       (eq 'request
           (disco-api-preload-channel-messages-async
            '("dm1" "dm2")
            :on-success #'ignore
            :on-error #'ignore)))
      (should
       (equal
        '("POST"
          "/channels/preload-messages"
          (:payload ((channel_ids "dm1" "dm2"))
                    :on-success ignore
                    :on-error ignore))
        captured)))))

(ert-deftest disco-api-preload-channel-messages-async-validates-batch ()
  (cl-letf (((symbol-function 'disco-api--request-async)
             (lambda (&rest _args)
               (ert-fail "invalid batch reached the transport"))))
    (should-error
     (disco-api-preload-channel-messages-async nil)
     :type 'error)
    (let ((disco-api-preload-channel-messages-limit 2))
      (should-error
       (disco-api-preload-channel-messages-async '("dm1" "dm2" "dm3"))
       :type 'error))))

(ert-deftest disco-api-channel-search-messages-tabs-builds-endpoint-and-payload ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok
                  (disco-api-channel-search-messages-tabs
                   "c1"
                   :tabs '((messages :limit 5 :content "foo" :sort-by timestamp :sort-order desc))
                   :track-exact-total-hits t)))
      (should (equal
               '("POST"
                 "/channels/c1/messages/search/tabs"
                 ((tabs (messages (limit . 5)
                                  (content . "foo")
                                  (sort_by . "timestamp")
                                  (sort_order . "desc")))
                  (track_exact_total_hits . t))
                 nil nil nil nil nil)
               captured)))))

(ert-deftest disco-api-guild-search-messages-builds-query ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok
                  (disco-api-guild-search-messages
                   "g1"
                   :channel-ids '("c1")
                   :content "foo"
                   :author-ids '("u1")
                   :limit 10
                   :max-id "99"
                   :sort-order 'asc)))
      (should (equal
               '("GET" "/guilds/g1/messages/search" nil
                 (("limit" . "10")
                  ("max_id" . "99")
                  ("content" . "foo")
                  ("author_id" . "u1")
                  ("sort_order" . "asc")
                  ("channel_id" . "c1"))
                 nil nil nil nil)
               captured)))))

(ert-deftest disco-api-channel-search-messages-builds-query ()
  (let (captured)
    (cl-letf (((symbol-function 'disco-api--request)
               (lambda (method endpoint &optional payload query unauthenticated raw-body extra-headers body-type)
                 (setq captured (list method endpoint payload query unauthenticated raw-body extra-headers body-type))
                 'ok)))
      (should (eq 'ok
                  (disco-api-channel-search-messages
                   "c1"
                   :content "foo"
                   :author-ids '("u1")
                   :limit 5
                   :min-id "10"
                   :sort-order 'desc)))
      (should (equal
               '("GET" "/channels/c1/messages/search" nil
                 (("limit" . "5")
                  ("min_id" . "10")
                  ("content" . "foo")
                  ("author_id" . "u1")
                  ("sort_order" . "desc"))
                 nil nil nil nil)
               captured)))))

(provide 'disco-api-test)

;;; disco-api-test.el ends here
