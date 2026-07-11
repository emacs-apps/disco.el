;;; disco-api-normalize-test.el --- Tests for disco-api-normalize -*- lexical-binding: t; -*-

(require 'ert)

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

(ert-deftest disco-api-normalize-allowed-mentions-rejects-plist ()
  (should-error (disco-api--normalize-allowed-mentions '(:parse ["users"]))
                :type 'error))

(ert-deftest disco-api-normalize-message-reference-rejects-plist ()
  (should-error (disco-api--normalize-message-reference
                 '(:type forward :message_id "1" :channel_id "2")
                 nil)
                :type 'error))

(ert-deftest disco-api-normalize-forward-only-rejects-plist ()
  (should-error (disco-api--normalize-message-forward-only
                 '(:embed_indices [0]))
                :type 'error))

(ert-deftest disco-api-normalize-message-content-length-limit ()
  (let ((max (make-string disco-api--message-content-limit ?a))
        (too-long (make-string (1+ disco-api--message-content-limit) ?a)))
    (should (equal `((content . ,max))
                   (disco-api--message-send-payload max nil nil nil nil nil)))
    (should (equal `((content . ,max))
                   (disco-api--message-edit-payload max nil)))
    (should-error (disco-api--message-send-payload too-long nil nil nil nil nil)
                  :type 'error)
    (should-error (disco-api--message-edit-payload too-long nil)
                  :type 'error)))

(ert-deftest disco-api-normalize-message-send-payload-trims-before-limit-check ()
  (let* ((trimmed (make-string disco-api--message-content-limit ?a))
         (content (concat trimmed "   ")))
    (should (equal `((content . ,trimmed))
                   (disco-api--message-send-payload content nil nil nil nil nil)))))

(ert-deftest disco-api-normalize-message-send-payload-enforces-nonce ()
  (let ((payload (disco-api--message-send-payload
                  "hello" nil nil nil nil nil "12345")))
    (should (equal "12345" (alist-get 'nonce payload)))
    (should (eq t (alist-get 'enforce_nonce payload)))))

(ert-deftest disco-api-normalize-token-payload ()
  (should (eq :empty-object (disco-api--token-payload nil)))
  (should (equal '((token . "tok")) (disco-api--token-payload "tok")))
  (should (equal '((token . :null)) (disco-api--token-payload :null)))
  (should-error (disco-api--token-payload 42) :type 'error))

(ert-deftest disco-api-normalize-message-search-query ()
  (should
   (equal '(("limit" . "10")
            ("max_id" . "99")
            ("content" . "openclaw")
            ("author_type" . "user")
            ("author_id" . "u1")
            ("mentions" . "u2")
            ("pinned" . "true")
            ("sort_by" . "relevance")
            ("sort_order" . "asc")
            ("channel_id" . "c1"))
          (disco-api--message-search-query
           :limit 10
           :max-id "99"
           :content "openclaw"
           :author-types '("user")
           :author-ids '("u1")
           :mentions '("u2")
           :pinned t
           :sort-by 'relevance
           :sort-order 'asc
           :channel-ids '("c1")))))

(ert-deftest disco-api-normalize-message-search-tab-payload ()
  (should
   (equal '((limit . 10)
            (slop . 2)
            (content . "openclaw")
            (author_type . ["user" "bot"])
            (has . ["link" "file"])
            (pinned . t)
            (sort_by . "timestamp")
            (sort_order . "desc"))
          (disco-api--message-search-tab-payload
           :limit 10
           :slop 2
           :content "openclaw"
           :author-types '("user" "bot")
           :has '("link" "file")
           :pinned t
           :sort-by 'timestamp
           :sort-order 'desc))))

(ert-deftest disco-api-normalize-message-search-tabs-payload ()
  (should
   (equal '((tabs (messages (limit . 5)
                            (content . "foo")
                            (sort_by . "timestamp")
                            (sort_order . "desc"))
                  (links (limit . 5)
                         (content . "foo")
                         (has . ["link"]) 
                         (sort_by . "timestamp")
                         (sort_order . "desc")))
            (channel_ids . ["1" "2"])
            (track_exact_total_hits . t))
          (disco-api--message-search-tabs-payload
           :tabs '((messages :limit 5 :content "foo" :sort-by timestamp :sort-order desc)
                   (links :limit 5 :content "foo" :has ("link")
                          :sort-by timestamp :sort-order desc))
           :channel-ids '("1" "2")
           :include-nsfw nil
           :track-exact-total-hits t))))

(ert-deftest disco-api-normalize-read-state-type ()
  (should (= 0 (disco-api--normalize-read-state-type nil "read_state_type" 0)))
  (should (= 1 (disco-api--normalize-read-state-type 'guild-event "read_state_type" nil)))
  (should (= 2 (disco-api--normalize-read-state-type 2 "read_state_type" nil)))
  (should-error (disco-api--normalize-read-state-type 'unknown "read_state_type" nil)
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
