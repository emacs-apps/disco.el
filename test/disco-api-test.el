;;; disco-api-test.el --- Tests for disco-api read-state wrappers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-api)

(defmacro disco-api-test--with-session-state (&rest body)
  "Run BODY with isolated API lifecycle and rate-limit state."
  (declare (indent 0) (debug t))
  `(let ((disco-api--generation 7)
         (disco-api--reset-in-progress nil)
         (disco-api--retry-owners nil)
         (disco-api--global-rate-limit-until 0.0)
         (disco-api--route-rate-limit-until
          (make-hash-table :test #'equal))
         (disco-api--route-bucket-map
          (make-hash-table :test #'equal))
         (disco-api--bucket-rate-limit-until
          (make-hash-table :test #'equal))
         (disco-rate-limit-safety-margin 0.0)
         (disco-rate-limit-max-retries 2))
     ,@body))

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

(ert-deftest disco-api-reset-barrier-rejects-sync-and-async-starts ()
  (disco-api-test--with-session-state
    (let ((disco-api--reset-in-progress t)
          transport-called)
      (cl-letf (((symbol-function 'disco-http-request)
                 (lambda (&rest _args)
                   (setq transport-called t)))
                ((symbol-function 'disco-http-request-async)
                 (lambda (&rest _args)
                   (setq transport-called t))))
        (should-error
         (disco-api--request "GET" "/barrier" nil nil t)
         :type 'user-error)
        (should-error
         (disco-api--request-async
          "GET" "/barrier" :unauthenticated t)
         :type 'user-error)
        (should-not transport-called)))))

(ert-deftest disco-api-sync-transport-reset-discards-retired-response ()
  (disco-api-test--with-session-state
    (let ((transport-calls 0))
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'disco-http-request)
                 (lambda (&rest _args)
                   (cl-incf transport-calls)
                   (disco-api-reset-rate-limit-state)
                   (list :status 200
                         :body "{\"old_account\":true}"
                         :headers
                         '((x-ratelimit-bucket . "retired-bucket")
                           (x-ratelimit-remaining . "0")
                           (x-ratelimit-reset-after . "30"))))))
        (should-error
         (disco-api--request "GET" "/sync-late" nil nil t)
         :type 'disco-api-error)
        (should (= transport-calls 1))
        (should (= disco-api--generation 8))
        (should (= (hash-table-count disco-api--route-bucket-map) 0))
        (should (= (hash-table-count
                    disco-api--bucket-rate-limit-until)
                   0))))))

(ert-deftest disco-api-sync-rate-limit-wait-reset-prevents-dispatch ()
  (disco-api-test--with-session-state
    (let (transport-called)
      (puthash "GET /sync-wait" 110.0
               disco-api--route-rate-limit-until)
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'sleep-for)
                 (lambda (&rest _args)
                   (disco-api-reset-rate-limit-state)))
                ((symbol-function 'disco-http-request)
                 (lambda (&rest _args)
                   (setq transport-called t))))
        (should-error
         (disco-api--request "GET" "/sync-wait" nil nil t)
         :type 'disco-api-error)
        (should-not transport-called)
        (should (= disco-api--generation 8))
        (should (= (hash-table-count
                    disco-api--route-rate-limit-until)
                   0))))))

(ert-deftest disco-api-sync-429-sleep-reset-prevents-old-token-retry ()
  (disco-api-test--with-session-state
    (let ((transport-calls 0)
          (sleep-calls 0))
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'sleep-for)
                 (lambda (&rest _args)
                   (cl-incf sleep-calls)
                   (disco-api-reset-rate-limit-state)))
                ((symbol-function 'disco-http-request)
                 (lambda (&rest _args)
                   (cl-incf transport-calls)
                   (list :status 429
                         :body "{\"retry_after\":3}"
                         :headers nil))))
        (should-error
         (disco-api--request "GET" "/sync-retry" nil nil t)
         :type 'disco-api-error)
        (should (= transport-calls 1))
        (should (= sleep-calls 1))
        (should (= disco-api--generation 8))
        (should (= (hash-table-count
                    disco-api--route-rate-limit-until)
                   0))))))

(ert-deftest disco-api-synchronous-timer-fire-cannot-resurrect-owner ()
  (disco-api-test--with-session-state
    (let (canceled-timers
          callback-ran)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (apply function args)
                   'returned-after-fire))
                ((symbol-function 'timerp)
                 (lambda (object)
                   (eq object 'returned-after-fire)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer canceled-timers))))
        (disco-api--schedule-session-timer
         7 1.0 (lambda () (setq callback-ran t)))
        (should callback-ran)
        (should-not disco-api--retry-owners)
        (should (equal canceled-timers '(returned-after-fire)))))))

(ert-deftest disco-api-fired-timer-retires-before-nested-reset ()
  (disco-api-test--with-session-state
    (let (timer-callback
          canceled-timers)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest args)
                   (setq timer-callback
                         (lambda () (apply function args)))
                   'nested-reset-timer))
                ((symbol-function 'timerp)
                 (lambda (object)
                   (eq object 'nested-reset-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer canceled-timers))))
        (disco-api--schedule-session-timer
         7 1.0 #'disco-api-reset-rate-limit-state)
        (should (= (length disco-api--retry-owners) 1))
        (funcall timer-callback)
        (should (= disco-api--generation 8))
        (should-not disco-api--retry-owners)
        ;; The fired owner retired its handle before invoking reset, so reset
        ;; must not rediscover and cancel that already-running callback.
        (should-not canceled-timers)))))

(ert-deftest disco-api-reset-cancels-pending-rate-limit-dispatch ()
  (disco-api-test--with-session-state
    (let (timer-callback
          scheduled-delay
          canceled-timers
          reentry-error
          (transport-calls 0))
      (puthash "GET /wait" 110.0 disco-api--route-rate-limit-until)
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'run-at-time)
                 (lambda (delay _repeat function &rest args)
                   (setq scheduled-delay delay
                         timer-callback
                         (lambda () (apply function args)))
                   'api-wait-timer))
                ((symbol-function 'timerp)
                 (lambda (object)
                   (eq object 'api-wait-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer canceled-timers)
                   ;; Cancellation is allowed to run instrumented user code,
                   ;; but the reset barrier must reject its successor work.
                   (setq reentry-error
                         (condition-case err
                             (progn
                               (disco-api--request-async
                                "GET" "/successor"
                                :unauthenticated t)
                               nil)
                           (user-error err)))))
                ((symbol-function 'disco-http-request-async)
                 (lambda (&rest _args)
                   (cl-incf transport-calls))))
        (disco-api--request-async
         "GET" "/wait" :unauthenticated t)
        (should (= scheduled-delay 10.0))
        (should (= (length disco-api--retry-owners) 1))
        (should (= transport-calls 0))

        (disco-api-reset-rate-limit-state)

        (should (= disco-api--generation 8))
        (should-not disco-api--retry-owners)
        (should (equal canceled-timers '(api-wait-timer)))
        (should (eq (car reentry-error) 'user-error))
        (should (= (hash-table-count
                    disco-api--route-rate-limit-until)
                   0))
        ;; Even a hostile scheduler invoking the canceled closure cannot send
        ;; a request carrying the retired session's Authorization header.
        (funcall timer-callback)
        (should (= transport-calls 0))))))

(ert-deftest disco-api-reset-cancels-429-retry ()
  (disco-api-test--with-session-state
    (let (request-options
          retry-callback
          retry-delay
          canceled-timers
          (transport-calls 0)
          (error-calls 0))
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'run-at-time)
                 (lambda (delay _repeat function &rest args)
                   (setq retry-delay delay
                         retry-callback
                         (lambda () (apply function args)))
                   'api-retry-timer))
                ((symbol-function 'timerp)
                 (lambda (object)
                   (eq object 'api-retry-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer canceled-timers)))
                ((symbol-function 'disco-http-request-async)
                 (lambda (&rest options)
                   (cl-incf transport-calls)
                   (setq request-options options))))
        (disco-api--request-async
         "GET" "/retry"
         :unauthenticated t
         :on-error (lambda (_error)
                     (cl-incf error-calls)))
        (funcall
         (plist-get request-options :on-success)
         (list :status 429
               :body "{\"retry_after\":3}"
               :headers nil))

        (should (= transport-calls 1))
        (should (= retry-delay 3.0))
        (should (= (length disco-api--retry-owners) 1))
        (should (= error-calls 0))

        (disco-api-reset-rate-limit-state)

        (should (equal canceled-timers '(api-retry-timer)))
        (should-not disco-api--retry-owners)
        (should (= disco-api--global-rate-limit-until 0.0))
        (should (= (hash-table-count
                    disco-api--route-rate-limit-until)
                   0))
        (funcall retry-callback)
        (should (= transport-calls 1))
        (should (= error-calls 0))))))

(ert-deftest disco-api-stale-http-responses-cannot-publish-or-refill-state ()
  (disco-api-test--with-session-state
    (let (request-options
          (success-calls 0)
          (error-calls 0)
          (retry-timers 0))
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (cl-incf retry-timers)))
                ((symbol-function 'disco-http-request-async)
                 (lambda (&rest options)
                   (setq request-options options))))
        (disco-api--request-async
         "GET" "/late"
         :unauthenticated t
         :on-success (lambda (_body)
                       (cl-incf success-calls))
         :on-error (lambda (_error)
                     (cl-incf error-calls)))
        (should request-options)

        (disco-api-reset-rate-limit-state)

        (funcall
         (plist-get request-options :on-success)
         (list :status 200
               :body "{\"ok\":true}"
               :headers
               '((x-ratelimit-bucket . "retired-bucket")
                 (x-ratelimit-remaining . "0")
                 (x-ratelimit-reset-after . "30"))))
        (funcall
         (plist-get request-options :on-error)
         (list :status 429
               :body "{\"global\":true,\"retry_after\":40}"
               :headers nil))

        (should (= success-calls 0))
        (should (= error-calls 0))
        (should (= retry-timers 0))
        (should (= disco-api--global-rate-limit-until 0.0))
        (should (= (hash-table-count disco-api--route-bucket-map) 0))
        (should (= (hash-table-count
                    disco-api--route-rate-limit-until)
                   0))
        (should (= (hash-table-count
                    disco-api--bucket-rate-limit-until)
                   0))))))

(ert-deftest disco-api-timer-constructor-reset-cancels-returned-handle ()
  (disco-api-test--with-session-state
    (let (canceled-timers
          constructor-entered
          transport-called)
      (puthash "GET /constructor" 110.0
               disco-api--route-rate-limit-until)
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (setq constructor-entered t)
                   (disco-api-reset-rate-limit-state)
                   'constructed-after-reset))
                ((symbol-function 'timerp)
                 (lambda (object)
                   (eq object 'constructed-after-reset)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer canceled-timers)))
                ((symbol-function 'disco-http-request-async)
                 (lambda (&rest _args)
                   (setq transport-called t))))
        (disco-api--request-async
         "GET" "/constructor" :unauthenticated t)
        (should constructor-entered)
        (should-not transport-called)
        (should (= disco-api--generation 8))
        (should-not disco-api--retry-owners)
        (should (equal canceled-timers '(constructed-after-reset)))
        (should (= (hash-table-count
                    disco-api--route-rate-limit-until)
                   0))))))

(ert-deftest disco-api-pure-rate-limit-clear-does-not-cancel-lifecycle ()
  (disco-api-test--with-session-state
    (let* ((owner (list :generation 7 :timer 'owned-timer))
           (disco-api--retry-owners (list owner))
           cancel-called)
      (setq disco-api--global-rate-limit-until 120.0)
      (puthash "GET /pure" 121.0
               disco-api--route-rate-limit-until)
      (puthash "GET /pure" "bucket"
               disco-api--route-bucket-map)
      (puthash "bucket" 122.0
               disco-api--bucket-rate-limit-until)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (&rest _args)
                   (setq cancel-called t))))
        (disco-api--clear-rate-limit-memory))
      (should-not cancel-called)
      (should (= disco-api--generation 7))
      (should (equal disco-api--retry-owners (list owner)))
      (should (eq (plist-get owner :timer) 'owned-timer))
      (should (= disco-api--global-rate-limit-until 0.0))
      (should (= (hash-table-count
                  disco-api--route-rate-limit-until)
                 0))
      (should (= (hash-table-count disco-api--route-bucket-map) 0))
      (should (= (hash-table-count
                  disco-api--bucket-rate-limit-until)
                 0)))))

(ert-deftest disco-api-fresh-generation-works-after-reset ()
  (disco-api-test--with-session-state
    (let (request-options
          success-body)
      (disco-api-reset-rate-limit-state)
      (cl-letf (((symbol-function 'disco-api--now)
                 (lambda () 100.0))
                ((symbol-function 'disco-http-request-async)
                 (lambda (&rest options)
                   (setq request-options options))))
        (disco-api--request-async
         "GET" "/fresh"
         :unauthenticated t
         :on-success (lambda (body)
                       (setq success-body body)))
        (funcall
         (plist-get request-options :on-success)
         (list :status 200
               :body "{\"ok\":true}"
               :headers
               '((x-ratelimit-bucket . "fresh-bucket")
                 (x-ratelimit-remaining . "0")
                 (x-ratelimit-reset-after . "2"))))
        (should (eq (alist-get 'ok success-body) t))
        (should (= disco-api--generation 8))
        (should (equal
                 (gethash "GET /fresh" disco-api--route-bucket-map)
                 "fresh-bucket"))
        (should (=
                 (gethash "fresh-bucket"
                          disco-api--bucket-rate-limit-until)
                 102.0))))))

(provide 'disco-api-test)

;;; disco-api-test.el ends here
