;;; disco-http-test.el --- HTTP session lifecycle tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco-http)

(ert-deftest disco-http-reset-revokes-direct-async-callbacks ()
  (let ((disco-http-serialize-requests nil)
        (disco-http--generation 7)
        (disco-http--reset-in-progress nil)
        (disco-http--direct-request-owners nil)
        then-callback
        else-callback
        published)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (setq then-callback (plist-get args :then)
                       else-callback (plist-get args :else))
                 'fake-process)))
      (disco-http-request-async
       :method "GET" :url "https://old.invalid" :timeout 1
       :on-success (lambda (_) (push 'success published))
       :on-error (lambda (_) (push 'error published)))
      (should (= 1 (length disco-http--direct-request-owners)))
      (disco-http-reset-queue-state)
      (should (= 8 disco-http--generation))
      (should-not disco-http--direct-request-owners)
      ;; Payloads need not be valid response objects: a stale callback must be
      ;; rejected before response normalization or user publication.
      (funcall then-callback 'old-response)
      (funcall else-callback 'old-error)
      (should-not published))))

(ert-deftest disco-http-direct-constructor-compensates-reset-before-return ()
  (let ((disco-http-serialize-requests nil)
        (disco-http--generation 2)
        (disco-http--reset-in-progress nil)
        (disco-http--direct-request-owners nil)
        published)
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest _)
                 (disco-http-reset-queue-state)
                 'stale-returned-process)))
      (disco-http-request-async
       :method "GET" :url "https://old.invalid" :timeout 1
       :on-success (lambda (_) (setq published t))
       :on-error (lambda (_) (setq published t)))
      (should (= 3 disco-http--generation))
      (should-not disco-http--direct-request-owners)
      (should-not published))))

(ert-deftest disco-http-reset-revokes-queue-before-synchronous-clear-callback ()
  (let ((disco-http--plz-queue 'old-queue)
        (disco-http--plz-queue-limit 4)
        (disco-http--generation 11)
        (disco-http--reset-in-progress nil)
        (disco-http--direct-request-owners nil)
        observed-queue
        reentrant-error)
    (cl-letf (((symbol-function 'plz-clear)
               (lambda (queue)
                 (setq observed-queue disco-http--plz-queue)
                 (condition-case err
                     (disco-http-request-async
                      :method "GET" :url "https://new.invalid" :timeout 1)
                   (error (setq reentrant-error err)))
                 queue)))
      (disco-http-reset-queue-state)
      (should-not observed-queue)
      (should reentrant-error)
      (should-not disco-http--plz-queue)
      (should-not disco-http--plz-queue-limit)
      (should (= 12 disco-http--generation)))))

(ert-deftest disco-http-public-entrypoints-reject-reset-reentry ()
  (let ((disco-http--reset-in-progress t)
        called)
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest _) (setq called t))))
      (should-error
       (disco-http-request
        :method "GET" :url "https://old.invalid" :timeout 1))
      (should-error
       (disco-http-request-async
        :method "GET" :url "https://old.invalid" :timeout 1))
      (should-not called))))

(provide 'disco-http-test)

;;; disco-http-test.el ends here
