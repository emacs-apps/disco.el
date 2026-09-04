;;; disco-room-thread-test.el --- Tests for room thread interactions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-room)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))
(require 'disco-room-thread)
(require 'disco-state)

(ert-deftest disco-room-thread-create-from-message-errors-without-create-public-threads ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "chat"
      (setq-local disco-room--channel-id "chat")
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
      (should-error (disco-room-thread-create-from-message "topic" "m1") :type 'user-error))))

(ert-deftest disco-room-thread-create-errors-without-create-private-threads ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "chat"
      (setq-local disco-room--channel-id "chat")
      (disco-state-reset)
      (disco-state-upsert-channel
       `((id . "chat")
         (type . 0)
         (guild_id . "g1")
         (permissions . ,(number-to-string (ash 1 35)))))
      (should-error (disco-room-thread-create "topic" 12 nil nil nil) :type 'user-error))))

(ert-deftest disco-room-thread-rename-errors-when-archived ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "thread"
      (setq-local disco-room--channel-id "thread")
      (disco-state-reset)
      (disco-state-upsert-channel
       `((id . "thread")
         (type . 11)
         (guild_id . "g1")
         (permissions . ,(number-to-string (ash 1 34)))
         (thread_metadata . ((archived . t)))))
      (should-error (disco-room-thread-rename "new-name") :type 'user-error))))

(ert-deftest disco-room-thread-toggle-archived-errors-without-manage-threads ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "thread"
      (setq-local disco-room--channel-id "thread")
      (disco-state-reset)
      (disco-state-upsert-channel
       `((id . "thread")
         (type . 11)
         (guild_id . "g1")
         (permissions . ,(number-to-string (ash 1 38)))
         (thread_metadata . ((archived . :false) (locked . :false)))))
      (should-error (disco-room-thread-toggle-archived) :type 'user-error))))

(ert-deftest disco-room-thread-join-errors-when-already-joined ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "thread"
      (setq-local disco-room--channel-id "thread")
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "thread") (type . 11) (guild_id . "g1")
                                    (thread_metadata . ((archived . :false)))))
      (disco-state-upsert-thread-member "thread" "u1")
      (cl-letf (((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1")))
        (should-error (disco-room-thread-join) :type 'user-error)))))

(ert-deftest disco-room-thread-leave-errors-when-not-joined ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "thread"
      (setq-local disco-room--channel-id "thread")
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "thread") (type . 11) (guild_id . "g1")
                                    (thread_metadata . ((archived . :false)))))
      (cl-letf (((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1")))
        (should-error (disco-room-thread-leave) :type 'user-error)))))

(ert-deftest disco-room-thread-set-muted-errors-when-not-joined ()
  (disco-room-test-with-runtime
    (disco-room-test-with-surface "thread"
      (setq-local disco-room--channel-id "thread")
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "thread") (type . 11) (guild_id . "g1")
                                    (thread_metadata . ((archived . :false)))))
      (cl-letf (((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1")))
        (should-error (disco-room-thread-set-muted t) :type 'user-error)))))

(ert-deftest disco-room-thread-rename-commits-through-controller-invalidation ()
  (disco-room-test-with-surface "thread"
    (disco-state-upsert-channel
     `((id . "thread") (name . "old-name") (type . 11)
       (guild_id . "g1")
       (permissions . ,(number-to-string (ash 1 34)))
       (thread_metadata . ((archived . :false)))))
    (cl-letf (((symbol-function 'disco-api-update-thread)
               (lambda (_channel-id &rest _)
                 `((id . "thread") (name . "new-name") (type . 11)
                   (guild_id . "g1")
                   (permissions . ,(number-to-string (ash 1 34)))
                   (thread_metadata . ((archived . :false)))))))
      (disco-room-thread-rename "new-name"))
    (disco-room-test-drain surface)
    (should (equal "new-name" (alist-get 'name (disco-state-channel "thread"))))
    (should (equal "new-name" disco-room--channel-name))))
