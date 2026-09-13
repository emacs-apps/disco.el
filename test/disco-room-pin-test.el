;;; disco-room-pin-test.el --- Tests for room pin interaction -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-room)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest disco-room-pinned-messages-page-with-returned-timestamp ()
  (disco-room-test-with-runtime
   (let ((disco-runtime--app nil)
         buffer
         requests)
     (disco-state-reset)
     (disco-state-upsert-channel
      '((id . "pins")
        (name . "pins-room")
        (type . 0)
        (permissions . "2048")))
     (cl-letf (((symbol-function 'pop-to-buffer)
                (lambda (target &rest _args)
                  (setq buffer target)))
               ((symbol-function 'disco-api-channel-pins-async)
                (lambda (channel-id &rest args)
                  (push (cons channel-id args) requests)))
               ((symbol-function 'disco-gateway-stop) #'ignore))
       (unwind-protect
           (progn
             (disco-room-list-pinned-messages "pins")
             (should (buffer-live-p buffer))
             (should (equal "pins" (caar requests)))
             (should-not (plist-get (cdar requests) :before))
             (should (appkit-surface-p
                      (plist-get (cdar requests) :owner)))
             (funcall
              (plist-get (cdar requests) :on-success)
              '((items . (((pinned_at . "2026-08-16T02:00:00.000000+00:00")
                           (message . ((id . "m2") (content . "second"))))
                          ((pinned_at . "2026-08-16T01:00:00.000000+00:00")
                           (message . ((id . "m1") (content . "first"))))))
                (has_more . t)))
             (with-current-buffer buffer
               (should (equal '("m2" "m1")
                              (mapcar #'disco-room-pinned-messages--entry-message-id
                                      disco-room-pinned-messages--items)))
               (should (equal "2026-08-16T01:00:00.000000+00:00"
                              disco-room-pinned-messages--next-before))
               (disco-room-pinned-messages-load-more))
             (should (= 2 (length requests)))
             (should
              (equal "2026-08-16T01:00:00.000000+00:00"
                     (plist-get (cdr (car requests)) :before)))
             (funcall
              (plist-get (cdr (car requests)) :on-success)
              '((items . (((pinned_at . "2026-08-16T00:00:00.000000+00:00")
                           (message . ((id . "m1") (content . "duplicate"))))
                          ((pinned_at . "2026-08-15T23:00:00.000000+00:00")
                           (message . ((id . "m0") (content . "oldest"))))))
                (has_more . :false)))
             (with-current-buffer buffer
               (should (equal '("m2" "m1" "m0")
                              (mapcar #'disco-room-pinned-messages--entry-message-id
                                      disco-room-pinned-messages--items)))
               (should-not disco-room-pinned-messages--has-more-p)
               (appkit-surface-send (appkit-current-surface) 'render)
               (should (string-match-p "second" (buffer-string)))))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (disco-runtime-stop))))))

(ert-deftest disco-room-pinned-messages-refresh-rejects-stale-response ()
  (disco-room-test-with-runtime
   (let ((disco-runtime--app nil)
         buffer
         callbacks)
     (disco-state-reset)
     (disco-state-upsert-channel '((id . "pins") (type . 0) (permissions . "2048")))
     (cl-letf (((symbol-function 'pop-to-buffer)
                (lambda (target &rest _args)
                  (setq buffer target)))
               ((symbol-function 'disco-api-channel-pins-async)
                (lambda (_channel-id &rest args)
                  (push (plist-get args :on-success) callbacks)))
               ((symbol-function 'disco-gateway-stop) #'ignore))
       (unwind-protect
           (progn
             (disco-room-list-pinned-messages "pins")
             (with-current-buffer buffer
               (disco-room-pinned-messages-refresh))
             (funcall
              (car (last callbacks))
              '((items . (((pinned_at . "2026-08-16T00:00:00.000000+00:00")
                           (message . ((id . "stale") (content . "stale"))))))
                (has_more . :false)))
             (with-current-buffer buffer
               (should-not disco-room-pinned-messages--items))
             (funcall
              (car callbacks)
              '((items . (((pinned_at . "2026-08-16T01:00:00.000000+00:00")
                           (message . ((id . "current") (content . "current"))))))
                (has_more . :false)))
             (with-current-buffer buffer
               (should (equal '("current")
                              (mapcar #'disco-room-pinned-messages--entry-message-id
                                      disco-room-pinned-messages--items)))))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (disco-runtime-stop))))))

;;; disco-room-pin-test.el ends here
