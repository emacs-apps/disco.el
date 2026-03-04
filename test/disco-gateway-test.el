;;; disco-gateway-test.el --- Tests for disco-gateway read-state flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-gateway)

(ert-deftest disco-gateway-dispatch-message-create-passes-current-user-id ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) nil))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message current-user-id watched)
                 (setq captured (list channel-id message current-user-id watched))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m1")
         (author . ((id . "u1")))))
      (should (equal '("c"
                       ((channel_id . "c")
                        (id . "m1")
                        (author (id . "u1")))
                       "u1"
                       nil)
                     captured)))))

(ert-deftest disco-gateway-dispatch-message-create-passes-watched-flag ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) t))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message current-user-id watched)
                 (setq captured (list channel-id message current-user-id watched))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m3")
         (author . ((id . "u2")))))
      (should (equal '("c"
                       ((channel_id . "c")
                        (id . "m3")
                        (author (id . "u2")))
                       "u1"
                       t)
                     captured)))))

(ert-deftest disco-gateway-dispatch-message-create-upserts-when-watched ()
  (let (upsert-called)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) t))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (_channel-id _message _current-user-id _watched) nil))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (channel-id payload)
                 (setq upsert-called (list channel-id payload)))))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m4")
         (author . ((id . "u2")))))
      (should (equal '("c"
                       ((channel_id . "c")
                        (id . "m4")
                        (author (id . "u2"))))
                     upsert-called)))))

(provide 'disco-gateway-test)

;;; disco-gateway-test.el ends here
