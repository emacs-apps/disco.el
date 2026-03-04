;;; disco-gateway-test.el --- Tests for disco-gateway read-state flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-gateway)

(ert-deftest disco-gateway-dispatch-message-create-classifies-own-message ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) nil))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message-id watched own-message)
                 (setq captured (list channel-id message-id watched own-message))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m1")
         (author . ((id . "u1")))))
      (should (equal '("c" "m1" nil t) captured)))))

(ert-deftest disco-gateway-dispatch-message-create-classifies-other-message ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) nil))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message-id watched own-message)
                 (setq captured (list channel-id message-id watched own-message))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m2")
         (author . ((id . "u2")))))
      (should (equal '("c" "m2" nil nil) captured)))))

(ert-deftest disco-gateway-dispatch-message-create-passes-watched-flag ()
  (let (captured)
    (setq disco-gateway--current-user-id "u1")
    (cl-letf (((symbol-function 'disco-gateway--channel-watched-p)
               (lambda (_channel-id) t))
              ((symbol-function 'disco-state-apply-message-create)
               (lambda (channel-id message-id watched own-message)
                 (setq captured (list channel-id message-id watched own-message))))
              ((symbol-function 'disco-gateway--emit)
               (lambda (_event) nil))
              ((symbol-function 'disco-gateway--upsert-message)
               (lambda (_channel-id _payload) nil)))
      (disco-gateway--dispatch-message-create
       '((channel_id . "c")
         (id . "m3")
         (author . ((id . "u2")))))
      (should (equal '("c" "m3" t nil) captured)))))

(provide 'disco-gateway-test)

;;; disco-gateway-test.el ends here
