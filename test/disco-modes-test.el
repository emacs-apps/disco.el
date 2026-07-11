;;; disco-modes-test.el --- Tests for Discord mode-line status -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco-modes)

(ert-deftest disco-client-mode-line-counts-known-messages-and-all-mentions ()
  (cl-letf (((symbol-function 'disco-state-channels)
             (lambda () '(((id . one)) ((id . two)) ((id . three)))))
            ((symbol-function 'disco-state-channel-unread-mention-count)
             (lambda (channel-id) (pcase channel-id ('one 0) ('two 3) (_ 2))))
            ((symbol-function 'disco-state-channel-muted-p)
             (lambda (channel) (eq (alist-get 'id channel) 'two)))
            ((symbol-function 'disco-state-channel-known-unread-message-count)
             (lambda (channel) (pcase (alist-get 'id channel)
                                 ('one 0) ('two 3) (_ 4)))))
    (let ((disco-client-mode-line--cached-counts
           (disco-client-mode-line--counts)))
      (should (equal '(4 . 5) disco-client-mode-line--cached-counts))
      (should (equal " 4" (substring-no-properties
                            (disco-client-mode-line-unread))))
      (should (equal " @5" (substring-no-properties
                             (disco-client-mode-line-mentions)))))))

(ert-deftest disco-client-mode-line-update-skips-unrelated-gateway-events ()
  (let ((disco-client-mode-line-mode t)
        (disco-client-mode-line--cached-counts '(2 . 3))
        (scans 0))
    (cl-letf (((symbol-function 'disco-client-mode-line--counts)
               (lambda () (cl-incf scans) '(4 . 5)))
              ((symbol-function 'appkit-mode-line-update-cache) #'ignore))
      (disco-client-mode-line-update '(:type typing-start))
      (should (= 0 scans))
      (should (equal '(2 . 3) disco-client-mode-line--cached-counts))
      (disco-client-mode-line-update '(:type message-create))
      (should (= 1 scans))
      (should (equal '(4 . 5) disco-client-mode-line--cached-counts)))))

(ert-deftest disco-client-mode-line-ready-event-refreshes-initial-counts ()
  (let ((disco-client-mode-line-mode t)
        (disco-client-mode-line--cached-counts '(0 . 0))
        (scans 0))
    (cl-letf (((symbol-function 'disco-client-mode-line--counts)
               (lambda () (cl-incf scans) '(7 . 2)))
              ((symbol-function 'appkit-mode-line-update-cache) #'ignore))
      (disco-client-mode-line-update '(:type ready))
      (should (= 1 scans))
      (should (equal '(7 . 2) disco-client-mode-line--cached-counts)))))

(ert-deftest disco-client-mode-line-mode-manages-provider-and-hook ()
  (let ((mode-line-misc-info nil)
        (disco-gateway-event-hook nil))
    (unwind-protect
        (progn
          (disco-client-mode-line-mode 1)
          (should (memq 'disco-client-mode-line-format mode-line-misc-info))
          (should (memq #'disco-client-mode-line-update
                        disco-gateway-event-hook)))
      (disco-client-mode-line-mode -1))
    (should-not (memq 'disco-client-mode-line-format mode-line-misc-info))
    (should-not (memq #'disco-client-mode-line-update
                      disco-gateway-event-hook))))

(provide 'disco-modes-test)

;;; disco-modes-test.el ends here
