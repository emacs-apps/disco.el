;;; disco-modes-test.el --- Tests for Discord mode-line status -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco-modes)

(ert-deftest disco-client-mode-line-counts-known-messages-and-all-mentions ()
  (cl-letf (((symbol-function 'disco-state-channels)
             (lambda () '(one two three)))
            ((symbol-function 'disco-state-channel-own-unread-count)
             (lambda (channel) (pcase channel ('one 0) ('two 3) (_ 2))))
            ((symbol-function 'disco-state-channel-muted-p)
             (lambda (channel) (eq channel 'two)))
            ((symbol-function 'disco-state-channel-known-unread-message-count)
             (lambda (channel) (pcase channel ('one 0) ('two 3) (_ 4)))))
    (should (equal '(4 . 5) (disco-client-mode-line--counts)))
    (should (equal " 4" (substring-no-properties
                          (disco-client-mode-line-unread))))
    (should (equal " @5" (substring-no-properties
                           (disco-client-mode-line-mentions))))))

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
