;;; disco-runtime-test.el --- Disco vNext runtime tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-runtime)

(ert-deftest disco-runtime-owns-one-vnext-app-lifecycle ()
  (let ((disco-runtime--app nil)
        (shutdown-count 0))
    (cl-letf (((symbol-function 'disco-gateway-stop)
               (lambda () (cl-incf shutdown-count))))
      (unwind-protect
          (let* ((first (disco-runtime-app))
                 (second (disco-runtime-app))
                 (ticket (appkit-app-send first 'unexpected)))
            (should (eq first second))
            (should (eq disco-runtime--app first))
            (should (eq (appkit-app-type first) disco-runtime--app-type))
            (should (eq (appkit-app-model first) 'running))
            (should (eq (appkit-app-identity first) 'default))
            (should (eq (appkit-loop-ticket-state ticket) 'rejected))
            (should
             (equal (appkit-loop-ticket-outcome ticket)
                    '(disco-lifecycle-app-has-no-domain-messages unexpected)))
            (disco-runtime-stop)
            (should-not disco-runtime--app)
            (should (eq (appkit-app-status first) 'stopped))
            (should (= shutdown-count 1))
            (disco-runtime-stop)
            (should (= shutdown-count 1)))
        (when (appkit-app-p disco-runtime--app)
          (disco-runtime-stop))))))

(provide 'disco-runtime-test)

;;; disco-runtime-test.el ends here
