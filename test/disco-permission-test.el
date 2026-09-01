;;; disco-permission-test.el --- Tests for disco-permission helpers -*- lexical-binding: t; -*-

(require 'ert)

(require 'disco-permission)

(ert-deftest disco-permission-display-name-normalizes-designators ()
  (should (equal "VIEW_CHANNEL"
                 (disco-permission-display-name 'view-channel)))
  (should (equal "READ_MESSAGE_HISTORY"
                 (disco-permission-display-name :read-message-history)))
  (should (equal "0X10"
                 (disco-permission-display-name #x10))))

(ert-deftest disco-permission-channel-known-p-accepts-parseable-bitfields ()
  (should (disco-permission-channel-known-p '((permissions . "1024"))))
  (should (disco-permission-channel-known-p '((permissions . 1024))))
  (should-not (disco-permission-channel-known-p '((permissions . "abc"))))
  (should-not (disco-permission-channel-known-p '((id . "c1")))))

(ert-deftest disco-permission-ensure-channel-signals-user-error ()
  (should (disco-permission-ensure-channel
           '((permissions . "1024"))
           '(view-channel)
           :unknown-value nil))
  (should-error
   (disco-permission-ensure-channel
    '((permissions . "0"))
    '(view-channel read-message-history)
    :unknown-value nil
    :action "jump target channel 123")
   :type 'user-error))

(ert-deftest disco-permission-error-missing-access-p-detects-discord-errors ()
  (should (disco-permission-error-missing-access-p
           '(disco-api-error "Missing Access" 403 ((code . 50001)))))
  (should (disco-permission-error-missing-access-p
           '(disco-api-error "Missing Access" 403 ((message . "Missing Access")))))
  (should-not (disco-permission-error-missing-access-p
               '(disco-api-error "Forbidden" 403 ((code . 12345)))))
  (should-not (disco-permission-error-missing-access-p
               '(disco-api-error "Not Found" 404 ((code . 50001))))))

(provide 'disco-permission-test)

;;; disco-permission-test.el ends here
