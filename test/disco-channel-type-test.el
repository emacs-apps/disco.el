;;; disco-channel-type-test.el --- Channel type helper tests -*- lexical-binding: t; -*-

(require 'ert)

(require 'disco-channel-type)

(ert-deftest disco-channel-obfuscated-p-requires-the-exact-channel-flag ()
  (should (disco-channel-obfuscated-p
           `((flags . ,disco-channel-flag-obfuscated))))
  (should (disco-channel-obfuscated-p
           `((flags . ,(logior disco-channel-flag-obfuscated (ash 1 3))))))
  (should-not (disco-channel-obfuscated-p '((flags . 0))))
  (should-not (disco-channel-obfuscated-p '((flags . 131071))))
  (should-not (disco-channel-obfuscated-p '((flags . "131072"))))
  (should-not (disco-channel-obfuscated-p '((flags . -1))))
  (should-not (disco-channel-obfuscated-p '((id . "c"))))
  (should-not (disco-channel-obfuscated-p nil)))

(provide 'disco-channel-type-test)

;;; disco-channel-type-test.el ends here
