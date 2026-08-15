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

(ert-deftest disco-channel-titles-use-conversation-domain-brackets ()
  (should (equal "{Alice}" (disco-channel-format-title 1 "Alice")))
  (should (equal "⦃Alice⦄" (disco-channel-format-title 18 "Alice")))
  (should
   (equal
    "(Study group)" (disco-channel-format-title 3 "Study group")))
  (should
   (equal "[general]" (disco-channel-format-title 0 "general")))
  (should (equal "⟨thread⟩" (disco-channel-format-title 11 "thread")))
  (should
   (equal "[[Emacs CN]]" (disco-title-format 'guild "Emacs CN")))
  (should (equal '("{" "}") (disco-channel-title-brackets 1)))
  (should (equal '("⦃" "⦄") (disco-channel-title-brackets 18)))
  (should (equal '("(" ")") (disco-channel-title-brackets 3)))
  (should (equal '("[" "]") (disco-channel-title-brackets 0)))
  (should (equal '("「" "」") (disco-channel-title-brackets 2)))
  (should (equal '("⟪" "⟫") (disco-channel-title-brackets 5)))
  (should (equal '("⟨" "⟩") (disco-channel-title-brackets 11)))
  (should (equal '("⦇" "⦈") (disco-channel-title-brackets 12)))
  (should (equal '("『" "』") (disco-channel-title-brackets 13)))
  (should (equal '("〔" "〕") (disco-channel-title-brackets 14)))
  (should (equal '("⟦" "⟧") (disco-channel-title-brackets 15)))
  (should (equal '("【" "】") (disco-channel-title-brackets 16)))
  (should (equal '("⌜" "⌝") (disco-channel-title-brackets 17)))
  (should (equal '("" "") (disco-channel-title-brackets 4))))

(ert-deftest disco-title-compact-count-formats-trails ()
  (should (equal "999" (disco-title-compact-count 999)))
  (should (equal "1.2k" (disco-title-compact-count 1234)))
  (should (equal "2m" (disco-title-compact-count 2000000))))

(provide 'disco-channel-type-test)

;;; disco-channel-type-test.el ends here
