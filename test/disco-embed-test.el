;;; disco-embed-test.el --- Tests for disco-embed -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-embed)

(ert-deftest disco-embed-stringify-uses-internal-markdown-renderer ()
  (let* ((disco-markdown-backend 'internal)
         (disco-embed--current-message '((id . "m1")))
         (disco-embed--current-spoiler-message-id "m1")
         (disco-embed--reveal-spoilers nil)
         (rendered (disco-embed--stringify "[link](https://example.com)\n> quote"))
         (plain (substring-no-properties rendered))
         (link-pos (string-match "link" plain))
         (quote-pos (string-match "quote" plain)))
    (should (equal "link\nquote" plain))
    (should (equal "https://example.com"
                   (get-text-property link-pos 'disco-markdown-url rendered)))
    (should (equal "| "
                   (substring-no-properties
                    (get-text-property quote-pos 'line-prefix rendered))))))

(ert-deftest disco-embed-stringify-passes-spoiler-context-to-internal-renderer ()
  (let* ((disco-markdown-backend 'internal)
         (disco-embed--current-message '((id . "m1")))
         (disco-embed--current-spoiler-message-id "m1")
         (disco-embed--reveal-spoilers nil)
         (rendered (disco-embed--stringify "|| spoiler ||"))
         (plain (substring-no-properties rendered))
         (pos (string-match " spoiler " plain)))
    (should pos)
    (should (equal " spoiler " plain))
    (should (equal "m1"
                   (get-text-property pos 'disco-markdown-spoiler-message-id rendered)))
    (should (equal "█"
                   (get-text-property pos 'display rendered)))))

(provide 'disco-embed-test)

;;; disco-embed-test.el ends here
