;;; disco-markdown-test.el --- Tests for disco-markdown -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(let ((markdown-mode-dir "/home/_WD_/.config/emacs/.local/straight/repos/markdown-mode"))
  (when (file-directory-p markdown-mode-dir)
    (add-to-list 'load-path markdown-mode-dir)))

(require 'disco-markdown)

(ert-deftest disco-markdown-render-internal-inline-links-are-openable ()
  (let ((disco-markdown-backend 'internal))
    (let* ((rendered (disco-markdown-render
                      "hello [link](https://example.com)"
                      :context 'test-internal-link))
           (pos (string-match "link" rendered)))
      (should pos)
      (should (equal "hello link" (substring-no-properties rendered)))
      (should (equal "https://example.com"
                     (get-text-property pos 'disco-markdown-url rendered)))
      (should (disco-markdown--face-match-p
               (get-text-property pos 'face rendered)
               'disco-markdown-link-face))
      (should (keymapp (get-text-property pos 'keymap rendered))))))

(ert-deftest disco-markdown-render-internal-bare-links-are-openable ()
  (let ((disco-markdown-backend 'internal))
    (let* ((rendered (disco-markdown-render
                      "hello https://example.com"
                      :context 'test-internal-bare-link))
           (pos (string-match "https://example.com" rendered)))
      (should pos)
      (should (equal "https://example.com"
                     (get-text-property pos 'disco-markdown-url rendered)))
      (should (disco-markdown--face-match-p
               (get-text-property pos 'face rendered)
               'disco-markdown-link-face))
      (should (keymapp (get-text-property pos 'keymap rendered))))))

(ert-deftest disco-markdown-render-internal-angle-links-are-openable ()
  (let ((disco-markdown-backend 'internal))
    (let* ((rendered (disco-markdown-render
                      "hello <https://example.com>"
                      :context 'test-internal-angle-link))
           (pos (string-match "https://example.com" rendered)))
      (should pos)
      (should (equal "hello https://example.com"
                     (substring-no-properties rendered)))
      (should (equal "https://example.com"
                     (get-text-property pos 'disco-markdown-url rendered)))
      (should (keymapp (get-text-property pos 'keymap rendered))))))

(ert-deftest disco-markdown-render-internal-spoilers-mask-and-tag-message ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "Look ||spoiler|| now"
                    :context 'test-internal-spoiler
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (pos (string-match "spoiler" plain)))
    (should pos)
    (should (equal "Look spoiler now" plain))
    (should (equal "m1"
                   (get-text-property pos 'disco-markdown-spoiler-message-id rendered)))
    (should (equal "█"
                   (get-text-property pos 'display rendered)))
    (should (keymapp (get-text-property pos 'keymap rendered)))))

(ert-deftest disco-markdown-render-internal-spoilers-mask-edge-spaces ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "Look || spoiler || now"
                    :context 'test-internal-spoiler-spaces
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (pos (string-match " spoiler " plain)))
    (should pos)
    (should (equal "Look  spoiler  now" plain))
    (should (equal "█" (get-text-property pos 'display rendered)))
    (should (equal "█" (get-text-property (1- (match-end 0)) 'display rendered)))))

(ert-deftest disco-markdown-render-internal-subtitle-lines-strip-marker ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render "-# Small print"
                                          :context 'test-internal-subtitle))
         (plain (substring-no-properties rendered)))
    (should (equal "Small print" plain))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-subtitle-face))))

(ert-deftest disco-markdown-render-internal-headings-strip-markers-for-all-levels ()
  (let ((disco-markdown-backend 'internal))
    (dolist (entry '(("# One" . disco-markdown-heading-1-face)
                     ("## Two" . disco-markdown-heading-2-face)
                     ("### Three" . disco-markdown-heading-3-face)
                     ("#### Four" . disco-markdown-heading-4-face)))
      (let* ((rendered (disco-markdown-render (car entry)
                                              :context 'test-internal-heading))
             (plain (substring-no-properties rendered)))
        (should-not (string-prefix-p "#" plain))
        (should (disco-markdown--face-match-p
                 (get-text-property 0 'face rendered)
                 (cdr entry)))))))

(ert-deftest disco-markdown-render-internal-emphasis-strips-markers ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "**bold** *italic* __under__ ~~strike~~"
                    :context 'test-internal-emphasis))
         (plain (substring-no-properties rendered))
         (bold-pos (string-match "bold" plain))
         (italic-pos (string-match "italic" plain))
         (underline-pos (string-match "under" plain))
         (strike-pos (string-match "strike" plain)))
    (should (equal "bold italic under strike" plain))
    (should (disco-markdown--face-match-p
             (get-text-property bold-pos 'face rendered)
             'disco-markdown-strong-face))
    (should (disco-markdown--face-match-p
             (get-text-property italic-pos 'face rendered)
             'disco-markdown-emphasis-face))
    (should (disco-markdown--face-match-p
             (get-text-property underline-pos 'face rendered)
             'disco-markdown-underline-face))
    (should (disco-markdown--face-match-p
             (get-text-property strike-pos 'face rendered)
             'disco-markdown-strikethrough-face))))

(ert-deftest disco-markdown-render-internal-inline-code-protects-content ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "`https://example.com` `<@123>` `||spoiler||`"
                    :context 'test-internal-inline-code
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (url-pos (string-match "https://example.com" plain))
         (mention-pos (string-match "<@123>" plain))
         (spoiler-pos (string-match "||spoiler||" plain)))
    (should (equal "https://example.com <@123> ||spoiler||" plain))
    (should (disco-markdown--face-match-p
             (get-text-property url-pos 'face rendered)
             'disco-markdown-code-face))
    (should-not (get-text-property url-pos 'disco-markdown-url rendered))
    (should-not (get-text-property mention-pos 'disco-markdown-spoiler-message-id rendered))
    (should-not (get-text-property spoiler-pos 'disco-markdown-spoiler-message-id rendered))))

(ert-deftest disco-markdown-render-internal-fenced-code-protects-block-content ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "```elisp\n# Title\n-# subtitle\nhttps://example.com\n<@123>\n```"
                    :context 'test-internal-fence
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (heading-pos (string-match "# Title" plain))
         (subtitle-pos (string-match "-# subtitle" plain))
         (url-pos (string-match "https://example.com" plain))
         (mention-pos (string-match "<@123>" plain)))
    (should (equal "# Title\n-# subtitle\nhttps://example.com\n<@123>\n" plain))
    (dolist (pos (list heading-pos subtitle-pos url-pos mention-pos))
      (should (disco-markdown--face-match-p
               (get-text-property pos 'face rendered)
               'disco-markdown-code-face)))
    (should-not (get-text-property heading-pos 'disco-markdown-url rendered))
    (should-not (get-text-property url-pos 'disco-markdown-url rendered))
    (should-not (get-text-property mention-pos 'disco-markdown-spoiler-message-id rendered))))

(ert-deftest disco-markdown-render-internal-fenced-code-uses-language-highlighting ()
  (let* ((disco-markdown-backend 'internal)
         (disco-markdown-fontify-code-blocks-natively t)
         (rendered (disco-markdown-render
                    "```elisp\n(let ((x 1))\n  x)\n```"
                    :context 'test-internal-fence-highlight))
         (plain (substring-no-properties rendered))
         (let-pos (string-match "let" plain)))
    (should let-pos)
    (should (equal "(let ((x 1))\n  x)\n" plain))
    (should (disco-markdown--face-match-p
             (get-text-property let-pos 'face rendered)
             'disco-markdown-code-face))
    (should (disco-markdown--face-match-p
             (get-text-property let-pos 'face rendered)
             'font-lock-keyword-face))))

(ert-deftest disco-markdown-render-internal-blockquotes-add-prefix-and-face ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render "> quoted"
                                          :context 'test-internal-blockquote))
         (plain (substring-no-properties rendered))
         (prefix (get-text-property 0 'line-prefix rendered)))
    (should (equal "quoted" plain))
    (should (stringp prefix))
    (should (equal "| " (substring-no-properties prefix)))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-blockquote-face))))

(ert-deftest disco-markdown-render-internal-blockquote-rest-quotes-following-lines ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render ">>> first line\nsecond line"
                                          :context 'test-internal-blockquote-rest))
         (plain (substring-no-properties rendered))
         (second-pos (string-match "second" plain)))
    (should (equal "first line\nsecond line" plain))
    (should (equal "| "
                   (substring-no-properties
                    (get-text-property 0 'line-prefix rendered))))
    (should (equal "| "
                   (substring-no-properties
                    (get-text-property second-pos 'line-prefix rendered))))))

(ert-deftest disco-markdown-render-internal-blockquote-allows-headings-inside ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render "> # Quoted Title"
                                          :context 'test-internal-blockquote-heading))
         (plain (substring-no-properties rendered)))
    (should (equal "Quoted Title" plain))
    (should (equal "| "
                   (substring-no-properties
                    (get-text-property 0 'line-prefix rendered))))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-heading-1-face))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-blockquote-face))))

(ert-deftest disco-markdown-render-internal-unordered-lists-normalize-markers ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render "* one\n  + two\n- three"
                                          :context 'test-internal-list))
         (plain (substring-no-properties rendered))
         (two-pos (string-match "two" plain))
         (three-pos (string-match "three" plain)))
    (should (equal "- one\n  - two\n- three" plain))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-list-marker-face))
    (should (disco-markdown--face-match-p
             (get-text-property (- two-pos 2) 'face rendered)
             'disco-markdown-list-marker-face))
    (should (disco-markdown--face-match-p
             (get-text-property (- three-pos 2) 'face rendered)
             'disco-markdown-list-marker-face))))

(ert-deftest disco-markdown-render-internal-escapes-protect-inline-markup ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "\\*literal\\* \\||spoiler||"
                    :context 'test-internal-escapes-inline
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (literal-pos (string-match "literal" plain))
         (spoiler-pos (string-match "spoiler" plain)))
    (should (equal "*literal* ||spoiler||" plain))
    (should-not (disco-markdown--face-match-p
                 (get-text-property literal-pos 'face rendered)
                 'disco-markdown-emphasis-face))
    (should-not (get-text-property spoiler-pos
                                   'disco-markdown-spoiler-message-id
                                   rendered))))

(ert-deftest disco-markdown-render-internal-escapes-protect-block-markup ()
  (let* ((disco-markdown-backend 'internal)
         (rendered (disco-markdown-render
                    "\\> not quote\n\\- not list\n\\# not heading\n\\-# not subtitle"
                    :context 'test-internal-escapes-block))
         (plain (substring-no-properties rendered))
         (list-pos (string-match "- not list" plain))
         (heading-pos (string-match "# not heading" plain))
         (subtitle-pos (string-match "-# not subtitle" plain)))
    (should (equal "> not quote\n- not list\n# not heading\n-# not subtitle"
                   plain))
    (should-not (get-text-property 0 'line-prefix rendered))
    (should-not (disco-markdown--face-match-p
                 (get-text-property list-pos 'face rendered)
                 'disco-markdown-list-marker-face))
    (should-not (disco-markdown--face-match-p
                 (get-text-property heading-pos 'face rendered)
                 'disco-markdown-heading-1-face))
    (should-not (disco-markdown--face-match-p
                 (get-text-property subtitle-pos 'face rendered)
                 'disco-markdown-subtitle-face))))

(ert-deftest disco-markdown-render-inline-links-are-openable ()
  (skip-unless (disco-markdown--markdown-mode-available-p))
  (let ((disco-markdown-backend 'markdown-mode))
    (let* ((rendered (disco-markdown-render
                      "hello [link](https://example.com)"
                      :context 'test-link))
           (pos (string-match "link" rendered)))
      (should pos)
      (should (equal "https://example.com"
                     (get-text-property pos 'disco-markdown-url rendered)))
      (should (keymapp (get-text-property pos 'keymap rendered))))))

(ert-deftest disco-markdown-render-bare-links-are-openable ()
  (skip-unless (disco-markdown--markdown-mode-available-p))
  (let ((disco-markdown-backend 'markdown-mode))
    (let* ((rendered (disco-markdown-render
                      "hello https://example.com"
                      :context 'test-bare-link))
           (pos (string-match "https://example.com" rendered)))
      (should pos)
      (should (equal "https://example.com"
                     (get-text-property pos 'disco-markdown-url rendered)))
      (should (keymapp (get-text-property pos 'keymap rendered))))))

(ert-deftest disco-markdown-render-spoilers-mask-and-tag-message ()
  (skip-unless (disco-markdown--markdown-mode-available-p))
  (let* ((disco-markdown-backend 'markdown-mode)
         (rendered (disco-markdown-render
                    "Look ||spoiler|| now"
                    :context 'test-spoiler
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (pos (string-match "spoiler" plain)))
    (should pos)
    (should (equal "Look spoiler now" plain))
    (should (equal "m1"
                   (get-text-property pos 'disco-markdown-spoiler-message-id rendered)))
    (should (equal "█"
                   (get-text-property pos 'display rendered)))
    (should (keymapp (get-text-property pos 'keymap rendered)))))

(ert-deftest disco-markdown-render-spoilers-can-be-revealed ()
  (skip-unless (disco-markdown--markdown-mode-available-p))
  (let* ((disco-markdown-backend 'markdown-mode)
         (rendered (disco-markdown-render
                    "Look ||spoiler|| now"
                    :context 'test-spoiler-reveal
                    :spoiler-message-id "m1"
                    :reveal-spoilers t))
         (plain (substring-no-properties rendered))
         (pos (string-match "spoiler" plain)))
    (should pos)
    (should (string-match-p "spoiler" plain))
    (should (equal "m1"
                   (get-text-property pos 'disco-markdown-spoiler-message-id rendered)))))

(ert-deftest disco-markdown-render-subtitle-lines-strip-marker ()
  (skip-unless (disco-markdown--markdown-mode-available-p))
  (let* ((disco-markdown-backend 'markdown-mode)
         (rendered (disco-markdown-render "-# Small print"
                                          :context 'test-subtitle))
         (plain (substring-no-properties rendered)))
    (should (equal "Small print" plain))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-subtitle-face))))

(ert-deftest disco-markdown-render-headings-strip-markers-for-all-levels ()
  (skip-unless (disco-markdown--markdown-mode-available-p))
  (let ((disco-markdown-backend 'markdown-mode))
    (dolist (entry '(("# One" . disco-markdown-heading-1-face)
                     ("## Two" . disco-markdown-heading-2-face)
                     ("### Three" . disco-markdown-heading-3-face)
                     ("#### Four" . disco-markdown-heading-4-face)))
      (let* ((rendered (disco-markdown-render (car entry)
                                              :context 'test-heading))
             (plain (substring-no-properties rendered)))
        (should-not (string-prefix-p "#" plain))
        (should (disco-markdown--face-match-p
                 (get-text-property 0 'face rendered)
                 (cdr entry)))))))

(provide 'disco-markdown-test)

;;; disco-markdown-test.el ends here
