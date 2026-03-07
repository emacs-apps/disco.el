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
         (masked (disco-markdown--hide-spoiler-text "spoiler"))
         (rendered (disco-markdown-render
                    "Look ||spoiler|| now"
                    :context 'test-internal-spoiler
                    :spoiler-message-id "m1"))
         (pos (string-match (regexp-quote masked) rendered)))
    (should pos)
    (should-not (string-match-p "spoiler" (substring-no-properties rendered)))
    (should (equal "m1"
                   (get-text-property pos 'disco-markdown-spoiler-message-id rendered)))
    (should (keymapp (get-text-property pos 'keymap rendered)))))

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
         (masked (disco-markdown--hide-spoiler-text "spoiler"))
         (rendered (disco-markdown-render
                    "Look ||spoiler|| now"
                    :context 'test-spoiler
                    :spoiler-message-id "m1"))
         (pos (string-match (regexp-quote masked) rendered)))
    (should pos)
    (should-not (string-match-p "spoiler" (substring-no-properties rendered)))
    (should (equal "m1"
                   (get-text-property pos 'disco-markdown-spoiler-message-id rendered)))
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
