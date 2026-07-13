;;; disco-markdown-test.el --- Tests for disco-markdown -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mouse)

(require 'disco-markdown)

(defun disco-markdown-test--primary-click (window position)
  "Return a real primary-click event pair in WINDOW at POSITION."
  (let ((posn (list window position '(0 . 0) 0 nil position)))
    (vector (list 'down-mouse-1 posn)
            (list 'mouse-1 posn))))

(ert-deftest disco-markdown-render-internal-inline-links-are-openable ()
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
    (let ((map (get-text-property pos 'keymap rendered)))
      (should (keymapp map))
      (should (eq (lookup-key map [mouse-1])
                  #'disco-markdown-open-at-point)))
    (should-not (get-text-property pos 'follow-link rendered))))

(ert-deftest disco-markdown-link-dispatches-exact-primary-click ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((rendered
              (disco-markdown-render
               "[first](https://first.example) [second](https://second.example)"
               :context 'test-exact-link-click))
             (first-pos (string-match "first" rendered))
             (second-pos (string-match "second" rendered))
             opened)
        (insert rendered)
        (switch-to-buffer (current-buffer))
        (goto-char (1+ first-pos))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _args) (setq opened url))))
          (let ((mouse-1-click-follows-link 450))
            (execute-kbd-macro
             (disco-markdown-test--primary-click
              (selected-window) (1+ second-pos)))))
        (should (equal "https://second.example" opened))))))

(ert-deftest disco-markdown-multiline-link-keeps-dedicated-exact-click-map ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((payload (disco-markdown--make-link-string
                       "first line\nsecond line" "https://example.com/multiline"))
             (second-line-pos (1+ (string-match "second" payload)))
             opened)
        (insert payload)
        (should (eq (lookup-key (get-text-property second-line-pos 'keymap)
                                [mouse-1])
                    #'disco-markdown-open-at-point))
        (should-not (get-text-property second-line-pos 'follow-link))
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _args) (setq opened url))))
          (let ((mouse-1-click-follows-link 450))
            (execute-kbd-macro
             (disco-markdown-test--primary-click
              (selected-window) second-line-pos))))
        (should (equal "https://example.com/multiline" opened))))))

(ert-deftest disco-markdown-spoiler-dispatches-exact-primary-click ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((first (disco-markdown--make-spoiler-string "first" "m1" nil))
             (second (disco-markdown--make-spoiler-string "second" "m2" nil))
             (second-pos (+ (length first) 2))
             toggled)
        (insert first " " second)
        (should-not (get-text-property second-pos 'follow-link))
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'disco-room-toggle-message-spoilers)
                   (lambda (message-id) (setq toggled message-id))))
          (let ((mouse-1-click-follows-link 450))
            (execute-kbd-macro
             (disco-markdown-test--primary-click
              (selected-window) second-pos))))
        (should (equal "m2" toggled))))))

(ert-deftest disco-markdown-open-at-point-does-not-fall-back-to-line-start ()
  (with-temp-buffer
    (insert (disco-markdown--make-link-string "link" "https://example.com")
            " blank")
    (goto-char (point-max))
    (should-error (disco-markdown-open-at-point) :type 'user-error)))

(ert-deftest disco-markdown-render-internal-inline-links-support-escaped-delimiters ()
  (let* ((rendered (disco-markdown-render
                    "[te\\]st](https://example.com/a\\)b)"
                    :context 'test-internal-link-escapes))
         (plain (substring-no-properties rendered))
         (pos (string-match "te]st" plain)))
    (should pos)
    (should (equal "te]st" plain))
    (should (equal "https://example.com/a)b"
                   (get-text-property pos 'disco-markdown-url rendered)))
    (should (keymapp (get-text-property pos 'keymap rendered)))))

(ert-deftest disco-markdown-render-internal-escaped-links-stay-literal ()
  (dolist (entry '(("\\[link](https://example.com)" . "[link](https://example.com)")
                   ("\\<https://example.com>" . "<https://example.com>")))
    (let* ((rendered (disco-markdown-render (car entry)
                                            :context 'test-internal-link-literal))
           (plain (substring-no-properties rendered))
           (url-pos (string-match "https://example.com" plain)))
      (should (equal (cdr entry) plain))
      (should url-pos)
      (should-not (get-text-property url-pos 'disco-markdown-url rendered)))))

(ert-deftest disco-markdown-render-internal-bare-links-are-openable ()
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
    (should (keymapp (get-text-property pos 'keymap rendered)))))

(ert-deftest disco-markdown-render-internal-angle-links-are-openable ()
  (let* ((rendered (disco-markdown-render
                    "hello <https://example.com>"
                    :context 'test-internal-angle-link))
         (pos (string-match "https://example.com" rendered)))
    (should pos)
    (should (equal "hello https://example.com"
                   (substring-no-properties rendered)))
    (should (equal "https://example.com"
                   (get-text-property pos 'disco-markdown-url rendered)))
    (should (keymapp (get-text-property pos 'keymap rendered)))))

(ert-deftest disco-markdown-render-internal-spoilers-mask-and-tag-message ()
  (let* ((rendered (disco-markdown-render
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
  (let* ((rendered (disco-markdown-render
                    "Look || spoiler || now"
                    :context 'test-internal-spoiler-spaces
                    :spoiler-message-id "m1"))
         (plain (substring-no-properties rendered))
         (pos (string-match " spoiler " plain)))
    (should pos)
    (should (equal "Look  spoiler  now" plain))
    (should (equal "█" (get-text-property pos 'display rendered)))
    (should (equal "█" (get-text-property (1- (match-end 0)) 'display rendered)))))

(ert-deftest disco-markdown-render-internal-inline-code-has-stable-copy-property ()
  (let* ((rendered (disco-markdown-render
                    "Use `code` now"
                    :context 'test-internal-inline-code-property))
         (plain (substring-no-properties rendered))
         (pos (string-match "code" plain)))
    (should pos)
    (should (get-text-property pos 'disco-markdown-code rendered))
    (should (eq 'inline
                (get-text-property pos 'disco-markdown-code-kind rendered)))))

(ert-deftest disco-markdown-copy-export-materializes-blockquotes-and-reveals-spoilers ()
  (let* ((exported (disco-markdown-copy-export
                    "> quote\nLook ||secret||"
                    :context 'test-copy-export
                    :spoiler-message-id "m1"
                    :reveal-spoilers t))
         (plain (substring-no-properties exported)))
    (should (equal "| quote\nLook secret" plain))
    (should-not (get-text-property 0 'line-prefix exported))
    (should-not (get-text-property 0 'keymap exported))))

(ert-deftest disco-markdown-render-internal-subtitle-lines-strip-marker ()
  (let* ((rendered (disco-markdown-render "-# Small print"
                                          :context 'test-internal-subtitle))
         (plain (substring-no-properties rendered)))
    (should (equal "Small print" plain))
    (should (disco-markdown--face-match-p
             (get-text-property 0 'face rendered)
             'disco-markdown-subtitle-face))))

(ert-deftest disco-markdown-render-internal-headings-strip-markers-for-all-levels ()
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
               (cdr entry))))))

(ert-deftest disco-markdown-render-internal-emphasis-strips-markers ()
  (let* ((rendered (disco-markdown-render
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

(ert-deftest disco-markdown-render-internal-nested-emphasis-combines-faces ()
  (let* ((rendered (disco-markdown-render
                    "***both*** **bold *italic*** *italic **bold*** [***link***](https://example.com)"
                    :context 'test-internal-nested-emphasis))
         (plain (substring-no-properties rendered))
         (both-pos (string-match "both" plain))
         (nested-italic-pos (string-match "italic" plain))
         (nested-bold-pos (string-match "bold" plain (1+ nested-italic-pos)))
         (link-pos (string-match "link" plain)))
    (should (equal "both bold italic italic bold link" plain))
    (should (disco-markdown--face-match-p
             (get-text-property both-pos 'face rendered)
             'disco-markdown-strong-face))
    (should (disco-markdown--face-match-p
             (get-text-property both-pos 'face rendered)
             'disco-markdown-emphasis-face))
    (should (disco-markdown--face-match-p
             (get-text-property nested-italic-pos 'face rendered)
             'disco-markdown-strong-face))
    (should (disco-markdown--face-match-p
             (get-text-property nested-italic-pos 'face rendered)
             'disco-markdown-emphasis-face))
    (should (disco-markdown--face-match-p
             (get-text-property nested-bold-pos 'face rendered)
             'disco-markdown-strong-face))
    (should (disco-markdown--face-match-p
             (get-text-property nested-bold-pos 'face rendered)
             'disco-markdown-emphasis-face))
    (should (equal "https://example.com"
                   (get-text-property link-pos 'disco-markdown-url rendered)))
    (should (disco-markdown--face-match-p
             (get-text-property link-pos 'face rendered)
             'disco-markdown-link-face))
    (should (disco-markdown--face-match-p
             (get-text-property link-pos 'face rendered)
             'disco-markdown-strong-face))
    (should (disco-markdown--face-match-p
             (get-text-property link-pos 'face rendered)
             'disco-markdown-emphasis-face))))

(ert-deftest disco-markdown-render-internal-inline-code-protects-content ()
  (let* ((rendered (disco-markdown-render
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
  (let* ((rendered (disco-markdown-render
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
  (let* ((disco-markdown-fontify-code-blocks-natively t)
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

(ert-deftest disco-markdown-render-cache-distinguishes-code-fontification-policy ()
  (let ((disco-markdown-cache-enabled t)
        (source "```elisp\n(let ((x 1)) x)\n```"))
    (disco-markdown-clear-cache)
    (unwind-protect
        (let* ((disco-markdown-fontify-code-blocks-natively nil)
               (plain-rendered
                (disco-markdown-render source :context 'test-cache-policy))
               (plain-pos (string-match "let" plain-rendered)))
          (should plain-pos)
          (should-not
           (disco-markdown--face-match-p
            (get-text-property plain-pos 'face plain-rendered)
            'font-lock-keyword-face))
          (let* ((disco-markdown-fontify-code-blocks-natively t)
                 (highlighted
                  (disco-markdown-render source :context 'test-cache-policy))
                 (highlighted-pos (string-match "let" highlighted)))
            (should highlighted-pos)
            (should
             (disco-markdown--face-match-p
              (get-text-property highlighted-pos 'face highlighted)
              'font-lock-keyword-face))))
      (disco-markdown-clear-cache))))

(ert-deftest disco-markdown-render-cache-tracks-state-channel-names ()
  (let ((disco-markdown-cache-enabled t)
        (channel-name "before"))
    (disco-markdown-clear-cache)
    (unwind-protect
        (cl-letf (((symbol-function 'disco-state-channel)
                   (lambda (channel-id)
                     `((id . ,channel-id) (name . ,channel-name)))))
          (should
           (equal "#before"
                  (substring-no-properties
                   (disco-markdown-render
                    "<#123>" :context 'test-cache-channel-name))))
          (setq channel-name "after")
          (should
           (equal "#after"
                  (substring-no-properties
                   (disco-markdown-render
                    "<#123>" :context 'test-cache-channel-name)))))
      (disco-markdown-clear-cache))))

(ert-deftest disco-markdown-render-internal-blockquotes-add-prefix-and-face ()
  (let* ((rendered (disco-markdown-render "> quoted"
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
  (let* ((rendered (disco-markdown-render ">>> first line\nsecond line"
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
  (let* ((rendered (disco-markdown-render "> # Quoted Title"
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

(ert-deftest disco-markdown-render-internal-lists-stay-plain-text ()
  (let* ((rendered (disco-markdown-render "* one\n  + two\n- three\n1. four"
                                          :context 'test-internal-list))
         (plain (substring-no-properties rendered)))
    (should (equal "* one\n  + two\n- three\n1. four" plain))))

(ert-deftest disco-markdown-render-internal-escapes-protect-inline-markup ()
  (let* ((rendered (disco-markdown-render
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
  (let* ((rendered (disco-markdown-render
                    "\\> not quote\n\\- not list\n\\# not heading\n\\-# not subtitle"
                    :context 'test-internal-escapes-block))
         (plain (substring-no-properties rendered))
         (heading-pos (string-match "# not heading" plain))
         (subtitle-pos (string-match "-# not subtitle" plain)))
    (should (equal "> not quote\n- not list\n# not heading\n-# not subtitle"
                   plain))
    (should-not (get-text-property 0 'line-prefix rendered))
    (should-not (disco-markdown--face-match-p
                 (get-text-property heading-pos 'face rendered)
                 'disco-markdown-heading-1-face))
    (should-not (disco-markdown--face-match-p
                 (get-text-property subtitle-pos 'face rendered)
                 'disco-markdown-subtitle-face))))

(ert-deftest disco-markdown-render-internal-spoilers-can-be-revealed ()
  (let* ((rendered (disco-markdown-render
                    "Look ||spoiler|| now"
                    :context 'test-internal-spoiler-reveal
                    :spoiler-message-id "m1"
                    :reveal-spoilers t))
         (plain (substring-no-properties rendered))
         (pos (string-match "spoiler" plain)))
    (should pos)
    (should (equal "Look spoiler now" plain))
    (should (equal "m1"
                   (get-text-property
                    pos 'disco-markdown-spoiler-message-id rendered)))
    (should-not (get-text-property pos 'display rendered))))

(ert-deftest disco-markdown-render-internal-fenced-code-keeps-indented-hash-lines-literal ()
  (let* ((rendered (disco-markdown-render
                    "```nix\n      # Can use ssh instead of password on system\n```"
                    :context 'test-internal-fence-indented-hash))
         (plain (substring-no-properties rendered))
         (pos (string-match "# Can use ssh instead of password on system" plain)))
    (should (equal "      # Can use ssh instead of password on system\n" plain))
    (should pos)
    (should (disco-markdown--face-match-p
             (get-text-property pos 'face rendered)
             'disco-markdown-code-face))
    (should-not (disco-markdown--face-match-p
                 (get-text-property pos 'face rendered)
                 'disco-markdown-heading-1-face))))

(provide 'disco-markdown-test)

;;; disco-markdown-test.el ends here
