;;; disco-markdown-test.el --- Semantic tests for disco-markdown -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'appkit-markup)
(require 'appkit-ui)
(require 'disco-markdown)

(defun disco-markdown-test--block-inlines (block)
  "Return every inline sequence recursively contained in BLOCK."
  (cond
   ((appkit-markup-paragraph-p block)
    (list (appkit-markup-paragraph-children block)))
   ((appkit-markup-heading-p block)
    (list (appkit-markup-heading-children block)))
   ((appkit-markup-quote-p block)
    (apply #'append
           (mapcar #'disco-markdown-test--block-inlines
                   (appkit-markup-quote-blocks block))))
   ((appkit-markup-list-p block)
    (apply
     #'append
     (mapcar
      (lambda (item)
        (apply #'append
               (mapcar #'disco-markdown-test--block-inlines
                       (appkit-markup-list-item-blocks item))))
      (appkit-markup-list-items block))))
   ((appkit-markup-object-block-p block)
    (apply #'append
           (mapcar #'disco-markdown-test--block-inlines
                   (appkit-markup-object-block-fallback block))))
   (t nil)))

(defun disco-markdown-test--document-inlines (document)
  "Return every inline sequence recursively contained in DOCUMENT."
  (apply #'append
         (mapcar #'disco-markdown-test--block-inlines
                 (appkit-markup-document-blocks document))))

(defun disco-markdown-test--objects-in-inlines (children)
  "Return provider objects recursively contained in inline CHILDREN."
  (let (result)
    (dolist (node children (nreverse result))
      (cond
       ((appkit-markup-object-p node)
        (push node result)
        (dolist (nested
                 (disco-markdown-test--objects-in-inlines
                  (appkit-markup-object-fallback node)))
          (push nested result)))
       ((appkit-markup-link-p node)
        (dolist (nested
                 (disco-markdown-test--objects-in-inlines
                  (appkit-markup-link-children node)))
          (push nested result)))))))

(defun disco-markdown-test--document-objects (document)
  "Return every provider object recursively contained in DOCUMENT."
  (apply #'append
         (mapcar #'disco-markdown-test--objects-in-inlines
                 (disco-markdown-test--document-inlines document))))

(defun disco-markdown-test--face-has-p (face expected)
  "Return non-nil when FACE contains EXPECTED."
  (if (listp face) (memq expected face) (eq face expected)))

(ert-deftest disco-markdown-parse-builds-bounded-appkit-document ()
  (let* ((result (disco-markdown-parse
                  "# Heading\n\nParagraph with **bold** and *italic*.\n\n- one\n- two"))
         (document (appkit-markup-parse-result-document result))
         (blocks (appkit-markup-document-blocks document)))
    (should (appkit-markup-document-p document))
    (should (appkit-markup-heading-p (nth 0 blocks)))
    (should (appkit-markup-paragraph-p (nth 1 blocks)))
    (should (appkit-markup-list-p (nth 2 blocks)))
    (should-not (appkit-markup-parse-result-diagnostics result))
    (should (appkit-markup-validate document))))

(ert-deftest disco-markdown-adapts-underline-and-nested-spoiler-semantics ()
  (let* ((message '((mentions . (((id . "1") (username . "Ada"))))))
         (document
          (disco-markdown-document
           "__under **bold**__ and ||**secret** <@1>||"
           :message message :spoiler-message-id "m1"))
         (inlines (car (disco-markdown-test--document-inlines document)))
         (under (car inlines))
         (objects (disco-markdown-test--document-objects document))
         (spoiler
          (seq-find
           (lambda (node)
             (eq (disco-markdown-object-kind
                  (appkit-markup-object-value node))
                 'spoiler))
           objects)))
    (should (memq 'underline (appkit-markup-text-styles under)))
    (should spoiler)
    (should (equal "secret @Ada"
                   (mapconcat
                    (lambda (node)
                      (cond
                       ((appkit-markup-text-p node)
                        (appkit-markup-text-text node))
                       ((appkit-markup-object-p node)
                        (mapconcat #'appkit-markup-text-text
                                   (appkit-markup-object-fallback node) ""))
                       (t "")))
                    (appkit-markup-object-fallback spoiler) "")))))

(ert-deftest disco-markdown-provider-token-occurrences-remain-distinct ()
  (let* ((message '((mentions . (((id . "1") (global_name . "Ada"))))))
         (document (disco-markdown-document "<@1> <@1>" :message message))
         (objects (disco-markdown-test--document-objects document))
         (left (nth 0 objects))
         (right (nth 1 objects)))
    (should (= 2 (length objects)))
    (should-not (eq left right))
    (should-not (eq (appkit-markup-object-value left)
                    (appkit-markup-object-value right)))
    (should (equal (appkit-markup-object-value left)
                   (appkit-markup-object-value right)))
    (should (equal "@Ada @Ada" (appkit-markup-plain-text document)))))

(ert-deftest disco-markdown-adapts-all-provider-token-kinds ()
  (let* ((message
          '((mentions . (((id . "1") (username . "Ada"))))
            (mention_channels . (((id . "2") (name . "general"))))
            (resolved . ((roles . (((id . "3") (name . "admin"))))))))
         (document
          (disco-markdown-document
           "<@1> <@&3> <#2> </deploy:4> <:wave:5> <t:0:d> <id:home> @everyone"
           :message message))
         (kinds
          (mapcar
           (lambda (node)
             (disco-markdown-object-kind (appkit-markup-object-value node)))
           (disco-markdown-test--document-objects document))))
    (should (equal '(user role channel command emoji timestamp navigation everyone)
                   kinds))))

(ert-deftest disco-markdown-code-regions-never-adapt-provider-syntax ()
  (let* ((source "`<@1> ||secret|| __under__`\n\n```text\n<@1> ||secret|| __under__\n```")
         (document (disco-markdown-document source)))
    (should-not (disco-markdown-test--document-objects document))
    (should (equal "<@1> ||secret|| __under__\n<@1> ||secret|| __under__"
                   (appkit-markup-plain-text document)))))

(ert-deftest disco-markdown-escaped-provider-delimiters-stay-literal ()
  (let* ((document
          (disco-markdown-document
           "\\<@1> \\||secret|| \\__under__ \\-# subtitle"))
         (plain (appkit-markup-plain-text document)))
    (should-not (disco-markdown-test--document-objects document))
    (should (equal "<@1> ||secret|| __under__ -# subtitle" plain))))

(ert-deftest disco-markdown-malformed-provider-delimiters-stay-visible ()
  (let ((document (disco-markdown-document "before ||open and __open")))
    (should-not (disco-markdown-test--document-objects document))
    (should (equal "before ||open and __open"
                   (appkit-markup-plain-text document)))))

(ert-deftest disco-markdown-native-renderer-handles-links ()
  (let* ((rendered
          (disco-markdown-render "[Appkit](https://example.com/a\\)b)"))
         (position (string-match "Appkit" rendered))
         (action (get-text-property position appkit-ui-action-property rendered))
         opened)
    (should (functionp action))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &optional _new-window) (setq opened url))))
      (funcall action))
    (should (equal "https://example.com/a)b" opened))))

(ert-deftest disco-markdown-native-spoiler-keeps-underlying-copy-text ()
  (let* ((rendered
          (disco-markdown-render
           "Look || secret ||" :spoiler-message-id "m1"))
         (position (string-match "secret" rendered)))
    (should (equal "Look  secret " (substring-no-properties rendered)))
    (should (equal "█" (get-text-property position 'display rendered)))
    (should (equal "m1"
                   (get-text-property
                    position 'disco-markdown-spoiler-message-id rendered)))
    (should (functionp
             (get-text-property position appkit-ui-action-property rendered)))))

(ert-deftest disco-markdown-native-subtitle-uses-provider-object-face ()
  (let* ((rendered (disco-markdown-render "-# **Small** print"))
         (position (string-match "Small" rendered)))
    (should (equal "Small print" (substring-no-properties rendered)))
    (should (disco-markdown-test--face-has-p
             (get-text-property position 'face rendered)
             'disco-markdown-subtitle-face))))

(ert-deftest disco-markdown-native-list-and-quote-use-shared-geometry ()
  (let* ((rendered (disco-markdown-render "> quote\n\n- one\n- two"))
         (plain (substring-no-properties rendered))
         (one (string-match "one" plain)))
    (should (equal "quote\none\ntwo" plain))
    (should (stringp (get-text-property 0 'line-prefix rendered)))
    (should (string-match-p "•"
                            (get-text-property one 'line-prefix rendered)))))

(ert-deftest disco-markdown-copy-export-is-semantic-and-property-free ()
  (let ((exported
         (disco-markdown-copy-export "> quote\n> Look ||secret||")))
    (should (equal "> quote\n> Look secret" exported))
    (should-not (text-property-not-all 0 (length exported) nil nil exported))))

(ert-deftest disco-markdown-custom-emoji-render-preserves-outer-scan-boundary ()
  (cl-letf (((symbol-function 'disco-emoji-image-display-string)
             (lambda (emoji-id _animated fallback)
               ;; Deliberately replace match data; the adapter must have frozen
               ;; its source boundary before invoking later provider code.
               (string-match "[0-9]+" emoji-id)
               fallback)))
    (let* ((rendered
            (disco-markdown-render
             "Ups and downs<:ghostty_bobr:1386470157009944726>"
             :context 'room-message))
           (plain (substring-no-properties rendered))
           (position (string-match ":ghostty_bobr:" plain)))
      (should (equal "Ups and downs:ghostty_bobr:" plain))
      (should (equal "1386470157009944726"
                     (get-text-property position 'disco-emoji-id rendered))))))

(ert-deftest disco-markdown-parse-does-not-run-markdown-or-language-hooks ()
  (let ((markdown-ts-mode-hook (list (lambda () (error "mode hook ran"))))
        (emacs-lisp-mode-hook (list (lambda () (error "language hook ran")))))
    (should
     (equal "code"
            (appkit-markup-plain-text
             (disco-markdown-document "```elisp\ncode\n```"))))))

(ert-deftest disco-markdown-provider-printer-preserves-discord-capabilities ()
  (let* ((document
          (appkit-markup-document
           (list
            (appkit-markup-heading
             2 (list (appkit-markup-text "Heading")))
            (appkit-markup-paragraph
             (list (appkit-markup-text "under" '(underline))
                   (appkit-markup-text " bold" '(underline bold))
                   (appkit-markup-line-break)
                   (appkit-markup-link
                    "https://example.com/a)b"
                    (list (appkit-markup-text "link")))))
            (appkit-markup-preformatted "code" "elisp"))))
         (printed (appkit-markup-print 'discord-markdown document)))
    (should-not (appkit-markup-print-result-losses printed))
    (should
     (equal
      "## Heading\n\n__under** bold**__  \n[link](https://example.com/a\\)b)\n\n```elisp\ncode\n```"
      (appkit-markup-print-result-source printed)))))

(ert-deftest disco-markdown-codec-round-trips-spoilers-and-escaped-pipes ()
  (let* ((parsed
          (appkit-markup-parse
           'discord-markdown
           "before ||**secret**|| and \\|\\|literal\\|\\|"))
         (printed
          (appkit-markup-print
           'discord-markdown
           (appkit-markup-parse-result-document parsed))))
    (should-not (appkit-markup-print-result-losses printed))
    (should
     (equal "before ||**secret**|| and \\|\\|literal\\|\\|"
            (appkit-markup-print-result-source printed)))))

(ert-deftest disco-markdown-missing-grammar-is-content-free-error ()
  (cl-letf (((symbol-function 'treesit-language-available-p)
             (lambda (_language) nil)))
    (let ((error (should-error
                  (disco-markdown-document "secret source")
                  :type 'appkit-markup-codec-error)))
      (should (equal '(markdown-tree-sitter-grammars-unavailable)
                     (cdr error)))
      (should-not (member "secret source" error)))))

(ert-deftest disco-markdown-has-no-render-cache-or-fontification-buffers ()
  (should-not (boundp 'disco-markdown--cache))
  (should-not (boundp 'disco-markdown--fontification-buffers))
  (should-not (boundp 'disco-markdown--fontification-buffer-owner-p)))

(provide 'disco-markdown-test)

;;; disco-markdown-test.el ends here
