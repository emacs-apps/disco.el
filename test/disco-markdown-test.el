;;; disco-markdown-test.el --- Semantic tests for disco-markdown -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'appkit-markup)
(require 'appkit-ui)
(require 'disco-markdown)

;;; Helpers

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

(defconst disco-markdown-test--escaped-provider-fixtures
  '("\\@everyone"
    "\\<@123456789012345678>"
    "\\<https://example.com>")
  "Escaped provider forms that must survive a lossless round trip.")

(defconst disco-markdown-test--literal-block-fixtures
  '("#### heading"
    "+ item"
    "1) item"
    "~~~js\ncode\n~~~"
    "    indented code"
    "  -# indented subtitle")
  "CommonMark block forms that Discord treats as literal text.")

(defconst disco-markdown-test--provider-token-fixtures
  '((user . "<@1>")
    (role . "<@&3>")
    (channel . "<#2>")
    (command . "</deploy:4>")
    (emoji . "<:wave:5>")
    (timestamp . "<t:0:d>")
    (navigation . "<id:home>")
    (everyone . "@everyone")
    (email . "<nelly@discord.com>")
    (phone . "<+1 (555) 123 4567>")
    (standard-emoji . ":100:")
    (suppressed-link . "<https://example.com>"))
  "Documented provider token fixtures paired with semantic kinds.")

(defun disco-markdown-test--object-kind (node)
  "Return Discord provider kind carried by object NODE."
  (disco-markdown-object-kind (appkit-markup-object-value node)))

(defun disco-markdown-test--document-object-kinds (document)
  "Return ordered Discord provider kinds in DOCUMENT."
  (mapcar #'disco-markdown-test--object-kind
          (disco-markdown-test--document-objects document)))

(defun disco-markdown-test--inline-plain-text (children)
  "Return semantic plain text for inline CHILDREN."
  (appkit-markup-plain-text
   (appkit-markup-document
    (list (appkit-markup-paragraph children)))))

(defun disco-markdown-test--capture (source &rest parse-options)
  "Parse and print SOURCE with optional PARSE-OPTIONS.

Return a plist carrying :document, :objects, :printed, and :wire."
  (let* ((document
          (apply #'disco-markdown-document source parse-options))
         (printed (appkit-markup-print 'discord-markdown document)))
    (list :document document
          :objects (disco-markdown-test--document-objects document)
          :printed printed
          :wire (appkit-markup-print-result-source printed))))

(defun disco-markdown-test--assert-lossless-round-trip
    (source &rest parse-options)
  "Assert that SOURCE round-trips losslessly with PARSE-OPTIONS."
  (ert-info ((format "Discord source: %S" source))
    (let* ((capture
            (apply #'disco-markdown-test--capture source parse-options))
           (printed (plist-get capture :printed)))
      (should-not (appkit-markup-print-result-losses printed))
      (should (equal source (plist-get capture :wire)))
      capture)))

(defun disco-markdown-test--assert-action (rendered label)
  "Assert that RENDERED text LABEL carries a native action."
  (let* ((plain (substring-no-properties rendered))
         (position (string-match (regexp-quote label) plain)))
    (should position)
    (should
     (functionp
      (get-text-property position appkit-ui-action-property rendered)))))

;;; Semantic parsing

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

(ert-deftest disco-markdown-drops-transformed-source-diagnostics ()
  (let ((result
         (disco-markdown-parse "<@123456789>\n- [x] task")))
    (should-not (appkit-markup-parse-result-diagnostics result))))

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
             (eq 'spoiler (disco-markdown-test--object-kind node)))
           objects)))
    (should (memq 'underline (appkit-markup-text-styles under)))
    (should spoiler)
    (should
     (equal
      "secret @Ada"
      (disco-markdown-test--inline-plain-text
       (appkit-markup-object-fallback spoiler))))))

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
  (let ((message
         '((mentions . (((id . "1") (username . "Ada"))))
           (mention_channels . (((id . "2") (name . "general"))))
           (resolved . ((roles . (((id . "3") (name . "admin")))))))))
    (dolist (fixture disco-markdown-test--provider-token-fixtures)
      (pcase-let ((`(,expected-kind . ,source) fixture))
        (ert-info ((format "Provider fixture: %S" fixture))
          (let* ((capture
                  (disco-markdown-test--assert-lossless-round-trip
                   source :message message))
                 (kinds
                  (disco-markdown-test--document-object-kinds
                   (plist-get capture :document))))
            (should (equal (list expected-kind) kinds))))))))

(ert-deftest disco-markdown-code-regions-never-adapt-provider-syntax ()
  (let* ((source "`<@1> ||secret|| __under__`\n\n```text\n<@1> ||secret|| __under__\n```")
         (document (disco-markdown-document source)))
    (should-not (disco-markdown-test--document-objects document))
    (should (equal "<@1> ||secret|| __under__\n<@1> ||secret|| __under__"
                   (appkit-markup-plain-text document)))))

(ert-deftest disco-markdown-restores-fenced-code-language-metadata ()
  (dolist (language '("c++" "c#"))
    (let* ((document
            (disco-markdown-document
             (format "```%s\ncode\n```" language)))
           (block (car (appkit-markup-document-blocks document))))
      (should (appkit-markup-preformatted-p block))
      (should (equal language
                     (appkit-markup-preformatted-language block))))))

(ert-deftest disco-markdown-escaped-provider-delimiters-stay-literal ()
  (let* ((document
          (disco-markdown-document
           "\\<@1> \\||secret|| \\__under__ \\-# subtitle"))
         (objects (disco-markdown-test--document-objects document))
         (plain (appkit-markup-plain-text document)))
    (should (= 1 (length objects)))
    (should (eq 'literal
                (disco-markdown-test--object-kind (car objects))))
    (should (equal "<@1> ||secret|| __under__ -# subtitle" plain))))

(ert-deftest disco-markdown-malformed-provider-delimiters-stay-visible ()
  (let ((document (disco-markdown-document "before ||open and __open")))
    (should-not (disco-markdown-test--document-objects document))
    (should (equal "before ||open and __open"
                   (appkit-markup-plain-text document)))))

;;; Native rendering

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
    (should (equal "quote\n\none\ntwo" plain))
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

;;; Provider printing

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

(ert-deftest disco-markdown-provider-printer-preserves-object-styles ()
  (disco-markdown-test--assert-lossless-round-trip
   "**<@1>**"
   :message '((mentions . (((id . "1") (username . "Ada")))))))

(ert-deftest disco-markdown-codec-round-trips-spoilers-and-escaped-pipes ()
  (disco-markdown-test--assert-lossless-round-trip
   "before ||**secret**|| and \\|\\|literal\\|\\|"))

;;; Discord dialect fixtures

(ert-deftest disco-markdown-code-zones-bound-discord-delimiters ()
  (let* ((source "||outer `code || inner` tail||")
         (capture
          (disco-markdown-test--assert-lossless-round-trip source))
         (objects (plist-get capture :objects)))
    (should (= 1 (length objects)))
    (should (eq 'spoiler
                (disco-markdown-test--object-kind (car objects))))))

(ert-deftest disco-markdown-preserves-escaped-provider-literals ()
  (dolist (source disco-markdown-test--escaped-provider-fixtures)
    (disco-markdown-test--assert-lossless-round-trip source)))

(ert-deftest disco-markdown-provider-tokens-require-exact-boundaries ()
  (let* ((document
          (disco-markdown-document
           "mail@everyoneelse x@hereafter id:guide"))
         (objects (disco-markdown-test--document-objects document)))
    (should-not objects)
    (should
     (equal "mail@everyoneelse x@hereafter id:guide"
            (appkit-markup-plain-text document)))))

(ert-deftest disco-markdown-multiline-quote-extends-to-message-end ()
  (let* ((capture
          (disco-markdown-test--capture
           "before\n\n>>> first\nsecond\nthird"))
         (document (plist-get capture :document))
         (blocks (appkit-markup-document-blocks document))
         (quote (nth 1 blocks)))
    (should (= 2 (length blocks)))
    (should (appkit-markup-quote-p quote))
    (should
     (equal "> first\n> second\n> third"
            (appkit-markup-plain-text
             (appkit-markup-document (list quote)))))
    (should-not
     (appkit-markup-print-result-losses
      (plist-get capture :printed)))
    (should
     (equal "before\n\n> first  \n> second  \n> third"
            (plist-get capture :wire)))))

(ert-deftest disco-markdown-preserves-suppressed-link-provider-form ()
  (let* ((source "<https://example.com/path>")
         (capture
          (disco-markdown-test--assert-lossless-round-trip source))
         (object (car (plist-get capture :objects)))
         (rendered (disco-markdown-render source)))
    (should (eq 'suppressed-link
                (disco-markdown-test--object-kind object)))
    (disco-markdown-test--assert-action
     rendered "https://example.com/path")))

(ert-deftest disco-markdown-rejects-commonmark-only-block-extensions ()
  (dolist (source disco-markdown-test--literal-block-fixtures)
    (ert-info ((format "Discord literal block: %S" source))
      (let* ((document (disco-markdown-document source))
             (blocks (appkit-markup-document-blocks document)))
        (should (= 1 (length blocks)))
        (should (appkit-markup-paragraph-p (car blocks)))
        (should (equal source (appkit-markup-plain-text document)))))))

(ert-deftest disco-markdown-native-provider-objects-expose-actions ()
  (let* ((message
          '((mentions . (((id . "1") (username . "Ada"))))
            (mention_channels . (((id . "2") (name . "general"))))))
         (rendered
          (disco-markdown-render
           "<@1> <#2> </ping:3> <t:1618953630:F>"
           :message message)))
    (dolist (label '("@Ada" "#general" "/ping" "2021"))
      (disco-markdown-test--assert-action rendered label))))

;;; Semantic chunking

(ert-deftest disco-markdown-chunks-balance-spoilers-and-styles ()
  (let* ((source
          (concat (make-string 1998 ?x)
                  "||"
                  (make-string 20 ?s)
                  "||"))
         (document (disco-markdown-document source))
         (chunks (disco-markdown-print-chunks document 2000))
         (sources (mapcar (lambda (chunk) (plist-get chunk :source)) chunks)))
    (should (= 2 (length chunks)))
    (should (cl-every (lambda (wire) (<= (length wire) 2000)) sources))
    (should (equal (make-string 1998 ?x) (car sources)))
    (should (equal (concat "||" (make-string 20 ?s) "||")
                   (cadr sources)))
    (should
     (cl-every
      (lambda (chunk)
        (appkit-markup-document-p (plist-get chunk :document)))
      chunks))))

(ert-deftest disco-markdown-chunks-preformatted-content-with-valid-fences ()
  (let* ((document
          (appkit-markup-document
           (list
            (appkit-markup-preformatted (make-string 80 ?x) "text"))))
         (chunks (disco-markdown-print-chunks document 32)))
    (should (> (length chunks) 1))
    (dolist (chunk chunks)
      (let ((source (plist-get chunk :source)))
        (should (<= (length source) 32))
        (should (string-prefix-p "```text\n" source))
        (should (string-suffix-p "\n```" source))))))

;;; Safety and lifecycle

(ert-deftest disco-markdown-parse-does-not-run-markdown-or-language-hooks ()
  (let ((markdown-ts-mode-hook (list (lambda () (error "mode hook ran"))))
        (emacs-lisp-mode-hook (list (lambda () (error "language hook ran")))))
    (should
     (equal "code"
            (appkit-markup-plain-text
             (disco-markdown-document "```elisp\ncode\n```"))))))

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
