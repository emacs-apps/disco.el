;;; disco-ins-test.el --- Tests for disco-ins helpers -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-ins)
(require 'disco-ui)

(ert-deftest disco-ins-prefix-string-handles-strings-and-prefix-state ()
  (let ((state (disco-ui-make-prefix-state "a> " "b> ")))
    (should (equal "raw> "
                   (disco-ins-prefix-string "raw> " nil "fallback> ")))
    (should (equal "a> "
                   (disco-ins-prefix-string state t "fallback> ")))
    (should (equal "b> "
                   (disco-ins-prefix-string state nil "fallback> ")))))

(ert-deftest disco-ins-insert-full-width-divider-applies-face-and-properties ()
  (with-temp-buffer
    (let ((span (disco-ins-insert-full-width-divider "Label" 'shadow 24 '(demo t))))
      (should (string-match-p "( Label )" (buffer-string)))
      (should (eq t (get-text-property (car span) 'demo)))
      (let ((face (get-text-property (car span) 'face)))
        (should (or (eq face 'shadow)
                    (and (listp face) (memq 'shadow face))))))))

(ert-deftest disco-ins-insert-divider-row-is-read-only ()
  (with-temp-buffer
    (let ((span (disco-ins-insert-divider-row "Unread" 'shadow 20 '(section unread))))
      (should (string-match-p "Unread" (buffer-string)))
      (should (eq t (get-text-property (car span) 'read-only)))
      (should (equal 'unread (get-text-property (car span) 'section))))))

(ert-deftest disco-ins-insert-reference-line-supports-body-and-button ()
  (with-temp-buffer
    (let (clicked)
      (disco-ins-insert-reference-line
       "preview"
       :prefix "    "
       :face 'shadow
       :button-label "[Jump]"
       :button-action (lambda ()
                        (setq clicked t)))
      (goto-char (point-min))
      (should (string-match-p (regexp-quote "↪ preview [Jump]")
                              (buffer-string)))
      (should (eq 'shadow (get-text-property (point) 'face)))
      (search-forward "[Jump]")
      (button-activate (button-at (match-beginning 0)))
      (should clicked))))

(ert-deftest disco-ins-insert-reaction-line-renders-selected-and-unselected-chips ()
  (with-temp-buffer
    (let* ((selected '((emoji . ((name . ":wave:")))
                       (count . 2)
                       (me . true)))
           (plain '((emoji . ((name . ":sparkles:")))
                    (total_count . 1)
                    (is_chosen . :false)))
           (span (disco-ins-insert-reaction-line
                  (list selected plain)
                  :prefix "    "
                  :selected-face 'success
                  :unselected-face 'shadow
                  :line-face 'default)))
      (should span)
      (should (string-match-p "\[:wave: 2\] \[:sparkles: 1\]" (buffer-string)))
      (goto-char (point-min))
      (search-forward "[:wave: 2]")
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (eq face 'success)
                    (and (listp face) (memq 'success face)))))
      (search-forward "[:sparkles: 1]")
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (eq face 'shadow)
                    (and (listp face) (memq 'shadow face))))))))

(ert-deftest disco-ins-insert-forward-card-renders-metadata-and-action ()
  (with-temp-buffer
    (let (clicked)
      (disco-ins-insert-forward-card
       :source-text "Guild / channel"
       :sent-at "2026-03-09 10:00"
       :content "hello forward"
       :insert-source-icon (lambda () (insert "[icon]"))
       :jump-label "[Jump to source]"
       :jump-action (lambda () (setq clicked t))
       :jump-face 'link
       :border-face 'shadow
       :title-face 'bold
       :meta-face 'italic)
      (should (string-match-p (regexp-quote "[forwarded message]") (buffer-string)))
      (should (string-match-p (regexp-quote "source: [icon] Guild / channel")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "sent: 2026-03-09 10:00")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "hello forward") (buffer-string)))
      (goto-char (point-min))
      (search-forward "[Jump to source]")
      (button-activate (button-at (match-beginning 0)))
      (should clicked))))

(ert-deftest disco-ins-insert-attachment-lines-renders-summary-and-url ()
  (with-temp-buffer
    (let ((span (disco-ins-insert-attachment-lines
                 "[file] doc.txt"
                 :prefix "    "
                 :url "https://example.invalid/doc.txt"
                 :summary-face 'bold
                 :url-face 'shadow)))
      (should span)
      (should (string-match-p (regexp-quote "[file] doc.txt") (buffer-string)))
      (should (string-match-p (regexp-quote "https://example.invalid/doc.txt")
                              (buffer-string)))
      (goto-char (point-min))
      (should (eq 'bold (get-text-property (point) 'face)))
      (search-forward "https://example.invalid/doc.txt")
      (should (eq 'shadow (get-text-property (match-beginning 0) 'face))))))

(provide 'disco-ins-test)

;;; disco-ins-test.el ends here
