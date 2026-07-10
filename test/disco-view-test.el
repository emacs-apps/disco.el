;;; disco-view-test.el --- Tests for disco-view helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-view)

(ert-deftest disco-view-render-list-spec-renders-items-and-footer ()
  (with-temp-buffer
    (disco-view-render-list-spec
     (disco-view-list-spec-create
      :title "Threads"
      :key-hints "g: refresh"
      :summary "2 items"
      :items '("one" "two")
      :item-inserter (lambda (item)
                       (insert (format "- %s\n" item)))
      :footer-lines '("footer")))
    (should (string-match-p "Threads" (buffer-string)))
    (should (string-match-p "- one" (buffer-string)))
    (should (string-match-p "footer" (buffer-string)))))

(ert-deftest disco-view-render-list-spec-preserving-position-restores-anchor ()
  (with-temp-buffer
    (let ((items '(("a" . "first")
                   ("b" . "second"))))
      (cl-labels ((render-spec (rows)
                    (disco-view-list-spec-create
                     :items rows
                     :item-inserter
                     (lambda (item)
                       (let ((start (point)))
                         (insert (format "%s\n" (cdr item)))
                         (add-text-properties
                          start
                          (point)
                          (list 'row-id (car item))))))))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (disco-view-render-list-spec (render-spec items)))
        (goto-char (point-min))
        (search-forward "second")
        (beginning-of-line)
        (disco-view-render-list-spec-preserving-position
         (render-spec '(("a" . "first updated")
                        ("b" . "second updated")))
         :anchor-property 'row-id)
        (should (equal "b" (get-text-property (point) 'row-id)))
        (should (looking-at-p "second updated"))))))

(ert-deftest disco-view-canonicalize-number-supports-ratio-and-bounds ()
  (should (= 42 (disco-view-canonicalize-number 42 100)))
  (should (= 35 (disco-view-canonicalize-number 0.35 100)))
  (should (= 20 (disco-view-canonicalize-number '(0.1 20 60) 100)))
  (should (= 60 (disco-view-canonicalize-number '(0.8 20 60) 100)))
  (should (= 90 (disco-view-canonicalize-number '(0.9 20) 100))))

(ert-deftest disco-view-one-line-column-widths-follow-context-ratio ()
  (let ((widths (disco-view-one-line-column-widths 60 '(0.45 20))))
    (should (= 27 (plist-get widths :context-inner-width)))
    (should (= 30 (plist-get widths :preview-width)))
    (should (= 1 (plist-get widths :separator-width))))
  (let ((widths (disco-view-one-line-column-widths 30 '(0.45 20))))
    (should (= 20 (plist-get widths :context-inner-width)))
    (should (= 7 (plist-get widths :preview-width)))
    (should (= 1 (plist-get widths :separator-width)))))

(ert-deftest disco-view-one-line-row-collapses-preview-newlines ()
  (with-temp-buffer
    (disco-view-insert-one-line-row
     (disco-view-one-line-row-create
      :context "Group\nName"
      :preview "first line\nsecond line\r\nthird"
      :time "12:34")
     :width 80
     :icon-slot-width 4
     :context-width-spec '(0.32 16 30))
    (should (= (count-lines (point-min) (point-max)) 1))
    (should (string-match-p "Group Name" (buffer-string)))
    (should (string-match-p "first line second line third"
                            (buffer-string)))))

(ert-deftest disco-view-chars-xwidth-does-not-select-display-window ()
  (let ((original-window (selected-window))
        other-window
        (buffer (generate-new-buffer " *disco-view-width*")))
    (unwind-protect
        (progn
          (setq other-window (split-window original-window))
          (set-window-buffer other-window buffer)
          (with-current-buffer buffer
            (insert "window point\nbuffer insertion point")
            (set-window-point other-window (point-min))
            (goto-char (point-max))
            (let ((expected-point (point)))
              (cl-letf (((symbol-function 'display-graphic-p)
                         (lambda (&optional _display) t))
                        ((symbol-function 'string-pixel-width)
                         (lambda (_string &optional measured-buffer)
                           (should (eq measured-buffer buffer))
                           9)))
                (should (= 9 (disco-view--chars-xwidth 1 other-window))))
              (should (= expected-point (point)))
              (should (eq original-window (selected-window))))))
      (when (window-live-p other-window)
        (delete-window other-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest disco-view-move-to-column-inserts-align-spacer-when-needed ()
  (with-temp-buffer
    (insert "abc")
    (let ((insert-pos (point)))
      (disco-view-move-to-column 3)
      (should (= (point) (1+ insert-pos)))
      (should (>= (disco-view-current-column) 3))
      (let ((display-prop (get-text-property insert-pos 'display)))
        (should (consp display-prop))
        (should (eq (car display-prop) 'space))
        (should (plist-member (cdr display-prop) :align-to))))))

(ert-deftest disco-view-move-to-column-does-not-align-backwards ()
  (with-temp-buffer
    (insert "abcdef")
    (let ((insert-pos (point)))
      (disco-view-move-to-column 2)
      (should (= (point) insert-pos))
      (should-not (get-text-property (max (point-min) (1- insert-pos))
                                     'display)))))

(ert-deftest disco-view-window-fill-column-uses-remapped-width-and-margins ()
  (let* ((win (selected-window))
         (buffer (window-buffer win))
         (margins (window-margins win))
         (expected (- (+ (window-width win 'remap)
                         (or (car margins) 0)
                         (or (cdr margins) 0))
                      1)))
    (with-current-buffer buffer
      (let ((display-line-numbers-mode nil))
        (should (= (disco-view-window-fill-column win 1) expected))))))

(ert-deftest disco-view-elide-string-adds-display-ellipsis ()
  (let* ((text "abcdefghijklmnopqrstuvwxyz")
         (elided (disco-view-elide-string text 8 'shadow)))
    (should (> (length elided) 8))
    (let ((display-pos (next-single-property-change 0 'display elided)))
      (should (integerp display-pos))
      (should (equal "…"
                     (get-text-property display-pos 'display elided))))))

(ert-deftest disco-view-elide-string-noop-when-string-fits ()
  (let ((text "short"))
    (should (equal text (disco-view-elide-string text 12 'shadow)))))

(ert-deftest disco-view-insert-label-row-applies-struct-fields ()
  (with-temp-buffer
    (disco-view-insert-label-row
     (disco-view-label-row-create
      :label "Section"
      :prefix "[+] "
      :suffix " (4)"
      :face 'font-lock-keyword-face
      :line-properties '(row-kind section)))
    (goto-char (point-min))
    (should (equal "[+] Section (4)\n" (buffer-string)))
    (should (equal 'section (get-text-property (point) 'row-kind)))
    (should (equal 'font-lock-keyword-face
                   (get-text-property (point) 'face)))))

(ert-deftest disco-view-insert-label-line-supports-prefix-icon-and-suffix ()
  (with-temp-buffer
    (disco-view-insert-label-line
     "Guild"
     :prefix "  [-] "
     :icon-inserter (lambda ()
                      (insert "*"))
     :icon-separator " "
     :suffix " (2)"
     :line-properties '(row-kind guild)
     :help-echo "toggle")
    (goto-char (point-min))
    (should (looking-at-p "  \\[-\\] \\* Guild (2)"))
    (should (equal 'guild (get-text-property (point) 'row-kind)))
    (should (equal "toggle" (get-text-property (point) 'help-echo)))))

(ert-deftest disco-view-insert-heading-line-applies-face-and-properties ()
  (with-temp-buffer
    (disco-view-insert-heading-line
     "Heading"
     :face 'font-lock-keyword-face
     :line-properties '(row-kind section))
    (goto-char (point-min))
    (should (equal 'section (get-text-property (point) 'row-kind)))
    (should (equal 'font-lock-keyword-face
                   (get-text-property (point) 'face)))))

(ert-deftest disco-view-insert-action-line-uses-default-link-styling ()
  (with-temp-buffer
    (disco-view-insert-action-line
     "Show more"
     :line-properties '(row-kind action)
     :help-echo "Show more")
    (goto-char (point-min))
    (should (looking-at-p "  \\[Show more\\]"))
    (should (equal 'action (get-text-property (point) 'row-kind)))
    (should (equal 'link (get-text-property (point) 'face)))
    (should (equal 'highlight (get-text-property (point) 'mouse-face)))
    (should (equal "Show more" (get-text-property (point) 'help-echo)))))
