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
