;;; disco-embed-test.el --- Tests for disco-embed -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-embed)
(require 'disco-room-test-support)

(ert-deftest disco-embed-stringify-uses-internal-markdown-renderer ()
  (let* ((disco-embed--current-message '((id . "m1")))
         (disco-embed--current-spoiler-message-id "m1")
         (disco-embed--reveal-spoilers nil)
         (rendered (disco-embed--stringify "[link](https://example.com)\n> quote"))
         (plain (substring-no-properties rendered))
         (link-pos (string-match "link" plain))
         (quote-pos (string-match "quote" plain)))
    (should (equal "link\nquote" plain))
    (should (equal "https://example.com"
                   (get-text-property link-pos 'disco-markdown-url rendered)))
    (should (equal "│ "
                   (substring-no-properties
                    (get-text-property quote-pos 'line-prefix rendered))))))

(ert-deftest disco-embed-stringify-passes-spoiler-context-to-internal-renderer ()
  (let* ((disco-embed--current-message '((id . "m1")))
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

(ert-deftest disco-embed-normalize-embeds-merges-trailing-image-only-embeds ()
  (let* ((embeds (list '((type . "rich")
                         (url . "https://example.invalid/post")
                         (title . "Gallery")
                         (image . ((url . "https://example.invalid/1.png"))))
                       '((type . "rich")
                         (url . "https://example.invalid/post")
                         (image . ((url . "https://example.invalid/2.png"))))
                       '((type . "rich")
                         (url . "https://example.invalid/other")
                         (title . "Other")
                         (image . ((url . "https://example.invalid/3.png"))))))
         (normalized (disco-embed--normalize-embeds embeds))
         (first (car normalized))
         (images (alist-get 'images first)))
    (should (= 2 (length normalized)))
    (should (= 2 (length images)))
    (should (equal "https://example.invalid/1.png"
                   (alist-get 'url (nth 0 images))))
    (should (equal "https://example.invalid/2.png"
                   (alist-get 'url (nth 1 images))))
    (should (equal "https://example.invalid/other"
                   (alist-get 'url (cadr normalized))))))

(ert-deftest disco-embed-grid-preview-row-renders-images-as-slices ()
  (let (insert-calls)
    (with-temp-buffer
      (cl-letf (((symbol-function 'image-size)
                 (lambda (image &optional _pixels _frame)
                   (pcase image
                     (:img-a '(4 . 2))
                     (:img-b '(5 . 3))
                     (_ '(1 . 1)))))
                ((symbol-function 'insert-image)
                 (lambda (image string &optional area slice)
                   (push (list image string area slice) insert-calls)
                   (insert (format "[%s:%s]"
                                   image
                                   (if slice "slice" "plain")))))
                ((symbol-function 'appkit-media-image-slice-count)
                 (lambda (image)
                   (pcase image
                     (:img-a 2)
                     (:img-b 3)
                     (_ 1))))
                ((symbol-function 'disco-embed--background-face)
                 (lambda (_embed) nil)))
        (disco-embed--insert-grid-preview-row
         (list (list :image :img-a :status 'ready :url "https://example.invalid/a.png")
               (list :image :img-b :status 'ready :url "https://example.invalid/b.png"))
         nil
         "    "))
      (setq insert-calls (nreverse insert-calls))
      (should (= 5 (length insert-calls)))
      (should (equal '(:img-a :img-b :img-a :img-b :img-b)
                     (mapcar #'car insert-calls)))
      (dolist (call insert-calls)
        (should (equal "[image]" (nth 1 call)))
        (should-not (nth 2 call))
        (should (consp (nth 3 call)))))))

(ert-deftest disco-embed-preview-attachment-separates-source-and-proxy-urls ()
  "Proxy URLs feed previews while the original CDN URL remains the open target."
  (let* ((embed '((type . "rich")
                  (image . ((url . "https://cdn.example.invalid/cat.png")
                            (proxy_url . "https://media.example.invalid/cat.png")
                            (width . 640)
                            (height . 480)))))
         (attachment
          (disco-embed--preview-attachment '((id . "m1")) embed 1)))
    (should (equal (alist-get 'url attachment)
                   "https://cdn.example.invalid/cat.png"))
    (should (equal (alist-get 'proxy_url attachment)
                   "https://media.example.invalid/cat.png"))))

(ert-deftest disco-embed-attachment-scheme-source-resolves-original-url ()
  (let* ((msg '((id . "m1")
                (attachments
                 . (((filename . "cat.png")
                     (url . "https://cdn.example.invalid/cat.png")
                     (proxy_url . "https://media.example.invalid/cat.png"))))))
         (embed '((type . "rich")
                  (image . ((url . "attachment://cat.png")))))
         (attachment (disco-embed--preview-attachment msg embed 1)))
    (should (equal (alist-get 'url attachment)
                   "https://cdn.example.invalid/cat.png"))
    (should (equal (alist-get 'proxy_url attachment)
                   "https://media.example.invalid/cat.png"))))

(ert-deftest disco-embed-message-preview-cache-keys-cover-media-and-author-icon ()
  (let* ((msg
          '((id . "m1")
            (embeds
             . (((type . "rich")
                 (image . ((url . "https://media.invalid/image.png")))
                 (author
                  . ((name . "author")
                     (icon_url . "https://media.invalid/avatar.png"))))))))
         (keys (disco-embed-message-preview-cache-keys msg)))
    (should (= 2 (length keys)))
    (should (seq-some (lambda (key) (string-match-p "embed:m1:1:image" key))
                      keys))
    (should (seq-some (lambda (key) (string-match-p "embed-author-icon:m1:1" key))
                      keys))))

(ert-deftest disco-embed-image-actions-present-only-after-owned-acquisition ()
  (let ((file (make-temp-file "disco-embed-open-" nil ".txt"))
        (disco-embed-show-author-icons nil)
        (disco-embed-show-image-previews t)
        (disco-embed-show-urls nil)
        resolve requested stale-action)
    (unwind-protect
        (progn
          (with-temp-file file (insert "image presentation fixture"))
          (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p)
                     (lambda () t))
                    ((symbol-function 'disco-media-attachment-preview-image)
                     (lambda (&rest _) :preview))
                    ((symbol-function 'appkit-media-insert-image-slices)
                     (lambda (_image action &rest _)
                       (appkit-ui-insert-action-button "[Preview]" action)))
                    ((symbol-function 'appkit-media-image-acquisition-start)
                     (lambda (_context input _observe success _reject)
                       (push (alist-get 'url (appkit-media-image-acquisition-resource input)) requested)
                       (setq resolve success)
                       (appkit-cancellation-create :kind 'transport :cancel #'ignore)))
                    ((symbol-function 'browse-url)
                     (lambda (&rest _) (error "Image action escaped to browser"))))
            (disco-room-test-with-surface "embed-actions"
              (let ((surface (appkit-current-surface)))
                (dolist (label '("Image title" "[Open]" "[Media]" "[Icon]" "[Preview]"))
                  (with-current-buffer (appkit-surface-buffer surface)
                    (let ((inhibit-read-only t))
                      (goto-char (point-min))
                      (let ((start (point)))
                        (disco-embed-insert-card
                         '((id . "embed-message"))
                         '((type . "image") (title . "Image title")
                           (url . "https://proxy.invalid/media.png")
                           (image . ((url . "https://cdn.invalid/media.png")
                                     (proxy_url . "https://proxy.invalid/media.png")))
                           (author . ((name . "Author")
                                      (icon_url . "https://cdn.invalid/icon.png")
                                      (proxy_icon_url . "https://proxy.invalid/icon.png"))))
                         1 surface)
                        (let ((end (point)))
                          (goto-char start)
                          (search-forward label end)
                          (let* ((position (- (point) (length label)))
                                 (button (button-at position)))
                            (if button
                                (let ((action (button-get button 'action)))
                                  (setq stale-action (lambda () (funcall action button)))
                                  (button-activate button))
                              (setq stale-action (appkit-ui-action-at position))
                              (appkit-ui-activate-at position)))))))
                  (should-not (get-file-buffer file))
                  (funcall resolve file)
                  (should-not (get-file-buffer file))
                  (disco-room-test-drain surface)
                  (let ((viewer (get-file-buffer file)))
                    (should (equal (with-current-buffer viewer (buffer-string))
                                   "image presentation fixture"))
                    (kill-buffer viewer)))
                (should (equal (nreverse requested)
                               '("https://proxy.invalid/media.png"
                                 "https://proxy.invalid/media.png"
                                 "https://cdn.invalid/media.png"
                                 "https://cdn.invalid/icon.png"
                                 "https://cdn.invalid/media.png")))
                (appkit-surface-stop surface)
                (should-error (funcall stale-action))))))
      (when-let* ((viewer (get-file-buffer file))) (kill-buffer viewer))
      (delete-file file))))

(provide 'disco-embed-test)

;;; disco-embed-test.el ends here
