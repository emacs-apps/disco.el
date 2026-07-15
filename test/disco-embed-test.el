;;; disco-embed-test.el --- Tests for disco-embed -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-embed)

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
    (should (equal "| "
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

(ert-deftest disco-embed-preview-slice-opens-image-through-shared-backend ()
  (let (open-url open-key)
    (with-temp-buffer
      (cl-letf (((symbol-function 'image-size)
                 (lambda (&rest _args) '(4 . 2)))
                ((symbol-function 'insert-image)
                 (lambda (_image fallback &optional _area _slice)
                   (insert fallback)))
                ((symbol-function 'appkit-media-add-open-image-properties)
                 (lambda (_start _end resource &rest options)
                   (setq open-url (alist-get 'url resource)
                         open-key (plist-get options :cache-key)))))
        (disco-embed--insert-preview-image-slice
         :image 0 "https://cdn.example.invalid/cat.png"
         "[image]" "embed-open-image:cat")))
    (should (equal open-url "https://cdn.example.invalid/cat.png"))
    (should (equal open-key "embed-open-image:cat"))))

(ert-deftest disco-embed-direct-image-title-and-open-stay-in-emacs ()
  (let* ((url "https://cdn.example.invalid/cat.png")
         (msg '((id . "m1")))
         (embed `((type . "image") (url . ,url) (title . "cat")))
         title-resource
         opened-resource)
    (with-temp-buffer
      (let ((disco-embed-show-image-previews nil)
            (disco-embed-show-urls nil))
        (cl-letf (((symbol-function 'appkit-media-add-open-image-properties)
                   (lambda (_start _end resource &rest _options)
                     (setq title-resource resource)))
                  ((symbol-function 'appkit-media-add-open-url-properties)
                   (lambda (&rest _arguments)
                     (ert-fail "direct image must not use browser properties")))
                  ((symbol-function 'disco-media-open-discord-resource)
                   (lambda (resource kind &optional _cache-key)
                     (should (eq kind 'image))
                     (setq opened-resource resource)))
                  ((symbol-function 'browse-url)
                   (lambda (&rest _arguments)
                     (ert-fail "direct image action must not use a browser"))))
          (disco-embed-insert-card msg embed 1)
          (should (equal url (alist-get 'url title-resource)))
          (should-not (string-match-p (regexp-quote "[Media]")
                                      (buffer-string)))
          (goto-char (point-min))
          (search-forward "[Open]")
          (let ((button (button-at (match-beginning 0))))
            (should button)
            (button-activate button))
          (should (equal url (alist-get 'url opened-resource))))))))

(ert-deftest disco-embed-insert-card-threads-exact-video-owner ()
  (let ((owner (list 'exact-disco-app))
        captured-owners)
    (with-temp-buffer
      (let ((disco-embed-show-urls t))
        (cl-letf (((symbol-function 'disco-embed--add-url-properties)
                   (lambda (_start _end _url _kind &optional received-owner)
                     (push received-owner captured-owners)))
                  ((symbol-function 'disco-embed--insert-preview-row)
                   (lambda (&rest arguments)
                     (push (car (last arguments)) captured-owners)))
                  ((symbol-function 'disco-embed--insert-action-row)
                   (lambda (&rest arguments)
                     (push (car (last arguments)) captured-owners))))
          (disco-embed-insert-card
           '((id . "m-video"))
           '((type . "video")
             (title . "clip")
             (url . "https://example.invalid/watch")
             (video . ((url . "https://example.invalid/clip.mp4"))))
           1 owner))))
    (should (>= (length captured-owners) 3))
    (should (seq-every-p (lambda (captured) (eq owner captured))
                         captured-owners))))

(ert-deftest disco-embed-video-properties-and-actions-capture-exact-owner ()
  (let ((owner (list 'exact-disco-app))
        property-owner
        main-action-owner
        play-action-owner
        play-action)
    (with-temp-buffer
      (insert "video")
      (cl-letf (((symbol-function 'appkit-media-add-play-video-properties)
                 (lambda (_start _end _url _label &rest options)
                   (setq property-owner (plist-get options :owner))))
                ((symbol-function 'appkit-media-play-video-url)
                 (lambda (_url _label &rest options)
                   (if play-action
                       (setq play-action-owner (plist-get options :owner))
                     (setq main-action-owner (plist-get options :owner)))))
                ((symbol-function 'disco-embed--insert-action-button)
                 (lambda (label callback _help)
                   (when (equal label "[Play]")
                     (setq play-action callback))
                   (insert label))))
        (disco-embed--add-url-properties
         (point-min) (point-max) "https://example.invalid/clip.mp4" 'video owner)
        (funcall (car (disco-embed--url-action
                       "https://example.invalid/main.mp4" 'video owner)))
        (disco-embed--insert-action-row
         nil 'page nil "https://example.invalid/extra.mp4"
         nil nil nil nil "" owner)
        (should (functionp play-action))
        (funcall play-action)))
    (should (eq owner property-owner))
    (should (eq owner main-action-owner))
    (should (eq owner play-action-owner))))

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

(provide 'disco-embed-test)

;;; disco-embed-test.el ends here
