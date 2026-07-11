;;; disco-chat-avatar-test.el --- Tests for shared chat avatars -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory
                                (or load-file-name buffer-file-name))))

(require 'disco-chat-avatar)

(ert-deftest disco-chat-avatar-prefixes-have-stable-text-placeholder ()
  (let ((prefixes (disco-chat-avatar-prefixes nil "@")))
    (should (equal (plist-get prefixes :header) "@ "))
    (should (equal (plist-get prefixes :first-body) "  "))
    (should (equal (plist-get prefixes :rest-body) "  "))))

(ert-deftest disco-chat-avatar-placeholder-reserves-loaded-image-width ()
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _args) 8)))
    (let ((prefixes
           (disco-chat-avatar-prefixes nil "A" :pixel-size 32)))
      (should (equal (plist-get prefixes :header) "A    "))
      (should (equal (plist-get prefixes :first-body) "     "))
      (should (equal (plist-get prefixes :rest-body) "     ")))))

(ert-deftest disco-chat-avatar-prefixes-slice-image-across-two-lines ()
  (cl-letf (((symbol-function 'disco-media-image-object-valid-p)
             (lambda (image) (consp image)))
            ((symbol-function 'image-size)
             (lambda (&rest _args) (cons 32 32)))
            ((symbol-function 'frame-char-width) (lambda (&rest _args) 8)))
    (let* ((prefixes
            (disco-chat-avatar-prefixes
             '(image :type png :data "avatar") "A"
             :pixel-size 32
             :resize t))
           (header (plist-get prefixes :header))
           (first-body (plist-get prefixes :first-body))
           (rest-body (plist-get prefixes :rest-body))
           (header-display (get-text-property 0 'display header))
           (body-display (get-text-property 0 'display first-body)))
      (should (= (string-width header) (string-width first-body)))
      (should (= (string-width header) (string-width rest-body)))
      (should (equal (car header-display) '(slice 0 0 1.0 16)))
      (should (equal (car body-display) '(slice 0 16 1.0 16)))
      (should-not (get-text-property 0 'display rest-body)))))

(provide 'disco-chat-avatar-test)
;;; disco-chat-avatar-test.el ends here
