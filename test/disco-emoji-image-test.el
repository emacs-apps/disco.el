;;; disco-emoji-image-test.el --- Tests for custom emoji images -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-emoji-image)

(ert-deftest disco-emoji-image-url-respects-size-and-animation-policy ()
  (let ((disco-emoji-image-size 30)
        (disco-emoji-image-animate t)
        (appkit-media-inline-animation-enabled t))
    (should
     (equal
      "https://cdn.discordapp.com/emojis/123.webp?size=32&animated=true"
      (disco-emoji-image-url "123" t)))
    (should
     (equal
      "https://cdn.discordapp.com/emojis/123.webp?size=32"
      (disco-emoji-image-url "123" nil)))
    (let ((disco-emoji-image-animate nil))
      (should
       (equal
        "https://cdn.discordapp.com/emojis/123.webp?size=32"
        (disco-emoji-image-url "123" t))))
    (let ((appkit-media-inline-animation-enabled nil))
      (should
       (equal
        "https://cdn.discordapp.com/emojis/123.webp?size=32"
        (disco-emoji-image-url "123" t))))))

(ert-deftest disco-emoji-image-does-no-io-without-inline-rendering ()
  (let ((requests 0))
    (cl-letf (((symbol-function
                'appkit-media-inline-image-rendering-available-p)
               #'ignore)
              ((symbol-function 'disco-emoji-image--start-fetch)
               (lambda (&rest _) (cl-incf requests))))
      (should-not (disco-emoji-image-image "123" t))
      (should (= 0 requests)))))

(ert-deftest disco-emoji-image-reset-retires-pending-fetch ()
  (let ((disco-emoji-image--images (make-hash-table :test #'equal))
        (disco-emoji-image--fetching (make-hash-table :test #'equal))
        (disco-emoji-image--known (make-hash-table :test #'equal))
        (disco-emoji-image--pending-resource-updates
         (make-hash-table :test #'equal))
        (disco-emoji-image--resource-update-timer nil)
        (disco-emoji-image--generation 4)
        success
        cancelled)
    (cl-letf (((symbol-function
                'appkit-media-inline-image-rendering-available-p)
               (lambda () t))
              ((symbol-function 'appkit-media-image-cache-existing-file)
               #'ignore)
              ((symbol-function 'make-directory) #'ignore)
              ((symbol-function 'appkit-media-cache-image-resource-async)
               (lambda (_resource _cache-base on-success _on-error &rest _)
                 (setq success on-success)
                 'handle))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (handle) (setq cancelled handle)))
              ((symbol-function 'disco-emoji-image--decode-file)
               (lambda (_file) 'image-object))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (image) (eq image 'image-object))))
      (should-not (disco-emoji-image-image "123" nil))
      (should (= 1 (hash-table-count disco-emoji-image--fetching)))
      (disco-emoji-image-reset)
      (should (eq cancelled 'handle))
      (funcall success "/tmp/emoji.webp")
      (should (= 0 (hash-table-count disco-emoji-image--images)))
      (should (= 0 (hash-table-count
                    disco-emoji-image--pending-resource-updates))))))

(ert-deftest disco-emoji-image-completion-prefix-reflects-cache-state ()
  (let ((available nil)
        (prefix (disco-emoji-image-completion-prefix "123" nil)))
    (cl-letf (((symbol-function 'disco-emoji-image-image)
               (lambda (&rest _) (and available 'image-object)))
              ((symbol-function
                'appkit-media-one-line-image-display-string)
               (lambda (image fallback)
                 (propertize fallback 'display image))))
      (should (equal "" (funcall prefix nil)))
      (setq available t)
      (let ((value (funcall prefix nil)))
        (should (= 2 (length value)))
        (should (eq 'image-object
                    (get-text-property 0 'display value)))))))

(ert-deftest disco-emoji-image-flush-coalesces-resource-notifications ()
  (let ((disco-emoji-image--pending-resource-updates
         (make-hash-table :test #'equal))
        (disco-emoji-image--resource-update-timer 'timer)
        received)
    (puthash '(:emoji ("1" 32 nil)) t
             disco-emoji-image--pending-resource-updates)
    (puthash '(:emoji ("2" 32 t)) t
             disco-emoji-image--pending-resource-updates)
    (let ((disco-emoji-image-resources-updated-hook
           (list (lambda (resources) (setq received resources)))))
      (disco-emoji-image--flush-resource-updates))
    (should-not disco-emoji-image--resource-update-timer)
    (should (= 0 (hash-table-count
                  disco-emoji-image--pending-resource-updates)))
    (should (= 2 (length received)))
    (should (member '(:emoji ("1" 32 nil)) received))
    (should (member '(:emoji ("2" 32 t)) received))))

(provide 'disco-emoji-image-test)

;;; disco-emoji-image-test.el ends here
