;;; disco-media-test.el --- Tests for disco-media helpers -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-media)

(defvar disco-room-attachment-download-directory)

(ert-deftest disco-media-attachment-kind-and-summary-classify-common-types ()
  (let* ((photo '((filename . "cat.png")
                  (size . 4096)
                  (width . 640)
                  (height . 480)))
         (video '((content_type . "video/mp4")
                  (filename . "clip.mp4")
                  (size . 8192)
                  (width . 1280)
                  (height . 720)))
         (audio '((content_type . "audio/ogg")
                  (filename . "voice.ogg")
                  (duration_secs . 61.0)))
         (document '((filename . "report.pdf")
                     (size . 128))))
    (should (eq 'photo (disco-media-attachment-kind photo)))
    (should (eq 'video (disco-media-attachment-kind video)))
    (should (eq 'audio (disco-media-attachment-kind audio)))
    (should (eq 'document (disco-media-attachment-kind document)))
    (should (string-prefix-p "[img] cat.png"
                             (disco-media-attachment-summary photo)))
    (should (string-match-p (regexp-quote "640x480")
                            (disco-media-attachment-summary photo)))
    (should (string-prefix-p "[video] clip.mp4"
                             (disco-media-attachment-summary video)))
    (should (string-prefix-p "[audio] voice.ogg"
                             (disco-media-attachment-summary audio)))
    (should (string-match-p (regexp-quote "1:01")
                            (disco-media-attachment-summary audio)))
    (should (string-prefix-p "[file] report.pdf"
                             (disco-media-attachment-summary document)))))

(ert-deftest disco-media-attachment-display-name-prefers-title ()
  (should (equal "Quarterly report"
                 (disco-media-attachment-display-name
                  '((title . "Quarterly report")
                    (filename . "report.pdf")))))
  (should (equal "cat.png"
                 (disco-media-attachment-display-name
                  '((filename . "SPOILER_cat.png")))))
  (should (equal "attachment-42"
                 (disco-media-attachment-display-name
                  '((id . "42"))))))

(ert-deftest disco-media-attachment-spoiler-p-detects-common-discord-shapes ()
  (should (disco-media-attachment-spoiler-p
           '((flags . 8) (filename . "cat.png"))))
  (should (disco-media-attachment-spoiler-p
           '((flags . "8") (filename . "cat.png"))))
  (should (disco-media-attachment-spoiler-p
           '((is_spoiler . t) (filename . "cat.png"))))
  (should (disco-media-attachment-spoiler-p
           '((filename . "SPOILER_cat.png"))))
  (should-not (disco-media-attachment-spoiler-p
               '((filename . "cat.png"))))
  (should (equal "[spoiler image hidden]"
                 (disco-media-attachment-spoiler-label
                  '((filename . "SPOILER_cat.png")))))
  (should (equal "[spoiler attachment hidden]"
                 (disco-media-attachment-spoiler-label
                  '((filename . "secret.bin"))))))

(ert-deftest disco-media-attachment-meta-line-includes-duration-and-ephemeral ()
  (let ((meta (disco-media-attachment-meta-line
               '((content_type . "audio/ogg")
                 (duration_secs . 61.0)
                 (ephemeral . t))
               t)))
    (should (string-match-p (regexp-quote "type=audio/ogg") meta))
    (should (string-match-p (regexp-quote "duration=1:01") meta))
    (should (string-match-p (regexp-quote "ephemeral") meta))))

(ert-deftest disco-media-svg-append-spoiler-node-adds-noise-filter ()
  (skip-unless (and (fboundp 'svg-create)
                    (fboundp 'svg-print)
                    (fboundp 'svg--append)
                    (fboundp 'dom-node)))
  (let ((svg (svg-create 8 8)))
    (disco-media--svg-append-spoiler-node svg "noise")
    (let ((xml (with-temp-buffer
                 (svg-print svg)
                 (buffer-string))))
      (should (string-match-p "feTurbulence" xml))
      (should (string-match-p "feDisplacementMap" xml))
      (should (string-match-p "id=\"noise\"" xml)))))

(ert-deftest disco-media-attachment-preview-image-falls-back-to-placeholder ()
  (cl-letf (((symbol-function 'disco-media-attachment-preview-rendering-available-p)
             (lambda () t))
            ((symbol-function 'disco-media--attachment-image-p)
             (lambda (_attachment) t))
            ((symbol-function 'disco-media--attachment-video-p)
             (lambda (_attachment) nil))
            ((symbol-function 'disco-media--attachment-real-preview-image)
             (lambda (&rest _args) nil))
            ((symbol-function 'disco-media-attachment-placeholder-image)
             (lambda (_attachment) :placeholder)))
    (should (eq :placeholder
                (disco-media-attachment-preview-image
                 '((filename . "cat.png") (placeholder . "abcd")))))))

(ert-deftest disco-media-attachment-spoiler-preview-image-falls-back-to-placeholder ()
  (cl-letf (((symbol-function 'disco-media-attachment-preview-rendering-available-p)
             (lambda () t))
            ((symbol-function 'disco-media--attachment-real-spoiler-preview-image)
             (lambda (_attachment) nil))
            ((symbol-function 'disco-media-attachment-spoiler-placeholder-image)
             (lambda (_attachment) :spoiler-placeholder)))
    (should (eq :spoiler-placeholder
                (disco-media-attachment-spoiler-preview-image
                 '((filename . "SPOILER_cat.png") (placeholder . "abcd")))))))

(ert-deftest disco-media-attachment-download-state-detects-existing-file ()
  (let ((tmpdir (make-temp-file "disco-media-downloads" t))
        (disco-media--attachment-download-state-table (make-hash-table :test #'equal)))
    (unwind-protect
        (let* ((disco-room-attachment-download-directory tmpdir)
               (attachment '((id . "42")
                             (filename . "doc.txt")
                             (url . "https://example.invalid/doc.txt")))
               (path (disco-media-attachment-download-path attachment)))
          (with-temp-file path
            (insert "cached"))
          (let ((state (disco-media-attachment-download-state attachment)))
            (should (eq 'downloaded (plist-get state :status)))
            (should (equal path (plist-get state :path)))))
      (delete-directory tmpdir t))))

(provide 'disco-media-test)

;;; disco-media-test.el ends here
