;;; disco-media-test.el --- Tests for disco-media helpers -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-media)

(defvar disco-media-download-directory)

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

(ert-deftest disco-media-attachment-waveform-string-decodes-and-colors-progress ()
  (let* ((attachment `((content_type . "audio/ogg")
                       (duration_secs . 4.0)
                       (waveform . ,(base64-encode-string
                                     (unibyte-string 0 64 255 128)
                                     t))))
         (text (disco-media-attachment-waveform-string
                attachment
                :width 8
                :progress 2.0
                :played-face 'success
                :unplayed-face 'shadow)))
    (should (stringp text))
    (should (= 8 (length text)))
    (let ((played-face (get-text-property 0 'face text))
          (unplayed-face (get-text-property 7 'face text)))
      (should (or (eq played-face 'success)
                  (and (listp played-face) (memq 'success played-face))))
      (should (or (eq unplayed-face 'shadow)
                  (and (listp unplayed-face) (memq 'shadow unplayed-face)))))))

(ert-deftest disco-media-attachment-waveform-image-renders-svg-bars ()
  (let* ((attachment `((content_type . "audio/ogg")
                       (duration_secs . 4.0)
                       (waveform . ,(base64-encode-string
                                     (unibyte-string 0 64 255 128)
                                     t))))
         lines)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'image-type-available-p)
               (lambda (_type) t))
              ((symbol-function 'svg-create)
               (lambda (&rest _args) :svg))
              ((symbol-function 'svg-line)
               (lambda (&rest args)
                 (push args lines)))
              ((symbol-function 'frame-char-height)
               (lambda () 16))
              ((symbol-function 'disco-media--svg-image)
               (lambda (_svg &rest props)
                 (list :waveform-image props))))
      (let ((image (disco-media-attachment-waveform-image
                    attachment :width 8 :progress 2.0)))
        (should (equal :waveform-image (car image)))
        (should (= 8 (length lines)))))))

(ert-deftest disco-media-svg-append-spoiler-node-adds-noise-filter ()
  (skip-unless (and (fboundp 'svg-create)
                    (fboundp 'svg-print)
                    (fboundp 'svg--append)
                    (fboundp 'dom-node)))
  (let ((svg (svg-create 8 8))
        (disco-media-spoiler-turbulence-base-frequency '(0.125 . 0.125))
        (disco-media-spoiler-turbulence-num-octaves 3)
        (disco-media-spoiler-displacement-min-scale 12.0)
        (disco-media-spoiler-displacement-max-scale 36.0)
        (disco-media-spoiler-displacement-divisor 4.0)
        (disco-media-spoiler-filter-margin-ratio 0.25))
    (disco-media--svg-append-spoiler-node svg "noise" 120 180)
    (let ((xml (with-temp-buffer
                 (svg-print svg)
                 (buffer-string))))
      (should (string-match-p "feTurbulence" xml))
      (should (string-match-p "feDisplacementMap" xml))
      (should (string-match-p "id=\"noise\"" xml))
      (should (string-match-p "baseFrequency=\"0.125 0.125\"" xml))
      (should (string-match-p "numOctaves=\"3\"" xml))
      (should (string-match-p "scale=\"30.0\"" xml))
      (should (string-match-p "width=\"150%\"" xml)))))

(ert-deftest disco-media-start-video-preview-fetch-uses-thumbnail-filter-without-seek ()
  (let ((disco-media--attachment-preview-image-cache (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetching (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetch-budget nil)
        command)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (prog)
                 (and (equal prog "ffmpeg") "/usr/bin/ffmpeg")))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq command (plist-get plist :command))
                 'dummy-process))
              ((symbol-function 'generate-new-buffer)
               (lambda (&rest _args)
                 (get-buffer-create " *disco-video-preview-test*")))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (let ((cache-dir (make-temp-file "disco-video-preview" t)))
        (unwind-protect
            (disco-media--start-video-preview-fetch
             "cache-key"
             "https://example.invalid/video.mp4"
             (expand-file-name "preview" cache-dir))
          (delete-directory cache-dir t)))
      (should command)
      (should-not (member "-ss" command))
      (should (member "thumbnail=24,scale=960:-2:force_original_aspect_ratio=decrease"
                      command)))))

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

(ert-deftest disco-media-attachment-spoiler-placeholder-image-avoids-extra-distortion ()
  (let ((disco-media-spoiler-placeholder-dim-opacity 0.17))
    (cl-letf (((symbol-function 'disco-media-attachment-placeholder-image)
               (lambda (_attachment) :placeholder))
              ((symbol-function 'disco-media--decorate-preview-image)
               (lambda (image &rest args)
                 (list image args))))
      (should (equal `(:placeholder (:spoiler-filter-p nil :video-p nil :dim-opacity ,disco-media-spoiler-placeholder-dim-opacity))
                     (disco-media-attachment-spoiler-placeholder-image
                      '((filename . "SPOILER_cat.png") (placeholder . "abcd"))))))))

(ert-deftest disco-media-attachment-video-display-image-adds-play-icon-decoration ()
  (cl-letf (((symbol-function 'disco-media--decorate-preview-image)
             (lambda (image &rest args)
               (list image args))))
    (should (equal '(:preview (:spoiler-filter-p nil :video-p t :dim-opacity 0.0))
                   (disco-media-attachment-video-display-image :preview)))))

(ert-deftest disco-media-decorate-preview-image-strips-source-props-from-output ()
  (let ((disco-media--attachment-decorated-preview-cache (make-hash-table :test #'equal))
        captured-props)
    (cl-letf (((symbol-function 'disco-media-image-object-valid-p)
               (lambda (image)
                 (memq image '(:decorated))
                 (or (eq image :decorated)
                     (and (consp image) (eq (car image) 'image)))))
              ((symbol-function 'image-type-available-p)
               (lambda (_type) t))
              ((symbol-function 'svg-create)
               (lambda (&rest _args) :svg))
              ((symbol-function 'svg-embed)
               (lambda (&rest _args) nil))
              ((symbol-function 'svg-print)
               (lambda (&rest _args) nil))
              ((symbol-function 'image-size)
               (lambda (&rest _args) '(80 . 40)))
              ((symbol-function 'disco-media--preview-image-source-spec)
               (lambda (_preview)
                 '(:source "/tmp/demo.png" :data-p nil :mime "image/png")))
              ((symbol-function 'disco-media--svg-image)
               (lambda (_svg &rest props)
                 (setq captured-props props)
                 :decorated)))
      (should (eq :decorated
                  (disco-media--decorate-preview-image
                   '(image :type png :file "/tmp/demo.png"
                     :height (2 . ch)
                     :disco-nslices 2)
                   :video-p nil)))
      (should (plist-get captured-props :height))
      (should (equal 2 (plist-get captured-props :disco-nslices)))
      (should-not (plist-get captured-props :file))
      (should-not (plist-get captured-props :data))
      (should-not (plist-get captured-props :type)))))

(ert-deftest disco-media-attachment-download-state-detects-existing-file ()
  (let ((tmpdir (make-temp-file "disco-media-downloads" t))
        (disco-media--attachment-download-state-table (make-hash-table :test #'equal)))
    (unwind-protect
        (let* ((disco-media-download-directory tmpdir)
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

(ert-deftest disco-media-play-attachment-audio-downloads-before-inline-playback ()
  (let ((disco-media--attachment-download-state-table (make-hash-table :test #'equal))
        (disco-media--attachment-audio-state-table (make-hash-table :test #'equal))
        (downloaded nil)
        started-source)
    (cl-letf (((symbol-function 'disco-media-audio-inline-playback-available-p)
               (lambda () t))
              ((symbol-function 'disco-media-attachment-download-state)
               (lambda (_attachment)
                 (list :status (if downloaded 'downloaded 'not-downloaded)
                       :path "/tmp/voice.ogg")))
              ((symbol-function 'file-exists-p)
               (lambda (path)
                 (and downloaded (equal path "/tmp/voice.ogg"))))
              ((symbol-function 'disco-media-start-attachment-download)
               (lambda (_attachment _open-after on-success)
                 (setq downloaded t)
                 (funcall on-success "/tmp/voice.ogg")))
              ((symbol-function 'disco-media--start-inline-audio-player)
               (lambda (_attachment source &optional _start-at)
                 (setq started-source source))))
      (disco-media-play-attachment-audio
       '((id . "a1")
         (filename . "voice.ogg")
         (url . "https://example.invalid/voice.ogg")))
      (should (equal "/tmp/voice.ogg" started-source)))))

(provide 'disco-media-test)

;;; disco-media-test.el ends here
