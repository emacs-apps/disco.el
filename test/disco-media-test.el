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

(ert-deftest disco-media-card-context-prefers-exact-card-before-fallback ()
  "Shared actions stay attachment-specific inside multi-media messages."
  (with-temp-buffer
    (let* ((opened nil)
           (exact (disco-media-card-context-create
                   :payload 'exact
                   :kind 'image
                   :open-action (lambda () (setq opened 'exact))))
           (fallback (disco-media-card-context-create
                      :payload 'fallback
                      :kind 'video
                      :open-action (lambda () (setq opened 'fallback))))
           (card-start (point)))
      (insert "exact card\n")
      (add-text-properties
       card-start (point)
       (list disco-media-card-context-property exact))
      (insert "message text\n")
      (setq-local disco-media-card-fallback-context-function
                  (lambda () fallback))
      (goto-char card-start)
      (should (eq (plist-get (disco-media-card-context-at-point) :payload)
                  'exact))
      (disco-media-card-open)
      (should (eq opened 'exact))
      (forward-line 1)
      (should (eq (plist-get (disco-media-card-context-at-point) :payload)
                  'fallback))
      (disco-media-card-open)
      (should (eq opened 'fallback)))))

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
        (disco-media--video-preview-processes (make-hash-table :test #'equal))
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
             '((url . "https://example.invalid/video.mp4")
               (content_type . "video/mp4"))
             (expand-file-name "preview" cache-dir))
          (delete-directory cache-dir t)))
      (should command)
      (should-not (member "-ss" command))
      (should (member "thumbnail=24,scale=960:-2:force_original_aspect_ratio=decrease"
                      command)))))

(ert-deftest disco-media-static-video-preview-prefers-poster-source ()
  (let ((disco-media--attachment-preview-image-cache (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetching (make-hash-table :test #'equal))
        (disco-media--video-preview-processes (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetch-budget nil)
        command)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (equal program "ffmpeg") "/usr/bin/ffmpeg")))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq command (plist-get plist :command))
                 'dummy-process))
              ((symbol-function 'generate-new-buffer)
               (lambda (&rest _args)
                 (get-buffer-create " *disco-video-poster-test*"))))
      (let ((cache-dir (make-temp-file "disco-video-poster" t)))
        (unwind-protect
            (disco-media--start-video-preview-fetch
             "poster-key"
             '((url . "https://cdn.example/video.mp4")
               (proxy_url . "https://media.example/poster.jpg")
               (content_type . "video/mp4")
               (size . 99999999))
             (expand-file-name "preview" cache-dir))
          (delete-directory cache-dir t)))
      (should (member "https://media.example/poster.jpg" command))
      (should-not (member "https://cdn.example/video.mp4" command)))))

(ert-deftest disco-media-short-small-video-preview-uses-animated-gif ()
  (let ((disco-media--attachment-preview-image-cache (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetching (make-hash-table :test #'equal))
        (disco-media--video-preview-processes (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetch-budget nil)
        (disco-media-inline-animations t)
        (disco-media-inline-animation-max-duration 10)
        (disco-media-inline-animation-max-file-size 4096)
        command)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (pcase program
                   ("ffmpeg" "/usr/bin/ffmpeg")
                   ("ffprobe" "/usr/bin/ffprobe"))))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq command (plist-get plist :command))
                 'dummy-process))
              ((symbol-function 'generate-new-buffer)
               (lambda (&rest _args)
                 (get-buffer-create " *disco-video-animation-test*"))))
      (let ((cache-dir (make-temp-file "disco-video-animation" t)))
        (unwind-protect
            (disco-media--start-video-preview-fetch
             "animation-key"
             '((url . "https://example.invalid/short.mp4")
               (content_type . "video/mp4")
               (size . 2048)
               (duration_secs . 2.5))
             (expand-file-name "preview" cache-dir))
          (delete-directory cache-dir t)))
      (should command)
      (should (member "-filter_complex" command))
      (should (member "-loop" command))
      (should (string-suffix-p ".gif" (car (last command))))
      (should-not (seq-some (lambda (argument)
                              (and (stringp argument)
                                   (string-prefix-p "thumbnail=" argument)))
                            command)))))

(ert-deftest disco-media-video-preview-policy-separates-animation-caches ()
  (let ((attachment '((id . "video")
                      (filename . "clip.mp4")
                      (content_type . "video/mp4")
                      (url . "https://cdn.example/clip.mp4")))
        animated-key static-key)
    (let ((disco-media-inline-animations t))
      (setq animated-key
            (disco-media-attachment-preview-cache-key attachment)))
    (let ((disco-media-inline-animations nil))
      (setq static-key
            (disco-media-attachment-preview-cache-key attachment)))
    (should-not (equal animated-key static-key))))

(ert-deftest disco-media-marks-only-bounded-multi-frame-previews ()
  (let ((disco-media-inline-animations t)
        (disco-media-inline-animation-max-duration 10)
        (disco-media-inline-animation-max-file-size 4096)
        (image '(image :type gif)))
    (cl-letf (((symbol-function 'disco-media-image-object-valid-p)
               (lambda (_image) t))
              ((symbol-function 'disco-media--file-size)
               (lambda (_file) 2048))
              ((symbol-function 'disco-media--inline-animation-frame-data)
               (lambda (_image) '(20 . 2.5))))
      (should (eq image
                  (disco-media--mark-inline-animation-image image "/tmp/a.gif")))
      (should (disco-media-inline-animation-image-p image))
      (should (= 2.5 (plist-get (cdr image)
                                :disco-inline-animation-duration))))))

(ert-deftest disco-media-start-inline-animation-is-one-bounded-cycle ()
  (let ((disco-media-inline-animations t)
        (image '(image :type gif
                       :disco-inline-animation t
                       :disco-inline-animation-duration 2.0))
        animated reset-delay)
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) :window))
                ((symbol-function 'image-animate)
                 (lambda (candidate index limit &optional _position)
                   (setq animated (list candidate index limit))))
                ((symbol-function 'run-at-time)
                 (lambda (delay _repeat function &rest args)
                   (setq reset-delay delay)
                   (list function args))))
        (should (disco-media-start-inline-animation image))
        (should (equal animated (list image 0 nil)))
        (should (= 2.4 reset-delay))
        (should (plist-get (cdr image) :disco-inline-animation-played))))))

(ert-deftest disco-media-scroll-discovers-newly-visible-animation ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((window (selected-window))
             (image '(image :type gif :disco-inline-animation t))
             position started)
        (dotimes (index 20)
          (insert (format "line %s\n" index)))
        (setq position (point))
        (insert (propertize "x" 'display image))
        (set-window-buffer window (current-buffer))
        (set-window-start window position)
        (cl-letf (((symbol-function 'disco-media-start-inline-animation)
                   (lambda (candidate) (setq started candidate))))
          (disco-media--start-window-inline-animations-after-scroll
           window position))
        (should (eq started image))))))

(ert-deftest disco-media-stop-inline-animation-ignores-non-images ()
  (should-not (disco-media-stop-inline-animation '(20)))
  (should-not (disco-media-stop-inline-animation nil)))

(ert-deftest disco-media-cancel-video-preview-detaches-before-killing ()
  (let* ((disco-media--video-preview-processes
          (make-hash-table :test #'equal))
         (buffer (generate-new-buffer " *disco-cancel-video-test*"))
         deleted)
    (puthash "preview" 'process disco-media--video-preview-processes)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (process)
                 (setq deleted process)
                 (should-not
                  (gethash "preview" disco-media--video-preview-processes))))
              ((symbol-function 'process-buffer)
               (lambda (_process) buffer)))
      (should (disco-media-cancel-video-preview "preview"))
      (should (eq deleted 'process))
      (should-not (buffer-live-p buffer))
      (should-not (gethash "preview" disco-media--video-preview-processes)))))

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

(ert-deftest disco-media-animated-video-preview-keeps-multi-frame-image ()
  (let ((image '(image :type gif :disco-inline-animation t)))
    (cl-letf (((symbol-function 'disco-media--decorate-preview-image)
               (lambda (&rest _args)
                 (ert-fail "animated previews must not be flattened to SVG"))))
      (should (eq image (disco-media-attachment-video-display-image image))))))

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

(ert-deftest disco-media-open-remote-image-caches-and-opens-in-emacs ()
  "Remote images are cached by their bytes and never sent to a browser."
  (let* ((disco-media-preview-cache-directory
          (make-temp-file "disco-media-open" t))
         (disco-media-animate-gifs nil)
         (resource '((url . "https://example.com/assets/no-extension")))
         opened-file)
    (unwind-protect
        (cl-letf (((symbol-function 'url-copy-file)
                   (lambda (_url target &optional _ok-if-already-exists)
                     (with-temp-file target
                       (set-buffer-multibyte nil)
                       (insert "GIF89a-discovery"))))
                  ((symbol-function 'browse-url)
                   (lambda (&rest _args)
                     (ert-fail "images must not open in a browser"))))
          (let ((disco-media-open-file-function
                 (lambda (file)
                   (setq opened-file file))))
            (disco-media-open-resource resource 'image "image:test"))
          (should (disco-media-file-present-p opened-file))
          (should (equal (file-name-extension opened-file) "gif"))
          (should (file-in-directory-p
                   opened-file disco-media-preview-cache-directory)))
      (when (file-directory-p disco-media-preview-cache-directory)
        (delete-directory disco-media-preview-cache-directory t)))))

(ert-deftest disco-media-open-gif-starts-only-an-idle-animation ()
  "Opening a GIF starts looping without toggling an active animation off."
  (let ((disco-media-animate-gifs t)
        (gif "/tmp/disco-animated.gif")
        timer
        (toggle-count 0))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-file-buffer)
                 (lambda (_file) (current-buffer)))
                ((symbol-function 'derived-mode-p)
                 (lambda (&rest _modes) t))
                ((symbol-function 'image-get-display-property)
                 (lambda () '(image :type gif)))
                ((symbol-function 'image-multi-frame-p)
                 (lambda (_image) '(2 . 0.1)))
                ((symbol-function 'image-animate-timer)
                 (lambda (_image) timer))
                ((symbol-function 'image-toggle-animation)
                 (lambda () (cl-incf toggle-count))))
        (disco-media--maybe-start-gif-animation gif)
        (should (= toggle-count 1))
        (should image-animate-loop)
        (setq timer t)
        (disco-media--maybe-start-gif-animation gif)
        (should (= toggle-count 1))))))

(ert-deftest disco-media-open-video-url-uses-configured-player ()
  "Video URLs are passed to the configured player without a browser path."
  (let ((disco-media-video-player-command "mpv --no-terminal")
        command)
    (cl-letf (((symbol-function 'disco-media--command-runnable-p)
               (lambda (_configured-command) t))
              ((symbol-function 'make-process)
               (lambda (&rest properties)
                 (setq command (plist-get properties :command))
                 'disco-test-player))
              ((symbol-function 'browse-url)
               (lambda (&rest _args)
                 (ert-fail "videos must not open in a browser"))))
      (should
       (eq (disco-media-open-resource
            '((url . "https://example.com/movie.mp4")) 'video)
           'disco-test-player))
      (should (equal command
                     '("mpv" "--no-terminal"
                       "https://example.com/movie.mp4"))))))

(ert-deftest disco-media-open-video-errors-when-player-is-unavailable ()
  "Missing video players are explicit rather than browser fallbacks."
  (let ((disco-media-video-player-command nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (&rest _args)
                 (ert-fail "videos must not open in a browser"))))
      (should-error
       (disco-media-open-resource
        '((url . "https://example.com/movie.mp4")) 'video)
       :type 'user-error))))

(ert-deftest disco-media-remote-file-download-streams-asynchronously-with-plz ()
  (let ((disco-media--open-file-plz-queue nil)
        queued-properties
        success-value
        failure)
    (cl-letf (((symbol-function 'make-plz-queue) (lambda (&rest _) 'queue))
              ((symbol-function 'plz-run) #'ignore)
              ((symbol-function 'plz-queue)
               (lambda (&rest arguments)
                 (setq queued-properties (nthcdr 3 arguments))))
              ((symbol-function 'url-copy-file)
               (lambda (&rest _)
                 (ert-fail "file downloads must not block on url-copy-file"))))
      (disco-media-copy-or-download-resource-async
       '((url . "https://example.com/report.pdf"))
       "/tmp/disco-report.pdf"
       (lambda (file) (setq success-value file))
       (lambda (reason) (setq failure reason)))
      (should-not success-value)
      (should-not failure)
      (should (equal (plist-get queued-properties :as)
                     '(file "/tmp/disco-report.pdf")))
      (funcall (plist-get queued-properties :then) "/tmp/disco-report.pdf")
      (should (equal success-value "/tmp/disco-report.pdf")))))

(ert-deftest disco-media-open-photo-attachment-uses-original-not-proxy-url ()
  "The CDN proxy remains preview-only when opening a Discord image."
  (let (opened-arguments)
    (cl-letf (((symbol-function 'disco-media-attachment-download-state)
               (lambda (_attachment)
                 '(:status not-downloaded :path "/missing/cat.png")))
              ((symbol-function 'disco-media-open-resource)
               (lambda (&rest arguments)
                 (setq opened-arguments arguments))))
      (disco-media-open-attachment
       '((id . "42")
         (filename . "cat.png")
         (content_type . "image/png")
         (url . "https://cdn.example.invalid/cat.png")
         (proxy_url . "https://media.example.invalid/cat.png")))
      (should (equal
               (alist-get 'url (car opened-arguments))
               "https://cdn.example.invalid/cat.png"))
      (should (eq (nth 1 opened-arguments) 'image)))))

(ert-deftest disco-media-action-properties-wrap-zero-argument-callbacks ()
  (with-temp-buffer
    (let ((called nil))
      (insert "media")
      (disco-media-add-action-properties
       (point-min) (point-max) (lambda () (setq called t)) "Open media")
      (let* ((map (get-text-property (point-min) 'keymap))
             (command (lookup-key map (kbd "RET"))))
        (should (commandp command))
        (call-interactively command)
        (should called)))))

(ert-deftest disco-media-state-notifications-pass-explicit-resource ()
  (let (received)
    (let ((disco-media-rerender-function
           (lambda (kind key) (setq received (cons kind key)))))
      (disco-media--notify-state-updated 'preview "preview-key")
      (should (equal '(preview . "preview-key") received)))))

(provide 'disco-media-test)

;;; disco-media-test.el ends here
