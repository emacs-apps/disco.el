;;; disco-media-test.el --- Tests for disco-media helpers -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-media)
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

(ert-deftest disco-media-video-preview-adapter-passes-explicit-metadata ()
  (let ((disco-media--attachment-preview-image-cache
         (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetching
         (make-hash-table :test #'equal))
        (disco-media--attachment-preview-owner-table
         (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetch-budget nil)
        captured)
    (cl-letf (((symbol-function 'appkit-media-start-video-preview)
               (lambda (&rest arguments)
                 (setq captured arguments))))
      (disco-media--start-video-preview-fetch
       "cache-key"
       '((url . "https://cdn.example/video.mp4")
         (proxy_url . "https://media.example/poster.jpg")
         (content_type . "video/mp4")
         (size . 2048)
         (duration_secs . 2.5))
       "/tmp/disco-preview"))
    (should (equal "disco:cache-key" (plist-get captured :key)))
    (should (equal "https://cdn.example/video.mp4"
                   (plist-get captured :source)))
    (should (equal "https://media.example/poster.jpg"
                   (plist-get captured :preview-source)))
    (should (= 2048 (plist-get captured :source-size)))
    (should (= 2.5 (plist-get captured :duration)))
    (should (equal "/tmp/disco-preview"
                   (plist-get captured :cache-base)))
    (should (functionp (plist-get captured :callback)))))

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

(ert-deftest disco-media-decorate-preview-image-strips-source-props-from-output ()
  (let ((disco-media--attachment-decorated-preview-cache (make-hash-table :test #'equal))
        captured-props)
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
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
			   :appkit-media-nslices 2)
                   :video-p nil)))
      (should (plist-get captured-props :height))
      (should (equal 2 (plist-get captured-props :appkit-media-nslices)))
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

(ert-deftest disco-media-attachment-preview-uses-shared-image-transfer ()
  (let ((disco-media--attachment-preview-fetching
         (make-hash-table :test #'equal))
        (disco-media--attachment-preview-image-cache
         (make-hash-table :test #'equal))
        (disco-media--attachment-preview-owner-table
         (make-hash-table :test #'equal))
        captured-resource
        captured-base)
    (cl-letf (((symbol-function 'appkit-media-cache-image-resource-async)
               (lambda (resource cache-base _success _error &rest _arguments)
                 (setq captured-resource resource
                       captured-base cache-base)
                 :image-transfer)))
      (disco-media--start-attachment-preview-fetch
       "preview-key" "https://media.example.invalid/image" "/tmp/cache-base")
      (should (equal '((url . "https://media.example.invalid/image"))
                     captured-resource))
      (should (equal "/tmp/cache-base" captured-base))
      (should (eq :image-transfer
                  (gethash "preview-key"
                           disco-media--attachment-preview-fetching))))))

(ert-deftest disco-media-attachment-download-uses-shared-transfer-runtime ()
  (let ((tmpdir (make-temp-file "disco-media-transfer" t))
        (disco-media--attachment-download-state-table
         (make-hash-table :test #'equal))
        (disco-media--attachment-download-owner-table
         (make-hash-table :test #'equal))
        captured-resource
        captured-target)
    (unwind-protect
        (let* ((disco-media-download-directory tmpdir)
               (attachment '((id . "42")
                             (filename . "clip.mp4")
                             (content_type . "video/mp4")
                             (url . "https://cdn.example.invalid/clip.mp4"))))
          (cl-letf (((symbol-function
                      'appkit-media-copy-or-download-resource-async)
                     (lambda (resource target _success _error)
                       (setq captured-resource resource
                             captured-target target)
                       :opaque-transfer))
                    ((symbol-function 'message) #'ignore))
            (disco-media-start-attachment-download attachment)
            (should
             (equal '((url . "https://cdn.example.invalid/clip.mp4")
                      (name . "clip.mp4")
                      (mime-type . "video/mp4"))
                    captured-resource))
            (should (equal (disco-media-attachment-download-path attachment)
                           captured-target))
            (let ((state (disco-media-attachment-download-state attachment)))
              (should (eq 'downloading (plist-get state :status)))
              (should (eq :opaque-transfer (plist-get state :transfer))))))
      (delete-directory tmpdir t))))

(ert-deftest disco-media-attachment-download-preserves-shared-error-text ()
  (let ((tmpdir (make-temp-file "disco-media-transfer-error" t))
        (disco-media--attachment-download-state-table
         (make-hash-table :test #'equal))
        (disco-media--attachment-download-owner-table
         (make-hash-table :test #'equal)))
    (unwind-protect
        (let* ((disco-media-download-directory tmpdir)
               (attachment '((id . "42")
                             (filename . "clip.mp4")
                             (url . "https://cdn.example.invalid/clip.mp4"))))
          (cl-letf (((symbol-function
                      'appkit-media-copy-or-download-resource-async)
                     (lambda (_resource _target _success error)
                       (funcall error "connection reset")
                       nil))
                    ((symbol-function 'message) #'ignore))
            (disco-media-start-attachment-download attachment)
            (let ((state (disco-media-attachment-download-state attachment)))
              (should (eq 'error (plist-get state :status)))
              (should (equal "connection reset" (plist-get state :error))))))
      (delete-directory tmpdir t))))

(ert-deftest disco-media-attachment-download-cancel-uses-transfer-handle ()
  (let* ((attachment '((id . "42") (filename . "clip.mp4")))
         (key (disco-media-attachment-download-key attachment))
         (disco-media--attachment-download-state-table
          (make-hash-table :test #'equal))
         canceled)
    (puthash key '(:status downloading :transfer opaque)
             disco-media--attachment-download-state-table)
    (cl-letf (((symbol-function 'appkit-media-cancel-transfer)
               (lambda (transfer) (setq canceled transfer) t))
              ((symbol-function 'message) #'ignore))
      (should (disco-media-cancel-attachment-download attachment))
      (should (eq 'opaque canceled))
      (should (plist-get
               (gethash key disco-media--attachment-download-state-table)
               :cancel-requested)))))

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
              ((symbol-function 'file-regular-p)
               (lambda (path)
                 (and downloaded (equal path "/tmp/voice.ogg"))))
              ((symbol-function 'disco-media-start-attachment-download)
               (lambda (_attachment _open-after on-success)
                 (setq downloaded t)
                 (funcall on-success "/tmp/voice.ogg")))
              ((symbol-function 'disco-media--start-appkit-audio-player)
               (lambda (_attachment source &rest _arguments)
                 (setq started-source source))))
      (disco-media-play-attachment-audio
       '((id . "a1")
         (filename . "voice.ogg")
         (url . "https://example.invalid/voice.ogg")))
      (should (equal "/tmp/voice.ogg" started-source)))))

(ert-deftest disco-media-video-playback-forwards-exact-app-owner ()
  (let ((owner (list 'exact-disco-app))
        (video-file (make-temp-file "disco-media-owner" nil ".mp4"))
        local-owner
        remote-owner
        remote-cache-key)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'disco-media-attachment-download-state)
                     (lambda (_attachment)
                       `(:status downloaded :path ,video-file)))
                    ((symbol-function 'appkit-media-play-video-file)
                     (lambda (_path _label &rest options)
                       (setq local-owner (plist-get options :owner)))))
            (disco-media-play-attachment-video
             '((filename . "local.mp4")) owner))
          (cl-letf (((symbol-function 'disco-media-attachment-download-state)
                     (lambda (_attachment)
                       '(:status not-downloaded :path nil)))
                    ((symbol-function 'appkit-media-play-video-url)
                     (lambda (_url _label &rest options)
                       (setq remote-owner (plist-get options :owner)
                             remote-cache-key
                             (plist-get options :cache-key)))))
            (disco-media-play-attachment-video
             '((id . "remote-id")
               (filename . "remote.mp4")
               (url . "https://example.invalid/remote.mp4"))
             owner))
          (should (eq owner local-owner))
          (should (eq owner remote-owner))
          (should
           (equal remote-cache-key "disco-attachment:remote-id")))
      (ignore-errors (delete-file video-file)))))

(ert-deftest disco-media-resource-adapter-forwards-video-owner ()
  (let ((owner (list 'exact-disco-app))
        forwarded-owner
        forwarded-kind)
    (cl-letf (((symbol-function 'appkit-media-open-resource)
               (lambda (_resource &rest options)
                 (setq forwarded-owner (plist-get options :owner)
                       forwarded-kind (plist-get options :kind)))))
      (disco-media-open-discord-resource
       '((url . "https://example.invalid/video.mp4"))
       'video nil :owner owner))
    (should (eq owner forwarded-owner))
    (should (eq 'video forwarded-kind))))

(ert-deftest disco-media-open-photo-attachment-uses-original-not-proxy-url ()
  "The CDN proxy remains preview-only when opening a Discord image."
  (let (opened-arguments)
    (cl-letf (((symbol-function 'disco-media-attachment-download-state)
               (lambda (_attachment)
                 '(:status not-downloaded :path "/missing/cat.png")))
              ((symbol-function 'disco-media-open-discord-resource)
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

(ert-deftest disco-media-message-one-line-preview-projects-safe-visual-prefix ()
  (let* ((image '(image :type png :data "bytes"))
         (message
          '((attachments
             . (((id . "a1")
                 (filename . "cat.png")
                 (content_type . "image/png")
                 (url . "https://example.invalid/cat.png")))))))
    (cl-letf (((symbol-function
                'disco-media-attachment-one-line-preview-image)
               (lambda (_attachment) image)))
      (let ((preview
             (disco-media-message-one-line-preview message "(attachment)")))
        (should (equal "(attachment)"
                       (appkit-ui-one-line-preview-text preview)))
        (should (= disco-media-one-line-preview-columns
                   (appkit-ui-one-line-preview-visual-columns preview)))
        (let ((display
               (get-text-property
                0 'display (appkit-ui-one-line-preview-visual preview))))
          (should (eq 'slice (caar display)))
          (should
           (equal "bytes"
                  (plist-get (cdr (cadr display)) :data))))))))

(ert-deftest disco-media-message-one-line-preview-never-reveals-spoiler-image ()
  (let ((message
         '((attachments
            . (((id . "a1")
                (filename . "SPOILER_cat.png")
                (content_type . "image/png")
                (url . "https://example.invalid/cat.png"))))))
        called)
    (cl-letf (((symbol-function
                'disco-media-attachment-one-line-preview-image)
               (lambda (_attachment)
                 (setq called t)
                 '(image :type png))))
      (let ((preview
             (disco-media-message-one-line-preview
              message "[spoiler image hidden]")))
        (should-not called)
        (should-not (appkit-ui-one-line-preview-visual preview))
        (should
         (equal "[spoiler image hidden]"
                (appkit-ui-one-line-preview-text preview)))))))

(ert-deftest disco-media-state-notifications-pass-explicit-resource ()
  (let (received)
    (let ((disco-media-rerender-hook
           (list (lambda (kind key)
                   (setq received (cons kind key))))))
      (disco-media--notify-state-updated 'preview "preview-key")
      (should (equal '(preview . "preview-key") received)))))

(ert-deftest disco-media-reset-session-state-stops-appkit-audio-and-clears-memory ()
  (let* ((secret "OLD_ACCOUNT_SECRET")
         (key "old-audio")
         (session
          (appkit-media-player-session--create :status 'playing))
         (disco-media--generation 7)
         (disco-media--attachment-preview-image-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-preview-fetching
          (make-hash-table :test #'equal))
         (disco-media--attachment-preview-owner-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-download-state-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-download-owner-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-audio-state-table
          (make-hash-table :test #'equal))
         (disco-media--attachment-waveform-image-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-placeholder-image-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-decorated-preview-cache
          (make-hash-table :test #'equal))
         (disco-media--attachment-preview-fetch-budget 3)
         (disco-media--attachment-audio-current-key key)
         stopped)
    (puthash key (list :session session :private secret)
             disco-media--attachment-audio-state-table)
    (puthash key secret disco-media--attachment-preview-image-cache)
    (puthash key '(:status downloaded)
             disco-media--attachment-download-state-table)
    (cl-letf (((symbol-function 'appkit-media-player-stop)
               (lambda (current) (setq stopped current)))
              ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore)
              ((symbol-function 'appkit-media-clear-video-decoration-cache)
               #'ignore))
      (disco-media-reset-session-state))
    (should (eq session stopped))
    (should (= 8 disco-media--generation))
    (dolist (table
             (list disco-media--attachment-preview-image-cache
                   disco-media--attachment-preview-fetching
                   disco-media--attachment-preview-owner-table
                   disco-media--attachment-download-state-table
                   disco-media--attachment-download-owner-table
                   disco-media--attachment-audio-state-table
                   disco-media--attachment-waveform-image-cache
                   disco-media--attachment-placeholder-image-cache
                   disco-media--attachment-decorated-preview-cache))
      (should (= 0 (hash-table-count table))))
    (should-not disco-media--attachment-preview-fetch-budget)
    (should-not disco-media--attachment-audio-current-key)
    (should-not
     (string-match-p
      secret
      (prin1-to-string
       (list disco-media--attachment-audio-state-table
             disco-media--attachment-preview-image-cache))))))

(ert-deftest disco-media-reset-before-constructor-return-cancels-returned-handles ()
  (let ((disco-media--attachment-preview-image-cache (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetching (make-hash-table :test #'equal))
        (disco-media--attachment-preview-owner-table (make-hash-table :test #'equal))
        (disco-media--attachment-download-state-table (make-hash-table :test #'equal))
        (disco-media--attachment-download-owner-table (make-hash-table :test #'equal))
        canceled)
    (cl-letf (((symbol-function 'appkit-media-cancel-transfer)
               (lambda (transfer) (push transfer canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore)
              ((symbol-function 'appkit-media-clear-video-decoration-cache) #'ignore)
              ((symbol-function 'appkit-media-cache-image-resource-async)
               (lambda (&rest _arguments)
                 (disco-media-reset-session-state)
                 :returned-preview))
              ((symbol-function 'appkit-media-copy-or-download-resource-async)
               (lambda (&rest _arguments)
                 (disco-media-reset-session-state)
                 :returned-download))
              ((symbol-function 'message) #'ignore))
      (disco-media--start-attachment-preview-fetch
       "old-preview" "https://old.invalid/image" "/tmp/old-preview")
      (disco-media-start-attachment-download
       '((id . "old-download") (filename . "old.bin")
         (url . "https://old.invalid/file")))
      (should (memq :returned-preview canceled))
      (should (memq :returned-download canceled))
      (should (= 0 (hash-table-count disco-media--attachment-preview-fetching)))
      (should (= 0 (hash-table-count disco-media--attachment-download-state-table))))))

(ert-deftest disco-media-late-callbacks-after-reset-do-not-refill-or-rerender ()
  (let ((disco-media--attachment-preview-image-cache (make-hash-table :test #'equal))
        (disco-media--attachment-preview-fetching (make-hash-table :test #'equal))
        (disco-media--attachment-preview-owner-table (make-hash-table :test #'equal))
        (disco-media--attachment-download-state-table (make-hash-table :test #'equal))
        (disco-media--attachment-download-owner-table (make-hash-table :test #'equal))
        preview-success download-success (rerenders 0) opened succeeded)
    (let ((disco-media-rerender-hook
           (list (lambda (&rest _arguments) (cl-incf rerenders)))))
      (cl-letf (((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (_resource _base success _error &rest _arguments)
                   (setq preview-success success)
                   :preview))
                ((symbol-function 'appkit-media-copy-or-download-resource-async)
                 (lambda (_resource _target success _error)
                   (setq download-success success)
                   :download))
                ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
                ((symbol-function 'appkit-media-cancel-video-preview) #'ignore)
                ((symbol-function 'appkit-media-clear-video-decoration-cache) #'ignore)
                ((symbol-function 'appkit-media-open-file)
                 (lambda (_path) (setq opened t)))
                ((symbol-function 'message) #'ignore))
        (disco-media--start-attachment-preview-fetch
         "old" "https://old.invalid/image" "/tmp/old")
        (disco-media-start-attachment-download
         '((id . "old") (filename . "old.bin")
           (url . "https://old.invalid/file"))
         t (lambda (_path) (setq succeeded t)))
        (setq rerenders 0)
        (disco-media-reset-session-state)
        (funcall preview-success "/tmp/nonexistent-old-image")
        (funcall download-success "/tmp/nonexistent-old-download")
        (should (= 0 rerenders))
        (should-not opened)
        (should-not succeeded)
        (should (= 0 (hash-table-count disco-media--attachment-preview-image-cache)))
        (should (= 0 (hash-table-count disco-media--attachment-download-state-table)))))))

(ert-deftest disco-media-audio-delegates-session-lifecycle-to-appkit ()
  (let* ((file (make-temp-file "disco-audio-" nil ".ogg"))
         (attachment
          '((id . "voice")
            (filename . "voice.ogg")
            (duration_secs . 61.0)))
         (owner (list 'exact-app-owner))
         (disco-media--attachment-audio-state-table
          (make-hash-table :test #'equal))
         start-arguments
         toggled
         session)
    (unwind-protect
        (cl-letf
            (((symbol-function 'appkit-media-player-available-p)
              (lambda (&rest _arguments) t))
             ((symbol-function 'appkit-media-player-start-file)
              (lambda (path &rest arguments)
                (should (equal file path))
                (setq start-arguments arguments
                      session
                      (appkit-media-player-session--create
                       :status 'playing
                       :played-seconds 3.5
                       :progress-observed-p t))
                session))
             ((symbol-function 'appkit-media-player-toggle)
              (lambda (current) (setq toggled current) current))
             ((symbol-function 'disco-media-attachment-download-state)
              (lambda (_attachment)
                `(:status downloaded :path ,file))))
          (let ((result
                 (disco-media--start-appkit-audio-player
                  attachment file :owner owner)))
            (should (eq session result)))
          (should (eq owner (plist-get start-arguments :owner)))
          (should (= 61.0
                     (plist-get start-arguments :duration-seconds)))
          (should (functionp (plist-get start-arguments :on-change)))
          (should (eq 'playing
                      (plist-get
                       (disco-media-attachment-audio-state attachment)
                       :status)))
          (should (= 3.5
                     (disco-media-attachment-audio-progress attachment)))
          (disco-media-play-attachment-audio attachment owner)
          (should (eq session toggled)))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest disco-media-save-as-transfers-are-reset-owned-and-late-callbacks-stale ()
  (let ((disco-media--attachment-export-owners nil)
        (disco-media--attachment-download-state-table (make-hash-table :test #'equal))
        callbacks
        canceled
        (messages 0))
    (cl-letf (((symbol-function 'appkit-media-copy-or-download-resource-async)
               (lambda (_resource target success error)
                 (push (list success error) callbacks)
                 (intern (concat ":transfer-" target))))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (transfer) (push transfer canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore)
              ((symbol-function 'appkit-media-clear-video-decoration-cache) #'ignore)
              ((symbol-function 'message)
               (lambda (&rest _arguments) (cl-incf messages))))
      (disco-media-download-attachment
       '((id . "save-1") (filename . "one.bin")
         (url . "https://old.invalid/OLD_ACCOUNT_SECRET-one"))
       "/tmp/save-one")
      (disco-media-download-attachment
       '((id . "save-2") (filename . "two.bin")
         (url . "https://old.invalid/OLD_ACCOUNT_SECRET-two"))
       "/tmp/save-two")
      (should (= 2 (length disco-media--attachment-export-owners)))
      (disco-media-reset-session-state)
      (should-not disco-media--attachment-export-owners)
      (should (= 2 (length canceled)))
      (setq messages 0)
      (dolist (pair callbacks)
        (funcall (car pair) "/tmp/late-success")
        (funcall (cadr pair) "late error"))
      (should (= 0 messages)))))

(ert-deftest disco-media-save-as-reset-before-return-cancels-returned-transfer ()
  (let ((disco-media--attachment-export-owners nil)
        (disco-media--attachment-download-state-table (make-hash-table :test #'equal))
        canceled)
    (cl-letf (((symbol-function 'appkit-media-copy-or-download-resource-async)
               (lambda (&rest _arguments)
                 (disco-media-reset-session-state)
                 :returned-save-as))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (transfer) (push transfer canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore)
              ((symbol-function 'appkit-media-clear-video-decoration-cache) #'ignore))
      (should-not
       (disco-media-download-attachment
        '((id . "save") (filename . "old.bin")
          (url . "https://old.invalid/OLD_ACCOUNT_SECRET"))
        "/tmp/save-old"))
      (should (equal '(:returned-save-as) canceled))
      (should-not disco-media--attachment-export-owners))))

(provide 'disco-media-test)

;;; disco-media-test.el ends here
