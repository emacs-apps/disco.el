;;; disco-ins-test.el --- Tests for disco-ins helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media)

(require 'disco-ins)
(require 'appkit-ui)


(ert-deftest disco-ins-insert-reference-line-makes-preview-navigable ()
  (with-temp-buffer
    (let (clicked)
      (disco-ins-insert-reference-line
       "preview"
       :prefix "    "
       :face 'shadow
       :action (lambda () (setq clicked t))
       :help-echo "Open reply")
      (goto-char (point-min))
      (should (equal "↪ preview\n" (buffer-string)))
      (should (eq 'shadow (get-text-property (point) 'face)))
      (should (equal "Open reply" (get-text-property (point) 'help-echo)))
      (call-interactively
       (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
      (should clicked))))

(ert-deftest disco-ins-insert-reaction-line-renders-selected-and-unselected-chips ()
  (with-temp-buffer
    (let* ((selected '((emoji . ((name . ":wave:")))
                       (count . 2)
                       (me . true)))
           (plain '((emoji . ((name . ":sparkles:")))
                    (total_count . 1)
                    (is_chosen . :false)))
           (span (disco-ins-insert-reaction-line
                  (list selected plain)
                  :prefix "    "
                  :selected-face 'success
                  :unselected-face 'shadow
                  :line-face 'default)))
      (should span)
      (should (string-match-p "\[:wave: 2\] \[:sparkles: 1\]" (buffer-string)))
      (goto-char (point-min))
      (search-forward "[:wave: 2]")
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (eq face 'success)
                    (and (listp face) (memq 'success face)))))
      (search-forward "[:sparkles: 1]")
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (eq face 'shadow)
                    (and (listp face) (memq 'shadow face))))))))

(ert-deftest disco-ins-insert-reaction-line-supports-action-and-help ()
  (with-temp-buffer
    (let ((reaction '((emoji . ((name . ":wave:")))
                      (count . 3)
                      (me . true)))
          clicked)
      (disco-ins-insert-reaction-line
       (list reaction)
       :selected-face 'success
       :unselected-face 'shadow
       :action-function (lambda (item) (setq clicked item))
       :help-echo-function (lambda (_item) "Toggle reaction"))
      (goto-char (point-min))
      (search-forward "[:wave: 3]")
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (should (equal (button-get button 'help-echo) "Toggle reaction"))
        (button-activate button)
        (should (equal clicked reaction))))))

(ert-deftest disco-ins-insert-forward-card-renders-navigable-metadata ()
  (with-temp-buffer
    (let (clicked)
      (disco-ins-insert-forward-card
       :source-text "Guild / channel"
       :sent-at "2026-03-09 10:00"
       :content "hello forward"
       :insert-source-icon (lambda () (insert "[icon]"))
       :open-action (lambda () (setq clicked t))
       :open-help-echo "Open source"
       :border-face 'shadow
       :title-face 'bold
       :meta-face 'italic)
      (should (string-match-p (regexp-quote "Forwarded message") (buffer-string)))
      (should (string-match-p (regexp-quote "source: [icon] Guild / channel")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "sent: 2026-03-09 10:00")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "hello forward") (buffer-string)))
      (should-not (string-match-p (regexp-quote "[Jump") (buffer-string)))
      (goto-char (point-min))
      (should (equal "Open source" (get-text-property (point) 'help-echo)))
      (call-interactively
       (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
      (should clicked))))

(ert-deftest disco-ins-insert-attachment-lines-renders-summary-and-url ()
  (with-temp-buffer
    (let ((span (disco-ins-insert-attachment-lines
                 "[file] doc.txt"
                 :prefix "    "
                 :url "https://example.invalid/doc.txt"
                 :summary-face 'bold
                 :url-face 'shadow)))
      (should span)
      (should (string-match-p (regexp-quote "[file] doc.txt") (buffer-string)))
      (should (string-match-p (regexp-quote "https://example.invalid/doc.txt")
                              (buffer-string)))
      (goto-char (point-min))
      (should (eq 'bold (get-text-property (point) 'face)))
      (search-forward "https://example.invalid/doc.txt")
      (should (eq 'shadow (get-text-property (match-beginning 0) 'face))))))


(ert-deftest disco-ins-insert-attachment-spoiler-placeholder-renders-reveal-button ()
  (with-temp-buffer
    (let (revealed)
      (disco-ins-insert-attachment-spoiler-placeholder
       '((filename . "SPOILER_cat.png"))
       :prefix "    "
       :line-face 'shadow
       :button-face 'link
       :toggle-action (lambda ()
                        (setq revealed t)))
      (should (string-match-p (regexp-quote "[spoiler image hidden]")
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "[Reveal spoiler]")
      (button-activate (button-at (match-beginning 0)))
      (should revealed)
      (goto-char (point-min))
      (should (stringp (get-text-property (point) 'help-echo))))))

(ert-deftest disco-ins-insert-attachment-spoiler-placeholder-prefers-obscured-preview ()
  (with-temp-buffer
    (let (revealed)
      (cl-letf (((symbol-function 'disco-media-attachment-spoiler-preview-image)
                 (lambda (_attachment)
                   :spoiler-preview))
                ((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (image)
                   (eq image :spoiler-preview)))
                ((symbol-function 'appkit-media-insert-image-slices)
                 (lambda (_image &rest _args)
                   (insert "[spoiler-preview]"))))
        (disco-ins-insert-attachment-spoiler-placeholder
         '((filename . "SPOILER_cat.png"))
         :prefix "    "
         :line-face 'shadow
         :button-face 'link
         :toggle-action (lambda ()
                          (setq revealed t)))
        (should (string-match-p (regexp-quote "[spoiler-preview]")
                                (buffer-string)))
        (goto-char (point-min))
        (should (string-match-p "Reveal spoiler"
                                (or (get-text-property (point) 'help-echo) "")))
        (search-forward "[Reveal spoiler]")
        (button-activate (button-at (match-beginning 0)))
        (should revealed)))))

(ert-deftest disco-ins-insert-attachment-document-uses-compact-card-context ()
  (with-temp-buffer
    (let (downloaded saved)
      (cl-letf (((symbol-function 'disco-media-attachment-download-state)
                 (lambda (_attachment)
                   '(:status not-downloaded :path "/tmp/doc.txt")))
                ((symbol-function 'disco-media-start-attachment-download)
                 (lambda (_attachment &optional _open-after)
                   (setq downloaded t)))
                ((symbol-function 'disco-media-download-attachment)
                 (lambda (_attachment &optional _target-path)
                   (setq saved t))))
        (disco-ins-insert-attachment-document
         '((filename . "doc.txt")
           (size . 128)
           (url . "https://example.invalid/doc.txt"))
         :prefix "    "
         :title-face 'bold
         :meta-face 'shadow
         :action-face 'link)
        (should (string-match-p (regexp-quote "[file] doc.txt") (buffer-string)))
        (goto-char (point-min))
        (search-forward "doc.txt")
        (let ((pos (- (point) (length "doc.txt"))))
          (should (equal "Open attachment"
                         (get-text-property pos 'help-echo)))
          (goto-char pos)
          (should (appkit-media-card-context-at-point)))
        (should-not (string-match-p (regexp-quote "[Download]")
                                    (buffer-string)))
        (should-not (string-match-p (regexp-quote "[Save As]")
                                    (buffer-string)))
        (appkit-media-card-call-action 'download)
        (appkit-media-card-call-action 'save-as)
        (should downloaded)
        (should saved)))))

(ert-deftest disco-ins-insert-attachment-photo-renders-meta-preview-caption-and-url ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'disco-media-attachment-download-state)
               (lambda (_attachment)
                 '(:status not-downloaded :path "/tmp/cat.png")))
              ((symbol-function 'disco-media-attachment-preview-image)
               (lambda (_attachment)
                 :fake-preview))
              ((symbol-function 'appkit-media-insert-image-slices)
               (lambda (_image &rest _args)
                 (insert "[image-preview]"))))
      (disco-ins-insert-attachment-photo
       '((filename . "cat.png")
         (size . 4096)
         (width . 640)
         (height . 480)
         (description . "nice cat")
         (url . "https://example.invalid/cat.png")
         (proxy_url . "https://proxy.example.invalid/cat.png"))
       :prefix "    "
       :title-face 'bold
       :meta-face 'shadow
       :action-face 'link
       :show-url t)
      (should (string-match-p (regexp-quote "[image] cat.png") (buffer-string)))
      (should (string-match-p (regexp-quote "640x480") (buffer-string)))
      (should (string-match-p (regexp-quote "[image-preview]") (buffer-string)))
      (should (string-match-p (regexp-quote "caption: nice cat") (buffer-string)))
      (should (string-match-p (regexp-quote "https://example.invalid/cat.png")
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "https://example.invalid/cat.png")
      (let ((pos (match-beginning 0)))
        (should (stringp (get-text-property pos 'help-echo)))))))

(ert-deftest disco-ins-insert-attachment-video-uses-card-as-play-action ()
  (let ((video-file (make-temp-file "disco-ins-video" nil ".mp4"))
        (owner (list 'exact-disco-app))
        played
        played-owner
        decorated
        preview-action
        preview-help)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'disco-media-attachment-download-state)
                     (lambda (_attachment)
                       `(:status downloaded :path ,video-file)))
                    ((symbol-function 'disco-media-attachment-preview-image)
                     (lambda (_attachment)
                       :fake-preview))
                    ((symbol-function 'appkit-media-image-object-valid-p)
                     (lambda (image)
                       (memq image '(:fake-preview :decorated-preview))))
                    ((symbol-function 'appkit-media-video-preview-display-image)
                     (lambda (image &optional _namespace)
                       (setq decorated image)
                       :decorated-preview))
                    ((symbol-function 'appkit-media-insert-image-slices)
                     (lambda (_image &optional action _prefix _fallback help)
                       (setq preview-action action
                             preview-help help)
                       (insert "[video-preview]")))
                    ((symbol-function 'disco-media-play-attachment-video)
                     (lambda (_attachment &optional received-owner)
                       (setq played t
                             played-owner received-owner))))
            (disco-ins-insert-attachment-video
             '((filename . "clip.mp4")
               (size . 8192)
               (width . 1280)
               (height . 720)
               (url . "https://example.invalid/clip.mp4"))
             :prefix "    "
             :title-face 'bold
             :meta-face 'shadow
             :action-face 'link
             :owner owner)
            (should (eq :fake-preview decorated))
            (should (string-match-p (regexp-quote "[video] clip.mp4") (buffer-string)))
            (should (string-match-p (regexp-quote "[video-preview]") (buffer-string)))
            (should-not (string-match-p (regexp-quote "[Play]") (buffer-string)))
            (goto-char (point-min))
            (appkit-media-card-call-action 'open)
            (should played)
            (should (eq owner played-owner))
            (should (functionp preview-action))
            (should (string-match-p "Play video" preview-help))
            (setq played nil
                  played-owner nil)
            (funcall preview-action)
            (should played)
            (should (eq owner played-owner))))
      (ignore-errors (delete-file video-file)))))

(ert-deftest disco-ins-insert-attachment-transfer-line-renders-status-only ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'disco-media-attachment-download-state)
               (lambda (_attachment)
                 '(:status downloading :path "/tmp/track.mp3"))))
        (disco-ins-insert-attachment-transfer-line
         '((content_type . "audio/mpeg")
           (filename . "track.mp3")
           (url . "https://example.invalid/track.mp3"))
         :prefix "    ")
      (should (string-match-p "downloading" (buffer-string)))
      (should-not (string-match-p (regexp-quote "[Play]") (buffer-string)))
      (should-not (string-match-p (regexp-quote "[Cancel]") (buffer-string))))))

(ert-deftest disco-ins-insert-attachment-audio-renders-inline-controls-and-waveform ()
  (with-temp-buffer
    (let (played stopped)
      (cl-letf (((symbol-function 'disco-media-audio-inline-playback-available-p)
                 (lambda () t))
                ((symbol-function 'disco-media-attachment-audio-playing-p)
                 (lambda (_attachment) nil))
                ((symbol-function 'disco-media-attachment-audio-paused-p)
                 (lambda (_attachment) 12.0))
                ((symbol-function 'disco-media-attachment-audio-progress)
                 (lambda (_attachment) 12.0))
                ((symbol-function 'disco-media-attachment-download-state)
                 (lambda (_attachment)
                   '(:status not-downloaded :path "/tmp/voice.ogg")))
                ((symbol-function 'disco-media-attachment-waveform-image)
                 (lambda (&rest _args) nil))
                ((symbol-function 'disco-media-attachment-waveform-string)
                 (lambda (&rest _args)
                   "[waveform]"))
                ((symbol-function 'disco-media-play-attachment-audio)
                 (lambda (_attachment)
                   (setq played t)))
                ((symbol-function 'disco-media-stop-attachment-audio)
                 (lambda (_attachment)
                   (setq stopped t))))
        (disco-ins-insert-attachment-audio
         '((content_type . "audio/ogg")
           (filename . "voice.ogg")
           (duration_secs . 61.0)
           (waveform . "AAAA"))
         :prefix "    "
         :title-face 'bold
         :meta-face 'shadow
         :action-face 'link)
        (should (string-match-p (regexp-quote "[voice] voice.ogg")
                                (buffer-string)))
        (should (string-match-p (regexp-quote "[Resume] [waveform]  0:12 / 1:01 [Stop]")
                                (buffer-string)))
        (should-not (string-match-p (regexp-quote "transfer: [Play]")
                                    (buffer-string)))
        (goto-char (point-min))
        (search-forward "[Resume]")
        (button-activate (button-at (match-beginning 0)))
        (goto-char (point-min))
        (search-forward "[Stop]")
        (button-activate (button-at (match-beginning 0)))
        (should played)
        (should stopped)))))

(ert-deftest disco-ins-insert-attachment-audio-prefers-waveform-image-when-available ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'disco-media-audio-inline-playback-available-p)
               (lambda () nil))
              ((symbol-function 'disco-media-attachment-waveform-image)
               (lambda (&rest _args) :waveform-image))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (image)
                 (eq image :waveform-image)))
              ((symbol-function 'insert-image)
               (lambda (_image &optional _string _slice)
                 (insert "[waveform-image]")))
              ((symbol-function 'disco-media-play-attachment-audio)
               (lambda (&rest _args) nil)))
      (disco-ins-insert-attachment-audio
       '((content_type . "audio/ogg")
         (filename . "voice.ogg")
         (duration_secs . 61.0)
         (waveform . "AAAA")
         (url . "https://example.invalid/voice.ogg"))
       :prefix "    "
       :title-face 'bold
       :meta-face 'shadow
       :action-face 'link)
      (should (string-match-p (regexp-quote "[Play] [waveform-image]  1:01")
                              (buffer-string))))))

(provide 'disco-ins-test)

;;; disco-ins-test.el ends here
