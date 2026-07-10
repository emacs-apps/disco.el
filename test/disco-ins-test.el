;;; disco-ins-test.el --- Tests for disco-ins helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-ins)
(require 'disco-ui)

(ert-deftest disco-ins-prefix-string-handles-strings-and-prefix-state ()
  (let ((state (disco-ui-make-prefix-state "a> " "b> ")))
    (should (equal "raw> "
                   (disco-ins-prefix-string "raw> " nil "fallback> ")))
    (should (equal "a> "
                   (disco-ins-prefix-string state t "fallback> ")))
    (should (equal "b> "
                   (disco-ins-prefix-string state nil "fallback> ")))))

(ert-deftest disco-ins-insert-full-width-divider-applies-face-and-properties ()
  (with-temp-buffer
    (let ((span (disco-ins-insert-full-width-divider "Label" 'shadow 24 '(demo t))))
      (should (string-match-p "( Label )" (buffer-string)))
      (should (eq t (get-text-property (car span) 'demo)))
      (let ((face (get-text-property (car span) 'face)))
        (should (or (eq face 'shadow)
                    (and (listp face) (memq 'shadow face))))))))

(ert-deftest disco-ins-insert-divider-row-is-read-only ()
  (with-temp-buffer
    (let ((span (disco-ins-insert-divider-row "Unread" 'shadow 20 '(section unread))))
      (should (string-match-p "Unread" (buffer-string)))
      (should (eq t (get-text-property (car span) 'read-only)))
      (should (equal 'unread (get-text-property (car span) 'section))))))

(ert-deftest disco-ins-insert-reference-line-supports-body-and-button ()
  (with-temp-buffer
    (let (clicked)
      (disco-ins-insert-reference-line
       "preview"
       :prefix "    "
       :face 'shadow
       :button-label "[Jump]"
       :button-action (lambda ()
                        (setq clicked t)))
      (goto-char (point-min))
      (should (string-match-p (regexp-quote "↪ preview [Jump]")
                              (buffer-string)))
      (should (eq 'shadow (get-text-property (point) 'face)))
      (search-forward "[Jump]")
      (button-activate (button-at (match-beginning 0)))
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

(ert-deftest disco-ins-insert-reaction-line-supports-adapter-label-and-action ()
  (with-temp-buffer
    (let ((reaction '((code . "178") (count . 3) (chosen-p . t)))
          clicked)
      (disco-ins-insert-reaction-line
       (list reaction)
       :selected-face 'success
       :unselected-face 'shadow
       :label-function
       (lambda (item)
         (format " face-%s %d "
                 (alist-get 'code item) (alist-get 'count item)))
       :selected-p-function (lambda (item) (alist-get 'chosen-p item))
       :action-function (lambda (item) (setq clicked item))
       :help-echo-function (lambda (_item) "Toggle reaction"))
      (goto-char (point-min))
      (search-forward "face-178 3")
      (let ((button (button-at (match-beginning 0))))
        (should button)
        (should (equal (button-get button 'help-echo) "Toggle reaction"))
        (button-activate button)
        (should (equal clicked reaction))))))

(ert-deftest disco-ins-insert-right-aligned-text-uses-target-width ()
  (with-temp-buffer
    (insert "Alice")
    (let ((span (disco-ins-insert-right-aligned-text
                 "12:34" 30 :face 'shadow)))
      (should (equal (buffer-substring-no-properties
                      (car span) (cdr span))
                     " 12:34"))
      (should (equal (get-text-property (car span) 'display)
                     '(space :align-to 25)))
      (should (eq (get-text-property (1- (point)) 'face) 'shadow)))))

(ert-deftest disco-ins-insert-right-aligned-text-reserves-future-prefix ()
  (with-temp-buffer
    (insert (make-string 20 ?x))
    (let ((span (disco-ins-insert-right-aligned-text
                 "12:34" 30 :left-prefix-width 4)))
      (should (= (car span) (line-beginning-position)))
      (should (= (line-number-at-pos) 2))
      (should (equal (get-text-property (car span) 'display)
                     '(space :align-to 25))))))

(ert-deftest disco-ins-insert-forward-card-renders-metadata-and-action ()
  (with-temp-buffer
    (let (clicked)
      (disco-ins-insert-forward-card
       :source-text "Guild / channel"
       :sent-at "2026-03-09 10:00"
       :content "hello forward"
       :insert-source-icon (lambda () (insert "[icon]"))
       :jump-label "[Jump to source]"
       :jump-action (lambda () (setq clicked t))
       :jump-face 'link
       :border-face 'shadow
       :title-face 'bold
       :meta-face 'italic)
      (should (string-match-p (regexp-quote "[forwarded message]") (buffer-string)))
      (should (string-match-p (regexp-quote "source: [icon] Guild / channel")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "sent: 2026-03-09 10:00")
                              (buffer-string)))
      (should (string-match-p (regexp-quote "hello forward") (buffer-string)))
      (goto-char (point-min))
      (search-forward "[Jump to source]")
      (button-activate (button-at (match-beginning 0)))
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
                ((symbol-function 'disco-media-image-object-valid-p)
                 (lambda (image)
                   (eq image :spoiler-preview)))
                ((symbol-function 'disco-media-insert-image-slices)
                 (lambda (_image &optional _url _prefix _fallback)
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
          (should (disco-media-card-context-at-point)))
        (should-not (string-match-p (regexp-quote "[Download]")
                                    (buffer-string)))
        (should-not (string-match-p (regexp-quote "[Save As]")
                                    (buffer-string)))
        (disco-media-card-call-action 'download)
        (disco-media-card-call-action 'save-as)
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
              ((symbol-function 'disco-media-insert-image-slices)
               (lambda (_image &optional _url _prefix _fallback)
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
        played
        decorated)
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'disco-media-attachment-download-state)
                     (lambda (_attachment)
                       `(:status downloaded :path ,video-file)))
                    ((symbol-function 'disco-media-attachment-preview-image)
                     (lambda (_attachment)
                       :fake-preview))
                    ((symbol-function 'disco-media-image-object-valid-p)
                     (lambda (image)
                       (memq image '(:fake-preview :decorated-preview))))
                    ((symbol-function 'disco-media-attachment-video-display-image)
                     (lambda (image)
                       (setq decorated image)
                       :decorated-preview))
                    ((symbol-function 'disco-media-insert-image-slices)
                     (lambda (_image &optional _url _prefix _fallback)
                       (insert "[video-preview]")))
                    ((symbol-function 'disco-media-play-attachment-video)
                     (lambda (_attachment)
                       (setq played t))))
            (disco-ins-insert-attachment-video
             '((filename . "clip.mp4")
               (size . 8192)
               (width . 1280)
               (height . 720)
               (url . "https://example.invalid/clip.mp4"))
             :prefix "    "
             :title-face 'bold
             :meta-face 'shadow
             :action-face 'link)
            (should (eq :fake-preview decorated))
            (should (string-match-p (regexp-quote "[video] clip.mp4") (buffer-string)))
            (should (string-match-p (regexp-quote "[video-preview]") (buffer-string)))
            (should-not (string-match-p (regexp-quote "[Play]") (buffer-string)))
            (goto-char (point-min))
            (disco-media-card-call-action 'open)
            (should played)
            (goto-char (point-min))
            (search-forward "[video-preview]")
            (let ((pos (match-beginning 0)))
              (should (string-match-p "Play video"
                                      (or (get-text-property pos 'help-echo) ""))))))
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
         :prefix "    "
         :action-face 'link
         :kind 'audio)
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
              ((symbol-function 'disco-media-image-object-valid-p)
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
