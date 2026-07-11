;;; disco-ins.el --- Discord insertion adapter for Appkit chat cards -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Adapt Discord reactions, references, attachments, spoilers, and forwards to
;; Appkit's protocol-neutral insertion and media-card primitives.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-ins)
(require 'appkit-media)
(require 'disco-media)
(require 'disco-msg)
(require 'appkit-ui)


(cl-defun disco-ins-insert-reference-line
    (body &key prefix face action help-echo properties)
  "Insert one prefixed reference line with optional whole-line ACTION.

PREFIX is applied with `appkit-ui-apply-line-prefix'.  FACE and PROPERTIES are
applied to the inserted span.  ACTION makes the reference itself navigable,
without adding a synthetic button label.  Return the inserted span as
`(START . END)'."
  (let ((line-start (point))
        (text (and (stringp body) (not (string-empty-p body)) body)))
    (insert "↪")
    (when text
      (insert " "))
    (when text
      (insert text))
    (insert "\n")
    (appkit-ui-apply-line-prefix line-start (point) (or prefix "    "))
    (when (or face properties)
      (add-text-properties
       line-start (point)
       (append properties
               (when face (list 'face face)))))
    (when (functionp action)
      (appkit-media-add-action-properties
       line-start (max line-start (1- (point))) action
       (or help-echo "Open referenced message")))
    (cons line-start (point))))

(cl-defun disco-ins-insert-reaction-line
    (reactions &key prefix selected-face unselected-face line-face
               action-function help-echo-function)
  "Insert one reaction chip line for REACTIONS.

PREFIX is applied with `appkit-ui-apply-line-prefix'.  SELECTED-FACE and
UNSELECTED-FACE style each reaction chip.  LINE-FACE applies to the whole
inserted span.

ACTION-FUNCTION makes chips clickable and is called with the selected
reaction.  HELP-ECHO-FUNCTION supplies hover text.  Discord reaction labels
and current-account selection are adapted here.  Return the inserted span as
(START . END), or nil when REACTIONS is empty."
  (appkit-chat-ins-insert-reaction-line
   reactions
   :prefix prefix
   :selected-face selected-face
   :unselected-face unselected-face
   :line-face line-face
   :label-function
   (lambda (reaction)
     (format "[%s %s]"
             (disco-msg-reaction-emoji reaction)
             (disco-msg-reaction-count reaction)))
   :selected-p-function #'disco-msg-reaction-selected-p
   :action-function action-function
   :help-echo-function help-echo-function))

(cl-defun disco-ins-insert-attachment-lines (summary &key prefix url
                                                     summary-face url-face)
  "Insert plain attachment SUMMARY and optional URL lines.

PREFIX is applied with `appkit-ui-apply-line-prefix'.  SUMMARY-FACE styles the
summary line, and URL-FACE styles the optional URL line.  Return the inserted
span as (START . END), or nil when SUMMARY is empty."
  (when (and (stringp summary) (not (string-empty-p summary)))
    (let ((start (point)))
      (let ((summary-start (point)))
        (insert summary "\n")
        (appkit-ui-apply-line-prefix summary-start (point) (or prefix "    "))
        (when summary-face
          (add-text-properties summary-start (point) (list 'face summary-face))))
      (when (and (stringp url) (not (string-empty-p url)))
        (let ((url-start (point)))
          (insert "  " url "\n")
          (appkit-ui-apply-line-prefix url-start (point) (or prefix "    "))
          (when url-face
            (add-text-properties url-start (point) (list 'face url-face)))))
      (cons start (point)))))


(cl-defun disco-ins-insert-attachment-caption-line (caption &key prefix face
                                                            (label "caption: "))
  "Insert attachment CAPTION line when CAPTION is non-empty."
  (when (and (stringp caption) (not (string-empty-p caption)))
    (appkit-chat-ins-insert-prefixed-line
     (concat label caption)
     :prefix (or prefix "    ")
     :face face)))

(cl-defun disco-ins-insert-attachment-url-line
    (url &key prefix face action help-echo)
  "Insert attachment URL and bind ACTION to it when non-empty."
  (when (appkit-media-url-present-p url)
    (let ((start (point)))
      (insert url "\n")
      (when (functionp action)
        (appkit-media-add-action-properties
         start (max start (1- (point))) action
         (or help-echo "Open attachment")))
      (appkit-ui-apply-line-prefix start (point) (or prefix "    "))
      (when face
        (appkit-ui-append-face start (point) face))
      (cons start (point)))))

(cl-defun disco-ins-insert-attachment-spoiler-placeholder
    (attachment &key prefix border-face line-face button-face
                toggle-action toggle-help-echo reveal-label)
  "Insert hidden spoiler block for ATTACHMENT.

When a cached media preview exists, show an obscured preview tile similar to
telega; otherwise fall back to a text placeholder line."
  (let* ((prefix-state (appkit-chat-ins-media-prefix-state prefix border-face))
         (label (disco-media-attachment-spoiler-label attachment))
         (help (or toggle-help-echo "Reveal spoiler"))
         (preview (disco-media-attachment-spoiler-preview-image attachment)))
    (if (appkit-media-image-object-valid-p preview)
        (let ((preview-start (point)))
          (appkit-media-insert-image-slices preview nil nil label)
          (when (functionp toggle-action)
            (appkit-media-add-action-properties
             preview-start (point)
             (lambda (&optional _event)
               (interactive)
               (funcall toggle-action))
             help))
          (insert "\n")
          (appkit-ui-apply-line-prefix preview-start (point) prefix-state)
          (let ((action-start (point)))
            (if (functionp toggle-action)
                (appkit-ui-insert-action-button
                 (or reveal-label "[Reveal spoiler]")
                 toggle-action
                 :face button-face
                 :help-echo help)
              (insert (or reveal-label "[Reveal spoiler]")))
            (insert "\n")
            (appkit-ui-apply-line-prefix action-start (point) prefix-state)
            (when line-face
              (appkit-ui-append-face action-start (point) line-face)))
          (cons preview-start (point)))
      (let ((start (point)))
        (insert label)
        (when (functionp toggle-action)
          (appkit-media-add-action-properties
           start (point)
           (lambda (&optional _event)
             (interactive)
             (funcall toggle-action))
           help)
          (insert " ")
          (appkit-ui-insert-action-button
           (or reveal-label "[Reveal spoiler]")
           toggle-action
           :face button-face
           :help-echo help))
        (insert "\n")
        (appkit-ui-apply-line-prefix start (point) prefix-state)
        (when line-face
          (appkit-ui-append-face start (point) line-face))
        (cons start (point))))))

(cl-defun disco-ins-insert-attachment-transfer-line (attachment &key prefix face)
  "Insert compact transfer status for ATTACHMENT.

Open/play/download/save actions now live on the card context and its transient;
only meaningful transfer state remains inline."
  (appkit-chat-ins-insert-media-status-line
   (appkit-chat-ins-media-transfer-status-text
    (disco-media-attachment-download-state attachment))
   :prefix (or prefix "    ")
   :face face))

(cl-defun disco-ins-insert-attachment-preview-block (attachment &key prefix face
                                                                kind required)
  "Insert preview block for ATTACHMENT when available.

When REQUIRED is non-nil, insert a placeholder line when preview output cannot
be shown yet."
  (let* ((kind (or kind (disco-media-attachment-kind attachment)))
         (video-p (eq kind 'video))
         (preview-source (disco-media-attachment-preview-image attachment))
         (preview (if (and video-p
                           (appkit-media-image-object-valid-p preview-source))
                      (or (appkit-media-video-preview-display-image
                           preview-source 'disco)
                          preview-source)
                    preview-source))
         (preview-url (or (disco-media-attachment-preview-url attachment)
                          (disco-media-attachment-download-url attachment)))
         (rendering-ok (disco-media-attachment-preview-rendering-available-p))
         (cache-key (and (memq kind '(photo video))
                         (disco-media-attachment-preview-cache-key attachment)))
         (cache-state (disco-media-attachment-preview-cache-state cache-key))
         (fetching (disco-media-attachment-preview-fetching-p cache-key))
         (placeholder (cond
                       (preview nil)
                       ((and (not required)
                             (or (not rendering-ok)
                                 (not (appkit-media-url-present-p preview-url))))
                        nil)
                       ((and (not required)
                             (null cache-key))
                        nil)
                       ((not rendering-ok) "[preview disabled]")
                       ((not (appkit-media-url-present-p preview-url))
                        "[no preview URL]")
                       ((eq cache-state :missing)
                        (if video-p
                            "[video preview unavailable]"
                          "[image unavailable]"))
                       ((or fetching cache-key) "[loading preview]")
                       (video-p "[video preview unavailable]")
                       (t "[image unavailable]"))))
    (when (or preview placeholder)
      (let ((start (point))
            (apply-face t))
        (if preview
            (condition-case _
                (progn
                  (setq apply-face nil)
                  (appkit-media-insert-image-slices
                   preview
                   (lambda ()
                     (disco-media-open-attachment attachment))
                   nil
                   (or (alist-get 'description attachment)
                       (if video-p "[video]" "[image]"))
                   (if video-p "Play video" "Open image in Emacs")))
              (error
               (setq apply-face t)
               (insert (or placeholder
                           (if video-p
                               "[video preview unavailable]"
                             "[image unavailable]")))))
          (insert placeholder))
        (insert "\n")
        (appkit-ui-apply-line-prefix start (point) (or prefix "    "))
        (when (and face apply-face)
          (appkit-ui-append-face start (point) face))
        (cons start (point))))))

(cl-defun disco-ins-insert-attachment-document (attachment &key prefix border-face
                                                           title-face meta-face
                                                           action-face show-url
                                                           spoiler-hidden
                                                           spoiler-toggle-action)
  "Insert one document-style attachment block for ATTACHMENT."
  (let* ((kind (disco-media-attachment-kind attachment))
         (name (disco-media-attachment-display-name attachment))
         (details (delq nil (list (disco-media-attachment-size-label attachment)
                                  (when (eq kind 'audio)
                                    (disco-media-attachment-duration-label attachment)))))
         (meta-parts
          (delq nil
                (list (disco-media-attachment-content-type-label attachment)
                      (disco-media-attachment-dimensions-label attachment)
                      (when (disco-media-attachment-ephemeral-p attachment)
                        "ephemeral"))))
         (context (disco-media-attachment-card-context attachment)))
    (when spoiler-hidden
      (setq context (plist-put context :open-action nil)))
    (appkit-chat-ins-insert-media-card
     :kind kind
     :title name
     :details details
     :meta meta-parts
     :status (appkit-chat-ins-media-transfer-status-text
              (disco-media-attachment-download-state attachment))
     :prefix prefix
     :border-face border-face
     :title-face title-face
     :meta-face meta-face
     :context context
     :open-help-echo "Open attachment"
     :body-inserter
     (lambda (prefix-state)
       (if spoiler-hidden
           (disco-ins-insert-attachment-spoiler-placeholder
            attachment
            :prefix prefix-state
            :line-face meta-face
            :button-face action-face
            :toggle-action spoiler-toggle-action)
         (disco-ins-insert-attachment-preview-block
          attachment :prefix prefix-state :face meta-face :kind kind :required nil)
         (disco-ins-insert-attachment-caption-line
          (alist-get 'description attachment) :prefix prefix-state :face meta-face)
         (when show-url
           (disco-ins-insert-attachment-url-line
            (disco-media-attachment-download-url attachment)
            :prefix prefix-state
            :face 'shadow
            :action (lambda () (disco-media-open-attachment attachment)))))))))

(cl-defun disco-ins-insert-attachment-photo (attachment &key prefix border-face
                                                        title-face meta-face
                                                        action-face show-url
                                                        spoiler-hidden
                                                        spoiler-toggle-action)
  "Insert one photo-style attachment block for ATTACHMENT."
  (let* ((name (disco-media-attachment-display-name attachment))
         (meta-parts (delq nil (list (disco-media-attachment-dimensions-label attachment)
                                     (disco-media-attachment-size-label attachment)
                                     (when (disco-media-attachment-ephemeral-p attachment)
                                       "ephemeral"))))
         (context (disco-media-attachment-card-context attachment)))
    (when spoiler-hidden
      (setq context (plist-put context :open-action nil)))
    (appkit-chat-ins-insert-media-card
     :kind 'photo
     :title name
     :meta meta-parts
     :status (appkit-chat-ins-media-transfer-status-text
              (disco-media-attachment-download-state attachment))
     :prefix prefix
     :border-face border-face
     :title-face title-face
     :meta-face meta-face
     :context context
     :open-help-echo "Open image"
     :body-inserter
     (lambda (prefix-state)
       (if spoiler-hidden
           (disco-ins-insert-attachment-spoiler-placeholder
            attachment
            :prefix prefix-state
            :line-face meta-face
            :button-face action-face
            :toggle-action spoiler-toggle-action)
         (disco-ins-insert-attachment-preview-block
          attachment :prefix prefix-state :face meta-face :kind 'photo :required t)
         (disco-ins-insert-attachment-caption-line
          (alist-get 'description attachment) :prefix prefix-state :face meta-face)
         (when show-url
           (disco-ins-insert-attachment-url-line
            (disco-media-attachment-download-url attachment)
            :prefix prefix-state
            :face 'shadow
            :action (lambda () (disco-media-open-attachment attachment))
            :help-echo "Open image in Emacs")))))))

(cl-defun disco-ins-insert-attachment-video (attachment &key prefix border-face
                                                        title-face meta-face
                                                        action-face show-url
                                                        spoiler-hidden
                                                        spoiler-toggle-action)
  "Insert one video-style attachment block for ATTACHMENT."
  (let* ((name (disco-media-attachment-display-name attachment))
         (details (delq nil (list (disco-media-attachment-dimensions-label attachment)
                                  (disco-media-attachment-size-label attachment)
                                  (disco-media-attachment-duration-label attachment)
                                  (when (disco-media-attachment-ephemeral-p attachment)
                                    "ephemeral"))))
         (context (disco-media-attachment-card-context attachment)))
    (when spoiler-hidden
      (setq context (plist-put context :open-action nil)))
    (appkit-chat-ins-insert-media-card
     :kind 'video
     :title name
     :details details
     :status (appkit-chat-ins-media-transfer-status-text
              (disco-media-attachment-download-state attachment))
     :prefix prefix
     :border-face border-face
     :title-face title-face
     :meta-face meta-face
     :context context
     :open-help-echo "Play video"
     :body-inserter
     (lambda (prefix-state)
       (if spoiler-hidden
           (disco-ins-insert-attachment-spoiler-placeholder
            attachment
            :prefix prefix-state
            :line-face meta-face
            :button-face action-face
            :toggle-action spoiler-toggle-action)
         (disco-ins-insert-attachment-preview-block
          attachment :prefix prefix-state :face meta-face :kind 'video :required t)
         (disco-ins-insert-attachment-caption-line
          (alist-get 'description attachment) :prefix prefix-state :face meta-face)
         (when show-url
           (disco-ins-insert-attachment-url-line
            (disco-media-attachment-download-url attachment)
            :prefix prefix-state
            :face 'shadow
            :action (lambda () (disco-media-open-attachment attachment))
            :help-echo "Play video")))))))

(defun disco-ins--attachment-audio-tag (attachment)
  "Return header tag string for ATTACHMENT audio payload."
  (if (disco-media-attachment-voice-message-p attachment)
      "[voice]"
    (appkit-chat-ins-media-kind-tag 'audio)))

(defun disco-ins--attachment-audio-progress-label (attachment progress)
  "Return human-readable playback label for ATTACHMENT at PROGRESS seconds."
  (let ((duration (disco-media-attachment-duration-label attachment)))
    (cond
     ((and (numberp progress) duration)
      (format "%s / %s"
              (disco-media-attachment-duration-label
               `((duration_secs . ,(max 0.0 progress))))
              duration))
     (duration duration)
     ((numberp progress)
      (disco-media-attachment-duration-label
       `((duration_secs . ,(max 0.0 progress)))))
     (t nil))))

(cl-defun disco-ins-insert-attachment-audio (attachment &key prefix border-face
                                                        title-face meta-face
                                                        action-face show-url
                                                        spoiler-hidden
                                                        spoiler-toggle-action)
  "Insert one audio-style attachment block for ATTACHMENT."
  (let* ((name (disco-media-attachment-display-name attachment))
         (details (delq nil (list (disco-media-attachment-size-label attachment)
                                  (disco-media-attachment-duration-label attachment)
                                  (when (disco-media-attachment-ephemeral-p attachment)
                                    "ephemeral"))))
         (prefix-state (appkit-chat-ins-media-prefix-state prefix border-face))
         (context (disco-media-attachment-card-context attachment))
         (inline-playback-p (disco-media-audio-inline-playback-available-p))
         (playing-p (and inline-playback-p
                         (disco-media-attachment-audio-playing-p attachment)))
         (paused-at (and inline-playback-p
                         (disco-media-attachment-audio-paused-p attachment)))
         (progress (and inline-playback-p
                        (or (disco-media-attachment-audio-progress attachment)
                            paused-at)))
         (progress-label (disco-ins--attachment-audio-progress-label attachment progress))
         (waveform-width (max 12 (min 44 (- (window-width) 28))))
         (waveform-image (disco-media-attachment-waveform-image
                          attachment
                          :width waveform-width
                          :progress progress))
         (waveform-text (unless (appkit-media-image-object-valid-p waveform-image)
                          (disco-media-attachment-waveform-string
                           attachment
                           :width waveform-width
                           :progress progress
                           :played-face title-face
                           :unplayed-face meta-face)))
         (play-help (if playing-p
                        "Pause audio playback"
                      "Play audio attachment"))
         (card-start (point)))
    (when spoiler-hidden
      (setq context (plist-put context :open-action nil)))
    (appkit-chat-ins-insert-prefixed-line
     (format "%s %s%s"
             (disco-ins--attachment-audio-tag attachment)
             name
             (appkit-chat-ins-media-detail-text details))
     :prefix prefix-state
     :face title-face
     :action (plist-get context :open-action)
     :help-echo play-help)
    (let* ((content-type (disco-media-attachment-content-type-label attachment))
           (meta-parts (delq nil (list content-type))))
      (when meta-parts
        (appkit-chat-ins-insert-prefixed-line
         (string-join meta-parts "  ")
         :prefix prefix-state
         :face meta-face)))
    (if spoiler-hidden
        (disco-ins-insert-attachment-spoiler-placeholder
         attachment
         :prefix prefix-state
         :line-face meta-face
         :button-face action-face
         :toggle-action spoiler-toggle-action)
      (progn
        (let ((playback-start (point)))
          (appkit-ui-insert-action-button
           (cond
            (playing-p "[Pause]")
            (paused-at "[Resume]")
            (t "[Play]"))
           (lambda ()
             (disco-media-play-attachment-audio attachment))
           :face action-face
           :help-echo play-help)
          (when (or waveform-image waveform-text progress-label
                    (and inline-playback-p (or playing-p paused-at progress)))
            (insert " "))
          (let ((wave-start (point)))
            (cond
             ((appkit-media-image-object-valid-p waveform-image)
              (insert-image waveform-image "[waveform]")
              (appkit-media-add-action-properties
               wave-start (point)
               (lambda (&optional _event)
                 (interactive)
                 (disco-media-play-attachment-audio attachment))
               play-help))
             ((and (stringp waveform-text) (not (string-empty-p waveform-text)))
              (insert waveform-text)
              (appkit-media-add-action-properties
               wave-start (point)
               (lambda (&optional _event)
                 (interactive)
                 (disco-media-play-attachment-audio attachment))
               play-help))))
          (when progress-label
            (insert "  " progress-label))
          (when (and inline-playback-p (or playing-p paused-at progress))
            (insert " ")
            (appkit-ui-insert-action-button
             "[Stop]"
             (lambda ()
               (disco-media-stop-attachment-audio attachment))
             :face action-face
             :help-echo "Stop audio playback"))
          (insert "\n")
          (appkit-ui-apply-line-prefix playback-start (point) prefix-state)
          (when meta-face
            (appkit-ui-append-face playback-start (point) meta-face)))
        (disco-ins-insert-attachment-transfer-line
         attachment :prefix prefix-state :face meta-face)
        (disco-ins-insert-attachment-caption-line
         (alist-get 'description attachment) :prefix prefix-state :face meta-face)
        (when show-url
          (disco-ins-insert-attachment-url-line
           (disco-media-attachment-download-url attachment)
           :prefix prefix-state
           :face 'shadow
           :action (lambda () (disco-media-open-attachment attachment))
           :help-echo "Play audio"))))
    (add-text-properties
     card-start (point)
     (list appkit-media-card-context-property context))
    (cons card-start (point))))

(cl-defun disco-ins-insert-forward-card
    (&key source-text sent-at content insert-source-icon title-label
          open-action open-help-echo border-face title-face meta-face)
  "Insert one forwarded-message card.

SOURCE-TEXT is the rendered source label line body.  SENT-AT and CONTENT are
optional metadata/body strings.  INSERT-SOURCE-ICON, when non-nil, is called to
insert an inline source icon before SOURCE-TEXT.  OPEN-ACTION makes the title
and source lines navigable without adding a separate button row.
BORDER-FACE, TITLE-FACE, and META-FACE control the card styling.  Return the
inserted span as `(START . END)'."
  (let ((card-start (point))
        (prefix-state (appkit-ui-card-prefix-state :face border-face)))
    (let ((title-start (point)))
      (insert (or title-label "Forwarded message") "\n")
      (appkit-ui-apply-line-prefix title-start (point) prefix-state)
      (when title-face
        (appkit-ui-append-face title-start (point) title-face))
      (when (functionp open-action)
        (appkit-media-add-action-properties
         title-start (max title-start (1- (point)))
         open-action (or open-help-echo "Open forwarded message"))))
    (let ((source-start (point)))
      (insert "source: ")
      (when (functionp insert-source-icon)
        (funcall insert-source-icon)
        (insert " "))
      (insert (or source-text "unknown"))
      (insert "\n")
      (appkit-ui-apply-line-prefix source-start (point) prefix-state)
      (when meta-face
        (appkit-ui-append-face source-start (point) meta-face))
      (when (functionp open-action)
        (appkit-media-add-action-properties
         source-start (max source-start (1- (point)))
         open-action (or open-help-echo "Open forwarded message"))))
    (when (and (stringp sent-at) (not (string-empty-p sent-at)))
      (let ((time-start (point)))
        (insert "sent: " sent-at "\n")
        (appkit-ui-apply-line-prefix time-start (point) prefix-state)
        (when meta-face
          (appkit-ui-append-face time-start (point) meta-face))))
    (when (and (stringp content) (not (string-empty-p content)))
      (let ((content-start (point)))
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        (appkit-ui-apply-line-prefix content-start (point) prefix-state)))
    (cons card-start (point))))

(provide 'disco-ins)

;;; disco-ins.el ends here
