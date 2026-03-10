;;; disco-ins.el --- Shared insert/render leaf helpers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared insertion helpers for chat-like renderers.  This module is the
;; owner for small render leaves and formatting primitives; room/root EWOC and
;; timeline orchestration should stay with their UI facades.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-media)
(require 'disco-msg)
(require 'disco-ui)

(defun disco-ins-prefix-string (prefix &optional consume default)
  "Return normalized prefix string from PREFIX.

When PREFIX is a mutable prefix-state and CONSUME is non-nil, consume its
first-prefix.  DEFAULT falls back to the empty string when omitted."
  (disco-ui-prefix-string prefix consume (or default "")))

(defun disco-ins-insert-prefixed-lines (prefix text &optional face)
  "Insert TEXT as newline-separated lines, each prefixed by PREFIX.

When FACE is non-nil, apply FACE to each inserted line."
  (disco-ui-insert-prefixed-lines prefix text :face face))

(defun disco-ins-insert-full-width-divider (label face target-width
                                                  &optional properties)
  "Insert a centered divider for LABEL spanning TARGET-WIDTH columns.

FACE is applied to the entire inserted span.  PROPERTIES is an optional plist
of additional text properties.  Return the inserted span as (START . END)."
  (let* ((open "( ")
         (close " )")
         (inner-width (+ (string-width open)
                         (string-width label)
                         (string-width close)))
         (fill-col (max 0 (or target-width 0)))
         (total-bar (max 4 (- fill-col inner-width)))
         (left-bars (/ total-bar 2))
         (right-bars (- total-bar left-bars))
         (start (point)))
    (insert (make-string left-bars ?─)
            open label close
            (make-string right-bars ?─)
            "\n")
    (add-face-text-property start (point) face t)
    (when properties
      (add-text-properties start (point) properties))
    (cons start (point))))

(defun disco-ins-insert-divider-row (text face target-width &optional properties)
  "Insert read-only divider row TEXT using FACE across TARGET-WIDTH.

PROPERTIES is appended before the standard read-only divider properties.
Return the inserted span as (START . END)."
  (disco-ins-insert-full-width-divider
   text face target-width
   (append properties
           '(read-only t
             front-sticky (read-only)
             rear-nonsticky (read-only)))))

(cl-defun disco-ins-insert-reference-line (body &key prefix face button-label
                                                button-action button-face
                                                properties)
  "Insert one prefixed reference line with optional BODY and action button.

PREFIX is applied with `disco-ui-apply-line-prefix'.  FACE and PROPERTIES are
applied to the inserted span.  When BUTTON-LABEL is non-nil, BUTTON-ACTION is
inserted using `disco-ui-insert-action-button'.  Return the inserted span as
(START . END)."
  (let ((line-start (point))
        (text (and (stringp body) (not (string-empty-p body)) body)))
    (insert "↪")
    (when (or text button-label)
      (insert " "))
    (when text
      (insert text))
    (when button-label
      (when text
        (insert " "))
      (if (functionp button-action)
          (disco-ui-insert-action-button
           button-label button-action :face button-face)
        (insert (if button-face
                    (propertize button-label 'face button-face)
                  button-label))))
    (insert "\n")
    (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
    (when (or face properties)
      (add-text-properties
       line-start (point)
       (append properties
               (when face (list 'face face)))))
    (cons line-start (point))))

(cl-defun disco-ins-insert-reaction-line (reactions &key prefix selected-face
                                                    unselected-face line-face)
  "Insert one reaction chip line for REACTIONS.

PREFIX is applied with `disco-ui-apply-line-prefix'.  SELECTED-FACE and
UNSELECTED-FACE style each reaction chip.  LINE-FACE is applied to the whole
inserted span.  Return the inserted span as (START . END), or nil when
REACTIONS is empty."
  (when reactions
    (let ((line-start (point))
          (first t))
      (dolist (reaction reactions)
        (unless first
          (insert " "))
        (setq first nil)
        (let ((chip (format "[%s %s]"
                            (disco-msg-reaction-emoji reaction)
                            (disco-msg-reaction-count reaction))))
          (insert (propertize chip
                              'face (if (disco-msg-reaction-selected-p reaction)
                                        selected-face
                                      unselected-face)))))
      (insert "\n")
      (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
      (when line-face
        (add-face-text-property line-start (point) line-face nil))
      (cons line-start (point)))))

(cl-defun disco-ins-insert-attachment-lines (summary &key prefix url
                                                     summary-face url-face)
  "Insert plain attachment SUMMARY and optional URL lines.

PREFIX is applied with `disco-ui-apply-line-prefix'.  SUMMARY-FACE styles the
summary line, and URL-FACE styles the optional URL line.  Return the inserted
span as (START . END), or nil when SUMMARY is empty."
  (when (and (stringp summary) (not (string-empty-p summary)))
    (let ((start (point)))
      (let ((summary-start (point)))
        (insert summary "\n")
        (disco-ui-apply-line-prefix summary-start (point) (or prefix "    "))
        (when summary-face
          (add-text-properties summary-start (point) (list 'face summary-face))))
      (when (and (stringp url) (not (string-empty-p url)))
        (let ((url-start (point)))
          (insert "  " url "\n")
          (disco-ui-apply-line-prefix url-start (point) (or prefix "    "))
          (when url-face
            (add-text-properties url-start (point) (list 'face url-face)))))
      (cons start (point)))))


(defun disco-ins--attachment-prefix-state (prefix border-face)
  "Return effective attachment prefix state from PREFIX and BORDER-FACE."
  (cond
   ((disco-ui-prefix-state-p prefix) prefix)
   ((stringp prefix) (disco-ui-make-prefix-state prefix prefix))
   (t (disco-ui-card-prefix-state :face border-face))))

(cl-defun disco-ins--insert-prefixed-line (text &key prefix face properties
                                                action help-echo)
  "Insert TEXT as one prefixed line with optional ACTION and FACE."
  (let ((start (point)))
    (insert (or text "") "\n")
    (when (and (functionp action)
               (> (point) start))
      (disco-media-add-action-properties
       start
       (max start (1- (point)))
       (lambda (&optional _event)
         (interactive)
         (funcall action))
       help-echo))
    (disco-ui-apply-line-prefix start (point) (or prefix "    "))
    (when properties
      (add-text-properties start (point) properties))
    (when face
      (disco-ui-append-face start (point) face))
    (cons start (point))))

(defun disco-ins--attachment-open-info (attachment)
  "Return (ACTION . HELP-ECHO) for ATTACHMENT header, or nil."
  (let* ((state (disco-media-attachment-download-state attachment))
         (path (plist-get state :path))
         (url (disco-media-attachment-download-url attachment)))
    (cond
     ((and (stringp path) (file-exists-p path))
      (cons (lambda ()
              (disco-media-open-downloaded-attachment attachment))
            "Open downloaded attachment file"))
     ((disco-media-url-present-p url)
      (cons (lambda ()
              (browse-url url t))
            "Open attachment URL"))
     (t nil))))

(defun disco-ins--attachment-kind-tag (kind)
  "Return human-oriented header tag string for attachment KIND."
  (pcase kind
    ('photo "[image]")
    ('video "[video]")
    ('audio "[audio]")
    (_ "[file]")))

(defun disco-ins--attachment-detail-text (details)
  "Return formatted header detail string for DETAILS list."
  (if details
      (format " (%s)" (string-join details ", "))
    ""))

(cl-defun disco-ins-insert-attachment-caption-line (caption &key prefix face
                                                            (label "caption: "))
  "Insert attachment CAPTION line when CAPTION is non-empty."
  (when (and (stringp caption) (not (string-empty-p caption)))
    (disco-ins--insert-prefixed-line
     (concat label caption)
     :prefix (or prefix "    ")
     :face face)))

(cl-defun disco-ins-insert-attachment-url-line (url &key prefix face)
  "Insert clickable attachment URL line when URL is non-empty."
  (when (disco-media-url-present-p url)
    (let ((start (point)))
      (insert url "\n")
      (disco-media-add-open-url-properties start (max start (1- (point))) url)
      (disco-ui-apply-line-prefix start (point) (or prefix "    "))
      (when face
        (disco-ui-append-face start (point) face))
      (cons start (point)))))

(cl-defun disco-ins-insert-attachment-spoiler-placeholder
    (attachment &key prefix border-face line-face button-face
                toggle-action toggle-help-echo reveal-label)
  "Insert hidden spoiler block for ATTACHMENT.

When a cached media preview exists, show an obscured preview tile similar to
telega; otherwise fall back to a text placeholder line."
  (let* ((prefix-state (disco-ins--attachment-prefix-state prefix border-face))
         (label (disco-media-attachment-spoiler-label attachment))
         (help (or toggle-help-echo "Reveal spoiler"))
         (preview (disco-media-attachment-spoiler-preview-image attachment)))
    (if (disco-media-image-object-valid-p preview)
        (let ((preview-start (point)))
          (disco-media-insert-image-slices preview nil nil label)
          (when (functionp toggle-action)
            (disco-media-add-action-properties
             preview-start (point)
             (lambda (&optional _event)
               (interactive)
               (funcall toggle-action))
             help))
          (insert "\n")
          (disco-ui-apply-line-prefix preview-start (point) prefix-state)
          (let ((action-start (point)))
            (if (functionp toggle-action)
                (disco-ui-insert-action-button
                 (or reveal-label "[Reveal spoiler]")
                 toggle-action
                 :face button-face
                 :help-echo help)
              (insert (or reveal-label "[Reveal spoiler]")))
            (insert "\n")
            (disco-ui-apply-line-prefix action-start (point) prefix-state)
            (when line-face
              (disco-ui-append-face action-start (point) line-face)))
          (cons preview-start (point)))
      (let ((start (point)))
        (insert label)
        (when (functionp toggle-action)
          (disco-media-add-action-properties
           start (point)
           (lambda (&optional _event)
             (interactive)
             (funcall toggle-action))
           help)
          (insert " ")
          (disco-ui-insert-action-button
           (or reveal-label "[Reveal spoiler]")
           toggle-action
           :face button-face
           :help-echo help))
        (insert "\n")
        (disco-ui-apply-line-prefix start (point) prefix-state)
        (when line-face
          (disco-ui-append-face start (point) line-face))
        (cons start (point))))))

(cl-defun disco-ins-insert-attachment-transfer-line (attachment &key prefix face
                                                                action-face kind
                                                                (allow-play t))
  "Insert telega-style transfer/progress line for ATTACHMENT."
  (let* ((line-start (point))
         (state (disco-media-attachment-download-state attachment))
         (status (plist-get state :status))
         (path (plist-get state :path))
         (error-text (plist-get state :error))
         (url (disco-media-attachment-download-url attachment))
         (effective-kind (or kind (disco-media-attachment-kind attachment)))
         (has-local (and (stringp path) (file-exists-p path)))
         (has-url (disco-media-url-present-p url))
         (playable-video-p (and allow-play (eq effective-kind 'video)))
         (playable-audio-p (and allow-play (eq effective-kind 'audio)))
         (inserted-action nil))
    (cl-labels ((emit-button (label action help)
                  (when inserted-action
                    (insert " "))
                  (disco-ui-insert-action-button
                   label action :face action-face :help-echo help)
                  (setq inserted-action t))
                (emit-play-button (downloaded-p)
                  (cond
                   (playable-video-p
                    (emit-button "[Play]"
                                 (lambda ()
                                   (disco-media-play-attachment-video attachment))
                                 (if downloaded-p
                                     "Play downloaded video"
                                   "Play video URL")))
                   (playable-audio-p
                    (emit-button "[Play]"
                                 (lambda ()
                                   (disco-media-play-attachment-audio attachment))
                                 (if downloaded-p
                                     "Play downloaded audio"
                                   "Play audio URL"))))))
      (insert "transfer: ")
      (pcase status
        ('downloading
         (insert "[Downloading...] ")
         (emit-button "[Cancel]"
                      (lambda ()
                        (disco-media-cancel-attachment-download attachment))
                      "Cancel attachment download"))
        ('downloaded
         (emit-play-button t)
         (emit-button "[Open Local]"
                      (lambda ()
                        (disco-media-open-downloaded-attachment attachment))
                      "Open downloaded attachment file")
         (emit-button "[Save As]"
                      (lambda ()
                        (disco-media-download-attachment attachment))
                      "Copy downloaded file (or download) to chosen path")
         (when has-local
           (insert "  " (file-name-nondirectory path))))
        ('error
         (when has-url
           (emit-play-button nil)
           (emit-button "[Retry]"
                        (lambda ()
                          (disco-media-start-attachment-download attachment nil))
                        "Retry attachment download"))
         (when (or has-url has-local)
           (emit-button "[Save As]"
                        (lambda ()
                          (disco-media-download-attachment attachment))
                        "Download attachment to chosen path"))
         (unless inserted-action
           (insert "[No URL]"))
         (when (and (stringp error-text) (not (string-empty-p error-text)))
           (insert "  error="
                   (truncate-string-to-width error-text 68 nil nil t))))
        (_
         (when has-url
           (emit-play-button nil)
           (emit-button "[Download]"
                        (lambda ()
                          (disco-media-start-attachment-download attachment nil))
                        "Download attachment into local cache directory")
           (emit-button "[Save As]"
                        (lambda ()
                          (disco-media-download-attachment attachment))
                        "Download attachment to chosen path"))
         (unless (or has-url has-local inserted-action)
           (insert "[No URL]"))))
      (insert "\n")
      (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
      (when face
        (disco-ui-append-face line-start (point) face))
      (cons line-start (point)))))

(cl-defun disco-ins-insert-attachment-preview-block (attachment &key prefix face
                                                                kind required)
  "Insert preview block for ATTACHMENT when available.

When REQUIRED is non-nil, insert a placeholder line when preview output cannot
be shown yet."
  (let* ((kind (or kind (disco-media-attachment-kind attachment)))
         (video-p (eq kind 'video))
         (preview-source (disco-media-attachment-preview-image attachment))
         (preview (if (and video-p
                           (disco-media-image-object-valid-p preview-source))
                      (or (disco-media-attachment-video-display-image preview-source)
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
                                 (not (disco-media-url-present-p preview-url))))
                        nil)
                       ((and (not required)
                             (null cache-key))
                        nil)
                       ((not rendering-ok) "[preview disabled]")
                       ((not (disco-media-url-present-p preview-url)) "[no preview URL]")
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
                (let ((slice-start (point)))
                  (setq apply-face nil)
                  (disco-media-insert-image-slices
                   preview
                   (unless video-p preview-url)
                   nil
                   (if video-p "[video]" "[image]"))
                  (when (and video-p
                             (disco-media-url-present-p preview-url))
                    (disco-media-add-play-video-properties
                     slice-start
                     (point)
                     preview-url)))
              (error
               (setq apply-face t)
               (insert (or placeholder
                           (if video-p
                               "[video preview unavailable]"
                             "[image unavailable]")))))
          (insert placeholder))
        (insert "\n")
        (disco-ui-apply-line-prefix start (point) (or prefix "    "))
        (when (and face apply-face)
          (disco-ui-append-face start (point) face))
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
         (header (format "%s %s%s"
                         (disco-ins--attachment-kind-tag kind)
                         name
                         (disco-ins--attachment-detail-text details)))
         (prefix-state (disco-ins--attachment-prefix-state prefix border-face))
         (open-info (unless spoiler-hidden
                      (disco-ins--attachment-open-info attachment)))
         (card-start (point)))
    (disco-ins--insert-prefixed-line
     header
     :prefix prefix-state
     :face title-face
     :action (car-safe open-info)
     :help-echo (cdr-safe open-info))
    (let* ((content-type (disco-media-attachment-content-type-label attachment))
           (dims (disco-media-attachment-dimensions-label attachment))
           (ephemeral (when (disco-media-attachment-ephemeral-p attachment)
                        "ephemeral"))
           (meta-parts (delq nil (list content-type dims ephemeral))))
      (when meta-parts
        (disco-ins--insert-prefixed-line
         (string-join meta-parts "  ")
         :prefix prefix-state
         :face meta-face)))
    (disco-ins-insert-attachment-transfer-line
     attachment :prefix prefix-state :face meta-face :action-face action-face
     :kind kind)
    (if spoiler-hidden
        (disco-ins-insert-attachment-spoiler-placeholder
         attachment
         :prefix prefix-state
         :line-face meta-face
         :button-face action-face
         :toggle-action spoiler-toggle-action)
      (progn
        (disco-ins-insert-attachment-preview-block
         attachment :prefix prefix-state :face meta-face :kind kind :required nil)
        (disco-ins-insert-attachment-caption-line
         (alist-get 'description attachment) :prefix prefix-state :face meta-face)
        (when show-url
          (disco-ins-insert-attachment-url-line
           (disco-media-attachment-download-url attachment)
           :prefix prefix-state
           :face 'shadow))))
    (cons card-start (point))))

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
         (prefix-state (disco-ins--attachment-prefix-state prefix border-face))
         (open-info (unless spoiler-hidden
                      (disco-ins--attachment-open-info attachment)))
         (card-start (point)))
    (disco-ins--insert-prefixed-line
     (format "%s %s" (disco-ins--attachment-kind-tag 'photo) name)
     :prefix prefix-state
     :face title-face
     :action (car-safe open-info)
     :help-echo (cdr-safe open-info))
    (when meta-parts
      (disco-ins--insert-prefixed-line
       (string-join meta-parts "  ")
       :prefix prefix-state
       :face meta-face))
    (disco-ins-insert-attachment-transfer-line
     attachment :prefix prefix-state :face meta-face :action-face action-face
     :kind 'photo)
    (if spoiler-hidden
        (disco-ins-insert-attachment-spoiler-placeholder
         attachment
         :prefix prefix-state
         :line-face meta-face
         :button-face action-face
         :toggle-action spoiler-toggle-action)
      (progn
        (disco-ins-insert-attachment-preview-block
         attachment :prefix prefix-state :face meta-face :kind 'photo :required t)
        (disco-ins-insert-attachment-caption-line
         (alist-get 'description attachment) :prefix prefix-state :face meta-face)
        (when show-url
          (disco-ins-insert-attachment-url-line
           (disco-media-attachment-download-url attachment)
           :prefix prefix-state
           :face 'shadow))))
    (cons card-start (point))))

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
         (prefix-state (disco-ins--attachment-prefix-state prefix border-face))
         (open-info (unless spoiler-hidden
                      (disco-ins--attachment-open-info attachment)))
         (card-start (point)))
    (disco-ins--insert-prefixed-line
     (format "%s %s%s"
             (disco-ins--attachment-kind-tag 'video)
             name
             (disco-ins--attachment-detail-text details))
     :prefix prefix-state
     :face title-face
     :action (car-safe open-info)
     :help-echo (cdr-safe open-info))
    (disco-ins-insert-attachment-transfer-line
     attachment :prefix prefix-state :face meta-face :action-face action-face
     :kind 'video)
    (if spoiler-hidden
        (disco-ins-insert-attachment-spoiler-placeholder
         attachment
         :prefix prefix-state
         :line-face meta-face
         :button-face action-face
         :toggle-action spoiler-toggle-action)
      (progn
        (disco-ins-insert-attachment-preview-block
         attachment :prefix prefix-state :face meta-face :kind 'video :required t)
        (disco-ins-insert-attachment-caption-line
         (alist-get 'description attachment) :prefix prefix-state :face meta-face)
        (when show-url
          (disco-ins-insert-attachment-url-line
           (disco-media-attachment-download-url attachment)
           :prefix prefix-state
           :face 'shadow))))
    (cons card-start (point))))

(defun disco-ins--attachment-audio-tag (attachment)
  "Return header tag string for ATTACHMENT audio payload."
  (if (disco-media-attachment-voice-message-p attachment)
      "[voice]"
    (disco-ins--attachment-kind-tag 'audio)))

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
         (prefix-state (disco-ins--attachment-prefix-state prefix border-face))
         (open-info (unless spoiler-hidden
                      (disco-ins--attachment-open-info attachment)))
         (inline-playback-p (disco-media-audio-inline-playback-available-p))
         (playing-p (and inline-playback-p
                         (disco-media-attachment-audio-playing-p attachment)))
         (paused-at (and inline-playback-p
                         (disco-media-attachment-audio-paused-p attachment)))
         (progress (and inline-playback-p
                        (or (disco-media-attachment-audio-progress attachment)
                            paused-at)))
         (progress-label (disco-ins--attachment-audio-progress-label attachment progress))
         (waveform-width (max 12 (min 36 (- (window-width) 28))))
         (waveform (disco-media-attachment-waveform-string
                    attachment
                    :width waveform-width
                    :progress progress
                    :played-face title-face
                    :unplayed-face meta-face))
         (card-start (point)))
    (disco-ins--insert-prefixed-line
     (format "%s %s%s"
             (disco-ins--attachment-audio-tag attachment)
             name
             (disco-ins--attachment-detail-text details))
     :prefix prefix-state
     :face title-face
     :action (car-safe open-info)
     :help-echo (cdr-safe open-info))
    (let* ((content-type (disco-media-attachment-content-type-label attachment))
           (meta-parts (delq nil (list content-type))))
      (when meta-parts
        (disco-ins--insert-prefixed-line
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
        (when inline-playback-p
          (let ((playback-start (point)))
            (insert "playback: ")
            (disco-ui-insert-action-button
             (cond
              (playing-p "[Pause]")
              (paused-at "[Resume]")
              (t "[Play]"))
             (lambda ()
               (disco-media-play-attachment-audio attachment))
             :face action-face
             :help-echo (if playing-p
                            "Pause audio playback"
                          "Play audio attachment"))
            (when (or playing-p paused-at progress)
              (insert " ")
              (disco-ui-insert-action-button
               "[Stop]"
               (lambda ()
                 (disco-media-stop-attachment-audio attachment))
               :face action-face
               :help-echo "Stop audio playback"))
            (when progress-label
              (insert "  " progress-label))
            (insert "\n")
            (disco-ui-apply-line-prefix playback-start (point) prefix-state)
            (when meta-face
              (disco-ui-append-face playback-start (point) meta-face))))
        (when waveform
          (let ((wave-start (point)))
            (insert waveform "\n")
            (disco-ui-apply-line-prefix wave-start (point) prefix-state)
            (when meta-face
              (disco-ui-append-face wave-start (point) meta-face))))
        (disco-ins-insert-attachment-transfer-line
         attachment :prefix prefix-state :face meta-face :action-face action-face
         :kind 'audio :allow-play (not inline-playback-p))
        (disco-ins-insert-attachment-caption-line
         (alist-get 'description attachment) :prefix prefix-state :face meta-face)
        (when show-url
          (disco-ins-insert-attachment-url-line
           (disco-media-attachment-download-url attachment)
           :prefix prefix-state
           :face 'shadow))))
    (cons card-start (point))))

(cl-defun disco-ins-insert-forward-card (&key source-text sent-at content
                                              insert-source-icon title-label
                                              jump-label jump-action
                                              jump-face jump-help-echo
                                              border-face title-face meta-face)
  "Insert one forwarded-message card.

SOURCE-TEXT is the rendered source label line body.  SENT-AT and CONTENT are
optional metadata/body strings.  INSERT-SOURCE-ICON, when non-nil, is called to
insert an inline source icon before SOURCE-TEXT.  JUMP-LABEL and JUMP-ACTION
configure an optional action button row.  BORDER-FACE, TITLE-FACE, and
META-FACE control the card styling.  Return the inserted span as (START . END)."
  (let ((card-start (point))
        (prefix-state (disco-ui-card-prefix-state :face border-face)))
    (let ((title-start (point)))
      (insert (or title-label "[forwarded message]") "\n")
      (disco-ui-apply-line-prefix title-start (point) prefix-state)
      (when title-face
        (disco-ui-append-face title-start (point) title-face)))
    (let ((source-start (point)))
      (insert "source: ")
      (when (functionp insert-source-icon)
        (funcall insert-source-icon)
        (insert " "))
      (insert (or source-text "unknown"))
      (insert "\n")
      (disco-ui-apply-line-prefix source-start (point) prefix-state)
      (when meta-face
        (disco-ui-append-face source-start (point) meta-face)))
    (when (and (stringp sent-at) (not (string-empty-p sent-at)))
      (let ((time-start (point)))
        (insert "sent: " sent-at "\n")
        (disco-ui-apply-line-prefix time-start (point) prefix-state)
        (when meta-face
          (disco-ui-append-face time-start (point) meta-face))))
    (when (and (stringp content) (not (string-empty-p content)))
      (let ((content-start (point)))
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        (disco-ui-apply-line-prefix content-start (point) prefix-state)))
    (when jump-label
      (let ((action-start (point)))
        (if (functionp jump-action)
            (disco-ui-insert-action-button
             jump-label jump-action
             :face jump-face
             :help-echo jump-help-echo)
          (insert (if jump-face
                      (propertize jump-label 'face jump-face)
                    jump-label)))
        (insert "\n")
        (disco-ui-apply-line-prefix action-start (point) prefix-state)
        (when meta-face
          (disco-ui-append-face action-start (point) meta-face))))
    (cons card-start (point))))

(provide 'disco-ins)

;;; disco-ins.el ends here
