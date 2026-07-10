;;; disco-ins.el --- Shared insert/render leaf helpers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared insertion helpers for chat-like renderers.  This module is the
;; owner for small render leaves and formatting primitives; room/root EWOC and
;; timeline orchestration should stay with their UI facades.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)
(require 'disco-media)
(require 'disco-msg)
(require 'disco-ui)
(require 'disco-view)

(defun disco-ins-prefix-string (prefix &optional consume default)
  "Return normalized prefix string from PREFIX.

When PREFIX is a mutable prefix-state and CONSUME is non-nil, consume its
first-prefix.  DEFAULT falls back to the empty string when omitted."
  (disco-ui-prefix-string prefix consume (or default "")))

(defun disco-ins-insert-prefixed-lines (prefix text &optional face)
  "Insert TEXT as newline-separated lines, each prefixed by PREFIX.

When FACE is non-nil, apply FACE to each inserted line."
  (disco-ui-insert-prefixed-lines prefix text :face face))

(defun disco-ins--current-line-prefix-width ()
  "Return the display prefix width already attached to the current line."
  (let ((prefix (or (get-text-property (line-beginning-position) 'line-prefix)
                    (get-text-property (line-beginning-position) 'wrap-prefix))))
    (if (stringp prefix) (string-width prefix) 0)))

(cl-defun disco-ins-insert-right-aligned-text
    (text target-width &key face (right-align-p t) left-prefix-width
          (minimum-gap 2) (overflow-newline-p t))
  "Insert TEXT at the right edge of TARGET-WIDTH and return its span.

FACE styles TEXT.  When RIGHT-ALIGN-P is nil, insert one ordinary separating
space instead.  LEFT-PREFIX-WIDTH reserves a display-only prefix which the
caller will apply after insertion.  MINIMUM-GAP is the required gap between
existing line content and TEXT.  If the line cannot fit and
OVERFLOW-NEWLINE-P is non-nil, place TEXT on a new right-aligned line."
  (let* ((raw (or text ""))
         (rendered (if face (propertize raw 'face face) raw))
         (target-width (max 0 (or target-width 0)))
         (prefix-width (+ (max 0 (or left-prefix-width 0))
                          (disco-ins--current-line-prefix-width)))
         (start (point)))
    (if right-align-p
        (let* ((tail-width (string-width raw))
               (target-column (max 0 (- target-width tail-width)))
               (current-column (+ prefix-width
                                  (disco-view-current-column))))
          (when (and overflow-newline-p
                     (> current-column
                        (max 0 (- target-column
                                  (max 0 (or minimum-gap 0))))))
            (insert "\n")
            (setq start (point)))
          (disco-view-move-to-column target-column))
      (insert " "))
    (insert rendered)
    (cons start (point))))

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

(cl-defun disco-ins-insert-reaction-line
    (reactions &key prefix selected-face unselected-face line-face
               label-function selected-p-function action-function
               help-echo-function)
  "Insert one reaction chip line for REACTIONS.

PREFIX is applied with `disco-ui-apply-line-prefix'.  SELECTED-FACE and
UNSELECTED-FACE style each reaction chip.  LINE-FACE applies to the whole
inserted span.

LABEL-FUNCTION formats one reaction, SELECTED-P-FUNCTION identifies the
current account's reactions, ACTION-FUNCTION makes chips clickable and is
called with the selected reaction, and HELP-ECHO-FUNCTION supplies hover
text.  Defaults preserve the Discord reaction representation.  Return the
inserted span as (START . END), or nil when REACTIONS is empty."
  (when reactions
    (let ((line-start (point))
          (first t))
      (dolist (reaction reactions)
        (unless first
          (insert " "))
        (setq first nil)
        (let* ((item reaction)
               (chip (if label-function
                         (funcall label-function item)
                       (format "[%s %s]"
                               (disco-msg-reaction-emoji item)
                               (disco-msg-reaction-count item))))
               (selected-p
                (if selected-p-function
                    (funcall selected-p-function item)
                  (disco-msg-reaction-selected-p item)))
               (face (if selected-p selected-face unselected-face))
               (help-echo (and help-echo-function
                               (funcall help-echo-function item))))
          (if action-function
              (insert-text-button
               chip
               'follow-link t
               'face face
               'help-echo help-echo
               'action
               (lambda (_button)
                 (funcall action-function item)))
            (insert (propertize chip 'face face)))))
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

(defun disco-ins--attachment-kind-tag (kind)
  "Return human-oriented header tag string for attachment KIND."
  (pcase kind
    ((or 'photo 'image) "[image]")
    ('video "[video]")
    ('audio "[audio]")
    ('sticker "[sticker]")
    (_ "[file]")))

(defun disco-ins--attachment-detail-text (details)
  "Return formatted header detail string for DETAILS list."
  (if details
      (format " (%s)" (string-join details ", "))
    ""))

(defun disco-ins-media-transfer-status-text (state)
  "Return compact transfer status text for normalized media STATE.

STATE uses the shared `:status', `:path', and `:error' plist shape already
returned by both disco and emacs-qq media adapters.  Ordinary remote media has
no status line; only active, local, or failed transfer state occupies timeline
space."
  (let ((status (plist-get state :status))
        (path (plist-get state :path))
        (error-text (plist-get state :error)))
    (pcase status
      ('downloading "downloading…")
      ('downloaded
       (if (and (stringp path) (not (string-empty-p path)))
           (format "local: %s" (file-name-nondirectory path))
         "downloaded"))
      ('error
       (if (and (stringp error-text) (not (string-empty-p error-text)))
           (format "download failed: %s"
                   (truncate-string-to-width error-text 68 nil nil t))
         "download failed"))
      (_ nil))))

(cl-defun disco-ins-insert-media-status-line (status &key prefix face)
  "Insert compact media STATUS using PREFIX and FACE."
  (when (and (stringp status) (not (string-empty-p status)))
    (disco-ins--insert-prefixed-line
     status :prefix (or prefix "    ") :face face)))

(cl-defun disco-ins-insert-media-card
    (&key kind title details meta status prefix border-face title-face meta-face
          properties context open-action open-help-echo body-inserter)
  "Insert one backend-neutral compact media card.

KIND, TITLE, DETAILS, META, and STATUS describe presentation only.  CONTEXT is
a `disco-media-card-context-create' value stored as a text property across the
card, so message transients can target the exact attachment/segment at point.
OPEN-ACTION defaults to the context's open callback.  BODY-INSERTER, when
non-nil, receives the mutable prefix-state and inserts previews, captions, or
stateful controls owned by the client adapter.  PROPERTIES are applied across
the final card span."
  (let* ((card-start (point))
         (prefix-state (disco-ins--attachment-prefix-state prefix border-face))
         (details (delq nil (copy-sequence (or details '()))))
         (meta-text (cond
                     ((stringp meta) meta)
                     ((listp meta) (string-join (delq nil (copy-sequence meta)) "  "))
                     (t nil)))
         (action (or open-action (plist-get context :open-action))))
    (disco-ins--insert-prefixed-line
     (format "%s %s%s"
             (disco-ins--attachment-kind-tag kind)
             (or title "media")
             (disco-ins--attachment-detail-text details))
     :prefix prefix-state
     :face title-face
     :action action
     :help-echo (or open-help-echo "Open media"))
    (when (and (stringp meta-text) (not (string-empty-p meta-text)))
      (disco-ins--insert-prefixed-line
       meta-text :prefix prefix-state :face meta-face))
    (disco-ins-insert-media-status-line
     status :prefix prefix-state :face meta-face)
    (when (functionp body-inserter)
      (funcall body-inserter prefix-state))
    (when properties
      (add-text-properties card-start (point) properties))
    (when context
      (add-text-properties
       card-start (point)
       (list disco-media-card-context-property context)))
    (cons card-start (point))))

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
  "Insert compact transfer status for ATTACHMENT.

ACTION-FACE, KIND, and ALLOW-PLAY remain accepted for source compatibility.
Open/play/download/save actions now live on the card context and its transient;
only meaningful transfer state remains inline."
  (ignore action-face kind allow-play)
  (disco-ins-insert-media-status-line
   (disco-ins-media-transfer-status-text
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
         (meta-parts
          (delq nil
                (list (disco-media-attachment-content-type-label attachment)
                      (disco-media-attachment-dimensions-label attachment)
                      (when (disco-media-attachment-ephemeral-p attachment)
                        "ephemeral"))))
         (context (disco-media-attachment-card-context attachment)))
    (when spoiler-hidden
      (setq context (plist-put context :open-action nil)))
    (disco-ins-insert-media-card
     :kind kind
     :title name
     :details details
     :meta meta-parts
     :status (disco-ins-media-transfer-status-text
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
            :face 'shadow)))))))

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
    (disco-ins-insert-media-card
     :kind 'photo
     :title name
     :meta meta-parts
     :status (disco-ins-media-transfer-status-text
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
            :face 'shadow)))))))

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
    (disco-ins-insert-media-card
     :kind 'video
     :title name
     :details details
     :status (disco-ins-media-transfer-status-text
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
            :face 'shadow)))))))

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
         (waveform-text (unless (disco-media-image-object-valid-p waveform-image)
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
    (disco-ins--insert-prefixed-line
     (format "%s %s%s"
             (disco-ins--attachment-audio-tag attachment)
             name
             (disco-ins--attachment-detail-text details))
     :prefix prefix-state
     :face title-face
     :action (plist-get context :open-action)
     :help-echo play-help)
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
        (let ((playback-start (point)))
          (disco-ui-insert-action-button
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
             ((disco-media-image-object-valid-p waveform-image)
              (insert-image waveform-image "[waveform]")
              (disco-media-add-action-properties
               wave-start (point)
               (lambda (&optional _event)
                 (interactive)
                 (disco-media-play-attachment-audio attachment))
               play-help))
             ((and (stringp waveform-text) (not (string-empty-p waveform-text)))
              (insert waveform-text)
              (disco-media-add-action-properties
               wave-start (point)
               (lambda (&optional _event)
                 (interactive)
                 (disco-media-play-attachment-audio attachment))
               play-help))))
          (when progress-label
            (insert "  " progress-label))
          (when (and inline-playback-p (or playing-p paused-at progress))
            (insert " ")
            (disco-ui-insert-action-button
             "[Stop]"
             (lambda ()
               (disco-media-stop-attachment-audio attachment))
             :face action-face
             :help-echo "Stop audio playback"))
          (insert "\n")
          (disco-ui-apply-line-prefix playback-start (point) prefix-state)
          (when meta-face
            (disco-ui-append-face playback-start (point) meta-face)))
        (disco-ins-insert-attachment-transfer-line
         attachment :prefix prefix-state :face meta-face :action-face action-face
         :kind 'audio :allow-play nil)
        (disco-ins-insert-attachment-caption-line
         (alist-get 'description attachment) :prefix prefix-state :face meta-face)
        (when show-url
          (disco-ins-insert-attachment-url-line
           (disco-media-attachment-download-url attachment)
           :prefix prefix-state
           :face 'shadow))))
    (add-text-properties
     card-start (point)
     (list disco-media-card-context-property context))
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
