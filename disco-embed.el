;;; disco-embed.el --- Embed rendering component for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Telega-inspired embed rendering extracted from room timeline logic.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'browse-url)
(require 'disco-ui)
(require 'disco-util)

(declare-function disco-room--attachment-preview-url "disco-room" (attachment))
(declare-function disco-room--attachment-download-url "disco-room" (attachment))
(declare-function disco-room--attachment-preview-cache-key "disco-room" (attachment))
(declare-function disco-room--attachment-preview-cache-existing-file "disco-room" (cache-key))
(declare-function disco-room--attachment-preview-image "disco-room" (attachment &optional for-display))
(declare-function disco-room--inline-image-rendering-available-p "disco-room" ())
(declare-function disco-room--image-object-valid-p "disco-room" (image))

(defvar disco-room-show-embeds)
(defvar disco-room-use-rich-embed-cards)
(defvar disco-room-show-embed-urls)
(defvar disco-room-show-embed-image-previews)
(defvar disco-room-show-embed-author-icons)
(defvar disco-room-embed-author-icon-size)
(defvar disco-room-embed-description-limit)

(defun disco-embed--url-present-p (url)
  "Return non-nil when URL is a non-empty string."
  (and (stringp url) (not (string-empty-p url))))

(defun disco-embed--normalize-text (value)
  "Normalize VALUE into trimmed non-empty string, or nil."
  (let ((text (and value (string-trim (format "%s" value)))))
    (when (and (stringp text) (not (string-empty-p text)))
      text)))

(defun disco-embed--truncate-text (text limit)
  "Truncate TEXT to LIMIT characters with ellipsis when needed."
  (cond
   ((not (and (stringp text) (not (string-empty-p text))))
    nil)
   ((eq limit 0)
    nil)
   ((and (integerp limit)
         (> limit 0)
         (> (length text) limit))
    (concat (substring text 0 limit) "..."))
   (t text)))

(defun disco-embed--type (embed)
  "Return short embed type string for EMBED object."
  (let ((raw (disco-util-object-get embed 'type)))
    (if (and (stringp raw) (not (string-empty-p raw)))
        (downcase raw)
      "rich")))

(defun disco-embed--summary (embed)
  "Return one-line embed summary string for EMBED object."
  (let* ((author (disco-util-object-get embed 'author))
         (provider (disco-util-object-get embed 'provider))
         (title (or (disco-util-object-get embed 'title)
                    (and (listp author) (disco-util-object-get author 'name))
                    (and (listp provider) (disco-util-object-get provider 'name))
                    (disco-util-object-get embed 'description)
                    (disco-util-object-get embed 'url)
                    "embed"))
         (headline (string-trim
                    (replace-regexp-in-string "[\n\r\t]+" " "
                                              (format "%s" title)))))
    (format "[%s] %s"
            (disco-embed--type embed)
            (truncate-string-to-width headline 92 nil nil t))))

(defun disco-embed--color-hex (embed)
  "Return hexadecimal color string for EMBED, or nil."
  (let ((value (disco-util-object-get embed 'color)))
    (cond
     ((and (integerp value) (>= value 0) (<= value #xFFFFFF))
      (format "#%06x" value))
     ((and (stringp value)
           (string-match "\\`#?\\([0-9A-Fa-f]\\{6\\}\\)\\'" value))
      (concat "#" (downcase (match-string 1 value))))
     (t nil))))

(defun disco-embed--meta-line (embed)
  "Return compact metadata line for EMBED object."
  (let* ((provider (disco-util-object-get embed 'provider))
         (author (disco-util-object-get embed 'author))
         (provider-name (and (listp provider) (disco-util-object-get provider 'name)))
         (author-name (and (listp author) (disco-util-object-get author 'name)))
         (timestamp (disco-util-object-get embed 'timestamp))
         (timestamp-text (and (stringp timestamp)
                              (not (string-empty-p timestamp))
                              (format "time=%s" (disco-util-format-time timestamp))))
         (fields (or (disco-util-object-get embed 'fields) '()))
         (color (disco-embed--color-hex embed))
         (parts (delq nil
                      (list (format "type=%s" (disco-embed--type embed))
                            (and (stringp provider-name)
                                 (not (string-empty-p provider-name))
                                 (format "provider=%s" provider-name))
                            (and (stringp author-name)
                                 (not (string-empty-p author-name))
                                 (format "author=%s" author-name))
                            (and (> (length fields) 0)
                                 (format "fields=%d" (length fields)))
                            timestamp-text
                            (and color (format "color=%s" color))))))
    (if parts
        (mapconcat #'identity parts "  ")
      "type=rich")))

(defun disco-embed--message-attachment-by-filename (msg filename)
  "Return attachment from MSG matching FILENAME, or nil."
  (when (and (stringp filename) (not (string-empty-p filename)))
    (seq-find
     (lambda (attachment)
       (equal (disco-util-object-get attachment 'filename) filename))
     (or (alist-get 'attachments msg) '()))))

(defun disco-embed--resolve-attachment-scheme-url (msg url)
  "Resolve attachment:// URL in URL using attachments from MSG."
  (cond
   ((not (and (stringp url) (not (string-empty-p url))))
    nil)
   ((string-match "\\`attachment://\\(.+\\)\\'" url)
    (let* ((filename (match-string 1 url))
           (attachment (disco-embed--message-attachment-by-filename msg filename)))
      (or (and attachment (disco-room--attachment-preview-url attachment))
          (and attachment (disco-room--attachment-download-url attachment))
          (and attachment url))))
   (t url)))

(defun disco-embed--author-object (embed)
  "Return author object for EMBED, or nil."
  (let ((author (disco-util-object-get embed 'author)))
    (and (consp author) author)))

(defun disco-embed--provider-object (embed)
  "Return provider object for EMBED, or nil."
  (let ((provider (disco-util-object-get embed 'provider)))
    (and (consp provider) provider)))

(defun disco-embed--author-url (msg embed)
  "Return author URL for EMBED in MSG, if available."
  (let* ((author (disco-embed--author-object embed))
         (raw-url (and author (disco-util-object-get author 'url))))
    (disco-embed--resolve-attachment-scheme-url msg raw-url)))

(defun disco-embed--provider-url (msg embed)
  "Return provider URL for EMBED in MSG, if available."
  (let* ((provider (disco-embed--provider-object embed))
         (raw-url (and provider (disco-util-object-get provider 'url))))
    (disco-embed--resolve-attachment-scheme-url msg raw-url)))

(defun disco-embed--author-icon-url (msg embed)
  "Return author icon URL for EMBED in MSG, if available."
  (let* ((author (disco-embed--author-object embed))
         (raw-url (and author
                       (or (disco-util-object-get author 'proxy_icon_url 'proxyIconUrl)
                           (disco-util-object-get author 'icon_url 'iconUrl)
                           (disco-util-object-get author 'icon_canonical_url 'iconCanonicalUrl)))))
    (disco-embed--resolve-attachment-scheme-url msg raw-url)))

(defun disco-embed--author-icon-attachment (msg embed embed-index)
  "Build pseudo attachment object for EMBED author icon in MSG."
  (let* ((icon-url (disco-embed--author-icon-url msg embed))
         (message-id (format "%s" (or (alist-get 'id msg) "unknown")))
         (size (max 8
                    (if (numberp disco-room-embed-author-icon-size)
                        disco-room-embed-author-icon-size
                      18))))
    (when (disco-embed--url-present-p icon-url)
      `((id . ,(format "embed-author-icon:%s:%s:%s:%s"
                       message-id
                       embed-index
                       size
                       (md5 icon-url)))
        (filename . ,(format "embed-author-%s-icon" embed-index))
        (content_type . "image/embed")
        (url . ,icon-url)
        (proxy_url . ,icon-url)
        (width . ,size)
        (height . ,size)))))

(defun disco-embed--author-icon-image (msg embed embed-index)
  "Return inline author icon image for EMBED in MSG, or nil while loading."
  (when (and disco-room-show-embed-author-icons
             (disco-room--inline-image-rendering-available-p))
    (let* ((size (max 8
                      (if (numberp disco-room-embed-author-icon-size)
                          disco-room-embed-author-icon-size
                        18)))
           (attachment (disco-embed--author-icon-attachment msg embed embed-index))
           image)
      (when attachment
        ;; Reuse attachment preview fetch/cache pipeline for author icons.
        (disco-room--attachment-preview-image attachment t)
        (let* ((cache-key (disco-room--attachment-preview-cache-key attachment))
               (cache-file (and cache-key
                                (disco-room--attachment-preview-cache-existing-file cache-key))))
          (when cache-file
            (setq image
                  (ignore-errors
                    (create-image cache-file nil nil
                                  :width size
                                  :height size
                                  :ascent 'center)))
            (unless (disco-room--image-object-valid-p image)
              (when (image-type-available-p 'imagemagick)
                (setq image
                      (ignore-errors
                        (create-image cache-file 'imagemagick nil
                                      :width size
                                      :height size
                                      :ascent 'center)))))
            (when (disco-room--image-object-valid-p image)
              image)))))))

(defun disco-embed--main-url (msg embed)
  "Return primary URL for EMBED in MSG, resolving attachment:// links."
  (or (disco-embed--resolve-attachment-scheme-url
       msg
       (or (disco-util-object-get embed 'url)
           (disco-util-object-get embed 'canonical_url 'canonicalUrl)))
      (disco-embed--author-url msg embed)
      (disco-embed--provider-url msg embed)))

(defun disco-embed--media-entry (embed)
  "Return media entry cons for EMBED as (KIND . OBJECT), or nil."
  (let ((image (disco-util-object-get embed 'image))
        (thumbnail (disco-util-object-get embed 'thumbnail))
        (video (disco-util-object-get embed 'video))
        (images (disco-util-object-get embed 'images)))
    (cond
     ((consp image) (cons 'image image))
     ((and (listp images) (consp (car images))) (cons 'image (car images)))
     ((and (vectorp images)
           (> (length images) 0)
           (consp (aref images 0)))
      (cons 'image (aref images 0)))
     ((consp thumbnail) (cons 'thumbnail thumbnail))
     ((consp video) (cons 'video video))
     ((equal (disco-embed--type embed) "image") (cons 'image nil))
     (t nil))))

(defun disco-embed--media-url (msg embed)
  "Return best media URL for EMBED in MSG, resolving attachment:// links."
  (let* ((media-entry (disco-embed--media-entry embed))
         (media (cdr media-entry))
         (proxy-url (and (listp media)
                         (or (disco-util-object-get media 'proxy_url)
                             (disco-util-object-get media 'proxyUrl))))
         (source-url (and (listp media)
                          (or (disco-util-object-get media 'url)
                              (disco-util-object-get media 'canonical_url 'canonicalUrl))))
         (resolved-proxy (disco-embed--resolve-attachment-scheme-url msg proxy-url))
         (resolved-source (disco-embed--resolve-attachment-scheme-url msg source-url))
         (main-url (disco-embed--main-url msg embed)))
    (or (and (disco-embed--url-present-p resolved-proxy)
             resolved-proxy)
        (and (disco-embed--url-present-p resolved-source)
             resolved-source)
        (and (equal (disco-embed--type embed) "image")
             (disco-embed--url-present-p main-url)
             main-url))))

(defun disco-embed--preview-attachment (msg embed embed-index)
  "Build pseudo attachment object used to render EMBED preview from MSG."
  (let* ((media-entry (disco-embed--media-entry embed))
         (media-kind (car media-entry))
         (media (cdr media-entry))
         (media-url (disco-embed--media-url msg embed))
         (message-id (format "%s" (or (alist-get 'id msg) "unknown")))
         (cache-suffix (md5 (or media-url ""))))
    (when (and media-url (memq media-kind '(image thumbnail)))
      `((id . ,(format "embed:%s:%s:%s:%s"
                       message-id
                       embed-index
                       (symbol-name media-kind)
                       cache-suffix))
        (filename . ,(format "embed-%s-%s"
                             embed-index
                             (symbol-name media-kind)))
        (content_type . "image/embed")
        (url . ,media-url)
        (proxy_url . ,media-url)
        (width . ,(and (listp media) (disco-util-object-get media 'width)))
        (height . ,(and (listp media) (disco-util-object-get media 'height)))))))

(defun disco-embed--description-line (embed)
  "Return one-line embed description for EMBED, respecting user limit."
  (let* ((raw (disco-util-object-get embed 'description))
         (flat (and raw
                    (replace-regexp-in-string "[\n\r\t]+" " "
                                              (format "%s" raw))))
         (text (disco-embed--normalize-text flat)))
    (disco-embed--truncate-text text disco-room-embed-description-limit)))

(defun disco-embed--insert-action-button (label callback help-echo)
  "Insert one compact action button."
  (disco-ui-insert-action-button
   label
   callback
   :face 'disco-room-embed-card-action
   :help-echo help-echo))

(defun disco-embed--insert-field-row (field)
  "Insert one field row for FIELD object."
  (let* ((name (disco-embed--normalize-text
                (disco-util-object-get field 'name)))
         (raw-value (disco-util-object-get field 'value))
         (value (disco-embed--normalize-text raw-value))
         (inline (disco-util-json-true-p (disco-util-object-get field 'inline))))
    (when (or name value)
      (let ((field-start (point)))
        (insert "    | ")
        (insert (if name name "(unnamed field)"))
        (when inline
          (insert " [inline]"))
        (when value
          (insert ": ")
          (insert value))
        (insert "\n")
        (add-text-properties field-start (point)
                             '(face disco-room-embed-card-meta))))))

(defun disco-embed--insert-preview-row (msg embed embed-index media-kind media-url)
  "Insert media preview row for EMBED in MSG."
  (let* ((preview-rendering-available
          (and disco-room-show-embed-image-previews
               (disco-room--inline-image-rendering-available-p)))
         (preview-attachment (disco-embed--preview-attachment msg embed embed-index))
         (preview (and preview-attachment
                       preview-rendering-available
                       (disco-room--attachment-preview-image preview-attachment t)))
         (preview-start (point)))
    (insert "    | ")
    (cond
     ((memq media-kind '(image thumbnail))
      (if preview
          (condition-case _
              (insert-image preview "[image]")
            (error
             (insert "[image unavailable]")))
        (cond
         ((not preview-rendering-available)
          (insert "[preview disabled]"))
         ((not (disco-embed--url-present-p media-url))
          (insert "[no preview URL]"))
         (t
          (insert "[loading preview]")))))
     ((eq media-kind 'video)
      (insert "[video embed]"))
     (t
      (insert "[no preview]")))
    (insert "\n")
    (add-text-properties preview-start (point)
                         '(face disco-room-embed-card-meta))))

(defun disco-embed--insert-action-row (main-url media-url author-url provider-url
                                                author-icon-url)
  "Insert compact action buttons row for one embed."
  (let ((actions '()))
    (when (disco-embed--url-present-p main-url)
      (push (list "[Open]"
                  (lambda () (browse-url main-url t))
                  "Open embed URL")
            actions)
      (push (list "[Copy]"
                  (lambda ()
                    (kill-new main-url)
                    (message "disco: copied embed URL"))
                  "Copy embed URL")
            actions))
    (when (and (disco-embed--url-present-p media-url)
               (not (equal media-url main-url)))
      (push (list "[Media]"
                  (lambda () (browse-url media-url t))
                  "Open embed media URL")
            actions))
    (when (and (disco-embed--url-present-p author-url)
               (not (equal author-url main-url)))
      (push (list "[Author]"
                  (lambda () (browse-url author-url t))
                  "Open embed author URL")
            actions))
    (when (and (disco-embed--url-present-p provider-url)
               (not (equal provider-url main-url))
               (not (equal provider-url author-url)))
      (push (list "[Provider]"
                  (lambda () (browse-url provider-url t))
                  "Open embed provider URL")
            actions))
    (when (and (disco-embed--url-present-p author-icon-url)
               (not (equal author-icon-url main-url)))
      (push (list "[Icon]"
                  (lambda () (browse-url author-icon-url t))
                  "Open embed author icon URL")
            actions))
    (when actions
      (let ((action-start (point))
            (first t))
        (insert "    | ")
        (dolist (action (nreverse actions))
          (unless first
            (insert " "))
          (setq first nil)
          (disco-embed--insert-action-button
           (nth 0 action)
           (nth 1 action)
           (nth 2 action)))
        (insert "\n")
        (add-text-properties action-start (point)
                             '(face disco-room-embed-card-meta))))))

(defun disco-embed-insert-card (msg embed embed-index)
  "Insert one telega-inspired rich embed card for EMBED from MSG."
  (let* ((summary (disco-embed--summary embed))
         (meta-fallback (disco-embed--meta-line embed))
         (description (disco-embed--description-line embed))
         (fields (or (disco-util-object-get embed 'fields) '()))
         (author (disco-embed--author-object embed))
         (author-name (and (listp author)
                           (disco-embed--normalize-text
                            (disco-util-object-get author 'name))))
         (author-url (disco-embed--author-url msg embed))
         (author-icon-url (disco-embed--author-icon-url msg embed))
         (author-icon-image (disco-embed--author-icon-image msg embed embed-index))
         (provider (disco-embed--provider-object embed))
         (provider-name (and (listp provider)
                             (disco-embed--normalize-text
                              (disco-util-object-get provider 'name))))
         (provider-url (disco-embed--provider-url msg embed))
         (main-url (disco-embed--main-url msg embed))
         (timestamp (disco-util-object-get embed 'timestamp))
         (timestamp-text (and (stringp timestamp)
                              (not (string-empty-p timestamp))
                              (disco-util-format-time timestamp)))
         (footer (disco-util-object-get embed 'footer))
         (footer-text (and (listp footer)
                           (disco-embed--normalize-text
                            (disco-util-object-get footer 'text))))
         (footer-line (let ((parts (delq nil (list footer-text timestamp-text))))
                        (and parts (string-join parts "  -  "))))
         (media-entry (disco-embed--media-entry embed))
         (media-kind (car media-entry))
         (media (cdr media-entry))
         (media-url (disco-embed--media-url msg embed))
         (media-width (and (listp media) (disco-util-object-get media 'width)))
         (media-height (and (listp media) (disco-util-object-get media 'height)))
         (media-dims (when (and (numberp media-width) (numberp media-height))
                       (format "%dx%d" media-width media-height)))
         (meta-parts (delq nil
                           (list provider-name
                                 (and author-name
                                      (not (equal author-name provider-name))
                                      author-name)
                                 timestamp-text))))
    (let ((top-start (point)))
      (insert "    +-- " summary "\n")
      (add-text-properties top-start (point)
                           '(face disco-room-embed-card-title)))
    (let ((meta-start (point)))
      (insert "    | ")
      (when author-icon-image
        (condition-case _
            (insert-image author-icon-image "[icon]")
          (error
           (insert "[icon]")))
        (insert " "))
      (insert (if meta-parts
                  (string-join meta-parts "  -  ")
                meta-fallback))
      (when media-dims
        (insert "  [" media-dims "]"))
      (insert "\n")
      (add-text-properties meta-start (point)
                           '(face disco-room-embed-card-meta)))
    (when description
      (let ((desc-start (point)))
        (insert "    | " description "\n")
        (add-text-properties desc-start (point)
                             '(face disco-room-embed-card-meta))))
    (when media-entry
      (disco-embed--insert-preview-row msg embed embed-index media-kind media-url))
    (dolist (field fields)
      (disco-embed--insert-field-row field))
    (when footer-line
      (let ((footer-start (point)))
        (insert "    | " footer-line "\n")
        (add-text-properties footer-start (point)
                             '(face disco-room-embed-card-meta))))
    (disco-embed--insert-action-row main-url media-url author-url provider-url
                                    author-icon-url)
    (let ((bottom-start (point)))
      (insert "    +--\n")
      (add-text-properties bottom-start (point)
                           '(face disco-room-embed-card-border)))
    (when disco-room-show-embed-urls
      (dolist (raw-url (delete-dups (delq nil (list main-url media-url author-url
                                                    provider-url author-icon-url))))
        (when (disco-embed--url-present-p raw-url)
          (let ((url-start (point)))
            (insert "      " raw-url "\n")
            (add-text-properties url-start (point) '(face shadow))))))))

(defun disco-embed-insert-message-embeds (msg)
  "Insert embed detail lines for MSG."
  (when disco-room-show-embeds
    (let ((embed-index 0))
      (dolist (embed (or (alist-get 'embeds msg) '()))
        (setq embed-index (1+ embed-index))
        (condition-case _
            (if disco-room-use-rich-embed-cards
                (disco-embed-insert-card msg embed embed-index)
              (let* ((line-start (point))
                     (url (disco-embed--main-url msg embed)))
                (insert "    " (disco-embed--summary embed) "\n")
                (add-text-properties line-start (point) '(face disco-room-message-meta))
                (when (and disco-room-show-embed-urls
                           (disco-embed--url-present-p url))
                  (let ((url-start (point)))
                    (insert "      " url "\n")
                    (add-text-properties url-start (point) '(face shadow))))))
          (error
           (let ((line-start (point)))
             (insert "    [embed] [render fallback]\n")
             (add-text-properties line-start (point) '(face shadow)))))))))

(provide 'disco-embed)

;;; disco-embed.el ends here
