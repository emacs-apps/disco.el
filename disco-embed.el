;;; disco-embed.el --- Embed rendering component for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Telega-inspired embed rendering extracted from room timeline logic.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'browse-url)
(require 'color)
(require 'disco-ui)
(require 'disco-util)
(require 'disco-markdown)
(require 'disco-media)

(defvar disco-room-show-embeds)
(defvar disco-room-use-rich-embed-cards)
(defvar disco-room-show-embed-urls)
(defvar disco-room-show-embed-image-previews)
(defvar disco-room-show-embed-author-icons)
(defvar disco-room-embed-author-icon-size)
(defvar disco-room-embed-description-limit)
(defvar disco-room-attachment-preview-max-width)
(defvar disco-room-attachment-preview-max-height)
(defvar disco-ui-card-indent-prefix)
(defvar disco-ui-card-indent-prefix-state)

(defun disco-embed--url-present-p (url)
  "Return non-nil when URL is a non-empty string."
  (and (stringp url) (not (string-empty-p url))))

(defun disco-embed--stringify (value)
  "Return VALUE as display string, or nil when VALUE is nil.

Keeps original line breaks and applies markdown renderer pipeline."
  (and value
       (disco-markdown-render (format "%s" value)
                              :context 'embed-text)))

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
  "Return embed summary string for EMBED object."
  (let* ((author (disco-util-object-get embed 'author))
         (provider (disco-util-object-get embed 'provider))
         (title (or (disco-util-object-get embed 'title)
                    (and (listp author) (disco-util-object-get author 'name))
                    (and (listp provider) (disco-util-object-get provider 'name))
                    (disco-util-object-get embed 'description)
                    (disco-util-object-get embed 'url)
                    "embed"))
         (headline (or (disco-embed--stringify title) "embed")))
    (format "[%s] %s"
            (disco-embed--type embed)
            headline)))

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

(defun disco-embed--accent-rgb (embed)
  "Return EMBED color as RGB triple, or nil."
  (when-let* ((hex (disco-embed--color-hex embed)))
    (ignore-errors (color-name-to-rgb hex))))

(defun disco-embed--rgb-triple-p (value)
  "Return non-nil if VALUE is an RGB triple of numeric channels."
  (and (listp value)
       (= (length value) 3)
       (numberp (nth 0 value))
       (numberp (nth 1 value))
       (numberp (nth 2 value))))

(defun disco-embed--default-background-rgb ()
  "Return current default face background as RGB triple, or nil."
  (let ((bg (face-background 'default nil t)))
    (when (and (stringp bg)
               (not (member bg '("unspecified" "unspecified-bg"))))
      (ignore-errors (color-name-to-rgb bg)))))

(defun disco-embed--blend-rgb (fg-rgb bg-rgb alpha)
  "Blend FG-RGB over BG-RGB with ALPHA and return RGB triple."
  (when (and (disco-embed--rgb-triple-p fg-rgb)
             (disco-embed--rgb-triple-p bg-rgb))
    (list (+ (* alpha (nth 0 fg-rgb)) (* (- 1 alpha) (nth 0 bg-rgb)))
          (+ (* alpha (nth 1 fg-rgb)) (* (- 1 alpha) (nth 1 bg-rgb)))
          (+ (* alpha (nth 2 fg-rgb)) (* (- 1 alpha) (nth 2 bg-rgb))))))

(defun disco-embed--background-face (embed)
  "Return subtle background face plist for EMBED, or nil."
  (let* ((accent (disco-embed--accent-rgb embed))
         (default-bg (disco-embed--default-background-rgb))
         (blended (disco-embed--blend-rgb accent default-bg 0.10)))
    (when blended
      `(:background ,(apply #'color-rgb-to-hex (append blended '(2))) :extend t))))

(defun disco-embed--accent-face (embed)
  "Return face used for EMBED accent marker."
  (let ((color (disco-embed--color-hex embed)))
    (if color
        `(:foreground ,color :weight bold)
      'disco-room-embed-card-border)))

(defun disco-embed--line-prefix (embed)
  "Return colored visual line prefix state for EMBED card rows."
  (disco-ui-card-prefix-state :face (disco-embed--accent-face embed)))

(defun disco-embed--meta-line (embed)
  "Return compact metadata line for EMBED object."
  (let* ((provider (disco-util-object-get embed 'provider))
         (author (disco-util-object-get embed 'author))
         (provider-name (and (listp provider)
                             (disco-embed--stringify
                              (disco-util-object-get provider 'name))))
         (author-name (and (listp author)
                           (disco-embed--stringify
                            (disco-util-object-get author 'name))))
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
      (or (and attachment (disco-media-attachment-preview-url attachment))
          (and attachment (disco-media-attachment-download-url attachment))
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
             (disco-media-inline-image-rendering-available-p))
    (let* ((size (max 8
                      (if (numberp disco-room-embed-author-icon-size)
                          disco-room-embed-author-icon-size
                        18)))
           (attachment (disco-embed--author-icon-attachment msg embed embed-index))
           image)
      (when attachment
        ;; Reuse attachment preview fetch/cache pipeline for author icons.
        (disco-media-attachment-preview-image attachment t)
        (let* ((cache-key (disco-media-attachment-preview-cache-key attachment))
               (cache-file (and cache-key
                                (disco-media-attachment-preview-cache-existing-file cache-key))))
          (when cache-file
            (setq image
                  (ignore-errors
                    (create-image cache-file nil nil
                                  :width size
                                  :height size
                                  :ascent 'center)))
            (unless (disco-media-image-object-valid-p image)
              (when (image-type-available-p 'imagemagick)
                (setq image
                      (ignore-errors
                        (create-image cache-file 'imagemagick nil
                                      :width size
                                      :height size
                                      :ascent 'center)))))
            (when (disco-media-image-object-valid-p image)
              image)))))))

(defun disco-embed--main-url (msg embed)
  "Return primary URL for EMBED in MSG, resolving attachment:// links."
  (or (disco-embed--resolve-attachment-scheme-url
       msg
       (or (disco-util-object-get embed 'url)
           (disco-util-object-get embed 'canonical_url 'canonicalUrl)))
      (disco-embed--author-url msg embed)
      (disco-embed--provider-url msg embed)))

(defun disco-embed--embed-url-key (embed)
  "Return normalized URL key for EMBED dedup/grouping logic."
  (let ((raw-url (or (disco-util-object-get embed 'url)
                     (disco-util-object-get embed 'canonical_url 'canonicalUrl))))
    (and (disco-embed--url-present-p raw-url)
         (string-trim raw-url))))

(defun disco-embed--image-object-key (image)
  "Return stable de-dup key for IMAGE object."
  (let ((raw-url (or (disco-util-object-get image 'proxy_url 'proxyUrl)
                     (disco-util-object-get image 'url)
                     (disco-util-object-get image 'canonical_url 'canonicalUrl))))
    (or (and (disco-embed--url-present-p raw-url)
             (string-trim raw-url))
        (format "raw:%s" (sxhash image)))))

(defun disco-embed--dedupe-image-objects (images)
  "Return IMAGES without duplicate URL/image entries."
  (let ((seen (make-hash-table :test #'equal))
        (result '()))
    (dolist (image images)
      (when (consp image)
        (let ((key (disco-embed--image-object-key image)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push image result)))))
    (nreverse result)))

(defun disco-embed--embed-image-objects (embed)
  "Return all image objects for EMBED, including merged `images' entries."
  (let* ((single (disco-util-object-get embed 'image))
         (images (disco-util-object-get embed 'images))
         (image-list (cond
                      ((vectorp images) (append images nil))
                      ((listp images) images)
                      (t nil)))
         (all-images (append (and (consp single) (list single)) image-list)))
    (disco-embed--dedupe-image-objects all-images)))

(defun disco-embed--image-object-url (msg image)
  "Return best resolved URL for IMAGE object in MSG."
  (let* ((proxy-url (disco-util-object-get image 'proxy_url 'proxyUrl))
         (source-url (or (disco-util-object-get image 'url)
                         (disco-util-object-get image 'canonical_url 'canonicalUrl)))
         (resolved-proxy (disco-embed--resolve-attachment-scheme-url msg proxy-url))
         (resolved-source (disco-embed--resolve-attachment-scheme-url msg source-url)))
    (or (and (disco-embed--url-present-p resolved-proxy)
             resolved-proxy)
        (and (disco-embed--url-present-p resolved-source)
             resolved-source))))

(defun disco-embed--embed-with-images (embed images)
  "Return EMBED cloned with normalized IMAGES list."
  (if (listp embed)
      (let* ((deduped-images (disco-embed--dedupe-image-objects images))
             (cleaned-embed
              (seq-remove
               (lambda (entry)
                 (let ((key (car-safe entry)))
                   (or (eq key 'images)
                       (eq key :images)
                       (and (stringp key)
                            (string= key "images")))))
               (copy-tree embed))))
        (cons (cons 'images deduped-images) cleaned-embed))
    embed))

(defun disco-embed--seq-empty-p (value)
  "Return non-nil when VALUE is nil or an empty list/vector."
  (or (null value)
      (and (listp value) (null value))
      (and (vectorp value) (= (length value) 0))))

(defun disco-embed--trailing-image-embed-p (embed url-key)
  "Return non-nil when EMBED is a mergeable trailing image-only embed."
  (let* ((description (disco-util-object-get embed 'description))
         (description-present
          (and description
               (not (and (stringp description)
                         (string-empty-p description)))))
         (fields (disco-util-object-get embed 'fields))
         (images (disco-embed--embed-image-objects embed)))
    (and (disco-embed--url-present-p url-key)
         (equal (disco-embed--embed-url-key embed) url-key)
         (null (disco-util-object-get embed 'timestamp))
         (null (disco-util-object-get embed 'author))
         (null (disco-util-object-get embed 'color))
         (not description-present)
         (disco-embed--seq-empty-p fields)
         (null (disco-util-object-get embed 'thumbnail))
         (null (disco-util-object-get embed 'video))
         (null (disco-util-object-get embed 'footer))
         (= (length images) 1))))

(defun disco-embed--normalize-embeds (embeds)
  "Normalize EMBEDS to match Discord client multi-image embed behavior."
  (let* ((embed-list (cond
                      ((vectorp embeds) (append embeds nil))
                      ((listp embeds) embeds)
                      (t nil)))
         (result '())
         (index 0)
         (count (length embed-list)))
    (while (< index count)
      (let* ((embed (nth index embed-list))
             (base-url (and embed (disco-embed--embed-url-key embed)))
             (base-images (and embed (disco-embed--embed-image-objects embed)))
             (merged-images base-images)
             (next-index (1+ index))
             (merged nil))
        (when (and (disco-embed--url-present-p base-url)
                   (not (null base-images)))
          ;; Discord splits multi-image rich embeds into same-URL trailing image embeds.
          (while (and (< next-index count)
                      (disco-embed--trailing-image-embed-p
                       (nth next-index embed-list)
                       base-url))
            (setq merged t)
            (setq merged-images
                  (append merged-images
                          (disco-embed--embed-image-objects
                           (nth next-index embed-list))))
            (setq next-index (1+ next-index))))
        (push (if merged
                  (disco-embed--embed-with-images embed merged-images)
                embed)
              result)
        (setq index next-index)))
    (nreverse result)))

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

(defun disco-embed--video-url (msg embed)
  "Return best video URL for EMBED in MSG, resolving attachment:// links."
  (let* ((video (disco-util-object-get embed 'video))
         (proxy-url (and (listp video)
                         (or (disco-util-object-get video 'proxy_url)
                             (disco-util-object-get video 'proxyUrl))))
         (source-url (and (listp video)
                          (or (disco-util-object-get video 'url)
                              (disco-util-object-get video 'canonical_url 'canonicalUrl))))
         (resolved-proxy (disco-embed--resolve-attachment-scheme-url msg proxy-url))
         (resolved-source (disco-embed--resolve-attachment-scheme-url msg source-url)))
    (or (and (disco-embed--url-present-p resolved-proxy)
             resolved-proxy)
        (and (disco-embed--url-present-p resolved-source)
             resolved-source))))

(defun disco-embed--preview-attachment (msg embed embed-index)
  "Build pseudo attachment object used to render EMBED preview from MSG."
  (let* ((media-entry (disco-embed--media-entry embed))
         (media-kind (car media-entry))
         (media (cdr media-entry))
         (media-url (disco-embed--media-url msg embed))
         (message-id (format "%s" (or (alist-get 'id msg) "unknown")))
         (cache-suffix (md5 (or media-url ""))))
    (when (and media-url (memq media-kind '(image thumbnail video)))
      `((id . ,(format "embed:%s:%s:%s:%s"
                       message-id
                       embed-index
                       (symbol-name media-kind)
                       cache-suffix))
        (filename . ,(format "embed-%s-%s"
                             embed-index
                             (symbol-name media-kind)))
        (content_type . ,(if (eq media-kind 'video)
                             "video/embed"
                           "image/embed"))
        (url . ,media-url)
        (proxy_url . ,media-url)
        (width . ,(and (listp media) (disco-util-object-get media 'width)))
        (height . ,(and (listp media) (disco-util-object-get media 'height)))))))

(defun disco-embed--image-preview-attachment (msg embed-index image-index image image-url)
  "Build pseudo attachment object used to render one embed IMAGE preview."
  (let* ((message-id (format "%s" (or (alist-get 'id msg) "unknown")))
         (cache-suffix (md5 (or image-url ""))))
    (when (disco-embed--url-present-p image-url)
      `((id . ,(format "embed:%s:%s:image:%s:%s"
                       message-id
                       embed-index
                       image-index
                       cache-suffix))
        (filename . ,(format "embed-%s-image-%s" embed-index image-index))
        (content_type . "image/embed")
        (url . ,image-url)
        (proxy_url . ,image-url)
        (width . ,(and (listp image) (disco-util-object-get image 'width)))
        (height . ,(and (listp image) (disco-util-object-get image 'height)))))))

(defun disco-embed--description-line (embed)
  "Return embed description for EMBED, preserving original formatting."
  (let ((text (disco-embed--stringify
               (disco-util-object-get embed 'description))))
    (disco-embed--truncate-text text disco-room-embed-description-limit)))

(defun disco-embed--insert-action-button (label callback help-echo)
  "Insert one compact action button."
  (disco-ui-insert-action-button
   label
   callback
   :face 'disco-room-embed-card-action
   :help-echo help-echo))

(defun disco-embed--insert-field-row (field embed prefix-str)
  "Insert one field row for FIELD object."
  (let* ((name (disco-embed--stringify
                (disco-util-object-get field 'name)))
         (value (disco-embed--stringify
                 (disco-util-object-get field 'value)))
         (inline (disco-util-json-true-p (disco-util-object-get field 'inline))))
    (when (or name value)
      (let ((content-start (point)))
        (insert (if name name "(unnamed field)"))
        (when inline
          (insert " [inline]"))
        (when value
          (insert ": ")
          (insert value))
        (insert "\n")
        (disco-ui-apply-line-prefix content-start (point) prefix-str)
        (disco-ui-append-face
         content-start
         (point)
         (disco-ui-combine-faces
          (disco-embed--background-face embed)
          'disco-room-embed-card-meta))))))

(defun disco-embed--preview-max-width ()
  "Return max inline preview width in pixels."
  (max 64
       (if (numberp disco-room-attachment-preview-max-width)
           disco-room-attachment-preview-max-width
         460)))

(defun disco-embed--preview-max-height ()
  "Return max inline preview height in pixels."
  (max 64
       (if (numberp disco-room-attachment-preview-max-height)
           disco-room-attachment-preview-max-height
         360)))

(defun disco-embed--preview-grid-max-width ()
  "Return max per-image width used by multi-image embed grid previews."
  (max 96
       (/ (disco-embed--preview-max-width) 2)))

(defun disco-embed--preview-image-from-file (file max-width)
  "Create inline preview image from FILE constrained by MAX-WIDTH."
  (disco-media-preview-image-from-file
   file
   max-width
   (disco-embed--preview-max-height)))

(defun disco-embed--preview-image-for-attachment (attachment max-width)
  "Return inline preview image for ATTACHMENT with MAX-WIDTH, or nil."
  (let* ((cache-key (disco-media-attachment-preview-cache-key attachment))
         (cache-file (and cache-key
                          (disco-media-attachment-preview-cache-existing-file cache-key))))
    (and cache-file
         (disco-embed--preview-image-from-file cache-file max-width))))

(defun disco-embed--preview-status-label (status)
  "Return compact text label for preview STATUS symbol."
  (pcase status
    ('disabled "[preview disabled]")
    ('missing "[image unavailable]")
    ('no-url "[no preview URL]")
    (_ "[loading preview]")))

(defun disco-embed--insert-grid-preview-row (items embed prefix-str)
  "Insert multi-image preview ITEMS as a two-column text grid."
  (let ((content-start (point))
        (index 0)
        (count (length items)))
    (dolist (item items)
      (let ((image (plist-get item :image))
            (status (plist-get item :status))
            (url (plist-get item :url)))
        (if image
            (condition-case _
                (let ((image-start (point)))
                  (insert-image image "[image]")
                  (disco-media-add-open-url-properties image-start (point) url))
              (error
               (insert "[image unavailable]")))
          (insert (disco-embed--preview-status-label status))))
      (setq index (1+ index))
      (when (< index count)
        (if (= (% index 2) 0)
            (insert "\n")
          (insert " "))))
    (insert "\n")
    (disco-ui-apply-line-prefix content-start (point) prefix-str)
    (disco-ui-append-face
     content-start
     (point)
     (disco-ui-combine-faces
      (disco-embed--background-face embed)
      'disco-room-embed-card-meta))))

(defun disco-embed--insert-preview-row (msg embed embed-index media-kind media-url video-url prefix-str)
  "Insert media preview row for EMBED in MSG."
  (let* ((preview-rendering-available
          (and disco-room-show-embed-image-previews
               (disco-media-inline-image-rendering-available-p)))
         (embed-images (disco-embed--embed-image-objects embed)))
    (if (> (length embed-images) 1)
        (let ((items '())
              (image-index 0)
              (grid-max-width (disco-embed--preview-grid-max-width)))
          (dolist (image embed-images)
            (setq image-index (1+ image-index))
            (let* ((image-url (disco-embed--image-object-url msg image))
                   (status 'loading)
                   (display-image nil)
                   (attachment (and (disco-embed--url-present-p image-url)
                                    (disco-embed--image-preview-attachment
                                     msg
                                     embed-index
                                     image-index
                                     image
                                     image-url))))
              (cond
               ((not preview-rendering-available)
                (setq status 'disabled))
               ((not (disco-embed--url-present-p image-url))
                (setq status 'no-url))
               ((not attachment)
                (setq status 'no-url))
               (t
                (disco-media-attachment-preview-image attachment t)
                (setq display-image
                      (disco-embed--preview-image-for-attachment attachment grid-max-width))
                (let* ((cache-key (disco-media-attachment-preview-cache-key attachment))
                       (cache-state (and cache-key
                                         (disco-media-attachment-preview-cache-state cache-key))))
                  (setq status
                        (cond
                         (display-image 'ready)
                         ((eq cache-state :missing) 'missing)
                         (t 'loading))))))
              (push (list :image display-image :status status :url image-url) items)))
          (disco-embed--insert-grid-preview-row (nreverse items) embed prefix-str))
      (let* ((preview-attachment (disco-embed--preview-attachment msg embed embed-index))
             (preview (and preview-attachment
                           preview-rendering-available
                           (disco-media-attachment-preview-image preview-attachment t)))
             (preview-cache-key (and preview-attachment
                                     (disco-media-attachment-preview-cache-key
                                      preview-attachment)))
             (preview-cache-state (and preview-cache-key
                                       (disco-media-attachment-preview-cache-state
                                        preview-cache-key)))
             (content-start (point))
             (video-preview-p (or (eq media-kind 'video)
                                  (disco-embed--url-present-p video-url)))
             (play-video-url (or video-url media-url))
             (apply-meta-face t))
        (cond
         ((memq media-kind '(image thumbnail video))
          (if preview
              (condition-case _
                  (let ((slice-start (point)))
                    (setq apply-meta-face nil)
                    (disco-media-insert-image-slices
                     preview
                     (unless video-preview-p media-url)
                     nil
                     (if video-preview-p "[video]" "[image]"))
                    (when (and video-preview-p
                               (disco-embed--url-present-p play-video-url))
                      (disco-media-add-play-video-properties
                       slice-start
                       (point)
                       play-video-url)))
                (error
                 (insert (if video-preview-p
                             "[video preview unavailable]"
                           "[image unavailable]"))))
            (cond
             ((not preview-rendering-available)
              (insert "[preview disabled]"))
             ((not (disco-embed--url-present-p media-url))
              (insert "[no preview URL]"))
             ((eq preview-cache-state :missing)
              (insert (if video-preview-p
                          "[video preview unavailable]"
                        "[image unavailable]")))
             (t
              (insert "[loading preview]")))))
         (t
          (insert "[no preview]")))
        (insert "\n")
        (disco-ui-apply-line-prefix content-start (point) prefix-str)
        (disco-ui-append-face
         content-start
         (point)
         (if apply-meta-face
             (disco-ui-combine-faces
              (disco-embed--background-face embed)
              'disco-room-embed-card-meta)
           (disco-embed--background-face embed)))))))

(defun disco-embed--insert-action-row (main-url media-url video-url author-url provider-url
                                                author-icon-url embed prefix-str)
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
    (when (disco-embed--url-present-p video-url)
      (push (list "[Play]"
                  (lambda () (disco-media-play-video-url video-url))
                  "Play embed video")
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
      (let ((content-start (point))
            (first t))
        (dolist (action (nreverse actions))
          (unless first
            (insert " "))
          (setq first nil)
          (disco-embed--insert-action-button
           (nth 0 action)
           (nth 1 action)
           (nth 2 action)))
        (insert "\n")
        (disco-ui-apply-line-prefix content-start (point) prefix-str)
        (disco-ui-append-face
         content-start
         (point)
         (disco-embed--background-face embed))))))

(defun disco-embed-insert-card (msg embed embed-index)
  "Insert one telega-inspired rich embed card for EMBED from MSG."
  (let* ((summary (disco-embed--summary embed))
         (meta-fallback (disco-embed--meta-line embed))
         (description (disco-embed--description-line embed))
         (fields (or (disco-util-object-get embed 'fields) '()))
         (author (disco-embed--author-object embed))
         (author-name (and (listp author)
                           (disco-embed--stringify
                            (disco-util-object-get author 'name))))
         (author-url (disco-embed--author-url msg embed))
         (author-icon-url (disco-embed--author-icon-url msg embed))
         (author-icon-image (disco-embed--author-icon-image msg embed embed-index))
         (provider (disco-embed--provider-object embed))
         (provider-name (and (listp provider)
                             (disco-embed--stringify
                              (disco-util-object-get provider 'name))))
         (provider-url (disco-embed--provider-url msg embed))
         (main-url (disco-embed--main-url msg embed))
         (timestamp (disco-util-object-get embed 'timestamp))
         (timestamp-text (and (stringp timestamp)
                              (not (string-empty-p timestamp))
                              (disco-util-format-time timestamp)))
         (footer (disco-util-object-get embed 'footer))
         (footer-text (and (listp footer)
                           (disco-embed--stringify
                            (disco-util-object-get footer 'text))))
         (footer-line (let ((parts (delq nil (list footer-text timestamp-text))))
                        (and parts (string-join parts "  -  "))))
         (media-entry (disco-embed--media-entry embed))
         (media-kind (car media-entry))
         (media (cdr media-entry))
         (media-url (disco-embed--media-url msg embed))
         (video-url (disco-embed--video-url msg embed))
         (media-width (and (listp media) (disco-util-object-get media 'width)))
         (media-height (and (listp media) (disco-util-object-get media 'height)))
         (media-dims (when (and (numberp media-width) (numberp media-height))
                       (format "%dx%d" media-width media-height)))
         (meta-parts (delq nil
                           (list provider-name
                                 (and author-name
                                      (not (equal author-name provider-name))
                                      author-name)
                                 timestamp-text)))
         (prefix-str (disco-embed--line-prefix embed))
         (accent-color (disco-embed--color-hex embed))
         (card-bg-face (disco-embed--background-face embed))
         (title-face (disco-ui-combine-faces
                      card-bg-face
                      (and accent-color `(:foreground ,accent-color))
                      'disco-room-embed-card-title))
         (meta-face (disco-ui-combine-faces
                     card-bg-face
                     'disco-room-embed-card-meta))
         (shadow-face (disco-ui-combine-faces card-bg-face 'shadow)))
    (let ((title-start (point)))
      (insert summary)
      (when (disco-embed--url-present-p main-url)
        (disco-media-add-open-url-properties title-start (point) main-url))
      (insert "\n")
      (disco-ui-apply-line-prefix title-start (point) prefix-str)
      (disco-ui-append-face title-start (point) title-face))
    (let ((meta-start (point)))
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
      (disco-ui-apply-line-prefix meta-start (point) prefix-str)
      (disco-ui-append-face meta-start (point) meta-face))
    (when description
      (let ((desc-start (point)))
        (insert description "\n")
        (disco-ui-apply-line-prefix desc-start (point) prefix-str)
        (disco-ui-append-face desc-start (point) meta-face)))
    (when media-entry
      (disco-embed--insert-preview-row
       msg embed embed-index media-kind media-url video-url prefix-str))
    (dolist (field fields)
      (disco-embed--insert-field-row field embed prefix-str))
    (when footer-line
      (let ((footer-start (point)))
        (insert footer-line "\n")
        (disco-ui-apply-line-prefix footer-start (point) prefix-str)
        (disco-ui-append-face footer-start (point) meta-face)))
    (disco-embed--insert-action-row
     main-url media-url video-url author-url provider-url author-icon-url embed prefix-str)
    (when disco-room-show-embed-urls
      (dolist (raw-url (delete-dups (delq nil (list main-url media-url author-url
                                                    provider-url author-icon-url))))
        (when (disco-embed--url-present-p raw-url)
          (let ((url-start (point)))
            (insert raw-url "\n")
            (disco-media-add-open-url-properties url-start (1- (point)) raw-url)
            (disco-ui-apply-line-prefix url-start (point) prefix-str)
            (disco-ui-append-face url-start (point) shadow-face)))))))

(defun disco-embed-insert-message-embeds (msg)
  "Insert embed detail lines for MSG."
  (when disco-room-show-embeds
    (let ((embed-index 0)
          (embeds (disco-embed--normalize-embeds (alist-get 'embeds msg))))
      (dolist (embed embeds)
        (setq embed-index (1+ embed-index))
        (condition-case _
            (if disco-room-use-rich-embed-cards
                (disco-embed-insert-card msg embed embed-index)
              (let* ((prefix-source (or disco-ui-card-indent-prefix-state
                                        (or disco-ui-card-indent-prefix "    ")))
                     (line-start (point))
                     (url (disco-embed--main-url msg embed)))
                (insert (disco-embed--summary embed) "\n")
                (disco-ui-apply-line-prefix line-start (point) prefix-source)
                (add-text-properties line-start (point) '(face disco-room-message-meta))
                (when (and disco-room-show-embed-urls
                           (disco-embed--url-present-p url))
                  (let ((url-start (point)))
                    (insert "  " url "\n")
                    (disco-ui-apply-line-prefix url-start (point) prefix-source)
                    (add-text-properties url-start (point) '(face shadow))))))
          (error
           (let* ((prefix-source (or disco-ui-card-indent-prefix-state
                                     (or disco-ui-card-indent-prefix "    ")))
                  (line-start (point)))
             (insert "[embed] [render fallback]\n")
             (disco-ui-apply-line-prefix line-start (point) prefix-source)
             (add-text-properties line-start (point) '(face shadow)))))))))

(provide 'disco-embed)

;;; disco-embed.el ends here
