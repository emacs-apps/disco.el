;;; disco-media.el --- Shared media helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared image/media helpers used by room and embed renderers.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'browse-url)
(require 'url-handlers)
(require 'dom)
(require 'svg nil t)
(require 'plz)

(defvar disco-room-show-attachment-image-previews)
(defvar disco-room-attachment-preview-max-width)
(defvar disco-room-attachment-preview-max-height)
(defvar disco-room-attachment-preview-fetch-concurrency)
(defvar disco-room-attachment-cache-directory)
(defvar disco-room-attachment-download-directory)
(defvar disco-room-video-player-command)

(defconst disco-media--cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "img")
  "Preferred media cache file extension candidates.")

(defconst disco-media--attachment-flag-is-spoiler (ash 1 3)
  "Attachment flag bit marking spoiler attachments.")

(defconst disco-media--spoiler-turbulence-attrs
  '((baseFrequency . "0.1 0.1")
    (numOctaves . "2"))
  "Attributes for spoiler `feTurbulence' SVG node.")

(defconst disco-media--spoiler-displacement-attrs
  '((scale . "80"))
  "Attributes for spoiler `feDisplacementMap' SVG node.")

(defvar disco-media--attachment-preview-image-cache (make-hash-table :test #'equal)
  "Preview image cache keyed by attachment preview cache key.

Values are image objects or the symbol `:missing'.")

(defvar disco-media--attachment-preview-fetching (make-hash-table :test #'equal)
  "Set of attachment preview cache keys currently being fetched.")

(defvar disco-media--attachment-preview-fetch-budget nil
  "Dynamic cap for number of preview fetches started during one render pass.")

(defvar disco-media--attachment-preview-plz-queue nil
  "Shared plz queue used for asynchronous attachment preview downloads.")

(defvar disco-media--attachment-preview-plz-queue-limit nil
  "Last applied queue limit for `disco-media--attachment-preview-plz-queue'.")

(defvar disco-media-preview-rerender-function nil
  "Compatibility callback used after media preview cache updates.")

(defvar disco-media-rerender-function nil
  "Function called after media state/cache updates.")

(defvar disco-media--attachment-download-state-table (make-hash-table :test #'equal)
  "Attachment download state keyed by stable attachment download key.")

(defvar disco-media--attachment-spoiler-preview-cache (make-hash-table :test #'equal)
  "Cache of spoiler-preview image objects keyed by preview cache identity.")

(defvar disco-media--attachment-placeholder-image-cache (make-hash-table :test #'equal)
  "Cache of decoded attachment placeholder image objects.")

(defvar disco-media--attachment-decorated-preview-cache (make-hash-table :test #'equal)
  "Cache of SVG-decorated preview images keyed by source/spec/mode.")

(defun disco-media-clear-preview-memory-cache ()
  "Clear in-memory preview image cache without touching disk files."
  (clrhash disco-media--attachment-preview-image-cache)
  (clrhash disco-media--attachment-preview-fetching)
  (clrhash disco-media--attachment-spoiler-preview-cache)
  (clrhash disco-media--attachment-placeholder-image-cache)
  (clrhash disco-media--attachment-decorated-preview-cache))

(defun disco-media-inline-image-rendering-available-p ()
  "Return non-nil when current frame supports inline image rendering."
  (and (display-images-p)
       (or (image-type-available-p 'png)
           (image-type-available-p 'webp)
           (image-type-available-p 'jpeg)
           (image-type-available-p 'gif)
           (image-type-available-p 'imagemagick))))

(defun disco-media-image-object-valid-p (image)
  "Return non-nil when IMAGE can be rendered by Emacs."
  (and image
       (condition-case _
           (progn
             (image-size image t)
             t)
         (error nil))))

(defun disco-media-url-present-p (url)
  "Return non-nil when URL is a non-empty string."
  (and (stringp url)
       (not (string-empty-p url))))

(defun disco-media--split-command-args (command)
  "Normalize COMMAND into argv list, or nil when unusable."
  (cond
   ((and (listp command)
         command
         (seq-every-p #'stringp command))
    command)
   ((and (stringp command)
         (not (string-empty-p command)))
    (split-string-and-unquote command))
   (t nil)))

(defun disco-media--start-video-player (source)
  "Start configured video player for SOURCE and return non-nil on success."
  (let* ((argv (disco-media--split-command-args disco-room-video-player-command))
         (program (car argv))
         (args (append (cdr argv) (list source))))
    (when (and program
               (executable-find program))
      (condition-case nil
          (progn
            (make-process
             :name "disco-video-player"
             :buffer nil
             :command (cons program args)
             :noquery t)
            t)
        (error nil)))))

(defun disco-media-play-video-url (url)
  "Play video URL using configured player, falling back to browser."
  (when (disco-media-url-present-p url)
    (unless (disco-media--start-video-player url)
      (browse-url url t))))

(defun disco-media-play-video-file (path)
  "Play local video file at PATH using configured player or browser fallback."
  (when (and (stringp path)
             (file-exists-p path))
    (unless (disco-media--start-video-player path)
      (browse-url-of-file path))))

(defun disco-media-add-action-properties (start end callback help-echo)
  "Attach CALLBACK mouse/key handlers to text between START and END."
  (when (and (functionp callback)
             (< start end))
    (let ((action-map (make-sparse-keymap)))
      (define-key action-map [mouse-1] callback)
      (define-key action-map (kbd "RET") callback)
      (add-text-properties
       start
       end
       (list 'keymap action-map
             'mouse-face 'highlight
             'help-echo (or help-echo "Activate"))))))

(defun disco-media-add-open-url-properties (start end url)
  "Attach mouse/key handlers to open URL for text between START and END."
  (when (and (disco-media-url-present-p url)
             (< start end))
    (disco-media-add-action-properties
     start
     end
     (lambda (&optional _event)
       (interactive)
       (browse-url url t))
     (format "Open media: %s" url))))

(defun disco-media-add-play-video-properties (start end video-url)
  "Attach mouse/key handlers to play VIDEO-URL for text START..END."
  (when (and (disco-media-url-present-p video-url)
             (< start end))
    (disco-media-add-action-properties
     start
     end
     (lambda (&optional _event)
       (interactive)
       (disco-media-play-video-url video-url))
     (format "Play video: %s" video-url))))

(defun disco-media-image-slice-count (image)
  "Return line count used to render IMAGE as vertical slices."
  (let* ((props (cdr-safe image))
         (explicit-slices (or (plist-get props :disco-nslices)
                              (plist-get props :telega-nslices)))
         (size (and (disco-media-image-object-valid-p image)
                    (ignore-errors
                      (image-size image nil (selected-frame)))))
         (height (and (consp size) (cdr size))))
    (max 1
         (cond
          ((and (integerp explicit-slices)
                (> explicit-slices 0))
           explicit-slices)
          ((numberp height)
           (round height))
          (t 1)))))

(defun disco-media-insert-slice-newline ()
  "Insert newline between image slices without adding extra line gap."
  (let ((newline-start (point)))
    (insert "\n")
    (add-text-properties newline-start (point)
                         '(line-height t
                           rear-nonsticky (line-height)))))

(defun disco-media-insert-image-slices (image &optional url prefix-str fallback)
  "Insert IMAGE as line slices with optional URL open behavior."
  (let* ((slice-count (disco-media-image-slice-count image))
         (slice-height-px (max 1
                               (or (ignore-errors (line-pixel-height))
                                   (frame-char-height))))
         (label (or fallback "[image]")))
    (dotimes (slice-index slice-count)
      (when (> slice-index 0)
        (disco-media-insert-slice-newline)
        (when prefix-str
          (insert prefix-str)))
      (let ((slice-start (point))
            (slice (list 0
                         (* slice-index slice-height-px)
                         1.0
                         slice-height-px)))
        (insert-image image label nil slice)
        (disco-media-add-open-url-properties slice-start (point) url)))))

(defun disco-media--char-pixel-width ()
  "Return default character width in pixels for current frame."
  (max 1 (frame-char-width)))

(defun disco-media--char-pixel-height ()
  "Return default character height in pixels for current frame."
  (max 1 (frame-char-height)))

(defun disco-media--pixels->chars-width (pixels)
  "Convert PIXELS to character columns using current frame metrics."
  (max 1
       (ceiling (/ (float (max 1 pixels))
                   (float (disco-media--char-pixel-width))))))

(defun disco-media--pixels->chars-height (pixels)
  "Convert PIXELS to text lines using current frame metrics."
  (max 1
       (ceiling (/ (float (max 1 pixels))
                   (float (disco-media--char-pixel-height))))))

(defun disco-media--em-height-ratio ()
  "Return em height ratio for default face in selected frame."
  (let* ((frame (selected-frame))
         (font-name (face-font 'default frame))
         (font-info (and font-name (font-info font-name frame)))
         (font-height (and (vectorp font-info) (aref font-info 3)))
         (font-size (and (vectorp font-info) (aref font-info 2))))
    (if (and (numberp font-height)
             (numberp font-size)
             (> font-size 0))
        (/ (float font-height) font-size)
      1.0)))

(defun disco-media--ch-height-spec (chars)
  "Return image height spec for CHARS text lines."
  (let ((lines (max 1 chars)))
    (if (version< emacs-version "30.1")
        (cons (* lines (disco-media--em-height-ratio)) 'em)
      (cons lines 'ch))))

(defun disco-media--image-file-size-pixels (file)
  "Return FILE image size in pixels as (WIDTH . HEIGHT), or nil."
  (let ((probe (ignore-errors
                 (create-image file nil nil :ascent 'center))))
    (and (disco-media-image-object-valid-p probe)
         (ignore-errors
           (image-size probe t)))))

(defun disco-media--preview-height-chars (image-size max-width max-height)
  "Return target preview height in lines for IMAGE-SIZE and pixel limits."
  (let* ((max-cols (disco-media--pixels->chars-width max-width))
         (max-rows (disco-media--pixels->chars-height max-height))
         (char-width (float (disco-media--char-pixel-width)))
         (char-height (float (disco-media--char-pixel-height)))
         (image-width (max 1.0 (float (car image-size))))
         (image-height (max 1.0 (float (cdr image-size))))
         (image-cols (/ image-width char-width))
         (image-rows (/ image-height char-height))
         (scale (min 1.0
                     (/ (float max-cols) (max 1.0 image-cols))
                     (/ (float max-rows) (max 1.0 image-rows)))))
    (max 1
         (min max-rows
              (round (* image-rows scale))))))

(defun disco-media-attachment-preview-rendering-available-p ()
  "Return non-nil when inline attachment image previews are available."
  (and disco-room-show-attachment-image-previews
       (disco-media-inline-image-rendering-available-p)))

(defun disco-media-attachment-preview-url (attachment)
  "Return best preview URL for ATTACHMENT object."
  (let ((proxy (alist-get 'proxy_url attachment))
        (url (alist-get 'url attachment)))
    (cond
     ((and (stringp proxy) (not (string-empty-p proxy))) proxy)
     ((and (stringp url) (not (string-empty-p url))) url)
     (t nil))))

(defun disco-media-attachment-download-url (attachment)
  "Return canonical download URL for ATTACHMENT object."
  (let ((url (alist-get 'url attachment))
        (proxy (alist-get 'proxy_url attachment)))
    (cond
     ((and (stringp url) (not (string-empty-p url))) url)
     ((and (stringp proxy) (not (string-empty-p proxy))) proxy)
     (t nil))))

(defun disco-media-attachment-preview-cache-key (attachment)
  "Return stable preview cache key for ATTACHMENT object."
  (let* ((attachment-id (alist-get 'id attachment))
         (url (disco-media-attachment-preview-url attachment))
         (name (or (alist-get 'filename attachment) "unnamed"))
         (seed (or (and attachment-id (format "%s" attachment-id))
                   (and url (md5 url))
                   name)))
    (format "%s:%s:%s:%s"
            seed
            name
            (max 64
                 (if (numberp disco-room-attachment-preview-max-width)
                     disco-room-attachment-preview-max-width
                   460))
            (max 64
                 (if (numberp disco-room-attachment-preview-max-height)
                     disco-room-attachment-preview-max-height
                   360)))))

(defun disco-media-attachment-preview-cache-state (cache-key)
  "Return preview cache state for CACHE-KEY.

Return value is nil, `:missing', or an image object."
  (and cache-key
       (gethash cache-key disco-media--attachment-preview-image-cache)))

(defun disco-media-attachment-preview-fetching-p (cache-key)
  "Return non-nil when preview fetch for CACHE-KEY is currently active."
  (and cache-key
       (gethash cache-key disco-media--attachment-preview-fetching)))

(defun disco-media-set-preview-fetch-budget (value)
  "Set preview fetch budget to VALUE for this render pass.

VALUE should be nil for uncapped mode or a non-negative integer."
  (setq disco-media--attachment-preview-fetch-budget
        (and (numberp value)
             (max 0 value))))

(defun disco-media--attachment-preview-cache-file-base (cache-key)
  "Return attachment preview cache file base path for CACHE-KEY."
  (expand-file-name (md5 cache-key) disco-room-attachment-cache-directory))

(defun disco-media--attachment-preview-cache-file (cache-key extension)
  "Return attachment preview cache file path for CACHE-KEY and EXTENSION."
  (format "%s.%s"
          (disco-media--attachment-preview-cache-file-base cache-key)
          extension))

(defun disco-media-attachment-preview-cache-existing-file (cache-key)
  "Return existing preview cache file path for CACHE-KEY, or nil."
  (seq-find #'file-exists-p
            (mapcar (lambda (ext)
                      (disco-media--attachment-preview-cache-file cache-key ext))
                    disco-media--cache-extensions)))

(defun disco-media--attachment-preview-ensure-queue ()
  "Return active queue for attachment preview fetches."
  (let ((limit (max 1 disco-room-attachment-preview-fetch-concurrency)))
    (when (or (null disco-media--attachment-preview-plz-queue)
              (not (equal disco-media--attachment-preview-plz-queue-limit limit)))
      (setq disco-media--attachment-preview-plz-queue (make-plz-queue :limit limit))
      (setq disco-media--attachment-preview-plz-queue-limit limit))
    disco-media--attachment-preview-plz-queue))

(defun disco-media--attachment-preview-delete-stale-cache-files (cache-base)
  "Delete stale cached preview files for CACHE-BASE."
  (dolist (ext disco-media--cache-extensions)
    (let ((old-file (format "%s.%s" cache-base ext)))
      (when (file-exists-p old-file)
        (ignore-errors (delete-file old-file))))))

(defun disco-media-preview-image-from-file (file max-width max-height)
  "Create inline preview image from FILE constrained by MAX-WIDTH/MAX-HEIGHT."
  (let* ((safe-max-width (max 1 (if (numberp max-width) max-width 460)))
         (safe-max-height (max 1 (if (numberp max-height) max-height 360)))
         (file-size (disco-media--image-file-size-pixels file))
         (target-height-chars
          (if (consp file-size)
              (disco-media--preview-height-chars
               file-size
               safe-max-width
               safe-max-height)
            (disco-media--pixels->chars-height safe-max-height)))
         (height-spec (disco-media--ch-height-spec target-height-chars))
         (image
          (ignore-errors
            (create-image file nil nil
                          :height height-spec
                          :disco-nslices target-height-chars
                          :scale 1.0
                          :ascent 'center))))
    (unless (disco-media-image-object-valid-p image)
      (when (image-type-available-p 'imagemagick)
        (setq image
              (ignore-errors
                (create-image file 'imagemagick nil
                              :height height-spec
                              :disco-nslices target-height-chars
                              :scale 1.0
                              :ascent 'center)))))
    (and (disco-media-image-object-valid-p image)
         image)))

(defun disco-media--attachment-preview-image-from-file (file)
  "Create inline attachment preview image from FILE, or nil when unavailable."
  (let ((max-width (max 64
                        (if (numberp disco-room-attachment-preview-max-width)
                            disco-room-attachment-preview-max-width
                          460)))
        (max-height (max 64
                         (if (numberp disco-room-attachment-preview-max-height)
                             disco-room-attachment-preview-max-height
                           360))))
    (disco-media-preview-image-from-file file max-width max-height)))

(defun disco-media--image-mime-type (file)
  "Return MIME type string for image FILE extension, or nil."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (pcase ext
      ("png" "image/png")
      ((or "jpg" "jpeg") "image/jpeg")
      ("gif" "image/gif")
      ("webp" "image/webp")
      ("svg" "image/svg+xml")
      (_ nil))))

(defun disco-media--svg-image (svg &rest props)
  "Return image object created from SVG with PROPS."
  (let ((svg-data (with-temp-buffer
                    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                    (svg-print svg)
                    (buffer-string))))
    (apply #'create-image svg-data 'svg t props)))

(defun disco-media--spoiler-displacement-scale (width height)
  "Return telega-like spoiler displacement scale for WIDTH and HEIGHT."
  (let ((base (float (max 24 (min (or width 120) (or height 120))))))
    (format "%.1f" (max 10.0 (min 28.0 (/ base 5.0))))))

(defun disco-media--svg-append-spoiler-node (svg node-id &optional width height)
  "Append telega-like spoiler turbulence filter with NODE-ID into SVG."
  (when (and (fboundp 'svg--append)
             (fboundp 'dom-node))
    (svg--append
     svg
     (dom-node 'filter
               `((id . ,node-id)
                 (x . "-20%")
                 (y . "-20%")
                 (width . "140%")
                 (height . "140%"))
               (dom-node 'feTurbulence
                         `((type . "turbulence")
                           (result . "NOISE")
                           ,@disco-media--spoiler-turbulence-attrs))
               (dom-node 'feDisplacementMap
                         `((in . "SourceGraphic")
                           (in2 . "NOISE")
                           (xChannelSelector . "R")
                           (yChannelSelector . "G")
                           (scale . ,(disco-media--spoiler-displacement-scale
                                      width height))))))))

(defun disco-media--svg-spoiler-filter-ref (svg &optional width height)
  "Return spoiler filter ref string for SVG, appending node when possible."
  (when (and svg
             (fboundp 'svg--append)
             (fboundp 'dom-node))
    (let ((node-id "disco-spoiler-noise"))
      (disco-media--svg-append-spoiler-node svg node-id width height)
      (format "url(#%s)" node-id))))

(defun disco-media--svg-append-spoiler-video-icon (svg width height)
  "Append spoiler video play icon into SVG of WIDTH by HEIGHT."
  (when (and (fboundp 'svg-circle)
             (fboundp 'svg-polygon))
    (let* ((cx (/ width 2.0))
           (cy (/ height 2.0))
           (radius (max 6.0 (min (/ width 6.0) (/ height 6.0))))
           (tri-w (* radius 0.95))
           (tri-h (* radius 1.10))
           (x1 (- cx (/ tri-w 3.0)))
           (x2 (- cx (/ tri-w 3.0)))
           (x3 (+ cx (* tri-w 0.67)))
           (y1 (- cy (/ tri-h 2.0)))
           (y2 (+ cy (/ tri-h 2.0)))
           (y3 cy))
      (svg-circle svg cx cy radius
                  :fill "#ffffff"
                  :fill-opacity 0.82)
      (svg-polygon svg
                   (list (cons x1 y1)
                         (cons x2 y2)
                         (cons x3 y3))
                   :fill "#000000"
                   :fill-opacity 0.75))))

(defun disco-media--svg-append-spoiler-overlay (svg width height &optional video-p)
  "Append telega-like spoiler overlay chrome to SVG of WIDTH by HEIGHT.

The actual spoiler distortion comes from the turbulence/displacement filter.
This overlay only darkens slightly and optionally adds a video play marker."
  (when (fboundp 'svg-rectangle)
    (svg-rectangle svg 0 0 width height
                   :fill "#000000"
                   :fill-opacity 0.08))
  (when video-p
    (disco-media--svg-append-spoiler-video-icon svg width height)))

(defun disco-media--preview-image-display-props (preview-image)
  "Return safe display-only image properties derived from PREVIEW-IMAGE."
  (let* ((props (cdr-safe preview-image))
         (slices (or (plist-get props :disco-nslices)
                     (plist-get props :telega-nslices)
                     (disco-media-image-slice-count preview-image)))
         (height (plist-get props :height))
         (width (plist-get props :width))
         (scale (plist-get props :scale))
         (ascent (plist-get props :ascent))
         (mask (plist-get props :mask))
         result)
    (when height
      (setq result (plist-put result :height height)))
    (when width
      (setq result (plist-put result :width width)))
    (when slices
      (setq result (plist-put result :disco-nslices slices)))
    (setq result (plist-put result :scale (or scale 1.0)))
    (setq result (plist-put result :ascent (or ascent 'center)))
    (when mask
      (setq result (plist-put result :mask mask)))
    result))

(defun disco-media--preview-image-source-spec (preview-image)
  "Return source plist for PREVIEW-IMAGE usable by SVG overlay builders."
  (let* ((props (cdr-safe preview-image))
         (file (plist-get props :file))
         (data (plist-get props :data))
         (type (plist-get props :type))
         (mime (cond
                ((and (stringp file) (file-exists-p file))
                 (disco-media--image-mime-type file))
                ((eq type 'svg) "image/svg+xml")
                ((eq type 'png) "image/png")
                ((memq type '(jpeg jpg)) "image/jpeg")
                ((eq type 'gif) "image/gif")
                ((eq type 'webp) "image/webp")
                (t nil))))
    (cond
     ((and (stringp file) mime)
      (list :source file :data-p nil :mime mime))
     ((and (stringp data) mime)
      (list :source data :data-p t :mime mime))
     (t nil))))

(cl-defun disco-media--decorated-preview-cache-key (preview-image &key spoiler-filter-p
                                                                  video-p dim-opacity)
  "Return stable cache key for decorated PREVIEW-IMAGE."
  (let* ((props (cdr-safe preview-image))
         (source (or (plist-get props :file)
                     (plist-get props :data)
                     preview-image))
         (display-size (ignore-errors (image-size preview-image t (selected-frame)))))
    (list source
          (and (consp display-size) (car display-size))
          (and (consp display-size) (cdr display-size))
          (plist-get props :height)
          (plist-get props :width)
          (plist-get props :disco-nslices)
          (plist-get props :telega-nslices)
          spoiler-filter-p
          video-p
          dim-opacity)))

(cl-defun disco-media--decorate-preview-image (preview-image &key spoiler-filter-p
                                                             video-p dim-opacity)
  "Return SVG-decorated PREVIEW-IMAGE with optional spoiler/video chrome."
  (when (and (disco-media-image-object-valid-p preview-image)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-embed)
             (fboundp 'svg-print))
    (let* ((cache-key (disco-media--decorated-preview-cache-key
                       preview-image
                       :spoiler-filter-p spoiler-filter-p
                       :video-p video-p
                       :dim-opacity dim-opacity))
           (cached (gethash cache-key disco-media--attachment-decorated-preview-cache))
           (source-spec (disco-media--preview-image-source-spec preview-image)))
      (or (and (disco-media-image-object-valid-p cached) cached)
          (when source-spec
            (let* ((display-size (ignore-errors
                                   (image-size preview-image t (selected-frame))))
                   (svg-width (max 1 (round (or (and (consp display-size)
                                                     (car display-size))
                                                64))))
                   (svg-height (max 1 (round (or (and (consp display-size)
                                                      (cdr display-size))
                                                 64))))
                   (props (disco-media--preview-image-display-props preview-image))
                   (svg (svg-create svg-width svg-height))
                   (filter-ref (and spoiler-filter-p
                                    (disco-media--svg-spoiler-filter-ref
                                     svg svg-width svg-height))))
              (svg-embed svg
                         (plist-get source-spec :source)
                         (plist-get source-spec :mime)
                         (plist-get source-spec :data-p)
                         :x 0 :y 0 :width svg-width :height svg-height
                         :filter filter-ref)
              (when (and (numberp dim-opacity)
                         (> dim-opacity 0)
                         (fboundp 'svg-rectangle))
                (svg-rectangle svg 0 0 svg-width svg-height
                               :fill "#000000"
                               :fill-opacity dim-opacity))
              (when video-p
                (disco-media--svg-append-spoiler-video-icon svg svg-width svg-height))
              (let ((image (apply #'disco-media--svg-image svg props)))
                (when (disco-media-image-object-valid-p image)
                  (puthash cache-key image disco-media--attachment-decorated-preview-cache)
                  image))))))))

(defun disco-media-attachment-video-display-image (preview-image)
  "Return PREVIEW-IMAGE decorated with a play marker for normal video preview."
  (disco-media--decorate-preview-image preview-image
                                       :spoiler-filter-p nil
                                       :video-p t
                                       :dim-opacity 0.0))

(defun disco-media--attachment-spoiler-preview-cache-key (cache-file preview-image)
  "Return cache key for spoiler preview built from CACHE-FILE and PREVIEW-IMAGE."
  (let* ((attrs (and (stringp cache-file) (file-exists-p cache-file)
                     (file-attributes cache-file)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (props (cdr-safe preview-image)))
    (list cache-file
          mtime
          (plist-get props :height)
          (plist-get props :width)
          (plist-get props :disco-nslices)
          (plist-get props :telega-nslices))))

(defun disco-media--make-attachment-spoiler-preview-image (cache-file preview-image &optional video-p)
  "Return obscured spoiler preview image for CACHE-FILE using PREVIEW-IMAGE sizing."
  (when (and (stringp cache-file)
             (file-exists-p cache-file)
             (disco-media-image-object-valid-p preview-image)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-embed)
             (fboundp 'svg-print))
    (let* ((display-size (ignore-errors (image-size preview-image t (selected-frame))))
           (svg-width (max 1 (round (or (and (consp display-size) (car display-size))
                                        (car (or (disco-media--image-file-size-pixels cache-file)
                                                 '(1 . 1)))))))
           (svg-height (max 1 (round (or (and (consp display-size) (cdr display-size))
                                         (cdr (or (disco-media--image-file-size-pixels cache-file)
                                                  '(1 . 1)))))))
           (mime (disco-media--image-mime-type cache-file))
           (props (disco-media--preview-image-display-props preview-image))
           (svg (and mime (svg-create svg-width svg-height)))
           (filter-ref (and svg (disco-media--svg-spoiler-filter-ref
                                 svg svg-width svg-height))))
      (when svg
        (svg-embed svg cache-file mime nil
                   :x 0 :y 0 :width svg-width :height svg-height
                   :filter filter-ref)
        (disco-media--svg-append-spoiler-overlay svg svg-width svg-height video-p)
        (apply #'disco-media--svg-image svg props)))))

(defun disco-media--attachment-placeholder-version (attachment)
  "Return normalized placeholder version for ATTACHMENT, or nil."
  (let ((raw (alist-get 'placeholder_version attachment)))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

(defun disco-media-attachment-placeholder-present-p (attachment)
  "Return non-nil when ATTACHMENT carries a decodable placeholder."
  (let ((placeholder (alist-get 'placeholder attachment))
        (version (disco-media--attachment-placeholder-version attachment)))
    (and (stringp placeholder)
         (not (string-empty-p (string-trim placeholder)))
         (or (null version) (= version 1)))))

(defun disco-media--thumbhash-normalize-string (value)
  "Return VALUE normalized as padded base64/base64url string."
  (let* ((trimmed (replace-regexp-in-string "[[:space:]]+" "" (string-trim value)))
         (base64url (replace-regexp-in-string "_" "/"
                                              (replace-regexp-in-string "-" "+" trimmed)))
         (pad (mod (- 4 (mod (length base64url) 4)) 4)))
    (concat base64url (make-string pad ?=))))

(defun disco-media--thumbhash-bytes (value)
  "Decode thumbhash string VALUE into an unibyte string, or nil."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (condition-case _
        (let ((decoded (base64-decode-string
                        (disco-media--thumbhash-normalize-string value))))
          (encode-coding-string decoded 'binary))
      (error nil))))

(defun disco-media--thumbhash-aspect-ratio (bytes)
  "Return approximate aspect ratio represented by thumbhash BYTES."
  (when (and (stringp bytes) (>= (length bytes) 5))
    (let* ((header24 (+ (aref bytes 0)
                        (ash (aref bytes 1) 8)
                        (ash (aref bytes 2) 16)))
           (header16 (+ (aref bytes 3)
                        (ash (aref bytes 4) 8)))
           (has-alpha (not (zerop (logand header24 (ash 1 23)))))
           (is-landscape (not (zerop (logand header16 (ash 1 15)))))
           (long-count (if has-alpha 5 7))
           (short-count (max 3 (logand header16 7)))
           (lx (if is-landscape long-count short-count))
           (ly (if is-landscape short-count long-count)))
      (/ (float lx) (float (max 1 ly))))))

(defun disco-media--thumbhash-output-size (ratio)
  "Return decode output size cons for thumbhash aspect RATIO."
  (let* ((safe-ratio (if (and (numberp ratio) (> ratio 0)) ratio 1.0))
         (max-side 32))
    (if (>= safe-ratio 1.0)
        (cons max-side (max 1 (round (/ max-side safe-ratio))))
      (cons (max 1 (round (* max-side safe-ratio))) max-side))))

(defun disco-media--attachment-placeholder-source-size (attachment ratio)
  "Return source pixel size cons for ATTACHMENT placeholder display."
  (let ((width (alist-get 'width attachment))
        (height (alist-get 'height attachment)))
    (if (and (numberp width) (> width 0)
             (numberp height) (> height 0))
        (cons width height)
      (disco-media--thumbhash-output-size ratio))))

(defun disco-media--attachment-placeholder-height-chars (attachment ratio)
  "Return display height in text lines for ATTACHMENT placeholder."
  (let* ((safe-max-width (max 64
                              (if (numberp disco-room-attachment-preview-max-width)
                                  disco-room-attachment-preview-max-width
                                460)))
         (safe-max-height (max 64
                               (if (numberp disco-room-attachment-preview-max-height)
                                   disco-room-attachment-preview-max-height
                                 360))))
    (disco-media--preview-height-chars
     (disco-media--attachment-placeholder-source-size attachment ratio)
     safe-max-width
     safe-max-height)))

(defun disco-media--thumbhash-decode-channel (bytes start nx ny dc scale)
  "Decode one thumbhash channel from BYTES at START with NX by NY coefficients."
  (let* ((count (max 1 (* nx ny)))
         (channel (make-vector count 0.0))
         (nibbles (max 0 (1- count))))
    (aset channel 0 dc)
    (dotimes (i nibbles)
      (let* ((byte-index (+ start (ash i -1)))
             (byte (if (< byte-index (length bytes))
                       (aref bytes byte-index)
                     0))
             (nibble (if (zerop (logand i 1))
                         (logand byte #x0F)
                       (ash byte -4))))
        (aset channel (1+ i)
              (* scale (- (/ (float nibble) 7.5) 1.0)))))
    (cons channel (+ start (/ (+ nibbles 1) 2)))))

(defun disco-media--thumbhash-channel-value (channel nx ny x y width height)
  "Return reconstructed channel sample at X,Y from CHANNEL coefficients."
  (let ((sum 0.0)
        (index 0)
        (xfactor (/ (* float-pi (+ x 0.5)) (float (max 1 width))))
        (yfactor (/ (* float-pi (+ y 0.5)) (float (max 1 height))))
        (cy 0))
    (while (< cy ny)
      (let ((yc (cos (* yfactor cy)))
            (cx 0))
        (while (< cx nx)
          (setq sum (+ sum (* (aref channel index)
                              yc
                              (cos (* xfactor cx)))))
          (setq index (1+ index))
          (setq cx (1+ cx))))
      (setq cy (1+ cy)))
    sum))

(defun disco-media--clamp-unit (value)
  "Clamp VALUE into [0, 1] float range."
  (max 0.0 (min 1.0 (float value))))

(defun disco-media--thumbhash->svg-image (thumbhash attachment &optional spoiler-p)
  "Decode THUMBHASH for ATTACHMENT and return placeholder SVG image.

When SPOILER-P is non-nil, return a spoiler-obscured variant."
  (let* ((bytes (disco-media--thumbhash-bytes thumbhash))
         (ratio (and bytes (disco-media--thumbhash-aspect-ratio bytes))))
    (when (and bytes
               ratio
               (image-type-available-p 'svg)
               (fboundp 'svg-create)
               (fboundp 'svg-rectangle)
               (fboundp 'svg-print))
      (let* ((header24 (+ (aref bytes 0)
                          (ash (aref bytes 1) 8)
                          (ash (aref bytes 2) 16)))
             (header16 (+ (aref bytes 3)
                          (ash (aref bytes 4) 8)))
             (has-alpha (not (zerop (logand header24 (ash 1 23)))))
             (is-landscape (not (zerop (logand header16 (ash 1 15)))))
             (long-count (if has-alpha 5 7))
             (short-count (max 3 (logand header16 7)))
             (lx (if is-landscape long-count short-count))
             (ly (if is-landscape short-count long-count))
             (l-dc (/ (float (logand header24 63)) 63.0))
             (p-dc (- (/ (float (logand (ash header24 -6) 63)) 31.5) 1.0))
             (q-dc (- (/ (float (logand (ash header24 -12) 63)) 31.5) 1.0))
             (l-scale (/ (float (logand (ash header24 -18) 31)) 31.0))
             (p-scale (/ (float (logand (ash header16 -3) 63)) 63.0))
             (q-scale (/ (float (logand (ash header16 -9) 63)) 63.0))
             (a-dc (if has-alpha
                       (/ (float (logand (aref bytes 5) 15)) 15.0)
                     1.0))
             (a-scale (if has-alpha
                          (/ (float (ash (aref bytes 5) -4)) 15.0)
                        0.0))
             (offset (if has-alpha 6 5))
             (pixel-size (disco-media--thumbhash-output-size ratio))
             (pixel-width (max 1 (car pixel-size)))
             (pixel-height (max 1 (cdr pixel-size)))
             (display-lines (disco-media--attachment-placeholder-height-chars
                             attachment ratio))
             (height-spec (disco-media--ch-height-spec display-lines))
             (kind (disco-media-attachment-kind attachment))
             l-channel p-channel q-channel a-channel)
        (pcase-let* ((`(,decoded-l . ,offset-1)
                      (disco-media--thumbhash-decode-channel bytes offset lx ly l-dc l-scale))
                     (`(,decoded-p . ,offset-2)
                      (disco-media--thumbhash-decode-channel bytes offset-1 3 3 p-dc p-scale))
                     (`(,decoded-q . ,offset-3)
                      (disco-media--thumbhash-decode-channel bytes offset-2 3 3 q-dc q-scale))
                     (`(,decoded-a . ,_offset-4)
                      (if has-alpha
                          (disco-media--thumbhash-decode-channel bytes offset-3 lx ly a-dc a-scale)
                        (cons nil offset-3))))
          (setq l-channel decoded-l
                p-channel decoded-p
                q-channel decoded-q
                a-channel decoded-a)
          (let* ((svg (svg-create pixel-width pixel-height))
                 (filter-ref (and spoiler-p
                                  (disco-media--svg-spoiler-filter-ref
                                   svg pixel-width pixel-height)))
                 (target-group (and filter-ref
                                    (fboundp 'dom-node)
                                    (dom-node 'g `((filter . ,filter-ref))))))
            (dotimes (y pixel-height)
              (dotimes (x pixel-width)
                (let* ((l (disco-media--thumbhash-channel-value
                           l-channel lx ly x y pixel-width pixel-height))
                       (p (disco-media--thumbhash-channel-value
                           p-channel 3 3 x y pixel-width pixel-height))
                       (q (disco-media--thumbhash-channel-value
                           q-channel 3 3 x y pixel-width pixel-height))
                       (a (if has-alpha
                              (disco-media--thumbhash-channel-value
                               a-channel lx ly x y pixel-width pixel-height)
                            1.0))
                       (r (disco-media--clamp-unit (+ l (/ p 3.0) (/ q 2.0))))
                       (g (disco-media--clamp-unit (+ l (/ p 3.0) (/ q -2.0))))
                       (b (disco-media--clamp-unit (- l (/ (* 2.0 p) 3.0))))
                       (alpha (disco-media--clamp-unit a))
                       (fill (format "#%02x%02x%02x"
                                     (round (* r 255.0))
                                     (round (* g 255.0))
                                     (round (* b 255.0)))))
                  (if target-group
                      (dom-append-child
                       target-group
                       (dom-node 'rect
                                 `((x . ,(number-to-string x))
                                   (y . ,(number-to-string y))
                                   (width . "1")
                                   (height . "1")
                                   (fill . ,fill)
                                   (fill-opacity . ,(format "%.4f" alpha)))))
                    (svg-rectangle svg x y 1 1
                                   :fill fill
                                   :fill-opacity alpha)))))
            (when target-group
              (svg--append svg target-group))
            (when spoiler-p
              (disco-media--svg-append-spoiler-overlay
               svg pixel-width pixel-height (eq kind 'video)))
            (disco-media--svg-image
             svg
             :scale 1.0
             :height height-spec
             :disco-nslices display-lines
             :ascent 'center)))))))

(defun disco-media--attachment-placeholder-cache-key (attachment &optional spoiler-p)
  "Return stable cache key for ATTACHMENT placeholder image.

When SPOILER-P is non-nil, key the spoilerized placeholder variant."
  (let ((placeholder (alist-get 'placeholder attachment))
        (version (disco-media--attachment-placeholder-version attachment)))
    (when (and (stringp placeholder)
               (not (string-empty-p (string-trim placeholder))))
      (list placeholder
            version
            spoiler-p
            (alist-get 'width attachment)
            (alist-get 'height attachment)
            (max 64 (if (numberp disco-room-attachment-preview-max-width)
                        disco-room-attachment-preview-max-width
                      460))
            (max 64 (if (numberp disco-room-attachment-preview-max-height)
                        disco-room-attachment-preview-max-height
                      360))))))

(defun disco-media-attachment-placeholder-image (attachment)
  "Return decoded placeholder image for ATTACHMENT, or nil."
  (let ((cache-key (disco-media--attachment-placeholder-cache-key attachment nil)))
    (when (and (disco-media-inline-image-rendering-available-p)
               (or (disco-media--attachment-image-p attachment)
                   (disco-media--attachment-video-p attachment))
               cache-key)
      (let ((cached (gethash cache-key disco-media--attachment-placeholder-image-cache)))
        (or (and (disco-media-image-object-valid-p cached) cached)
            (let ((image (disco-media--thumbhash->svg-image
                          (alist-get 'placeholder attachment)
                          attachment nil)))
              (when (disco-media-image-object-valid-p image)
                (puthash cache-key image disco-media--attachment-placeholder-image-cache)
                image)))))))

(defun disco-media-attachment-spoiler-placeholder-image (attachment)
  "Return spoiler placeholder image for ATTACHMENT without extra distortion."
  (when-let* ((placeholder-image (disco-media-attachment-placeholder-image attachment)))
    (disco-media--decorate-preview-image
     placeholder-image
     :spoiler-filter-p nil
     :video-p (eq (disco-media-attachment-kind attachment) 'video)
     :dim-opacity 0.10)))

(defun disco-media--attachment-real-spoiler-preview-image (attachment)
  "Return obscured real preview image for ATTACHMENT, or nil."
  (let* ((image-like (disco-media--attachment-image-p attachment))
         (video-like (disco-media--attachment-video-p attachment))
         (preview-image (and (or image-like video-like)
                             (disco-media--attachment-real-preview-image
                              attachment image-like video-like))))
    (when (disco-media-image-object-valid-p preview-image)
      (disco-media--decorate-preview-image
       preview-image
       :spoiler-filter-p t
       :video-p video-like
       :dim-opacity 0.08))))

(defun disco-media-attachment-spoiler-preview-image (attachment)
  "Return obscured preview image for spoiler ATTACHMENT, or nil."
  (when (disco-media-attachment-preview-rendering-available-p)
    (or (disco-media--attachment-real-spoiler-preview-image attachment)
        (disco-media-attachment-spoiler-placeholder-image attachment))))

(defun disco-media--notify-state-updated ()
  "Notify UI after media state/cache updates."
  (let ((callback (or disco-media-rerender-function
                      disco-media-preview-rerender-function)))
    (when (functionp callback)
      (funcall callback))))

(defun disco-media--attachment-preview-complete-fetch (cache-key image &optional target-file)
  "Finalize one attachment preview fetch for CACHE-KEY with IMAGE."
  (when (and (null image) target-file (file-exists-p target-file))
    (ignore-errors (delete-file target-file)))
  (puthash cache-key (or image :missing) disco-media--attachment-preview-image-cache)
  (remhash cache-key disco-media--attachment-preview-fetching)
  (disco-media--notify-state-updated))

(defun disco-media--bytes-prefix-p (bytes offset prefix-bytes)
  "Return non-nil when BYTES at OFFSET starts with PREFIX-BYTES list."
  (and (stringp bytes)
       (<= (+ offset (length prefix-bytes)) (length bytes))
       (cl-loop for b in prefix-bytes
                for i from 0
                always (= (aref bytes (+ offset i)) b))))

(defun disco-media--webp-bytes-p-at (bytes offset)
  "Return non-nil when BYTES has WEBP signature at OFFSET."
  (and (disco-media--bytes-prefix-p bytes offset '(82 73 70 70))
       (disco-media--bytes-prefix-p bytes (+ offset 8) '(87 69 66 80))))

(defun disco-media--known-image-signature-at-p (bytes offset)
  "Return non-nil when BYTES has known image signature at OFFSET."
  (and (<= offset (length bytes))
       (or (disco-media--bytes-prefix-p bytes offset '(137 80 78 71 13 10 26 10))
           (disco-media--bytes-prefix-p bytes offset '(255 216 255))
           (disco-media--bytes-prefix-p bytes offset '(71 73 70 56 55 97))
           (disco-media--bytes-prefix-p bytes offset '(71 73 70 56 57 97))
           (disco-media--webp-bytes-p-at bytes offset))))

(defun disco-media-normalize-image-bytes (bytes)
  "Normalize downloaded image BYTES by stripping stray leading newlines."
  (cond
   ((and (stringp bytes)
         (>= (length bytes) 2)
         (eq (aref bytes 0) ?\n)
         (disco-media--known-image-signature-at-p bytes 1))
    (substring bytes 1))
   ((and (stringp bytes)
         (>= (length bytes) 3)
         (eq (aref bytes 0) ?\r)
         (eq (aref bytes 1) ?\n)
         (disco-media--known-image-signature-at-p bytes 2))
    (substring bytes 2))
   (t bytes)))

(defun disco-media-bytes->extension (bytes fallback-extension)
  "Infer image extension from BYTES, else return FALLBACK-EXTENSION."
  (cond
   ((disco-media--known-image-signature-at-p bytes 0)
    (cond
     ((disco-media--bytes-prefix-p bytes 0 '(137 80 78 71 13 10 26 10))
      "png")
     ((disco-media--bytes-prefix-p bytes 0 '(255 216 255))
      "jpg")
     ((or (disco-media--bytes-prefix-p bytes 0 '(71 73 70 56 55 97))
          (disco-media--bytes-prefix-p bytes 0 '(71 73 70 56 57 97)))
      "gif")
     ((disco-media--webp-bytes-p-at bytes 0)
      "webp")
     (t fallback-extension)))
   (t fallback-extension)))

(defun disco-media--attachment-image-p (attachment)
  "Return non-nil when ATTACHMENT is image-like."
  (let ((content-type (downcase (or (alist-get 'content_type attachment) "")))
        (filename (downcase (or (alist-get 'filename attachment) ""))))
    (or (string-prefix-p "image/" content-type)
        (string-match-p "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|bmp\\|svg\\)\\'" filename))))

(defun disco-media--attachment-video-p (attachment)
  "Return non-nil when ATTACHMENT is video-like."
  (let ((content-type (downcase (or (alist-get 'content_type attachment) "")))
        (filename (downcase (or (alist-get 'filename attachment) ""))))
    (or (string-prefix-p "video/" content-type)
        (string-match-p "\\.\\(?:mp4\\|mov\\|mkv\\|webm\\|avi\\|m4v\\)\\'" filename))))

(defun disco-media--start-video-preview-fetch (cache-key url cache-base)
  "Start asynchronous video preview extraction for CACHE-KEY from URL."
  (unless (or (gethash cache-key disco-media--attachment-preview-fetching)
              (gethash cache-key disco-media--attachment-preview-image-cache)
              (and (numberp disco-media--attachment-preview-fetch-budget)
                   (<= disco-media--attachment-preview-fetch-budget 0)))
    (when (numberp disco-media--attachment-preview-fetch-budget)
      (cl-decf disco-media--attachment-preview-fetch-budget))
    (puthash cache-key t disco-media--attachment-preview-fetching)
    (let ((ffmpeg (executable-find "ffmpeg")))
      (if (not ffmpeg)
          (disco-media--attachment-preview-complete-fetch cache-key nil)
        (let ((target-file (format "%s.jpg" cache-base)))
          (disco-media--attachment-preview-delete-stale-cache-files cache-base)
          (make-directory (file-name-directory target-file) t)
          (condition-case err
              (make-process
               :name (format "disco-video-preview-%s" (substring (md5 cache-key) 0 8))
               :buffer (generate-new-buffer " *disco-video-preview*")
               :command (list ffmpeg
                              "-nostdin"
                              "-y"
                              "-loglevel"
                              "error"
                              "-i"
                              url
                              "-vf"
                              "thumbnail=24,scale=960:-2:force_original_aspect_ratio=decrease"
                              "-frames:v"
                              "1"
                              target-file)
               :noquery t
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (unwind-protect
                       (if (and (= (process-exit-status proc) 0)
                                (file-exists-p target-file))
                           (disco-media--attachment-preview-complete-fetch
                            cache-key
                            (disco-media--attachment-preview-image-from-file target-file)
                            target-file)
                         (disco-media--attachment-preview-complete-fetch cache-key nil target-file))
                     (when (buffer-live-p (process-buffer proc))
                       (kill-buffer (process-buffer proc)))))))
            (error
             (disco-media--attachment-preview-complete-fetch cache-key nil)
             (message "disco: video preview enqueue failed for %s: %s"
                      cache-key
                      (error-message-string err)))))))))

(defun disco-media--start-attachment-preview-fetch (cache-key url cache-base)
  "Start asynchronous preview fetch for CACHE-KEY from URL into CACHE-BASE."
  (unless (or (gethash cache-key disco-media--attachment-preview-fetching)
              (gethash cache-key disco-media--attachment-preview-image-cache)
              (and (numberp disco-media--attachment-preview-fetch-budget)
                   (<= disco-media--attachment-preview-fetch-budget 0)))
    (when (numberp disco-media--attachment-preview-fetch-budget)
      (cl-decf disco-media--attachment-preview-fetch-budget))
    (puthash cache-key t disco-media--attachment-preview-fetching)
    (let ((queue (disco-media--attachment-preview-ensure-queue))
          (headers '(("Accept" . "image/png,image/webp,image/*;q=0.8,*/*;q=0.1"))))
      (condition-case err
          (progn
            (plz-queue
              queue
              'get
              url
              :headers headers
              :as 'binary
              :noquery t
              :then
              (lambda (data)
                (let* ((raw-bytes
                        (disco-media-normalize-image-bytes
                         (if (multibyte-string-p data)
                             (encode-coding-string data 'binary)
                           data)))
                       (extension (disco-media-bytes->extension raw-bytes "img"))
                       (target-file (format "%s.%s" cache-base extension))
                       image)
                  (disco-media--attachment-preview-delete-stale-cache-files cache-base)
                  (make-directory (file-name-directory target-file) t)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert raw-bytes)
                    (let ((coding-system-for-write 'binary))
                      (write-region (point-min) (point-max) target-file nil 'silent)))
                  (setq image (disco-media--attachment-preview-image-from-file target-file))
                  (disco-media--attachment-preview-complete-fetch cache-key image target-file)))
              :else
              (lambda (_err)
                (disco-media--attachment-preview-complete-fetch cache-key nil)))
            (plz-run queue))
        (error
         (disco-media--attachment-preview-complete-fetch cache-key nil)
         (message "disco: attachment preview enqueue failed for %s: %s"
                  cache-key
                  (error-message-string err)))))))

(defun disco-media--attachment-real-preview-image (attachment image-like video-like)
  "Return real fetched preview image for ATTACHMENT, or nil."
  (let* ((cache-key (disco-media-attachment-preview-cache-key attachment))
         (cached (and cache-key
                      (gethash cache-key disco-media--attachment-preview-image-cache))))
    (cond
     ((eq cached :missing)
      nil)
     ((and cached (disco-media-image-object-valid-p cached))
      cached)
     (cached
      (remhash cache-key disco-media--attachment-preview-image-cache)
      nil)
     (t
      (let* ((url (disco-media-attachment-preview-url attachment))
             (cache-base (and cache-key
                              (disco-media--attachment-preview-cache-file-base cache-key)))
             (cache-file (and cache-key
                              (disco-media-attachment-preview-cache-existing-file cache-key)))
             (file-image (and cache-file
                              (disco-media--attachment-preview-image-from-file cache-file))))
        (cond
         (file-image
          (puthash cache-key file-image disco-media--attachment-preview-image-cache)
          file-image)
         ((and url cache-base)
          (when (and cache-file (not file-image))
            (ignore-errors (delete-file cache-file)))
          (if image-like
              (disco-media--start-attachment-preview-fetch cache-key url cache-base)
            (when video-like
              (disco-media--start-video-preview-fetch cache-key url cache-base)))
          nil)
         (t nil)))))))

(defun disco-media-attachment-preview-image (attachment &optional bypass-user-toggle)
  "Return preview image object for ATTACHMENT when available."
  (let* ((rendering-ok (if bypass-user-toggle
                           (disco-media-inline-image-rendering-available-p)
                         (disco-media-attachment-preview-rendering-available-p)))
         (image-like (disco-media--attachment-image-p attachment))
         (video-like (disco-media--attachment-video-p attachment)))
    (when (and rendering-ok
               (or image-like video-like))
      (or (disco-media--attachment-real-preview-image attachment image-like video-like)
          (disco-media-attachment-placeholder-image attachment)))))

(defun disco-media-attachment-ephemeral-p (attachment)
  "Return non-nil when ATTACHMENT is ephemeral."
  (let ((value (alist-get 'ephemeral attachment)))
    (and value (not (eq value :false)))))

(defun disco-media--attachment-flags (attachment)
  "Return normalized integer attachment flags from ATTACHMENT, or nil."
  (let ((raw (alist-get 'flags attachment)))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

(defun disco-media-attachment-spoiler-p (attachment)
  "Return non-nil when ATTACHMENT should be treated as a spoiler."
  (let ((filename (alist-get 'filename attachment))
        (flagged (disco-media--attachment-flags attachment))
        (explicit (alist-get 'is_spoiler attachment)))
    (or (and explicit (not (eq explicit :false)))
        (and (integerp flagged)
             (/= 0 (logand flagged disco-media--attachment-flag-is-spoiler)))
        (and (stringp filename)
             (string-prefix-p "SPOILER_" filename t)))))

(defun disco-media-attachment-spoiler-label (attachment)
  "Return placeholder label shown while spoiler ATTACHMENT stays hidden."
  (pcase (disco-media-attachment-kind attachment)
    ('photo "[spoiler image hidden]")
    ('video "[spoiler video hidden]")
    ('audio "[spoiler audio hidden]")
    (_ "[spoiler attachment hidden]")))

(defun disco-media-attachment-kind (attachment)
  "Return normalized media kind symbol for ATTACHMENT.

Return value is one of `photo', `video', `audio' or `document'."
  (cond
   ((disco-media--attachment-image-p attachment) 'photo)
   ((disco-media--attachment-video-p attachment) 'video)
   ((or (string-prefix-p "audio/" (downcase (or (alist-get 'content_type attachment) "")))
        (numberp (alist-get 'duration_secs attachment))
        (stringp (alist-get 'waveform attachment)))
    'audio)
   (t 'document)))

(defun disco-media-attachment-display-name (attachment)
  "Return best display name for ATTACHMENT."
  (or (and (stringp (alist-get 'title attachment))
           (not (string-empty-p (string-trim (alist-get 'title attachment))))
           (string-trim (alist-get 'title attachment)))
      (and (stringp (alist-get 'filename attachment))
           (not (string-empty-p (string-trim (alist-get 'filename attachment))))
           (let ((filename (string-trim (alist-get 'filename attachment))))
             (if (and (disco-media-attachment-spoiler-p attachment)
                      (string-prefix-p "SPOILER_" filename t))
                 (substring filename 8)
               filename)))
      (let ((id (alist-get 'id attachment)))
        (if (and id (not (string-empty-p (format "%s" id))))
            (format "attachment-%s" id)
          "attachment.bin"))))

(defun disco-media-attachment-size-label (attachment)
  "Return human-readable size label for ATTACHMENT, or nil."
  (let ((size (alist-get 'size attachment)))
    (when (numberp size)
      (file-size-human-readable size))))

(defun disco-media-attachment-dimensions-label (attachment)
  "Return WIDTHxHEIGHT label for ATTACHMENT, or nil."
  (let ((width (alist-get 'width attachment))
        (height (alist-get 'height attachment)))
    (when (and (numberp width) (numberp height))
      (format "%dx%d" width height))))

(defun disco-media-attachment-duration-label (attachment)
  "Return human-readable duration label for ATTACHMENT, or nil."
  (let ((seconds (alist-get 'duration_secs attachment)))
    (when (numberp seconds)
      (let* ((total (max 0 (floor seconds)))
             (hours (/ total 3600))
             (minutes (% (/ total 60) 60))
             (secs (% total 60)))
        (if (> hours 0)
            (format "%d:%02d:%02d" hours minutes secs)
          (format "%d:%02d" minutes secs))))))

(defun disco-media-attachment-content-type-label (attachment)
  "Return media type label for ATTACHMENT, or nil."
  (let ((content-type (alist-get 'content_type attachment)))
    (when (and (stringp content-type)
               (not (string-empty-p (string-trim content-type))))
      (string-trim content-type))))

(defun disco-media-attachment-summary (attachment)
  "Return compact one-line summary string for ATTACHMENT."
  (let* ((kind (pcase (disco-media-attachment-kind attachment)
                 ('photo "img")
                 ('video "video")
                 ('audio "audio")
                 (_ "file")))
         (name (disco-media-attachment-display-name attachment))
         (details (delq nil (list (disco-media-attachment-size-label attachment)
                                  (disco-media-attachment-dimensions-label attachment)
                                  (disco-media-attachment-duration-label attachment))))
         (detail-text (if details
                          (format " (%s)" (string-join details ", "))
                        "")))
    (format "[%s] %s%s" kind name detail-text)))

(defun disco-media-attachment-meta-line (attachment &optional include-content-type)
  "Return verbose metadata line for ATTACHMENT.

When INCLUDE-CONTENT-TYPE is non-nil, include the attachment media type when
available."
  (let ((parts nil))
    (when include-content-type
      (when-let* ((content-type
                   (disco-media-attachment-content-type-label attachment)))
        (push (format "type=%s" content-type) parts)))
    (when-let* ((size (disco-media-attachment-size-label attachment)))
      (push (format "size=%s" size) parts))
    (when-let* ((dims (disco-media-attachment-dimensions-label attachment)))
      (push (format "dims=%s" dims) parts))
    (when-let* ((duration (disco-media-attachment-duration-label attachment)))
      (push (format "duration=%s" duration) parts))
    (when (disco-media-attachment-ephemeral-p attachment)
      (push "ephemeral" parts))
    (if parts
        (string-join (nreverse parts) "  ")
      "size=n/a")))

(defun disco-media--sanitize-filename (filename)
  "Return filesystem-safe variant of FILENAME."
  (replace-regexp-in-string
   "[[:cntrl:]/\\]+"
   "_"
   (or filename "attachment.bin")))

(defun disco-media-attachment-default-save-name (attachment)
  "Return default filename for saving ATTACHMENT locally."
  (or (and (stringp (alist-get 'filename attachment))
           (not (string-empty-p (alist-get 'filename attachment)))
           (alist-get 'filename attachment))
      (let ((id (alist-get 'id attachment)))
        (if (and id (not (string-empty-p (format "%s" id))))
            (format "attachment-%s" id)
          "attachment.bin"))))

(defun disco-media-attachment-download-key (attachment)
  "Return stable download-state key for ATTACHMENT."
  (format "%s"
          (or (alist-get 'id attachment)
              (disco-media-attachment-download-url attachment)
              (alist-get 'filename attachment)
              (sxhash attachment))))

(defun disco-media-attachment-download-path (attachment)
  "Return default local download path for ATTACHMENT."
  (let* ((key (disco-media-attachment-download-key attachment))
         (safe-name (disco-media--sanitize-filename
                     (disco-media-attachment-default-save-name attachment))))
    (expand-file-name
     (format "%s-%s" (substring (md5 key) 0 10) safe-name)
     disco-room-attachment-download-directory)))

(defun disco-media-attachment-download-state (attachment)
  "Return normalized download state plist for ATTACHMENT."
  (let* ((key (disco-media-attachment-download-key attachment))
         (entry (copy-tree (or (gethash key disco-media--attachment-download-state-table) '())))
         (path (or (plist-get entry :path)
                   (disco-media-attachment-download-path attachment)))
         (status (plist-get entry :status)))
    (setq entry (plist-put entry :path path))
    (when (and (eq status 'downloaded) (not (file-exists-p path)))
      (setq entry (plist-put entry :status 'not-downloaded))
      (setq entry (plist-put entry :error nil))
      (setq status 'not-downloaded))
    (when (and (not (eq status 'downloading))
               (file-exists-p path))
      (setq entry (plist-put entry :status 'downloaded))
      (setq entry (plist-put entry :error nil)))
    (unless (plist-get entry :status)
      (setq entry (plist-put entry :status (if (file-exists-p path)
                                               'downloaded
                                             'not-downloaded))))
    (puthash key entry disco-media--attachment-download-state-table)
    entry))

(defun disco-media--attachment-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (condition-case _
          (error-message-string err)
        (error
         (format "%S" err)))))

(defun disco-media-start-attachment-download (attachment &optional open-after)
  "Start asynchronous default-location download for ATTACHMENT.

When OPEN-AFTER is non-nil, open downloaded file in Emacs after completion."
  (let* ((url (disco-media-attachment-download-url attachment))
         (key (disco-media-attachment-download-key attachment))
         (entry (disco-media-attachment-download-state attachment))
         (path (plist-get entry :path))
         (status (plist-get entry :status))
         (expected-size (alist-get 'size attachment)))
    (unless (disco-media-url-present-p url)
      (user-error "disco: attachment has no downloadable URL"))
    (when (eq status 'downloading)
      (user-error "disco: attachment download already in progress"))
    (make-directory (or (file-name-directory path)
                        disco-room-attachment-download-directory)
                    t)
    (let ((process
           (plz 'get
             url
             :as 'binary
             :noquery t
             :then
             (lambda (data)
               (let ((raw-bytes (if (multibyte-string-p data)
                                    (encode-coding-string data 'binary)
                                  data))
                     (current (copy-tree (or (gethash key disco-media--attachment-download-state-table)
                                             '()))))
                 (if (and (numberp expected-size)
                          (> expected-size 0)
                          (<= (string-bytes raw-bytes) 0))
                     (condition-case fallback-err
                         (progn
                           (url-copy-file url path t)
                           (let ((actual-size (nth 7 (file-attributes path))))
                             (when (and (numberp expected-size)
                                        (> expected-size 0)
                                        (numberp actual-size)
                                        (<= actual-size 0))
                               (error "empty download body (expected %d bytes)" expected-size)))
                           (setq current (plist-put current :status 'downloaded))
                           (setq current (plist-put current :path path))
                           (setq current (plist-put current :process nil))
                           (setq current (plist-put current :cancel-requested nil))
                           (setq current (plist-put current :error nil))
                           (puthash key current disco-media--attachment-download-state-table)
                           (disco-media--notify-state-updated)
                           (message "disco: downloaded attachment -> %s (url fallback)" path)
                           (when open-after
                             (find-file path)))
                       (error
                        (setq current (plist-put current :status 'error))
                        (setq current (plist-put current :process nil))
                        (setq current (plist-put current :cancel-requested nil))
                        (setq current (plist-put current :error
                                                 (disco-media--attachment-error-message fallback-err)))
                        (puthash key current disco-media--attachment-download-state-table)
                        (disco-media--notify-state-updated)
                        (message "disco: attachment download failed: %s"
                                 (plist-get current :error))))
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert raw-bytes)
                     (let ((coding-system-for-write 'binary))
                       (write-region (point-min) (point-max) path nil 'silent)))
                   (setq current (plist-put current :status 'downloaded))
                   (setq current (plist-put current :path path))
                   (setq current (plist-put current :process nil))
                   (setq current (plist-put current :cancel-requested nil))
                   (setq current (plist-put current :error nil))
                   (puthash key current disco-media--attachment-download-state-table)
                   (disco-media--notify-state-updated)
                   (message "disco: downloaded attachment -> %s" path)
                   (when open-after
                     (find-file path)))))
             :else
             (lambda (err)
               (let* ((current (copy-tree (or (gethash key disco-media--attachment-download-state-table)
                                              '())))
                      (cancel-requested (plist-get current :cancel-requested)))
                 (setq current (plist-put current :process nil))
                 (setq current (plist-put current :cancel-requested nil))
                 (if cancel-requested
                     (progn
                       (setq current (plist-put current :status 'not-downloaded))
                       (setq current (plist-put current :error nil))
                       (puthash key current disco-media--attachment-download-state-table)
                       (disco-media--notify-state-updated)
                       (message "disco: attachment download canceled"))
                   (condition-case fallback-err
                       (progn
                         (url-copy-file url path t)
                         (setq current (plist-put current :status 'downloaded))
                         (setq current (plist-put current :path path))
                         (setq current (plist-put current :error nil))
                         (puthash key current disco-media--attachment-download-state-table)
                         (disco-media--notify-state-updated)
                         (message "disco: downloaded attachment -> %s (url fallback)" path)
                         (when open-after
                           (find-file path)))
                     (error
                      (setq current (plist-put current :status 'error))
                      (setq current (plist-put current :error
                                               (or (disco-media--attachment-error-message err)
                                                   (disco-media--attachment-error-message fallback-err))))
                      (puthash key current disco-media--attachment-download-state-table)
                      (disco-media--notify-state-updated)
                      (message "disco: attachment download failed: %s"
                               (plist-get current :error))))))))))
      (setq entry (plist-put entry :status 'downloading))
      (setq entry (plist-put entry :process process))
      (setq entry (plist-put entry :cancel-requested nil))
      (setq entry (plist-put entry :error nil))
      (puthash key entry disco-media--attachment-download-state-table)
      (disco-media--notify-state-updated)
      (message "disco: downloading attachment %s"
               (disco-media-attachment-display-name attachment)))))

(defun disco-media-cancel-attachment-download (attachment)
  "Cancel active asynchronous download for ATTACHMENT."
  (let* ((key (disco-media-attachment-download-key attachment))
         (entry (copy-tree (or (gethash key disco-media--attachment-download-state-table) '())))
         (process (plist-get entry :process))
         (status (plist-get entry :status)))
    (unless (eq status 'downloading)
      (user-error "disco: attachment is not downloading"))
    (setq entry (plist-put entry :cancel-requested t))
    (puthash key entry disco-media--attachment-download-state-table)
    (when (process-live-p process)
      (ignore-errors (delete-process process)))
    (message "disco: canceling attachment download")))

(defun disco-media-open-downloaded-attachment (attachment)
  "Open local downloaded file for ATTACHMENT."
  (let* ((entry (disco-media-attachment-download-state attachment))
         (path (plist-get entry :path)))
    (unless (and (stringp path) (file-exists-p path))
      (user-error "disco: attachment file is not downloaded yet"))
    (find-file path)))

(defun disco-media-play-attachment-video (attachment)
  "Play ATTACHMENT video preferring local file when available."
  (let* ((entry (disco-media-attachment-download-state attachment))
         (path (plist-get entry :path))
         (url (disco-media-attachment-download-url attachment)))
    (cond
     ((and (stringp path) (file-exists-p path))
      (disco-media-play-video-file path))
     ((disco-media-url-present-p url)
      (disco-media-play-video-url url))
     (t
      (user-error "disco: video attachment has no playable source")))))

(defun disco-media-download-attachment (attachment &optional target-path)
  "Download ATTACHMENT to TARGET-PATH.

When TARGET-PATH is nil, prompt interactively for destination path."
  (interactive)
  (let* ((url (disco-media-attachment-download-url attachment))
         (download-state (disco-media-attachment-download-state attachment))
         (cached-path (plist-get download-state :path))
         (has-cached (and (stringp cached-path) (file-exists-p cached-path)))
         (has-url (disco-media-url-present-p url))
         (default-name (disco-media-attachment-default-save-name attachment))
         (target (or target-path
                     (read-file-name "Save attachment as: "
                                     nil
                                     default-name
                                     nil
                                     default-name))))
    (unless (or has-cached has-url)
      (user-error "disco: attachment has neither local cache nor downloadable URL"))
    (condition-case err
        (progn
          (make-directory (or (file-name-directory target) default-directory) t)
          (if has-cached
              (copy-file cached-path target t)
            (url-copy-file url target t))
          (message "disco: downloaded attachment -> %s" target))
      (error
       (user-error "disco: attachment download failed: %s"
                   (disco-media--attachment-error-message err))))))

(provide 'disco-media)

;;; disco-media.el ends here
