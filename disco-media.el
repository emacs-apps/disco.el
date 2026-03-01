;;; disco-media.el --- Shared media helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared image/media helpers used by room and embed renderers.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'browse-url)
(require 'plz)

(defvar disco-room-show-attachment-image-previews)
(defvar disco-room-attachment-preview-max-width)
(defvar disco-room-attachment-preview-max-height)
(defvar disco-room-attachment-preview-fetch-concurrency)
(defvar disco-room-attachment-cache-directory)

(defconst disco-media--cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "img")
  "Preferred media cache file extension candidates.")

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
  "Function called after media preview cache updates.")

(defun disco-media-clear-preview-memory-cache ()
  "Clear in-memory preview image cache without touching disk files."
  (clrhash disco-media--attachment-preview-image-cache)
  (clrhash disco-media--attachment-preview-fetching))

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

(defun disco-media-add-open-url-properties (start end url)
  "Attach mouse/key handlers to open URL for text between START and END."
  (when (and (disco-media-url-present-p url)
             (< start end))
    (let* ((open-callback
            (lambda (&optional _event)
              (interactive)
              (browse-url url t)))
           (open-map (make-sparse-keymap)))
      (define-key open-map [mouse-1] open-callback)
      (define-key open-map (kbd "RET") open-callback)
      (add-text-properties
       start
       end
       (list 'keymap open-map
             'mouse-face 'highlight
             'help-echo (format "Open media: %s" url))))))

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

(defun disco-media--notify-preview-cache-updated ()
  "Notify UI after preview cache updates."
  (when (functionp disco-media-preview-rerender-function)
    (funcall disco-media-preview-rerender-function)))

(defun disco-media--attachment-preview-complete-fetch (cache-key image &optional target-file)
  "Finalize one attachment preview fetch for CACHE-KEY with IMAGE."
  (when (and (null image) target-file (file-exists-p target-file))
    (ignore-errors (delete-file target-file)))
  (puthash cache-key (or image :missing) disco-media--attachment-preview-image-cache)
  (remhash cache-key disco-media--attachment-preview-fetching)
  (disco-media--notify-preview-cache-updated))

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

(defun disco-media-attachment-preview-image (attachment &optional bypass-user-toggle)
  "Return preview image object for ATTACHMENT when available."
  (let ((rendering-ok (if bypass-user-toggle
                          (disco-media-inline-image-rendering-available-p)
                        (disco-media-attachment-preview-rendering-available-p))))
    (when (and rendering-ok
               (disco-media--attachment-image-p attachment))
      (let* ((cache-key (disco-media-attachment-preview-cache-key attachment))
             (cached (gethash cache-key disco-media--attachment-preview-image-cache)))
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
                 (cache-base (disco-media--attachment-preview-cache-file-base cache-key))
                 (cache-file (disco-media-attachment-preview-cache-existing-file cache-key))
                 (file-image (and cache-file
                                  (disco-media--attachment-preview-image-from-file cache-file))))
            (cond
             (file-image
              (puthash cache-key file-image disco-media--attachment-preview-image-cache)
              file-image)
             ((and url cache-base)
              (when (and cache-file (not file-image))
                (ignore-errors (delete-file cache-file)))
              (disco-media--start-attachment-preview-fetch cache-key url cache-base)
              nil)
             (t nil)))))))))

(provide 'disco-media)

;;; disco-media.el ends here
