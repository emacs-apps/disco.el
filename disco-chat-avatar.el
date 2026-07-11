;;; disco-chat-avatar.el --- Shared two-line chat avatars -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Telega renders a chat avatar as two image slices: the upper slice prefixes
;; the sender heading and the lower slice prefixes the first content row.  This
;; module exposes that geometry as protocol-neutral prefix strings so chat
;; clients can share the same layout through `disco-ui' prefix states.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-media)

(defun disco-chat-avatar-line-pixel-height ()
  "Return one text line's pixel height at the current text scale."
  (let* ((line-height (ignore-errors (line-pixel-height)))
         (base-height (or (ignore-errors (default-line-height))
                          (frame-char-height)
                          16))
         (scale-step (if (and (boundp 'text-scale-mode-step)
                              (numberp text-scale-mode-step)
                              (> text-scale-mode-step 0))
                         text-scale-mode-step
                       1.2))
         (scale-amount (if (and (boundp 'text-scale-mode-amount)
                                (numberp text-scale-mode-amount))
                           text-scale-mode-amount
                         0))
         (scale (if (zerop scale-amount)
                    1.0
                  (expt scale-step scale-amount)))
         (scaled-base (round (* (float base-height) scale)))
         (use-line-height (and (numberp line-height)
                               (> line-height 2)
                               (>= line-height (floor (* 0.5 base-height))))))
    (max 1 (if use-line-height line-height scaled-base))))

(defun disco-chat-avatar-two-line-pixel-size ()
  "Return the pixel size of an avatar occupying exactly two text lines."
  (* 2 (disco-chat-avatar-line-pixel-height)))

(defun disco-chat-avatar-column-width (nchars)
  "Return an image width specification occupying NCHARS text columns."
  (let ((columns (max 1 nchars)))
    (if (string-version-lessp emacs-version "30.1")
        (* columns (max 1 (frame-char-width)))
      (cons columns 'cw))))

(defun disco-chat-avatar-resize-image (image pixel-size)
  "Return IMAGE resized to square PIXEL-SIZE, or nil when it is invalid."
  (when (disco-media-image-object-valid-p image)
    (let* ((type (car image))
           (properties (copy-sequence (cdr image))))
      (setq properties (plist-put properties :width pixel-size))
      (setq properties (plist-put properties :height pixel-size))
      (setq properties (plist-put properties :ascent 'center))
      (cons type properties))))

(defun disco-chat-avatar--fit-text (text width)
  "Return TEXT truncated or padded to exactly WIDTH columns."
  (let* ((target (max 1 width))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (trim-width (string-width trimmed)))
    (if (< trim-width target)
        (concat trimmed (make-string (- target trim-width) ?\s))
      trimmed)))

(defun disco-chat-avatar--image-text (image slice-index)
  "Return textual fallback stored on IMAGE for SLICE-INDEX."
  (let ((text (and (consp image)
                   (plist-get (cdr image) :disco-chat-avatar-text))))
    (cond
     ((stringp text) text)
     ((and (listp text)
           (integerp slice-index)
           (>= slice-index 0)
           (< slice-index (length text)))
      (nth slice-index text))
     (t nil))))

(defun disco-chat-avatar-image-char-width (image)
  "Return IMAGE's rendered width in text columns."
  (or (and (consp image)
           (let ((width (plist-get (cdr image)
                                   :disco-chat-avatar-char-width)))
             (and (integerp width) (> width 0) width)))
      (let* ((size (and (disco-media-image-object-valid-p image)
                        (ignore-errors
                          (image-size image t (selected-frame)))))
             (width-pixels (and (consp size) (car size)))
             (char-width (max 1 (frame-char-width))))
        (max 1
             (if (numberp width-pixels)
                 (ceiling (/ (float width-pixels) (float char-width)))
               1)))))

(cl-defun disco-chat-avatar--prepare-image
    (image fallback &key pixel-size resize slice-height)
  "Return IMAGE decorated for two-line slicing.

FALLBACK supplies the textual first slice.  PIXEL-SIZE is used when RESIZE is
non-nil.  SLICE-HEIGHT overrides the inferred one-line pixel height."
  (let* ((target-size (or pixel-size
                          (disco-chat-avatar-two-line-pixel-size)))
         (base-image (if resize
                         (disco-chat-avatar-resize-image image target-size)
                       image)))
    (when (disco-media-image-object-valid-p base-image)
      (let* ((size (ignore-errors
                     (image-size base-image t (selected-frame))))
             (height-pixels (or (and (consp size) (cdr size)) target-size))
             (width-chars (disco-chat-avatar-image-char-width base-image))
             (effective-slice-height
              (or slice-height
                  (and (consp base-image)
                       (plist-get (cdr base-image)
                                  :disco-chat-avatar-slice-height))
                  (max 1 (floor (/ (float height-pixels) 2)))))
             (type (car base-image))
             (properties (copy-sequence (cdr base-image)))
             (top-text (disco-chat-avatar--fit-text fallback width-chars))
             (bottom-text (make-string width-chars ?\u00a0)))
        ;; Preserve a stable column width even when image pixel geometry and
        ;; the current font's character width do not divide evenly.
        (setq properties
              (plist-put properties :width
                         (disco-chat-avatar-column-width width-chars)))
        (setq properties
              (plist-put properties :disco-chat-avatar-char-width width-chars))
        (setq properties
              (plist-put properties :disco-chat-avatar-slice-height
                         effective-slice-height))
        (setq properties
              (plist-put properties :disco-chat-avatar-text
                         (list top-text bottom-text)))
        (cons type properties)))))

(defun disco-chat-avatar--slice-display (image slice-index)
  "Return an Emacs display specification for IMAGE SLICE-INDEX."
  (when (disco-media-image-object-valid-p image)
    (let* ((size (ignore-errors (image-size image t (selected-frame))))
           (height (and (consp size) (cdr size)))
           (slice-height (or (and (consp image)
                                  (plist-get
                                   (cdr image)
                                   :disco-chat-avatar-slice-height))
                             (and (numberp height)
                                  (max 1 (floor (/ (float height) 2))))))
           (slice-y (and slice-height (* slice-height slice-index)))
           (remaining (and (numberp height) slice-y (- height slice-y)))
           (visible-height (and slice-height
                                (if (numberp remaining)
                                    (max 1 (min slice-height remaining))
                                  slice-height))))
      (when (and slice-y visible-height)
        (list (list 'slice 0 slice-y 1.0 visible-height) image)))))

(defun disco-chat-avatar--slice-string (image slice-index)
  "Return a display string for IMAGE SLICE-INDEX."
  (let* ((text (or (disco-chat-avatar--image-text image slice-index)
                   (make-string
                    (disco-chat-avatar-image-char-width image) ?\s)))
         (display (disco-chat-avatar--slice-display image slice-index)))
    (if display
        (propertize text 'display display 'rear-nonsticky '(display))
      text)))

(defun disco-chat-avatar--pad-prefix (prefix width)
  "Right-pad PREFIX so it occupies WIDTH columns."
  (let* ((text (or prefix ""))
         (target (max 0 width))
         (current (max 0 (string-width text))))
    (if (< current target)
        (concat text (make-string (- target current) ?\s))
      text)))

(cl-defun disco-chat-avatar-prefixes
    (image fallback &key pixel-size resize slice-height)
  "Return telega-style prefixes for a two-line chat avatar.

The result is a plist containing `:header', `:first-body', and `:rest-body'.
IMAGE is split between the heading and first body line; later lines receive a
same-width blank prefix.  FALLBACK occupies the heading while IMAGE is absent.
When RESIZE is non-nil, resize IMAGE to square PIXEL-SIZE before slicing."
  (let* ((fallback-text (if (and (stringp fallback)
                                 (not (string-empty-p fallback)))
                            fallback
                          "@"))
         (prepared (disco-chat-avatar--prepare-image
                    image fallback-text
                    :pixel-size pixel-size
                    :resize resize
                    :slice-height slice-height)))
    (if prepared
        (let* ((width (disco-chat-avatar-image-char-width prepared))
               (header (concat
                        (disco-chat-avatar--slice-string prepared 0) " "))
               (first-body (concat
                            (disco-chat-avatar--slice-string prepared 1) " "))
               (normalized-width (max (1+ width)
                                      (string-width header)
                                      (string-width first-body)))
               (rest-body (make-string normalized-width ?\s)))
          (list :header (disco-chat-avatar--pad-prefix
                         header normalized-width)
                :first-body (disco-chat-avatar--pad-prefix
                             first-body normalized-width)
                :rest-body rest-body))
      (let* ((expected-image-width
              (and (numberp pixel-size)
                   (> pixel-size 0)
                   (ceiling (/ (float pixel-size)
                               (float (max 1 (frame-char-width)))))))
             (image-width (max 1
                               (string-width fallback-text)
                               (or expected-image-width 0)))
             (width (1+ image-width))
             (rest-body (make-string width ?\s)))
        (list :header (disco-chat-avatar--pad-prefix
                       (concat (disco-chat-avatar--fit-text
                                fallback-text image-width)
                               " ")
                       width)
              :first-body rest-body
              :rest-body rest-body)))))

(provide 'disco-chat-avatar)
;;; disco-chat-avatar.el ends here
