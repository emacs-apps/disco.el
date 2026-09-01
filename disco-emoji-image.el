;;; disco-emoji-image.el --- Shared Discord custom emoji images -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared, cache-backed Discord custom emoji acquisition and rendering.
;; Consumers map opaque resource notifications onto their own projected rows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-media-image)
(require 'appkit-media-resource)

(defcustom disco-emoji-image-cache-directory
  (locate-user-emacs-file "disco-emoji-cache/")
  "Directory used to cache Discord custom emoji images."
  :type 'directory
  :group 'disco)

(defcustom disco-emoji-image-size 32
  "Pixel size requested for Discord custom emoji images."
  :type 'integer
  :group 'disco)

(defcustom disco-emoji-image-animate t
  "When non-nil, request animation for animated Discord custom emoji."
  :type 'boolean
  :group 'disco)

(defcustom disco-emoji-image-invalidation-delay 0.05
  "Seconds used to coalesce custom emoji resource notifications."
  :type 'number
  :group 'disco)

(defvar disco-emoji-image-resources-updated-hook nil
  "Hook run with coalesced custom emoji resource keys after images change.")

(defvar disco-emoji-image--images (make-hash-table :test #'equal)
  "Variant key to immutable image descriptor or `missing'.")

(defvar disco-emoji-image--fetching (make-hash-table :test #'equal)
  "Variant key to exact asynchronous request owner.")

(defvar disco-emoji-image--known (make-hash-table :test #'equal)
  "Custom emoji variants observed in the current account session.")

(defvar disco-emoji-image--pending-resource-updates
  (make-hash-table :test #'equal)
  "Resource keys awaiting one coalesced notification.")

(defvar disco-emoji-image--resource-update-timer nil
  "Timer coalescing successful custom emoji image notifications.")

(defvar disco-emoji-image--generation 0
  "Generation revoking callbacks from retired account sessions.")

(defun disco-emoji-image--normalize-id (emoji-id)
  "Return EMOJI-ID as a decimal string, or nil."
  (let ((value (cond
                ((stringp emoji-id) emoji-id)
                ((integerp emoji-id) (number-to-string emoji-id)))))
    (and value (string-match-p "\\`[0-9]+\\'" value) value)))

(defun disco-emoji-image--request-size ()
  "Return a valid Discord CDN power-of-two image size."
  (let* ((requested (max 16 (min 4096 disco-emoji-image-size)))
         (power 16))
    (while (< power requested)
      (setq power (* power 2)))
    power))

(defun disco-emoji-image--animated-request-p (animated)
  "Return non-nil when ANIMATED emoji playback should be requested."
  (and animated
       disco-emoji-image-animate
       appkit-media-inline-animation-enabled))

(defun disco-emoji-image--variant-key (emoji-id animated)
  "Return cache identity for EMOJI-ID and ANIMATED policy."
  (when-let* ((normalized (disco-emoji-image--normalize-id emoji-id)))
    (list normalized
          (disco-emoji-image--request-size)
          (and (disco-emoji-image--animated-request-p animated) t))))

(defun disco-emoji-image-resource-key (emoji-id animated)
  "Return the opaque resource key for EMOJI-ID and ANIMATED metadata."
  (when-let* ((variant (disco-emoji-image--variant-key emoji-id animated)))
    (list :emoji variant)))

(defun disco-emoji-image-url (emoji-id animated)
  "Return Discord CDN WebP URL for EMOJI-ID and ANIMATED metadata."
  (unless (disco-emoji-image--normalize-id emoji-id)
    (error "disco: custom emoji ID must be a decimal snowflake"))
  (let ((size (disco-emoji-image--request-size)))
    (format "https://cdn.discordapp.com/emojis/%s.webp?size=%d%s"
            emoji-id size
            (if (disco-emoji-image--animated-request-p animated)
                "&animated=true"
              ""))))

(defun disco-emoji-image--cache-base (variant)
  "Return disk cache base path for VARIANT."
  (expand-file-name
   (md5 (format "%S" variant))
   disco-emoji-image-cache-directory))

(defun disco-emoji-image--decode-file (file)
  "Return a valid one-line custom emoji image for FILE, or nil."
  (when (and (stringp file) (file-readable-p file))
    (condition-case nil
        (appkit-media-one-line-preview-image-from-file
         file (disco-emoji-image--request-size))
      ((error quit) nil))))

(defun disco-emoji-image--flush-resource-updates ()
  "Publish and clear coalesced custom emoji resource updates."
  (setq disco-emoji-image--resource-update-timer nil)
  (let (resources)
    (maphash (lambda (resource _present) (push resource resources))
             disco-emoji-image--pending-resource-updates)
    (clrhash disco-emoji-image--pending-resource-updates)
    (when resources
      (run-hook-with-args 'disco-emoji-image-resources-updated-hook
                          (nreverse resources)))))

(defun disco-emoji-image--schedule-resource-update (variant)
  "Schedule one coalesced resource notification for VARIANT."
  (when variant
    (puthash (list :emoji variant) t
             disco-emoji-image--pending-resource-updates)
    (unless (timerp disco-emoji-image--resource-update-timer)
      (setq disco-emoji-image--resource-update-timer
            (run-at-time
             (max 0 disco-emoji-image-invalidation-delay) nil
             #'disco-emoji-image--flush-resource-updates)))))

(defun disco-emoji-image--owner-current-p (variant owner)
  "Return non-nil when OWNER still owns VARIANT's active request."
  (and (= (plist-get owner :generation) disco-emoji-image--generation)
       (eq (gethash variant disco-emoji-image--fetching) owner)))

(defun disco-emoji-image--finish-fetch (variant owner image)
  "Finish OWNER's VARIANT request with IMAGE or `missing'."
  (when (disco-emoji-image--owner-current-p variant owner)
    (remhash variant disco-emoji-image--fetching)
    (puthash variant
             (if (appkit-media-image-object-valid-p image) image 'missing)
             disco-emoji-image--images)
    (disco-emoji-image--schedule-resource-update variant)))

(defun disco-emoji-image--start-fetch (variant emoji-id animated cache-base)
  "Start one shared fetch for VARIANT from Discord's emoji CDN."
  (unless (gethash variant disco-emoji-image--fetching)
    (let ((owner (list :generation disco-emoji-image--generation
                       :handle nil)))
      (puthash variant owner disco-emoji-image--fetching)
      (condition-case nil
          (let ((handle
                 (appkit-media-cache-image-resource-async
                  `((url . ,(disco-emoji-image-url emoji-id animated))
                    (name . ,(format "%s.webp" emoji-id)))
                  cache-base
                  (lambda (file)
                    (when (disco-emoji-image--owner-current-p variant owner)
                      (disco-emoji-image--finish-fetch
                       variant owner (disco-emoji-image--decode-file file))))
                  (lambda (_reason)
                    (disco-emoji-image--finish-fetch
                     variant owner 'missing)))))
            (if handle
                (setf (plist-get owner :handle) handle)
              (remhash variant disco-emoji-image--fetching)))
        (error
         (disco-emoji-image--finish-fetch variant owner 'missing))))))

(defun disco-emoji-image-image (emoji-id animated)
  "Return EMOJI-ID's cached image, scheduling acquisition when needed."
  (when (appkit-media-inline-image-rendering-available-p)
    (when-let* ((variant (disco-emoji-image--variant-key emoji-id animated)))
      (puthash variant t disco-emoji-image--known)
      (let ((cached (gethash variant disco-emoji-image--images)))
        (cond
         ((appkit-media-image-object-valid-p cached) cached)
         ((eq cached 'missing) nil)
         (t
          (when cached
            (remhash variant disco-emoji-image--images))
          (let* ((cache-base (disco-emoji-image--cache-base variant))
                 (existing
                  (appkit-media-image-cache-existing-file cache-base))
                 (image (and existing
                             (disco-emoji-image--decode-file existing))))
            (cond
             (image
              (puthash variant image disco-emoji-image--images)
              image)
             ((gethash variant disco-emoji-image--fetching) nil)
             (t
              (when existing
                (ignore-errors (delete-file existing)))
              (make-directory disco-emoji-image-cache-directory t)
              (disco-emoji-image--start-fetch
               variant (car variant) animated cache-base)
              nil)))))))))

(defun disco-emoji-image-display-string (emoji-id animated &optional fallback)
  "Return EMOJI-ID image display text or FALLBACK while unavailable."
  (let ((fallback (or fallback "")))
    (if-let* ((image (disco-emoji-image-image emoji-id animated)))
        (appkit-media-one-line-image-display-string
         image (if (string-empty-p fallback) " " fallback))
      fallback)))

(defun disco-emoji-image-completion-prefix (emoji-id animated)
  "Return a lazy affixation prefix function for EMOJI-ID."
  (lambda (_candidate)
    (let ((image (disco-emoji-image-display-string emoji-id animated "")))
      (if (string-empty-p image) "" (concat image " ")))))

(defun disco-emoji-image--cancel-timer (timer)
  "Cancel TIMER while isolating ordinary cancellation failures."
  (when (timerp timer)
    (condition-case nil
        (cancel-timer timer)
      ((error quit) nil))))

(defun disco-emoji-image-reset ()
  "Retire account-scoped custom emoji work and clear in-memory state."
  (cl-incf disco-emoji-image--generation)
  (let ((timer disco-emoji-image--resource-update-timer))
    (setq disco-emoji-image--resource-update-timer nil)
    (disco-emoji-image--cancel-timer timer))
  (maphash
   (lambda (_variant owner)
     (when-let* ((handle (plist-get owner :handle)))
       (condition-case nil
           (appkit-media-cancel-transfer handle)
         ((error quit) nil))))
   disco-emoji-image--fetching)
  (clrhash disco-emoji-image--fetching)
  (clrhash disco-emoji-image--images)
  (clrhash disco-emoji-image--known)
  (clrhash disco-emoji-image--pending-resource-updates))

(defun disco-emoji-image-clear-cache ()
  "Clear custom emoji memory and disk caches, then refresh dependents."
  (interactive)
  (let (known)
    (maphash (lambda (variant _present) (push variant known))
             disco-emoji-image--known)
    (disco-emoji-image-reset)
    (when (file-directory-p disco-emoji-image-cache-directory)
      (delete-directory disco-emoji-image-cache-directory t))
    (dolist (variant known)
      (disco-emoji-image--schedule-resource-update variant)))
  (message "disco: emoji image cache cleared"))

(provide 'disco-emoji-image)

;;; disco-emoji-image.el ends here
