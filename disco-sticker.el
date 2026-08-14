;;; disco-sticker.el --- Discord sticker catalogs and rendering -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Discord sticker catalog acquisition, account ranking, visual selection, CDN
;; caching, and Appkit-backed image projection.  Canonical catalog snapshots
;; remain in `disco-state'; room buffers only project and send selected IDs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-completion)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'disco-api)
(require 'disco-settings)
(require 'disco-state)

(defcustom disco-sticker-cache-directory
  (locate-user-emacs-file "disco-sticker-cache/")
  "Directory used to cache Discord sticker source and rendered files."
  :type 'directory
  :group 'disco)

(defcustom disco-sticker-size 160
  "Maximum pixel width and height for timeline sticker images."
  :type 'integer
  :group 'disco)

(defcustom disco-sticker-animate t
  "When non-nil, render animated Discord stickers when Appkit permits it."
  :type 'boolean
  :group 'disco)

(defcustom disco-sticker-lottie-renderer-command
  (executable-find "lottie_convert.py")
  "Python Lottie converter used for Discord format-type 3 stickers.

Nil means Lottie stickers retain their textual fallback without failing room
rendering or send operations."
  :type '(choice (const :tag "Unavailable" nil) file)
  :group 'disco)

(defcustom disco-sticker-frecency-limit 42
  "Maximum account sticker-frecency entries shown in the picker."
  :type 'integer
  :group 'disco)

(defcustom disco-sticker-invalidation-delay 0.05
  "Seconds used to coalesce sticker resource notifications."
  :type 'number
  :group 'disco)
(defcustom disco-sticker-media-retry-delay 30
  "Seconds before retrying transient Sticker media failures."
  :type 'number
  :group 'disco)


(defvar disco-sticker-resources-updated-hook nil
  "Hook run with coalesced opaque sticker resource keys after images change.")

(defvar disco-sticker--catalog-requests (make-hash-table :test #'equal)
  "Catalog request key to shared owner and listener state.")

(defvar disco-sticker--files (make-hash-table :test #'equal)
  "Sticker variant to a local file or a `:retry-at' failure marker.")

(defvar disco-sticker--images (make-hash-table :test #'equal)
  "Display cache key to immutable Appkit image descriptor.")

(defvar disco-sticker--fetching (make-hash-table :test #'equal)
  "Sticker variant to exact asynchronous transfer/conversion owner.")

(defvar disco-sticker--known (make-hash-table :test #'equal)
  "Sticker variants observed in the current account session.")

(defvar disco-sticker--pending-resource-updates
  (make-hash-table :test #'equal)
  "Resource keys awaiting one coalesced notification.")

(defvar disco-sticker--resource-update-timer nil
  "Timer coalescing sticker image notifications.")

(defvar disco-sticker--generation 0
  "Generation revoking callbacks from retired account sessions.")

(defvar disco-sticker--history nil
  "Minibuffer history for visual Discord sticker readers.")
(defvar-local disco-sticker--picker-owner-p nil
  "Non-nil only in the minibuffer owned by an active Sticker reader.")


(defun disco-sticker--normalize-id (value)
  "Return VALUE as a decimal Discord snowflake string, or nil."
  (let ((text (cond
               ((stringp value) value)
               ((integerp value) (number-to-string value)))))
    (and text (string-match-p "\\`[0-9]+\\'" text) text)))

(defun disco-sticker-format-type (sticker)
  "Return STICKER's supported numeric Discord format type, or nil."
  (let* ((raw (and (listp sticker) (alist-get 'format_type sticker)))
         (value (cond
                 ((integerp raw) raw)
                 ((and (stringp raw)
                       (string-match-p "\\`[0-9]+\\'" raw))
                  (string-to-number raw)))))
    (and (memq value '(1 2 3 4)) value)))

(defun disco-sticker-id (sticker)
  "Return STICKER's normalized snowflake, or nil."
  (disco-sticker--normalize-id
   (and (listp sticker) (alist-get 'id sticker))))

(defun disco-sticker-name (sticker)
  "Return STICKER's non-empty display name."
  (let ((name (and (listp sticker) (alist-get 'name sticker))))
    (if (and (stringp name) (not (string-empty-p (string-trim name))))
        (string-trim name)
      (or (disco-sticker-id sticker) "Sticker"))))

(defun disco-sticker--animation-enabled-p (format-type)
  "Return non-nil when FORMAT-TYPE should retain animation."
  (and (memq format-type '(2 3 4))
       disco-sticker-animate
       appkit-media-inline-animation-enabled))

(defun disco-sticker--variant-key (sticker)
  "Return cache identity for STICKER and current animation policy."
  (when-let* ((id (disco-sticker-id sticker))
              (format-type (disco-sticker-format-type sticker)))
    (list id format-type
          (and (disco-sticker--animation-enabled-p format-type) t))))

(defun disco-sticker-resource-key (sticker)
  "Return opaque Appkit resource identity for STICKER."
  (when-let* ((variant (disco-sticker--variant-key sticker)))
    (list :sticker variant)))

(defun disco-sticker-url (sticker)
  "Return the Discord CDN URL for STICKER."
  (let ((id (or (disco-sticker-id sticker)
                (error "disco: sticker ID must be a decimal snowflake")))
        (format-type (or (disco-sticker-format-type sticker)
                         (error "disco: unsupported sticker format_type"))))
    (format "https://cdn.discordapp.com/stickers/%s.%s"
            id
            (pcase format-type
              ((or 1 2) "png")
              (3 "json")
              (4 "gif")))))

(defun disco-sticker--normalize-list-sequence (value)
  "Return VALUE as a proper list, or nil."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun disco-sticker-message-items (message)
  "Return copied, renderable sticker items carried by MESSAGE."
  (let (items)
    (dolist (sticker
             (disco-sticker--normalize-list-sequence
              (and (listp message) (alist-get 'sticker_items message))))
      (when (and (listp sticker)
                 (disco-sticker-id sticker)
                 (disco-sticker-format-type sticker))
        (push (copy-tree sticker) items)))
    (nreverse items)))

(defun disco-sticker--catalog-loaded-p (key)
  "Return non-nil when sticker catalog KEY is authoritative."
  (pcase (car-safe key)
    ('standard (disco-state-standard-sticker-packs-loaded-p))
    ('guild (disco-state-guild-stickers-loaded-p (cadr key)))))

(defun disco-sticker--catalog-listeners-run (listeners success value)
  "Run LISTENERS' SUCCESS or error callback with VALUE."
  (dolist (listener listeners)
    (when-let* ((callback (if success (car listener) (cdr listener))))
      (condition-case nil
          (funcall callback value)
        ((error quit) nil)))))

(defun disco-sticker--catalog-finish (key owner response error)
  "Finish KEY request owned by OWNER with RESPONSE or ERROR."
  (when (eq owner (gethash key disco-sticker--catalog-requests))
    (remhash key disco-sticker--catalog-requests)
    (let ((listeners (nreverse (plist-get owner :listeners))))
      (if error
          (disco-sticker--catalog-listeners-run listeners nil error)
        (condition-case err
            (progn
              (pcase (car key)
                ('standard
                 (disco-state-set-standard-sticker-packs
                  (disco-sticker--normalize-list-sequence
                   (and (listp response)
                        (alist-get 'sticker_packs response)))))
                ('guild
                 ;; A Gateway snapshot arriving during this REST request is
                 ;; newer and remains authoritative.
                 (unless (disco-state-guild-stickers-loaded-p (cadr key))
                   (disco-state-set-guild-stickers
                    (cadr key)
                    (disco-sticker--normalize-list-sequence response)))))
              (disco-sticker--catalog-listeners-run listeners t response))
          ((error quit)
           (disco-sticker--catalog-listeners-run
            listeners nil (error-message-string err))))))))

(defun disco-sticker--request-catalog (key success error)
  "Ensure sticker catalog KEY, notifying SUCCESS or ERROR."
  (if (disco-sticker--catalog-loaded-p key)
      (when success (funcall success nil))
    (let ((owner (gethash key disco-sticker--catalog-requests)))
      (if owner
          (when (or success error)
            (push (cons success error) (plist-get owner :listeners)))
        (setq owner (list :listeners
                          (and (or success error)
                               (list (cons success error)))))
        (puthash key owner disco-sticker--catalog-requests)
        (condition-case err
            (pcase (car key)
              ('standard
               (disco-api-standard-sticker-packs-async
                :on-success
                (lambda (response)
                  (disco-sticker--catalog-finish key owner response nil))
                :on-error
                (lambda (reason)
                  (disco-sticker--catalog-finish key owner nil reason))))
              ('guild
               (disco-api-guild-stickers-async
                (cadr key)
                :on-success
                (lambda (response)
                  (disco-sticker--catalog-finish key owner response nil))
                :on-error
                (lambda (reason)
                  (disco-sticker--catalog-finish key owner nil reason)))))
          ((error quit)
           (disco-sticker--catalog-finish
            key owner nil (error-message-string err))))))))

(cl-defun disco-sticker-ensure-catalogs
    (guild-id &key on-success on-error)
  "Ensure standard and optional GUILD-ID catalogs asynchronously.

The sources load independently.  ON-SUCCESS runs when every request settles
and at least one applicable catalog is authoritative.  ON-ERROR runs only when
no applicable catalog could be loaded."
  (setq guild-id (disco-sticker--normalize-id guild-id))
  (let ((keys
         (delq nil
               (list
                (unless (disco-state-standard-sticker-packs-loaded-p)
                  '(standard))
                (and guild-id
                     (not (disco-state-guild-stickers-loaded-p guild-id))
                     (list 'guild guild-id))))))
    (if (null keys)
        (when on-success (funcall on-success))
      (let ((remaining (length keys))
            first-error)
        (cl-labels
            ((finish
              (success value)
              (unless success
                (setq first-error (or first-error value)))
              (setq remaining (1- remaining))
              (when (zerop remaining)
                (if (or (disco-state-standard-sticker-packs-loaded-p)
                        (and guild-id
                             (disco-state-guild-stickers-loaded-p guild-id)))
                    (when on-success (funcall on-success))
                  (when on-error (funcall on-error first-error))))))
          (dolist (key keys)
            (disco-sticker--request-catalog
             key
             (lambda (_response) (finish t nil))
             (lambda (reason) (finish nil reason)))))))))

(defun disco-sticker-catalogs-loaded-p (&optional guild-id)
  "Return non-nil when standard and optional GUILD-ID catalogs are loaded."
  (let ((guild-id (disco-sticker--normalize-id guild-id)))
    (and (disco-state-standard-sticker-packs-loaded-p)
         (or (null guild-id)
             (disco-state-guild-stickers-loaded-p guild-id)))))
(defun disco-sticker-catalog-available-p (&optional guild-id)
  "Return non-nil when at least one applicable Sticker catalog is loaded."
  (let ((guild-id (disco-sticker--normalize-id guild-id)))
    (or (disco-state-standard-sticker-packs-loaded-p)
        (and guild-id
             (disco-state-guild-stickers-loaded-p guild-id)))))


(defun disco-sticker-ready-p (&optional guild-id)
  "Return non-nil when the picker has an authoritative catalog to show.

Account rankings and another applicable catalog may still load in the
background without blocking an already usable picker."
  (disco-sticker-catalog-available-p guild-id))

(cl-defun disco-sticker-ensure-ready
    (guild-id &key on-success on-error)
  "Ensure picker catalogs and account rankings for GUILD-ID.

Settings failure degrades ranking only; catalog failure invokes ON-ERROR."
  (let ((remaining 2)
        catalog-error)
    (cl-labels
        ((finish
          (&optional error)
          (setq catalog-error (or catalog-error error)
                remaining (1- remaining))
          (when (zerop remaining)
            (if catalog-error
                (when on-error (funcall on-error catalog-error))
              (when on-success (funcall on-success))))))
      (disco-sticker-ensure-catalogs
       guild-id
       :on-success (lambda () (finish))
       :on-error (lambda (reason) (finish reason)))
      (disco-settings-ensure-loaded
       (lambda (_value) (finish))
       (lambda (_reason) (finish))))))

(defun disco-sticker--cache-token (variant)
  "Return stable filesystem token for VARIANT."
  (md5 (format "%S" variant)))

(defun disco-sticker--direct-cache-base (variant)
  "Return Appkit image cache base for direct-image VARIANT."
  (expand-file-name
   (disco-sticker--cache-token variant)
   (expand-file-name "images/" disco-sticker-cache-directory)))

(defun disco-sticker--lottie-source-file (variant)
  "Return raw Lottie JSON cache file for VARIANT."
  (expand-file-name
   (concat (disco-sticker--cache-token variant) ".json")
   (expand-file-name "lottie/" disco-sticker-cache-directory)))

(defun disco-sticker--lottie-render-file (variant)
  "Return derived image cache file for Lottie VARIANT."
  (expand-file-name
   (format "%s.%s"
           (disco-sticker--cache-token variant)
           (if (nth 2 variant) "webp" "png"))
   (expand-file-name "rendered/" disco-sticker-cache-directory)))

(defun disco-sticker--file-valid-p (file)
  "Return non-nil when FILE decodes as an Appkit image."
  (and (stringp file)
       (file-readable-p file)
       (condition-case nil
           (appkit-media-image-object-valid-p
            (appkit-media-preview-image-from-file file 32 32))
         ((error quit) nil))))

(defun disco-sticker--flush-resource-updates ()
  "Publish and clear coalesced sticker resource updates."
  (setq disco-sticker--resource-update-timer nil)
  (let (resources)
    (maphash (lambda (resource _present) (push resource resources))
             disco-sticker--pending-resource-updates)
    (clrhash disco-sticker--pending-resource-updates)
    (when resources
      (run-hook-with-args 'disco-sticker-resources-updated-hook
                          (nreverse resources)))))

(defun disco-sticker--schedule-resource-update (variant)
  "Schedule one coalesced resource notification for VARIANT."
  (when variant
    (puthash (list :sticker variant) t
             disco-sticker--pending-resource-updates)
    (unless (timerp disco-sticker--resource-update-timer)
      (setq disco-sticker--resource-update-timer
            (run-at-time
             (max 0 disco-sticker-invalidation-delay) nil
             #'disco-sticker--flush-resource-updates)))))

(defun disco-sticker--owner-current-p (variant owner)
  "Return non-nil when OWNER still owns VARIANT's active request."
  (and (= (plist-get owner :generation) disco-sticker--generation)
       (eq (gethash variant disco-sticker--fetching) owner)))

(defun disco-sticker--clear-variant-images (variant)
  "Forget immutable image descriptors derived from VARIANT."
  (let (keys)
    (maphash
     (lambda (key _image)
       (when (equal (car-safe key) variant)
         (push key keys)))
     disco-sticker--images)
    (dolist (key keys)
      (remhash key disco-sticker--images))))

(defun disco-sticker--finish-file (variant owner file)
  "Finish OWNER's VARIANT request with renderable FILE or retry state."
  (when (disco-sticker--owner-current-p variant owner)
    (remhash variant disco-sticker--fetching)
    (disco-sticker--clear-variant-images variant)
    (let* ((valid-file (and (disco-sticker--file-valid-p file) file))
           (generation disco-sticker--generation)
           (delay (max 0.1 disco-sticker-media-retry-delay))
           (cached
            (or valid-file
                (list :retry-at (+ (float-time) delay)))))
      (puthash variant cached disco-sticker--files)
      (unless valid-file
        (run-at-time
         delay nil
         (lambda (expected-variant expected-generation expected-cache)
           (when (and (= expected-generation disco-sticker--generation)
                      (eq expected-cache
                          (gethash expected-variant disco-sticker--files)))
             (remhash expected-variant disco-sticker--files)
             (disco-sticker--schedule-resource-update expected-variant)))
         variant generation cached)))
    (disco-sticker--schedule-resource-update variant)))

(defun disco-sticker--start-lottie-render (variant owner source)
  "Render Lottie SOURCE for VARIANT still owned by OWNER."
  (if-let* ((renderer disco-sticker-lottie-renderer-command)
            ((file-executable-p renderer)))
      (condition-case nil
          (let* ((target (disco-sticker--lottie-render-file variant))
                 (_ (make-directory (file-name-directory target) t))
                 (staging
                  (make-temp-file
                   (expand-file-name ".render-" (file-name-directory target))
                   nil
                   (if (nth 2 variant) ".webp" ".png")))
                 (_ (delete-file staging))
                 (command
                  (if (nth 2 variant)
                      (list renderer "--webp-quality" "80" "--webp-method" "4"
                            source staging)
                    (list renderer "--frame" "0" source staging)))
                 process)
            (setq process
                  (make-process
                   :name (format "disco-sticker-%s"
                                 (substring (disco-sticker--cache-token variant)
                                            0 10))
                   :command command
                   :buffer nil
                   :stderr nil
                   :noquery t
                   :connection-type 'pipe
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (if (and (disco-sticker--owner-current-p variant owner)
                                (= (process-exit-status proc) 0)
                                (file-regular-p staging))
                           (condition-case nil
                               (progn
                                 (rename-file staging target t)
                                 (disco-sticker--finish-file
                                  variant owner target))
                             (error
                              (ignore-errors (delete-file staging))
                              (disco-sticker--finish-file
                               variant owner 'missing)))
                         (ignore-errors (delete-file staging))
                         (when (disco-sticker--owner-current-p variant owner)
                           (disco-sticker--finish-file
                            variant owner 'missing)))))))
            (setf (plist-get owner :process) process))
        (error
         (disco-sticker--finish-file variant owner 'missing)))
    (disco-sticker--finish-file variant owner 'missing)))

(defun disco-sticker--start-lottie-fetch (variant sticker owner)
  "Acquire and render Lottie STICKER for VARIANT and OWNER."
  (let ((source (disco-sticker--lottie-source-file variant)))
    (if (file-readable-p source)
        (disco-sticker--start-lottie-render variant owner source)
      (make-directory (file-name-directory source) t)
      (let ((handle
             (appkit-media-copy-or-download-resource-async
              `((url . ,(disco-sticker-url sticker))
                (name . ,(format "%s.json" (car variant))))
              source
              (lambda (file)
                (when (disco-sticker--owner-current-p variant owner)
                  (setf (plist-get owner :handle) nil)
                  (disco-sticker--start-lottie-render variant owner file)))
              (lambda (_reason)
                (disco-sticker--finish-file variant owner 'missing)))))
        (when handle
          (setf (plist-get owner :handle) handle))))))

(defun disco-sticker--start-direct-fetch (variant sticker owner)
  "Acquire direct-image STICKER for VARIANT and OWNER."
  (let ((handle
         (appkit-media-cache-image-resource-async
          `((url . ,(disco-sticker-url sticker))
            (name . ,(format "%s.%s"
                             (car variant)
                             (if (= (cadr variant) 4) "gif" "png"))))
          (disco-sticker--direct-cache-base variant)
          (lambda (file)
            (disco-sticker--finish-file variant owner file))
          (lambda (_reason)
            (disco-sticker--finish-file variant owner 'missing)))))
    (when handle
      (setf (plist-get owner :handle) handle))))

(defun disco-sticker--start-fetch (variant sticker)
  "Start one shared Appkit-backed fetch for STICKER VARIANT."
  (unless (gethash variant disco-sticker--fetching)
    (let ((owner (list :generation disco-sticker--generation
                       :handle nil
                       :process nil)))
      (puthash variant owner disco-sticker--fetching)
      (condition-case nil
          (if (= (cadr variant) 3)
              (disco-sticker--start-lottie-fetch variant sticker owner)
            (disco-sticker--start-direct-fetch variant sticker owner))
        (error
         (disco-sticker--finish-file variant owner 'missing))))))

(defun disco-sticker--cached-file (variant sticker)
  "Return VARIANT's renderable file, scheduling STICKER acquisition."
  (puthash variant t disco-sticker--known)
  (let ((cached (gethash variant disco-sticker--files)))
    (cond
     ((and (stringp cached) (file-readable-p cached)) cached)
     ((and (listp cached)
           (> (or (plist-get cached :retry-at) 0) (float-time)))
      nil)
     (t
      (when cached
        (remhash variant disco-sticker--files))
      (let* ((format-type (cadr variant))
             (existing
              (if (= format-type 3)
                  (disco-sticker--lottie-render-file variant)
                (appkit-media-image-cache-existing-file
                 (disco-sticker--direct-cache-base variant)))))
        (cond
         ((disco-sticker--file-valid-p existing)
          (puthash variant existing disco-sticker--files)
          existing)
         ((gethash variant disco-sticker--fetching) nil)
         (t
          (when (and existing (file-exists-p existing))
            (ignore-errors (delete-file existing)))
          (make-directory disco-sticker-cache-directory t)
          (disco-sticker--start-fetch variant sticker)
          nil)))))))

(defun disco-sticker-image (sticker &optional purpose)
  "Return STICKER's cached immutable image descriptor for PURPOSE.

PURPOSE is `completion' for a one-line preview or `timeline' for the configured
multi-line sticker size.  Acquisition is scheduled when the cache is cold."
  (when (appkit-media-inline-image-rendering-available-p)
    (when-let* ((variant (disco-sticker--variant-key sticker))
                (file (disco-sticker--cached-file variant sticker)))
      (let* ((purpose (or purpose 'timeline))
             (cache-key (list variant purpose disco-sticker-size))
             (cached (gethash cache-key disco-sticker--images)))
        (if (appkit-media-image-object-valid-p cached)
            cached
          (when cached
            (remhash cache-key disco-sticker--images))
          (let ((image
                 (condition-case nil
                     (if (eq purpose 'completion)
                         (appkit-media-one-line-preview-image-from-file file 32)
                       (appkit-media-preview-image-from-file
                        file disco-sticker-size disco-sticker-size))
                   ((error quit) nil))))
            (when (appkit-media-image-object-valid-p image)
              (puthash cache-key image disco-sticker--images)
              image)))))))

(defun disco-sticker-completion-prefix (sticker)
  "Return lazy Appkit affixation prefix function for STICKER."
  (lambda (_candidate)
    (if-let* ((image (disco-sticker-image sticker 'completion)))
        (concat (appkit-media-image-display-string image " ") " ")
      "")))

(defun disco-sticker-image-slice-rows (sticker)
  "Return STICKER image as Appkit-owned line slice rows, or nil."
  (when-let* ((image (disco-sticker-image sticker 'timeline)))
    (appkit-media-image-slice-rows image)))

(defun disco-sticker--available-p (sticker)
  "Return non-nil when STICKER is usable according to Discord metadata."
  (or (not (assq 'available sticker))
      (eq (alist-get 'available sticker) t)))

(defun disco-sticker--guild-name (guild-id)
  "Return cached name for GUILD-ID, or nil."
  (when-let* ((guild
               (seq-find
                (lambda (item)
                  (equal guild-id
                         (disco-sticker--normalize-id
                          (and (listp item) (alist-get 'id item)))))
                (disco-state-guilds))))
    (let ((name (alist-get 'name guild)))
      (and (stringp name) name))))

(defun disco-sticker--candidate
    (sticker group &optional guild-id guild-name pack-name)
  "Return internal visual candidate for STICKER in GROUP."
  (when (and (disco-sticker-id sticker)
             (disco-sticker-format-type sticker)
             (disco-sticker--available-p sticker))
    (let ((name (disco-sticker-name sticker))
          (tags (and (stringp (alist-get 'tags sticker))
                     (alist-get 'tags sticker))))
      (list :label name
            :insert (disco-sticker-id sticker)
            :group group
            :annotation (if-let* ((scope (or pack-name guild-name)))
                            (format "  %s" scope)
                          "")
            :sticker (copy-tree sticker)
            :sticker-id (disco-sticker-id sticker)
            :sticker-name name
            :sticker-tags tags
            :sticker-guild-id guild-id
            :sticker-guild-name guild-name
            :sticker-pack-name pack-name))))

(defun disco-sticker--base-candidates (guild-id)
  "Return current GUILD-ID and standard-pack internal candidates."
  (let ((guild-name (disco-sticker--guild-name guild-id))
        candidates)
    (dolist (sticker (and guild-id (disco-state-guild-stickers guild-id)))
      (when-let* ((candidate
                  (disco-sticker--candidate
                   sticker "This Server" guild-id guild-name)))
        (push candidate candidates)))
    (dolist (pack (disco-state-standard-sticker-packs))
      (let* ((raw-name (and (listp pack) (alist-get 'name pack)))
             (pack-name
              (and (stringp raw-name)
                   (not (string-empty-p (string-trim raw-name)))
                   (string-trim raw-name)))
             (group (if pack-name
                        (format "Standard · %s" pack-name)
                      "Standard Stickers")))
        (dolist (sticker
                 (disco-sticker--normalize-list-sequence
                  (and (listp pack) (alist-get 'stickers pack))))
          (when-let* ((candidate
                      (disco-sticker--candidate
                       sticker group nil nil pack-name)))
            (push candidate candidates)))))
    (nreverse candidates)))

(defun disco-sticker--section (ids source-table group)
  "Resolve sticker IDS from SOURCE-TABLE into copied candidate GROUP."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (id ids)
      (when-let* ((normalized (disco-sticker--normalize-id id))
                  (candidate (gethash normalized source-table)))
        (unless (gethash normalized seen)
          (puthash normalized t seen)
          (let ((copy (copy-sequence candidate)))
            (setq copy (plist-put copy :group group))
            (push copy out)))))
    (nreverse out)))

(defun disco-sticker--frecency-ids ()
  "Return sticker setting keys ordered by descending frecency."
  (let ((entries (disco-settings-sticker-frecency)))
    (setq entries
          (sort entries
                (lambda (left right)
                  (let* ((left-item (cdr left))
                         (right-item (cdr right))
                         (left-score (or (plist-get left-item :score) -1))
                         (right-score (or (plist-get right-item :score) -1))
                         (left-total (or (plist-get left-item :total-uses) 0))
                         (right-total (or (plist-get right-item :total-uses) 0)))
                    (if (= left-score right-score)
                        (> left-total right-total)
                      (> left-score right-score))))))
    (mapcar #'car
            (seq-take entries (max 0 disco-sticker-frecency-limit)))))

(defun disco-sticker--unique-labels (candidates)
  "Return CANDIDATES with globally unique visual labels."
  (let ((seen (make-hash-table :test #'equal))
        out)
    (dolist (candidate candidates)
      (let* ((copy (copy-sequence candidate))
             (base (or (plist-get copy :label) "Sticker"))
             (id (or (plist-get copy :sticker-id) ""))
             (short-id (substring id (max 0 (- (length id) 6))))
             (label base)
             (index 1))
        (while (gethash label seen)
          (setq label
                (format "%s · %s%s"
                        base short-id
                        (if (> index 1) (format "#%d" index) "")))
          (setq index (1+ index)))
        (puthash label t seen)
        (setq copy (plist-put copy :label label))
        (push copy out)))
    (nreverse out)))

(defun disco-sticker--appkit-candidate (candidate)
  "Adapt internal sticker CANDIDATE to Appkit completion."
  (let ((sticker (plist-get candidate :sticker)))
    (appkit-chat-completion-candidate-create
     :label (plist-get candidate :label)
     :insert (plist-get candidate :insert)
     :search-terms
     (seq-filter
      (lambda (value) (and (stringp value) (not (string-empty-p value))))
      (list (plist-get candidate :sticker-name)
            (plist-get candidate :sticker-id)
            (plist-get candidate :sticker-tags)
            (plist-get candidate :sticker-pack-name)
            (plist-get candidate :sticker-guild-name)
            (plist-get candidate :sticker-guild-id)))
     :prefix (disco-sticker-completion-prefix sticker)
     :group (plist-get candidate :group)
     :annotation (plist-get candidate :annotation)
     :value candidate)))

(defun disco-sticker-candidates (&optional guild-id ranked-only)
  "Return visual sticker candidates for GUILD-ID.

When RANKED-ONLY is non-nil, include only Favorite and Frequently Used
sections."
  (setq guild-id (disco-sticker--normalize-id guild-id))
  (let* ((base (disco-sticker--base-candidates guild-id))
         (source-table (make-hash-table :test #'equal))
         candidates)
    (dolist (candidate base)
      (puthash (plist-get candidate :sticker-id) candidate source-table))
    (setq candidates
          (append
           (disco-sticker--section
            (disco-settings-favorite-stickers)
            source-table
            "Favorite Stickers")
           (disco-sticker--section
            (disco-sticker--frecency-ids)
            source-table
            "Frequently Used Stickers")
           (unless ranked-only base)))
    (mapcar #'disco-sticker--appkit-candidate
            (disco-sticker--unique-labels candidates))))

(defun disco-sticker--read-visual-owned (prompt candidates)
  "Read from CANDIDATES under PROMPT and own only that minibuffer."
  (let (picker-buffer setup)
    (setq setup
          (lambda ()
            (setq picker-buffer (current-buffer))
            (setq-local disco-sticker--picker-owner-p t)
            (remove-hook 'minibuffer-setup-hook setup)))
    (unwind-protect
        (let ((minibuffer-setup-hook
               (cons setup minibuffer-setup-hook)))
          (appkit-chat-completion-read-visual
           prompt candidates :history 'disco-sticker--history))
      (when (buffer-live-p picker-buffer)
        (with-current-buffer picker-buffer
          (setq-local disco-sticker--picker-owner-p nil))))))

(defun disco-sticker-read (&optional guild-id ranked-only)
  "Read one sticker for GUILD-ID.

When RANKED-ONLY is non-nil, offer only Favorite and Frequently Used
sections."
  (let ((candidates (disco-sticker-candidates guild-id ranked-only)))
    (unless candidates
      (user-error
       (if ranked-only
           "disco: no favorite or frequently used stickers"
         "disco: no available stickers in the loaded catalog")))
    (let* ((candidate
            (disco-sticker--read-visual-owned
             (if ranked-only "Favorite/recent sticker" "Send sticker")
             candidates))
           (value (appkit-chat-completion-candidate-value candidate))
           (sticker (plist-get value :sticker)))
      (unless (and (listp sticker) (disco-sticker-id sticker))
        (error "disco: sticker candidate has no exact identity"))
      (copy-tree sticker))))

(defun disco-sticker--refresh-active-picker (_resources)
  "Refresh an active visual picker after sticker resources change."
  (when-let* ((window (active-minibuffer-window))
              (buffer (window-buffer window))
              ((buffer-local-value 'disco-sticker--picker-owner-p buffer)))
    (run-at-time
     0 nil
     (lambda (owner)
       (when (and (buffer-live-p owner)
                  (buffer-local-value
                   'disco-sticker--picker-owner-p owner)
                  (eq owner
                      (and (active-minibuffer-window)
                           (window-buffer (active-minibuffer-window)))))
         (with-current-buffer owner
           (if (and (fboundp 'vertico--exhibit)
                    (bound-and-true-p vertico--candidates-ov))
               (funcall (symbol-function 'vertico--exhibit))
             (redisplay t)))))
     buffer)))

(add-hook 'disco-sticker-resources-updated-hook
          #'disco-sticker--refresh-active-picker)

(defun disco-sticker--cancel-timer (timer)
  "Cancel TIMER while isolating ordinary cancellation failures."
  (when (timerp timer)
    (condition-case nil
        (cancel-timer timer)
      ((error quit) nil))))

(defun disco-sticker-clear-image-memory ()
  "Forget display descriptors while retaining downloaded sticker files."
  (clrhash disco-sticker--images))

(defun disco-sticker--reset-media-state ()
  "Cancel sticker media work and clear its in-memory caches."
  (cl-incf disco-sticker--generation)
  (let ((timer disco-sticker--resource-update-timer))
    (setq disco-sticker--resource-update-timer nil)
    (disco-sticker--cancel-timer timer))
  (maphash
   (lambda (_variant owner)
     (when-let* ((handle (plist-get owner :handle)))
       (condition-case nil
           (appkit-media-cancel-transfer handle)
         ((error quit) nil)))
     (when-let* ((process (plist-get owner :process)))
       (when (process-live-p process)
         (ignore-errors (delete-process process)))))
   disco-sticker--fetching)
  (clrhash disco-sticker--fetching)
  (clrhash disco-sticker--files)
  (clrhash disco-sticker--images)
  (clrhash disco-sticker--pending-resource-updates))

(defun disco-sticker-reset ()
  "Retire account-scoped sticker work and clear in-memory state."
  (clrhash disco-sticker--catalog-requests)
  (disco-sticker--reset-media-state)
  (clrhash disco-sticker--known)
  (setq disco-sticker--history nil))

(defun disco-sticker-clear-cache ()
  "Clear sticker memory and disk caches, then refresh dependents."
  (interactive)
  (let (known)
    (maphash (lambda (variant _present) (push variant known))
             disco-sticker--known)
    (disco-sticker--reset-media-state)
    (clrhash disco-sticker--known)
    (when (file-directory-p disco-sticker-cache-directory)
      (delete-directory disco-sticker-cache-directory t))
    (dolist (variant known)
      (disco-sticker--schedule-resource-update variant)))
  (message "disco: sticker cache cleared"))

(provide 'disco-sticker)

;;; disco-sticker.el ends here
