;;; disco-avatar.el --- Shared Discord user avatars -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Discord user identity, CDN addressing, avatar acquisition, retry policy,
;; caches, and account lifecycle.  Views remain responsible for adapting their
;; domain objects to Discord user alists and for mapping resource notifications
;; onto their own projected rows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'plz)
(require 'svg nil t)
(require 'appkit-media)
(require 'disco-customize)

(defcustom disco-avatar-show-images t
  "When non-nil, render Discord user avatars as inline images when possible."
  :type 'boolean
  :group 'disco)

(defcustom disco-avatar-image-size 28
  "Pixel size used when decoding shared Discord avatar images."
  :type 'integer
  :group 'disco)

(defcustom disco-avatar-cache-directory
  (locate-user-emacs-file "disco-avatar-cache/")
  "Directory used to cache downloaded Discord user avatars."
  :type 'directory
  :group 'disco)

(defcustom disco-avatar-fetch-concurrency 6
  "Maximum concurrent Discord avatar downloads in one plz queue."
  :type 'integer
  :group 'disco)

(defcustom disco-avatar-retry-delays '(1 3 10 30 120)
  "Backoff delays in seconds for transient avatar fetch failures.

After all entries are used, the final delay remains the retry ceiling."
  :type '(repeat number)
  :group 'disco)

(defcustom disco-avatar-invalidation-delay 0.05
  "Seconds used to coalesce avatar resource notifications."
  :type 'number
  :group 'disco)

(defvar disco-avatar-resources-updated-hook nil
  "Hook run after shared Discord avatar resources change.

Each hook function receives one coalesced list of opaque resource keys.  Every
resource currently has the shape `(:avatar CACHE-KEY)'.  Consumers must map
those identities to their own projection keys; this module never knows view or
row identities.")

(defvar disco-avatar--image-cache (make-hash-table :test #'equal)
  "Successful raw image cache keyed by avatar cache key.")

(defvar disco-avatar--rounded-image-cache (make-hash-table :test #'equal)
  "Generic circular image cache keyed by file, size, and mtime.")

(defvar disco-avatar--fetching (make-hash-table :test #'equal)
  "Exact active fetch owner keyed by avatar cache key.")

(defvar disco-avatar--failures (make-hash-table :test #'equal)
  "Avatar failure state keyed by avatar cache key.

Each value records attempt count, retry timing, source URL, cache base, HTTP
status, and the last readable error.  Permanent failures remain until an
explicit cache clear or account reset.")

(defvar disco-avatar--known-keys (make-hash-table :test #'equal)
  "Logical avatar cache keys observed in the current account session.")

(defvar disco-avatar--pending-resource-updates
  (make-hash-table :test #'equal)
  "Resource keys awaiting one coalesced update notification.")

(defvar disco-avatar--retry-timer nil
  "Timer for the next due avatar retry.")

(defvar disco-avatar--resource-update-timer nil
  "Timer coalescing successful avatar resource notifications.")

(defvar disco-avatar--fetch-generation 0
  "Generation revoking callbacks from retired avatar fetch batches.")

(defvar disco-avatar--reset-in-progress nil
  "Non-nil while account-scoped avatar work is being retired.")

(defvar disco-avatar--plz-queue nil
  "Current plz queue used for asynchronous avatar downloads.")

(defvar disco-avatar--plz-queue-limit nil
  "Concurrency limit used to construct `disco-avatar--plz-queue'.")

(defvar disco-avatar--plz-queues nil
  "All avatar queues that can still own requests in this generation.")

(defconst disco-avatar--cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "img")
  "Preferred Discord avatar cache file extension candidates.")

(defun disco-avatar--string-present-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun disco-avatar--user-id (user)
  "Return normalized Discord ID from USER, or nil."
  (let ((value (and (listp user) (alist-get 'id user))))
    (cond
     ((disco-avatar--string-present-p value) value)
     ((integerp value) (number-to-string value))
     (t nil))))

(defun disco-avatar--user-avatar-hash (user)
  "Return a non-empty Discord avatar hash from USER, or nil."
  (let ((value (and (listp user) (alist-get 'avatar user))))
    (and (disco-avatar--string-present-p value) value)))

(defun disco-avatar--default-index (user)
  "Return Discord's default avatar bucket for USER."
  (let* ((user-id (disco-avatar--user-id user))
         (raw-discriminator (and (listp user)
                                 (alist-get 'discriminator user)))
         (discriminator
          (cond
           ((stringp raw-discriminator) raw-discriminator)
           ((integerp raw-discriminator)
            (number-to-string raw-discriminator))
           (t nil))))
    (cond
     ((and (disco-avatar--string-present-p discriminator)
           (not (string= discriminator "0"))
           (string-match-p "\\`[0-9]+\\'" discriminator))
      (mod (string-to-number discriminator) 5))
     ((and user-id (string-match-p "\\`[0-9]+\\'" user-id))
      (mod (ash (string-to-number user-id) -22) 6))
     (t 0))))

(defun disco-avatar-url (user)
  "Return the Discord CDN avatar URL for USER, or nil."
  (let ((user-id (disco-avatar--user-id user))
        (avatar-hash (disco-avatar--user-avatar-hash user)))
    (cond
     ((and user-id avatar-hash)
      (format "https://cdn.discordapp.com/avatars/%s/%s.png?size=64"
              user-id avatar-hash))
     (user-id
      (format "https://cdn.discordapp.com/embed/avatars/%d.png"
              (disco-avatar--default-index user)))
     (t nil))))

(defun disco-avatar-cache-key (user)
  "Return the stable shared avatar cache key for Discord USER, or nil."
  (when-let* ((user-id (disco-avatar--user-id user)))
    (format "%s:%s:%s"
            user-id
            (or (disco-avatar--user-avatar-hash user)
                (format "default-%d" (disco-avatar--default-index user)))
            disco-avatar-image-size)))

(defun disco-avatar-resource-key (user)
  "Return the opaque avatar resource key for Discord USER, or nil."
  (when-let* ((cache-key (disco-avatar-cache-key user)))
    (list :avatar cache-key)))

(defun disco-avatar--note-known-key (cache-key)
  "Record CACHE-KEY as account-scoped avatar state and return it."
  (when cache-key
    (puthash cache-key t disco-avatar--known-keys))
  cache-key)

(defun disco-avatar--cache-file-base (cache-key)
  "Return avatar cache file base for CACHE-KEY, without extension."
  (expand-file-name (md5 cache-key) disco-avatar-cache-directory))

(defun disco-avatar--cache-file (cache-key extension)
  "Return avatar cache file for CACHE-KEY and EXTENSION."
  (format "%s.%s" (disco-avatar--cache-file-base cache-key) extension))

(defun disco-avatar--cached-file-for-key (cache-key)
  "Return an existing avatar file for CACHE-KEY, or nil."
  (and cache-key
       (seq-find #'file-exists-p
                 (mapcar (lambda (extension)
                           (disco-avatar--cache-file cache-key extension))
                         disco-avatar--cache-extensions))))

(defun disco-avatar-cached-file (user)
  "Return the existing shared avatar cache file for USER, or nil."
  (when-let* ((cache-key (disco-avatar-cache-key user)))
    (disco-avatar--note-known-key cache-key)
    (disco-avatar--cached-file-for-key cache-key)))

(defun disco-avatar--ensure-queue ()
  "Return the active avatar fetch queue for current concurrency settings."
  (let ((limit (max 1 disco-avatar-fetch-concurrency)))
    (when (or (null disco-avatar--plz-queue)
              (not (equal disco-avatar--plz-queue-limit limit)))
      (setq disco-avatar--plz-queue (make-plz-queue :limit limit)
            disco-avatar--plz-queue-limit limit)
      (push disco-avatar--plz-queue disco-avatar--plz-queues))
    disco-avatar--plz-queue))

(defun disco-avatar--delete-stale-cache-files (cache-base)
  "Delete cached avatar siblings rooted at CACHE-BASE."
  (dolist (extension disco-avatar--cache-extensions)
    (let ((file (format "%s.%s" cache-base extension)))
      (when (file-exists-p file)
        (ignore-errors (delete-file file))))))

(defun disco-avatar--retry-delay (attempts)
  "Return retry delay in seconds for ATTEMPTS transient failures."
  (let ((delays (seq-filter #'numberp disco-avatar-retry-delays)))
    (if delays
        (max 0.05
             (float
              (nth (min (max 0 (1- attempts))
                        (1- (length delays)))
                   delays)))
      30.0)))

(defun disco-avatar--transient-http-status-p (status)
  "Return non-nil when HTTP STATUS should be retried."
  (or (not (integerp status))
      (<= status 0)
      (memq status '(408 425 429))
      (>= status 500)))

(defun disco-avatar--plz-error-object (error-data)
  "Return the plz error object carried by ERROR-DATA, or nil."
  (cond
   ((and (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p error-data)))
    error-data)
   ((and (consp error-data)
         (symbolp (car error-data))
         (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p (cadr error-data))))
    (cadr error-data))
   (t nil)))

(defun disco-avatar--error-http-status (error-data)
  "Return HTTP status from ERROR-DATA, or nil if unavailable."
  (or (and (listp error-data) (plist-get error-data :status))
      (when-let* ((plz-error (disco-avatar--plz-error-object error-data)))
        (when (and (fboundp 'plz-error-response)
                   (fboundp 'plz-response-status))
          (let ((response (ignore-errors (plz-error-response plz-error))))
            (and response
                 (ignore-errors (plz-response-status response))))))))

(defun disco-avatar--error-message (error-data)
  "Return a readable message extracted from asynchronous ERROR-DATA."
  (or (and (listp error-data) (plist-get error-data :message))
      (and (listp error-data)
           (plist-get error-data :status)
           (format "HTTP %s" (plist-get error-data :status)))
      (when-let* ((plz-error (disco-avatar--plz-error-object error-data)))
        (let ((message (and (fboundp 'plz-error-message)
                            (ignore-errors
                              (plz-error-message plz-error))))
              (status (disco-avatar--error-http-status error-data)))
          (cond
           ((disco-avatar--string-present-p message)
            (if status
                (format "%s (HTTP %s)" message status)
              message))
           (status (format "HTTP %s" status))
           (t nil))))
      (condition-case nil
          (error-message-string error-data)
        (error (format "%S" error-data)))))

(defun disco-avatar--owner-current-p (cache-key owner)
  "Return non-nil when OWNER still owns CACHE-KEY in this account session."
  (and (not disco-avatar--reset-in-progress)
       (eq owner (gethash cache-key disco-avatar--fetching))
       (= (or (plist-get owner :generation) -1)
          disco-avatar--fetch-generation)))

(defun disco-avatar--flush-resource-updates ()
  "Publish one coalesced batch of changed avatar resources."
  (setq disco-avatar--resource-update-timer nil)
  (let (resources)
    (maphash (lambda (resource _present) (push resource resources))
             disco-avatar--pending-resource-updates)
    (clrhash disco-avatar--pending-resource-updates)
    (when (and resources (not disco-avatar--reset-in-progress))
      (run-hook-with-args 'disco-avatar-resources-updated-hook
                          (nreverse resources)))))

(defun disco-avatar--schedule-resource-update (cache-key)
  "Coalesce an update notification for avatar CACHE-KEY."
  (unless disco-avatar--reset-in-progress
    (puthash (list :avatar cache-key) t
             disco-avatar--pending-resource-updates)
    (unless (timerp disco-avatar--resource-update-timer)
      (setq disco-avatar--resource-update-timer
            (run-at-time (max 0.0 disco-avatar-invalidation-delay)
                         nil #'disco-avatar--flush-resource-updates)))))

(defun disco-avatar--run-due-retries ()
  "Start avatar retries whose backoff period has elapsed."
  (setq disco-avatar--retry-timer nil)
  (unless disco-avatar--reset-in-progress
    (let ((now (float-time)) due)
      (maphash
       (lambda (cache-key failure)
         (when (and (not (plist-get failure :permanent))
                    (not (gethash cache-key disco-avatar--fetching))
                    (<= (or (plist-get failure :retry-at) 0) now))
           (push (cons cache-key failure) due)))
       disco-avatar--failures)
      (dolist (item due)
        (disco-avatar--start-fetch
         (car item)
         (plist-get (cdr item) :url)
         (plist-get (cdr item) :cache-base)))
      (disco-avatar--schedule-next-retry))))

(defun disco-avatar--schedule-next-retry ()
  "Schedule the earliest pending transient avatar retry."
  (when (timerp disco-avatar--retry-timer)
    (cancel-timer disco-avatar--retry-timer))
  (setq disco-avatar--retry-timer nil)
  (let (next-at)
    (maphash
     (lambda (cache-key failure)
       (let ((retry-at (plist-get failure :retry-at)))
         (when (and (numberp retry-at)
                    (not (plist-get failure :permanent))
                    (not (gethash cache-key disco-avatar--fetching))
                    (or (null next-at) (< retry-at next-at)))
           (setq next-at retry-at))))
     disco-avatar--failures)
    (when (and next-at (not disco-avatar--reset-in-progress))
      (setq disco-avatar--retry-timer
            (run-at-time (max 0.05 (- next-at (float-time)))
                         nil #'disco-avatar--run-due-retries)))))

(defun disco-avatar--complete-fetch (cache-key owner image)
  "Publish successful IMAGE for CACHE-KEY while OWNER remains current."
  (when (disco-avatar--owner-current-p cache-key owner)
    (puthash cache-key image disco-avatar--image-cache)
    (remhash cache-key disco-avatar--failures)
    (remhash cache-key disco-avatar--fetching)
    (disco-avatar--schedule-next-retry)
    (disco-avatar--schedule-resource-update cache-key)
    image))

(defun disco-avatar--record-failure
    (cache-key owner reason retry-p &optional target-file status)
  "Record OWNER's avatar failure for CACHE-KEY.

REASON is readable text.  RETRY-P controls backoff.  Delete TARGET-FILE when
non-nil, and retain optional HTTP STATUS for diagnostics."
  (when (disco-avatar--owner-current-p cache-key owner)
    (when (and target-file (file-exists-p target-file))
      (ignore-errors (delete-file target-file)))
    (let* ((previous (gethash cache-key disco-avatar--failures))
           (attempts (1+ (or (plist-get previous :attempts) 0)))
           (delay (and retry-p (disco-avatar--retry-delay attempts)))
           (failure
            (list :attempts attempts
                  :reason reason
                  :status status
                  :url (plist-get owner :url)
                  :cache-base (plist-get owner :cache-base)
                  :permanent (not retry-p)
                  :retry-at (and delay (+ (float-time) delay)))))
      (puthash cache-key failure disco-avatar--failures)
      (remhash cache-key disco-avatar--fetching)
      (message "disco: avatar %s failed: %s%s"
               (substring (md5 cache-key) 0 8)
               reason
               (if delay (format "; retrying in %.1fs" delay) ""))
      (disco-avatar--schedule-next-retry)
      failure)))

(defun disco-avatar--image-from-file (file)
  "Create a shared inline avatar image from FILE, or nil."
  (let ((image
         (ignore-errors
           (create-image file nil nil
                         :width disco-avatar-image-size
                         :height disco-avatar-image-size
                         :ascent 'center))))
    (unless (appkit-media-image-object-valid-p image)
      (when (image-type-available-p 'imagemagick)
        (setq image
              (ignore-errors
                (create-image file 'imagemagick nil
                              :width disco-avatar-image-size
                              :height disco-avatar-image-size
                              :ascent 'center)))))
    (and (appkit-media-image-object-valid-p image) image)))

(defun disco-avatar--start-fetch (cache-key url cache-base)
  "Fetch CACHE-KEY asynchronously from URL into CACHE-BASE."
  (unless (or disco-avatar--reset-in-progress
              (not (appkit-media-url-present-p url))
              (not (disco-avatar--string-present-p cache-base))
              (gethash cache-key disco-avatar--fetching)
              (gethash cache-key disco-avatar--image-cache))
    (let* ((generation disco-avatar--fetch-generation)
           (owner (list :generation generation
                        :url url
                        :cache-base cache-base))
           (queue (disco-avatar--ensure-queue)))
      (puthash cache-key owner disco-avatar--fetching)
      (condition-case setup-error
          (progn
            (plz-queue
              queue 'get url
              :headers appkit-media-image-accept-headers
              :as 'binary
              :noquery t
              :then
              (lambda (data)
                (when (disco-avatar--owner-current-p cache-key owner)
                  (condition-case decode-error
                      (let* ((raw-bytes
                              (appkit-media-normalize-image-bytes
                               (if (multibyte-string-p data)
                                   (encode-coding-string data 'binary)
                                 data)))
                             (extension
                              (appkit-media-bytes-to-extension raw-bytes "img"))
                             (target-file (format "%s.%s" cache-base extension)))
                        (disco-avatar--delete-stale-cache-files cache-base)
                        (make-directory (file-name-directory target-file) t)
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert raw-bytes)
                          (let ((coding-system-for-write 'binary))
                            (write-region (point-min) (point-max)
                                          target-file nil 'silent)))
                        (let ((image
                               (disco-avatar--image-from-file target-file)))
                          (if image
                              (disco-avatar--complete-fetch
                               cache-key owner image)
                            (disco-avatar--record-failure
                             cache-key owner
                             "response was not a renderable image"
                             t target-file))))
                    (error
                     (disco-avatar--record-failure
                      cache-key owner (error-message-string decode-error) t)))))
              :else
              (lambda (fetch-error)
                (when (disco-avatar--owner-current-p cache-key owner)
                  (let ((status
                         (disco-avatar--error-http-status fetch-error)))
                    (disco-avatar--record-failure
                     cache-key owner
                     (disco-avatar--error-message fetch-error)
                     (disco-avatar--transient-http-status-p status)
                     nil status)))))
            (plz-run queue))
        (error
         (disco-avatar--record-failure
          cache-key owner (disco-avatar--error-message setup-error) t))))))

(defun disco-avatar--rendering-available-p ()
  "Return non-nil when shared avatars can render in the current context."
  (and disco-avatar-show-images
       (not disco-avatar--reset-in-progress)
       (appkit-media-inline-image-rendering-available-p)))

(defun disco-avatar-image (user)
  "Return USER's shared raw avatar image, scheduling fetch when needed."
  (when (disco-avatar--rendering-available-p)
    (when-let* ((cache-key (disco-avatar-cache-key user)))
      (disco-avatar--note-known-key cache-key)
      (let ((cached (gethash cache-key disco-avatar--image-cache)))
        (cond
         ((appkit-media-image-object-valid-p cached) cached)
         (t
          (when cached
            (remhash cache-key disco-avatar--image-cache))
          (let* ((url (disco-avatar-url user))
                 (cache-base (disco-avatar--cache-file-base cache-key))
                 (cache-file (disco-avatar--cached-file-for-key cache-key))
                 (file-image (and cache-file
                                  (disco-avatar--image-from-file cache-file)))
                 (failure (gethash cache-key disco-avatar--failures)))
            (cond
             (file-image
              (puthash cache-key file-image disco-avatar--image-cache)
              (remhash cache-key disco-avatar--failures)
              file-image)
             ((plist-get failure :permanent) nil)
             ((and (numberp (plist-get failure :retry-at))
                   (> (plist-get failure :retry-at) (float-time)))
              nil)
             ((appkit-media-url-present-p url)
              (when cache-file
                (ignore-errors (delete-file cache-file)))
              (disco-avatar--start-fetch cache-key url cache-base)
              nil)
             (t nil)))))))))

(defun disco-avatar--image-mime-type (file)
  "Return image MIME type for FILE, or nil."
  (pcase (downcase (or (file-name-extension file) ""))
    ("png" "image/png")
    ((or "jpg" "jpeg") "image/jpeg")
    ("gif" "image/gif")
    ("webp" "image/webp")
    (_ nil)))

(defun disco-avatar--rounded-image-from-file (file pixel-size)
  "Return a cached circular avatar from FILE at PIXEL-SIZE, or nil."
  (when (and (stringp file)
             (file-readable-p file)
             (integerp pixel-size)
             (> pixel-size 0)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-circle)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (let* ((attributes (file-attributes file))
           (mtime (and attributes
                       (file-attribute-modification-time attributes)))
           (cache-key (format "%s:%s:%s" file pixel-size mtime))
           (cached (gethash cache-key disco-avatar--rounded-image-cache)))
      (or cached
          (let* ((mime (disco-avatar--image-mime-type file))
                 (svg (and mime (svg-create pixel-size pixel-size))))
            (when svg
              (let* ((radius (/ (float pixel-size) 2.0))
                     (clip (svg-clip-path svg :id "clip")))
                (svg-circle clip radius radius radius)
                (svg-embed svg file mime nil
                           :x 0 :y 0
                           :width pixel-size :height pixel-size
                           :clip-path "url(#clip)")
                (let ((image (svg-image svg
                                        :ascent 'center
                                        :width pixel-size
                                        :height pixel-size)))
                  (when (appkit-media-image-object-valid-p image)
                    (puthash cache-key image
                             disco-avatar--rounded-image-cache)
                    image)))))))))

(defun disco-avatar--resize-image (image pixel-size)
  "Return IMAGE normalized to square PIXEL-SIZE, or nil."
  (when (appkit-media-image-object-valid-p image)
    (let ((properties (copy-sequence (cdr image))))
      (setq properties (plist-put properties :width pixel-size))
      (setq properties (plist-put properties :height pixel-size))
      (setq properties (plist-put properties :ascent 'center))
      (cons (car image) properties))))

(defun disco-avatar-rounded-image (user pixel-size)
  "Return USER's circular image at PIXEL-SIZE, scheduling fetch when needed."
  (when (and (integerp pixel-size) (> pixel-size 0))
    (let ((raw-image (disco-avatar-image user)))
      (when (appkit-media-image-object-valid-p raw-image)
        (let ((cache-file (disco-avatar-cached-file user)))
          (or (and cache-file
                   (disco-avatar--rounded-image-from-file
                    cache-file pixel-size))
              (disco-avatar--resize-image raw-image pixel-size)))))))

(defun disco-avatar--cancel-timer (timer)
  "Cancel TIMER while isolating ordinary cancellation failures."
  (when (timerp timer)
    (condition-case nil
        (cancel-timer timer)
      ((error quit) nil))))

(defun disco-avatar--clear-queue (queue)
  "Cancel all work in avatar QUEUE while isolating ordinary failures."
  (when queue
    (condition-case nil
        (plz-clear queue)
      ((error quit) nil))))

(defun disco-avatar--run-cleanups (functions)
  "Run every zero-argument cleanup in FUNCTIONS despite nonlocal exits."
  (when functions
    (unwind-protect
        (funcall (car functions))
      (disco-avatar--run-cleanups (cdr functions)))))

(defun disco-avatar--reset-fetch-state ()
  "Revoke callbacks, cancel avatar work, and clear transient bookkeeping."
  (let ((disco-avatar--reset-in-progress t)
        (queues (delete-dups
                 (delq nil (cons disco-avatar--plz-queue
                                 (copy-sequence disco-avatar--plz-queues)))))
        (retry-timer disco-avatar--retry-timer)
        (resource-timer disco-avatar--resource-update-timer))
    ;; Revoke exact owners before cancellation can synchronously invoke a
    ;; sentinel or completion callback.
    (cl-incf disco-avatar--fetch-generation)
    (setq disco-avatar--plz-queue nil
          disco-avatar--plz-queue-limit nil
          disco-avatar--plz-queues nil
          disco-avatar--retry-timer nil
          disco-avatar--resource-update-timer nil)
    (clrhash disco-avatar--fetching)
    (unwind-protect
        (disco-avatar--run-cleanups
         (append
          (mapcar (lambda (queue)
                    (lambda () (disco-avatar--clear-queue queue)))
                  queues)
          (list (lambda () (disco-avatar--cancel-timer retry-timer))
                (lambda () (disco-avatar--cancel-timer resource-timer)))))
      (clrhash disco-avatar--fetching)
      (clrhash disco-avatar--failures)
      (clrhash disco-avatar--pending-resource-updates))))

(defun disco-avatar--clear-session-memory ()
  "Clear account-scoped avatar memory without callbacks or cancellation."
  (setq disco-avatar--plz-queue nil
        disco-avatar--plz-queue-limit nil
        disco-avatar--plz-queues nil
        disco-avatar--retry-timer nil
        disco-avatar--resource-update-timer nil)
  (clrhash disco-avatar--image-cache)
  (clrhash disco-avatar--rounded-image-cache)
  (clrhash disco-avatar--fetching)
  (clrhash disco-avatar--failures)
  (clrhash disco-avatar--known-keys)
  (clrhash disco-avatar--pending-resource-updates))

(defun disco-avatar-reset-session-state ()
  "Destructively retire account-scoped avatar state without redrawing."
  (let ((disco-avatar--reset-in-progress t))
    (unwind-protect
        (disco-avatar--reset-fetch-state)
      ;; Repeat pure clears after cancellation hooks so reentrant writes cannot
      ;; retain data from the retired account.
      (disco-avatar--clear-session-memory))))

(defun disco-avatar--known-cache-keys ()
  "Return a snapshot of logical avatar keys known in this session."
  (let (keys)
    (maphash (lambda (key _present) (push key keys))
             disco-avatar--known-keys)
    keys))

(defun disco-avatar--publish-known-keys (keys)
  "Schedule resource updates for logical avatar KEYS."
  (dolist (cache-key keys)
    (disco-avatar--schedule-resource-update cache-key)))

(defun disco-avatar-clear-cache ()
  "Clear shared in-memory avatar state and refresh dependent projections."
  (interactive)
  (let ((keys (disco-avatar--known-cache-keys)))
    (disco-avatar--reset-fetch-state)
    (clrhash disco-avatar--image-cache)
    (clrhash disco-avatar--rounded-image-cache)
    (disco-avatar--publish-known-keys keys))
  (message "disco: avatar cache cleared"))

(defun disco-avatar-refetch ()
  "Drop shared avatar memory and disk caches, then refresh dependents."
  (interactive)
  (let ((keys (disco-avatar--known-cache-keys)))
    (disco-avatar--reset-fetch-state)
    (clrhash disco-avatar--image-cache)
    (clrhash disco-avatar--rounded-image-cache)
    (when (file-directory-p disco-avatar-cache-directory)
      (delete-directory disco-avatar-cache-directory t))
    (disco-avatar--publish-known-keys keys))
  (message "disco: avatar cache reset; refetching"))

(provide 'disco-avatar)

;;; disco-avatar.el ends here
