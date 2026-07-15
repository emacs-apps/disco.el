;;; disco-media.el --- Discord attachment media adapter -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Adapt Discord attachments, spoilers, placeholders, downloads, voice
;; messages, and audio state to the protocol-neutral appkit media runtime.

;;; Code:

(require 'subr-x)
(require 'appkit-media)
(require 'cl-lib)
(require 'image)
(require 'dom)
(require 'svg nil t)

(defgroup disco-media nil
  "Media preview, placeholder, and spoiler rendering for disco."
  :group 'disco)

(defcustom disco-media-preview-cache-directory
  (locate-user-emacs-file "disco-attachment-cache/")
  "Directory used to cache downloaded attachment preview images."
  :type 'directory
  :group 'disco-media)

(defcustom disco-media-download-directory
  (locate-user-emacs-file "disco-attachment-downloads/")
  "Directory used for telega-style default attachment downloads."
  :type 'directory
  :group 'disco-media)

(defcustom disco-media-preview-max-fetches-per-render 4
  "Maximum attachment preview fetches started during one room render pass.

Set to nil to disable per-render capping."
  :type '(choice
          (const :tag "No per-render cap" nil)
          integer)
  :group 'disco-media)

(defcustom disco-media-show-previews t
  "When non-nil, render inline previews for image/video attachments."
  :type 'boolean
  :group 'disco-media)

(defcustom disco-media-audio-player-command
  (cond
   ((executable-find "ffplay") "ffplay -nodisp -autoexit")
   ((executable-find "mpv") "mpv --no-video")
   ((executable-find "vlc") "vlc --intf dummy --play-and-exit")
   (t nil))
  "Command used to play audio URLs/files from cards.

When this resolves to `ffplay', disco can track play/pause/progress inline in a
telega-style attachment card.  Other players are launched as best-effort
external commands without inline playback state.  An unavailable player is an
explicit error."
  :type '(choice
          (const :tag "No audio player" nil)
          (string :tag "Command line"))
  :group 'disco-media)

(defconst disco-media--attachment-flag-is-spoiler (ash 1 3)
  "Attachment flag bit marking spoiler attachments.")

(defvar disco-media--attachment-preview-image-cache (make-hash-table :test #'equal)
  "Preview image cache keyed by attachment preview cache key.

Values are image objects or the symbol `:missing'.")

(defvar disco-media--attachment-preview-fetching (make-hash-table :test #'equal)
  "Set of attachment preview cache keys currently being fetched.")

(defvar disco-media--attachment-preview-owner-table
  (make-hash-table :test #'equal)
  "Exact asynchronous preview owner keyed by preview cache key.")

(defvar disco-media--attachment-preview-fetch-budget nil
  "Dynamic cap for number of preview fetches started during one render pass.")

(defvar disco-media-rerender-function nil
  "Function called with KIND and KEY after media state/cache updates.")

(defvar disco-media--attachment-download-state-table (make-hash-table :test #'equal)
  "Attachment download state keyed by stable attachment download key.")

(defvar disco-media--attachment-download-owner-table
  (make-hash-table :test #'equal)
  "Exact asynchronous download owner keyed by attachment download key.")

(defvar disco-media--attachment-export-owners nil
  "Exact owners of concurrent explicit save-as attachment transfers.")

(defvar disco-media--attachment-audio-state-table (make-hash-table :test #'equal)
  "Inline audio playback state keyed by stable attachment download key.")

(defvar disco-media--attachment-audio-current-process nil
  "Current inline audio playback process, if any.")

(defvar disco-media--attachment-audio-current-owner nil
  "Exact owner of `disco-media--attachment-audio-current-process'.")

(defvar disco-media--attachment-external-audio-owners nil
  "Exact owners of non-inline attachment audio processes.")

(defvar disco-media--generation 0
  "Generation of the active account's asynchronous media work.")

(defvar disco-media--reset-in-progress nil
  "Non-nil while account-scoped media state is being destroyed.")

(defvar disco-media--attachment-waveform-image-cache (make-hash-table :test #'equal)
  "Cache of rendered audio waveform image objects.")

(defvar disco-media--attachment-placeholder-image-cache (make-hash-table :test #'equal)
  "Cache of decoded attachment placeholder image objects.")

(defvar disco-media--attachment-decorated-preview-cache (make-hash-table :test #'equal)
  "Cache of SVG-decorated preview images keyed by source/spec/mode.")

(defun disco-media--session-current-p (generation)
  "Return non-nil when GENERATION may still mutate active session state."
  (and (not disco-media--reset-in-progress)
       (integerp generation)
       (= generation disco-media--generation)))

(defun disco-media--start-allowed-p ()
  "Return non-nil when account-owned media work may be started."
  (not disco-media--reset-in-progress))

(defun disco-media--ensure-start-allowed ()
  "Reject creation of account-owned work during destructive reset."
  (unless (disco-media--start-allowed-p)
    (user-error "disco: media session reset is in progress")))

(defun disco-media--preview-owner-current-p (cache-key owner)
  "Return non-nil when OWNER still owns preview CACHE-KEY exactly."
  (and (consp owner)
       (disco-media--session-current-p (plist-get owner :generation))
       (eq owner (gethash cache-key disco-media--attachment-preview-owner-table))))

(defun disco-media--download-owner-current-p (key owner)
  "Return non-nil when OWNER still owns default download KEY exactly."
  (and (consp owner)
       (disco-media--session-current-p (plist-get owner :generation))
       (eq owner (gethash key disco-media--attachment-download-owner-table))
       (eq owner
           (plist-get (gethash key disco-media--attachment-download-state-table)
                      :owner))))

(defun disco-media--export-owner-current-p (owner)
  "Return non-nil when save-as transfer OWNER is still authoritative."
  (and (consp owner)
       (disco-media--session-current-p (plist-get owner :generation))
       (memq owner disco-media--attachment-export-owners)))

(defun disco-media--external-audio-owner-current-p (owner process)
  "Return non-nil when OWNER exactly owns external audio PROCESS."
  (and (consp owner)
       (processp process)
       (disco-media--session-current-p (plist-get owner :generation))
       (memq owner disco-media--attachment-external-audio-owners)
       (eq process (plist-get owner :process))))

(defun disco-media--audio-owner-current-p (process)
  "Return non-nil when PROCESS exactly owns active inline audio state."
  (when (processp process)
    (let* ((properties (process-plist process))
           (generation (plist-get properties :disco-media-generation))
           (owner (plist-get properties :disco-media-owner))
           (key (plist-get properties :attachment-key))
           (entry (and key
                       (gethash key disco-media--attachment-audio-state-table))))
      (and owner
           (disco-media--session-current-p generation)
           (eq owner disco-media--attachment-audio-current-owner)
           (eq process disco-media--attachment-audio-current-process)
           (eq owner (plist-get entry :owner))
           (eq process (plist-get entry :process))))))

(defun disco-media--run-cleanup-items (items function)
  "Apply FUNCTION to all ITEMS despite failures or a nonlocal transfer."
  (let ((remaining items)
        complete-p)
    (unwind-protect
        (progn
          (while remaining
            (let ((item (pop remaining)))
              (condition-case err
                  (funcall function item)
                (error
                 (message "disco: media cleanup failed: %s"
                          (error-message-string err)))
                (quit
                 (message "disco: media cleanup was interrupted")))))
          (setq complete-p t))
      ;; A cleanup hook may throw to a caller-owned catch.  Finish every
      ;; remaining privacy action while that transfer unwinds.
      (unless complete-p
        (disco-media--run-cleanup-items remaining function)))))

(defun disco-media--force-dispose-process-buffer (buffer)
  "Erase and kill exact process BUFFER without consulting its name."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Do this before dynamically binding `buffer-undo-list', so a failed
      ;; kill cannot restore an undo ring retaining erased account text.
      (buffer-disable-undo)
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (buffer-undo-list t)
            (kill-buffer-hook nil)
            (kill-buffer-query-functions nil)
            (buffer-offer-save nil)
            (quit-flag nil))
        (widen)
        (erase-buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

(defun disco-media--cancel-cleanup-item (item)
  "Cancel one account-owned media cleanup ITEM."
  (pcase (plist-get item :kind)
    ('video
     (appkit-media-cancel-video-preview (plist-get item :key)))
    ('transfer
     (appkit-media-cancel-transfer (plist-get item :transfer)))
    ('process
     (unwind-protect
         (let ((process (plist-get item :process)))
           (when (processp process)
             ;; The process object itself may outlive deletion in a caller.
             ;; Remove account keys/URLs before any synchronous sentinel.
             (unwind-protect
                 (set-process-plist process nil)
               (when (process-live-p process)
                 (delete-process process)))))
       (disco-media--force-dispose-process-buffer
        (plist-get item :buffer))))
    ('decoration
     (appkit-media-clear-video-decoration-cache 'disco))))

(defun disco-media--preview-cleanup-items ()
  "Return cleanup items for every currently owned preview operation."
  (let (keys transfers)
    (maphash
     (lambda (key fetching)
       (push (concat "disco:" key) keys)
       (when (appkit-media-transfer-p fetching)
         (push fetching transfers)))
     disco-media--attachment-preview-fetching)
    (maphash
     (lambda (key owner)
       (push (concat "disco:" key) keys)
       (when-let* ((transfer (plist-get owner :transfer)))
         (push transfer transfers)))
     disco-media--attachment-preview-owner-table)
    (append
     (mapcar (lambda (key) (list :kind 'video :key key))
             (delete-dups keys))
     (mapcar (lambda (transfer)
               (list :kind 'transfer :transfer transfer))
             (delete-dups transfers)))))

(defun disco-media--session-cleanup-items ()
  "Return cleanup items for tracked preview, transfer, and audio work."
  (let ((items (disco-media--preview-cleanup-items))
        transfers
        process-records)
    (maphash
     (lambda (_key entry)
       (when-let* ((transfer (plist-get entry :transfer)))
         (push transfer transfers))
       (when-let* ((process (plist-get entry :process)))
         (when (processp process)
           (push (cons process (plist-get entry :buffer)) process-records))))
     disco-media--attachment-download-state-table)
    (maphash
     (lambda (_key entry)
       (when-let* ((process (plist-get entry :process)))
         (when (processp process)
           (push (cons process (plist-get entry :buffer)) process-records))))
     disco-media--attachment-audio-state-table)
    (dolist (owner disco-media--attachment-export-owners)
      (when-let* ((transfer (plist-get owner :transfer)))
        (push transfer transfers)))
    (dolist (owner disco-media--attachment-external-audio-owners)
      (when-let* ((process (plist-get owner :process)))
        (when (processp process)
          (push (cons process (plist-get owner :buffer)) process-records))))
    (when (processp disco-media--attachment-audio-current-process)
      (push
       (cons
        disco-media--attachment-audio-current-process
        (plist-get (process-plist disco-media--attachment-audio-current-process)
                   :disco-media-owned-buffer))
       process-records))
    (let ((seen (make-hash-table :test #'eq))
          unique-records)
      (dolist (record process-records)
        (unless (gethash (car record) seen)
          (puthash (car record) t seen)
          (push record unique-records)))
    (setq items
          (append
           items
           (mapcar (lambda (transfer)
                     (list :kind 'transfer :transfer transfer))
                   (delete-dups transfers))
           (mapcar (lambda (record)
                     (list :kind 'process
                           :process (car record)
                           :buffer (cdr record)))
                   unique-records)
           (list (list :kind 'decoration))))
      items)))

(defun disco-media--clear-session-memory ()
  "Clear tracked attachment media state without running callbacks."
  (clrhash disco-media--attachment-preview-image-cache)
  (clrhash disco-media--attachment-preview-fetching)
  (clrhash disco-media--attachment-preview-owner-table)
  (clrhash disco-media--attachment-download-state-table)
  (clrhash disco-media--attachment-download-owner-table)
  (clrhash disco-media--attachment-audio-state-table)
  (clrhash disco-media--attachment-waveform-image-cache)
  (clrhash disco-media--attachment-placeholder-image-cache)
  (clrhash disco-media--attachment-decorated-preview-cache)
  (setq disco-media--attachment-preview-fetch-budget nil
        disco-media--attachment-export-owners nil
        disco-media--attachment-audio-current-process nil
        disco-media--attachment-audio-current-owner nil
        disco-media--attachment-external-audio-owners nil))

(defun disco-media-reset-session-state ()
  "Destroy tracked attachment media work for the retired account.

Downloaded files and the on-disk preview cache are intentionally preserved."
  (let ((disco-media--reset-in-progress t)
        (items (disco-media--session-cleanup-items)))
    ;; Revoke every callback before cancellation; Appkit cancellation may call
    ;; its error callback synchronously.
    (cl-incf disco-media--generation)
    (disco-media--clear-session-memory)
    (unwind-protect
        (disco-media--run-cleanup-items
         items #'disco-media--cancel-cleanup-item)
      ;; A broken cancellation hook may directly repopulate a cache.  The
      ;; reset barrier remains active throughout this final privacy sweep.
      (disco-media--clear-session-memory))))

(defun disco-media--visual-custom-set (symbol value)
  "Set SYMBOL to VALUE and invalidate media preview caches."
  (set-default symbol value)
  (when (fboundp 'disco-media-clear-preview-memory-cache)
    (disco-media-clear-preview-memory-cache))
  (let ((callback disco-media-rerender-function))
    (when (functionp callback)
      (funcall callback 'visual nil))))

(defcustom disco-media-spoiler-turbulence-base-frequency '(0.1 . 0.1)
  "Base-frequency pair passed to spoiler `feTurbulence'."
  :type '(cons (number :tag "X frequency")
          (number :tag "Y frequency"))
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-turbulence-num-octaves 2
  "Number of turbulence octaves used for spoiler distortion."
  :type 'integer
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-displacement-min-scale 10.0
  "Minimum spoiler displacement-map scale."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-displacement-max-scale 28.0
  "Maximum spoiler displacement-map scale."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-displacement-divisor 5.0
  "Divisor used to derive spoiler displacement scale from preview size."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-filter-margin-ratio 0.20
  "Extra spoiler filter margin ratio around the preview bounds."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-real-preview-dim-opacity 0.08
  "Dark overlay opacity applied over real spoiler previews."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-spoiler-placeholder-dim-opacity 0.10
  "Dark overlay opacity applied over placeholder spoiler previews."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-audio-waveform-colors '("#17c23a" . "#72d86f")
  "Colors used for audio waveform bars as (PLAYED . UNPLAYED)."
  :type '(cons (string :tag "Played color")
          (string :tag "Unplayed color"))
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-audio-waveform-height-factor 1.15
  "Line-height multiplier used for rendered audio waveform images."
  :type 'number
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-audio-waveform-bar-width 3
  "Stroke width in pixels used for each audio waveform bar."
  :type 'integer
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defcustom disco-media-audio-waveform-gap-width 2
  "Horizontal gap in pixels between adjacent audio waveform bars."
  :type 'integer
  :set #'disco-media--visual-custom-set
  :group 'disco-media)

(defun disco-media-clear-preview-memory-cache ()
  "Clear Discord attachment preview caches without touching other clients."
  (interactive)
  (let ((items (append (disco-media--preview-cleanup-items)
                       (list (list :kind 'decoration)))))
    ;; Revocation precedes cancellation so synchronous callbacks cannot refill
    ;; a visually invalidated cache.  This helper deliberately remains scoped
    ;; to preview/decoration state and does not retire downloads or audio.
    (clrhash disco-media--attachment-preview-fetching)
    (clrhash disco-media--attachment-preview-owner-table)
    (clrhash disco-media--attachment-preview-image-cache)
    (clrhash disco-media--attachment-waveform-image-cache)
    (clrhash disco-media--attachment-placeholder-image-cache)
    (clrhash disco-media--attachment-decorated-preview-cache)
    (unwind-protect
        (disco-media--run-cleanup-items
         items #'disco-media--cancel-cleanup-item)
      (clrhash disco-media--attachment-preview-fetching)
      (clrhash disco-media--attachment-preview-owner-table)
      (clrhash disco-media--attachment-preview-image-cache)
      (clrhash disco-media--attachment-waveform-image-cache)
      (clrhash disco-media--attachment-placeholder-image-cache)
      (clrhash disco-media--attachment-decorated-preview-cache))))

(defun disco-media--configured-audio-player-command ()
  "Return the configured Discord attachment audio player command."
  disco-media-audio-player-command)

(defun disco-media--audio-player-program-name ()
  "Return the configured Discord audio player's executable basename."
  (appkit-media-command-program-name
   (disco-media--configured-audio-player-command)))

(defun disco-media-audio-inline-playback-available-p ()
  "Return non-nil when the audio player supports tracked inline playback."
  (let ((command (disco-media--configured-audio-player-command)))
    (and (appkit-media-command-runnable-p command)
         (equal (disco-media--audio-player-program-name) "ffplay"))))

(defun disco-media-attachment-preview-rendering-available-p ()
  "Return non-nil when inline attachment image previews are available."
  (and disco-media-show-previews
       (appkit-media-inline-image-rendering-available-p)))

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
    (format "%s:%s:%s:%s%s"
            seed
            name
            (max 64
                 (if (numberp appkit-media-preview-max-width)
                     appkit-media-preview-max-width
                   460))
            (max 64
                 (if (numberp appkit-media-preview-max-height)
                     appkit-media-preview-max-height
                   360))
            (if (eq (disco-media-attachment-kind attachment) 'video)
                (concat ":" (appkit-media-video-preview-policy-key))
              ""))))

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
  (expand-file-name (md5 cache-key) disco-media-preview-cache-directory))

(defun disco-media-attachment-preview-cache-existing-file (cache-key)
  "Return existing preview cache file path for CACHE-KEY, or nil."
  (appkit-media-image-cache-existing-file
   (disco-media--attachment-preview-cache-file-base cache-key)))

(defun disco-media--attachment-preview-image-from-file (file)
  "Create inline attachment preview image from FILE, or nil when unavailable."
  (let ((max-width (max 64
                        (if (numberp appkit-media-preview-max-width)
                            appkit-media-preview-max-width
                          460)))
        (max-height (max 64
                         (if (numberp appkit-media-preview-max-height)
                             appkit-media-preview-max-height
                           360))))
    (appkit-media-preview-image-from-file file max-width max-height)))

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
  "Return spoiler displacement scale string for WIDTH and HEIGHT."
  (let* ((base (float (max 24 (min (or width 120) (or height 120)))))
         (divisor (max 0.1 (float disco-media-spoiler-displacement-divisor)))
         (min-scale (float disco-media-spoiler-displacement-min-scale))
         (max-scale (float disco-media-spoiler-displacement-max-scale)))
    (format "%.1f" (max min-scale (min max-scale (/ base divisor))))))

(defun disco-media--svg-append-spoiler-node (svg node-id &optional width height)
  "Append telega-like spoiler turbulence filter with NODE-ID into SVG."
  (when (and (fboundp 'svg--append)
             (fboundp 'dom-node))
    (let* ((freq-pair disco-media-spoiler-turbulence-base-frequency)
           (freq-x (float (or (car-safe freq-pair) 0.1)))
           (freq-y (float (or (cdr-safe freq-pair) 0.1)))
           (octaves (max 1 disco-media-spoiler-turbulence-num-octaves))
           (margin (* 100.0 (max 0.0 (float disco-media-spoiler-filter-margin-ratio))))
           (extent (+ 100.0 (* 2.0 margin))))
      (svg--append
       svg
       (dom-node 'filter
                 `((id . ,node-id)
                   (x . ,(format "%.0f%%" (- margin)))
                   (y . ,(format "%.0f%%" (- margin)))
                   (width . ,(format "%.0f%%" extent))
                   (height . ,(format "%.0f%%" extent)))
                 (dom-node 'feTurbulence
                           `((type . "turbulence")
                             (result . "NOISE")
                             (baseFrequency . ,(format "%.3f %.3f" freq-x freq-y))
                             (numOctaves . ,(number-to-string octaves))))
                 (dom-node 'feDisplacementMap
                           `((in . "SourceGraphic")
                             (in2 . "NOISE")
                             (xChannelSelector . "R")
                             (yChannelSelector . "G")
                             (scale . ,(disco-media--spoiler-displacement-scale
                                        width height)))))))))

(defun disco-media--svg-spoiler-filter-ref (svg &optional width height)
  "Return spoiler filter ref string for SVG, appending node when possible."
  (when (and svg
             (fboundp 'svg--append)
             (fboundp 'dom-node))
    (let ((node-id "disco-spoiler-noise"))
      (disco-media--svg-append-spoiler-node svg node-id width height)
      (format "url(#%s)" node-id))))

(defun disco-media--svg-append-spoiler-video-icon (svg width height)
  "Append the shared video play icon to spoiler SVG."
  (appkit-media-append-video-play-icon svg width height))

(defun disco-media--svg-append-spoiler-overlay (svg width height &optional video-p dim-opacity)
  "Append spoiler overlay chrome to SVG of WIDTH by HEIGHT.

The actual spoiler distortion comes from the turbulence/displacement filter.
This overlay only darkens slightly and optionally adds a video play marker."
  (when (fboundp 'svg-rectangle)
    (svg-rectangle svg 0 0 width height
                   :fill "#000000"
                   :fill-opacity (or dim-opacity
                                     disco-media-spoiler-real-preview-dim-opacity)))
  (when video-p
    (disco-media--svg-append-spoiler-video-icon svg width height)))

(defun disco-media--preview-image-display-props (preview-image)
  "Return safe display-only image properties derived from PREVIEW-IMAGE."
  (let* ((props (cdr-safe preview-image))
         (slices (or (plist-get props :appkit-media-nslices)
                     (appkit-media-image-slice-count preview-image)))
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
      (setq result (plist-put result :appkit-media-nslices slices)))
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
          (plist-get props :appkit-media-nslices)
          spoiler-filter-p
          video-p
          dim-opacity
          (and video-p appkit-media-video-play-icon-radius-divisor)
          (and video-p appkit-media-video-play-icon-circle-opacity)
          (and video-p appkit-media-video-play-icon-triangle-opacity))))

(cl-defun disco-media--decorate-preview-image (preview-image &key spoiler-filter-p
                                                             video-p dim-opacity)
  "Return SVG-decorated PREVIEW-IMAGE with optional spoiler/video chrome."
  (when (and (appkit-media-image-object-valid-p preview-image)
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
      (or (and (appkit-media-image-object-valid-p cached) cached)
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
                (when (appkit-media-image-object-valid-p image)
                  (puthash cache-key image disco-media--attachment-decorated-preview-cache)
                  image))))))))

(defun disco-media--attachment-placeholder-version (attachment)
  "Return normalized placeholder version for ATTACHMENT, or nil."
  (let ((raw (alist-get 'placeholder_version attachment)))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

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
                              (if (numberp appkit-media-preview-max-width)
                                  appkit-media-preview-max-width
                                460)))
         (safe-max-height (max 64
                               (if (numberp appkit-media-preview-max-height)
                                   appkit-media-preview-max-height
                                 360))))
    (appkit-media-preview-height-chars
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
             (height-spec (appkit-media-ch-height-spec display-lines))
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
               svg pixel-width pixel-height
               (eq kind 'video)
               disco-media-spoiler-placeholder-dim-opacity))
            (disco-media--svg-image
             svg
             :scale 1.0
             :height height-spec
             :appkit-media-nslices display-lines
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
            (max 64 (if (numberp appkit-media-preview-max-width)
                        appkit-media-preview-max-width
                      460))
            (max 64 (if (numberp appkit-media-preview-max-height)
                        appkit-media-preview-max-height
                      360))))))

(defun disco-media-attachment-placeholder-image (attachment)
  "Return decoded placeholder image for ATTACHMENT, or nil."
  (let ((cache-key (disco-media--attachment-placeholder-cache-key attachment nil)))
    (when (and (appkit-media-inline-image-rendering-available-p)
               (or (disco-media--attachment-image-p attachment)
                   (disco-media--attachment-video-p attachment))
               cache-key)
      (let ((cached (gethash cache-key disco-media--attachment-placeholder-image-cache)))
        (or (and (appkit-media-image-object-valid-p cached) cached)
            (let ((image (disco-media--thumbhash->svg-image
                          (alist-get 'placeholder attachment)
                          attachment nil)))
              (when (appkit-media-image-object-valid-p image)
                (puthash cache-key image disco-media--attachment-placeholder-image-cache)
                image)))))))

(defun disco-media-attachment-spoiler-placeholder-image (attachment)
  "Return spoiler placeholder image for ATTACHMENT without extra distortion."
  (when-let* ((placeholder-image (disco-media-attachment-placeholder-image attachment)))
    (disco-media--decorate-preview-image
     placeholder-image
     :spoiler-filter-p nil
     :video-p (eq (disco-media-attachment-kind attachment) 'video)
     :dim-opacity disco-media-spoiler-placeholder-dim-opacity)))

(defun disco-media--attachment-real-spoiler-preview-image (attachment)
  "Return obscured real preview image for ATTACHMENT, or nil."
  (let* ((image-like (disco-media--attachment-image-p attachment))
         (video-like (disco-media--attachment-video-p attachment))
         (preview-image (and (or image-like video-like)
                             (disco-media--attachment-real-preview-image
                              attachment image-like video-like))))
    (when (appkit-media-image-object-valid-p preview-image)
      (disco-media--decorate-preview-image
       preview-image
       :spoiler-filter-p t
       :video-p video-like
       :dim-opacity disco-media-spoiler-real-preview-dim-opacity))))

(defun disco-media-attachment-spoiler-preview-image (attachment)
  "Return obscured preview image for spoiler ATTACHMENT, or nil."
  (when (disco-media-attachment-preview-rendering-available-p)
    (or (disco-media--attachment-real-spoiler-preview-image attachment)
        (disco-media-attachment-spoiler-placeholder-image attachment))))

(defun disco-media--notify-state-updated (&optional kind key)
  "Notify UI after media state/cache updates.

KIND is a symbol such as `audio', `download', or `preview'.  KEY is the stable
attachment/download key associated with the update when available."
  (unless disco-media--reset-in-progress
    (let ((callback disco-media-rerender-function))
      (when (functionp callback)
        (funcall callback kind key)))))

(defun disco-media--attachment-preview-complete-fetch
    (cache-key image &optional target-file owner)
  "Finalize preview CACHE-KEY with IMAGE when OWNER is still exact.

When OWNER is nil, perform the completion unconditionally for compatibility
with direct callers; asynchronous entrypoints always provide an exact owner."
  (when (or (null owner)
            (disco-media--preview-owner-current-p cache-key owner))
    (when (and (null image) target-file (file-exists-p target-file))
      (ignore-errors (delete-file target-file)))
    (puthash cache-key (or image :missing)
             disco-media--attachment-preview-image-cache)
    (remhash cache-key disco-media--attachment-preview-fetching)
    (when (eq owner
              (gethash cache-key disco-media--attachment-preview-owner-table))
      (remhash cache-key disco-media--attachment-preview-owner-table))
    (disco-media--notify-state-updated 'preview cache-key)))

(cl-defun disco-media-open-discord-resource
    (resource &optional kind cache-key &key owner)
  "Adapt and open Discord RESOURCE through the shared media runtime.

OWNER is the exact Appkit app or view that owns external video playback."
  (appkit-media-open-resource
   (appkit-media-resource-create
    :file (alist-get 'file resource)
    :url (alist-get 'url resource)
    :name (alist-get 'filename resource)
    :mime-type (alist-get 'content_type resource))
   :kind kind
   :cache-key cache-key
   :cache-directory disco-media-preview-cache-directory
   :client-label "disco"
   :owner owner))

(defun disco-media--attachment-appkit-resource (attachment &optional file)
  "Return canonical appkit resource for Discord ATTACHMENT and local FILE."
  (appkit-media-resource-create
   :file file
   :url (disco-media-attachment-download-url attachment)
   :name (alist-get 'filename attachment)
   :mime-type (alist-get 'content_type attachment)))

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

(defun disco-media--start-video-preview-fetch (cache-key attachment cache-base)
  "Start asynchronous video preview extraction for ATTACHMENT."
  (when (and (disco-media--start-allowed-p)
             (not (gethash cache-key disco-media--attachment-preview-fetching))
             (not (gethash cache-key disco-media--attachment-preview-image-cache))
             (not (and (numberp disco-media--attachment-preview-fetch-budget)
                       (<= disco-media--attachment-preview-fetch-budget 0))))
    (when (numberp disco-media--attachment-preview-fetch-budget)
      (cl-decf disco-media--attachment-preview-fetch-budget))
    (let* ((owner (list :generation disco-media--generation
                        :kind 'video
                        :key cache-key
                        :transfer nil))
           (local-file (and (appkit-media-file-present-p
                             (alist-get 'file attachment))
                            (alist-get 'file attachment)))
           (preview-source (or local-file
                               (disco-media-attachment-preview-url attachment)))
           (source (or local-file
                       (disco-media-attachment-download-url attachment)
                       preview-source)))
      (puthash cache-key owner disco-media--attachment-preview-owner-table)
      (puthash cache-key t disco-media--attachment-preview-fetching)
      (condition-case err
          (progn
            (appkit-media-start-video-preview
             :key (concat "disco:" cache-key)
             :source source
             :preview-source preview-source
             :source-size (alist-get 'size attachment)
             :duration (alist-get 'duration_secs attachment)
             :cache-base cache-base
             :callback
             (lambda (image target-file)
               (disco-media--attachment-preview-complete-fetch
                cache-key image target-file owner)))
            ;; A mocked or reentrant constructor can reset the account before
            ;; returning, after the reset snapshot already ran.  Re-cancel the
            ;; known Appkit key instead of abandoning a newly published job.
            (unless (disco-media--preview-owner-current-p cache-key owner)
              (disco-media--run-cleanup-items
               (list (list :kind 'video :key (concat "disco:" cache-key)))
               #'disco-media--cancel-cleanup-item)))
        ((error quit)
         (disco-media--attachment-preview-complete-fetch
          cache-key nil nil owner)
         (when (disco-media--session-current-p (plist-get owner :generation))
           (message "disco: video preview enqueue failed for %s: %s"
                    cache-key
                    (error-message-string err))))))))

(defun disco-media--start-attachment-preview-fetch (cache-key url cache-base)
  "Start asynchronous preview fetch for CACHE-KEY from URL into CACHE-BASE."
  (when (and (disco-media--start-allowed-p)
             (not (gethash cache-key disco-media--attachment-preview-fetching))
             (not (gethash cache-key disco-media--attachment-preview-image-cache))
             (not (and (numberp disco-media--attachment-preview-fetch-budget)
                       (<= disco-media--attachment-preview-fetch-budget 0))))
    (when (numberp disco-media--attachment-preview-fetch-budget)
      (cl-decf disco-media--attachment-preview-fetch-budget))
    (let ((owner (list :generation disco-media--generation
                       :kind 'image
                       :key cache-key
                       :transfer nil)))
      (puthash cache-key owner disco-media--attachment-preview-owner-table)
      (puthash cache-key t disco-media--attachment-preview-fetching)
      (condition-case err
          (let ((transfer
                 (appkit-media-cache-image-resource-async
                  (appkit-media-resource-create :url url)
                  cache-base
                  (lambda (target-file)
                    (when (disco-media--preview-owner-current-p cache-key owner)
                      (disco-media--attachment-preview-complete-fetch
                       cache-key
                       (disco-media--attachment-preview-image-from-file target-file)
                       target-file
                       owner)))
                  (lambda (_reason)
                    (disco-media--attachment-preview-complete-fetch
                     cache-key nil nil owner)))))
            ;; A local resource or unusual backend may finish synchronously.
            ;; Publish its handle only if this exact operation still owns KEY.
            (if (disco-media--preview-owner-current-p cache-key owner)
                (progn
                  (setf (plist-get owner :transfer) transfer)
                  (puthash cache-key transfer
                           disco-media--attachment-preview-fetching))
              ;; Reset may have happened inside the constructor, before the
              ;; returned transfer was visible to its cleanup snapshot.
              (when transfer
                (disco-media--run-cleanup-items
                 (list (list :kind 'transfer :transfer transfer))
                 #'disco-media--cancel-cleanup-item))))
        ((error quit)
         (disco-media--attachment-preview-complete-fetch
          cache-key nil nil owner)
         (when (disco-media--session-current-p (plist-get owner :generation))
           (message "disco: attachment preview enqueue failed for %s: %s"
                    cache-key
                    (error-message-string err))))))))

(defun disco-media--attachment-real-preview-image (attachment image-like video-like)
  "Return real fetched preview image for ATTACHMENT, or nil."
  (let* ((cache-key (disco-media-attachment-preview-cache-key attachment))
         (cached (and cache-key
                      (gethash cache-key disco-media--attachment-preview-image-cache))))
    (cond
     ((eq cached :missing)
      nil)
     ((and cached (appkit-media-image-object-valid-p cached))
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
              (disco-media--start-video-preview-fetch
               cache-key attachment cache-base)))
          nil)
         (t nil)))))))

(defun disco-media-attachment-preview-image (attachment &optional bypass-user-toggle)
  "Return preview image object for ATTACHMENT when available."
  (let* ((rendering-ok (if bypass-user-toggle
                           (appkit-media-inline-image-rendering-available-p)
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

(defconst disco-media--attachment-waveform-chars ".:-=+*#%@"
  "ASCII ramp used to render text audio waveforms.")

(defun disco-media-attachment-waveform-samples (attachment)
  "Return decoded waveform samples for ATTACHMENT as a vector, or nil."
  (when-let* ((waveform (alist-get 'waveform attachment)))
    (condition-case nil
        (let ((bytes (base64-decode-string waveform)))
          (when (> (length bytes) 0)
            (vconcat bytes)))
      (error nil))))

(defun disco-media--waveform-resample (samples width)
  "Return resampled waveform peak vector from SAMPLES for target WIDTH."
  (let* ((source-len (length samples))
         (target-len (max 1 width))
         (result (make-vector target-len 0))
         (idx 0))
    (while (< idx target-len)
      (let* ((start (floor (/ (* idx source-len) (float target-len))))
             (end (max (1+ start)
                       (floor (/ (* (1+ idx) source-len) (float target-len)))))
             (peak 0))
        (dotimes (offset (max 1 (- end start)))
          (let ((sample-index (min (1- source-len) (+ start offset))))
            (setq peak (max peak (aref samples sample-index)))))
        (aset result idx peak)
        (setq idx (1+ idx))))
    result))

(defun disco-media--attachment-waveform-played-width (bar-width duration progress)
  "Return played bar count for BAR-WIDTH, DURATION, and PROGRESS."
  (if (and (numberp progress)
           (numberp duration)
           (> duration 0))
      (floor (* bar-width
                (min 1.0 (max 0.0 (/ (float progress) duration)))))
    0))

(cl-defun disco-media-attachment-waveform-string (attachment &key width progress
                                                             played-face unplayed-face)
  "Return propertized text waveform preview string for ATTACHMENT, or nil."
  (when-let* ((samples (disco-media-attachment-waveform-samples attachment)))
    (let* ((chars disco-media--attachment-waveform-chars)
           (bar-width (max 8 (or width 28)))
           (levels (disco-media--waveform-resample samples bar-width))
           (duration (alist-get 'duration_secs attachment))
           (played-width (disco-media--attachment-waveform-played-width
                          bar-width duration progress))
           (text (make-string bar-width ?.)))
      (dotimes (idx bar-width)
        (let* ((sample (aref levels idx))
               (char-index (min (1- (length chars))
                                (floor (* (/ (float sample) 255.0)
                                          (1- (length chars))))))
               (face (if (< idx played-width) played-face unplayed-face)))
          (aset text idx (aref chars char-index))
          (when face
            (put-text-property idx (1+ idx) 'face face text))))
      text)))

(cl-defun disco-media-attachment-waveform-image (attachment &key width progress)
  "Return SVG waveform image for ATTACHMENT, or nil when unavailable."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-line))
    (when-let* ((samples (disco-media-attachment-waveform-samples attachment))
                (waveform (alist-get 'waveform attachment)))
      (let* ((bar-count (max 8 (or width 28)))
             (levels (disco-media--waveform-resample samples bar-count))
             (duration (alist-get 'duration_secs attachment))
             (played-width (disco-media--attachment-waveform-played-width
                            bar-count duration progress))
             (bar-width (max 1 disco-media-audio-waveform-bar-width))
             (gap-width (max 0 disco-media-audio-waveform-gap-width))
             (pad-x (max 2 bar-width))
             (pad-y 3)
             (height (max 14 (round (* (frame-char-height)
                                       (max 0.5
                                            (float disco-media-audio-waveform-height-factor))))))
             (width-px (+ (* bar-count bar-width)
                          (* (max 0 (1- bar-count)) gap-width)
                          (* 2 pad-x)))
             (colors disco-media-audio-waveform-colors)
             (played-color (or (car-safe colors) "#17c23a"))
             (unplayed-color (or (cdr-safe colors) "#72d86f"))
             (key (list (md5 waveform)
                        bar-count
                        played-width
                        height
                        bar-width
                        gap-width
                        played-color
                        unplayed-color)))
        (or (gethash key disco-media--attachment-waveform-image-cache)
            (let ((svg (svg-create width-px height))
                  (available-height (max 4 (- height (* 2 pad-y)))))
              (dotimes (idx bar-count)
                (let* ((sample (aref levels idx))
                       (ratio (/ (float sample) 255.0))
                       (x (+ pad-x (* idx (+ bar-width gap-width))
                             (/ (float bar-width) 2.0)))
                       (y2 (- height pad-y))
                       (bar-height (max 2 (round (* ratio available-height))))
                       (y1 (- y2 bar-height))
                       (played-p (< idx played-width)))
                  (svg-line svg x y2 x y1
                            :stroke-color (if played-p played-color unplayed-color)
                            :stroke-width (if played-p
                                              (1+ bar-width)
                                            bar-width)
                            :stroke-linecap "round")))
              (when-let* ((image (disco-media--svg-image
                                  svg
                                  :scale 1.0
                                  :width width-px
                                  :height height
                                  :ascent 'center
                                  :mask 'heuristic)))
                (puthash key image disco-media--attachment-waveform-image-cache)
                image)))))))

(defun disco-media-attachment-voice-message-p (attachment)
  "Return non-nil when ATTACHMENT looks like a Discord voice message."
  (and (eq (disco-media-attachment-kind attachment) 'audio)
       (stringp (alist-get 'waveform attachment))))

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
         (safe-name (appkit-media-sanitize-filename
                     (disco-media-attachment-default-save-name attachment))))
    (expand-file-name
     (format "%s-%s" (substring (md5 key) 0 10) safe-name)
     disco-media-download-directory)))

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
  (or (and (stringp err) err)
      (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (condition-case _
          (error-message-string err)
        (error
         (format "%S" err)))))

(defun disco-media-start-attachment-download (attachment &optional open-after on-success)
  "Start asynchronous default-location download for ATTACHMENT.

When OPEN-AFTER is non-nil, open downloaded file in Emacs after completion.
When ON-SUCCESS is non-nil, call it with downloaded PATH after completion."
  (disco-media--ensure-start-allowed)
  (let* ((url (disco-media-attachment-download-url attachment))
         (key (disco-media-attachment-download-key attachment))
         (entry (disco-media-attachment-download-state attachment))
         (path (plist-get entry :path))
         (status (plist-get entry :status))
         (expected-size (alist-get 'size attachment))
         (owner (list :generation disco-media--generation
                      :key key
                      :transfer nil)))
    (unless (appkit-media-url-present-p url)
      (user-error "disco: attachment has no downloadable URL"))
    (when (eq status 'downloading)
      (user-error "disco: attachment download already in progress"))
    (cl-labels
        ((current-entry ()
           (copy-tree (or (gethash key disco-media--attachment-download-state-table) '())))
         (current-p ()
           (disco-media--download-owner-current-p key owner))
         (retire-owner ()
           (when (eq owner
                     (gethash key disco-media--attachment-download-owner-table))
             (remhash key disco-media--attachment-download-owner-table))
           (let ((current (gethash key disco-media--attachment-download-state-table)))
             (when (eq owner (plist-get current :owner))
               (setq current (plist-put current :owner nil))
               (setq current (plist-put current :generation nil))
               (puthash key current
                        disco-media--attachment-download-state-table))))
         (finish-success ()
           (when (current-p)
             (let ((current (current-entry)))
               (setq current (plist-put current :status 'downloaded))
               (setq current (plist-put current :path path))
               (setq current (plist-put current :transfer nil))
               (setq current (plist-put current :cancel-requested nil))
               (setq current (plist-put current :error nil))
               (puthash key current disco-media--attachment-download-state-table)
               (unwind-protect
                   (progn
                     (disco-media--notify-state-updated 'download key)
                     (when (current-p)
                       (message "disco: downloaded attachment -> %s" path))
                     (when (and open-after (current-p))
                       (appkit-media-open-file path))
                     (when (and (functionp on-success) (current-p))
                       (funcall on-success path)))
                 (retire-owner)))))
         (finish-error (err)
           (when (current-p)
             (let ((current (current-entry)))
               (setq current (plist-put current :status 'error))
               (setq current (plist-put current :transfer nil))
               (setq current (plist-put current :cancel-requested nil))
               (setq current
                     (plist-put current :error
                                (disco-media--attachment-error-message err)))
               (puthash key current disco-media--attachment-download-state-table)
               (unwind-protect
                   (progn
                     (disco-media--notify-state-updated 'download key)
                     (when (current-p)
                       (message "disco: attachment download failed: %s"
                                (plist-get current :error))))
                 (retire-owner)))))
         (finish-canceled ()
           (when (current-p)
             (let ((current (current-entry)))
               (setq current (plist-put current :status 'not-downloaded))
               (setq current (plist-put current :transfer nil))
               (setq current (plist-put current :cancel-requested nil))
               (setq current (plist-put current :error nil))
               (puthash key current disco-media--attachment-download-state-table)
               (unwind-protect
                   (progn
                     (disco-media--notify-state-updated 'download key)
                     (when (current-p)
                       (message "disco: attachment download canceled")))
                 (retire-owner))))))
      (setq entry (plist-put entry :status 'downloading))
      (setq entry (plist-put entry :transfer nil))
      (setq entry (plist-put entry :cancel-requested nil))
      (setq entry (plist-put entry :error nil))
      (setq entry (plist-put entry :generation disco-media--generation))
      (setq entry (plist-put entry :owner owner))
      (puthash key owner disco-media--attachment-download-owner-table)
      (puthash key entry disco-media--attachment-download-state-table)
      (let (setup-complete-p)
        (unwind-protect
            (progn
              (disco-media--notify-state-updated 'download key)
              (when (current-p)
                (message "disco: downloading attachment %s"
                         (disco-media-attachment-display-name attachment)))
              (when (current-p)
                (let ((transfer
                       (appkit-media-copy-or-download-resource-async
                        (disco-media--attachment-appkit-resource attachment)
                        path
                        (lambda (downloaded)
                          (when (current-p)
                            (let* ((attributes (file-attributes downloaded))
                                   (actual-size
                                    (and attributes
                                         (file-attribute-size attributes))))
                              (if (and (numberp expected-size)
                                       (> expected-size 0)
                                       (or (not (numberp actual-size))
                                           (<= actual-size 0)))
                                  (progn
                                    (ignore-errors (delete-file downloaded))
                                    (finish-error
                                     "downloaded attachment is empty"))
                                (finish-success)))))
                        (lambda (err)
                          (when (current-p)
                            (if (plist-get (current-entry) :cancel-requested)
                                (finish-canceled)
                              (finish-error err)))))))
                  ;; Setup or a local copy may finish synchronously.  Never
                  ;; resurrect its retired owner with the returned handle.
                  (if (current-p)
                      (progn
                        (setf (plist-get owner :transfer) transfer)
                        (let ((current (current-entry)))
                          (setq current (plist-put current :transfer transfer))
                          (puthash key current
                                   disco-media--attachment-download-state-table)))
                    ;; Reset can run synchronously inside the constructor,
                    ;; before its returned handle entered the reset snapshot.
                    (when transfer
                      (disco-media--run-cleanup-items
                       (list (list :kind 'transfer :transfer transfer))
                       #'disco-media--cancel-cleanup-item)))))
              (setq setup-complete-p t))
          (unless setup-complete-p
            (when (current-p)
              (remhash key disco-media--attachment-download-state-table)
              (retire-owner))))))))

(defun disco-media-cancel-attachment-download (attachment)
  "Cancel active asynchronous download for ATTACHMENT."
  (let* ((key (disco-media-attachment-download-key attachment))
         (entry (copy-tree (or (gethash key disco-media--attachment-download-state-table) '())))
         (transfer (plist-get entry :transfer))
         (status (plist-get entry :status)))
    (unless (eq status 'downloading)
      (user-error "disco: attachment is not downloading"))
    (setq entry (plist-put entry :cancel-requested t))
    (puthash key entry disco-media--attachment-download-state-table)
    (message "disco: canceling attachment download")
    (appkit-media-cancel-transfer transfer)))

(defun disco-media-open-downloaded-attachment (attachment)
  "Open local downloaded file for ATTACHMENT."
  (let* ((entry (disco-media-attachment-download-state attachment))
         (path (plist-get entry :path)))
    (unless (and (stringp path) (file-exists-p path))
      (user-error "disco: attachment file is not downloaded yet"))
    (appkit-media-open-file path)))

(defun disco-media-play-attachment-video (attachment &optional owner)
  "Play ATTACHMENT video preferring local file when available.

OWNER is the exact Appkit app or view that owns the external player."
  (let* ((entry (disco-media-attachment-download-state attachment))
         (path (plist-get entry :path))
         (url (disco-media-attachment-download-url attachment)))
    (cond
     ((and (stringp path) (file-exists-p path))
      (appkit-media-play-video-file path "disco" :owner owner))
     ((appkit-media-url-present-p url)
      (appkit-media-play-video-url url "disco" :owner owner))
     (t
      (user-error "disco: video attachment has no playable source")))))

(defun disco-media--audio-state-entry (key)
  "Return normalized inline audio playback state for KEY."
  (let ((entry (copy-tree (or (gethash key disco-media--attachment-audio-state-table) '()))))
    (unless (plist-get entry :status)
      (setq entry (plist-put entry :status 'stopped)))
    (when (and (eq (plist-get entry :status) 'playing)
               (not (process-live-p (plist-get entry :process))))
      (setq entry (plist-put entry :status 'stopped))
      (setq entry (plist-put entry :process nil)))
    entry))

(defun disco-media--audio-store-state (key entry)
  "Persist inline audio playback ENTRY for KEY and return ENTRY."
  (puthash key entry disco-media--attachment-audio-state-table)
  entry)

(defun disco-media-attachment-audio-state (attachment)
  "Return normalized inline audio playback state for ATTACHMENT."
  (let* ((key (disco-media-attachment-download-key attachment))
         (entry (disco-media--audio-state-entry key)))
    (disco-media--audio-store-state key entry)))

(defun disco-media-attachment-audio-playing-p (attachment)
  "Return non-nil when ATTACHMENT audio is currently playing inline."
  (eq (plist-get (disco-media-attachment-audio-state attachment) :status) 'playing))

(defun disco-media-attachment-audio-paused-p (attachment)
  "Return pause position for ATTACHMENT audio, or nil when not paused."
  (let ((entry (disco-media-attachment-audio-state attachment)))
    (when (eq (plist-get entry :status) 'paused)
      (plist-get entry :progress))))

(defun disco-media-attachment-audio-progress (attachment)
  "Return current known playback progress for ATTACHMENT audio, or nil."
  (plist-get (disco-media-attachment-audio-state attachment) :progress))

(defun disco-media-attachment-audio-pending-play-p (attachment)
  "Return non-nil when ATTACHMENT should auto-play after download."
  (plist-get (disco-media-attachment-audio-state attachment) :pending-play))

(defun disco-media--set-attachment-audio-pending-play (attachment pending-play)
  "Set ATTACHMENT pending autoplay state to PENDING-PLAY."
  (let* ((key (disco-media-attachment-download-key attachment))
         (entry (disco-media--audio-state-entry key)))
    (setq entry (plist-put entry :pending-play pending-play))
    (disco-media--audio-store-state key entry)
    entry))

(defun disco-media--external-audio-process-sentinel (process _event)
  "Retire exact external audio PROCESS ownership after it exits."
  (unless (process-live-p process)
    (let ((owner (plist-get (process-plist process)
                            :disco-media-external-owner)))
      (when (disco-media--external-audio-owner-current-p owner process)
        (setq disco-media--attachment-external-audio-owners
              (delq owner disco-media--attachment-external-audio-owners)))
      ;; A dead process may remain referenced by a caller; do not retain its
      ;; account owner through the process plist.
      (set-process-plist process nil))))

(defun disco-media--start-external-audio-player (source)
  "Start configured non-inline audio player for SOURCE.

Return non-nil on success."
  (disco-media--ensure-start-allowed)
  (let* ((command (disco-media--configured-audio-player-command))
         (argv (appkit-media-command-arguments command))
         (program (car argv))
         (args (append (cdr argv) (list source)))
         (generation disco-media--generation)
         (owner (list :generation disco-media--generation
                      :process nil
                      :buffer nil))
         process
         completed-p)
    (when (and program
               (appkit-media-command-runnable-p command))
      ;; Register the pending owner before process construction.  Reset may
      ;; run synchronously inside a mocked or unusual process constructor.
      (push owner disco-media--attachment-external-audio-owners)
      (unwind-protect
          (condition-case nil
              (progn
                (setq process
                      (make-process
                       :name "disco-audio-player"
                       :buffer nil
                       :command (cons program args)
                       :noquery t))
                (when (and (processp process)
                           (disco-media--session-current-p generation)
                           (memq owner
                                 disco-media--attachment-external-audio-owners))
                  (setf (plist-get owner :process) process
                        (plist-get owner :buffer) nil)
                  (set-process-plist
                   process
                   (list :disco-media-generation generation
                         :disco-media-external-owner owner))
                  (set-process-sentinel
                   process #'disco-media--external-audio-process-sentinel)
                  ;; Immediate normal exit may already retire OWNER.  It is
                  ;; still a successful launch when the session did not reset.
                  (setq completed-p
                        (disco-media--session-current-p generation))))
            ((error quit) nil))
        (unless completed-p
          ;; Revoke before compensating deletion so its synchronous sentinel
          ;; cannot mutate the active account.  This covers reset occurring
          ;; before MAKE-PROCESS returned and exposed PROCESS to the snapshot.
          (setq disco-media--attachment-external-audio-owners
                (delq owner disco-media--attachment-external-audio-owners))
          (when (processp process)
            (disco-media--run-cleanup-items
             (list (list :kind 'process :process process :buffer nil))
             #'disco-media--cancel-cleanup-item))))
      completed-p)))

(defun disco-media--stop-inline-audio-process (&optional process stop-reason)
  "Stop inline audio PROCESS and record STOP-REASON for the sentinel."
  (when-let* ((proc (or process disco-media--attachment-audio-current-process)))
    (when (and (processp proc) (process-live-p proc))
      (let ((proc-plist (process-plist proc)))
        (setq proc-plist (plist-put proc-plist :stop-reason stop-reason))
        (set-process-plist proc proc-plist)
        (ignore-errors (delete-process proc))))))

(defun disco-media--inline-audio-process-filter (proc output)
  "Track ffplay progress for inline audio PROC from OUTPUT."
  (when (disco-media--audio-owner-current-p proc)
    (let ((buffer (process-buffer proc))
          new-progress)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-modification-hooks t))
            (goto-char (point-max))
            (insert output)
            (when (> (buffer-size) 20000)
              (delete-region (point-min)
                             (max (point-min) (- (point-max) 10000)))))
          (cond
           ((save-excursion
              (re-search-backward "\r\\s-*\\([0-9.]+\\)" nil t))
            (setq new-progress (string-to-number (match-string 1))))
           ((save-excursion
              (re-search-backward
               " time=\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9.]+\\) "
               nil t))
            (setq new-progress
                  (+ (* 3600 (string-to-number (match-string 1)))
                     (* 60 (string-to-number (match-string 2)))
                     (string-to-number (match-string 3))))))))
      (when (and (numberp new-progress)
                 (disco-media--audio-owner-current-p proc))
        (let* ((proc-plist (process-plist proc))
               (key (plist-get proc-plist :attachment-key))
               (entry (disco-media--audio-state-entry key))
               (last-second (plist-get proc-plist :last-second))
               (next-second (floor new-progress)))
          (setq proc-plist (plist-put proc-plist :progress new-progress))
          (setq entry (plist-put entry :progress new-progress))
          (disco-media--audio-store-state key entry)
          (unless (equal last-second next-second)
            (setq proc-plist (plist-put proc-plist :last-second next-second)))
          (set-process-plist proc proc-plist)
          (when (and (not (equal last-second next-second))
                     (disco-media--audio-owner-current-p proc))
            (disco-media--notify-state-updated 'audio key)))))))

(defun disco-media--inline-audio-process-sentinel (proc _event)
  "Finalize inline audio state after PROC exits."
  (unless (process-live-p proc)
    (let* ((proc-plist (process-plist proc))
           (key (plist-get proc-plist :attachment-key))
           (generation (plist-get proc-plist :disco-media-generation))
           (owned-buffer (plist-get proc-plist :disco-media-owned-buffer))
           (current-p (disco-media--audio-owner-current-p proc))
           (stop-reason (plist-get proc-plist :stop-reason))
           (final-progress (or (and (consp stop-reason)
                                    (eq (car stop-reason) 'paused)
                                    (cdr stop-reason))
                               (plist-get proc-plist :progress))))
      (when current-p
        (let ((entry (disco-media--audio-state-entry key)))
          (cond
           ((and (consp stop-reason) (eq (car stop-reason) 'paused))
            (setq entry (plist-put entry :status 'paused))
            (setq entry
                  (plist-put entry :progress
                             (max 0.0 (or final-progress 0.0)))))
           (t
            (setq entry (plist-put entry :status 'stopped))
            (setq entry (plist-put entry :progress nil))))
          (setq entry (plist-put entry :pending-play nil))
          (setq entry (plist-put entry :process nil))
          (setq entry (plist-put entry :owner nil))
          (setq entry (plist-put entry :generation nil))
          (disco-media--audio-store-state key entry))
        (setq disco-media--attachment-audio-current-process nil
              disco-media--attachment-audio-current-owner nil))
      ;; Buffer ownership is by exact process pointer, never its conventional
      ;; name; stale sentinels may still dispose their own scratch buffer.
      (disco-media--force-dispose-process-buffer owned-buffer)
      (when (and current-p (disco-media--session-current-p generation))
        (disco-media--notify-state-updated 'audio key)))))

(defun disco-media--start-inline-audio-player (attachment source &optional start-at)
  "Start ffplay-backed inline audio playback for ATTACHMENT using SOURCE."
  (disco-media--ensure-start-allowed)
  (let* ((command (disco-media--configured-audio-player-command))
         (argv (appkit-media-command-arguments command))
         (program (car argv))
         (args (append (cdr argv)
                       (when (and (numberp start-at)
                                  (> start-at 0))
                         (list "-ss" (format "%.2f" start-at)))
                       (list source)))
         (key (disco-media-attachment-download-key attachment))
         (generation disco-media--generation)
         (owner (list :generation disco-media--generation :key key))
         buffer
         process
         published-p)
    (unless (and program
                 (disco-media-audio-inline-playback-available-p))
      (user-error "disco: inline audio playback requires ffplay"))
    (when (and (processp disco-media--attachment-audio-current-process)
               (process-live-p disco-media--attachment-audio-current-process))
      (disco-media--stop-inline-audio-process
       disco-media--attachment-audio-current-process 'stopped))
    (unless (disco-media--session-current-p generation)
      (user-error "disco: media session changed before audio start"))
    (setq buffer (generate-new-buffer " *disco-audio-player*"))
    (with-current-buffer buffer
      (buffer-disable-undo))
    (unwind-protect
        (progn
          (setq process
                (apply #'start-process "disco-audio-player" buffer program args))
          (unless (disco-media--session-current-p generation)
            (user-error "disco: media session changed during audio start"))
          (set-process-query-on-exit-flag process nil)
          (set-process-plist
           process
           (list :attachment-key key
                 :disco-media-generation generation
                 :disco-media-owner owner
                 :disco-media-owned-buffer buffer
                 :progress (and (numberp start-at)
                                (max 0.0 start-at))
                 :last-second (and (numberp start-at)
                                   (floor start-at))))
          (setq disco-media--attachment-audio-current-process process
                disco-media--attachment-audio-current-owner owner)
          (let ((entry (disco-media--audio-state-entry key)))
            (setq entry (plist-put entry :status 'playing))
            (setq entry
                  (plist-put entry :progress
                             (and (numberp start-at) (max 0.0 start-at))))
            (setq entry (plist-put entry :pending-play nil))
            (setq entry (plist-put entry :generation generation))
            (setq entry (plist-put entry :owner owner))
            (setq entry (plist-put entry :process process))
            (setq entry (plist-put entry :buffer buffer))
            (disco-media--audio-store-state key entry))
          (set-process-filter process #'disco-media--inline-audio-process-filter)
          (set-process-sentinel process #'disco-media--inline-audio-process-sentinel)
          (when (disco-media--audio-owner-current-p process)
            (disco-media--notify-state-updated 'audio key))
          (setq published-p (disco-media--audio-owner-current-p process))
          (and published-p process))
      (unless published-p
        ;; Revoke before deletion so a synchronous sentinel cannot repopulate
        ;; state or rerender the retired account.
        (when (eq process disco-media--attachment-audio-current-process)
          (setq disco-media--attachment-audio-current-process nil
                disco-media--attachment-audio-current-owner nil))
        (when (eq owner
                  (plist-get
                   (gethash key disco-media--attachment-audio-state-table)
                   :owner))
          (remhash key disco-media--attachment-audio-state-table))
        (if (processp process)
            (disco-media--run-cleanup-items
             (list (list :kind 'process :process process :buffer buffer))
             #'disco-media--cancel-cleanup-item)
          (disco-media--force-dispose-process-buffer buffer))))))

(defun disco-media-stop-attachment-audio (attachment)
  "Stop inline playback for ATTACHMENT and clear any paused position."
  (let* ((key (disco-media-attachment-download-key attachment))
         (entry (disco-media--audio-state-entry key))
         (process (plist-get entry :process)))
    (if (process-live-p process)
        (disco-media--stop-inline-audio-process process 'stopped)
      (setq entry (plist-put entry :status 'stopped))
      (setq entry (plist-put entry :progress nil))
      (setq entry (plist-put entry :pending-play nil))
      (setq entry (plist-put entry :process nil))
      (disco-media--audio-store-state key entry)
      (disco-media--notify-state-updated 'audio key))))

(defun disco-media-play-attachment-audio (attachment)
  "Play or pause ATTACHMENT audio.

Unlike the earlier URL-first implementation, this prefers a downloaded local
file for playback and will queue a download before first play when needed.
This matches telega's approach more closely and avoids ffplay timing quirks on
streamed Discord voice-message URLs."
  (disco-media--ensure-start-allowed)
  (let* ((download-state (disco-media-attachment-download-state attachment))
         (path (plist-get download-state :path))
         (status (plist-get download-state :status))
         (url (disco-media-attachment-download-url attachment))
         (key (disco-media-attachment-download-key attachment))
         (entry (disco-media--audio-state-entry key))
         (process (plist-get entry :process))
         (paused-at (and (eq (plist-get entry :status) 'paused)
                         (plist-get entry :progress))))
    (cond
     ((process-live-p process)
      (disco-media--stop-inline-audio-process
       process
       (cons 'paused
             (max 0.0
                  (or (plist-get (process-plist process) :progress)
                      (plist-get entry :progress)
                      0.0)))))
     ((and (stringp path) (file-exists-p path))
      (if (disco-media-audio-inline-playback-available-p)
          (disco-media--start-inline-audio-player attachment path paused-at)
        (disco-media--set-attachment-audio-pending-play attachment nil)
        (unless (disco-media--start-external-audio-player path)
          (user-error
           "disco: audio player is unavailable; customize `disco-media-audio-player-command'"))))
     ((eq status 'downloading)
      (if (disco-media-attachment-audio-pending-play-p attachment)
          (progn
            (disco-media--set-attachment-audio-pending-play attachment nil)
            (message "disco: canceled pending audio autoplay"))
        (disco-media--set-attachment-audio-pending-play attachment t)
        (message "disco: audio will play after download")))
     ((appkit-media-url-present-p url)
      (disco-media--set-attachment-audio-pending-play attachment t)
      (disco-media-start-attachment-download
       attachment
       nil
       (lambda (_path)
         (when (disco-media-attachment-audio-pending-play-p attachment)
           (disco-media-play-attachment-audio attachment)))))
     (t
      (user-error "disco: audio attachment has no playable source")))))

(defun disco-media-download-attachment (attachment &optional target-path)
  "Download ATTACHMENT to TARGET-PATH.

When TARGET-PATH is nil, prompt interactively for destination path."
  (interactive)
  (disco-media--ensure-start-allowed)
  (let* ((generation disco-media--generation)
         (url (disco-media-attachment-download-url attachment))
         (download-state (disco-media-attachment-download-state attachment))
         (cached-path (plist-get download-state :path))
         (has-cached (and (stringp cached-path) (file-exists-p cached-path)))
         (has-url (appkit-media-url-present-p url))
         (default-name (disco-media-attachment-default-save-name attachment))
         (target (or target-path
                     (read-file-name "Save attachment as: "
                                     nil
                                     default-name
                                     nil
                                     default-name))))
    (unless (or has-cached has-url)
      (user-error "disco: attachment has neither local cache nor downloadable URL"))
    (unless (disco-media--session-current-p generation)
      (user-error "disco: media session changed before attachment save"))
    (let ((owner (list :generation generation :transfer nil))
          transfer
          returned-p)
      (cl-labels
          ((current-p () (disco-media--export-owner-current-p owner))
           (retire ()
             (setq disco-media--attachment-export-owners
                   (delq owner disco-media--attachment-export-owners)))
           (cancel-returned ()
             (when transfer
               (disco-media--run-cleanup-items
                (list (list :kind 'transfer :transfer transfer))
                #'disco-media--cancel-cleanup-item))))
        (push owner disco-media--attachment-export-owners)
        (unwind-protect
            (progn
              (setq transfer
                    (appkit-media-copy-or-download-resource-async
                     (disco-media--attachment-appkit-resource
                      attachment (and has-cached cached-path))
                     target
                     (lambda (downloaded)
                       (when (current-p)
                         (unwind-protect
                             (when (current-p)
                               (message "disco: downloaded attachment -> %s"
                                        downloaded))
                           (retire))))
                     (lambda (err)
                       (when (current-p)
                         (unwind-protect
                             (when (current-p)
                               (message
                                "disco: attachment download failed: %s"
                                (disco-media--attachment-error-message err)))
                           (retire))))))
              (setq returned-p t)
              (if (current-p)
                  (progn
                    (setf (plist-get owner :transfer) transfer)
                    transfer)
                ;; Reset or synchronous completion may retire OWNER before
                ;; the constructor exposes its returned handle.
                (cancel-returned)
                nil))
          (unless returned-p
            (retire)
            (cancel-returned)))))))

(defun disco-media-open-attachment (attachment &optional owner)
  "Open or play ATTACHMENT according to its media kind.

OWNER is forwarded only to Appkit operations that can launch a video player."
  (let* ((kind (disco-media-attachment-kind attachment))
         (state (disco-media-attachment-download-state attachment))
         (path (plist-get state :path))
         (url (disco-media-attachment-download-url attachment)))
    (pcase kind
      ('video (disco-media-play-attachment-video attachment owner))
      ('audio (disco-media-play-attachment-audio attachment))
      ('photo
       (disco-media-open-discord-resource
        `((file . ,(and (appkit-media-file-present-p path) path))
          (url . ,url)
          (filename . ,(disco-media-attachment-display-name attachment))
          (content_type . ,(alist-get 'content_type attachment)))
        'image
        (format "attachment-open:%s"
                (disco-media-attachment-download-key attachment))))
      ('document
       (cond
        ((appkit-media-file-present-p path)
         (disco-media-open-downloaded-attachment attachment))
        ((appkit-media-url-present-p url)
         (disco-media-start-attachment-download attachment t))
        (t
         (user-error "disco: attachment has no openable source")))))))

(defun disco-media-attachment-card-context (attachment &optional owner)
  "Return shared media card context adapted from Discord ATTACHMENT.

OWNER is captured exactly by the card's open/play action."
  (let* ((state (disco-media-attachment-download-state attachment))
         (status (plist-get state :status))
         (path (plist-get state :path))
         (url (disco-media-attachment-download-url attachment))
         (has-local (and (stringp path) (file-exists-p path)))
         (has-url (appkit-media-url-present-p url)))
    (appkit-media-card-context-create
     :payload attachment
     :kind (disco-media-attachment-kind attachment)
     :title (disco-media-attachment-display-name attachment)
     :open-action (when (or has-local has-url)
                    (lambda ()
                      (disco-media-open-attachment attachment owner)))
     :download-action (when (and has-url
                                 (not (memq status '(downloading downloaded))))
                        (lambda ()
                          (disco-media-start-attachment-download attachment nil)))
     :cancel-action (when (eq status 'downloading)
                      (lambda ()
                        (disco-media-cancel-attachment-download attachment)))
     :save-as-action (when (or has-local has-url)
                       (lambda ()
                         (disco-media-download-attachment attachment)))
     :copy-url-action (when has-url
                        (lambda ()
                          (kill-new url)
                          (message "disco: copied attachment URL"))))))

(provide 'disco-media)

;;; disco-media.el ends here
