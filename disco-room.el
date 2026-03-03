;;; disco-room.el --- Channel room buffers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Per-channel room buffer with simple timeline rendering and message sending.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'seq)
(require 'ring)
(require 'cl-lib)
(require 'ewoc)
(require 'button)
(require 'browse-url)
(require 'url-handlers)
(require 'plz)
(require 'svg nil t)
(require 'disco-ui)
(require 'disco-util)
(require 'disco-markdown)
(require 'disco-media)
(require 'disco-embed)
(require 'disco-view)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-permission)
(require 'disco-transient)
(require 'disco-company)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--oldest-message-id nil)
(defvar disco-ui-card-indent-prefix)
(defvar disco-ui-card-indent-prefix-state)
(defvar-local disco-room--newest-message-id nil)
(defvar-local disco-room--history-exhausted nil)
(defvar-local disco-room--pending-reply-to nil)
(defvar-local disco-room--pending-jump-message-id nil)
(defvar-local disco-room--gateway-handler nil)
(defvar-local disco-room--refresh-generation 0)
(defvar-local disco-room--refresh-in-flight nil)
(defvar-local disco-room--older-in-flight nil)
(defvar-local disco-room--draft-input "")
(defvar-local disco-room--input-ring nil)
(defvar-local disco-room--input-index nil)
(defvar-local disco-room--input-pending nil)
(defvar-local disco-room--send-in-flight nil)
(defvar-local disco-room--last-search-query nil)
(defvar-local disco-room--input-marker nil)
(defvar-local disco-room--input-prompt-marker nil)
(defvar-local disco-room--rendering nil)
(defvar-local disco-room--ewoc nil)
(defvar-local disco-room--message-node-table nil)
(defvar-local disco-room--render-context-by-message-id nil)
(defvar-local disco-room--pending-attachments nil)
(defvar-local disco-room--attachment-token-table nil)
(defvar-local disco-room--attachment-token-seq 0)
(defvar-local disco-room--typing-users nil)
(defvar-local disco-room--typing-expire-timer nil)
(defvar-local disco-room--poll-selection-drafts nil)

(defconst disco-room--attachment-token-regexp "\\[file:\\([0-9]+\\)\\]"
  "Regexp used to match attachment tokens in room draft input.")

(defconst disco-room--message-flag-has-thread (ash 1 5)
  "Bit mask indicating message has an associated starter thread.")

(defconst disco-room--message-flag-has-snapshot (ash 1 14)
  "Bit mask indicating message carries a forward snapshot payload.")

(defconst disco-room--user-join-message-templates
  '["%s joined the party."
    "%s is here."
    "Welcome, %s. We hope you brought pizza."
    "A wild %s appeared."
    "%s just landed."
    "%s just slid into the server."
    "%s just showed up!"
    "Welcome %s. Say hi!"
    "%s hopped into the server."
    "Everyone welcome %s!"
    "Glad you're here, %s."
    "Good to see you, %s."
    "Yay you made it, %s!"]
  "Rendered templates for USER_JOIN system messages (type 7).")

(defvar disco-room--avatar-image-cache (make-hash-table :test #'equal)
  "Global avatar image cache keyed by avatar cache key.

Values are either image objects or the symbol `:missing'.")

(defvar disco-room--avatar-fetching (make-hash-table :test #'equal)
  "Global set of avatar cache keys currently being fetched.")

(defvar disco-room--avatar-round-image-cache (make-hash-table :test #'equal)
  "Global rounded avatar image cache keyed by file/size/mtime.")

(defvar disco-room--forward-guild-icon-image-cache (make-hash-table :test #'equal)
  "Global forwarded-source guild icon cache keyed by icon cache key.")

(defvar disco-room--forward-guild-icon-fetching (make-hash-table :test #'equal)
  "Global set of forwarded-source guild icon cache keys currently fetching.")

(defvar disco-room--attachment-download-state-table (make-hash-table :test #'equal)
  "Global attachment download state keyed by attachment download key.

Each value is a plist carrying :status, :path, :process, :error and
:cancel-requested flags.")

(defvar disco-room--avatar-fetch-budget nil
  "Dynamic cap for number of avatar fetches started in current render pass.")

(defvar disco-room--avatar-plz-queue nil
  "Shared plz queue used for asynchronous avatar downloads.")

(defvar disco-room--avatar-plz-queue-limit nil
  "Last applied queue limit for `disco-room--avatar-plz-queue'.")

(defconst disco-room--avatar-cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "img")
  "Preferred avatar cache file extension candidates.")

(defcustom disco-room-input-history-size 30
  "Maximum number of draft entries kept in room input history."
  :type 'integer
  :group 'disco)

(defcustom disco-room-send-on-return t
  "When non-nil, `RET' in room buffer sends current draft."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-enable-company-backend t
  "When non-nil, register `disco-room-company-completion' for room buffers.

The backend is only used when `company' is loaded and `company-mode' is
active."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-allowed-mentions nil
  "Default allowed mentions payload used when sending or editing messages.

Nil delegates to Discord defaults. `none' suppresses all mention parsing.
`all' explicitly enables users/roles/everyone parsing. Any alist/plist value is
forwarded as raw `allowed_mentions' object."
  :type '(choice
          (const :tag "Use Discord defaults" nil)
          (const :tag "Suppress all mentions" none)
          (const :tag "Allow users/roles/everyone" all)
          (sexp :tag "Custom allowed_mentions payload"))
  :group 'disco)

(defcustom disco-room-reply-mention-replied-user nil
  "When non-nil, include `allowed_mentions.replied_user' for replies."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-forward-comment-rejection-action 'split
  "How to handle sessions that reject forward comments with HTTP 400.

`split' retries as two messages: comment first, then pure forward.
`error' surfaces the API error without retrying."
  :type '(choice
          (const :tag "Retry as comment + forward" split)
          (const :tag "Show API error" error))
  :group 'disco)

(defcustom disco-room-forward-only-manual-fallback nil
  "When non-nil, allow manual forward_only entry if source fetch fails."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-avatar-images t
  "When non-nil, render author avatars as inline images when possible.

When image rendering is unavailable, room falls back to text placeholders."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-avatar-image-size 28
  "Pixel size used for inline avatar images in room timeline."
  :type 'integer
  :group 'disco)

(defcustom disco-room-avatar-round-images t
  "When non-nil, render room avatars using circular clipping when available."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-avatar-round-size-factor 0.90
  "Scale factor applied to computed avatar size for round rendering."
  :type 'number
  :group 'disco)

(defcustom disco-room-avatar-round-inset-ratio 0.08
  "Inset ratio used when clipping circular avatars."
  :type 'number
  :group 'disco)

(defcustom disco-room-avatar-factors-alist
  '((1 . (0.8 . 0.1))
    (2 . (0.8 . 0.1)))
  "Size coefficients used for avatar creation.

Each entry is (CHEIGHT CIRCLE-FACTOR . MARGIN-FACTOR), modeled after
telega's avatar sizing approach."
  :type '(alist :key-type (integer :tag "Height in chars")
          :value-type (cons (number :tag "Circle factor")
                            (number :tag "Margin factor")))
  :group 'disco)

(defcustom disco-room-avatar-extra-bottom-line t
  "When non-nil, add one extra hidden line to 2-line avatars.

This mirrors telega's gap workaround and keeps slice seams stable."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-avatar-rounding-default-char-width 2
  "Fallback avatar width in columns when geometry cannot be derived."
  :type 'integer
  :group 'disco)

(defcustom disco-room-avatar-cache-directory
  (locate-user-emacs-file "disco-avatar-cache/")
  "Directory used to cache downloaded avatar images."
  :type 'directory
  :group 'disco)

(defcustom disco-room-attachment-cache-directory
  (locate-user-emacs-file "disco-attachment-cache/")
  "Directory used to cache downloaded attachment preview images."
  :type 'directory
  :group 'disco)

(defcustom disco-room-attachment-download-directory
  (locate-user-emacs-file "disco-attachment-downloads/")
  "Directory used for telega-style default attachment downloads."
  :type 'directory
  :group 'disco)

(defcustom disco-room-avatar-max-fetches-per-render nil
  "Maximum avatar fetches started during one room render pass.

When nil, avatar fetches are uncapped per render pass
(queue concurrency still applies)."
  :type '(choice
          (const :tag "No per-render cap" nil)
          integer)
  :group 'disco)

(defcustom disco-room-attachment-preview-max-fetches-per-render 4
  "Maximum attachment preview fetches started during one room render pass.

Set to nil to disable per-render capping."
  :type '(choice
          (const :tag "No per-render cap" nil)
          integer)
  :group 'disco)

(defcustom disco-room-avatar-fetch-concurrency 20
  "Maximum concurrent avatar downloads in plz queue."
  :type 'integer
  :group 'disco)

(defcustom disco-room-attachment-preview-fetch-concurrency 6
  "Maximum concurrent attachment preview downloads in plz queue."
  :type 'integer
  :group 'disco)

(defcustom disco-room-show-attachments t
  "When non-nil, render attachment details under each message."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-use-rich-attachment-cards t
  "When non-nil, render telega-inspired rich cards for attachments."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-attachment-image-previews t
  "When non-nil, render inline previews for image/video attachments."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-video-player-command
  (cond
   ((executable-find "mpv") "mpv")
   ((executable-find "vlc") "vlc")
   ((executable-find "ffplay") "ffplay -autoexit")
   (t nil))
  "Command used to play video URLs/files from cards.

When nil, fallback uses browser handlers (`browse-url` / `browse-url-of-file`)."
  :type '(choice
          (const :tag "Use browser" nil)
          (string :tag "Command line"))
  :group 'disco)

(defcustom disco-room-attachment-preview-max-width 460
  "Maximum pixel width used for inline attachment previews."
  :type 'integer
  :group 'disco)

(defcustom disco-room-attachment-preview-max-height 360
  "Maximum pixel height used for inline attachment previews."
  :type 'integer
  :group 'disco)

(defcustom disco-room-show-embeds t
  "When non-nil, render embed details under each message."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-use-rich-embed-cards t
  "When non-nil, render telega-inspired rich cards for embeds."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-embed-image-previews t
  "When non-nil, render inline image/video previews for embed media."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-embed-author-icons t
  "When non-nil, render inline author icons in embed metadata rows."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-embed-author-icon-size 18
  "Pixel size used for inline embed author icons."
  :type 'integer
  :group 'disco)

(defcustom disco-room-use-rich-forward-cards t
  "When non-nil, render forwarded-message metadata as rich cards."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-forward-guild-icons t
  "When non-nil, show guild icons in forwarded-source metadata rows."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-forward-guild-icon-size 16
  "Pixel size used for forwarded-source guild icons."
  :type 'integer
  :group 'disco)

(defcustom disco-room-embed-description-limit nil
  "Maximum description length rendered in embed cards.

Set to 0 to disable embed description rendering, or nil for no limit."
  :type '(choice
          (const :tag "No limit" nil)
          (const :tag "Disable description" 0)
          integer)
  :group 'disco)

(defcustom disco-room-show-reactions t
  "When non-nil, render reaction chips under each message."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-polls t
  "When non-nil, render poll blocks under each message containing poll data."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-poll-show-voter-counts t
  "When non-nil, render per-answer vote counts in poll rows."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-poll-show-total-votes t
  "When non-nil, render total vote count in poll metadata line."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-poll-date-format "%Y-%m-%d %H:%M"
  "Time format used for poll expiry labels."
  :type 'string
  :group 'disco)

(defcustom disco-room-poll-auto-toggle-vote t
  "When non-nil, clicking a poll option immediately submits vote change."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-poll-confirm-expire t
  "When non-nil, ask before ending a poll via command/button."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-poll-default-duration-hours 24
  "Default duration in hours used by `disco-room-send-poll'."
  :type 'integer
  :group 'disco)

(defcustom disco-room-poll-max-options 10
  "Maximum number of options collected by `disco-room-send-poll'."
  :type 'integer
  :group 'disco)

(defcustom disco-room-poll-button-face 'disco-room-reaction
  "Face used for poll action buttons in message rows."
  :type 'face
  :group 'disco)

(defcustom disco-room-poll-voted-face 'disco-room-poll-option-selected
  "Face used for poll options selected by current user."
  :type 'face
  :group 'disco)

(defcustom disco-room-poll-option-face 'disco-room-poll-option
  "Face used for unselected poll options."
  :type 'face
  :group 'disco)

(defcustom disco-room-poll-meta-face 'disco-room-poll-meta
  "Face used for poll metadata lines."
  :type 'face
  :group 'disco)

(defcustom disco-room-poll-title-face 'disco-room-poll-title
  "Face used for poll question/title lines."
  :type 'face
  :group 'disco)

(defcustom disco-room-show-typing-indicators t
  "When non-nil, show live typing status above the room prompt.

Typing events require Discord gateway typing intents when custom intents are
specified via `disco-gateway-identify-intents'."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-typing-indicator-timeout 10
  "Seconds after which a typing indicator is considered stale.

Discord typing indicators are ephemeral and should expire quickly when no new
`TYPING_START' event arrives."
  :type 'integer
  :group 'disco)

(defcustom disco-room-group-messages t
  "When non-nil, collapse repeated message headers for same sender.

Grouping applies when sender stays the same and timestamps are within
`disco-room-group-messages-timespan' seconds."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-group-messages-timespan 120
  "Maximum age gap in seconds for grouped same-sender messages."
  :type 'integer
  :group 'disco)

(defcustom disco-room-show-date-separators t
  "When non-nil, insert date separator rows between day boundaries."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-unread-divider t
  "When non-nil, render an unread divider before first unread message."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-right-align-timestamps t
  "When non-nil, render message time tags aligned to the right edge."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-wrap-long-lines t
  "When non-nil, visually wrap long timeline lines in room buffers.

This mirrors telega chat buffers by enabling `visual-line-mode' and disabling
`truncate-lines'."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-use-visual-fill-column nil
  "When non-nil, enable `visual-fill-column-mode' in room buffers when available.

This is optional and requires the external `visual-fill-column' package."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-fill-column nil
  "Preferred fill column for room buffers.

When non-nil and visual fill mode is active, set local `fill-column' to this
value before enabling visual fill."
  :type '(choice
          (const :tag "Use current fill-column" nil)
          integer)
  :group 'disco)

(defcustom disco-room-show-attachment-urls nil
  "When non-nil, include raw attachment URLs in message rendering."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-show-embed-urls nil
  "When non-nil, include raw embed URLs in message rendering."
  :type 'boolean
  :group 'disco)

(defface disco-room-timestamp
  '((t :inherit shadow))
  "Face used for room message timestamps."
  :group 'disco)

(defface disco-room-message-meta
  '((t :inherit shadow))
  "Face used for room message metadata rows."
  :group 'disco)

(defface disco-room-typing-indicator
  '((t :inherit shadow :slant italic))
  "Face used for transient typing indicator text near the room prompt."
  :group 'disco)

(defface disco-room-attachment-card-border
  '((t :inherit shadow))
  "Face used for attachment card border glyphs."
  :group 'disco)

(defface disco-room-attachment-card-title
  '((t :inherit default :weight bold))
  "Face used for attachment card title row."
  :group 'disco)

(defface disco-room-attachment-card-meta
  '((t :inherit shadow))
  "Face used for attachment card metadata rows."
  :group 'disco)

(defface disco-room-attachment-card-action
  '((t :inherit link))
  "Face used for attachment card action buttons."
  :group 'disco)

(defface disco-room-embed-card-border
  '((t :inherit disco-room-attachment-card-border))
  "Face used for embed card border glyphs."
  :group 'disco)

(defface disco-room-embed-card-title
  '((t :inherit disco-room-attachment-card-title))
  "Face used for embed card title row."
  :group 'disco)

(defface disco-room-embed-card-meta
  '((t :inherit disco-room-attachment-card-meta))
  "Face used for embed card metadata rows."
  :group 'disco)

(defface disco-room-embed-card-action
  '((t :inherit disco-room-attachment-card-action))
  "Face used for embed card action buttons."
  :group 'disco)

(defface disco-room-forward-card-border
  '((t :inherit disco-room-embed-card-border))
  "Face used for forwarded-message card border glyphs."
  :group 'disco)

(defface disco-room-forward-card-title
  '((t :inherit disco-room-embed-card-title))
  "Face used for forwarded-message card title row."
  :group 'disco)

(defface disco-room-forward-card-meta
  '((t :inherit disco-room-embed-card-meta))
  "Face used for forwarded-message card metadata rows."
  :group 'disco)

(defface disco-room-forward-card-action
  '((t :inherit disco-room-embed-card-action))
  "Face used for forwarded-message card action buttons."
  :group 'disco)

(defface disco-room-reaction
  '((t :inherit mode-line-inactive))
  "Face used for unselected reaction chips."
  :group 'disco)

(defface disco-room-reaction-selected
  '((t :inherit success :weight bold))
  "Face used for reactions selected by the current user."
  :group 'disco)

(defface disco-room-poll-title
  '((t :inherit bold))
  "Face used for poll title lines."
  :group 'disco)

(defface disco-room-poll-meta
  '((t :inherit shadow))
  "Face used for poll metadata lines."
  :group 'disco)

(defface disco-room-poll-option
  '((t :inherit default))
  "Face used for poll option rows."
  :group 'disco)

(defface disco-room-poll-option-selected
  '((t :inherit success :weight bold))
  "Face used for selected poll option rows."
  :group 'disco)

(defface disco-room-date-separator
  '((t :inherit font-lock-comment-face :weight bold))
  "Face used for room date separator rows."
  :group 'disco)

(defface disco-room-unread-divider
  '((t :inherit warning :weight bold))
  "Face used for room unread divider row."
  :group 'disco)

(defface disco-room-author-color-1
  '((t :foreground "LightSkyBlue"))
  "Face palette entry 1 for room author names."
  :group 'disco)

(defface disco-room-author-color-2
  '((t :foreground "PaleGreen"))
  "Face palette entry 2 for room author names."
  :group 'disco)

(defface disco-room-author-color-3
  '((t :foreground "Khaki"))
  "Face palette entry 3 for room author names."
  :group 'disco)

(defface disco-room-author-color-4
  '((t :foreground "LightSalmon"))
  "Face palette entry 4 for room author names."
  :group 'disco)

(defface disco-room-author-color-5
  '((t :foreground "Plum1"))
  "Face palette entry 5 for room author names."
  :group 'disco)

(defface disco-room-author-color-6
  '((t :foreground "LightSteelBlue"))
  "Face palette entry 6 for room author names."
  :group 'disco)

(defface disco-room-author-color-7
  '((t :foreground "Aquamarine"))
  "Face palette entry 7 for room author names."
  :group 'disco)

(defface disco-room-author-color-8
  '((t :foreground "Wheat"))
  "Face palette entry 8 for room author names."
  :group 'disco)

(defconst disco-room--author-faces
  [disco-room-author-color-1
   disco-room-author-color-2
   disco-room-author-color-3
   disco-room-author-color-4
   disco-room-author-color-5
   disco-room-author-color-6
   disco-room-author-color-7
   disco-room-author-color-8]
  "Palette used for deterministic per-author name coloring.")

(defun disco-room--configure-input-map (map)
  "Apply draft-input bindings to MAP and return MAP."
  ;; Reset stale bindings before layering room actions over global editing.
  (setcdr map nil)
  (set-keymap-parent map (current-global-map))
  (define-key map (kbd "TAB") #'disco-room-complete-mention)
  (define-key map (kbd "<tab>") #'disco-room-complete-mention)
  (define-key map (kbd "C-M-i") #'disco-room-complete-mention)
  (define-key map (kbd "RET") #'disco-room-return-dwim)
  (define-key map (kbd "C-c C-c") #'disco-room-send-message)
  (define-key map (kbd "C-c '") #'disco-room-edit-draft)
  (define-key map (kbd "M-p") #'disco-room-draft-prev)
  (define-key map (kbd "M-n") #'disco-room-draft-next)
  (define-key map (kbd "C-c C-k") #'disco-room-cancel-reply)
  (define-key map (kbd "C-c C-p s") #'disco-room-send-poll)
  (define-key map (kbd "C-c C-p +") #'disco-room-vote-poll-answer)
  (define-key map (kbd "C-c C-p -") #'disco-room-remove-poll-vote)
  (define-key map (kbd "C-c C-p t") #'disco-room-toggle-poll-answer)
  (define-key map (kbd "C-c C-p v") #'disco-room-submit-poll-vote)
  (define-key map (kbd "C-c C-p c") #'disco-room-clear-poll-votes)
  (define-key map (kbd "C-c C-p e") #'disco-room-expire-poll)
  (define-key map (kbd "C-c C-f") #'disco-room-attach-file)
  (define-key map (kbd "C-c C-F") #'disco-room-forward-message)
  (define-key map (kbd "C-c C-d") #'disco-room-remove-attachment-token-at-point)
  (define-key map (kbd "C-c C-x") #'disco-room-clear-attachments)
  (define-key map (kbd "C-c M-l") #'disco-room-list-attachments)
  (define-key map (kbd "C-c M-e") #'disco-room-edit-attachment-description)
  (define-key map (kbd "C-c M-r") #'disco-room-reorder-attachments)
  map)

(defvar disco-room-input-map
  (disco-room--configure-input-map (make-sparse-keymap))
  "Keymap active when point is inside the room draft region.")

;; Refresh bindings on reload since `defvar' preserves existing map objects.
(disco-room--configure-input-map disco-room-input-map)

(defun disco-room--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-room--thread-channel-p (&optional channel)
  "Return non-nil when CHANNEL (or current room channel) is a thread."
  (let ((target (or channel (disco-room--channel-object))))
    (and target (disco-state-channel-thread-p target))))

(defun disco-room--thread-metadata (&optional channel)
  "Return thread metadata alist for CHANNEL or current room channel."
  (let ((target (or channel (disco-room--channel-object))))
    (or (alist-get 'thread_metadata target) '())))

(defun disco-room--thread-archived-p (&optional channel)
  "Return non-nil when CHANNEL thread is archived."
  (let* ((target (or channel (disco-room--channel-object)))
         (meta (disco-room--thread-metadata target)))
    (or (disco-util-json-true-p (alist-get 'archived meta))
        (disco-util-json-true-p (alist-get 'archived target)))))

(defun disco-room--thread-locked-p (&optional channel)
  "Return non-nil when CHANNEL thread is locked."
  (let* ((target (or channel (disco-room--channel-object)))
         (meta (disco-room--thread-metadata target)))
    (or (disco-util-json-true-p (alist-get 'locked meta))
        (disco-util-json-true-p (alist-get 'locked target)))))

(defun disco-room--thread-header-suffix ()
  "Return human-readable status suffix for thread header."
  (if (not (disco-room--thread-channel-p))
      ""
    (let (tags)
      (when (disco-room--thread-archived-p)
        (push "archived" tags))
      (when (disco-room--thread-locked-p)
        (push "locked" tags))
      (when (= (alist-get 'type (disco-room--channel-object)) 12)
        (push "private" tags))
      (if tags
          (format " [thread: %s]" (mapconcat #'identity (nreverse tags) ", "))
        " [thread]"))))

(defun disco-room--ensure-thread-channel ()
  "Signal user error unless current room channel is a thread."
  (unless (disco-room--thread-channel-p)
    (user-error "disco: current room is not a thread")))

(defun disco-room--ensure-parent-channel ()
  "Signal user error when current room channel is itself a thread."
  (when (disco-room--thread-channel-p)
    (user-error "disco: open a parent channel room to create a new thread")))

(defun disco-room--permission-display-name (permission)
  "Return human-readable display name for PERMISSION symbol/designator."
  (let* ((raw (cond
               ((keywordp permission) (substring (symbol-name permission) 1))
               ((symbolp permission) (symbol-name permission))
               ((stringp permission) permission)
               ((integerp permission) (format "0x%X" permission))
               (t (format "%s" permission))))
         (trimmed (replace-regexp-in-string "\\`:+" "" raw))
         (snake (replace-regexp-in-string "[[:space:]-]+" "_" trimmed)))
    (upcase snake)))

(defun disco-room--required-send-permissions (&optional channel)
  "Return permission list required to send message in CHANNEL.

When CHANNEL is nil, use current room channel."
  (if (disco-room--thread-channel-p channel)
      '(send-messages-in-threads)
    '(send-messages)))

(defun disco-room--poll-vote-required-permissions (&optional channel)
  "Return required permissions for poll vote actions in CHANNEL."
  (disco-room--required-send-permissions channel))

(defun disco-room--poll-expire-required-permissions (&optional channel)
  "Return required permissions for poll expire action in CHANNEL."
  (append (disco-room--required-send-permissions channel)
          '(send-polls)))

(cl-defun disco-room--channel-has-permissions-p (permissions &optional channel (unknown-value t))
  "Return non-nil when CHANNEL has all PERMISSIONS.

CHANNEL defaults to current room channel. UNKNOWN-VALUE controls fallback when
computed permissions are unavailable."
  (disco-permission-channel-has-all-p
   (or channel (disco-room--channel-object))
   permissions
   unknown-value))

(cl-defun disco-room--poll-owned-by-current-user-p (msg &optional (unknown-value t))
  "Return non-nil when poll in MSG is owned by current user.

If current user identity is unknown, return UNKNOWN-VALUE."
  (let* ((author-id (and (listp msg) (disco-room--message-author-id msg)))
         (self-id (disco-gateway-current-user-id)))
    (if (or (null author-id) (null self-id))
        unknown-value
      (equal (format "%s" author-id) (format "%s" self-id)))))

(defun disco-room--poll-can-vote-p (msg)
  "Return non-nil when current user can vote in poll message MSG."
  (let ((poll (disco-room--message-poll msg)))
    (and poll
         (not (disco-room--poll-expired-p poll))
         (disco-room--channel-has-permissions-p
          (disco-room--poll-vote-required-permissions)))))

(defun disco-room--poll-can-expire-p (msg)
  "Return non-nil when current user can end poll message MSG."
  (let ((poll (disco-room--message-poll msg)))
    (and poll
         (not (disco-room--poll-expired-p poll))
         (disco-room--poll-owned-by-current-user-p msg t)
         (disco-room--channel-has-permissions-p
          (disco-room--poll-expire-required-permissions)))))

(cl-defun disco-room--ensure-channel-permissions (permissions &key action (unknown-value t))
  "Signal user error when current room channel misses PERMISSIONS.

ACTION is optional text appended to error message.
When UNKNOWN-VALUE is non-nil, missing/unparseable channel permissions are
treated as allowed."
  (let* ((channel (disco-room--channel-object))
         (missing (disco-permission-channel-missing channel permissions unknown-value)))
    (when missing
      (user-error
       "disco: missing permission%s %s%s"
       (if (> (length missing) 1) "s" "")
       (mapconcat #'disco-room--permission-display-name missing ", ")
       (if (and (stringp action) (not (string-empty-p action)))
           (format " for %s" action)
         "")))))

(defun disco-room--typing-timeout-seconds ()
  "Return normalized typing indicator timeout in seconds."
  (max 1 (or disco-room-typing-indicator-timeout 10)))

(defun disco-room--typing-normalize-user-id (user-id)
  "Return USER-ID as string, or nil when missing."
  (and user-id (format "%s" user-id)))

(defun disco-room--typing-member-display-name (member)
  "Extract display name from gateway MEMBER payload."
  (when (listp member)
    (let* ((nick (alist-get 'nick member))
           (user (alist-get 'user member))
           (global-name (and (listp user) (alist-get 'global_name user)))
           (username (and (listp user) (alist-get 'username user))))
      (seq-find (lambda (candidate)
                  (and (stringp candidate)
                       (not (string-empty-p candidate))))
                (list nick global-name username)))))

(defun disco-room--typing-channel-recipient-display-name (user-id)
  "Resolve USER-ID display name from current DM/group channel metadata."
  (let* ((channel (disco-room--channel-object))
         (recipients (and (listp channel) (alist-get 'recipients channel)))
         (match
          (seq-find
           (lambda (recipient)
             (and (listp recipient)
                  (equal (disco-room--typing-normalize-user-id
                          (alist-get 'id recipient))
                         user-id)))
           (or recipients '()))))
    (when (listp match)
      (let ((global-name (alist-get 'global_name match))
            (username (alist-get 'username match)))
        (seq-find (lambda (candidate)
                    (and (stringp candidate)
                         (not (string-empty-p candidate))))
                  (list global-name username))))))

(defun disco-room--typing-history-display-name (user-id)
  "Resolve USER-ID display name from loaded room message history."
  (let ((found nil))
    (dolist (msg (or (disco-state-messages disco-room--channel-id) '()))
      (when (and (null found)
                 (equal (disco-room--typing-normalize-user-id
                         (disco-room--message-author-id msg))
                        user-id))
        (let ((candidate (disco-room--message-author msg)))
          (when (and (stringp candidate) (not (string-empty-p candidate)))
            (setq found candidate)))))
    found))

(defun disco-room--typing-display-name (user-id &optional member)
  "Return best-effort display name for typing USER-ID and MEMBER payload."
  (or (disco-room--typing-member-display-name member)
      (let ((existing (and (hash-table-p disco-room--typing-users)
                           (gethash user-id disco-room--typing-users))))
        (and (listp existing)
             (plist-get existing :display-name)))
      (disco-room--typing-channel-recipient-display-name user-id)
      (disco-room--typing-history-display-name user-id)
      (format "user-%s"
              (if (> (length user-id) 4)
                  (substring user-id (- (length user-id) 4))
                user-id))))

(defun disco-room--typing-prune-expired (&optional now)
  "Drop expired typing entries and return non-nil when anything changed."
  (let ((cutoff (or now (float-time)))
        stale-ids)
    (when (hash-table-p disco-room--typing-users)
      (maphash
       (lambda (user-id entry)
         (let ((expires-at (plist-get entry :expires-at)))
           (when (or (not (numberp expires-at))
                     (<= expires-at cutoff))
             (push user-id stale-ids))))
       disco-room--typing-users))
    (dolist (user-id stale-ids)
      (remhash user-id disco-room--typing-users))
    (and stale-ids t)))

(defun disco-room--typing-active-entries ()
  "Return active typing entries sorted by recent typing timestamp."
  (disco-room--typing-prune-expired)
  (let (entries)
    (when (hash-table-p disco-room--typing-users)
      (maphash
       (lambda (_user-id entry)
         (when (listp entry)
           (push entry entries)))
       disco-room--typing-users))
    (sort entries
          (lambda (left right)
            (let ((left-updated (or (plist-get left :updated-at) 0))
                  (right-updated (or (plist-get right :updated-at) 0))
                  (left-name (or (plist-get left :display-name) ""))
                  (right-name (or (plist-get right :display-name) "")))
              (if (= left-updated right-updated)
                  (string-lessp (downcase left-name) (downcase right-name))
                (> left-updated right-updated)))))))

(defun disco-room--typing-indicator-text ()
  "Return one-line typing indicator text, or nil when idle."
  (when disco-room-show-typing-indicators
    (let* ((entries (disco-room--typing-active-entries))
           (names (mapcar (lambda (entry)
                            (or (plist-get entry :display-name) "unknown"))
                          entries))
           (count (length names)))
      (pcase count
        (0 nil)
        (1 (format "%s is typing..." (nth 0 names)))
        (2 (format "%s and %s are typing..." (nth 0 names) (nth 1 names)))
        (3 (format "%s, %s and %s are typing..."
                   (nth 0 names) (nth 1 names) (nth 2 names)))
        (_ (format "%s, %s and %d others are typing..."
                   (nth 0 names) (nth 1 names) (- count 2)))))))

(defun disco-room--typing-next-expiry ()
  "Return nearest typing expiry timestamp, or nil when none remain."
  (let (next-expiry)
    (when (hash-table-p disco-room--typing-users)
      (maphash
       (lambda (_user-id entry)
         (let ((expires-at (plist-get entry :expires-at)))
           (when (numberp expires-at)
             (setq next-expiry
                   (if (or (null next-expiry)
                           (< expires-at next-expiry))
                       expires-at
                     next-expiry)))))
       disco-room--typing-users))
    next-expiry))

(defun disco-room--typing-cancel-expire-timer ()
  "Cancel room-local typing expiry timer when active."
  (when (timerp disco-room--typing-expire-timer)
    (cancel-timer disco-room--typing-expire-timer))
  (setq disco-room--typing-expire-timer nil))

(defun disco-room--typing-expire-timer-callback (buffer)
  "Expire stale typing entries for room BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq disco-room--typing-expire-timer nil)
      (when (disco-room--typing-prune-expired)
        (unless disco-room--rendering
          (disco-room--render-preserving-point)))
      (disco-room--typing-reschedule-expire-timer))))

(defun disco-room--typing-reschedule-expire-timer ()
  "Reschedule room-local timer for the next typing expiry."
  (disco-room--typing-cancel-expire-timer)
  (let ((next-expiry (disco-room--typing-next-expiry)))
    (when next-expiry
      (let ((delay (max 0.1 (- next-expiry (float-time))))
            (room-buffer (current-buffer)))
        (setq disco-room--typing-expire-timer
              (run-at-time delay nil
                           #'disco-room--typing-expire-timer-callback
                           room-buffer))))))

(defun disco-room--typing-reset ()
  "Clear all local typing indicator state for current room."
  (disco-room--typing-cancel-expire-timer)
  (setq disco-room--typing-users (make-hash-table :test #'equal)))

(defun disco-room--typing-track-user (user-id &optional member timestamp)
  "Track typing state for USER-ID with optional MEMBER and TIMESTAMP."
  (when disco-room-show-typing-indicators
    (let* ((normalized-id (disco-room--typing-normalize-user-id user-id))
           (self-id (disco-gateway-current-user-id)))
      (when (and normalized-id
                 (not (equal normalized-id self-id)))
        (unless (hash-table-p disco-room--typing-users)
          (setq disco-room--typing-users (make-hash-table :test #'equal)))
        (let* ((now (float-time))
               (base-time (if (numberp timestamp)
                              (float timestamp)
                            now))
               (expires-at (+ base-time (disco-room--typing-timeout-seconds))))
          (when (> expires-at now)
            (let* ((display-name (disco-room--typing-display-name normalized-id member))
                   (existing (gethash normalized-id disco-room--typing-users))
                   (changed (or (null existing)
                                (not (equal (plist-get existing :display-name) display-name)))))
              (puthash normalized-id
                       (list :user-id normalized-id
                             :display-name display-name
                             :expires-at expires-at
                             :updated-at now)
                       disco-room--typing-users)
              (disco-room--typing-reschedule-expire-timer)
              (when changed
                (unless disco-room--rendering
                  (disco-room--render-preserving-point))))))))))

(defun disco-room--typing-stop-user (user-id &optional no-rerender)
  "Remove USER-ID from typing indicators.

When NO-RERENDER is non-nil, update local state without rendering."
  (let ((normalized-id (disco-room--typing-normalize-user-id user-id)))
    (when (and normalized-id
               (hash-table-p disco-room--typing-users)
               (gethash normalized-id disco-room--typing-users))
      (remhash normalized-id disco-room--typing-users)
      (disco-room--typing-reschedule-expire-timer)
      (unless (or no-rerender disco-room--rendering)
        (disco-room--render-preserving-point)))))

(defun disco-room--latest-message-id ()
  "Return newest known message ID for current room, or nil."
  (alist-get 'id (car (disco-state-messages disco-room--channel-id))))

(defun disco-room--plz-error-http-status (err)
  "Return HTTP status from plz ERR object, or nil if unavailable."
  (when (and (fboundp 'plz-error-p)
             (plz-error-p err)
             (fboundp 'plz-error-response)
             (fboundp 'plz-response-status))
    (let ((response (ignore-errors (plz-error-response err))))
      (and response
           (ignore-errors (plz-response-status response))))))

(defun disco-room--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (and (fboundp 'plz-error-p)
           (plz-error-p err)
           (let* ((msg (and (fboundp 'plz-error-message)
                            (ignore-errors (plz-error-message err))))
                  (status (disco-room--plz-error-http-status err)))
             (cond
              ((and (stringp msg) (not (string-empty-p msg)))
               (if status
                   (format "%s (HTTP %s)" msg status)
                 msg))
              (status (format "HTTP %s" status))
              (t nil))))
      (condition-case _
          (error-message-string err)
        (error
         (format "%S" err)))))

(defun disco-room--callback-active-p (room-buffer channel-id generation)
  "Return non-nil when async callback state still matches ROOM-BUFFER context."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (= disco-room--refresh-generation generation)))))

(defun disco-room--channel-buffer-p (room-buffer channel-id)
  "Return non-nil when ROOM-BUFFER is alive and still bound to CHANNEL-ID."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)))))

(defun disco-room--current-draft ()
  "Return current room draft string."
  (or disco-room--draft-input ""))

(defun disco-room--next-attachment-token-id ()
  "Return next unique attachment token id for current room buffer."
  (setq disco-room--attachment-token-seq (1+ (or disco-room--attachment-token-seq 0)))
  (number-to-string disco-room--attachment-token-seq))

(defun disco-room--attachment-token-text (token-id)
  "Return textual draft token representation for TOKEN-ID."
  (format "[file:%s]" token-id))

(defun disco-room--attachment-token-ids-in-text (text)
  "Return attachment token ids found in TEXT, preserving first-seen order."
  (let ((pos 0)
        (ids '())
        (seen (make-hash-table :test #'equal)))
    (while (and (stringp text)
                (< pos (length text))
                (string-match disco-room--attachment-token-regexp text pos))
      (let ((token-id (match-string 1 text)))
        (unless (gethash token-id seen)
          (puthash token-id t seen)
          (push token-id ids)))
      (setq pos (match-end 0)))
    (nreverse ids)))

(defun disco-room--attachment-by-token-id (token-id)
  "Return attachment plist by TOKEN-ID from current room token table."
  (and disco-room--attachment-token-table
       (gethash token-id disco-room--attachment-token-table)))

(defun disco-room--attachments-from-draft (&optional draft)
  "Return ordered attachment list referenced by DRAFT token markers."
  (let ((text (or draft (disco-room--current-draft)))
        (attachments '()))
    (dolist (token-id (disco-room--attachment-token-ids-in-text text))
      (let ((attachment (disco-room--attachment-by-token-id token-id)))
        (when attachment
          (push (copy-tree attachment) attachments))))
    (nreverse attachments)))

(defun disco-room--draft-without-attachment-tokens (&optional draft)
  "Return DRAFT string with attachment tokens removed."
  (let* ((text (or draft (disco-room--current-draft)))
         (without (replace-regexp-in-string disco-room--attachment-token-regexp "" text)))
    (replace-regexp-in-string "[ \t][ \t]+" " " without)))

(defun disco-room--sync-pending-attachments-from-draft (&optional draft)
  "Refresh `disco-room--pending-attachments' using tokenized DRAFT references."
  (setq disco-room--pending-attachments
        (disco-room--attachments-from-draft draft)))

(defun disco-room--prune-unused-attachment-tokens (&optional draft)
  "Remove token table entries that are not referenced in DRAFT."
  (let* ((text (or draft (disco-room--current-draft)))
         (alive (make-hash-table :test #'equal)))
    (dolist (token-id (disco-room--attachment-token-ids-in-text text))
      (puthash token-id t alive))
    (when disco-room--attachment-token-table
      (maphash
       (lambda (token-id _attachment)
         (unless (gethash token-id alive)
           (remhash token-id disco-room--attachment-token-table)))
       disco-room--attachment-token-table))))

(defun disco-room--input-region-bounds ()
  "Return current writable draft region as (START . END), or nil."
  (when (and (markerp disco-room--input-marker)
             (eq (marker-buffer disco-room--input-marker) (current-buffer)))
    (let ((start (marker-position disco-room--input-marker)))
      (when (<= start (point-max))
        ;; Draft input is rendered at the end of buffer. Using point-max
        ;; avoids depending on field-property propagation edge cases.
        (cons start (point-max))))))

(defun disco-room--input-start-position ()
  "Return draft input start position for current room buffer, or nil."
  (let ((bounds (disco-room--input-region-bounds)))
    (and bounds (car bounds))))

(defun disco-room--input-prompt-start-position ()
  "Return prompt start position preceding current draft input, or nil."
  (and (markerp disco-room--input-prompt-marker)
       (eq (marker-buffer disco-room--input-prompt-marker) (current-buffer))
       (marker-position disco-room--input-prompt-marker)))

(defun disco-room--input-logical-end-position ()
  "Return logical draft end position, excluding synthetic trailing newline."
  (let ((bounds (disco-room--input-region-bounds)))
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (cond
         ((<= end start)
          start)
         ((eq (char-before end) ?\n)
          (max start (1- end)))
         (t end))))))

(defun disco-room--point-in-input-p (&optional position)
  "Return non-nil when POSITION (or point) is inside draft input region."
  (let* ((bounds (disco-room--input-region-bounds))
         (pos (or position (point))))
    (and bounds
         (<= (car bounds) pos)
         (<= pos (cdr bounds)))))

(defun disco-room--point-in-prompt-p (&optional position)
  "Return non-nil when POSITION (or point) is inside prompt glyph span."
  (let ((prompt-start (disco-room--input-prompt-start-position))
        (input-start (disco-room--input-start-position))
        (pos (or position (point))))
    (and (number-or-marker-p prompt-start)
         (number-or-marker-p input-start)
         (>= pos prompt-start)
         (< pos input-start))))

(defun disco-room--apply-input-text-properties ()
  "Ensure current draft span stays editable and uses `disco-room-input-map'."
  (let ((bounds (disco-room--input-region-bounds)))
    (when bounds
      (with-silent-modifications
        (add-text-properties
         (car bounds) (cdr bounds)
         (list 'read-only nil
               'local-map disco-room-input-map
               'rear-nonsticky '(read-only)))))))

(defun disco-room--post-command ()
  "Keep point out of prompt glyphs and off the synthetic trailing draft row."
  (unless disco-room--rendering
    (when (disco-room--point-in-prompt-p)
      (let ((input-start (disco-room--input-start-position)))
        (when (number-or-marker-p input-start)
          (goto-char input-start))))
    (let ((logical-end (disco-room--input-logical-end-position)))
      (when (and (number-or-marker-p logical-end)
                 (disco-room--point-in-input-p)
                 (> (point) logical-end))
        (goto-char logical-end)))))

(defun disco-room--sync-draft-from-buffer ()
  "Sync `disco-room--draft-input' from editable input region, when present."
  (let ((bounds (disco-room--input-region-bounds)))
    (when bounds
      (let* ((raw (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (text (replace-regexp-in-string "[\n\r]+\\'" "" raw)))
        (unless (equal text disco-room--draft-input)
          (setq disco-room--draft-input text)
          (setq disco-room--input-index nil)
          (setq disco-room--input-pending nil))
        (disco-room--prune-unused-attachment-tokens text)
        (disco-room--sync-pending-attachments-from-draft text)))))

(defun disco-room--after-change (beg end _old-len)
  "Keep draft state synced after editable-region changes from BEG to END."
  (unless disco-room--rendering
    (let ((bounds (disco-room--input-region-bounds)))
      (when (and bounds
                 (< beg (cdr bounds))
                 (> end (car bounds)))
        (disco-room--apply-input-text-properties)
        (disco-room--sync-draft-from-buffer)))))

(defun disco-room--set-draft (text)
  "Set room draft TEXT and re-render room."
  (setq disco-room--draft-input (or text ""))
  (disco-room--prune-unused-attachment-tokens disco-room--draft-input)
  (disco-room--sync-pending-attachments-from-draft disco-room--draft-input)
  (disco-room-render))

(defun disco-room--clear-draft ()
  "Clear room draft and reset draft history navigation state."
  (setq disco-room--draft-input "")
  (setq disco-room--input-index nil)
  (setq disco-room--input-pending nil))

(defun disco-room--input-history-push (input)
  "Push INPUT into draft history ring when non-empty and distinct."
  (let ((normalized (string-trim-right (or input ""))))
    (unless (or (string-empty-p normalized)
                (and disco-room--input-ring
                     (> (ring-length disco-room--input-ring) 0)
                     (equal normalized (ring-ref disco-room--input-ring 0))))
      (ring-insert disco-room--input-ring normalized)))
  (setq disco-room--input-index nil)
  (setq disco-room--input-pending nil))

(defun disco-room--input-history-goto (index)
  "Switch draft view to history entry INDEX.

When INDEX is nil, restore pending draft text."
  (setq disco-room--input-index index)
  (if (null index)
      (setq disco-room--draft-input (or disco-room--input-pending ""))
    (setq disco-room--draft-input (ring-ref disco-room--input-ring index)))
  (disco-room-render))

(defun disco-room-draft-prev (&optional n)
  "Replace draft with N previous entries from draft history."
  (interactive "p")
  (let* ((step (max 1 (or n 1)))
         (ring-size (and disco-room--input-ring (ring-length disco-room--input-ring))))
    (cond
     ((or (null ring-size) (= ring-size 0))
      (message "disco: draft history is empty"))
     (t
      (unless (integerp disco-room--input-index)
        (setq disco-room--input-pending (disco-room--current-draft))
        (setq disco-room--input-index -1))
      (let ((target (min (1- ring-size) (+ disco-room--input-index step))))
        (disco-room--input-history-goto target))))))

(defun disco-room-draft-next (&optional n)
  "Replace draft with N newer entries from draft history."
  (interactive "p")
  (let ((step (max 1 (or n 1))))
    (if (not (integerp disco-room--input-index))
        (message "disco: already at latest draft")
      (let ((target (- disco-room--input-index step)))
        (if (< target 0)
            (disco-room--input-history-goto nil)
          (disco-room--input-history-goto target))))))

(defun disco-room-edit-draft ()
  "Edit current room draft in minibuffer and re-render room."
  (interactive)
  (let ((updated (read-from-minibuffer "Draft: " (disco-room--current-draft))))
    (setq disco-room--input-index nil)
    (setq disco-room--input-pending nil)
    (disco-room--set-draft updated)))

(defun disco-room--mark-read (&optional message-id)
  "Mark current room as read and acknowledge MESSAGE-ID.

When MESSAGE-ID is nil, acknowledge the newest known message in the room.
Unread counters are always cleared locally."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (generation disco-room--refresh-generation)
         (channel (disco-room--channel-object))
         (target-id (or message-id
                        (disco-room--latest-message-id)
                        (and channel (alist-get 'last_message_id channel))))
         (last-read-id (disco-state-channel-last-read-message-id channel-id))
         (should-ack (and target-id
                          (or (null last-read-id)
                              (disco-state-snowflake< last-read-id target-id)))))
    (disco-state-clear-channel-unread channel-id)
    (when should-ack
      (let ((token (disco-state-channel-ack-token channel-id)))
        (disco-api-ack-message-async
         channel-id
         target-id
         :token token
         :on-success
         (lambda (response)
           (when (disco-room--callback-active-p room-buffer channel-id generation)
             (disco-state-set-channel-last-read-message-id channel-id target-id)
             (let ((token-pair (and (listp response) (assq 'token response))))
               (when token-pair
                 (disco-state-set-channel-ack-token channel-id (cdr token-pair))))))
         :on-error
         (lambda (err)
           (message "disco: read-state ack failed for %s: %s"
                    channel-id
                    (disco-room--async-error-message err))))))))

(defun disco-room--update-message-window-state (messages)
  "Update pagination cursors from MESSAGES (newest-first list)."
  (setq disco-room--newest-message-id (and messages (alist-get 'id (car messages))))
  (setq disco-room--oldest-message-id
        (and messages (alist-get 'id (car (last messages))))))

(defun disco-room--merge-message-pages (existing older)
  "Merge EXISTING newest-first messages with OLDER page, de-duplicated.

Both EXISTING and OLDER are newest-first lists."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (msg (append existing older))
      (let ((message-id (alist-get 'id msg)))
        (unless (and message-id (gethash message-id seen))
          (when message-id
            (puthash message-id t seen))
          (push msg merged))))
    (nreverse merged)))

(defun disco-room--message-id-at-point ()
  "Return message ID at point, or signal a user error.

Message lines carry the `disco-message-id' text property."
  (or (get-text-property (point) 'disco-message-id)
      (get-text-property (line-beginning-position) 'disco-message-id)
      (user-error "disco: point is not on a message line")))

(defun disco-room--capture-window-input-offsets ()
  "Return (WINDOW . OFFSET) pairs for windows currently in draft input."
  (let ((bounds (disco-room--input-region-bounds))
        offsets)
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (let ((window-point (window-point win)))
            (when (and (<= start window-point)
                       (<= window-point end))
              (push (cons win (- window-point start)) offsets))))))
    offsets))

(defun disco-room--restore-window-input-offsets (offsets)
  "Restore window points in OFFSETS relative to current draft input start."
  (let ((start (disco-room--input-start-position))
        (logical-end (disco-room--input-logical-end-position)))
    (when (and (number-or-marker-p start)
               (number-or-marker-p logical-end))
      (dolist (entry offsets)
        (let ((win (car entry))
              (offset (cdr entry)))
          (when (and (window-live-p win)
                     (eq (window-buffer win) (current-buffer)))
            (set-window-point
             win
             (min logical-end
                  (max start (+ start offset))))))))))

(defun disco-room--at-message-bottom-p ()
  "Return non-nil when point is at timeline bottom, outside draft input."
  (and (= (point) (point-max))
       (not (disco-room--point-in-input-p))))

(defun disco-room--render-preserving-point ()
  "Rerender room while preserving message reading position.

This follows telega-like behavior: preserve anchor + viewport for passive
updates, and keep draft cursor stable when point is in the composer."
  (let* ((window-input-offsets (disco-room--capture-window-input-offsets))
         (input-start (disco-room--input-start-position))
         (in-input (and (number-or-marker-p input-start)
                        (disco-room--point-in-input-p)))
         (input-offset (and in-input (- (point) input-start))))
    (unwind-protect
        (if (and in-input (numberp input-offset))
            (progn
              (disco-room-render)
              (let ((new-start (disco-room--input-start-position))
                    (logical-end (disco-room--input-logical-end-position)))
                (when (and (number-or-marker-p new-start)
                           (number-or-marker-p logical-end))
                  (goto-char (min logical-end
                                  (max new-start (+ new-start input-offset)))))))
          (disco-view-render-preserving-position
           #'disco-room-render
           :anchor-property 'disco-message-id
           :preserve-window-start t))
      (disco-room--restore-window-input-offsets window-input-offsets))))

(defun disco-room--message-by-id (message-id)
  "Return room message object for MESSAGE-ID, or nil."
  (seq-find (lambda (msg)
              (equal (alist-get 'id msg) message-id))
            (or (disco-state-messages disco-room--channel-id) '())))

(defun disco-room--channel-message-by-id (channel-id message-id)
  "Return cached MESSAGE-ID from CHANNEL-ID, or nil."
  (let ((normalized-channel-id (disco-room--normalize-id channel-id))
        (normalized-message-id (disco-room--normalize-id message-id)))
    (when (and normalized-channel-id normalized-message-id)
      (seq-find
       (lambda (msg)
         (equal (disco-room--normalize-id (alist-get 'id msg))
                normalized-message-id))
       (or (disco-state-messages normalized-channel-id) '())))))

(defun disco-room--normalize-id (value)
  "Return normalized snowflake-like ID string from VALUE, or nil.

String IDs are trimmed; integer IDs are stringified. Empty results are
rejected."
  (let ((normalized
         (cond
          ((stringp value) (string-trim value))
          ((integerp value) (number-to-string value))
          (t nil))))
    (when (and (stringp normalized)
               (not (string-empty-p normalized)))
      normalized)))

(defun disco-room--message-reference-id (msg)
  "Return generic referenced message ID for MSG, or nil."
  (let ((reference (and (listp msg) (alist-get 'message_reference msg))))
    (disco-room--normalize-id
     (and (listp reference) (alist-get 'message_id reference)))))

(defun disco-room--message-reference-channel-id (msg)
  "Return generic referenced channel ID for MSG, or nil."
  (let ((reference (and (listp msg) (alist-get 'message_reference msg))))
    (disco-room--normalize-id
     (and (listp reference) (alist-get 'channel_id reference)))))

(defun disco-room--message-reference-guild-id (msg)
  "Return generic referenced guild ID for MSG, or nil."
  (let ((reference (and (listp msg) (alist-get 'message_reference msg))))
    (disco-room--normalize-id
     (and (listp reference) (alist-get 'guild_id reference)))))

(defun disco-room--message-position (message-id)
  "Return buffer position for MESSAGE-ID in current room render, or nil."
  (when (and (stringp message-id) (not (string-empty-p message-id)))
    (let ((pos (point-min))
          found)
      (while (and (< pos (point-max)) (not found))
        (when (equal (get-text-property pos 'disco-message-id) message-id)
          (setq found pos))
        (setq pos (or (next-single-property-change
                       pos 'disco-message-id nil (point-max))
                      (point-max))))
      found)))

(defun disco-room--jump-to-visible-message (message-id)
  "Jump to visible MESSAGE-ID in current room buffer and recenter.

Return non-nil when jump succeeds without fetching older history."
  (let ((pos (disco-room--message-position message-id)))
    (when (number-or-marker-p pos)
      (goto-char pos)
      (when-let* ((win (get-buffer-window (current-buffer) t)))
        (set-window-point win pos)
        (with-selected-window win
          (goto-char pos)
          (recenter)))
      t)))

(defun disco-room--resolve-pending-jump ()
  "Resolve `disco-room--pending-jump-message-id' in current room buffer."
  (when (and (stringp disco-room--pending-jump-message-id)
             (not (string-empty-p disco-room--pending-jump-message-id)))
    (if (disco-room--jump-to-visible-message disco-room--pending-jump-message-id)
        (let ((target disco-room--pending-jump-message-id))
          (setq disco-room--pending-jump-message-id nil)
          (message "disco: jumped to message %s" target))
      (if disco-room--history-exhausted
          (let ((target disco-room--pending-jump-message-id))
            (setq disco-room--pending-jump-message-id nil)
            (message "disco: message %s not found in channel history" target))
        (unless disco-room--older-in-flight
          (when disco-room--oldest-message-id
            (disco-room-load-older-messages)))))))

(defun disco-room--channel-permissions-known-p (channel)
  "Return non-nil when CHANNEL has a parseable computed permissions field."
  (let ((raw (and (listp channel) (alist-get 'permissions channel))))
    (or (integerp raw)
        (and (stringp raw)
             (string-match-p "\\`[0-9]+\\'" raw)))))

(defun disco-room--jump-required-permissions (channel)
  "Return channel permissions required to jump into CHANNEL.

Guild channels require both visibility and read-history access."
  (when (and (listp channel)
             (alist-get 'guild_id channel)
             (not (disco-state-private-channel-p channel)))
    '(view-channel read-message-history)))

(defun disco-room--resolve-target-channel (channel-id)
  "Return channel object for CHANNEL-ID, fetching when not indexed locally."
  (or (disco-state-channel channel-id)
      (let ((fetched
             (condition-case err
                 (disco-api-channel channel-id)
               (error
                (user-error
                 "disco: cannot fetch jump target channel %s: %s"
                 channel-id
                 (disco-room--async-error-message err))))))
        (when (and fetched (listp fetched))
          (disco-state-upsert-channel fetched))
        fetched)))

(defun disco-room--ensure-jump-permissions (channel-id channel)
  "Signal user error when jump target CHANNEL-ID cannot be viewed/read."
  (unless (and channel (listp channel))
    (user-error "disco: cannot resolve jump target channel %s" channel-id))
  (let ((required (disco-room--jump-required-permissions channel)))
    (when required
      (if (disco-room--channel-permissions-known-p channel)
          (let ((missing (disco-permission-channel-missing channel required nil)))
            (when missing
              (user-error
               "disco: missing permission%s %s for jump target channel %s"
               (if (> (length missing) 1) "s" "")
               (mapconcat #'disco-room--permission-display-name missing ", ")
               channel-id)))
        ;; Fallback probe: if computed permissions are missing, a 1-message fetch
        ;; verifies effective read access before opening the target room.
        (condition-case err
            (disco-api-channel-messages channel-id nil 1)
          (error
           (user-error
            "disco: cannot access jump target channel %s: %s"
            channel-id
            (disco-room--async-error-message err))))))))

(defun disco-room-jump-to-message (message-id &optional channel-id)
  "Jump to MESSAGE-ID, optionally in CHANNEL-ID.

When MESSAGE-ID is not currently loaded in room history, fetch older pages
incrementally until found or history is exhausted."
  (interactive
   (list (read-string "Jump to message ID: "
                      (or (ignore-errors (disco-room--message-id-at-point))
                          ""))
         nil))
  (let* ((target-id (disco-room--normalize-id message-id))
         (target-channel (disco-room--normalize-id (or channel-id disco-room--channel-id)))
         (current-channel (disco-room--normalize-id disco-room--channel-id)))
    (unless (and (stringp target-id) (not (string-empty-p target-id)))
      (user-error "disco: message id is empty"))
    (if (or (null target-channel) (equal target-channel current-channel))
        (progn
          (setq disco-room--pending-jump-message-id target-id)
          (disco-room--resolve-pending-jump))
      (let* ((target-chan-obj (disco-room--resolve-target-channel target-channel))
             (target-name (or (and (listp target-chan-obj)
                                   (alist-get 'name target-chan-obj))
                              target-channel))
             (target-buffer-name (disco-room--buffer-name target-name target-channel)))
        (disco-room--ensure-jump-permissions target-channel target-chan-obj)
        (disco-room-open target-channel target-name)
        (when-let* ((target-buffer (get-buffer target-buffer-name)))
          (with-current-buffer target-buffer
            (setq disco-room--pending-jump-message-id target-id)
            (disco-room--resolve-pending-jump)))))))

(defun disco-room--message-flags (msg)
  "Return normalized integer flags value from message MSG."
  (let ((flags (alist-get 'flags msg)))
    (cond
     ((integerp flags) flags)
     ((and (stringp flags)
           (string-match-p "\\`[0-9]+\\'" flags))
      (string-to-number flags))
     (t 0))))

(defun disco-room--message-has-thread-p (msg)
  "Return non-nil when MSG is known to have a starter thread."
  (let ((message-id (alist-get 'id msg))
        (flags (disco-room--message-flags msg)))
    (or (and (stringp message-id)
             (listp (disco-state-channel message-id))
             (disco-state-channel-thread-p (disco-state-channel message-id)))
        (not (zerop (logand flags disco-room--message-flag-has-thread))))))

(defun disco-room--thread-from-message (msg)
  "Return thread channel object resolved from starter message MSG, or nil."
  (let ((message-id (alist-get 'id msg)))
    (when (stringp message-id)
      (let ((channel (disco-state-channel message-id)))
        (when (and (listp channel)
                   (disco-state-channel-thread-p channel))
          channel)))))

(defun disco-room-open-thread-from-message-at-point ()
  "Open starter thread associated with message at point.

Discord starter threads reuse source message ID as thread channel ID."
  (interactive)
  (let* ((msg (disco-room--message-at-point))
         (message-id (alist-get 'id msg))
         (thread (disco-room--thread-from-message msg))
         (target-thread-id (or (and (listp thread) (alist-get 'id thread))
                               (and (disco-room--message-has-thread-p msg)
                                    (stringp message-id)
                                    message-id)))
         (target-thread-name (or (and (listp thread) (alist-get 'name thread))
                                 (and (stringp message-id)
                                      (format "thread:%s" message-id)))))
    (unless (disco-room--message-has-thread-p msg)
      (user-error "disco: message %s has no starter thread" message-id))
    (unless target-thread-id
      (user-error "disco: cannot resolve starter thread id from message %s" message-id))
    (disco-room-open target-thread-id
                     (or target-thread-name target-thread-id))))

(defun disco-room--message-at-point ()
  "Return message object at point, or signal user error."
  (let* ((message-id (disco-room--message-id-at-point))
         (msg (disco-room--message-by-id message-id)))
    (or msg
        (user-error "disco: message not found in local room cache"))))

(defun disco-room--search-message-line (pattern forward)
  "Search PATTERN in room buffer and return matching message line position.

When FORWARD is non-nil, search forward; otherwise search backward.
Only lines tagged with `disco-message-id' are considered message hits."
  (let ((search-fn (if forward #'re-search-forward #'re-search-backward))
        hit)
    (while (and (not hit)
                (funcall search-fn pattern nil t))
      (let ((line-pos (line-beginning-position)))
        (when (get-text-property line-pos 'disco-message-id)
          (setq hit line-pos))))
    hit))

(defun disco-room--search-move (forward &optional count)
  "Move point to COUNTth next/previous search hit.

When FORWARD is non-nil move forward, otherwise backward.
Search uses `disco-room--last-search-query' and wraps once."
  (let* ((query (or disco-room--last-search-query ""))
         (pattern (regexp-quote query))
         (steps (max 1 (or count 1)))
         (case-fold-search t)
         (start (point))
         (moved nil))
    (unless (string-empty-p query)
      (let ((continue t)
            (i 0))
        (while (and continue (< i steps))
          (let (hit)
            (if forward
                (when (< (point) (point-max))
                  (forward-char 1))
              (when (> (point) (point-min))
                (backward-char 1)))
            (setq hit (disco-room--search-message-line pattern forward))
            (unless hit
              (goto-char (if forward (point-min) (point-max)))
              (setq hit (disco-room--search-message-line pattern forward)))
            (if hit
                (progn
                  (goto-char hit)
                  (setq moved t))
              (goto-char start)
              (setq moved nil)
              (setq continue nil)))
          (setq i (1+ i)))))
    moved))

(defun disco-room-search ()
  "Prompt for message search query and jump to the next hit."
  (interactive)
  (let ((query (read-from-minibuffer
                "Search messages: "
                (or disco-room--last-search-query ""))))
    (if (string-empty-p query)
        (message "disco: search query is empty")
      (setq disco-room--last-search-query query)
      (if (disco-room--search-move t 1)
          (message "disco: search -> %s" query)
        (message "disco: no message matches '%s'" query)))))

(defun disco-room-search-next (&optional n)
  "Jump to Nth next message search result."
  (interactive "p")
  (if (string-empty-p (or disco-room--last-search-query ""))
      (call-interactively #'disco-room-search)
    (if (disco-room--search-move t n)
        (message "disco: next match -> %s" disco-room--last-search-query)
      (message "disco: no message matches '%s'" disco-room--last-search-query))))

(defun disco-room-search-prev (&optional n)
  "Jump to Nth previous message search result."
  (interactive "p")
  (if (string-empty-p (or disco-room--last-search-query ""))
      (call-interactively #'disco-room-search)
    (if (disco-room--search-move nil n)
        (message "disco: previous match -> %s" disco-room--last-search-query)
      (message "disco: no message matches '%s'" disco-room--last-search-query))))

(defun disco-room--read-thread-auto-archive-duration ()
  "Prompt for optional auto archive duration in minutes.

Returns nil when left blank."
  (let* ((choices '("" "60" "1440" "4320" "10080"))
         (raw (completing-read
               "Auto archive minutes (empty for default): "
               choices nil t nil nil "")))
    (unless (string-empty-p raw)
      (string-to-number raw))))

(defun disco-room--read-required-thread-auto-archive-duration (&optional default)
  "Prompt for required auto archive duration in minutes.

DEFAULT, when non-nil, is preselected in completion candidates."
  (let* ((choices '("60" "1440" "4320" "10080"))
         (initial (and default (format "%s" default)))
         (raw (completing-read
               "Auto archive minutes: "
               choices nil t nil nil initial)))
    (string-to-number raw)))

(defun disco-room--read-tristate-bool (prompt current-value)
  "Read tri-state boolean with PROMPT and CURRENT-VALUE.

Return symbol `keep', t, or :false."
  (let* ((choice (completing-read
                  (format "%s (keep/yes/no, current %s): "
                          prompt
                          (if current-value "yes" "no"))
                  '("keep" "yes" "no") nil t nil nil "keep")))
    (pcase choice
      ("yes" t)
      ("no" :false)
      (_ 'keep))))

(defun disco-room--read-optional-nonnegative-int (prompt)
  "Read optional non-negative integer using PROMPT.

Returns nil when left blank."
  (let ((raw (read-string prompt)))
    (unless (string-empty-p raw)
      (let ((n (string-to-number raw)))
        (when (< n 0)
          (user-error "disco: value must be >= 0"))
        n))))

(defun disco-room--read-detached-thread-type ()
  "Prompt for detached thread type; return numeric channel type or nil."
  (let ((choice (completing-read
                 "Thread type (empty/public/private): "
                 '("" "public" "private") nil t nil nil "")))
    (pcase choice
      ("public" 11)
      ("private" 12)
      (_ nil))))

(defun disco-room--forum-or-media-channel-p (&optional channel)
  "Return non-nil when CHANNEL (or current room channel) is forum/media."
  (let* ((target (or channel (disco-room--channel-object)))
         (type (and target (alist-get 'type target))))
    (memq type '(15 16))))

(defun disco-room--thread-with-meta-field (channel key value)
  "Return CHANNEL with thread metadata KEY set to VALUE."
  (let* ((updated (copy-tree channel))
         (meta (copy-tree (or (alist-get 'thread_metadata updated) '()))))
    (setf (alist-get key meta nil 'remove) value)
    (setf (alist-get 'thread_metadata updated nil 'remove) meta)
    updated))

(defun disco-room--thread-with-field (channel key value)
  "Return CHANNEL with top-level KEY set to VALUE."
  (let ((updated (copy-tree channel)))
    (setf (alist-get key updated nil 'remove) value)
    updated))

(defun disco-room--resolve-thread-update (updated fallback)
  "Resolve UPDATED thread channel response with FALLBACK object.

When UPDATED does not contain a full channel object, FALLBACK is used."
  (let ((next (if (and (listp updated) (alist-get 'id updated))
                  updated
                fallback)))
    (when next
      (disco-state-upsert-channel next)
      (when (alist-get 'name next)
        (setq disco-room--channel-name (alist-get 'name next))))
    next))

(defun disco-room--buffer-name (channel-name channel-id)
  "Build room buffer name for CHANNEL-NAME and CHANNEL-ID."
  (format "*disco:%s (%s)*" channel-name channel-id))

(defun disco-room--insert-right-aligned-text (text &optional face)
  "Insert TEXT aligned to right edge on current line.

When FACE is non-nil, apply FACE to TEXT."
  (let* ((raw (or text ""))
         (width (max 1 (1+ (string-width raw))))
         (start (point)))
    (if disco-room-right-align-timestamps
        (insert (propertize " " 'display `(space :align-to (- right ,width))))
      (insert " "))
    (insert (if face
                (propertize raw 'face face)
              raw))
    (cons start (point))))

(defun disco-room--insert-prefixed-lines (prefix text &optional face)
  "Insert TEXT as newline-separated lines, each prefixed by PREFIX.

When FACE is non-nil, apply FACE to each inserted line."
  (disco-ui-insert-prefixed-lines prefix text :face face))

(defun disco-room--line-prefix-string (prefix &optional consume default)
  "Return normalized PREFIX as a string.

When PREFIX is a mutable prefix-state, consume first-prefix when CONSUME is
non-nil. DEFAULT falls back to four spaces."
  (disco-ui-prefix-string prefix consume (or default "    ")))

(defun disco-room--message-time (msg)
  "Return decoded time for MSG timestamp, or nil when unavailable."
  (let ((raw (alist-get 'timestamp msg)))
    (when (and (stringp raw) (not (string-empty-p raw)))
      (condition-case _
          (date-to-time raw)
        (error nil)))))

(defun disco-room--message-time-epoch (msg)
  "Return float epoch seconds for MSG timestamp, or nil."
  (let ((time (disco-room--message-time msg)))
    (and time (float-time time))))

(defun disco-room--message-day-key (msg)
  "Return local calendar day key string for MSG timestamp, or nil."
  (let ((time (disco-room--message-time msg)))
    (and time (format-time-string "%Y-%m-%d" time))))

(defun disco-room--message-day-label (day-key)
  "Return pretty date label for DAY-KEY (YYYY-MM-DD)."
  (if (not (stringp day-key))
      "Unknown date"
    (condition-case _
        (format-time-string "%A, %Y-%m-%d" (date-to-time (concat day-key "T00:00:00")))
      (error day-key))))

(defun disco-room--same-sender-p (left right)
  "Return non-nil when LEFT and RIGHT messages share sender identity."
  (let ((left-id (disco-room--message-author-id left))
        (right-id (disco-room--message-author-id right)))
    (if (and left-id right-id)
        (equal left-id right-id)
      (equal (disco-room--message-author left)
             (disco-room--message-author right)))))

(defun disco-room--messages-compact-group-p (previous current)
  "Return non-nil when CURRENT should be compact-grouped under PREVIOUS."
  (and disco-room-group-messages
       (listp previous)
       (listp current)
       (disco-room--same-sender-p previous current)
       (let ((previous-time (disco-room--message-time-epoch previous))
             (current-time (disco-room--message-time-epoch current)))
         (and previous-time
              current-time
              (<= (abs (- current-time previous-time))
                  (max 0 disco-room-group-messages-timespan))))))

(defun disco-room--set-message-render-context (message-id context)
  "Store render CONTEXT for MESSAGE-ID in current room buffer."
  (when (and message-id disco-room--render-context-by-message-id)
    (puthash message-id context disco-room--render-context-by-message-id)))

(defun disco-room--message-render-context (msg)
  "Return render context plist for MSG, or nil when missing."
  (let ((message-id (and (listp msg) (alist-get 'id msg))))
    (and message-id
         disco-room--render-context-by-message-id
         (gethash message-id disco-room--render-context-by-message-id))))

(defun disco-room--insert-divider-row (text face)
  "Insert read-only divider row TEXT with FACE."
  (disco-ui-insert-styled-line
   text
   :face face
   :properties '(read-only t
                 front-sticky (read-only)
                 rear-nonsticky (read-only))))

(defun disco-room--insert-date-separator-row (day-key)
  "Insert date separator row for DAY-KEY."
  (disco-room--insert-divider-row
   (format "────────  %s  ────────" (disco-room--message-day-label day-key))
   'disco-room-date-separator))

(defun disco-room--insert-unread-divider-row ()
  "Insert unread separator row."
  (disco-room--insert-divider-row
   "────────  Unread Messages  ────────"
   'disco-room-unread-divider))

(defun disco-room--message-effective-author (msg)
  "Return effective author object for MSG.

Type-21 thread starter rows should inherit author identity from the referenced
source message, not the synthetic starter row itself."
  (let ((author (and (listp msg) (alist-get 'author msg))))
    (if (= (disco-room--message-type msg) 21)
        (let* ((thread-source (disco-room--thread-starter-reference-message msg))
               (source-author (and (listp thread-source)
                                   (alist-get 'author thread-source))))
          (if (listp source-author)
              source-author
            author))
      author)))

(defun disco-room--message-author (msg)
  "Extract author name from message MSG alist."
  (let* ((author (disco-room--message-effective-author msg))
         (global-name (and (listp author) (alist-get 'global_name author)))
         (username (and (listp author) (alist-get 'username author))))
    (or global-name username "unknown")))

(defun disco-room--message-author-id (msg)
  "Extract author ID string from message MSG alist."
  (let ((author (disco-room--message-effective-author msg)))
    (and (listp author) (alist-get 'id author))))

(defun disco-room--author-face (msg)
  "Return deterministic face symbol for MSG author."
  (let* ((faces disco-room--author-faces)
         (count (length faces))
         (key (or (disco-room--message-author-id msg)
                  (disco-room--message-author msg)
                  "unknown"))
         (idx (if (> count 0)
                  (mod (abs (sxhash key)) count)
                0)))
    (aref faces idx)))

(defun disco-room--avatar-placeholder (msg)
  "Return text avatar placeholder for MSG author (for example `[AB]')."
  (let* ((name (disco-room--message-author msg))
         (parts (split-string (or name "") "[^[:alnum:]]+" t))
         (first (if parts (substring (car parts) 0 1) "?"))
         (second (if (> (length parts) 1)
                     (substring (cadr parts) 0 1)
                   ""))
         (initials (upcase (concat first second))))
    (format "[%s]" initials)))

(defun disco-room--guild-by-id (guild-id)
  "Return guild object for GUILD-ID, or nil."
  (when (and (stringp guild-id) (not (string-empty-p guild-id)))
    (seq-find (lambda (guild)
                (equal (alist-get 'id guild) guild-id))
              (or (disco-state-guilds) '()))))

(defun disco-room--forward-guild-icon-hash (guild)
  "Return icon hash string from GUILD, or nil when unavailable."
  (let ((icon (and (listp guild) (alist-get 'icon guild))))
    (and (stringp icon)
         (not (string-empty-p icon))
         icon)))

(defun disco-room--forward-guild-icon-url (guild)
  "Return Discord CDN guild icon URL for GUILD, or nil."
  (let ((guild-id (and (listp guild) (alist-get 'id guild)))
        (icon-hash (disco-room--forward-guild-icon-hash guild)))
    (when (and guild-id icon-hash)
      (format "https://cdn.discordapp.com/icons/%s/%s.png?size=64"
              guild-id icon-hash))))

(defun disco-room--forward-guild-icon-cache-key (guild)
  "Build stable cache key for forwarded-source guild icon image."
  (let ((guild-id (and (listp guild) (alist-get 'id guild)))
        (icon-hash (disco-room--forward-guild-icon-hash guild)))
    (when (and guild-id icon-hash)
      (format "%s:%s:%s"
              guild-id icon-hash disco-room-forward-guild-icon-size))))

(defun disco-room--forward-guild-icon-fallback (guild)
  "Return fallback textual icon for GUILD when image is unavailable."
  (let* ((name (or (and (listp guild) (alist-get 'name guild)) "?"))
         (initial (if (and (stringp name) (> (length name) 0))
                      (upcase (substring name 0 1))
                    "?")))
    (format "[%s]" initial)))

(defun disco-room--forward-guild-icon-image-valid-p (image)
  "Return non-nil when IMAGE object appears renderable."
  (disco-media-image-object-valid-p image))

(defun disco-room--forward-guild-icon-rendering-available-p ()
  "Return non-nil when forwarded-source guild icons can be rendered."
  (and disco-room-show-forward-guild-icons
       (disco-media-inline-image-rendering-available-p)
       (fboundp 'plz)))

(defun disco-room--start-forward-guild-icon-fetch (cache-key url)
  "Start asynchronous guild icon fetch for CACHE-KEY from URL."
  (unless (or (gethash cache-key disco-room--forward-guild-icon-fetching)
              (gethash cache-key disco-room--forward-guild-icon-image-cache))
    (puthash cache-key t disco-room--forward-guild-icon-fetching)
    (plz 'get url
      :as 'binary
      :headers '(("Accept" . "image/png,image/*;q=0.8,*/*;q=0.1"))
      :then
      (lambda (bytes)
        (let ((image
               (ignore-errors
                 (create-image bytes 'png t
                               :width disco-room-forward-guild-icon-size
                               :height disco-room-forward-guild-icon-size
                               :ascent 'center))))
          (puthash cache-key
                   (if (disco-room--forward-guild-icon-image-valid-p image)
                       image
                     :missing)
                   disco-room--forward-guild-icon-image-cache)
          (remhash cache-key disco-room--forward-guild-icon-fetching)
          (disco-room--rerender-open-rooms)))
      :else
      (lambda (_err)
        (puthash cache-key :missing disco-room--forward-guild-icon-image-cache)
        (remhash cache-key disco-room--forward-guild-icon-fetching)))))

(defun disco-room--forward-guild-icon-image (guild)
  "Return image object for forwarded-source GUILD icon when available."
  (when (disco-room--forward-guild-icon-rendering-available-p)
    (let* ((cache-key (disco-room--forward-guild-icon-cache-key guild))
           (cached (and cache-key
                        (gethash cache-key disco-room--forward-guild-icon-image-cache))))
      (cond
       ((null cache-key)
        nil)
       ((eq cached :missing)
        nil)
       ((disco-room--forward-guild-icon-image-valid-p cached)
        cached)
       (t
        (let ((url (disco-room--forward-guild-icon-url guild)))
          (when (and (stringp url) (not (string-empty-p url)))
            (disco-room--start-forward-guild-icon-fetch cache-key url)))
        nil)))))

(defun disco-room--insert-forward-guild-icon (guild)
  "Insert one forwarded-source guild icon for GUILD."
  (let ((fallback (disco-room--forward-guild-icon-fallback guild))
        (image (disco-room--forward-guild-icon-image guild)))
    (if (disco-room--forward-guild-icon-image-valid-p image)
        (insert-image image fallback)
      (insert fallback))))

(defun disco-room--image-rendering-available-p ()
  "Return non-nil when avatar images can be shown in current frame."
  (and disco-room-show-avatar-images
       (disco-media-inline-image-rendering-available-p)))

(defun disco-room--author-avatar-hash (msg)
  "Extract Discord avatar hash string from MSG author, or nil."
  (let ((author (disco-room--message-effective-author msg)))
    (and (listp author) (alist-get 'avatar author))))

(defun disco-room--author-default-avatar-index (msg)
  "Return Discord default avatar index for MSG author."
  (let* ((author (disco-room--message-effective-author msg))
         (user-id (and (listp author) (alist-get 'id author)))
         (discriminator (and (listp author) (alist-get 'discriminator author))))
    (cond
     ((and discriminator
           (stringp discriminator)
           (not (string= discriminator "0")))
      (mod (string-to-number discriminator) 5))
     ((and user-id (string-match-p "\\`[0-9]+\\'" user-id))
      ;; Modern Discord default avatar buckets derive from user snowflake.
      (mod (ash (string-to-number user-id) -22) 6))
     (t 0))))

(defun disco-room--avatar-url (msg)
  "Return avatar CDN URL for MSG author."
  (let* ((user-id (disco-room--message-author-id msg))
         (avatar-hash (disco-room--author-avatar-hash msg)))
    (cond
     ((and user-id avatar-hash)
      (format "https://cdn.discordapp.com/avatars/%s/%s.png?size=64"
              user-id avatar-hash))
     (user-id
      (format "https://cdn.discordapp.com/embed/avatars/%d.png"
              (disco-room--author-default-avatar-index msg)))
     (t nil))))

(defun disco-room--avatar-cache-key (msg)
  "Build stable avatar cache key for MSG author and avatar size."
  (let* ((user-id (disco-room--message-author-id msg))
         (avatar-hash (disco-room--author-avatar-hash msg))
         (default-index (disco-room--author-default-avatar-index msg)))
    (when user-id
      (format "%s:%s:%s"
              user-id
              (or avatar-hash (format "default-%d" default-index))
              disco-room-avatar-image-size))))

(defun disco-room--avatar-cache-file-base (cache-key)
  "Return avatar cache file base path for CACHE-KEY (without extension)."
  (expand-file-name (md5 cache-key) disco-room-avatar-cache-directory))

(defun disco-room--avatar-cache-file (cache-key extension)
  "Return avatar cache file path for CACHE-KEY and EXTENSION."
  (format "%s.%s"
          (disco-room--avatar-cache-file-base cache-key)
          extension))

(defun disco-room--avatar-cache-existing-file (cache-key)
  "Return an existing cached avatar path for CACHE-KEY, or nil."
  (seq-find #'file-exists-p
            (mapcar (lambda (ext)
                      (disco-room--avatar-cache-file cache-key ext))
                    disco-room--avatar-cache-extensions)))

(defun disco-room--avatar-ensure-queue ()
  "Return active queue for avatar fetches.

Queue is recreated when `disco-room-avatar-fetch-concurrency' changes."
  (let ((limit (max 1 disco-room-avatar-fetch-concurrency)))
    (when (or (null disco-room--avatar-plz-queue)
              (not (equal disco-room--avatar-plz-queue-limit limit)))
      (setq disco-room--avatar-plz-queue (make-plz-queue :limit limit))
      (setq disco-room--avatar-plz-queue-limit limit))
    disco-room--avatar-plz-queue))

(defun disco-room--avatar-delete-stale-cache-files (cache-base)
  "Delete stale cached avatar files for CACHE-BASE."
  (dolist (ext disco-room--avatar-cache-extensions)
    (let ((old-file (format "%s.%s" cache-base ext)))
      (when (file-exists-p old-file)
        (ignore-errors (delete-file old-file))))))

(defun disco-room--avatar-complete-fetch (cache-key image &optional target-file)
  "Finalize one avatar fetch for CACHE-KEY with IMAGE.

When IMAGE is nil and TARGET-FILE exists, delete TARGET-FILE."
  (when (and (null image)
             target-file
             (file-exists-p target-file))
    ;; Drop corrupted cache files so later refresh can retry cleanly.
    (ignore-errors (delete-file target-file)))
  (puthash cache-key (or image :missing) disco-room--avatar-image-cache)
  (remhash cache-key disco-room--avatar-fetching)
  (disco-room--rerender-open-rooms))

(defun disco-room--avatar-image-from-file (file)
  "Create inline avatar image from FILE, or nil when unsupported."
  (let ((image
         (ignore-errors
           (create-image file nil nil
                         :width disco-room-avatar-image-size
                         :height disco-room-avatar-image-size
                         :ascent 'center))))
    (unless (disco-media-image-object-valid-p image)
      (when (image-type-available-p 'imagemagick)
        (setq image
              (ignore-errors
                (create-image file 'imagemagick nil
                              :width disco-room-avatar-image-size
                              :height disco-room-avatar-image-size
                              :ascent 'center)))))
    (when (disco-media-image-object-valid-p image)
      image)))

(defun disco-room-clear-avatar-cache ()
  "Clear in-memory avatar cache and rerender all room buffers."
  (interactive)
  (clrhash disco-room--avatar-image-cache)
  (clrhash disco-room--avatar-fetching)
  (clrhash disco-room--avatar-round-image-cache)
  (disco-room--rerender-open-rooms)
  (message "disco: avatar cache cleared"))

(defun disco-room-refetch-avatars ()
  "Drop avatar caches (memory + disk) and refetch in open room buffers."
  (interactive)
  (clrhash disco-room--avatar-image-cache)
  (clrhash disco-room--avatar-fetching)
  (clrhash disco-room--avatar-round-image-cache)
  (when (file-directory-p disco-room-avatar-cache-directory)
    (delete-directory disco-room-avatar-cache-directory t))
  (disco-room--rerender-open-rooms)
  (message "disco: avatar cache reset; refetching"))

(defun disco-room--rerender-open-rooms ()
  "Rerender all open room buffers while preserving reading position."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'disco-room-mode)
          (disco-view-render-preserving-position
           #'disco-room-render
           :anchor-property 'disco-message-id
           :preserve-window-start t))))))

(defun disco-room--on-text-scale-change ()
  "Rerender room buffers after `text-scale-mode' changes."
  (when (eq major-mode 'disco-room-mode)
    ;; Recreate image objects from cache files so resized previews track text scale.
    (disco-media-clear-preview-memory-cache)
    (disco-room--rerender-open-rooms)))

(defun disco-room--buffer-substring-filter (beg end delete)
  "Copy region BEG..END while stripping display-only prefix properties."
  (let ((text (buffer-substring beg end)))
    (when delete
      (save-excursion
        (goto-char beg)
        (delete-region beg end)))
    (remove-text-properties 0 (length text)
                            '(line-prefix nil wrap-prefix nil)
                            text)
    text))

(defun disco-room--apply-breakline-settings ()
  "Apply telega-style line wrapping behavior to current room buffer."
  (let* ((visual-fill-feature-loaded
          (or (featurep 'visual-fill-column)
              (and disco-room-use-visual-fill-column
                   (require 'visual-fill-column nil t))))
         (visual-fill-mode-fn
          (and visual-fill-feature-loaded
               (fboundp 'visual-fill-column-mode)
               (symbol-function 'visual-fill-column-mode))))
    (if disco-room-wrap-long-lines
        (progn
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (visual-line-mode 1)
          (if (and disco-room-use-visual-fill-column visual-fill-mode-fn)
              (progn
                (when disco-room-fill-column
                  (setq-local fill-column disco-room-fill-column))
                (funcall visual-fill-mode-fn 1))
            (when visual-fill-mode-fn
              (funcall visual-fill-mode-fn -1))))
      (visual-line-mode -1)
      (setq-local truncate-lines t)
      (setq-local word-wrap nil)
      (when visual-fill-mode-fn
        (funcall visual-fill-mode-fn -1)))))

(setq disco-media-preview-rerender-function #'disco-room--rerender-open-rooms)

(defun disco-room--start-avatar-fetch (cache-key url cache-base)
  "Start asynchronous avatar fetch for CACHE-KEY from URL using CACHE-BASE."
  (unless (or (gethash cache-key disco-room--avatar-fetching)
              (gethash cache-key disco-room--avatar-image-cache)
              (and (numberp disco-room--avatar-fetch-budget)
                   (<= disco-room--avatar-fetch-budget 0)))
    (when (numberp disco-room--avatar-fetch-budget)
      (cl-decf disco-room--avatar-fetch-budget))
    (puthash cache-key t disco-room--avatar-fetching)
    (let ((queue (disco-room--avatar-ensure-queue))
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
                  (disco-room--avatar-delete-stale-cache-files cache-base)
                  (make-directory (file-name-directory target-file) t)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert raw-bytes)
                    (let ((coding-system-for-write 'binary))
                      (write-region (point-min) (point-max) target-file nil 'silent)))
                  (setq image (disco-room--avatar-image-from-file target-file))
                  (disco-room--avatar-complete-fetch cache-key image target-file)))
              :else
              (lambda (_err)
                (disco-room--avatar-complete-fetch cache-key nil)))
            (plz-run queue))
        (error
         (disco-room--avatar-complete-fetch cache-key nil)
         (message "disco: avatar fetch enqueue failed for %s: %s"
                  cache-key
                  (disco-room--async-error-message err)))))))

(defun disco-room--avatar-image (msg)
  "Return avatar image object for MSG when ready, otherwise nil.

If needed, schedule async fetch and fall back to text placeholder."
  (when (disco-room--image-rendering-available-p)
    (let* ((cache-key (disco-room--avatar-cache-key msg))
           (cached (and cache-key (gethash cache-key disco-room--avatar-image-cache))))
      (cond
       ((or (null cache-key) (eq cached :missing))
        nil)
       (cached
        (if (disco-media-image-object-valid-p cached)
            cached
          (remhash cache-key disco-room--avatar-image-cache)
          nil))
       (t
        (let* ((url (disco-room--avatar-url msg))
               (cache-base (and cache-key (disco-room--avatar-cache-file-base cache-key)))
               (cache-file (and cache-key (disco-room--avatar-cache-existing-file cache-key)))
               (file-image (and cache-file (disco-room--avatar-image-from-file cache-file))))
          (cond
           (file-image
            (puthash cache-key file-image disco-room--avatar-image-cache)
            file-image)
           ((and url cache-base)
            (when (and cache-file (not file-image))
              (ignore-errors (delete-file cache-file)))
            (disco-room--start-avatar-fetch cache-key url cache-base)
            nil)
           (t nil))))))))

(defun disco-room--avatar-line-pixel-height ()
  "Return line height in pixels for current room text scale."
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

(defun disco-room--avatar-display-size ()
  "Return full avatar size in pixels for two-line avatar rendering.

`disco-room-avatar-image-size' is interpreted as baseline size ratio where
`28' maps to exactly two text lines at current scale."
  (let* ((line-height (disco-room--avatar-line-pixel-height))
         (base-target (* 2 line-height))
         (size-factor (/ (float (max 1 disco-room-avatar-image-size)) 28.0)))
    (max 8 (round (* base-target size-factor)))))

(defun disco-room--avatar-cw-width (nchars)
  "Return image `:width' spec that occupies NCHARS columns."
  (let ((cols (max 1 nchars)))
    (if (string-version-lessp emacs-version "30.1")
        (* cols (max 1 (frame-char-width)))
      (cons cols 'cw))))

(defun disco-room--avatar-factors (&optional cheight)
  "Return avatar (circle . margin) factors for CHEIGHT lines."
  (let* ((entry (alist-get (or cheight 2) disco-room-avatar-factors-alist))
         (circle (and (consp entry) (car entry)))
         (margin (and (consp entry) (cdr entry))))
    (cons (if (numberp circle) circle 0.8)
          (if (numberp margin) margin 0.1))))

(defun disco-room--avatar-image-resized (image pixel-size)
  "Return IMAGE resized to PIXEL-SIZE, or nil when IMAGE is invalid."
  (when (disco-media-image-object-valid-p image)
    (let* ((type (car image))
           (props (copy-sequence (cdr image))))
      (setq props (plist-put props :width pixel-size))
      (setq props (plist-put props :height pixel-size))
      (setq props (plist-put props :ascent 'center))
      (cons type props))))

(defun disco-room--avatar-text-fit-width (text width)
  "Return TEXT truncated/padded to WIDTH columns."
  (let* ((target (max 1 width))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (trim-width (string-width trimmed)))
    (if (< trim-width target)
        (concat trimmed (make-string (- target trim-width) ?\s))
      trimmed)))

(defun disco-room--avatar-image-with-text (image fallback pixel-size &optional resized)
  "Return IMAGE at PIXEL-SIZE with stable two-line fallback text.

When RESIZED is non-nil, IMAGE is treated as already resized."
  (when (disco-media-image-object-valid-p image)
    (let* ((base-image (if resized
                           image
                         (disco-room--avatar-image-resized image pixel-size)))
           (size-px (and (disco-media-image-object-valid-p base-image)
                         (ignore-errors (image-size base-image t (selected-frame)))))
           (width-px (or (and (consp size-px) (car size-px)) pixel-size))
           (char-width (max 1 (frame-char-width)))
           (logical-width (and (consp base-image)
                               (plist-get (cdr base-image) :disco-char-width)))
           (width-chars (if (and (integerp logical-width) (> logical-width 0))
                            logical-width
                          (max 1
                               (ceiling (/ (float (max 1 width-px))
                                           (float char-width)))))))
      (when (disco-media-image-object-valid-p base-image)
        (let* ((type (car base-image))
               (props (copy-sequence (cdr base-image)))
               (top-text (disco-room--avatar-text-fit-width fallback width-chars))
               (bottom-text (make-string width-chars ?\s)))
          (setq props (plist-put props :disco-char-width width-chars))
          (setq props (plist-put props :width (disco-room--avatar-cw-width width-chars)))
          (setq props (plist-put props :disco-text (list top-text bottom-text)))
          (setq props (plist-put props :disco-nslices 2))
          (cons type props))))))

(defun disco-room--avatar-image-mime-type (file)
  "Return MIME type string for avatar FILE extension, or nil."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (pcase ext
      ("png" "image/png")
      ((or "jpg" "jpeg") "image/jpeg")
      ("gif" "image/gif")
      ("webp" "image/webp")
      (_ nil))))

(defun disco-room--avatar-svg-image (svg &rest props)
  "Return image object for SVG with properties in PROPS.

This mirrors telega's workaround: prepend XML header so some librsvg versions
render text correctly."
  (let ((svg-data (with-temp-buffer
                    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                    (svg-print svg)
                    (buffer-string))))
    (apply #'create-image svg-data 'svg t props)))

(defun disco-room--avatar--create-svg (file fallback cheight)
  "Create telega-style circular avatar SVG image.

FILE is cached avatar image path, FALLBACK is placeholder text.
CHEIGHT is avatar height in lines (chars), typically 2."
  (when (and (stringp file)
             (file-readable-p file)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-circle)
             (fboundp 'svg-embed)
             (fboundp 'svg-print)
             (integerp cheight)
             (> cheight 0))
    (let* ((attrs (file-attributes file))
           (mtime (and attrs (file-attribute-modification-time attrs)))
           (line-height (disco-room--avatar-line-pixel-height))
           (size-factor (/ (float (max 1 disco-room-avatar-image-size)) 28.0))
           (round-scale (max 0.1 disco-room-avatar-round-size-factor))
           (factors (disco-room--avatar-factors cheight))
           (cfactor (or (car factors) 0.8))
           (mfactor (or (cdr factors) 0.1))
           (xh (* cheight line-height size-factor round-scale))
           (margin (* mfactor xh))
           (inset-ratio (max 0.0 (min 0.45 disco-room-avatar-round-inset-ratio)))
           (ch-raw (* cfactor xh))
           (ch (max 1.0 (* ch-raw (- 1.0 (* 2 inset-ratio)))))
           (cfull (floor (+ ch margin)))
           (char-width (max 1 (frame-char-width)))
           (aw-chars (max 1 (ceiling (/ ch (float char-width)))))
           (svg-xw (* aw-chars char-width))
           ;; Telega uses +1 line for cheight==2 as a gaps workaround.
           (svg-xh (cond ((= cheight 1) cfull)
                         ((and (= cheight 2) disco-room-avatar-extra-bottom-line)
                          (+ cfull line-height))
                         (t xh)))
           (cache-key (format "%s:%s:%s:%s:%s:%s:%s:%s:%s:%s"
                              file mtime cheight aw-chars line-height
                              disco-room-avatar-image-size
                              disco-room-avatar-round-size-factor
                              disco-room-avatar-round-inset-ratio
                              disco-room-avatar-extra-bottom-line
                              fallback))
           (cached (gethash cache-key disco-room--avatar-round-image-cache))
           (mime (disco-room--avatar-image-mime-type file))
           (svg (and mime (svg-create svg-xw svg-xh)))
           (clip (and svg (svg-clip-path svg :id "clip")))
           (cx (/ svg-xw 2.0))
           (cy (/ cfull 2.0))
           (radius (/ ch 2.0))
           (x (/ (- svg-xw ch) 2.0))
           (y (/ margin 2.0)))
      (or cached
          (when (and svg clip)
            (svg-circle clip cx cy radius)
            (svg-embed svg file mime nil
                       :x x :y y :width ch :height ch
                       :clip-path "url(#clip)")
            (let* ((image (disco-room--avatar-svg-image
                           svg
                           :scale 1.0
                           :width (disco-room--avatar-cw-width aw-chars)
                           :ascent 'center
                           :mask 'heuristic))
                   (ttop (disco-room--avatar-text-fit-width fallback aw-chars))
                   (tpad (make-string aw-chars ?\u00A0))
                   (text (cons ttop (make-list (1- cheight) tpad))))
              (when image
                (let* ((type (car image))
                       (props (copy-sequence (cdr image))))
                  (setq props (plist-put props :disco-char-width aw-chars))
                  (setq props (plist-put props :disco-slice-height line-height))
                  (setq props (plist-put props :disco-text text))
                  (setq props (plist-put props :disco-nslices cheight))
                  (setq image (cons type props))
                  (puthash cache-key image disco-room--avatar-round-image-cache)
                  image))))))))

(defun disco-room--avatar-image-text (image &optional slice-index)
  "Return textual fallback for IMAGE and optional SLICE-INDEX."
  (let ((text (and (consp image)
                   (plist-get (cdr image) :disco-text))))
    (cond
     ((stringp text) text)
     ((and (listp text)
           (numberp slice-index)
           (>= slice-index 0)
           (< slice-index (length text)))
      (nth slice-index text))
     ((listp text)
      (mapconcat #'identity text "\n"))
     (t nil))))

(defun disco-room--avatar-image-char-width (image)
  "Return rendered width in columns for IMAGE, defaulting to 1."
  (or (and (consp image)
           (let ((w (plist-get (cdr image) :disco-char-width)))
             (and (integerp w) (> w 0) w)))
      (and (stringp (disco-room--avatar-image-text image 1))
           (max 1 (string-width (disco-room--avatar-image-text image 1))))
      (let* ((size-px (and (disco-media-image-object-valid-p image)
                           (ignore-errors (image-size image t (selected-frame)))))
             (width-px (and (consp size-px) (car size-px)))
             (char-width (max 1 (frame-char-width))))
        (max 1
             (if (numberp width-px)
                 (ceiling (/ (float width-px) (float char-width)))
               (max 1 disco-room-avatar-rounding-default-char-width))))))

(defun disco-room--avatar-image-slice-display (image slice-index &optional resized)
  "Return display spec for IMAGE slice at SLICE-INDEX (0 or 1).

When RESIZED is non-nil, IMAGE is treated as already resized."
  (when (disco-media-image-object-valid-p image)
    (let* ((display-size (disco-room--avatar-display-size))
           (scaled (if resized
                       image
                     (disco-room--avatar-image-resized image display-size))))
      (when (disco-media-image-object-valid-p scaled)
        (let* ((size-px (ignore-errors (image-size scaled t (selected-frame))))
               (height-px (if (and (consp size-px) (numberp (cdr size-px)))
                              (cdr size-px)
                            display-size))
               (custom-slice-height (and (consp scaled)
                                         (plist-get (cdr scaled) :disco-slice-height)))
               (slice-height (if (and (integerp custom-slice-height)
                                      (> custom-slice-height 0))
                                 custom-slice-height
                               (max 1 (/ height-px 2))))
               (slice-y (if (= slice-index 0) 0 (* slice-height slice-index)))
               (slice-max (max 1 (- height-px slice-y)))
               (slice-height* (max 1 (min slice-height slice-max)))
               (slice (list 'slice 0 slice-y 1.0 slice-height*)))
          (list slice scaled))))))

(defun disco-room--avatar-image-slice-string (image slice-index)
  "Return propertized avatar slice string for IMAGE at SLICE-INDEX."
  (let* ((text (or (disco-room--avatar-image-text image slice-index)
                   (make-string (disco-room--avatar-image-char-width image) ?\s)))
         (display (disco-room--avatar-image-slice-display image slice-index t)))
    (if display
        (propertize text 'display display 'rear-nonsticky '(display))
      text)))

(defun disco-room--pad-prefix-to-width (prefix width)
  "Right-pad PREFIX with spaces so it occupies WIDTH columns."
  (let* ((text (or prefix ""))
         (target (max 0 width))
         (current (max 0 (string-width text))))
    (if (< current target)
        (concat text (make-string (- target current) ?\s))
      text)))

(defun disco-room--avatar-prefixes (msg)
  "Return avatar-aware prefixes plist for MSG header/body lines."
  (let* ((image (disco-room--avatar-image msg))
         (fallback (disco-room--avatar-placeholder msg))
         (fallback-indent (max 1 (1+ (string-width fallback)))))
    (if (disco-media-image-object-valid-p image)
        (let* ((base-size (disco-room--avatar-display-size))
               (pixel-size (if disco-room-avatar-round-images
                               (max 8 (round (* base-size
                                                (max 0.1 disco-room-avatar-round-size-factor))))
                             base-size))
               (cache-key (disco-room--avatar-cache-key msg))
               (cache-file (and cache-key
                                (disco-room--avatar-cache-existing-file cache-key)))
               (svg-avatar (and disco-room-avatar-round-images
                                cache-file
                                (disco-room--avatar--create-svg
                                 cache-file
                                 fallback
                                 2)))
               (base-image (or svg-avatar
                               (disco-room--avatar-image-resized image pixel-size)))
               (scaled (disco-room--avatar-image-with-text
                        base-image
                        fallback
                        pixel-size
                        t))
               (target-width-chars (max 1 (disco-room--avatar-image-char-width scaled)))
               (image-indent (1+ target-width-chars))
               (top (if (disco-media-image-object-valid-p scaled)
                        (disco-room--avatar-image-slice-string scaled 0)
                      fallback))
               (bottom (if (disco-media-image-object-valid-p scaled)
                           (disco-room--avatar-image-slice-string scaled 1)
                         (make-string (max 1 target-width-chars) ?\s)))
               (header-prefix (concat top " "))
               (first-body-prefix (concat bottom " "))
               (normalized-width (max image-indent
                                      (string-width header-prefix)
                                      (string-width first-body-prefix)))
               (rest-prefix (make-string normalized-width ?\s)))
          (list :header (disco-room--pad-prefix-to-width header-prefix normalized-width)
                :first-body (disco-room--pad-prefix-to-width
                             first-body-prefix normalized-width)
                :rest-body rest-prefix))
      (let ((rest-prefix (make-string fallback-indent ?\s)))
        (list :header (disco-room--pad-prefix-to-width (concat fallback " ") fallback-indent)
              :first-body rest-prefix
              :rest-body rest-prefix)))))

(defun disco-room--attachment-meta-line (attachment)
  "Return compact metadata line for ATTACHMENT object."
  (let* ((content-type (or (alist-get 'content_type attachment) "unknown"))
         (size (alist-get 'size attachment))
         (width (alist-get 'width attachment))
         (height (alist-get 'height attachment))
         (size-text (if (numberp size) (file-size-human-readable size) "n/a"))
         (dims-text (if (and (numberp width) (numberp height))
                        (format "%dx%d" width height)
                      "-")))
    (format "type=%s  size=%s  dims=%s" content-type size-text dims-text)))

(defun disco-room--attachment-default-save-name (attachment)
  "Return default filename for saving ATTACHMENT locally."
  (or (alist-get 'filename attachment)
      (let ((id (alist-get 'id attachment)))
        (if (and id (not (string-empty-p (format "%s" id))))
            (format "attachment-%s" id)
          "attachment.bin"))))

(defun disco-room--sanitize-filename (filename)
  "Return filesystem-safe variant of FILENAME."
  (replace-regexp-in-string
   "[[:cntrl:]/\\\\]+"
   "_"
   (or filename "attachment.bin")))

(defun disco-room--attachment-download-key (attachment)
  "Return stable download state key for ATTACHMENT."
  (format "%s"
          (or (alist-get 'id attachment)
              (disco-media-attachment-download-url attachment)
              (alist-get 'filename attachment)
              (sxhash attachment))))

(defun disco-room--attachment-download-path (attachment)
  "Return default local download path for ATTACHMENT."
  (let* ((key (disco-room--attachment-download-key attachment))
         (safe-name (disco-room--sanitize-filename
                     (disco-room--attachment-default-save-name attachment))))
    (expand-file-name
     (format "%s-%s" (substring (md5 key) 0 10) safe-name)
     disco-room-attachment-download-directory)))

(defun disco-room--attachment-download-state (attachment)
  "Return normalized download state plist for ATTACHMENT."
  (let* ((key (disco-room--attachment-download-key attachment))
         (entry (copy-tree (or (gethash key disco-room--attachment-download-state-table) '())))
         (path (or (plist-get entry :path)
                   (disco-room--attachment-download-path attachment)))
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
    (puthash key entry disco-room--attachment-download-state-table)
    entry))

(defun disco-room--start-attachment-download (attachment &optional open-after)
  "Start asynchronous default-location download for ATTACHMENT.

When OPEN-AFTER is non-nil, open downloaded file in Emacs after completion."
  (let* ((url (disco-media-attachment-download-url attachment))
         (key (disco-room--attachment-download-key attachment))
         (entry (disco-room--attachment-download-state attachment))
         (path (plist-get entry :path))
         (status (plist-get entry :status))
         (expected-size (alist-get 'size attachment)))
    (unless (and (stringp url) (not (string-empty-p url)))
      (user-error "disco: attachment has no downloadable URL"))
    (when (eq status 'downloading)
      (user-error "disco: attachment download already in progress"))
    (make-directory (or (file-name-directory path) disco-room-attachment-download-directory) t)
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
                     (current (copy-tree (or (gethash key disco-room--attachment-download-state-table) '()))))
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
                           (puthash key current disco-room--attachment-download-state-table)
                           (disco-room--rerender-open-rooms)
                           (message "disco: downloaded attachment -> %s (url fallback)" path)
                           (when open-after
                             (find-file path)))
                       (error
                        (setq current (plist-put current :status 'error))
                        (setq current (plist-put current :process nil))
                        (setq current (plist-put current :cancel-requested nil))
                        (setq current (plist-put current :error
                                                 (disco-room--async-error-message fallback-err)))
                        (puthash key current disco-room--attachment-download-state-table)
                        (disco-room--rerender-open-rooms)
                        (message "disco: attachment download failed: %s"
                                 (or (plist-get current :error) "unknown error"))))
                   (progn
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
                     (puthash key current disco-room--attachment-download-state-table)
                     (disco-room--rerender-open-rooms)
                     (message "disco: downloaded attachment -> %s" path)
                     (when open-after
                       (find-file path)))))
               :else
               (lambda (err)
                 (let* ((current (copy-tree (or (gethash key disco-room--attachment-download-state-table) '())))
                        (cancel-requested (disco-util-json-true-p (plist-get current :cancel-requested)))
                        (err-msg (unless cancel-requested
                                   (disco-room--async-error-message err))))
                   (if cancel-requested
                       (progn
                         (setq current (plist-put current :status 'canceled))
                         (setq current (plist-put current :process nil))
                         (setq current (plist-put current :cancel-requested nil))
                         (setq current (plist-put current :error nil))
                         (puthash key current disco-room--attachment-download-state-table)
                         (disco-room--rerender-open-rooms)
                         (message "disco: attachment download canceled"))
                     (condition-case fallback-err
                         (progn
                           (url-copy-file url path t)
                           (setq current (plist-put current :status 'downloaded))
                           (setq current (plist-put current :path path))
                           (setq current (plist-put current :process nil))
                           (setq current (plist-put current :cancel-requested nil))
                           (setq current (plist-put current :error nil))
                           (puthash key current disco-room--attachment-download-state-table)
                           (disco-room--rerender-open-rooms)
                           (message "disco: downloaded attachment -> %s (url fallback)" path)
                           (when open-after
                             (find-file path)))
                       (error
                        (setq current (plist-put current :status 'error))
                        (setq current (plist-put current :process nil))
                        (setq current (plist-put current :cancel-requested nil))
                        (setq current (plist-put current :error
                                                 (or err-msg
                                                     (disco-room--async-error-message fallback-err))))
                        (puthash key current disco-room--attachment-download-state-table)
                        (disco-room--rerender-open-rooms)
                        (message "disco: attachment download failed: %s"
                                 (or (plist-get current :error) "unknown error")))))))))))
      (setq entry (plist-put entry :status 'downloading))
      (setq entry (plist-put entry :process process))
      (setq entry (plist-put entry :cancel-requested nil))
      (setq entry (plist-put entry :error nil))
      (puthash key entry disco-room--attachment-download-state-table)
      (disco-room--rerender-open-rooms)
      (message "disco: downloading attachment %s"
               (or (alist-get 'filename attachment)
                   (alist-get 'id attachment)
                   key)))))

(defun disco-room--cancel-attachment-download (attachment)
  "Cancel active asynchronous download for ATTACHMENT."
  (let* ((key (disco-room--attachment-download-key attachment))
         (entry (copy-tree (or (gethash key disco-room--attachment-download-state-table) '())))
         (status (plist-get entry :status))
         (process (plist-get entry :process)))
    (unless (eq status 'downloading)
      (user-error "disco: attachment is not downloading"))
    (setq entry (plist-put entry :cancel-requested t))
    (puthash key entry disco-room--attachment-download-state-table)
    (when (and (processp process) (process-live-p process))
      (kill-process process))
    (disco-room--rerender-open-rooms)
    (message "disco: canceling attachment download")))

(defun disco-room--open-downloaded-attachment (attachment)
  "Open local downloaded file for ATTACHMENT."
  (let* ((entry (disco-room--attachment-download-state attachment))
         (path (plist-get entry :path)))
    (unless (and (stringp path) (file-exists-p path))
      (user-error "disco: attachment file is not downloaded yet"))
    (find-file path)))

(defun disco-room--play-attachment-video (attachment)
  "Play ATTACHMENT video preferring local file when available."
  (let* ((entry (disco-room--attachment-download-state attachment))
         (path (plist-get entry :path))
         (url (disco-media-attachment-download-url attachment)))
    (cond
     ((and (stringp path) (file-exists-p path))
      (disco-media-play-video-file path))
     ((and (stringp url) (not (string-empty-p url)))
      (disco-media-play-video-url url))
     (t
      (user-error "disco: video attachment has no playable source")))))

(defun disco-room-download-attachment (attachment &optional target-path)
  "Download ATTACHMENT to TARGET-PATH.

When TARGET-PATH is nil, prompt interactively for destination path."
  (interactive)
  (let* ((url (disco-media-attachment-download-url attachment))
         (download-state (disco-room--attachment-download-state attachment))
         (cached-path (plist-get download-state :path))
         (has-cached (and (stringp cached-path) (file-exists-p cached-path)))
         (has-url (and (stringp url) (not (string-empty-p url))))
         (default-name (disco-room--attachment-default-save-name attachment))
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
                   (disco-room--async-error-message err))))))

(defun disco-room--insert-attachment-action-button (label callback help-echo)
  "Insert one attachment action button with LABEL, CALLBACK and HELP-ECHO."
  (disco-ui-insert-action-button
   label
   callback
   :face 'disco-room-attachment-card-action
   :help-echo help-echo))

(defun disco-room--insert-attachment-card (attachment)
  "Insert one rich attachment card for ATTACHMENT object."
  (let* ((kind (disco-room--attachment-kind attachment))
         (video-p (equal kind "video"))
         (summary (disco-room--attachment-summary attachment))
         (meta (disco-room--attachment-meta-line attachment))
         (description (alist-get 'description attachment))
         (url (disco-media-attachment-download-url attachment))
         (preview-url (disco-media-attachment-preview-url attachment))
         (preview-rendering-available (disco-media-attachment-preview-rendering-available-p))
         (preview-cache-key (and (member kind '("img" "video"))
                                 (disco-media-attachment-preview-cache-key attachment)))
         (preview-cache-state (disco-media-attachment-preview-cache-state preview-cache-key))
         (preview-fetching (disco-media-attachment-preview-fetching-p preview-cache-key))
         (preview (and (member kind '("img" "video"))
                       (disco-media-attachment-preview-image attachment)))
         (download-state (disco-room--attachment-download-state attachment))
         (download-status (plist-get download-state :status))
         (download-path (plist-get download-state :path))
         (download-error (plist-get download-state :error))
         (prefix-state (disco-ui-card-prefix-state :face 'disco-room-attachment-card-border)))
    (let ((title-start (point)))
      (insert summary "\n")
      (when (and (stringp url) (not (string-empty-p url)))
        (disco-media-add-open-url-properties title-start (1- (point)) url))
      (disco-ui-apply-line-prefix title-start (point) prefix-state)
      (disco-ui-append-face
       title-start (point) 'disco-room-attachment-card-title))
    (let ((meta-start (point)))
      (insert meta "\n")
      (disco-ui-apply-line-prefix meta-start (point) prefix-state)
      (disco-ui-append-face
       meta-start (point) 'disco-room-attachment-card-meta))
    (when (and (stringp description) (not (string-empty-p description)))
      (let ((desc-start (point)))
        (insert "caption: " description "\n")
        (disco-ui-apply-line-prefix desc-start (point) prefix-state)
        (disco-ui-append-face
         desc-start (point) 'disco-room-attachment-card-meta)))
    (let ((action-start (point)))
      (if (and (stringp url) (not (string-empty-p url)))
          (progn
            (when video-p
              (disco-room--insert-attachment-action-button
               "[Play]"
               (lambda ()
                 (disco-room--play-attachment-video attachment))
               "Play attachment video")
              (insert " "))
            (disco-room--insert-attachment-action-button
             "[Open]"
             (lambda () (browse-url url t))
             "Open attachment URL")
            (insert " ")
            (disco-room--insert-attachment-action-button
             "[Copy]"
             (lambda ()
               (kill-new url)
               (message "disco: copied attachment URL"))
             "Copy attachment URL"))
        (insert "[No URL]"))
      (insert "\n")
      (disco-ui-apply-line-prefix action-start (point) prefix-state)
      (disco-ui-append-face
       action-start (point) 'disco-room-attachment-card-meta))
    (let ((transfer-start (point)))
      (insert "transfer: ")
      (pcase download-status
        ('downloading
         (insert "[Downloading...] ")
         (disco-room--insert-attachment-action-button
          "[Cancel]"
          (lambda ()
            (disco-room--cancel-attachment-download attachment))
          "Cancel attachment download"))
        ('downloaded
         (when video-p
           (disco-room--insert-attachment-action-button
            "[Play]"
            (lambda ()
              (disco-room--play-attachment-video attachment))
            "Play downloaded video")
           (insert " "))
         (disco-room--insert-attachment-action-button
          "[Open Local]"
          (lambda ()
            (disco-room--open-downloaded-attachment attachment))
          "Open downloaded attachment file")
         (insert " ")
         (disco-room--insert-attachment-action-button
          "[Save As]"
          (lambda ()
            (disco-room-download-attachment attachment))
          "Copy downloaded file (or download) to chosen path")
         (when (and (stringp download-path) (file-exists-p download-path))
           (insert (format "  %s" (file-name-nondirectory download-path)))))
        ('error
         (when (and video-p (stringp url) (not (string-empty-p url)))
           (disco-room--insert-attachment-action-button
            "[Play]"
            (lambda ()
              (disco-room--play-attachment-video attachment))
            "Play video URL")
           (insert " "))
         (when (and (stringp url) (not (string-empty-p url)))
           (disco-room--insert-attachment-action-button
            "[Retry]"
            (lambda ()
              (disco-room--start-attachment-download attachment nil))
            "Retry attachment download")
           (insert " "))
         (disco-room--insert-attachment-action-button
          "[Save As]"
          (lambda ()
            (disco-room-download-attachment attachment))
          "Download attachment to chosen path")
         (when (and (stringp download-error) (not (string-empty-p download-error)))
           (insert (format "  error=%s"
                           (truncate-string-to-width download-error 68 nil nil t)))))
        (_
         (if (and (stringp url) (not (string-empty-p url)))
             (progn
               (when video-p
                 (disco-room--insert-attachment-action-button
                  "[Play]"
                  (lambda ()
                    (disco-room--play-attachment-video attachment))
                  "Play video URL")
                 (insert " "))
               (disco-room--insert-attachment-action-button
                "[Download]"
                (lambda ()
                  (disco-room--start-attachment-download attachment nil))
                "Download attachment into local cache directory")
               (insert " ")
               (disco-room--insert-attachment-action-button
                "[Save As]"
                (lambda ()
                  (disco-room-download-attachment attachment))
                "Download attachment to chosen path"))
           (insert "[No URL]"))))
      (insert "\n")
      (disco-ui-apply-line-prefix transfer-start (point) prefix-state)
      (disco-ui-append-face
       transfer-start (point) 'disco-room-attachment-card-meta))
    (when (member kind '("img" "video"))
      (let ((preview-start (point))
            (preview-open-url (or preview-url url))
            (apply-meta-face t)
            (video-preview-p (equal kind "video")))
        (if preview
            (condition-case _
                (let ((slice-start (point)))
                  (setq apply-meta-face nil)
                  (disco-media-insert-image-slices
                   preview
                   (unless video-preview-p preview-open-url)
                   nil
                   (if video-preview-p "[video]" "[image]"))
                  (when (and video-preview-p
                             (stringp preview-open-url)
                             (not (string-empty-p preview-open-url)))
                    (disco-media-add-play-video-properties
                     slice-start
                     (point)
                     preview-open-url)))
              (error
               (insert (if video-preview-p
                           "[video preview unavailable]"
                         "[image unavailable]"))))
          (cond
           ((not preview-rendering-available)
            (insert "[preview disabled]"))
           ((not (and (stringp preview-url) (not (string-empty-p preview-url))))
            (insert "[no preview URL]"))
           ((eq preview-cache-state :missing)
            (insert (if video-preview-p
                        "[video preview unavailable]"
                      "[image unavailable]")))
           ((or preview-fetching preview-cache-key)
            (insert "[loading preview]"))
           (t
            (insert (if video-preview-p
                        "[video preview unavailable]"
                      "[image unavailable]")))))
        (insert "\n")
        (disco-ui-apply-line-prefix preview-start (point) prefix-state)
        (when apply-meta-face
          (disco-ui-append-face
           preview-start (point) 'disco-room-attachment-card-meta))))
    (when (and disco-room-show-attachment-urls
               (stringp url)
               (not (string-empty-p url)))
      (let ((url-start (point)))
        (insert url "\n")
        (disco-media-add-open-url-properties url-start (1- (point)) url)
        (disco-ui-apply-line-prefix url-start (point) prefix-state)
        (disco-ui-append-face url-start (point) 'shadow)))))

(defun disco-room--normalize-list-sequence (value)
  "Normalize VALUE into a list, preserving list/vector elements."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun disco-room--message-reference-type (msg)
  "Return numeric `message_reference.type' for MSG, defaulting to 0."
  (let* ((reference (and (listp msg) (alist-get 'message_reference msg)))
         (raw-type (and (listp reference) (alist-get 'type reference))))
    (cond
     ((integerp raw-type) raw-type)
     ((and (stringp raw-type)
           (string-match-p "\\`[0-9]+\\'" raw-type))
      (string-to-number raw-type))
     (t 0))))

(defun disco-room--message-forward-snapshot (msg)
  "Return first forwarded snapshot message object for MSG, or nil."
  (let* ((snapshots (disco-room--normalize-list-sequence
                     (alist-get 'message_snapshots msg)))
         (first (car snapshots))
         (snapshot-msg (cond
                        ((and (listp first) (listp (alist-get 'message first)))
                         (alist-get 'message first))
                        ((listp first) first)
                        (t nil))))
    (and (listp snapshot-msg) snapshot-msg)))

(defun disco-room--message-forwarded-p (msg)
  "Return non-nil when MSG is a forwarded message."
  (or (= (disco-room--message-reference-type msg) 1)
      (not (null (disco-room--message-forward-snapshot msg)))
      (not (zerop (logand (disco-room--message-flags msg)
                          disco-room--message-flag-has-snapshot)))))

(defun disco-room--forward-recipient-display-name (recipient)
  "Return best display name for one DM RECIPIENT user object."
  (when (listp recipient)
    (or (alist-get 'global_name recipient)
        (alist-get 'username recipient)
        (alist-get 'id recipient)
        "unknown-user")))

(defun disco-room--forward-private-channel-display-name (channel)
  "Return best display name for private CHANNEL."
  (let* ((channel-type (and (listp channel) (alist-get 'type channel)))
         (explicit-name (and (listp channel)
                             (stringp (alist-get 'name channel))
                             (not (string-empty-p (alist-get 'name channel)))
                             (alist-get 'name channel)))
         (recipients (and (listp channel) (alist-get 'recipients channel)))
         (recipient-names
          (delq nil
                (mapcar #'disco-room--forward-recipient-display-name
                        (or recipients '())))))
    (pcase channel-type
      (1 (or (car recipient-names) explicit-name "direct-message"))
      (3 (or explicit-name
             (and recipient-names (mapconcat #'identity recipient-names ", "))
             "group-dm"))
      (_ (or explicit-name "(no-name)")))))

(defun disco-room--forward-channel-name (channel)
  "Return display name for CHANNEL independent of badge prefixes."
  (if (and channel (listp channel) (memq (alist-get 'type channel) '(1 3)))
      (disco-room--forward-private-channel-display-name channel)
    (or (and channel (listp channel) (alist-get 'name channel)) "(no-name)")))

(defun disco-room--forward-source-channel-label (channel channel-id)
  "Return human-readable source channel label for CHANNEL/CHANNEL-ID."
  (if (not (and channel (listp channel)))
      (if (and (stringp channel-id) (not (string-empty-p channel-id)))
          (format "channel:%s" channel-id)
        "unknown-channel")
    (let* ((channel-type (alist-get 'type channel))
           (name (disco-room--forward-channel-name channel)))
      (cond
       ((memq channel-type '(1 3))
        (if (= channel-type 3)
            (format "group:%s" name)
          (format "@%s" name)))
       ((disco-state-channel-thread-p channel)
        (let* ((parent-id (disco-room--normalize-id (alist-get 'parent_id channel)))
               (parent (and parent-id (disco-state-channel parent-id)))
               (parent-name (and parent
                                 (listp parent)
                                 (disco-room--forward-channel-name parent))))
          (if (and (stringp parent-name) (not (string-empty-p parent-name)))
              (format "#%s / #%s (thread)" parent-name name)
            (format "#%s (thread)" name))))
       (t
        (format "#%s" name))))))

(defun disco-room--forward-source-context (msg)
  "Return source context plist for forwarded MSG."
  (let* ((ref-channel-id (disco-room--message-reference-channel-id msg))
         (ref-guild-id (disco-room--message-reference-guild-id msg))
         (channel (and ref-channel-id (disco-state-channel ref-channel-id)))
         (resolved-guild-id
          (or ref-guild-id
              (disco-room--normalize-id
               (and (listp channel) (alist-get 'guild_id channel)))))
         (guild (and resolved-guild-id (disco-room--guild-by-id resolved-guild-id)))
         (guild-label
          (cond
           ((and (listp guild)
                 (stringp (alist-get 'name guild))
                 (not (string-empty-p (alist-get 'name guild))))
            (alist-get 'name guild))
           ((and (stringp resolved-guild-id)
                 (not (string-empty-p resolved-guild-id)))
            (format "guild:%s" resolved-guild-id))
           (t
            "direct message")))
         (channel-label (disco-room--forward-source-channel-label channel ref-channel-id)))
    (list :guild guild
          :guild-id resolved-guild-id
          :guild-label guild-label
          :channel-id ref-channel-id
          :channel-label channel-label)))

(defun disco-room--forward-snapshot-time-label (msg)
  "Return formatted snapshot timestamp for forwarded MSG, or nil."
  (let* ((snapshot (disco-room--message-forward-snapshot msg))
         (timestamp (and (listp snapshot) (alist-get 'timestamp snapshot))))
    (when (and (stringp timestamp) (not (string-empty-p timestamp)))
      (disco-util-format-time timestamp))))

(defun disco-room--forward-snapshot-content (msg)
  "Return forwarded snapshot content for MSG without line folding/truncation."
  (let* ((snapshot (disco-room--message-forward-snapshot msg))
         (text (and (listp snapshot) (alist-get 'content snapshot))))
    (when (and (stringp text) (not (string-empty-p text)))
      (disco-markdown-render text
                             :context 'forward-snapshot
                             :message snapshot))))

(defun disco-room--message-effective-attachments (msg)
  "Return attachments to render for MSG, including forward snapshots."
  (let ((attachments (disco-room--normalize-list-sequence (alist-get 'attachments msg))))
    (or attachments
        (let ((snapshot (disco-room--message-forward-snapshot msg)))
          (disco-room--normalize-list-sequence
           (and (listp snapshot) (alist-get 'attachments snapshot)))))))

(defun disco-room--message-effective-embeds (msg)
  "Return embeds to render for MSG, including forward snapshots."
  (let ((embeds (disco-room--normalize-list-sequence (alist-get 'embeds msg))))
    (or embeds
        (let ((snapshot (disco-room--message-forward-snapshot msg)))
          (disco-room--normalize-list-sequence
           (and (listp snapshot) (alist-get 'embeds snapshot)))))))

(defun disco-room--message-with-effective-embeds (msg)
  "Return MSG copy with effective embeds/attachments for render context."
  (let* ((effective-attachments (disco-room--message-effective-attachments msg))
         (effective-embeds (disco-room--message-effective-embeds msg))
         (raw-attachments (disco-room--normalize-list-sequence
                           (alist-get 'attachments msg)))
         (raw-embeds (disco-room--normalize-list-sequence
                      (alist-get 'embeds msg))))
    (if (and (equal effective-attachments raw-attachments)
             (equal effective-embeds raw-embeds))
        msg
      (let ((copy (copy-tree msg)))
        (setf (alist-get 'attachments copy nil 'remove) effective-attachments)
        (setf (alist-get 'embeds copy nil 'remove) effective-embeds)
        copy))))

(defun disco-room--forwarded-summary-content (msg)
  "Return one-line summary for forwarded MSG content, or nil."
  (when (disco-room--message-forwarded-p msg)
    (let* ((snapshot (disco-room--message-forward-snapshot msg))
           (text (and (listp snapshot) (alist-get 'content snapshot)))
           (display (and (stringp text)
                         (disco-markdown-render text
                                                :context 'forward-summary
                                                :message snapshot)))
           (trimmed (and (stringp display) (string-trim display))))
      (if (and (stringp trimmed) (not (string-empty-p trimmed)))
          (format "[forwarded] %s" trimmed)
        "[forwarded message]"))))

(defun disco-room--message-type (msg)
  "Return numeric message type for MSG, defaulting to 0."
  (let ((raw (alist-get 'type msg)))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t 0))))

(defun disco-room--message-reply-type-p (msg)
  "Return non-nil when MSG is a standard reply message."
  (= (disco-room--message-type msg) 19))

(defun disco-room--user-join-message (msg author)
  "Return rendered USER_JOIN (type 7) message for MSG and AUTHOR."
  (let* ((raw-ts (alist-get 'timestamp msg))
         (n (length disco-room--user-join-message-templates))
         (idx (if (and (stringp raw-ts)
                       (not (string-empty-p raw-ts))
                       (> n 0))
                  (condition-case _
                      (mod (floor (* 1000.0 (float-time (date-to-time raw-ts)))) n)
                    (error 0))
                0))
         (template (if (> n 0)
                       (aref disco-room--user-join-message-templates idx)
                     "%s joined.")))
    (format template author)))

(defun disco-room--thread-starter-reference-message (msg)
  "Resolve referenced source message object for thread starter MSG."
  (let* ((inline (and (listp msg) (alist-get 'referenced_message msg)))
         (ref-id (disco-room--message-reference-id msg))
         (ref-channel-id (or (disco-room--message-reference-channel-id msg)
                             disco-room--channel-id))
         (self-id (disco-room--normalize-id (alist-get 'id msg))))
    (cond
     ((listp inline)
      inline)
     ((not ref-id)
      nil)
     (t
      (or (disco-room--channel-message-by-id ref-channel-id ref-id)
          (let ((fallback (disco-room--channel-message-by-id
                           disco-room--channel-id
                           ref-id)))
            ;; Avoid treating the synthetic type-21 row as its own reference.
            (unless (and (listp fallback)
                         (equal (disco-room--normalize-id (alist-get 'id fallback))
                                self-id))
              fallback)))))))

(defun disco-room--thread-starter-reference-content (msg)
  "Return referenced message content for thread starter MSG, or nil."
  (let* ((resolved (disco-room--thread-starter-reference-message msg))
         (text (and (listp resolved) (alist-get 'content resolved)))
         (display (and (stringp text)
                       (disco-util-unescape-markdown-punctuation text))))
    (when (and (stringp display) (not (string-empty-p (string-trim display))))
      (string-trim display))))

(defun disco-room--message-guild-name (msg)
  "Return display guild name for MSG, or nil if unavailable."
  (let* ((msg-guild-id (disco-room--normalize-id (alist-get 'guild_id msg)))
         (guild-id (or msg-guild-id disco-room--guild-id))
         (guild (and guild-id (disco-room--guild-by-id guild-id))))
    (when (listp guild)
      (let ((name (alist-get 'name guild)))
        (and (stringp name)
             (not (string-empty-p name))
             name)))))

(defun disco-room--message-system-auto-moderation-content (msg)
  "Return human-readable auto moderation line for MSG."
  (let* ((embed (car (disco-room--message-effective-embeds msg)))
         (title (and (listp embed) (disco-util-object-get embed 'title)))
         (description (and (listp embed) (disco-util-object-get embed 'description)))
         (title-text (and (stringp title) (string-trim title)))
         (desc-text (and (stringp description) (string-trim description))))
    (cond
     ((and title-text desc-text
           (not (string-empty-p title-text))
           (not (string-empty-p desc-text)))
      (format "Auto moderation action: %s - %s" title-text desc-text))
     ((and title-text (not (string-empty-p title-text)))
      (format "Auto moderation action: %s" title-text))
     ((and desc-text (not (string-empty-p desc-text)))
      (format "Auto moderation action: %s" desc-text))
     (t
      "Auto moderation action was triggered."))))

(defun disco-room--message-system-content (msg)
  "Return rendered system content for MSG type, or nil if not handled."
  (let* ((type (disco-room--message-type msg))
         (author (disco-room--message-author msg))
         (content (string-trim (disco-util-unescape-markdown-punctuation
                                (or (alist-get 'content msg) ""))))
         (boost-times (and (not (string-empty-p content)) content))
         (guild-name (or (disco-room--message-guild-name msg) "this server")))
    (pcase type
      (6
       (format "%s pinned a message to this channel. View all pinned messages." author))
      (7
       (disco-room--user-join-message msg author))
      ((or 8 9 10 11)
       (let ((base (if boost-times
                       (format "%s just boosted the server %s times!" author boost-times)
                     (format "%s just boosted the server!" author))))
         (pcase type
           (8 base)
           (9 (concat base " Server has reached Level 1!"))
           (10 (concat base " Server has reached Level 2!"))
           (11 (concat base " Server has reached Level 3!")))))
      (12
       (format "%s has added %s to this channel. Its most important updates will show up here."
               author
               (if (string-empty-p content) "a followed channel" content)))
      (14
       "This server has been removed from Server Discovery because it no longer passes all the requirements. Check Server Settings for more details.")
      (15
       "This server is eligible for Server Discovery again and has been automatically relisted!")
      (16
       "This server has failed Discovery activity requirements for 1 week. If this server fails for 4 weeks in a row, it will be automatically removed from Discovery.")
      (17
       "This server has failed Discovery activity requirements for 3 weeks in a row. If this server fails for 1 more week, it will be removed from Discovery.")
      (18
       (if (string-empty-p content)
           (format "%s started a thread. See all threads." author)
         (format "%s started a thread: %s. See all threads." author content)))
      (21
       (or (disco-room--thread-starter-reference-content msg)
           "Sorry, we couldn't load the first message in this thread."))
      (22
       "Wondering who to invite? Start by inviting anyone who can help you build the server!")
      (24
       (disco-room--message-system-auto-moderation-content msg))
      (25
       (let* ((role-subscription
               (and (listp msg)
                    (disco-util-object-get msg 'role_subscription_data)))
              (tier-name (and (listp role-subscription)
                              (disco-util-object-get role-subscription 'tier_name)))
              (months (and (listp role-subscription)
                           (disco-util-object-get role-subscription
                                                  'total_months_subscribed)))
              (renewal (and (listp role-subscription)
                            (disco-util-json-true-p
                             (disco-util-object-get role-subscription 'is_renewal))))
              (tier-label (if (and (stringp tier-name)
                                   (not (string-empty-p tier-name)))
                              tier-name
                            "a role subscription tier")))
         (if (numberp months)
             (format "%s %s %s and has been a subscriber of %s for %d month%s!"
                     author
                     (if renewal "renewed" "joined")
                     tier-label
                     guild-name
                     months
                     (if (= months 1) "" "s"))
           (format "%s %s %s."
                   author
                   (if renewal "renewed" "joined")
                   tier-label))))
      (26
       (if (string-empty-p content)
           "A premium interaction upsell message was sent."
         content))
      (27
       (if (string-empty-p content)
           (format "%s started a Stage." author)
         (format "%s started %s" author content)))
      (28
       (if (string-empty-p content)
           (format "%s ended a Stage." author)
         (format "%s ended %s" author content)))
      (29
       (format "%s is now a speaker." author))
      (30
       (format "%s requested to speak." author))
      (31
       (if (string-empty-p content)
           (format "%s changed the Stage topic." author)
         (format "%s changed the Stage topic: %s" author content)))
      (32
       (let* ((application (and (listp msg)
                                (disco-util-object-get msg 'application)))
              (app-name (and (listp application)
                             (disco-util-object-get application 'name))))
         (format "%s upgraded %s to premium for this server!"
                 author
                 (if (and (stringp app-name) (not (string-empty-p app-name)))
                     app-name
                   "a deleted application"))))
      (36
       (if (string-empty-p content)
           (format "%s enabled security actions." author)
         (format "%s enabled security actions until %s." author content)))
      (37
       (format "%s disabled security actions." author))
      (38
       (format "%s reported a raid in %s." author guild-name))
      (39
       (format "%s reported a false alarm in %s." author guild-name))
      (44
       (let* ((purchase-notification (and (listp msg)
                                          (disco-util-object-get msg
                                                                 'purchase_notification)))
              (guild-product-purchase
               (and (listp purchase-notification)
                    (disco-util-object-get purchase-notification
                                           'guild_product_purchase)))
              (product-name (and (listp guild-product-purchase)
                                 (disco-util-object-get guild-product-purchase
                                                        'product_name))))
         (if (and (stringp product-name) (not (string-empty-p product-name)))
             (format "%s has purchased %s!" author product-name)
           (format "%s completed a guild product purchase." author))))
      (46
       "A poll result was finalized.")
      (_ nil))))

(defun disco-room--message-display-content (msg)
  "Return human-readable content string for message MSG."
  (let* ((raw-content (or (alist-get 'content msg) ""))
         (content (disco-markdown-render raw-content
                                         :context 'room-message
                                         :message msg))
         (attachments (disco-room--message-effective-attachments msg))
         (embeds (disco-room--message-effective-embeds msg))
         (poll (disco-room--message-poll msg))
         (attachment-count (length attachments))
         (embed-count (length embeds))
         (poll-count (if poll 1 0))
         (showing-attachments (and disco-room-show-attachments (> attachment-count 0)))
         (showing-embeds (and disco-room-show-embeds (> embed-count 0)))
         (showing-poll (and disco-room-show-polls (> poll-count 0)))
         (msg-type (disco-room--message-type msg))
         (system-content (disco-room--message-system-content msg))
         (forwarded-summary (and (string-empty-p content)
                                 (not disco-room-use-rich-forward-cards)
                                 (disco-room--forwarded-summary-content msg))))
    (if (and (stringp system-content) (not (string-empty-p system-content)))
        system-content
      (if (string-empty-p content)
          (cond
           ((and (stringp forwarded-summary) (not (string-empty-p forwarded-summary)))
            forwarded-summary)
           ((and disco-room-use-rich-forward-cards
                 (disco-room--message-forwarded-p msg))
            "")
           ((or showing-attachments showing-embeds showing-poll)
            "")
           ((and (> attachment-count 0) (> embed-count 0) (> poll-count 0))
            (format "[attachment x%d, embed x%d, poll]" attachment-count embed-count))
           ((and (> attachment-count 0) (> embed-count 0))
            (format "[attachment x%d, embed x%d]" attachment-count embed-count))
           ((and (> attachment-count 0) (> poll-count 0))
            (format "[attachment x%d, poll]" attachment-count))
           ((and (> embed-count 0) (> poll-count 0))
            (format "[embed x%d, poll]" embed-count))
           ((> attachment-count 0)
            (format "[attachment x%d]" attachment-count))
           ((> embed-count 0)
            (format "[embed x%d]" embed-count))
           ((> poll-count 0)
            "[poll]")
           ((/= msg-type 0)
            (format "[system message type %d]" msg-type))
           (t "[empty]"))
        content))))

(defun disco-room--attachment-kind (attachment)
  "Return short attachment kind string for ATTACHMENT object."
  (let* ((content-type (downcase (or (alist-get 'content_type attachment) "")))
         (filename (downcase (or (alist-get 'filename attachment) ""))))
    (cond
     ((string-prefix-p "image/" content-type) "img")
     ((string-prefix-p "video/" content-type) "video")
     ((string-prefix-p "audio/" content-type) "audio")
     ((string-match-p "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|bmp\\|svg\\)\\'" filename) "img")
     ((string-match-p "\\.\\(?:mp4\\|mov\\|mkv\\|webm\\|avi\\)\\'" filename) "video")
     ((string-match-p "\\.\\(?:mp3\\|wav\\|ogg\\|flac\\|m4a\\)\\'" filename) "audio")
     (t "file"))))

(defun disco-room--attachment-summary (attachment)
  "Return one-line attachment summary string for ATTACHMENT object."
  (let* ((kind (disco-room--attachment-kind attachment))
         (filename (or (alist-get 'filename attachment) "unnamed"))
         (size (alist-get 'size attachment))
         (width (alist-get 'width attachment))
         (height (alist-get 'height attachment))
         (size-text (when (numberp size)
                      (file-size-human-readable size)))
         (dims-text (when (and (numberp width) (numberp height))
                      (format "%dx%d" width height)))
         (detail (cond
                  ((and size-text dims-text)
                   (format " (%s, %s)" size-text dims-text))
                  (size-text
                   (format " (%s)" size-text))
                  (dims-text
                   (format " (%s)" dims-text))
                  (t ""))))
    (format "[%s] %s%s" kind filename detail)))

(defun disco-room--insert-message-attachments (msg &optional prefix)
  "Insert attachment detail lines for MSG.

PREFIX can be a fixed prefix string or mutable prefix-state."
  (when disco-room-show-attachments
    (dolist (attachment (or (disco-room--message-effective-attachments msg) '()))
      (condition-case _
          (if disco-room-use-rich-attachment-cards
              (disco-room--insert-attachment-card attachment)
            (let ((line-start (point))
                  (url (or (alist-get 'url attachment)
                           (alist-get 'proxy_url attachment))))
              (insert (disco-room--attachment-summary attachment))
              (insert "\n")
              (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
              (add-text-properties line-start (point) '(face disco-room-message-meta))
              (when (and disco-room-show-attachment-urls
                         (stringp url)
                         (not (string-empty-p url)))
                (let ((url-start (point)))
                  (insert "  " url "\n")
                  (disco-ui-apply-line-prefix url-start (point) (or prefix "    "))
                  (add-text-properties url-start (point) '(face shadow))))))
        (error
         (let ((line-start (point)))
           (insert "[file] "
                   (or (alist-get 'filename attachment)
                       (format "%s" (or (alist-get 'id attachment) "unknown")))
                   " [render fallback]\n")
           (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
           (add-text-properties line-start (point) '(face shadow))))))))

(defun disco-room--insert-message-embeds (msg)
  "Insert embed detail lines for MSG."
  (disco-embed-insert-message-embeds
   (disco-room--message-with-effective-embeds msg)))

(defun disco-room--message-poll (msg)
  "Return poll object from MSG, or nil when absent."
  (let ((poll (alist-get 'poll msg)))
    (and (listp poll) poll)))

(defun disco-room--poll-results (poll)
  "Return poll results object from POLL, or nil when unknown."
  (let ((results (and (listp poll) (alist-get 'results poll))))
    (and (listp results) results)))

(defun disco-room--poll-answer-id (answer)
  "Return normalized integer answer id from poll ANSWER, or nil."
  (let ((raw (and (listp answer) (alist-get 'answer_id answer))))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

(defun disco-room--poll-answer-media (answer)
  "Return poll media object for ANSWER."
  (let ((media (and (listp answer) (alist-get 'poll_media answer))))
    (and (listp media) media)))

(defun disco-room--poll-answer-text (answer)
  "Return display text for poll ANSWER."
  (let* ((media (disco-room--poll-answer-media answer))
         (text (and media (alist-get 'text media))))
    (if (and (stringp text) (not (string-empty-p (string-trim text))))
        (string-trim text)
      "(no text)")))

(defun disco-room--poll-answer-emoji (answer)
  "Return emoji label for poll ANSWER, or nil."
  (let* ((media (disco-room--poll-answer-media answer))
         (emoji (and media (alist-get 'emoji media)))
         (name (and (listp emoji) (alist-get 'name emoji))))
    (and (stringp name)
         (not (string-empty-p name))
         name)))

(defun disco-room--poll-question-text (poll)
  "Return normalized question text for POLL."
  (let* ((question (and (listp poll) (alist-get 'question poll)))
         (text (cond
                ((stringp question) question)
                ((listp question) (alist-get 'text question))
                (t nil))))
    (if (and (stringp text) (not (string-empty-p (string-trim text))))
        (string-trim text)
      "(untitled poll)")))

(defun disco-room--poll-answer-count-entry (poll answer-id)
  "Return result count entry from POLL for ANSWER-ID, or nil."
  (let ((counts (and (disco-room--poll-results poll)
                     (alist-get 'answer_counts (disco-room--poll-results poll)))))
    (seq-find
     (lambda (entry)
       (let ((entry-id (alist-get 'id entry)))
         (or (and (integerp entry-id)
                  (= entry-id answer-id))
             (and (stringp entry-id)
                  (string-match-p "\\`[0-9]+\\'" entry-id)
                  (= (string-to-number entry-id) answer-id)))))
     (or counts '()))))

(defun disco-room--poll-answer-count (poll answer-id)
  "Return vote count for ANSWER-ID in POLL."
  (let* ((entry (disco-room--poll-answer-count-entry poll answer-id))
         (count (and (listp entry) (alist-get 'count entry))))
    (if (numberp count)
        (max 0 count)
      0)))

(defun disco-room--poll-answer-me-voted-p (poll answer-id)
  "Return non-nil when current user voted ANSWER-ID in POLL."
  (let* ((entry (disco-room--poll-answer-count-entry poll answer-id))
         (me-voted (and (listp entry) (alist-get 'me_voted entry))))
    (disco-util-json-true-p me-voted)))

(defun disco-room--poll-total-votes (poll)
  "Return aggregate vote count from POLL results."
  (let ((counts (and (disco-room--poll-results poll)
                     (alist-get 'answer_counts (disco-room--poll-results poll))))
        (total 0))
    (dolist (entry (or counts '()) total)
      (let ((count (and (listp entry) (alist-get 'count entry))))
        (when (numberp count)
          (setq total (+ total (max 0 count))))))))

(defun disco-room--poll-multiselect-p (poll)
  "Return non-nil when POLL allows multiple answers."
  (disco-util-json-true-p (alist-get 'allow_multiselect poll)))

(defun disco-room--poll-expired-p (poll)
  "Return non-nil when POLL expiry is in the past."
  (let ((expiry (and (listp poll) (alist-get 'expiry poll))))
    (when (and (stringp expiry)
               (not (string-empty-p expiry)))
      (condition-case _
          (<= (float-time (date-to-time expiry)) (float-time))
        (error nil)))))

(defun disco-room--poll-expiry-label (poll)
  "Return formatted expiry text for POLL, or nil."
  (let ((expiry (and (listp poll) (alist-get 'expiry poll))))
    (when (and (stringp expiry)
               (not (string-empty-p expiry)))
      (condition-case _
          (format-time-string disco-room-poll-date-format (date-to-time expiry))
        (error nil)))))

(defun disco-room--poll-state-label (poll)
  "Return short status label for POLL."
  (let* ((results (disco-room--poll-results poll))
         (finalized (and (listp results)
                         (disco-util-json-true-p (alist-get 'is_finalized results))))
         (expired (disco-room--poll-expired-p poll)))
    (cond
     (finalized "finalized")
     (expired "closed")
     (t "open"))))

(defun disco-room--poll-voted-answer-ids (poll)
  "Return list of answer IDs voted by current user in POLL."
  (let ((answers (or (alist-get 'answers poll) '()))
        out)
    (dolist (answer answers (nreverse out))
      (let ((answer-id (disco-room--poll-answer-id answer)))
        (when (and answer-id
                   (disco-room--poll-answer-me-voted-p poll answer-id))
          (push answer-id out))))))

(defun disco-room--poll-normalize-answer-id-list (answer-ids)
  "Return ANSWER-IDS normalized as deduped integer list."
  (let (out)
    (dolist (it (or answer-ids '()) (nreverse out))
      (let ((id (cond
                 ((integerp it) it)
                 ((and (stringp it)
                       (string-match-p "\\`[0-9]+\\'" it))
                  (string-to-number it))
                 (t nil))))
        (when (and (integerp id) (> id 0) (not (member id out)))
          (push id out))))))

(defun disco-room--poll-draft-selection (message-id)
  "Return staged poll selection list for MESSAGE-ID.

This may return nil when a staged empty selection exists."
  (when (and (hash-table-p disco-room--poll-selection-drafts)
             message-id)
    (let ((value (gethash message-id disco-room--poll-selection-drafts :disco--missing)))
      (unless (eq value :disco--missing)
        value))))

(defun disco-room--poll-draft-selection-present-p (message-id)
  "Return non-nil when MESSAGE-ID has a staged poll selection entry."
  (and (hash-table-p disco-room--poll-selection-drafts)
       message-id
       (not (eq (gethash message-id disco-room--poll-selection-drafts :disco--missing)
                :disco--missing))))

(defun disco-room--poll-set-draft-selection (message-id answer-ids)
  "Store staged poll ANSWER-IDS for MESSAGE-ID and return normalized list."
  (let ((normalized (disco-room--poll-normalize-answer-id-list answer-ids)))
    (unless (hash-table-p disco-room--poll-selection-drafts)
      (setq disco-room--poll-selection-drafts (make-hash-table :test #'equal)))
    (if normalized
        (puthash message-id normalized disco-room--poll-selection-drafts)
      (puthash message-id '() disco-room--poll-selection-drafts))
    normalized))

(defun disco-room--poll-clear-draft-selection (message-id)
  "Clear staged poll selection for MESSAGE-ID."
  (when (and (hash-table-p disco-room--poll-selection-drafts)
             message-id)
    (remhash message-id disco-room--poll-selection-drafts)))

(defun disco-room--poll-effective-selection (message-id poll)
  "Return effective UI selection for MESSAGE-ID in POLL.

Staged selection takes precedence over committed vote state."
  (if (disco-room--poll-draft-selection-present-p message-id)
      (disco-room--poll-normalize-answer-id-list
       (disco-room--poll-draft-selection message-id))
    (disco-room--poll-voted-answer-ids poll)))

(defun disco-room--poll-add-selection (message-id poll answer-id)
  "Return staged selection with ANSWER-ID added for MESSAGE-ID/POLL."
  (let ((current (copy-sequence (disco-room--poll-effective-selection message-id poll))))
    (if (disco-room--poll-multiselect-p poll)
        (if (member answer-id current)
            current
          (append current (list answer-id)))
      (list answer-id))))

(defun disco-room--poll-toggle-draft-selection (message-id poll answer-id)
  "Return staged selection after toggling ANSWER-ID for MESSAGE-ID/POLL."
  (let* ((current (copy-sequence (disco-room--poll-effective-selection message-id poll)))
         (has (member answer-id current)))
    (if (disco-room--poll-multiselect-p poll)
        (if has
            (delete answer-id current)
          (append current (list answer-id)))
      (if has
          '()
        (list answer-id)))))

(defun disco-room--poll-draft-differs-p (message-id poll)
  "Return non-nil when staged selection differs from committed vote state."
  (let ((draft (disco-room--poll-draft-selection message-id))
        (committed (disco-room--poll-voted-answer-ids poll)))
    (and (disco-room--poll-draft-selection-present-p message-id)
         (not (equal (disco-room--poll-normalize-answer-id-list draft)
                     (disco-room--poll-normalize-answer-id-list committed))))))

(defun disco-room--poll-answer-selected-p (message-id poll answer-id)
  "Return non-nil when ANSWER-ID is selected in effective poll UI state."
  (member answer-id (disco-room--poll-effective-selection message-id poll)))

(defun disco-room--message-with-poll-vote-selection (msg selected-answer-ids)
  "Return MSG copy with current-user poll votes set to SELECTED-ANSWER-IDS."
  (let* ((updated (copy-tree msg))
         (poll (copy-tree (disco-room--message-poll msg))))
    (if (not (listp poll))
        updated
      (let* ((results (copy-tree (or (disco-room--poll-results poll) '())))
             (counts (copy-tree (or (alist-get 'answer_counts results) '())))
             (selected (delete-dups (copy-sequence (or selected-answer-ids '()))))
             (previous (disco-room--poll-voted-answer-ids poll)))
        (dolist (answer-id (delete-dups (append previous selected)))
          (let* ((existing (seq-find (lambda (it)
                                       (equal (alist-get 'id it) answer-id))
                                     counts))
                 (entry (or (copy-tree existing)
                            `((id . ,answer-id)
                              (count . 0)
                              (me_voted . :false))))
                 (count (max 0 (or (alist-get 'count entry) 0)))
                 (was-voted (member answer-id previous))
                 (now-voted (member answer-id selected)))
            (when (and (not was-voted) now-voted)
              (setq count (1+ count)))
            (when (and was-voted (not now-voted))
              (setq count (max 0 (1- count))))
            (setf (alist-get 'count entry nil 'remove) count)
            (setf (alist-get 'me_voted entry nil 'remove) (if now-voted t :false))
            (if existing
                (setq counts (mapcar (lambda (it)
                                       (if (equal (alist-get 'id it) answer-id)
                                           entry
                                         it))
                                     counts))
              (setq counts (append counts (list entry))))))
        (setf (alist-get 'answer_counts results nil 'remove) counts)
        (setf (alist-get 'results poll nil 'remove) results)
        (setf (alist-get 'poll updated nil 'remove) poll)
        updated))))

(defun disco-room--message-with-poll-vote-delta (msg answer-id addp user-id)
  "Return MSG copy updated with one poll vote delta.

ANSWER-ID is the poll answer receiving update. ADDP non-nil means add vote;
otherwise remove. USER-ID is used to set `me_voted' when event is for self."
  (let* ((updated (copy-tree msg))
         (poll (copy-tree (disco-room--message-poll msg))))
    (if (not (and (listp poll) (integerp answer-id)))
        updated
      (let* ((results (copy-tree (or (disco-room--poll-results poll) '())))
             (counts (copy-tree (or (alist-get 'answer_counts results) '())))
             (self-id (disco-gateway-current-user-id))
             (is-self (and self-id user-id
                           (equal (format "%s" self-id) (format "%s" user-id))))
             (existing (seq-find (lambda (it)
                                   (equal (alist-get 'id it) answer-id))
                                 counts))
             (entry (or (copy-tree existing)
                        `((id . ,answer-id)
                          (count . 0)
                          (me_voted . :false))))
             (count (max 0 (or (alist-get 'count entry) 0))))
        (setq count (if addp
                        (1+ count)
                      (max 0 (1- count))))
        (setf (alist-get 'count entry nil 'remove) count)
        (when is-self
          (setf (alist-get 'me_voted entry nil 'remove)
                (if addp t :false)))
        (if existing
            (setq counts (mapcar (lambda (it)
                                   (if (equal (alist-get 'id it) answer-id)
                                       entry
                                     it))
                                 counts))
          (setq counts (append counts (list entry))))
        (setf (alist-get 'answer_counts results nil 'remove) counts)
        (setf (alist-get 'results poll nil 'remove) results)
        (setf (alist-get 'poll updated nil 'remove) poll)
        updated))))

(defun disco-room--apply-live-poll-vote-event-partially (event)
  "Apply poll vote EVENT to local room state and EWOC incrementally."
  (let* ((event-type (plist-get event :type))
         (message-id (plist-get event :message-id))
         (raw-answer-id (plist-get event :answer-id))
         (answer-id (cond
                     ((integerp raw-answer-id) raw-answer-id)
                     ((and (stringp raw-answer-id)
                           (string-match-p "\\`[0-9]+\\'" raw-answer-id))
                      (string-to-number raw-answer-id))
                     (t nil)))
         (user-id (plist-get event :user-id))
         (self-id (disco-gateway-current-user-id))
         (is-self (and self-id user-id
                       (equal (format "%s" self-id) (format "%s" user-id))))
         (applied
          (and (integerp answer-id)
               (pcase event-type
                 ('message-poll-vote-add
                  (disco-room--update-message-locally
                   message-id
                   (lambda (msg)
                     (disco-room--message-with-poll-vote-delta msg answer-id t user-id))))
                 ('message-poll-vote-remove
                  (disco-room--update-message-locally
                   message-id
                   (lambda (msg)
                     (disco-room--message-with-poll-vote-delta msg answer-id nil user-id))))
                 (_ nil)))))
    (when (and applied is-self)
      (disco-room--poll-clear-draft-selection message-id))
    applied))

(defun disco-room--insert-message-poll (msg)
  "Insert poll detail block for MSG when present."
  (when disco-room-show-polls
    (let* ((poll (disco-room--message-poll msg))
           (message-id (alist-get 'id msg))
           (question (and poll (disco-room--poll-question-text poll)))
           (state (and poll (disco-room--poll-state-label poll)))
           (expiry-label (and poll (disco-room--poll-expiry-label poll)))
           (answers (and poll (or (alist-get 'answers poll) '())))
           (committed-selection (and poll (disco-room--poll-voted-answer-ids poll)))
           (effective-selection (and poll (disco-room--poll-effective-selection message-id poll)))
           (draft-differs (and poll (disco-room--poll-draft-differs-p message-id poll)))
           (can-vote (and poll (disco-room--poll-can-vote-p msg)))
           (can-expire (and poll (disco-room--poll-can-expire-p msg))))
      (when poll
        (let ((prefix-state (disco-ui-card-prefix-state :face 'disco-room-attachment-card-border)))
          (let ((title-start (point)))
            (insert "[poll] " question "\n")
            (disco-ui-apply-line-prefix title-start (point) prefix-state)
            (add-text-properties title-start (point)
                                 `(disco-message-id ,message-id))
            (disco-ui-append-face
             title-start (point) disco-room-poll-title-face))
          (let ((meta-start (point))
                (parts (list (format "status=%s" state))))
            (when (disco-room--poll-multiselect-p poll)
              (setq parts (append parts '("multi"))))
            (when (and disco-room-poll-show-total-votes
                       (disco-room--poll-results poll))
              (setq parts
                    (append parts
                            (list (format "votes=%d"
                                          (disco-room--poll-total-votes poll))))))
            (when expiry-label
              (setq parts (append parts (list (format "ends=%s" expiry-label)))))
            (insert (mapconcat #'identity parts "   ") "\n")
            (disco-ui-apply-line-prefix meta-start (point) prefix-state)
            (add-text-properties meta-start (point)
                                 `(disco-message-id ,message-id))
            (disco-ui-append-face
             meta-start (point) disco-room-poll-meta-face))
          (dolist (answer answers)
            (let* ((answer-id (disco-room--poll-answer-id answer))
                   (selected (and answer-id
                                  (member answer-id effective-selection)))
                   (count (and answer-id
                               (disco-room--poll-answer-count poll answer-id)))
                   (emoji (disco-room--poll-answer-emoji answer))
                   (label (disco-room--poll-answer-text answer))
                   (line-start (point)))
              (if (and can-vote answer-id disco-room-poll-auto-toggle-vote)
                  (disco-ui-insert-action-button
                   (format "%s %s%s"
                           (if selected "[x]" "[ ]")
                           (if emoji (concat emoji " ") "")
                           label)
                   (lambda ()
                     (disco-room-toggle-poll-answer answer-id message-id))
                   :face (if selected
                             disco-room-poll-voted-face
                           disco-room-poll-option-face)
                   :help-echo "Toggle staged selection for this answer")
                (insert (propertize
                         (format "%s %s%s"
                                 (if selected "[x]" "[ ]")
                                 (if emoji (concat emoji " ") "")
                                 label)
                         'face (if selected
                                   disco-room-poll-voted-face
                                 disco-room-poll-option-face))))
              (when (and disco-room-poll-show-voter-counts (integerp count))
                (insert (propertize (format "  (%d)" count)
                                    'face disco-room-poll-meta-face)))
              (insert "\n")
              (disco-ui-apply-line-prefix line-start (point) prefix-state)
              (add-text-properties line-start (point)
                                   `(disco-message-id ,message-id
                                     disco-poll-answer-id ,answer-id))))
          (let ((actions-start (point))
                (inserted nil))
            (when (and can-vote draft-differs effective-selection)
              (disco-ui-insert-action-button
               "[Vote]"
               (lambda ()
                 (disco-room-submit-poll-vote message-id))
               :face disco-room-poll-button-face
               :help-echo "Submit selected poll answers")
              (insert " ")
              (setq inserted t))
            (when (and can-vote
                       committed-selection
                       (or (not draft-differs)
                           (null effective-selection)))
              (disco-ui-insert-action-button
               "[Remove vote]"
               (lambda ()
                 (disco-room-clear-poll-votes message-id))
               :face disco-room-poll-button-face
               :help-echo "Remove all my poll votes")
              (insert " ")
              (setq inserted t))
            (when can-expire
              (disco-ui-insert-action-button
               "[End poll]"
               (lambda ()
                 (disco-room-expire-poll message-id))
               :face disco-room-poll-button-face
               :help-echo "End this poll now")
              (setq inserted t))
            (unless inserted
              (insert (propertize "[no poll actions available]" 'face 'shadow)))
            (insert "\n")
            (disco-ui-apply-line-prefix actions-start (point) prefix-state)
            (add-text-properties actions-start (point)
                                 `(disco-message-id ,message-id))
            (disco-ui-append-face
             actions-start (point) disco-room-poll-meta-face)))))))

(defun disco-room--reaction-emoji (reaction)
  "Extract display emoji string from REACTION object."
  (let* ((emoji (alist-get 'emoji reaction))
         (name (and (listp emoji) (alist-get 'name emoji))))
    (or name
        (and (stringp emoji) emoji)
        (alist-get 'emoji_name reaction)
        "?")))

(defun disco-room--reaction-count (reaction)
  "Return integer count for REACTION object."
  (or (alist-get 'count reaction)
      (alist-get 'total_count reaction)
      0))

(defun disco-room--reaction-selected-p (reaction)
  "Return non-nil when REACTION is selected by current user."
  (or (disco-util-json-true-p (alist-get 'me reaction))
      (disco-util-json-true-p (alist-get 'is_chosen reaction))))

(defun disco-room--message-reactions (msg)
  "Return normalized reactions list for MSG."
  (or (alist-get 'reactions msg)
      (alist-get 'reaction_counts msg)
      '()))

(defun disco-room--insert-message-reactions (msg &optional prefix)
  "Insert reaction chip line for MSG.

PREFIX can be a fixed prefix string or mutable prefix-state."
  (when disco-room-show-reactions
    (let ((reactions (disco-room--message-reactions msg))
          (line-start (point))
          (first t))
      (when reactions
        (dolist (reaction reactions)
          (unless first
            (insert " "))
          (setq first nil)
          (let ((chip (format "[%s %s]"
                              (disco-room--reaction-emoji reaction)
                              (disco-room--reaction-count reaction))))
            (insert (propertize chip
                                'face (if (disco-room--reaction-selected-p reaction)
                                          'disco-room-reaction-selected
                                        'disco-room-reaction)))))
        (insert "\n")
        (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
        (add-text-properties line-start (point) '(face disco-room-message-meta))))))

(defun disco-room--parse-reaction-input (emoji)
  "Parse user EMOJI input into plist with :id/:name.

Accepted forms: Unicode emoji, `name:id`, or `<:name:id>`/`<a:name:id>`."
  (let ((raw (string-trim (or emoji ""))))
    (cond
     ((string-match "^<a?:\\([^:>]+\\):\\([0-9]+\\)>$" raw)
      (list :name (match-string 1 raw)
            :id (match-string 2 raw)))
     ((string-match "^\\([^:]+\\):\\([0-9]+\\)$" raw)
      (list :name (match-string 1 raw)
            :id (match-string 2 raw)))
     (t
      (list :name raw :id nil)))))

(defun disco-room--reaction-matches-input-p (reaction emoji)
  "Return non-nil when REACTION matches EMOJI input string."
  (let* ((spec (disco-room--parse-reaction-input emoji))
         (target-id (plist-get spec :id))
         (target-name (plist-get spec :name))
         (emoji-obj (alist-get 'emoji reaction))
         (reaction-id (and (listp emoji-obj) (alist-get 'id emoji-obj)))
         (reaction-name (disco-room--reaction-emoji reaction)))
    (if target-id
        (and reaction-id (equal (format "%s" reaction-id) (format "%s" target-id)))
      (equal reaction-name target-name))))

(defun disco-room--message-has-own-reaction-p (msg emoji)
  "Return non-nil when MSG has current-user reaction EMOJI."
  (let ((found nil))
    (dolist (reaction (disco-room--message-reactions msg))
      (when (and (disco-room--reaction-matches-input-p reaction emoji)
                 (disco-room--reaction-selected-p reaction))
        (setq found t)))
    found))

(defun disco-room--message-with-reaction-delta (msg emoji addp)
  "Return MSG copy after applying one reaction delta for EMOJI.

When ADDP is non-nil, reaction count is increased and marked selected;
otherwise selected flag is cleared and count is decreased."
  (let* ((updated (copy-tree msg))
         (reactions (copy-tree (disco-room--message-reactions msg)))
         (spec (disco-room--parse-reaction-input emoji))
         (target-id (plist-get spec :id))
         (target-name (plist-get spec :name))
         (found nil)
         (next '()))
    (dolist (reaction reactions)
      (if (disco-room--reaction-matches-input-p reaction emoji)
          (let* ((count (max 0 (or (disco-room--reaction-count reaction) 0)))
                 (next-count (if addp (1+ count) (max 0 (1- count))))
                 (item (copy-tree reaction)))
            (setq found t)
            (setf (alist-get 'count item nil 'remove) next-count)
            (setf (alist-get 'me item nil 'remove) (if addp t :false))
            (when (> next-count 0)
              (push item next)))
        (push reaction next)))
    (unless (or found (not addp))
      (push `((count . 1)
              (me . t)
              (emoji . ((name . ,target-name)
                        (id . ,target-id))))
            next))
    (setf (alist-get 'reactions updated nil 'remove) (nreverse next))
    updated))

(defun disco-room--update-message-locally (message-id updater)
  "Apply UPDATER function to message with MESSAGE-ID in current room state."
  (let* ((messages (or (disco-state-messages disco-room--channel-id) '()))
         (updated-list nil)
         (updated-msg nil))
    (dolist (msg messages)
      (if (and message-id (equal (alist-get 'id msg) message-id))
          (let ((next (funcall updater msg)))
            (push next updated-list)
            (setq updated-msg next))
        (push msg updated-list)))
    (setq updated-list (nreverse updated-list))
    (disco-state-put-messages disco-room--channel-id updated-list)
    (when updated-msg
      (disco-room--upsert-message-node updated-msg))
    updated-msg))

(defun disco-room--event-emoji->input (emoji)
  "Normalize gateway EMOJI payload into reaction input string."
  (cond
   ((and (listp emoji) (alist-get 'id emoji))
    (format "%s:%s"
            (or (alist-get 'name emoji) "_")
            (alist-get 'id emoji)))
   ((and (listp emoji) (alist-get 'name emoji))
    (alist-get 'name emoji))
   ((stringp emoji)
    emoji)
   (t nil)))

(defun disco-room--message-cleared-reactions (msg)
  "Return MSG copy with all reactions removed."
  (let ((updated (copy-tree msg)))
    (setf (alist-get 'reactions updated nil 'remove) '())
    (setf (alist-get 'reaction_counts updated nil 'remove) '())
    updated))

(defun disco-room--message-removed-reaction-emoji (msg emoji)
  "Return MSG copy with reaction EMOJI removed completely."
  (let* ((updated (copy-tree msg))
         (reactions (copy-tree (disco-room--message-reactions msg)))
         (next '()))
    (dolist (reaction reactions)
      (unless (disco-room--reaction-matches-input-p reaction emoji)
        (push reaction next)))
    (setf (alist-get 'reactions updated nil 'remove) (nreverse next))
    updated))

(defun disco-room--apply-live-reaction-event-partially (event)
  "Apply reaction EVENT to local room state and EWOC incrementally.

Return non-nil when a local message update was applied."
  (let* ((event-type (plist-get event :type))
         (message-id (plist-get event :message-id))
         (emoji-input (disco-room--event-emoji->input (plist-get event :emoji))))
    (pcase event-type
      ('message-reaction-add
       (and (stringp emoji-input)
            (disco-room--update-message-locally
             message-id
             (lambda (msg)
               (disco-room--message-with-reaction-delta msg emoji-input t)))))
      ('message-reaction-remove
       (and (stringp emoji-input)
            (disco-room--update-message-locally
             message-id
             (lambda (msg)
               (disco-room--message-with-reaction-delta msg emoji-input nil)))))
      ('message-reaction-remove-all
       (disco-room--update-message-locally
        message-id
        #'disco-room--message-cleared-reactions))
      ('message-reaction-remove-emoji
       (and (stringp emoji-input)
            (disco-room--update-message-locally
             message-id
             (lambda (msg)
               (disco-room--message-removed-reaction-emoji msg emoji-input)))))
      (_ nil))))

(defun disco-room--reply-reference-id (msg)
  "Return referenced message ID for reply MSG, or nil."
  (when (disco-room--message-reply-type-p msg)
    (or (and (listp (alist-get 'referenced_message msg))
             (alist-get 'id (alist-get 'referenced_message msg)))
        (disco-room--message-reference-id msg))))

(defun disco-room--reply-preview (msg)
  "Return one-line preview string of MSG reply target, or nil."
  (when (disco-room--message-reply-type-p msg)
    (let* ((ref (alist-get 'referenced_message msg))
           (ref-id (or (and (listp ref) (alist-get 'id ref))
                       (disco-room--reply-reference-id msg)))
           (resolved (or (and (listp ref) ref)
                         (and ref-id (disco-room--message-by-id ref-id)))))
      (when ref-id
        (if resolved
            (let* ((author (disco-room--message-author resolved))
                   (content (disco-room--message-display-content resolved)))
              (format "%s: %s" author (truncate-string-to-width content 72 nil nil t)))
          (format "Original message unavailable (%s)" ref-id))))))

(defun disco-room--insert-reference-jump-button (message-id channel-id label &optional face)
  "Insert jump button for MESSAGE-ID in CHANNEL-ID with LABEL.

When FACE is non-nil, use it for button text."
  (disco-ui-insert-action-button
   label
   (lambda ()
     (disco-room-jump-to-message message-id channel-id))
   :face (or face 'shadow)
   :help-echo (if (and (stringp channel-id)
                       (not (string-empty-p channel-id))
                       (not (equal (disco-room--normalize-id channel-id)
                                   (disco-room--normalize-id disco-room--channel-id))))
                  (format "Open channel %s and jump to message %s" channel-id message-id)
                (format "Jump to message %s" message-id))))

(defun disco-room--insert-reply-preview-line (msg reply &optional prefix)
  "Insert one reply preview line for MSG with REPLY content.

PREFIX defaults to four spaces for room body alignment."
  (let ((line-start (point))
        (ref-id (disco-room--reply-reference-id msg))
        (ref-channel (disco-room--message-reference-channel-id msg)))
    (insert "↪ " reply)
    (when (and (stringp ref-id) (not (string-empty-p ref-id)))
      (insert " ")
      (disco-room--insert-reference-jump-button ref-id ref-channel "[Jump]"))
    (insert "\n")
    (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
    (add-text-properties line-start (point) '(face shadow))))

(defun disco-room--insert-forward-reference-line (msg &optional prefix)
  "Insert one compact forward-source jump line for MSG.

When PREFIX is non-nil, use it for line indentation."
  (when (disco-room--message-forwarded-p msg)
    (let ((ref-id (disco-room--message-reference-id msg))
          (ref-channel (disco-room--message-reference-channel-id msg)))
      (when (and (stringp ref-id) (not (string-empty-p ref-id)))
        (let ((line-start (point)))
          (insert "↪ ")
          (disco-room--insert-reference-jump-button
           ref-id ref-channel "[Jump to source]")
          (insert "\n")
          (disco-ui-apply-line-prefix line-start (point) (or prefix "    "))
          (add-text-properties line-start (point) '(face shadow)))))))

(defun disco-room--insert-forward-card (msg)
  "Insert one rich forwarded-message card for MSG."
  (let* ((ref-id (disco-room--message-reference-id msg))
         (ref-channel (disco-room--message-reference-channel-id msg))
         (source (disco-room--forward-source-context msg))
         (guild (plist-get source :guild))
         (guild-label (or (plist-get source :guild-label) "direct message"))
         (channel-label (or (plist-get source :channel-label) "unknown-channel"))
         (sent-at (disco-room--forward-snapshot-time-label msg))
         (content (disco-room--forward-snapshot-content msg))
         (prefix-state (disco-ui-card-prefix-state :face 'disco-room-forward-card-border)))
    (let ((title-start (point)))
      (insert "[forwarded message]" "\n")
      (disco-ui-apply-line-prefix title-start (point) prefix-state)
      (disco-ui-append-face title-start (point) 'disco-room-forward-card-title))
    (let ((source-start (point)))
      (insert "source: ")
      (when (and guild (listp guild))
        (disco-room--insert-forward-guild-icon guild)
        (insert " "))
      (insert guild-label)
      (insert " / " channel-label)
      (insert "\n")
      (disco-ui-apply-line-prefix source-start (point) prefix-state)
      (disco-ui-append-face source-start (point) 'disco-room-forward-card-meta))
    (when (and (stringp sent-at) (not (string-empty-p sent-at)))
      (let ((time-start (point)))
        (insert "sent: " sent-at "\n")
        (disco-ui-apply-line-prefix time-start (point) prefix-state)
        (disco-ui-append-face time-start (point) 'disco-room-forward-card-meta)))
    (when (and (stringp content) (not (string-empty-p content)))
      (let ((content-start (point)))
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        (disco-ui-apply-line-prefix content-start (point) prefix-state)))
    (when (and (stringp ref-id) (not (string-empty-p ref-id)))
      (let ((action-start (point)))
        (disco-room--insert-reference-jump-button
         ref-id ref-channel "[Jump to source]" 'disco-room-forward-card-action)
        (insert "\n")
        (disco-ui-apply-line-prefix action-start (point) prefix-state)
        (disco-ui-append-face action-start (point) 'disco-room-forward-card-meta)))))

(defun disco-room--insert-forward-section (msg &optional prefix)
  "Insert forwarded-message block for MSG when applicable.

When PREFIX is non-nil, use it for non-card fallback indentation."
  (when (disco-room--message-forwarded-p msg)
    (if disco-room-use-rich-forward-cards
        (disco-room--insert-forward-card msg)
      (disco-room--insert-forward-reference-line msg prefix))))

(defun disco-room--insert-message (msg)
  "Insert one message MSG in current buffer."
  (let* ((context (or (disco-room--message-render-context msg) '()))
         (compact (disco-util-json-true-p (plist-get context :compact)))
         (insert-date (plist-get context :insert-date))
         (insert-unread (disco-util-json-true-p (plist-get context :insert-unread)))
         (timestamp (disco-util-format-time (or (alist-get 'timestamp msg) "")))
         (short-time (disco-util-format-time-short (or (alist-get 'timestamp msg) "")))
         (author (disco-room--message-author msg))
         (author-face (disco-room--author-face msg))
         (content (disco-room--message-display-content msg))
         (reply (disco-room--reply-preview msg))
         (message-id (alist-get 'id msg))
         line-start
         author-start
         section-prefix-state)
    (when (and (stringp insert-date)
               (not (string-empty-p insert-date)))
      (disco-room--insert-date-separator-row insert-date))
    (when insert-unread
      (disco-room--insert-unread-divider-row))
    (setq line-start (point))
    (if compact
        (let* ((avatar-prefixes (disco-room--avatar-prefixes msg))
               (compact-prefix (or (plist-get avatar-prefixes :rest-body) "    ")))
          (setq section-prefix-state
                (disco-ui-make-prefix-state compact-prefix compact-prefix))
          (when reply
            (disco-room--insert-reply-preview-line msg reply section-prefix-state))
          (let ((content-start (point))
                (time-span nil))
            (unless (string-empty-p content)
              (insert content))
            (setq time-span
                  (disco-room--insert-right-aligned-text
                   short-time
                   'disco-room-timestamp))
            (when (and (stringp timestamp) (not (string-empty-p timestamp)))
              (add-text-properties
               (car time-span)
               (cdr time-span)
               (list 'help-echo timestamp)))
            (insert "\n")
            (disco-ui-apply-line-prefix content-start (point) section-prefix-state)))
      (let* ((avatar-prefixes (disco-room--avatar-prefixes msg))
             (header-prefix (or (plist-get avatar-prefixes :header) ""))
             (body-first-prefix (or (plist-get avatar-prefixes :first-body) "    "))
             (body-rest-prefix (or (plist-get avatar-prefixes :rest-body) "    ")))
        (setq section-prefix-state
              (disco-ui-make-prefix-state body-first-prefix body-rest-prefix))
        (let ((header-start (point)))
          (setq author-start (point))
          (insert author)
          (add-text-properties author-start (point) (list 'face author-face))
          (let ((time-span
                 (disco-room--insert-right-aligned-text
                  short-time
                  'disco-room-timestamp)))
            (when (and (stringp timestamp) (not (string-empty-p timestamp)))
              (add-text-properties
               (car time-span)
               (cdr time-span)
               (list 'help-echo timestamp))))
          (insert "\n")
          (disco-ui-apply-line-prefix header-start (point) header-prefix))
        (when reply
          (disco-room--insert-reply-preview-line msg reply section-prefix-state))
        (unless (string-empty-p content)
          (disco-room--insert-prefixed-lines section-prefix-state content))))
    (let ((disco-ui-card-indent-prefix-state section-prefix-state)
          (disco-ui-card-indent-prefix
           (disco-room--line-prefix-string section-prefix-state nil "    ")))
      (disco-room--insert-forward-section msg section-prefix-state)
      (when (disco-room--message-has-thread-p msg)
        (let* ((message-id (alist-get 'id msg))
               (thread (disco-room--thread-from-message msg))
               (target-thread-id (or (and (listp thread) (alist-get 'id thread))
                                     (and (stringp message-id) message-id)))
               (target-thread-name (or (and (listp thread) (alist-get 'name thread))
                                       (and (stringp message-id)
                                            (format "thread:%s" message-id)))))
          (let ((thread-start (point)))
            (if target-thread-id
                (disco-ui-insert-action-button
                 "[Open thread]"
                 (lambda ()
                   (disco-room-open
                    target-thread-id
                    (or target-thread-name target-thread-id)))
                 :face 'disco-room-message-meta
                 :help-echo "Open starter thread for this message")
              (insert (propertize "[Thread id unavailable]"
                                  'face 'shadow)))
            (insert "\n")
            (disco-ui-apply-line-prefix
             thread-start (point) section-prefix-state))))
      (disco-room--insert-message-attachments msg section-prefix-state)
      (disco-room--insert-message-embeds msg)
      (disco-room--insert-message-poll msg))
    (disco-room--insert-message-reactions msg section-prefix-state)
    (add-text-properties
     line-start
     (point)
     (list 'read-only t
           'front-sticky '(read-only)
           'disco-message-id message-id))))

(defun disco-room--ewoc-printer (msg)
  "EWOC pretty-printer for one room message MSG."
  (disco-room--insert-message msg))

(defun disco-room--input-footer-text (draft)
  "Build EWOC footer text containing room prompt with DRAFT.

Footer marks the editable input tail using `disco-room-input' property."
  (let ((typing-text (disco-room--typing-indicator-text))
        (prompt (propertize ">>> "
                            'read-only t
                            'field 'disco-room-prompt
                            'cursor-intangible t
                            'disco-room-prompt t
                            'front-sticky '(read-only field cursor-intangible)
                            'rear-nonsticky
                            '(read-only field cursor-intangible disco-room-input)))
        (input (if (string-empty-p draft)
                   "\n"
                 draft)))
    (concat "\n"
            (if (and (stringp typing-text)
                     (not (string-empty-p typing-text)))
                (propertize (concat typing-text "\n")
                            'read-only t
                            'face 'disco-room-typing-indicator
                            'front-sticky '(read-only)
                            'rear-nonsticky '(read-only))
              "")
            prompt
            (propertize input
                        'disco-room-input t
                        'read-only nil))))

(defun disco-room--bind-input-region-from-footer ()
  "Locate and bind editable input region from EWOC footer properties."
  (let ((input-start (text-property-any (point-min) (point-max) 'disco-room-input t)))
    (when input-start
      (let* ((probe-start (max (point-min) (- input-start 32)))
             (prompt-start (or (text-property-any probe-start input-start 'disco-room-prompt t)
                               (max (point-min) (1- input-start)))))
        (setq disco-room--input-prompt-marker (copy-marker prompt-start nil))
        (setq disco-room--input-marker (copy-marker input-start nil))
        (disco-room--apply-input-text-properties)))))

(defun disco-room--insert-message-node (msg)
  "Insert one message node for MSG at the end of room EWOC."
  (when disco-room--ewoc
    (let ((disco-room--rendering t)
          (inhibit-read-only t)
          (buffer-undo-list t))
      (let* ((node (ewoc-enter-last disco-room--ewoc msg))
             (message-id (and (listp msg) (alist-get 'id msg))))
        (when (and node message-id disco-room--message-node-table)
          (puthash message-id node disco-room--message-node-table))
        node))))

(defun disco-room--upsert-message-node (msg)
  "Insert or update EWOC node for message MSG.

Return non-nil when EWOC was updated."
  (let ((message-id (and (listp msg) (alist-get 'id msg))))
    (when (and message-id disco-room--ewoc disco-room--message-node-table)
      (let ((node (gethash message-id disco-room--message-node-table)))
        (if node
            (let* ((anchor-at-point
                    (or (get-text-property (point) 'disco-message-id)
                        (get-text-property (line-beginning-position)
                                           'disco-message-id)))
                   (snapshot (and anchor-at-point
                                  (disco-view-capture-position
                                   :anchor-property 'disco-message-id
                                   :preserve-window-start t)))
                   (disco-room--rendering t)
                   (inhibit-read-only t)
                   (buffer-undo-list t))
              (ewoc-set-data node msg)
              (ewoc-invalidate disco-room--ewoc node)
              (when snapshot
                (disco-view-restore-position snapshot))
              t)
          (and (disco-room--insert-message-node msg) t))))))

(defun disco-room--delete-message-node (message-id)
  "Delete EWOC node identified by MESSAGE-ID.

Return non-nil when a node is removed."
  (let ((node (and message-id
                   disco-room--message-node-table
                   (gethash message-id disco-room--message-node-table))))
    (when (and node disco-room--ewoc)
      (let ((disco-room--rendering t)
            (inhibit-read-only t)
            (buffer-undo-list t))
        (ewoc-delete disco-room--ewoc node)
        (remhash message-id disco-room--message-node-table)
        t))))

(defun disco-room--apply-live-message-event-partially (event)
  "Apply EVENT with EWOC-local message updates when possible.

Return non-nil when handled without full room rerender."
  (let* ((event-type (plist-get event :type))
         (event-message (plist-get event :message))
         (message-id (or (and (listp event-message) (alist-get 'id event-message))
                         (plist-get event :message-id)))
         (state-message (and message-id (disco-room--message-by-id message-id)))
         handled)
    (pcase event-type
      ('message-create
       (setq handled (disco-room--upsert-message-node
                      (or state-message event-message))))
      ('message-update
       (setq handled (and state-message
                          (disco-room--upsert-message-node state-message))))
      ('message-delete
       (setq handled (disco-room--delete-message-node message-id))))
    (when handled
      (disco-room--update-message-window-state
       (or (disco-state-messages disco-room--channel-id) '()))
      t)))

(defun disco-room-render ()
  "Render timeline for current room buffer."
  (let ((inhibit-read-only t)
        ;; Timeline redraws can be large (many image previews); do not
        ;; accumulate undo entries for background rendering.
        (buffer-undo-list t)
        (messages (disco-state-messages disco-room--channel-id))
        (draft (disco-room--current-draft))
        header-end
        (disco-room--avatar-fetch-budget
         (when (numberp disco-room-avatar-max-fetches-per-render)
           (max 0 disco-room-avatar-max-fetches-per-render)))
        (preview-fetch-budget
         (when (numberp disco-room-attachment-preview-max-fetches-per-render)
           (max 0 disco-room-attachment-preview-max-fetches-per-render))))
    (setq disco-room--rendering t)
    (disco-media-set-preview-fetch-budget preview-fetch-budget)
    (unwind-protect
        (progn
          (erase-buffer)
          (insert (format "Channel: %s%s\n"
                          disco-room--channel-name
                          (disco-room--thread-header-suffix)))
          (insert "g: refresh   M-<: older   s/n/p: search   r/e/d: reply/edit/delete   C-c C-g: jump msg-id   C-c C-w: toggle breakline   !/+/-: reactions   C-c C-p s/+/-/t/v/c/e: poll send/select/unselect/toggle/vote/remove/end   C-c C-f/C-F: attach/forward   C-c C-d: remove token   C-c C-x: clear attachments   C-c M-l/M-e/M-r: list/edit/reorder attachments   C-c C-t o: open message thread   C-c C-t: thread ops   RET/C-c C-c: send   TAB: @/# complete   C-c C-v: refetch avatars   type at >>>   M-p/M-n: history   q: quit")
          (when disco-room--refresh-in-flight
            (insert "   [refreshing...]"))
          (when disco-room--older-in-flight
            (insert "   [loading older...]"))
          (when disco-room--send-in-flight
            (insert "   [sending...]"))
          (insert "\n")
          (when disco-room--pending-reply-to
            (insert (format "Replying to: %s (C-c C-k to cancel)\n"
                            disco-room--pending-reply-to)))
          (when disco-room--pending-attachments
            (insert (format "Queued attachments: %s\n"
                            (mapconcat #'identity
                                       (disco-room--pending-attachment-labels)
                                       ", "))))
          (when disco-room--history-exhausted
            (insert "(older history exhausted)\n"))
          (insert "\n")
          (setq header-end (point))
          (put-text-property (point-min) header-end 'read-only t)
          (setq disco-room--input-marker nil)
          (setq disco-room--input-prompt-marker nil)
          (setq disco-room--message-node-table (make-hash-table :test #'equal))
          (setq disco-room--ewoc
                (ewoc-create
                 #'disco-room--ewoc-printer
                 nil
                 (disco-room--input-footer-text draft)
                 t))
          ;; API returns newest-first by default; reverse for chat-like display.
          (let* ((ordered (reverse messages))
                 (last-read-id (disco-state-channel-last-read-message-id disco-room--channel-id))
                 (previous-msg nil)
                 (previous-day nil)
                 (unread-divider-inserted nil))
            (setq disco-room--render-context-by-message-id (make-hash-table :test #'equal))
            (dolist (msg ordered)
              (let* ((message-id (alist-get 'id msg))
                     (day-key (disco-room--message-day-key msg))
                     (insert-date (and disco-room-show-date-separators
                                       (stringp day-key)
                                       (not (equal day-key previous-day))
                                       day-key))
                     (compact (and previous-msg
                                   (disco-room--messages-compact-group-p previous-msg msg)))
                     (insert-unread
                      (and disco-room-show-unread-divider
                           (not unread-divider-inserted)
                           (or (null last-read-id)
                               (and (stringp message-id)
                                    (stringp last-read-id)
                                    (disco-state-snowflake< last-read-id message-id)))))
                     (context (list :compact (and compact t)
                                    :insert-date insert-date
                                    :insert-unread (and insert-unread t))))
                (when message-id
                  (disco-room--set-message-render-context message-id context))
                (when insert-unread
                  (setq unread-divider-inserted t))
                (setq previous-msg msg)
                (setq previous-day day-key)
                (disco-room--insert-message-node msg))))
          (disco-room--bind-input-region-from-footer)
          (when (markerp disco-room--input-marker)
            ;; Keep typing position at end of draft text, not sentinel newline.
            (let ((logical-end (disco-room--input-logical-end-position)))
              (when logical-end
                (goto-char logical-end)))

            (disco-room--sync-draft-from-buffer)))
      (disco-media-set-preview-fetch-budget nil)
      (setq disco-room--rendering nil))))

(defun disco-room-refresh ()
  "Fetch and redraw latest messages for current room asynchronously."
  (interactive)
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (generation (1+ disco-room--refresh-generation)))
    (setq disco-room--refresh-generation generation)
    (setq disco-room--refresh-in-flight t)
    (disco-api-channel-messages-async
     channel-id
     :on-success
     (lambda (messages)
       (when (disco-room--callback-active-p room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq disco-room--history-exhausted nil)
           (disco-state-put-messages channel-id messages)
           (disco-room--update-message-window-state messages)
           (disco-room--mark-read)
           (setq disco-room--refresh-in-flight nil)
           (if (disco-room--at-message-bottom-p)
               (disco-room-render)
             (disco-room--render-preserving-point))
           (disco-room--resolve-pending-jump)
           (message "disco: loaded %d messages" (length messages)))))
     :on-error
     (lambda (err)
       (when (disco-room--callback-active-p room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq disco-room--refresh-in-flight nil)
           (message "disco: room refresh failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room--close-for-deleted-channel (reason)
  "Close current room because its backing channel is no longer valid.

REASON is shown in the minibuffer."
  (let ((buf (current-buffer)))
    (disco-room--detach-live-updates)
    (kill-buffer buf)
    (message "%s" reason)))

(defun disco-room--handle-gateway-event (event)
  "Handle one EVENT plist from `disco-gateway-event-hook'."
  (let ((event-type (plist-get event :type))
        (event-channel-id (plist-get event :channel-id))
        (event-guild-id (plist-get event :guild-id)))
    (cond
     ((and (memq event-type '(channel-delete thread-delete))
           (equal event-channel-id disco-room--channel-id))
      (disco-room--close-for-deleted-channel
       (format "disco: channel %s was deleted"
               (or disco-room--channel-name disco-room--channel-id))))
     ((and (eq event-type 'guild-delete)
           disco-room--guild-id
           (equal event-guild-id disco-room--guild-id))
      (disco-room--close-for-deleted-channel
       (format "disco: guild for channel %s was deleted"
               (or disco-room--channel-name disco-room--channel-id))))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(channel-update thread-update)))
      (let ((at-bottom (disco-room--at-message-bottom-p))
            (channel (disco-room--channel-object)))
        (when (and channel (alist-get 'name channel))
          (setq disco-room--channel-name (alist-get 'name channel)))
        (if at-bottom
            (disco-room-render)
          (disco-room--render-preserving-point))))
     ((and (equal event-channel-id disco-room--channel-id)
           (eq event-type 'typing-start))
      (disco-room--typing-track-user
       (plist-get event :user-id)
       (plist-get event :member)
       (plist-get event :timestamp)))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-create message-update message-delete)))
      (let* ((message (and (eq event-type 'message-create)
                           (plist-get event :message)))
             (author (and (listp message) (alist-get 'author message)))
             (author-id (and (listp author) (alist-get 'id author)))
             (message-id (and (listp message) (alist-get 'id message)))
             (at-bottom (disco-room--at-message-bottom-p)))
        (when author-id
          ;; Message arrival implicitly ends visible typing state for sender.
          (disco-room--typing-stop-user author-id t))
        ;; Message grouping/date/unread layout depends on surrounding rows,
        ;; so message create/update/delete keeps a full room rerender.
        (if at-bottom
            (disco-room-render)
          (disco-room--render-preserving-point))
        (when (eq event-type 'message-create)
          (disco-room--mark-read message-id))))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-reaction-add
                              message-reaction-remove
                              message-reaction-remove-all
                              message-reaction-remove-emoji)))
      (unless (disco-room--apply-live-reaction-event-partially event)
        (disco-room-refresh)))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-poll-vote-add
                              message-poll-vote-remove)))
      (unless (disco-room--apply-live-poll-vote-event-partially event)
        (disco-room-refresh))))))

(defun disco-room--attach-live-updates ()
  "Attach this room buffer to live update event stream."
  (when disco-room--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-room--gateway-handler)
    (when disco-room--channel-id
      (disco-gateway-unwatch-channel disco-room--channel-id)))
  (let ((room-buffer (current-buffer)))
    (setq disco-room--gateway-handler
          (lambda (event)
            (when (buffer-live-p room-buffer)
              (with-current-buffer room-buffer
                (disco-room--handle-gateway-event event))))))
  (add-hook 'disco-gateway-event-hook disco-room--gateway-handler)
  (disco-gateway-watch-channel disco-room--channel-id)
  (add-hook 'kill-buffer-hook #'disco-room--detach-live-updates nil t))

(defun disco-room--detach-live-updates ()
  "Detach this room buffer from live update event stream."
  (when disco-room--gateway-handler
    (remove-hook 'disco-gateway-event-hook disco-room--gateway-handler)
    (setq disco-room--gateway-handler nil))
  (when disco-room--channel-id
    (disco-gateway-unwatch-channel disco-room--channel-id))
  (disco-room--typing-reset))

(defun disco-room--pending-attachment-labels ()
  "Return compact filename labels for pending composer attachments."
  (let ((labels
         (mapcar
          (lambda (item)
            (let* ((token-id (plist-get item :token-id))
                   (path (or (plist-get item :path) ""))
                   (filename (if (string-empty-p path)
                                 "missing"
                               (file-name-nondirectory path)))
                   (description (or (plist-get item :description) ""))
                   (token-label (if token-id
                                    (disco-room--attachment-token-text token-id)
                                  "[file:?]")))
              (if (string-empty-p description)
                  (format "%s %s" token-label filename)
                (format "%s %s - %s" token-label filename description))))
          (or disco-room--pending-attachments '()))))
    (if (> (length labels) 3)
        (append (seq-take labels 3)
                (list (format "+%d more" (- (length labels) 3))))
      labels)))

(defun disco-room--append-attachment-token-to-draft (token-id)
  "Append attachment TOKEN-ID marker to current draft input."
  (let* ((token-text (disco-room--attachment-token-text token-id))
         (draft (disco-room--current-draft))
         (separator (if (or (string-empty-p draft)
                            (string-match-p "[ \t\n]\\'" draft))
                        ""
                      " ")))
    (disco-room--set-draft (concat draft separator token-text))))

(defun disco-room--remove-first-token-from-draft (draft token-id)
  "Return DRAFT with first TOKEN-ID marker removed."
  (let* ((token-text (disco-room--attachment-token-text token-id))
         (regexp (regexp-quote token-text)))
    (if (string-match regexp draft)
        (concat (substring draft 0 (match-beginning 0))
                (substring draft (match-end 0)))
      draft)))

(defun disco-room--attachment-token-choices (&optional draft)
  "Return completion candidates for attachment tokens in DRAFT.

Each candidate is a cons cell (LABEL . TOKEN-ID)."
  (let ((text (or draft (disco-room--current-draft)))
        (out '()))
    (dolist (token-id (disco-room--attachment-token-ids-in-text text))
      (let* ((attachment (disco-room--attachment-by-token-id token-id))
             (path (or (plist-get attachment :path) ""))
             (filename (if (string-empty-p path)
                           "missing"
                         (file-name-nondirectory path)))
             (description (or (plist-get attachment :description) ""))
             (label (if (string-empty-p description)
                        (format "%s %s"
                                (disco-room--attachment-token-text token-id)
                                filename)
                      (format "%s %s - %s"
                              (disco-room--attachment-token-text token-id)
                              filename
                              description))))
        (push (cons label token-id) out)))
    (nreverse out)))

(defun disco-room--choose-attachment-token-id (prompt)
  "Prompt for one attachment token id with PROMPT."
  (let* ((choices (disco-room--attachment-token-choices))
         (labels (mapcar #'car choices))
         (picked (completing-read prompt labels nil t)))
    (or (cdr (assoc picked choices))
        (user-error "disco: invalid attachment token selection"))))

(defun disco-room--rewrite-draft-attachment-token-order (ordered-token-ids)
  "Rewrite current draft so ORDERED-TOKEN-IDS becomes the token sequence.

The free text body is preserved, all token markers are removed, then token
markers are appended in requested order."
  (let* ((text-only (string-trim-right
                     (disco-room--draft-without-attachment-tokens
                      (disco-room--current-draft))))
         (token-tail (mapconcat #'disco-room--attachment-token-text ordered-token-ids " "))
         (next
          (cond
           ((and (not (string-empty-p text-only))
                 (not (string-empty-p token-tail)))
            (format "%s %s" text-only token-tail))
           ((not (string-empty-p text-only)) text-only)
           (t token-tail))))
    (disco-room--set-draft next)))

(defun disco-room--attachment-token-bounds-at-point ()
  "Return bounds of attachment token around point in input region, or nil."
  (let ((bounds (disco-room--input-region-bounds))
        (pos (point))
        found)
    (when (and bounds (disco-room--point-in-input-p pos))
      (save-excursion
        (goto-char (car bounds))
        (while (and (not found)
                    (re-search-forward disco-room--attachment-token-regexp (cdr bounds) t))
          (when (and (<= (match-beginning 0) pos)
                     (<= pos (match-end 0)))
            (setq found (cons (match-beginning 0) (match-end 0))))))
      found)))

(defun disco-room-remove-attachment-token-at-point ()
  "Remove attachment token at point, or prompt for one when point is outside token."
  (interactive)
  (let ((token-bounds (disco-room--attachment-token-bounds-at-point)))
    (cond
     (token-bounds
      (let* ((token-text (buffer-substring-no-properties (car token-bounds) (cdr token-bounds)))
             (token-id (and (string-match disco-room--attachment-token-regexp token-text)
                            (match-string 1 token-text))))
        (delete-region (car token-bounds) (cdr token-bounds))
        (disco-room--sync-draft-from-buffer)
        (when token-id
          (remhash token-id disco-room--attachment-token-table))
        (disco-room-render)
        (message "disco: removed attachment token %s" (or token-id ""))))
     (t
      (let* ((ids (disco-room--attachment-token-ids-in-text (disco-room--current-draft)))
             (picked (and ids
                          (completing-read "Remove attachment token: " ids nil t))))
        (unless (and (stringp picked) (not (string-empty-p picked)))
          (user-error "disco: no attachment token at point"))
        (let ((updated (disco-room--remove-first-token-from-draft
                        (disco-room--current-draft)
                        picked)))
          (remhash picked disco-room--attachment-token-table)
          (disco-room--set-draft updated)
          (message "disco: removed attachment token %s" picked)))))))

(defun disco-room-list-attachments ()
  "List queued attachment tokens for current draft."
  (interactive)
  (let ((choices (disco-room--attachment-token-choices)))
    (if (null choices)
        (message "disco: no queued attachments")
      (message "disco: %s" (mapconcat #'car choices " | ")))))

(defun disco-room-edit-attachment-description ()
  "Edit description of one queued attachment token."
  (interactive)
  (let ((token-id (disco-room--choose-attachment-token-id "Edit attachment token: ")))
    (let* ((entry (copy-tree (or (disco-room--attachment-by-token-id token-id)
                                 (user-error "disco: token %s not found" token-id))))
           (current (or (plist-get entry :description) ""))
           (next-input (read-string
                        (format "Description for %s (empty clears): "
                                (disco-room--attachment-token-text token-id))
                        current))
           (next (string-trim next-input)))
      (setq entry (plist-put entry :description (unless (string-empty-p next) next)))
      (puthash token-id entry disco-room--attachment-token-table)
      (disco-room--sync-pending-attachments-from-draft)
      (disco-room-render)
      (if (string-empty-p next)
          (message "disco: cleared description for %s" token-id)
        (message "disco: updated description for %s" token-id)))))

(defun disco-room-reorder-attachments ()
  "Reorder one queued attachment token in the current draft."
  (interactive)
  (let* ((ids (disco-room--attachment-token-ids-in-text (disco-room--current-draft)))
         (count (length ids)))
    (when (< count 2)
      (user-error "disco: need at least two attachments to reorder"))
    (let* ((token-id (disco-room--choose-attachment-token-id "Move attachment token: "))
           (current-index (or (cl-position token-id ids :test #'equal)
                              (user-error "disco: token %s not found in draft" token-id)))
           (target-index-input
            (read-number
             (format "Move %s from %d to position (1-%d): "
                     (disco-room--attachment-token-text token-id)
                     (1+ current-index)
                     count)
             (1+ current-index)))
           (target-index (max 0 (min (1- count) (1- target-index-input))))
           (without-token (seq-remove (lambda (id) (equal id token-id)) ids))
           (prefix (seq-take without-token target-index))
           (suffix (seq-drop without-token target-index))
           (next-order (append prefix (list token-id) suffix)))
      (disco-room--rewrite-draft-attachment-token-order next-order)
      (message "disco: moved %s to position %d"
               (disco-room--attachment-token-text token-id)
               (1+ target-index)))))

(defun disco-room--message-id-required-at-point ()
  "Return message ID at point, or signal user error."
  (or (disco-room--message-id-at-point)
      (user-error "disco: point is not on a message")))

(defun disco-room--default-reaction-emoji (msg)
  "Return best default reaction emoji suggestion from MSG."
  (let* ((reactions (disco-room--message-reactions msg))
         (selected (seq-find #'disco-room--reaction-selected-p reactions))
         (candidate (or selected (car reactions))))
    (or (and candidate (disco-room--reaction-emoji candidate))
        "👍")))

(defun disco-room--read-reaction-emoji (prompt &optional default)
  "Prompt for emoji with PROMPT and DEFAULT fallback."
  (let* ((raw (read-string
               (if default
                   (format "%s (default %s): " prompt default)
                 (format "%s: " prompt))
               nil nil default))
         (emoji (string-trim raw)))
    (if (string-empty-p emoji)
        (or default (user-error "disco: emoji cannot be empty"))
      emoji)))

(defun disco-room-add-reaction (&optional emoji message-id)
  "Add EMOJI reaction to MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message")))
          (default (disco-room--default-reaction-emoji msg))
          (picked (disco-room--read-reaction-emoji "Add reaction" default)))
     (list picked (alist-get 'id msg))))
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (emoji-text emoji))
    (disco-api-add-reaction-async
     channel-id
     target-id
     emoji-text
     :on-success
     (lambda (_response)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (disco-room--update-message-locally
            target-id
            (lambda (msg)
              (disco-room--message-with-reaction-delta msg emoji-text t)))
           (message "disco: reaction added (%s)" emoji-text))))
     :on-error
     (lambda (err)
       (message "disco: add reaction failed: %s"
                (disco-room--async-error-message err))))))

(defun disco-room-remove-reaction (&optional emoji message-id)
  "Remove current user's EMOJI reaction from MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message")))
          (default (disco-room--default-reaction-emoji msg))
          (picked (disco-room--read-reaction-emoji "Remove reaction" default)))
     (list picked (alist-get 'id msg))))
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (emoji-text emoji))
    (disco-api-remove-own-reaction-async
     channel-id
     target-id
     emoji-text
     :on-success
     (lambda (_response)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (disco-room--update-message-locally
            target-id
            (lambda (msg)
              (disco-room--message-with-reaction-delta msg emoji-text nil)))
           (message "disco: reaction removed (%s)" emoji-text))))
     :on-error
     (lambda (err)
       (message "disco: remove reaction failed: %s"
                (disco-room--async-error-message err))))))

(defun disco-room-toggle-reaction (&optional emoji message-id)
  "Toggle current user's EMOJI reaction on MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message")))
          (default (disco-room--default-reaction-emoji msg))
          (picked (disco-room--read-reaction-emoji "Toggle reaction" default)))
     (list picked (alist-get 'id msg))))
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (msg (or (disco-room--message-by-id target-id)
                  (disco-room--message-at-point)
                  (user-error "disco: message not found in room state"))))
    (if (disco-room--message-has-own-reaction-p msg emoji)
        (disco-room-remove-reaction emoji target-id)
      (disco-room-add-reaction emoji target-id))))

(defun disco-room--poll-message-required (&optional message-id)
  "Return poll message object by MESSAGE-ID or point, or raise user error."
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (msg (or (disco-room--message-by-id target-id)
                  (user-error "disco: message not found in room state")))
         (poll (disco-room--message-poll msg)))
    (unless poll
      (user-error "disco: message %s has no poll" target-id))
    msg))

(defun disco-room--poll-answer-id-at-point ()
  "Return poll answer id text property at point, or nil."
  (let ((raw (or (get-text-property (point) 'disco-poll-answer-id)
                 (save-excursion
                   (beginning-of-line)
                   (get-text-property (point) 'disco-poll-answer-id)))))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

(defun disco-room--poll-answer-choices (msg)
  "Return completion choices for poll answers in MSG.

Each item is (LABEL . ANSWER-ID)."
  (let* ((poll (or (disco-room--message-poll msg) '()))
         (answers (or (alist-get 'answers poll) '()))
         out)
    (dolist (answer answers (nreverse out))
      (let ((answer-id (disco-room--poll-answer-id answer)))
        (when answer-id
          (let* ((emoji (disco-room--poll-answer-emoji answer))
                 (text (disco-room--poll-answer-text answer))
                 (label (format "%d: %s%s"
                                answer-id
                                (if emoji (concat emoji " ") "")
                                text)))
            (push (cons label answer-id) out)))))))

(defun disco-room--read-poll-answer-id (msg &optional default)
  "Prompt poll answer id for MSG, using DEFAULT answer id when provided."
  (let* ((choices (disco-room--poll-answer-choices msg))
         (labels (mapcar #'car choices))
         (default-label (and default
                             (car (rassoc default choices))))
         (picked (completing-read
                  (if default-label
                      (format "Poll answer (default %s): " default-label)
                    "Poll answer: ")
                  labels
                  nil
                  t
                  nil
                  nil
                  default-label)))
    (or (cdr (assoc picked choices))
        default
        (user-error "disco: invalid poll answer"))))

(defun disco-room--submit-poll-vote (message-id selected-answer-ids)
  "Submit SELECTED-ANSWER-IDS for poll MESSAGE-ID asynchronously."
  (let* ((msg (disco-room--poll-message-required message-id))
         (poll (disco-room--message-poll msg))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (target-id (alist-get 'id msg))
         (normalized (disco-room--poll-normalize-answer-id-list selected-answer-ids)))
    (when (disco-room--poll-expired-p poll)
      (user-error "disco: poll is closed"))
    (disco-room--ensure-channel-permissions
     (disco-room--poll-vote-required-permissions)
     :action "poll voting")
    (disco-api-create-poll-vote-async
     channel-id
     target-id
     normalized
     :on-success
     (lambda (_response)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (disco-room--poll-clear-draft-selection target-id)
           (disco-room--update-message-locally
            target-id
            (lambda (message)
              (disco-room--message-with-poll-vote-selection message normalized)))
           (message "disco: poll vote updated"))))
     :on-error
     (lambda (err)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (message "disco: poll vote failed: %s"
                  (disco-room--async-error-message err)))))))

(defun disco-room--pick-poll-answer-id (msg &optional explicit-answer-id)
  "Return poll answer id from EXPLICIT-ANSWER-ID, point, or prompt for MSG."
  (or explicit-answer-id
      (disco-room--poll-answer-id-at-point)
      (disco-room--read-poll-answer-id msg nil)))

(defun disco-room--stage-poll-selection (message-id selection)
  "Stage poll SELECTION for MESSAGE-ID and rerender room buffer."
  (disco-room--poll-set-draft-selection message-id selection)
  (disco-room--render-preserving-point))

(defun disco-room-vote-poll-answer (&optional answer-id message-id)
  "Stage ANSWER-ID as selected for poll MESSAGE-ID.

In single-select polls, this replaces the staged selection."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-room--message-poll msg))
         (picked (disco-room--pick-poll-answer-id msg answer-id)))
    (unless (disco-room--poll-can-vote-p msg)
      (user-error "disco: poll voting is unavailable here"))
    (disco-room--stage-poll-selection
     target-id
     (disco-room--poll-add-selection target-id poll picked))))

(defun disco-room-remove-poll-vote (&optional answer-id message-id)
  "Stage removal of ANSWER-ID vote from poll MESSAGE-ID."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-room--message-poll msg))
         (picked (disco-room--pick-poll-answer-id msg answer-id))
         (current (disco-room--poll-effective-selection target-id poll)))
    (unless (disco-room--poll-can-vote-p msg)
      (user-error "disco: poll voting is unavailable here"))
    (unless (member picked current)
      (user-error "disco: answer %s is not selected" picked))
    (disco-room--stage-poll-selection
     target-id
     (delete picked (copy-sequence current)))))

(defun disco-room-toggle-poll-answer (&optional answer-id message-id)
  "Toggle staged poll ANSWER-ID in MESSAGE-ID.

This only updates local staged selection. Use `disco-room-submit-poll-vote' to
send votes to Discord."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-room--message-poll msg))
         (picked (disco-room--pick-poll-answer-id msg answer-id)))
    (unless (disco-room--poll-can-vote-p msg)
      (user-error "disco: poll voting is unavailable here"))
    (disco-room--stage-poll-selection
     target-id
     (disco-room--poll-toggle-draft-selection target-id poll picked))))

(defun disco-room-submit-poll-vote (&optional message-id)
  "Submit staged poll selection for MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-room--message-poll msg))
         (staged (disco-room--poll-effective-selection target-id poll))
         (committed (disco-room--poll-voted-answer-ids poll)))
    (unless (disco-room--poll-can-vote-p msg)
      (user-error "disco: poll voting is unavailable here"))
    (when (null staged)
      (user-error "disco: select at least one answer before voting"))
    (when (equal (disco-room--poll-normalize-answer-id-list staged)
                 (disco-room--poll-normalize-answer-id-list committed))
      (user-error "disco: no pending poll vote changes"))
    (disco-room--submit-poll-vote target-id staged)))

(defun disco-room-clear-poll-votes (&optional message-id)
  "Remove all current-user votes for poll MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-room--message-poll msg))
         (committed (disco-room--poll-voted-answer-ids poll)))
    (unless (disco-room--poll-can-vote-p msg)
      (user-error "disco: poll voting is unavailable here"))
    (unless committed
      (user-error "disco: no existing poll vote to remove"))
    (disco-room--submit-poll-vote target-id '())))

(defun disco-room-expire-poll (&optional message-id)
  "End poll in MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (poll (disco-room--message-poll msg))
         (target-id (alist-get 'id msg))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id))
    (when (disco-room--poll-expired-p poll)
      (user-error "disco: poll is already closed"))
    (unless (disco-room--poll-owned-by-current-user-p msg nil)
      (user-error "disco: only poll author can end this poll"))
    (disco-room--ensure-channel-permissions
     (disco-room--poll-expire-required-permissions)
     :action "ending polls")
    (when (or (not disco-room-poll-confirm-expire)
              (y-or-n-p (format "End poll %s now? " target-id)))
      (disco-api-expire-poll-async
       channel-id
       target-id
       :on-success
       (lambda (_response)
         (when (disco-room--channel-buffer-p room-buffer channel-id)
           (with-current-buffer room-buffer
             (disco-room-refresh)
             (message "disco: poll ended"))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id)
           (message "disco: end poll failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room--split-csv-values (raw)
  "Split comma-separated RAW string into trimmed non-empty values."
  (let ((trimmed (string-trim (or raw ""))))
    (unless (string-empty-p trimmed)
      (mapcar #'string-trim
              (split-string trimmed "," t "[[:space:]]*")))))

(defun disco-room--parse-forward-embed-indices (raw)
  "Parse RAW comma-separated embed indices into a vector or nil."
  (let ((tokens (disco-room--split-csv-values raw))
        values)
    (dolist (token tokens)
      (unless (string-match-p "\\`[0-9]+\\'" token)
        (user-error "disco: forward embed indices must be non-negative integers"))
      (push (string-to-number token) values))
    (when values
      (vconcat (nreverse values)))))

(defun disco-room--parse-forward-attachment-ids (raw)
  "Parse RAW comma-separated attachment ids into a vector or nil."
  (let ((tokens (disco-room--split-csv-values raw)))
    (when tokens
      (vconcat tokens))))

(defun disco-room--forward-source-message (source-channel-id message-id)
  "Resolve SOURCE-CHANNEL-ID/MESSAGE-ID to a message object, or nil."
  (let ((channel-id (disco-room--normalize-id source-channel-id))
        (target-id (disco-room--normalize-id message-id)))
    (when (and channel-id target-id)
      (or (seq-find (lambda (msg)
                      (equal (disco-room--normalize-id (alist-get 'id msg))
                             target-id))
                    (or (disco-state-messages channel-id) '()))
          (condition-case _
              (disco-api-channel-message channel-id target-id)
            (error nil))))))

(defun disco-room--forward-only-select-values (prompt choices)
  "Read one or more values from CHOICES with PROMPT.

CHOICES is an alist of (LABEL . VALUE). Empty input means no selection."
  (if (null choices)
      nil
    (let* ((labels (mapcar #'car choices))
           (picked (completing-read-multiple
                    (format "%s (RET to skip)" prompt)
                    labels
                    nil
                    t
                    nil
                    nil
                    ""))
           (normalized
            (delq nil
                  (mapcar (lambda (label)
                            (let ((text (string-trim (or label ""))))
                              (unless (string-empty-p text)
                                text)))
                          picked)))
           values)
      (dolist (label normalized)
        (let ((entry (assoc label choices)))
          (unless entry
            (user-error "disco: invalid forward-only selection `%s'" label))
          (push (cdr entry) values)))
      (delete-dups (nreverse values)))))

(defun disco-room--read-forward-only-manual ()
  "Read `forward_only' payload by manual embed-index/attachment-id input."
  (let* ((embed-input
          (read-string "Embed indices (comma-separated, empty for none): "))
         (attachment-input
          (read-string "Attachment IDs (comma-separated, empty for none): "))
         (embed-indices (disco-room--parse-forward-embed-indices embed-input))
         (attachment-ids (disco-room--parse-forward-attachment-ids
                          attachment-input))
         payload)
    (when embed-indices
      (push `(embed_indices . ,embed-indices) payload))
    (when attachment-ids
      (push `(attachment_ids . ,attachment-ids) payload))
    (unless payload
      (user-error "disco: forward-only selection requires embed indices or attachment ids"))
    (nreverse payload)))

(defun disco-room--read-forward-only-from-message (source-message)
  "Read `forward_only' payload by selecting embeds/attachments from SOURCE-MESSAGE."
  (let* ((embeds (or (alist-get 'embeds source-message) '()))
         (attachments (or (alist-get 'attachments source-message) '()))
         (embed-choices nil)
         (attachment-choices nil)
         (idx 0)
         payload)
    (dolist (embed embeds)
      (let* ((kind (or (alist-get 'type embed) "embed"))
             (title (string-trim (or (alist-get 'title embed)
                                     (alist-get 'description embed)
                                     (alist-get 'url embed)
                                     "(no title)")))
             (label (format "#%d [%s] %s" idx kind title)))
        (push (cons label idx) embed-choices)
        (setq idx (1+ idx))))
    (setq embed-choices (nreverse embed-choices))
    (dolist (attachment attachments)
      (let* ((attachment-id (disco-room--normalize-id (alist-get 'id attachment)))
             (filename (or (alist-get 'filename attachment) "(unnamed)"))
             (label (and attachment-id
                         (format "%s %s" attachment-id filename))))
        (when (and label attachment-id)
          (push (cons label attachment-id) attachment-choices))))
    (setq attachment-choices (nreverse attachment-choices))
    (unless (or embed-choices attachment-choices)
      (user-error "disco: source message has no embeds or attachments to subset"))
    (let ((picked-embed-indices
           (disco-room--forward-only-select-values
            "Pick embeds to forward (comma list): "
            embed-choices))
          (picked-attachment-ids
           (disco-room--forward-only-select-values
            "Pick attachments to forward (comma list): "
            attachment-choices)))
      (when picked-embed-indices
        (push `(embed_indices . ,(vconcat picked-embed-indices)) payload))
      (when picked-attachment-ids
        (push `(attachment_ids . ,(vconcat picked-attachment-ids)) payload))
      (unless payload
        (user-error "disco: forward-only selection is empty"))
      (nreverse payload))))

(defun disco-room--read-forward-only (&optional source-channel-id message-id)
  "Read optional forward_only selection from minibuffer prompts."
  (when (y-or-n-p "Forward only selected embeds/attachments? ")
    (let ((source-message (disco-room--forward-source-message
                           source-channel-id
                           message-id)))
      (if source-message
          (condition-case err
              (disco-room--read-forward-only-from-message source-message)
            (error
             (if disco-room-forward-only-manual-fallback
                 (progn
                   (message "disco: source-based forward-only failed (%s); using manual entry"
                            (error-message-string err))
                   (disco-room--read-forward-only-manual))
               (user-error "disco: source-based forward-only failed: %s"
                           (error-message-string err)))))
        (if disco-room-forward-only-manual-fallback
            (progn
              (message "disco: source message unavailable; using manual forward-only entry")
              (disco-room--read-forward-only-manual))
          (user-error "disco: source message unavailable for forward-only"))))))

(defun disco-room--send-allowed-mentions (&optional replying-p)
  "Return normalized allowed_mentions payload for outgoing message send/edit.

When REPLYING-P is non-nil and `disco-room-reply-mention-replied-user' is
enabled, include `replied_user'."
  (let ((base
         (pcase disco-room-allowed-mentions
           ('none '((parse . [])))
           ('all '((parse . ["users" "roles" "everyone"])))
           ((pred listp) (copy-tree disco-room-allowed-mentions))
           (_ nil))))
    (when (and replying-p disco-room-reply-mention-replied-user)
      (let ((value t))
        (if (listp base)
            (let ((cell (or (assq 'replied_user base)
                            (assq 'replied-user base))))
              (if cell
                  (setcdr cell value)
                (setq base (append base `((replied_user . ,value))))))
          (setq base `((replied_user . ,value))))))
    base))

(defun disco-room-send-poll (question options &optional duration allow-multiselect content)
  "Create and send a poll with QUESTION and OPTIONS in current room.

DURATION is in hours. ALLOW-MULTISELECT toggles multi-select behavior.
CONTENT is optional extra text sent alongside the poll."
  (interactive
   (let* ((question-input (string-trim (read-string "Poll question: ")))
          (duration-input (read-number "Poll duration (hours): "
                                       disco-room-poll-default-duration-hours))
          (allow-multi (y-or-n-p "Allow multiple answers? "))
          (content-input (string-trim (read-string "Optional message content: ")))
          (max-options (max 2 disco-room-poll-max-options))
          (idx 1)
          (options nil)
          opt)
     (while (and (<= idx max-options)
                 (not (string-empty-p
                       (setq opt (string-trim
                                  (read-string
                                   (format "Option %d (empty to finish): " idx)))))))
       (push opt options)
       (setq idx (1+ idx)))
     (unless (and (stringp question-input)
                  (not (string-empty-p question-input)))
       (user-error "disco: poll question cannot be empty"))
     (unless (>= (length options) 2)
       (user-error "disco: poll requires at least 2 options"))
     (list question-input
           (nreverse options)
           duration-input
           allow-multi
           (unless (string-empty-p content-input)
             content-input))))
  (let* ((poll `((question . ((text . ,question)))
                 (answers . ,(mapcar (lambda (option)
                                       `((poll_media . ((text . ,option)))) )
                                     options))
                 (duration . ,duration)
                 (allow_multiselect . ,(if allow-multiselect t :false))))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (required-permissions
          (append (disco-room--required-send-permissions)
                  '(send-polls))))
    (disco-room--ensure-channel-permissions
     required-permissions
     :action "sending poll")
    (setq disco-room--send-in-flight t)
    (disco-room-render)
    (disco-api-create-message-async
     channel-id
     :content content
     :poll poll
     :allowed-mentions (disco-room--send-allowed-mentions)
     :on-success
     (lambda (_response)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (setq disco-room--send-in-flight nil)
           (disco-room-refresh)
           (message "disco: poll sent"))))
     :on-error
     (lambda (err)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (setq disco-room--send-in-flight nil)
           (disco-room-render)
           (message "disco: send poll failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room-attach-file (path &optional description)
  "Queue attachment PATH for next room send.

DESCRIPTION is optional per-file description."
  (interactive
   (let* ((path (read-file-name "Attach file: " nil nil t))
          (description-input (string-trim (read-string "Attachment description (optional): ")))
          (description (unless (string-empty-p description-input)
                         description-input)))
     (list path description)))
  (unless (file-readable-p path)
    (user-error "disco: file is not readable: %s" path))
  (let* ((token-id (disco-room--next-attachment-token-id))
         (entry (list :token-id token-id
                      :path path
                      :description description)))
    (puthash token-id entry disco-room--attachment-token-table)
    (disco-room--append-attachment-token-to-draft token-id)
    (message "disco: queued attachment %s as %s"
             (file-name-nondirectory path)
             (disco-room--attachment-token-text token-id))))

(defun disco-room-clear-attachments ()
  "Clear queued attachments for next send in current room."
  (interactive)
  (setq disco-room--pending-attachments nil)
  (when disco-room--attachment-token-table
    (clrhash disco-room--attachment-token-table))
  (disco-room--set-draft
   (string-trim-right (disco-room--draft-without-attachment-tokens)))
  (message "disco: cleared queued attachments"))

(defun disco-room-send-message ()
  "Send current draft message to this room asynchronously.

When called with prefix argument, force draft edit in minibuffer first."
  (interactive)
  (disco-room--sync-draft-from-buffer)
  (cond
   (disco-room--send-in-flight
    (message "disco: send already in progress"))
   (t
    (let* ((current-draft (disco-room--current-draft))
           (initial-has-attachments
            (not (null (disco-room--attachments-from-draft current-draft))))
           (content (if (or current-prefix-arg
                            (and (string-empty-p (string-trim-right current-draft))
                                 (not initial-has-attachments)))
                        (read-from-minibuffer "Message: " current-draft)
                      current-draft))
           (token-attachments (disco-room--attachments-from-draft content))
           (has-attachments (not (null token-attachments)))
           (content-without-tokens (disco-room--draft-without-attachment-tokens content))
           (normalized (string-trim-right (or content-without-tokens ""))))
      (if (and (string-empty-p normalized)
               (not has-attachments))
          (message "disco: draft is empty")
        (let* ((room-buffer (current-buffer))
               (channel-id disco-room--channel-id)
               (reply-to disco-room--pending-reply-to)
               (allowed-mentions (disco-room--send-allowed-mentions
                                  (not (null reply-to))))
               (attachments (copy-tree token-attachments))
               (required-permissions
                (append
                 (disco-room--required-send-permissions)
                 (when has-attachments
                   '(attach-files))
                 (when reply-to
                   '(read-message-history)))))
          (disco-room--ensure-channel-permissions
           required-permissions
           :action "sending messages")
          (unless (string-empty-p normalized)
            (disco-room--input-history-push normalized))
          (setq disco-room--draft-input "")
          (setq disco-room--send-in-flight t)
          (disco-room-render)
          (if has-attachments
              (disco-api-send-message-with-attachments-async
               channel-id
               :content (unless (string-empty-p normalized) normalized)
               :reply-to-message-id reply-to
               :allowed-mentions allowed-mentions
               :attachments attachments
               :on-success
               (lambda (_response)
                 (when (disco-room--channel-buffer-p room-buffer channel-id)
                   (with-current-buffer room-buffer
                     (setq disco-room--send-in-flight nil)
                     (setq disco-room--pending-reply-to nil)
                     (setq disco-room--pending-attachments nil)
                     (when disco-room--attachment-token-table
                       (clrhash disco-room--attachment-token-table))
                     (disco-room-refresh)
                     (message "disco: message with attachment(s) sent"))))
               :on-error
               (lambda (err)
                 (when (disco-room--channel-buffer-p room-buffer channel-id)
                   (with-current-buffer room-buffer
                     (setq disco-room--send-in-flight nil)
                     (setq disco-room--draft-input content)
                     (disco-room-render)
                     (message "disco: send failed: %s"
                              (disco-room--async-error-message err))))))
            (disco-api-send-message-async
             channel-id
             normalized
             :reply-to-message-id reply-to
             :allowed-mentions allowed-mentions
             :on-success
             (lambda (_response)
               (when (disco-room--channel-buffer-p room-buffer channel-id)
                 (with-current-buffer room-buffer
                   (setq disco-room--send-in-flight nil)
                   (setq disco-room--pending-reply-to nil)
                   (disco-room-refresh)
                   (message "disco: message sent"))))
             :on-error
             (lambda (err)
               (when (disco-room--channel-buffer-p room-buffer channel-id)
                 (with-current-buffer room-buffer
                   (setq disco-room--send-in-flight nil)
                   (setq disco-room--draft-input normalized)
                   (disco-room-render)
                   (message "disco: send failed: %s"
                            (disco-room--async-error-message err)))))))))))))

(defun disco-room-load-older-messages ()
  "Load one older message page before the oldest loaded message asynchronously."
  (interactive)
  (cond
   (disco-room--history-exhausted
    (message "disco: no older messages available"))
   (disco-room--older-in-flight
    (message "disco: older history load already in progress"))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (generation disco-room--refresh-generation)
           (before (or disco-room--oldest-message-id
                       (user-error "disco: no oldest message cursor; refresh first"))))
      (setq disco-room--older-in-flight t)
      (disco-api-channel-messages-async
       channel-id
       :before before
       :on-success
       (lambda (older)
         (when (buffer-live-p room-buffer)
           (with-current-buffer room-buffer
             (when (equal channel-id disco-room--channel-id)
               (setq disco-room--older-in-flight nil)
               (if (/= generation disco-room--refresh-generation)
                   (message "disco: discarded stale older-history page")
                 (let ((existing (or (disco-state-messages channel-id) '())))
                   (if (null older)
                       (progn
                         (setq disco-room--history-exhausted t)
                         (disco-room--render-preserving-point)
                         (disco-room--resolve-pending-jump)
                         (message "disco: reached beginning of history"))
                     (let ((merged (disco-room--merge-message-pages existing older)))
                       (disco-state-put-messages channel-id merged)
                       (disco-room--update-message-window-state merged)
                       (disco-room--render-preserving-point)
                       (disco-room--resolve-pending-jump)
                       (message "disco: loaded %d older messages" (length older))))))))))
       :on-error
       (lambda (err)
         (when (buffer-live-p room-buffer)
           (with-current-buffer room-buffer
             (when (equal channel-id disco-room--channel-id)
               (setq disco-room--older-in-flight nil)
               (message "disco: older history load failed: %s"
                        (disco-room--async-error-message err)))))))))))

(defun disco-room-reply-to-message (&optional message-id)
  "Set pending reply target MESSAGE-ID for next send.

When called interactively, defaults to message under point."
  (interactive
   (let* ((at-point (ignore-errors (disco-room--message-id-at-point)))
          (fallback (or at-point (disco-room--latest-message-id)))
          (raw (read-string
                (if fallback
                    (format "Reply to message ID (default %s): " fallback)
                  "Reply to message ID: "))))
     (list (if (string-empty-p raw)
               (or fallback
                   (user-error "disco: no target message available"))
             raw))))
  (setq disco-room--pending-reply-to message-id)
  (disco-room-render)
  (message "disco: next message will reply to %s" message-id))

(defun disco-room--forward-comment-rejected-p (status message)
  "Return non-nil when STATUS/MESSAGE indicate forward comment rejection."
  (and (numberp status)
       (= status 400)
       (stringp message)
       (string-match-p "Forward messages cannot have additional content" message)))

(defun disco-room-forward-message (&optional message-id source-channel-id content forward-only)
  "Forward MESSAGE-ID from SOURCE-CHANNEL-ID into current room.

CONTENT is optional text sent alongside the forwarded reference.
FORWARD-ONLY optionally narrows embeds/attachments included in the forward."
  (interactive
   (let* ((at-point (ignore-errors (disco-room--message-id-at-point)))
          (fallback-message (or at-point (disco-room--latest-message-id)))
          (message-raw (read-string
                        (if fallback-message
                            (format "Forward message ID (default %s): " fallback-message)
                          "Forward message ID: ")))
          (message-id (if (string-empty-p message-raw)
                          (or fallback-message
                              (user-error "disco: no message id provided"))
                        message-raw))
          (fallback-channel (or disco-room--channel-id ""))
          (channel-raw (read-string
                        (if (string-empty-p fallback-channel)
                            "Source channel ID: "
                          (format "Source channel ID (default %s): " fallback-channel))))
          (source-channel-id (if (string-empty-p channel-raw)
                                 (or fallback-channel
                                     (user-error "disco: no source channel id provided"))
                               channel-raw))
          (content-raw (string-trim (read-string "Optional forward comment: ")))
          (forward-only (disco-room--read-forward-only source-channel-id message-id)))
     (list message-id
           source-channel-id
           (unless (string-empty-p content-raw)
             content-raw)
           forward-only)))
  (let* ((target-channel-id disco-room--channel-id)
         (source-channel-id (or source-channel-id disco-room--channel-id))
         (source-channel (and source-channel-id
                              (disco-room--resolve-target-channel source-channel-id)))
         (normalized-content
          (and (stringp content)
               (let ((trimmed (string-trim content)))
                 (unless (string-empty-p trimmed)
                   trimmed))))
         (room-buffer (current-buffer))
         (allowed-mentions (and normalized-content
                                (disco-room--send-allowed-mentions))))
    (unless (and message-id (not (string-empty-p (format "%s" message-id))))
      (user-error "disco: message id cannot be empty"))
    (unless (and source-channel-id
                 (not (string-empty-p (format "%s" source-channel-id))))
      (user-error "disco: source channel id cannot be empty"))
    (disco-room--ensure-channel-permissions
     (disco-room--required-send-permissions)
     :action "forwarding messages")
    (disco-room--ensure-jump-permissions source-channel-id source-channel)
    (setq disco-room--send-in-flight t)
    (disco-room-render)
    (cl-labels
        ((room-active-p ()
           (disco-room--channel-buffer-p room-buffer target-channel-id))
         (finish-success (split-p)
           (when (room-active-p)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room-refresh)
               (message (if split-p
                            "disco: sent comment + forward for %s from channel %s"
                          "disco: forwarded message %s from channel %s")
                        message-id source-channel-id))))
         (finish-error (text)
           (when (room-active-p)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room-render)
               (message "%s" text))))
         (send-comment-then-forward ()
           (when (room-active-p)
             (message "disco: forward comment rejected by API; retrying as comment + forward")
             (disco-api-send-message-async
              target-channel-id
              normalized-content
              :allowed-mentions allowed-mentions
              :on-success
              (lambda (_response)
                (when (room-active-p)
                  (send-forward nil t)))
              :on-error
              (lambda (err)
                (when (room-active-p)
                  (finish-error
                   (format "disco: forward comment send failed: %s"
                           (disco-room--async-error-message err))))))))
         (send-forward (forward-content split-p)
           (when (room-active-p)
             (disco-api-forward-message-async
              target-channel-id
              message-id
              source-channel-id
              :content forward-content
              :forward-only forward-only
              :allowed-mentions (and forward-content allowed-mentions)
              :on-success
              (lambda (_response)
                (finish-success split-p))
              :on-error
              (lambda (err)
                (let* ((msg (disco-room--async-error-message err))
                       (status (and (listp err) (plist-get err :status))))
                  (if (and forward-content
                           (eq disco-room-forward-comment-rejection-action 'split)
                           (disco-room--forward-comment-rejected-p status msg))
                      (send-comment-then-forward)
                    (finish-error (format "disco: forward failed: %s" msg)))))))))
      (send-forward normalized-content nil))))

(defun disco-room-cancel-reply ()
  "Cancel pending reply target for next send."
  (interactive)
  (setq disco-room--pending-reply-to nil)
  (disco-room-render)
  (message "disco: reply target cleared"))

(defun disco-room-return-dwim ()
  "RET behavior for room buffer.

When `disco-room-send-on-return' is non-nil, send current draft.
Otherwise open draft editor."
  (interactive)
  (if disco-room-send-on-return
      (disco-room-send-message)
    (disco-room-edit-draft)))

(defun disco-room-toggle-breakline ()
  "Toggle visual breakline wrapping in the current room buffer."
  (interactive)
  (setq-local disco-room-wrap-long-lines (not disco-room-wrap-long-lines))
  (disco-room--apply-breakline-settings)
  (message "disco: breakline wrapping %s"
           (if disco-room-wrap-long-lines "enabled" "disabled")))

(defun disco-room-edit-message ()
  "Edit message at point in current room."
  (interactive)
  (let* ((msg (disco-room--message-at-point))
         (message-id (alist-get 'id msg))
         (old-content (or (alist-get 'content msg) ""))
         (new-content (read-string (format "Edit message %s: " message-id) old-content))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id))
    (disco-api-edit-message-async
     channel-id
     message-id
     new-content
     :allowed-mentions (disco-room--send-allowed-mentions)
     :on-success
     (lambda (_response)
       (when (disco-room--channel-buffer-p room-buffer channel-id)
         (with-current-buffer room-buffer
           (disco-room-refresh)
           (message "disco: edited message %s" message-id))))
     :on-error
     (lambda (err)
       (message "disco: edit failed for %s: %s"
                message-id
                (disco-room--async-error-message err))))))

(defun disco-room-delete-message ()
  "Delete message at point in current room."
  (interactive)
  (let* ((message-id (disco-room--message-id-at-point)))
    (when (y-or-n-p (format "Delete message %s? " message-id))
      (let ((room-buffer (current-buffer))
            (channel-id disco-room--channel-id))
        (disco-api-delete-message-async
         channel-id
         message-id
         :on-success
         (lambda (_response)
           (when (disco-room--channel-buffer-p room-buffer channel-id)
             (with-current-buffer room-buffer
               (disco-room-refresh)
               (message "disco: deleted message %s" message-id))))
         :on-error
         (lambda (err)
           (message "disco: delete failed for %s: %s"
                    message-id
                    (disco-room--async-error-message err))))))))

(defun disco-room-create-thread-from-message (name message-id
                                                   &optional auto-archive-duration
                                                   rate-limit-per-user)
  "Create thread NAME from MESSAGE-ID in current channel.

AUTO-ARCHIVE-DURATION is optional minutes.
RATE-LIMIT-PER-USER is optional slowmode seconds."
  (interactive
   (let* ((name (read-string "Thread name: "))
          (default-message-id (disco-room--latest-message-id))
          (message-raw (read-string
                        (if default-message-id
                            (format "Message ID (default %s): " default-message-id)
                          "Message ID: ")))
          (message-id (if (string-empty-p message-raw)
                          (or default-message-id
                              (user-error "disco: no message id provided and no loaded messages"))
                        message-raw))
          (auto-archive-duration (disco-room--read-thread-auto-archive-duration))
          (rate-limit-per-user
           (disco-room--read-optional-nonnegative-int
            "Slowmode seconds (empty for none): ")))
     (list name message-id auto-archive-duration rate-limit-per-user)))
  (disco-room--ensure-parent-channel)
  (let* ((thread (disco-api-create-thread-from-message
                  disco-room--channel-id
                  message-id
                  name
                  auto-archive-duration
                  rate-limit-per-user))
         (thread-id (and (listp thread) (alist-get 'id thread)))
         (thread-name (or (and (listp thread) (alist-get 'name thread)) name)))
    (when thread-id
      (disco-state-upsert-channel thread)
      (disco-room-open thread-id thread-name))
    (message "disco: created thread %s" name)))

(defun disco-room-create-thread (name &optional type auto-archive-duration
                                      invitable rate-limit-per-user)
  "Create detached thread NAME in current channel.

TYPE is optional thread channel type.
AUTO-ARCHIVE-DURATION is optional minutes.
INVITABLE controls private-thread invites when TYPE is 12.
RATE-LIMIT-PER-USER is optional slowmode seconds."
  (interactive
   (let* ((name (read-string "Thread name: "))
          (type (unless (disco-room--forum-or-media-channel-p)
                  (disco-room--read-detached-thread-type)))
          (auto-archive-duration (disco-room--read-thread-auto-archive-duration))
          (invitable (when (equal type 12)
                       (y-or-n-p "Invitable by non-moderators? ")))
          (rate-limit-per-user
           (disco-room--read-optional-nonnegative-int
            "Slowmode seconds (empty for none): ")))
     (list name type auto-archive-duration invitable rate-limit-per-user)))
  (disco-room--ensure-parent-channel)
  (let* ((thread (disco-api-create-thread
                  disco-room--channel-id
                  name
                  type
                  auto-archive-duration
                  invitable
                  rate-limit-per-user))
         (thread-id (and (listp thread) (alist-get 'id thread)))
         (thread-name (or (and (listp thread) (alist-get 'name thread)) name)))
    (when thread-id
      (disco-state-upsert-channel thread)
      (disco-room-open thread-id thread-name))
    (message "disco: created detached thread %s" name)))

(defun disco-room-join-thread ()
  "Join current thread room as current user."
  (interactive)
  (disco-room--ensure-thread-channel)
  (disco-api-join-thread disco-room--channel-id)
  (message "disco: joined thread %s" disco-room--channel-name))

(defun disco-room-leave-thread ()
  "Leave current thread room as current user."
  (interactive)
  (disco-room--ensure-thread-channel)
  (disco-api-leave-thread disco-room--channel-id)
  (message "disco: left thread %s" disco-room--channel-name))

(defun disco-room-toggle-thread-archived ()
  "Toggle archived state for current thread."
  (interactive)
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-archived (not (disco-room--thread-archived-p channel)))
         (updated (disco-api-set-thread-archived disco-room--channel-id next-archived nil)))
    (if (and (listp updated) (alist-get 'id updated))
        (disco-state-upsert-channel updated)
      ;; Fallback when API returns empty body.
      (disco-state-upsert-channel
       (disco-room--thread-with-meta-field
        channel
        'archived
        (if next-archived t :false))))
    (disco-room--render-preserving-point)
    (message "disco: thread %s" (if next-archived "archived" "unarchived"))))

(defun disco-room-rename-thread (name)
  "Rename current thread to NAME."
  (interactive
   (let* ((channel (or (disco-room--channel-object)
                       (user-error "disco: unknown thread in state")))
          (current-name (or (alist-get 'name channel) ""))
          (name (string-trim
                 (read-string "Thread name: " current-name))))
     (list name)))
  (disco-room--ensure-thread-channel)
  (when (string-empty-p name)
    (user-error "disco: thread name cannot be empty"))
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (updated (disco-api-update-thread disco-room--channel-id :name name))
         (fallback (disco-room--thread-with-field channel 'name name)))
    (disco-room--resolve-thread-update updated fallback)
    (disco-room--render-preserving-point)
    (message "disco: thread renamed to %s" name)))

(defun disco-room-toggle-thread-locked ()
  "Toggle locked state for current thread."
  (interactive)
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-locked (not (disco-room--thread-locked-p channel)))
         (updated (disco-api-update-thread disco-room--channel-id :locked next-locked))
         (fallback (disco-room--thread-with-meta-field
                    channel
                    'locked
                    (if next-locked t :false))))
    (disco-room--resolve-thread-update updated fallback)
    (disco-room--render-preserving-point)
    (message "disco: thread %s" (if next-locked "locked" "unlocked"))))

(defun disco-room-set-thread-slowmode (seconds)
  "Set current thread slowmode to SECONDS.

When called interactively, empty input clears slowmode (sets to 0)."
  (interactive
   (list (or (disco-room--read-optional-nonnegative-int
              "Slowmode seconds (empty clears to 0): ")
             0)))
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (updated (disco-api-update-thread
                   disco-room--channel-id
                   :rate-limit-per-user seconds))
         (fallback (disco-room--thread-with-field channel 'rate_limit_per_user seconds)))
    (disco-room--resolve-thread-update updated fallback)
    (disco-room--render-preserving-point)
    (message "disco: thread slowmode -> %ss" seconds)))

(defun disco-room-set-thread-auto-archive-duration (minutes)
  "Set current thread auto archive duration to MINUTES."
  (interactive
   (let* ((channel (or (disco-room--channel-object)
                       (user-error "disco: unknown thread in state")))
          (meta (disco-room--thread-metadata channel))
          (current (or (alist-get 'auto_archive_duration meta)
                       (alist-get 'auto_archive_duration channel))))
     (list (disco-room--read-required-thread-auto-archive-duration current))))
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (updated (disco-api-update-thread
                   disco-room--channel-id
                   :auto-archive-duration minutes))
         (fallback (disco-room--thread-with-meta-field
                    channel
                    'auto_archive_duration
                    minutes)))
    (disco-room--resolve-thread-update updated fallback)
    (disco-room--render-preserving-point)
    (message "disco: auto archive -> %s minutes" minutes)))

(defun disco-room-set-thread-muted (muted)
  "Set current user's muted state for current thread to MUTED."
  (interactive
   (list (y-or-n-p "Mute this thread? ")))
  (disco-room--ensure-thread-channel)
  (disco-api-update-thread-member-settings disco-room--channel-id :muted muted)
  (message "disco: thread notifications %s" (if muted "muted" "unmuted")))

(defun disco-room-edit-thread-settings ()
  "Edit multiple thread settings in one PATCH request."
  (interactive)
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (meta (disco-room--thread-metadata channel))
         (current-name (or (alist-get 'name channel) ""))
         (name-input (string-trim
                      (read-string
                       (format "Thread name (empty keeps %s): " current-name))))
         (name (unless (string-empty-p name-input) name-input))
         (current-auto (or (alist-get 'auto_archive_duration meta)
                           (alist-get 'auto_archive_duration channel)))
         (auto-input (completing-read
                      (format "Auto archive minutes (empty keeps %s): "
                              (or current-auto "unset"))
                      '("" "60" "1440" "4320" "10080") nil t nil nil ""))
         (auto-archive-duration
          (unless (string-empty-p auto-input)
            (string-to-number auto-input)))
         (slow-input (read-string
                      (format "Slowmode seconds (empty keeps %s): "
                              (or (alist-get 'rate_limit_per_user channel) 0))))
         (rate-limit-per-user
          (unless (string-empty-p slow-input)
            (let ((n (string-to-number slow-input)))
              (when (< n 0)
                (user-error "disco: value must be >= 0"))
              n)))
         (archived-choice
          (disco-room--read-tristate-bool
           "Archived"
           (disco-room--thread-archived-p channel)))
         (locked-choice
          (disco-room--read-tristate-bool
           "Locked"
           (disco-room--thread-locked-p channel)))
         (archived (unless (eq archived-choice 'keep) archived-choice))
         (locked (unless (eq locked-choice 'keep) locked-choice))
         (has-change (or name
                         auto-archive-duration
                         (not (null rate-limit-per-user))
                         (not (eq archived-choice 'keep))
                         (not (eq locked-choice 'keep)))))
    (unless has-change
      (user-error "disco: no thread setting changes provided"))
    (let* ((updated
            (disco-api-update-thread
             disco-room--channel-id
             :name name
             :auto-archive-duration auto-archive-duration
             :rate-limit-per-user rate-limit-per-user
             :archived archived
             :locked locked))
           (fallback (copy-tree channel)))
      (when name
        (setf (alist-get 'name fallback nil 'remove) name))
      (when auto-archive-duration
        (setq fallback
              (disco-room--thread-with-meta-field
               fallback
               'auto_archive_duration
               auto-archive-duration)))
      (when (not (null rate-limit-per-user))
        (setf (alist-get 'rate_limit_per_user fallback nil 'remove)
              rate-limit-per-user))
      (when (not (eq archived-choice 'keep))
        (setq fallback
              (disco-room--thread-with-meta-field
               fallback
               'archived
               archived)))
      (when (not (eq locked-choice 'keep))
        (setq fallback
              (disco-room--thread-with-meta-field
               fallback
               'locked
               locked)))
      (disco-room--resolve-thread-update updated fallback)
      (disco-room--render-preserving-point)
      (message "disco: updated thread settings"))))

(defvar disco-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-room-refresh)
    (define-key map (kbd "M-<") #'disco-room-load-older-messages)
    (define-key map (kbd "RET") #'disco-room-return-dwim)
    (define-key map (kbd "C-c '") #'disco-room-edit-draft)
    (define-key map (kbd "M-p") #'disco-room-draft-prev)
    (define-key map (kbd "M-n") #'disco-room-draft-next)
    (define-key map (kbd "s") #'disco-room-search)
    (define-key map (kbd "n") #'disco-room-search-next)
    (define-key map (kbd "p") #'disco-room-search-prev)
    (define-key map (kbd "r") #'disco-room-reply-to-message)
    (define-key map (kbd "e") #'disco-room-edit-message)
    (define-key map (kbd "d") #'disco-room-delete-message)
    (define-key map (kbd "!") #'disco-room-toggle-reaction)
    (define-key map (kbd "+") #'disco-room-add-reaction)
    (define-key map (kbd "-") #'disco-room-remove-reaction)
    (define-key map (kbd "C-c C-p s") #'disco-room-send-poll)
    (define-key map (kbd "C-c C-p +") #'disco-room-vote-poll-answer)
    (define-key map (kbd "C-c C-p -") #'disco-room-remove-poll-vote)
    (define-key map (kbd "C-c C-p t") #'disco-room-toggle-poll-answer)
    (define-key map (kbd "C-c C-p v") #'disco-room-submit-poll-vote)
    (define-key map (kbd "C-c C-p c") #'disco-room-clear-poll-votes)
    (define-key map (kbd "C-c C-p e") #'disco-room-expire-poll)
    (define-key map (kbd "C-c C-c") #'disco-room-send-message)
    (define-key map (kbd "C-c C-f") #'disco-room-attach-file)
    (define-key map (kbd "C-c C-F") #'disco-room-forward-message)
    (define-key map (kbd "C-c C-d") #'disco-room-remove-attachment-token-at-point)
    (define-key map (kbd "C-c C-x") #'disco-room-clear-attachments)
    (define-key map (kbd "C-c M-l") #'disco-room-list-attachments)
    (define-key map (kbd "C-c M-e") #'disco-room-edit-attachment-description)
    (define-key map (kbd "C-c M-r") #'disco-room-reorder-attachments)
    (define-key map (kbd "C-c C-k") #'disco-room-cancel-reply)
    (define-key map (kbd "C-c C-g") #'disco-room-jump-to-message)
    (define-key map (kbd "C-c C-w") #'disco-room-toggle-breakline)
    (define-key map (kbd "C-c C-t m") #'disco-room-create-thread-from-message)
    (define-key map (kbd "C-c C-t o") #'disco-room-open-thread-from-message-at-point)
    (define-key map (kbd "C-c C-t c") #'disco-room-create-thread)
    (define-key map (kbd "C-c C-t r") #'disco-room-rename-thread)
    (define-key map (kbd "C-c C-t k") #'disco-room-toggle-thread-locked)
    (define-key map (kbd "C-c C-t s") #'disco-room-set-thread-slowmode)
    (define-key map (kbd "C-c C-t a") #'disco-room-set-thread-auto-archive-duration)
    (define-key map (kbd "C-c C-t e") #'disco-room-edit-thread-settings)
    (define-key map (kbd "C-c C-t u") #'disco-room-set-thread-muted)
    (define-key map (kbd "C-c C-j") #'disco-room-join-thread)
    (define-key map (kbd "C-c C-l") #'disco-room-leave-thread)
    (define-key map (kbd "C-c C-a") #'disco-room-toggle-thread-archived)
    (define-key map (kbd "C-c C-v") #'disco-room-refetch-avatars)
    (define-key map (kbd "?") #'disco-room-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-room-mode'.")

(define-derived-mode disco-room-mode special-mode "Disco-Room"
  "Major mode for disco.el room buffers."
  (setq buffer-read-only nil)
  (disco-room--apply-breakline-settings)
  ;; Avoid visible seams between vertically sliced inline images.
  (setq-local line-spacing 0)
  ;; Strip visual-only line prefixes from copied text.
  (setq-local filter-buffer-substring-function
              #'disco-room--buffer-substring-filter)
  (disco-room--typing-cancel-expire-timer)
  (setq-local disco-room--draft-input "")
  (setq-local disco-room--input-ring (make-ring (max 1 disco-room-input-history-size)))
  (setq-local disco-room--input-index nil)
  (setq-local disco-room--input-pending nil)
  (setq-local disco-room--send-in-flight nil)
  (setq-local disco-room--pending-jump-message-id nil)
  (setq-local disco-room--last-search-query nil)
  (setq-local disco-room--input-marker nil)
  (setq-local disco-room--input-prompt-marker nil)
  (setq-local disco-room--rendering nil)
  (setq-local disco-room--ewoc nil)
  (setq-local disco-room--render-context-by-message-id (make-hash-table :test #'equal))
  (setq-local disco-room--pending-attachments nil)
  (setq-local disco-room--attachment-token-table (make-hash-table :test #'equal))
  (setq-local disco-room--attachment-token-seq 0)
  (setq-local disco-room--typing-users (make-hash-table :test #'equal))
  (setq-local disco-room--typing-expire-timer nil)
  (setq-local disco-room--poll-selection-drafts (make-hash-table :test #'equal))
  (setq-local disco-room--message-node-table (make-hash-table :test #'equal))
  (when (fboundp 'cursor-intangible-mode)
    (cursor-intangible-mode 1))
  (funcall #'disco-company-setup-room-buffer)
  (add-hook 'text-scale-mode-hook #'disco-room--on-text-scale-change nil t)
  (add-hook 'after-change-functions #'disco-room--after-change nil t)
  (add-hook 'post-command-hook #'disco-room--post-command nil t))

(defun disco-room-open (channel-id channel-name)
  "Open room for CHANNEL-ID with CHANNEL-NAME."
  (let ((buf (get-buffer-create (disco-room--buffer-name channel-name channel-id))))
    (with-current-buffer buf
      (disco-room-mode)
      (setq disco-room--channel-id channel-id)
      (setq disco-room--channel-name channel-name)
      (let ((channel (disco-state-channel channel-id)))
        (setq disco-room--guild-id (and channel (alist-get 'guild_id channel))))
      (disco-room--attach-live-updates)
      (disco-room-refresh))
    (pop-to-buffer buf)))

(provide 'disco-room)

;;; disco-room.el ends here
