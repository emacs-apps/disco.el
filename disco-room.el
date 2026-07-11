;;; disco-room.el --- Channel room buffers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Per-channel room buffer with simple timeline rendering and message sending.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'seq)
(require 'transient)
(require 'ring)
(require 'cl-lib)
(require 'ewoc)
(require 'button)
(require 'browse-url)
(require 'url-handlers)
(require 'plz)
(require 'svg nil t)
(require 'disco-chat-avatar)
(require 'disco-chatbuf)
(require 'disco-chat-timeline)
(require 'disco-ins)
(require 'disco-ui)
(require 'disco-util)
(require 'disco-msg)
(require 'disco-thread)
(require 'disco-typing)
(require 'disco-markdown)
(require 'disco-media)
(require 'disco-embed)
(require 'disco-view)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-permission)
(require 'disco-company)
(require 'disco-room-search)

(declare-function disco-api--validate-message-content-length "disco-api-normalize"
                  (content field-name))
(defvar disco-api--message-content-limit)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--oldest-message-id nil)
(defvar disco-ui-card-indent-prefix)
(defvar disco-ui-card-indent-prefix-state)
(defvar-local disco-room--newest-message-id nil)
(defvar-local disco-room--history-exhausted nil)
(defvar-local disco-room--pending-reply-to nil)
(defvar-local disco-room--pending-edit nil)
(defvar-local disco-room--pending-jump-message-id nil)
(defvar-local disco-room--jump-in-flight nil)
(defvar-local disco-room--gateway-handler nil)
(defvar-local disco-room--refresh-generation 0)
(defvar-local disco-room--refresh-in-flight nil)
(defvar-local disco-room--older-in-flight nil)
(defvar-local disco-room--send-in-flight nil)

(defvar disco-room--send-nonce-counter 0
  "Monotonic low bits for client-generated Discord message nonces.")

(defun disco-room--next-send-nonce ()
  "Return a unique snowflake-shaped nonce for exact send reconciliation."
  (setq disco-room--send-nonce-counter
        (logand (1+ disco-room--send-nonce-counter) (1- (ash 1 22))))
  (number-to-string
   (+ (ash (- (truncate (* 1000 (float-time)))
              (* disco-state-discord-epoch-seconds 1000))
           22)
      disco-room--send-nonce-counter)))

(defun disco-room--render-send-state-change ()
  "Render an explicit local send-state mutation without fetching history."
  (disco-room-render))
(defvar-local disco-room--last-search-query nil)
(defvar-local disco-room--msg-filter nil)
(defvar-local disco-room--filter-generation 0)
(defvar-local disco-room--filter-in-flight nil)
(defvar-local disco-room--inplace-search-filter nil)
(defvar-local disco-room--inplace-search-generation 0)
(defvar-local disco-room--chat-fill-column nil)
(defvar-local disco-room--pending-attachments nil)
(defvar-local disco-room--attachment-token-table nil)
(defvar-local disco-room--attachment-token-seq 0)
(defvar-local disco-room--typing-users nil)
(defvar-local disco-room--typing-expire-timer nil)
(defvar-local disco-room--poll-selection-drafts nil)
(defvar-local disco-room--revealed-spoiler-message-id nil)
(defvar-local disco-room--optimistic-read-ack-seq 0)
(defvar-local disco-room--pending-optimistic-read-ack nil)

(defconst disco-room--attachment-token-regexp "\\[file:\\([0-9]+\\)\\]"
  "Regexp used to match attachment tokens in room draft input.")

(defconst disco-room--input-object-kind-attachment 'attachment
  "Structured input object kind used for queued file attachments.")

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

(defvar disco-room--avatar-fetch-budget nil
  "Dynamic cap for number of avatar fetches started in current render pass.")

(defvar disco-room--avatar-plz-queue nil
  "Shared plz queue used for asynchronous avatar downloads.")

(defvar disco-room--avatar-plz-queue-limit nil
  "Last applied queue limit for `disco-room--avatar-plz-queue'.")

(defconst disco-room--avatar-cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "img")
  "Preferred avatar cache file extension candidates.")

(defvar disco-room-draft-history-search-history nil
  "Minibuffer history for room draft-history searches.")

(defcustom disco-room-input-history-size 30
  "Maximum number of draft entries kept in room input history."
  :type 'integer
  :group 'disco)

(defcustom disco-room-send-on-return t
  "When non-nil, `RET' in room buffer sends current draft."
  :type 'boolean
  :group 'disco)

(defcustom disco-room-long-message-action 'split
  "How `disco-room-send-message' handles content longer than one Discord message.

`split' sends multiple messages by splitting near paragraph, line, or word
boundaries. `file' sends the text as a `.txt' attachment instead."
  :type '(choice
          (const :tag "Split into multiple messages" split)
          (const :tag "Send as text file attachment" file))
  :group 'disco)

(defcustom disco-room-long-message-file-name "message.txt"
  "Filename used when long room drafts are sent as text attachments."
  :type 'string
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
`all' explicitly enables users/roles/everyone parsing. Any alist value is
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

(defcustom disco-room-avatar-round-size-factor 1.0
  "Scale factor applied to computed avatar size for round rendering.

Set to 1.0 to match telega-like two-line avatar geometry."
  :type 'number
  :group 'disco)

(defcustom disco-room-avatar-round-inset-ratio 0.0
  "Inset ratio used when clipping circular avatars.

Set to 0.0 to match telega-like two-line avatar geometry."
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

(defcustom disco-room-avatar-cache-directory
  (locate-user-emacs-file "disco-avatar-cache/")
  "Directory used to cache downloaded avatar images."
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


(defcustom disco-room-avatar-fetch-concurrency 20
  "Maximum concurrent avatar downloads in plz queue."
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

(defcustom disco-room-auto-fill-margin-columns 1
  "Additional right margin columns used for timestamp alignment.

This mirrors telega auto-fill behavior and helps avoid edge clipping."
  :type '(choice (const :tag "No additional margin" nil)
          (integer :tag "Additional margin columns"))
  :group 'disco)

(defcustom disco-room-show-attachment-urls nil
  "When non-nil, include raw attachment URLs in message rendering."
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

(defface disco-room-search-highlight
  '((t :inherit isearch))
  "Face used to highlight active room search query matches."
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

(defface disco-room-system-divider
  '((t :inherit font-lock-comment-face))
  "Face used for system event divider lines (e.g. user join)."
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

(defvar disco-room-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'disco-msg-copy-dwim)
    (define-key map (kbd "l") #'disco-msg-copy-link)
    (define-key map (kbd "n") #'disco-msg-next)
    (define-key map (kbd "p") #'disco-msg-previous)
    (define-key map (kbd "o") #'disco-msg-operate)
    (define-key map (kbd "r") #'disco-msg-reply)
    (define-key map (kbd "f") #'disco-msg-forward)
    (define-key map (kbd "e") #'disco-msg-edit)
    (define-key map (kbd "d") #'disco-msg-delete)
    (define-key map (kbd "i") #'disco-msg-describe-message)
    (define-key map (kbd "L") #'disco-msg-redisplay)
    (define-key map (kbd "!") #'disco-msg-add-reaction)
    (define-key map (kbd "+") #'disco-msg-toggle-reaction)
    (define-key map (kbd "-") #'disco-msg-remove-reaction)
    (define-key map (kbd "T") #'disco-msg-open-thread)
    map)
  "Timeline-only keymap active when point is outside the room draft.")

(defvar disco-room-message-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'disco-msg-copy-dwim)
    (define-key map (kbd "l") #'disco-msg-copy-link)
    (define-key map (kbd "n") #'disco-msg-next)
    (define-key map (kbd "p") #'disco-msg-previous)
    (define-key map (kbd "o") #'disco-msg-operate)
    (define-key map (kbd "t") #'disco-msg-copy-text)
    (define-key map (kbd "r") #'disco-msg-reply)
    (define-key map (kbd "f") #'disco-msg-forward)
    (define-key map (kbd "e") #'disco-msg-edit)
    (define-key map (kbd "d") #'disco-msg-delete)
    (define-key map (kbd "i") #'disco-msg-describe-message)
    (define-key map (kbd "L") #'disco-msg-redisplay)
    (define-key map (kbd "!") #'disco-msg-add-reaction)
    (define-key map (kbd "+") #'disco-msg-toggle-reaction)
    (define-key map (kbd "-") #'disco-msg-remove-reaction)
    (define-key map (kbd "T") #'disco-msg-open-thread)
    map)
  "Prefix map for message actions at point in `disco-room-mode'.")

(define-minor-mode disco-room-timeline-mode
  "Buffer-local navigation bindings active outside the room draft."
  :init-value nil
  :lighter nil
  :keymap disco-room-timeline-mode-map)

(defun disco-room--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-room--channel-header-suffix (&optional channel)
  "Return human-readable suffixes for room header CHANNEL."
  (let ((channel (or channel (disco-room--channel-object))))
    (concat
     (if (disco-state-channel-age-restricted-p channel)
         " [18+]"
       "")
     (disco-thread-header-suffix channel))))

(defun disco-room--ensure-thread-channel ()
  "Signal user error unless current room channel is a thread."
  (unless (disco-thread-channel-p (disco-room--channel-object))
    (user-error "disco: current room is not a thread")))

(defun disco-room--ensure-parent-channel ()
  "Signal user error when current room channel is itself a thread."
  (when (disco-thread-channel-p (disco-room--channel-object))
    (user-error "disco: open a parent channel room to create a new thread")))

(defun disco-room--required-send-permissions (&optional channel)
  "Return permission list required to send message in CHANNEL.

When CHANNEL is nil, use current room channel."
  (if (disco-thread-channel-p (or channel (disco-room--channel-object)))
      '(send-messages-in-threads)
    '(send-messages)))

(defun disco-room--poll-vote-required-permissions (&optional channel)
  "Return required permissions for poll vote actions in CHANNEL."
  (disco-room--required-send-permissions channel))

(defun disco-room--poll-expire-required-permissions (&optional channel)
  "Return required permissions for poll expire action in CHANNEL."
  (append (disco-room--required-send-permissions channel)
          '(send-polls)))

(defun disco-room--composer-missing-permissions (&optional channel)
  "Return missing send permissions that should hide room composer for CHANNEL.

When computed permissions are unavailable, return nil to avoid false
negatives."
  (let ((channel (or channel (disco-room--channel-object))))
    (and channel
         (disco-permission-channel-known-p channel)
         (disco-permission-channel-missing
          channel
          (disco-room--required-send-permissions channel)
          nil))))

(defun disco-room--system-user-dm-restriction-reason (&optional channel)
  "Return read-only reason for official system-user DM CHANNEL, or nil."
  (let* ((channel (or channel (disco-room--channel-object)))
         (channel-type (and (listp channel) (alist-get 'type channel)))
         (recipients (and (listp channel) (alist-get 'recipients channel)))
         (recipient (and (equal channel-type 1) (car recipients))))
    (when (and (listp recipient)
               (alist-get 'system recipient))
      "official Discord system DMs are read-only")))

(defun disco-room--thread-send-restriction-reason (&optional channel)
  "Return thread-local send restriction reason for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (when (disco-thread-channel-p channel)
      (let ((tags (delq nil (list (and (disco-thread-archived-p channel) "archived")
                                  (and (disco-thread-locked-p channel) "locked")))))
        (when tags
          (format "current thread is %s" (mapconcat #'identity tags ", ")))))))

(defun disco-room--room-send-restriction-reason (&optional extra-permissions channel)
  "Return send restriction reason for current room CHANNEL, or nil.

EXTRA-PERMISSIONS augments the base send permission set for this room."
  (let* ((channel (or channel (disco-room--channel-object)))
         (system-dm-reason (disco-room--system-user-dm-restriction-reason channel))
         (thread-reason (disco-room--thread-send-restriction-reason channel))
         (permissions (append (disco-room--required-send-permissions channel)
                              (or extra-permissions '())))
         (missing (and channel
                       (disco-permission-channel-known-p channel)
                       (disco-permission-channel-missing channel permissions nil))))
    (or system-dm-reason
        thread-reason
        (when missing
          (format "missing %s"
                  (mapconcat #'disco-permission-display-name missing ", "))))))

(defun disco-room--composer-visible-p (&optional channel)
  "Return non-nil when room composer should be shown for CHANNEL."
  (not (disco-room--room-send-restriction-reason nil channel)))

(defun disco-room--composer-hidden-status-line (&optional channel)
  "Return read-only status line when room composer is hidden for CHANNEL."
  (when-let* ((reason (disco-room--room-send-restriction-reason nil channel)))
    (format "(read-only room; composer hidden: %s)" reason)))

(defun disco-room--current-composer-aux-state ()
  "Return current room-local composer aux plist, or nil."
  (cond
   ((and (listp disco-room--pending-edit)
         (eq (plist-get disco-room--pending-edit :type) 'edit))
    (let ((message-id (plist-get disco-room--pending-edit :message-id)))
      (list :aux-type 'edit
            :aux-msg (disco-room--composer-context-message message-id)
            :message-id message-id)))
   (disco-room--pending-reply-to
    (list :aux-type 'reply
          :aux-msg (disco-room--composer-context-message disco-room--pending-reply-to)
          :message-id disco-room--pending-reply-to))
   (t nil)))

(defun disco-room--composer-reply-message-id ()
  "Return target message id for active composer reply, or nil."
  (when (eq (plist-get (disco-chatbuf-aux-state) :aux-type) 'reply)
    (plist-get (disco-chatbuf-aux-state) :message-id)))

(defun disco-room--composer-edit-active-p ()
  "Return non-nil when room composer is editing an existing message."
  (eq (plist-get (disco-chatbuf-aux-state) :aux-type) 'edit))

(defun disco-room--composer-edit-message-id ()
  "Return target message id for active composer edit, or nil."
  (and (disco-room--composer-edit-active-p)
       (plist-get (disco-chatbuf-aux-state) :message-id)))

(defun disco-room--composer-aux-active-p ()
  "Return non-nil when room composer currently has reply/edit context."
  (not (null (disco-chatbuf-aux-state))))

(defun disco-room--composer-aux-context-name ()
  "Return human-readable name for active composer aux context, or nil."
  (pcase (plist-get (disco-chatbuf-aux-state) :aux-type)
    ('edit "editing a message")
    ('reply "replying to a message")
    (_ nil)))

(defun disco-room--message-owned-by-current-user-p (msg &optional unknown-value)
  "Return non-nil when MSG belongs to current user.

If ownership cannot be determined, return UNKNOWN-VALUE."
  (let* ((author-id (and (listp msg) (disco-room--message-author-id msg)))
         (self-id (disco-gateway-current-user-id)))
    (if (or (null author-id) (null self-id))
        unknown-value
      (equal (format "%s" author-id) (format "%s" self-id)))))

(defun disco-room--edit-permission-reason (&optional msg)
  "Return edit permission/restriction reason for MSG, or nil."
  (let* ((channel (disco-room--channel-object))
         (base-reason (disco-room--room-send-restriction-reason nil channel))
         (missing-manage
          (and (listp msg)
               channel
               (disco-permission-channel-known-p channel)
               (not (disco-room--message-owned-by-current-user-p msg t))
               (disco-permission-channel-missing channel '(manage-messages) nil))))
    (or base-reason
        (when missing-manage
          (format "missing %s"
                  (mapconcat #'disco-permission-display-name missing-manage ", "))))))

(defun disco-room--attach-unavailable-reason ()
  "Return reason attach-file action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason '(attach-files))
      (when (disco-room--composer-edit-active-p)
        "attachments are unavailable while editing a message")))

(defun disco-room--reply-unavailable-reason ()
  "Return reason reply action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason '(read-message-history))
      (when (disco-room--composer-edit-active-p)
        "cancel the active edit before starting a reply")))

(defun disco-room--forward-unavailable-reason ()
  "Return reason forward action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason)
      (when-let* ((aux (disco-room--composer-aux-context-name)))
        (format "cancel %s before forwarding" aux))))

(defun disco-room--poll-unavailable-reason ()
  "Return reason send-poll action is unavailable, or nil."
  (or (disco-room--room-send-restriction-reason '(send-polls))
      (when-let* ((aux (disco-room--composer-aux-context-name)))
        (format "cancel %s before sending a poll" aux))))

(defun disco-room--edit-start-unavailable-reason (&optional msg)
  "Return reason entering composer edit mode for MSG is unavailable, or nil."
  (or (when (disco-room--composer-edit-active-p)
        "already editing a message")
      (when (disco-room--composer-reply-message-id)
        "cancel the active reply before editing a message")
      (disco-room--edit-permission-reason msg)))

(defun disco-room--attachment-token-count ()
  "Return number of queued attachment refs in current draft."
  (length (disco-room--attachments-from-draft (disco-room--current-draft))))

(defun disco-room--attachment-token-action-unavailable-reason (&optional min-count)
  "Return reason attachment actions are unavailable, or nil.

MIN-COUNT optionally requires at least that many queued attachments."
  (or (when (disco-room--composer-edit-active-p)
        "attachments are unavailable while editing a message")
      (let ((count (disco-room--attachment-token-count)))
        (cond
         ((<= (or min-count 1) 0)
          nil)
         ((zerop count)
          "no queued attachments")
         ((and (integerp min-count) (< count min-count))
          (format "need at least %d queued attachments" min-count))))))

(defun disco-room--send-message-unavailable-reason ()
  "Return reason send-message action is unavailable, or nil."
  (let* ((draft (disco-room--current-draft))
         (has-attachments (not (null (disco-room--attachments-from-draft draft))))
         (reply-to (disco-room--composer-reply-message-id))
         (edit-message-id (disco-room--composer-edit-message-id))
         (edit-message (and edit-message-id
                            (disco-room--composer-context-message edit-message-id))))
    (if edit-message-id
        (or (disco-room--edit-permission-reason edit-message)
            (when has-attachments
              "attachments are unavailable while editing a message"))
      (disco-room--room-send-restriction-reason
       (append (when has-attachments '(attach-files))
               (when reply-to '(read-message-history)))))))

(defun disco-room--channel-permission-reason (permissions &optional channel)
  "Return missing-permission reason for PERMISSIONS on CHANNEL, or nil."
  (let* ((channel (or channel (disco-room--channel-object)))
         (missing (and channel
                       (disco-permission-channel-known-p channel)
                       (disco-permission-channel-missing channel permissions nil))))
    (when missing
      (format "missing %s"
              (mapconcat #'disco-permission-display-name missing ", ")))))

(defun disco-room--reaction-unavailable-reason (&optional _msg)
  "Return reason reaction actions are unavailable, or nil."
  (disco-room--room-send-restriction-reason '(add-reactions)))

(defun disco-room--poll-vote-unavailable-reason (&optional msg)
  "Return reason poll voting actions are unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (poll (and (listp msg) (disco-msg-poll msg))))
    (cond
     ((null msg)
      "point is not on a message")
     ((null poll)
      "point is not on a poll")
     ((disco-msg-poll-expired-p poll)
      "poll is closed")
     (t
      (disco-room--room-send-restriction-reason)))))

(defun disco-room--poll-submit-unavailable-reason (&optional msg)
  "Return reason staged poll submit is unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (base-reason (disco-room--poll-vote-unavailable-reason msg)))
    (or base-reason
        (let* ((target-id (alist-get 'id msg))
               (poll (disco-msg-poll msg))
               (staged (disco-room--poll-effective-selection target-id poll))
               (committed (disco-msg-poll-voted-answer-ids poll)))
          (cond
           ((null staged)
            "no staged poll selection")
           ((equal (disco-msg-poll-normalize-answer-id-list staged)
                   (disco-msg-poll-normalize-answer-id-list committed))
            "no pending poll vote changes"))))))

(defun disco-room--poll-clear-unavailable-reason (&optional msg)
  "Return reason clear-poll-votes is unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (base-reason (disco-room--poll-vote-unavailable-reason msg)))
    (or base-reason
        (let* ((poll (and (listp msg) (disco-msg-poll msg)))
               (committed (and poll (disco-msg-poll-voted-answer-ids poll))))
          (unless committed
            "no existing poll vote to remove")))))

(defun disco-room--poll-expire-unavailable-reason (&optional msg)
  "Return reason end-poll is unavailable for MSG, or nil."
  (let* ((msg (or msg (ignore-errors (disco-room--message-at-point))))
         (poll (and (listp msg) (disco-msg-poll msg))))
    (cond
     ((null msg)
      "point is not on a message")
     ((null poll)
      "point is not on a poll")
     ((disco-msg-poll-expired-p poll)
      "poll is already closed")
     ((not (disco-room--poll-owned-by-current-user-p msg nil))
      "only poll author can end this poll")
     (t
      (disco-room--room-send-restriction-reason '(send-polls))))))

(defun disco-room--thread-update-unavailable-reason (&optional channel)
  "Return reason thread update actions are unavailable for CHANNEL, or nil.

This covers operations like rename, lock, slowmode, and auto-archive changes
that require an active, mutable thread."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p (or channel (disco-room--channel-object))))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     (t
      (disco-room--channel-permission-reason '(manage-threads) channel)))))

(defun disco-room--thread-toggle-archived-unavailable-reason (&optional channel)
  "Return reason toggle-thread-archived is unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p (or channel (disco-room--channel-object))))
      "current room is not a thread")
     ((not (disco-thread-archived-p channel))
      (disco-room--channel-permission-reason '(manage-threads) channel))
     ((disco-thread-locked-p channel)
      (disco-room--channel-permission-reason '(manage-threads) channel))
     (t
      (disco-room--channel-permission-reason
       (disco-room--required-send-permissions channel)
       channel)))))

(defun disco-room--thread-joined-p (&optional channel)
  "Return non-nil when current user is known to be joined to thread CHANNEL."
  (let* ((channel (or channel (disco-room--channel-object)))
         (thread-id (and (listp channel) (alist-get 'id channel)))
         (self-id (disco-gateway-current-user-id)))
    (and thread-id
         self-id
         (member (format "%s" self-id)
                 (disco-state-thread-member-ids thread-id)))))

(defun disco-room--thread-join-unavailable-reason (&optional channel)
  "Return reason join-thread is unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p (or channel (disco-room--channel-object))))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     ((disco-room--thread-joined-p channel)
      "already joined to this thread")
     (t nil))))

(defun disco-room--thread-leave-unavailable-reason (&optional channel)
  "Return reason leave-thread is unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p (or channel (disco-room--channel-object))))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     ((not (disco-room--thread-joined-p channel))
      "not joined to this thread")
     (t nil))))

(defun disco-room--thread-mute-unavailable-reason (&optional channel)
  "Return reason set-thread-muted is unavailable for CHANNEL, or nil."
  (let ((channel (or channel (disco-room--channel-object))))
    (cond
     ((not (disco-thread-channel-p (or channel (disco-room--channel-object))))
      "current room is not a thread")
     ((disco-thread-archived-p channel)
      "current thread is archived")
     ((not (disco-room--thread-joined-p channel))
      "join the thread before changing mute state")
     (t nil))))

(defun disco-room--delete-message-unavailable-reason (&optional msg)
  "Return reason delete-message action is unavailable for MSG, or nil."
  (when (listp msg)
    (let* ((channel (disco-room--channel-object))
           (missing-manage
            (and channel
                 (disco-permission-channel-known-p channel)
                 (not (disco-room--message-owned-by-current-user-p msg t))
                 (disco-permission-channel-missing channel '(manage-messages) nil))))
      (when missing-manage
        (format "missing %s"
                (mapconcat #'disco-permission-display-name missing-manage ", "))))))

(defun disco-room--thread-create-unavailable-reason (&optional type)
  "Return reason detached thread creation is unavailable for TYPE, or nil.

When TYPE is `:any', accept either public or private-thread permissions on
regular parent channels. Forum/media parents still require public-thread
creation permission."
  (let ((channel (disco-room--channel-object)))
    (cond
     ((disco-thread-channel-p channel)
      "open a parent channel room to create a new thread")
     ((and channel (not (disco-thread-parent-channel-p channel)))
      "current room channel does not support threads")
     ((not (and channel (disco-permission-channel-known-p channel)))
      nil)
     ((and (eq type :any)
           (not (disco-thread-forum-or-media-channel-p channel)))
      (unless (or (disco-permission-channel-has-p channel 'create-public-threads nil)
                  (disco-permission-channel-has-p channel 'create-private-threads nil))
        (format "missing one of %s"
                (mapconcat #'disco-permission-display-name
                           '(create-public-threads create-private-threads)
                           ", "))))
     (t
      (let* ((permissions
              (if (equal type 12)
                  '(create-private-threads)
                '(create-public-threads)))
             (missing (disco-permission-channel-missing channel permissions nil)))
        (when missing
          (format "missing %s"
                  (mapconcat #'disco-permission-display-name missing ", "))))))))

(defun disco-room--thread-create-from-message-unavailable-reason ()
  "Return reason create-thread-from-message action is unavailable, or nil."
  (disco-room--thread-create-unavailable-reason 11))

(defun disco-room--ensure-action-available (reason action)
  "Signal `user-error' when ACTION is unavailable for REASON."
  (when reason
    (user-error "disco: cannot %s: %s" action reason)))

(defun disco-room--copy-attachment-token-table ()
  "Return deep copy of current attachment token table as alist entries."
  (let (entries)
    (when (hash-table-p disco-room--attachment-token-table)
      (maphash (lambda (token-id entry)
                 (push (cons token-id (copy-tree entry)) entries))
               disco-room--attachment-token-table))
    (nreverse entries)))

(defun disco-room--restore-attachment-token-table (entries)
  "Replace current attachment token table with ENTRIES alist copy."
  (unless (hash-table-p disco-room--attachment-token-table)
    (setq disco-room--attachment-token-table (make-hash-table :test #'equal)))
  (clrhash disco-room--attachment-token-table)
  (dolist (entry entries)
    (puthash (car entry) (copy-tree (cdr entry)) disco-room--attachment-token-table)))

(defun disco-room--composer-edit-saved-state ()
  "Capture composer state to be restored after edit cancel/success."
  (list :draft (disco-chatbuf-copy-string (disco-room--current-draft))
        :reply-to (disco-room--composer-reply-message-id)
        :attachment-token-seq disco-room--attachment-token-seq
        :attachment-token-entries (disco-room--copy-attachment-token-table)))

(defun disco-room--composer-edit-restore-state (state)
  "Restore composer STATE captured by `disco-room--composer-edit-saved-state'."
  (let ((draft (disco-chatbuf-copy-string (plist-get state :draft))))
    (disco-room--set-composer-aux-state nil (plist-get state :reply-to))
    (setq disco-room--attachment-token-seq
          (or (plist-get state :attachment-token-seq) 0))
    (disco-room--restore-attachment-token-table
     (plist-get state :attachment-token-entries))
    (disco-room--apply-draft-state draft :reset-history-p t)))

(defun disco-room--composer-edit-clear (&optional restore-state)
  "Clear active composer edit.

When RESTORE-STATE is non-nil, also restore the saved draft/reply/attachment
state that was present before edit mode was entered."
  (when (disco-room--composer-edit-active-p)
    (let ((saved-state (plist-get disco-room--pending-edit :saved-state)))
      (disco-room--set-composer-aux-state nil nil)
      (when restore-state
        (disco-room--composer-edit-restore-state saved-state)))
    t))

(defun disco-room--composer-context-message (message-id)
  "Return cached local message object for MESSAGE-ID, or nil."
  (and message-id (disco-room--message-by-id message-id)))

(defun disco-room--composer-context-text (action message-id)
  "Return multi-line composer context text for ACTION and MESSAGE-ID."
  (let* ((msg (disco-room--composer-context-message message-id))
         (author (and msg (disco-room--message-author msg)))
         (preview (and msg (disco-msg-preview-content msg)))
         (headline
          (if author
              (format "%s %s [%s]" action author message-id)
            (format "%s: %s" action message-id))))
    (concat
     headline
     " (C-c C-k to cancel)\n"
     (if (and (stringp preview) (not (string-empty-p preview)))
         (format "> %s\n" preview)
       ""))))

(defun disco-room--composer-enter-edit (msg)
  "Enter composer edit mode for MSG."
  (let* ((message-id (alist-get 'id msg))
         (old-content (or (alist-get 'content msg) ""))
         (saved-state (disco-room--composer-edit-saved-state)))
    (unless (and message-id (not (string-empty-p (format "%s" message-id))))
      (user-error "disco: message id is unavailable for edit"))
    (when (disco-room--composer-edit-active-p)
      (disco-room--composer-edit-clear t))
    (disco-room--set-composer-aux-state
     (list :type 'edit
           :message-id message-id
           :saved-state saved-state)
     nil)
    (disco-room--apply-draft-state old-content :reset-history-p t)
    (setq disco-room--attachment-token-seq 0)
    (when (hash-table-p disco-room--attachment-token-table)
      (clrhash disco-room--attachment-token-table))
    (disco-room--update-frame)
    (message "disco: editing message %s in composer" message-id)))

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
  (let ((poll (disco-msg-poll msg)))
    (and poll
         (not (disco-msg-poll-expired-p poll))
         (disco-permission-channel-has-all-p
          (disco-room--channel-object)
          (disco-room--poll-vote-required-permissions)))))

(defun disco-room--poll-can-expire-p (msg)
  "Return non-nil when current user can end poll message MSG."
  (let ((poll (disco-msg-poll msg)))
    (and poll
         (not (disco-msg-poll-expired-p poll))
         (disco-room--poll-owned-by-current-user-p msg t)
         (disco-permission-channel-has-all-p
          (disco-room--channel-object)
          (disco-room--poll-expire-required-permissions)))))

(defun disco-room--typing-timeout-seconds ()
  "Return normalized typing indicator timeout in seconds."
  (disco-typing-timeout-seconds disco-room-typing-indicator-timeout))

(defun disco-room--typing-normalize-user-id (user-id)
  "Return USER-ID as string, or nil when missing."
  (disco-typing-normalize-user-id user-id))

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
  (disco-typing-prune-expired disco-room--typing-users now))

(defun disco-room--typing-active-entries ()
  "Return active typing entries sorted by recent typing timestamp."
  (disco-typing-active-entries disco-room--typing-users (float-time)))

(defun disco-room--typing-indicator-text ()
  "Return one-line typing indicator text, or nil when idle."
  (when disco-room-show-typing-indicators
    (disco-typing-indicator-text-from-table
     disco-room--typing-users
     (float-time))))

(defun disco-room--typing-next-expiry ()
  "Return nearest typing expiry timestamp, or nil when none remain."
  (disco-typing-next-expiry disco-room--typing-users))

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
        (unless (disco-chatbuf-rendering-p)
          (disco-room--update-frame)))
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
                (unless (disco-chatbuf-rendering-p)
                  (disco-room--update-frame))))))))))

(defun disco-room--typing-stop-user (user-id &optional no-rerender)
  "Remove USER-ID from typing indicators.

When NO-RERENDER is non-nil, update local state without rendering."
  (let ((normalized-id (disco-room--typing-normalize-user-id user-id)))
    (when (and normalized-id
               (hash-table-p disco-room--typing-users)
               (gethash normalized-id disco-room--typing-users))
      (remhash normalized-id disco-room--typing-users)
      (disco-room--typing-reschedule-expire-timer)
      (unless (or no-rerender (disco-chatbuf-rendering-p))
        (disco-room--update-frame)))))

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
  "Return current room draft string, preserving text properties."
  (disco-chatbuf-input-state))

(defun disco-room--attachment-input-object-p (object)
  "Return non-nil when OBJECT is a queued attachment input object."
  (and (listp object)
       (eq (plist-get object :kind) disco-room--input-object-kind-attachment)))

(cl-defun disco-room--make-attachment-input-object (path &key description filename content-type)
  "Build one structured attachment input object for PATH."
  (unless (and (stringp path) (not (string-empty-p path)))
    (user-error "disco: attachment object requires a file path"))
  (let* ((resolved-filename (or filename (file-name-nondirectory path)))
         (trimmed-description (and (stringp description)
                                   (not (string-empty-p (string-trim description)))
                                   (string-trim description))))
    (list :kind disco-room--input-object-kind-attachment
          :path path
          :filename resolved-filename
          :description trimmed-description
          :content-type content-type)))

(defun disco-room--attachment-input-object-display-text (attachment)
  "Return visible composer text for ATTACHMENT input object."
  (format "[file] %s"
          (or (plist-get attachment :filename)
              (file-name-nondirectory (or (plist-get attachment :path) ""))
              "unnamed")))

(defun disco-room--attachment-input-object-string (attachment)
  "Return one propertized draft string representing ATTACHMENT."
  (let* ((object (copy-tree attachment))
         (text (disco-room--attachment-input-object-display-text object))
         (len (length text)))
    (add-text-properties
     0 len
     (list disco-chatbuf-input-object-property object
           'face 'disco-chatbuf-input-object)
     text)
    (when (> len 0)
      (add-text-properties 0 1
                           (list disco-chatbuf-input-object-start-property t)
                           text)
      (add-text-properties (1- len) len
                           (list disco-chatbuf-input-object-end-property t)
                           text))
    text))

(defun disco-room--insert-attachment-input-object (attachment)
  "Insert ATTACHMENT as one structured composer object at point."
  (let ((object (copy-tree attachment)))
    (when (and (disco-chatbuf-point-in-input-p)
               (> (point) (or (disco-chatbuf-input-start-position) (point-min)))
               (let ((before (char-before)))
                 (and before (not (memq before '(?\s ?\t ?\n))))))
      (disco-chatbuf-input-insert " "))
    (disco-chatbuf-input-insert
     (disco-room--attachment-input-object-display-text object)
     :object object)
    (when (let ((after (char-after)))
            (and after (not (memq after '(?\s ?\t ?\n)))))
      (disco-chatbuf-input-insert " "))
    object))

(defun disco-room--attachment-input-object-to-attachment (object)
  "Convert structured attachment OBJECT into upload plist."
  (when (disco-room--attachment-input-object-p object)
    (let ((attachment
           (list :path (plist-get object :path)
                 :filename (or (plist-get object :filename)
                               (file-name-nondirectory
                                (or (plist-get object :path) ""))))))
      (when-let* ((description (plist-get object :description)))
        (setq attachment (plist-put attachment :description description)))
      (when-let* ((content-type (plist-get object :content-type)))
        (setq attachment (plist-put attachment :content-type content-type)))
      attachment)))

(defun disco-room--attachment-label (attachment prefix)
  "Return one user-facing label for ATTACHMENT using PREFIX."
  (let* ((path (or (plist-get attachment :path) ""))
         (filename (or (plist-get attachment :filename)
                       (and (not (string-empty-p path))
                            (file-name-nondirectory path))
                       "missing"))
         (description (or (plist-get attachment :description) "")))
    (if (string-empty-p description)
        (format "%s %s" prefix filename)
      (format "%s %s - %s" prefix filename description))))

(defun disco-room--draft-substring-delete (draft start end)
  "Return DRAFT with region START..END removed, preserving properties."
  (concat (substring draft 0 start)
          (substring draft end)))

(defun disco-room--draft-substring-replace (draft start end replacement)
  "Return DRAFT with region START..END replaced by REPLACEMENT."
  (concat (substring draft 0 start)
          replacement
          (substring draft end)))

(defun disco-room--attachment-refs (&optional draft)
  "Return ordered attachment refs found in DRAFT."
  (let* ((text (or draft (disco-room--current-draft)))
         (len (length text))
         (pos 0)
         (refs '()))
    (while (< pos len)
      (let ((object (get-text-property pos disco-chatbuf-input-object-property text)))
        (if (disco-room--attachment-input-object-p object)
            (let* ((end (or (next-single-property-change
                             pos disco-chatbuf-input-object-property text len)
                            len))
                   (object-copy (copy-tree object))
                   (attachment (disco-room--attachment-input-object-to-attachment object-copy)))
              (push (list :type 'object
                          :start pos
                          :end end
                          :object object-copy
                          :attachment attachment
                          :label (disco-room--attachment-label attachment "[file]"))
                    refs)
              (setq pos end))
          (let* ((next-object (or (next-single-property-change
                                   pos disco-chatbuf-input-object-property text len)
                                  len))
                 (chunk (substring-no-properties text pos next-object))
                 (chunk-pos 0))
            (while (string-match disco-room--attachment-token-regexp chunk chunk-pos)
              (let* ((token-id (match-string 1 chunk))
                     (attachment (copy-tree (or (disco-room--attachment-by-token-id token-id)
                                                (list :token-id token-id))))
                     (start (+ pos (match-beginning 0)))
                     (end (+ pos (match-end 0))))
                (push (list :type 'token
                            :start start
                            :end end
                            :token-id token-id
                            :attachment attachment
                            :label (disco-room--attachment-label
                                    attachment
                                    (disco-room--attachment-token-text token-id)))
                      refs))
              (setq chunk-pos (match-end 0)))
            (setq pos next-object)))))
    (nreverse refs)))

(defun disco-room--choose-attachment-ref (prompt)
  "Prompt for one queued attachment ref using PROMPT."
  (let* ((refs (disco-room--attachment-refs))
         (labels (mapcar (lambda (ref) (plist-get ref :label)) refs))
         (picked (completing-read prompt labels nil t)))
    (or (seq-find (lambda (ref)
                    (equal (plist-get ref :label) picked))
                  refs)
        (user-error "disco: invalid attachment selection"))))

(defun disco-room--attachment-ref-string (ref)
  "Return serialized draft text for attachment REF."
  (pcase (plist-get ref :type)
    ('object
     (disco-room--attachment-input-object-string
      (or (plist-get ref :object)
          (disco-room--make-attachment-input-object
           (plist-get (plist-get ref :attachment) :path)
           :filename (plist-get (plist-get ref :attachment) :filename)
           :description (plist-get (plist-get ref :attachment) :description)
           :content-type (plist-get (plist-get ref :attachment) :content-type)))))
    (_
     (disco-room--attachment-token-text (plist-get ref :token-id)))))

(defun disco-room--draft-input-objects (&optional draft)
  "Return ordered structured input objects found in DRAFT."
  (let* ((text (or draft (disco-room--current-draft)))
         (len (length text))
         (pos 0)
         (objects '()))
    (while (< pos len)
      (let ((object (get-text-property pos disco-chatbuf-input-object-property text)))
        (if object
            (let ((end (or (next-single-property-change
                            pos disco-chatbuf-input-object-property text len)
                           len)))
              (push (copy-tree object) objects)
              (setq pos end))
          (setq pos (or (next-single-property-change
                         pos disco-chatbuf-input-object-property text len)
                        len)))))
    (nreverse objects)))

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

(defun disco-room--parse-draft-input (&optional draft)
  "Parse DRAFT into plain content, structured objects, and attachment uploads."
  (let* ((text (or draft (disco-room--current-draft)))
         (len (length text))
         (pos 0)
         (content-parts '())
         (objects '())
         (attachments '())
         (token-ids '())
         (seen-token-ids (make-hash-table :test #'equal)))
    (while (< pos len)
      (let ((object (get-text-property pos disco-chatbuf-input-object-property text)))
        (if object
            (let* ((end (or (next-single-property-change
                             pos disco-chatbuf-input-object-property text len)
                            len))
                   (object-copy (copy-tree object)))
              (push object-copy objects)
              (when-let* ((attachment
                           (disco-room--attachment-input-object-to-attachment object-copy)))
                (push attachment attachments))
              (setq pos end))
          (let* ((end (or (next-single-property-change
                           pos disco-chatbuf-input-object-property text len)
                          len))
                 (chunk (substring-no-properties text pos end))
                 (content-chunk
                  (replace-regexp-in-string disco-room--attachment-token-regexp "" chunk)))
            (push content-chunk content-parts)
            (dolist (token-id (disco-room--attachment-token-ids-in-text chunk))
              (unless (gethash token-id seen-token-ids)
                (puthash token-id t seen-token-ids)
                (push token-id token-ids)
                (when-let* ((attachment (disco-room--attachment-by-token-id token-id)))
                  (push (copy-tree attachment) attachments))))
            (setq pos end)))))
    (list :content (mapconcat #'identity (nreverse content-parts) "")
          :objects (nreverse objects)
          :attachments (nreverse attachments)
          :token-ids (nreverse token-ids))))

(defun disco-room--attachments-from-draft (&optional draft)
  "Return ordered attachment list referenced by DRAFT."
  (plist-get (disco-room--parse-draft-input draft) :attachments))

(defun disco-room--draft-without-attachment-tokens (&optional draft)
  "Return DRAFT plain content with attachment placeholders removed."
  (plist-get (disco-room--parse-draft-input draft) :content))

(defun disco-room--sync-pending-attachments-from-draft (&optional draft)
  "Refresh `disco-room--pending-attachments' using parsed DRAFT references."
  (setq disco-room--pending-attachments
        (disco-room--attachments-from-draft draft)))

(defun disco-room--prune-unused-attachment-tokens (&optional draft)
  "Remove token table entries that are not referenced in DRAFT."
  (let ((alive (make-hash-table :test #'equal)))
    (dolist (token-id (plist-get (disco-room--parse-draft-input draft) :token-ids))
      (puthash token-id t alive))
    (when disco-room--attachment-token-table
      (maphash
       (lambda (token-id _attachment)
         (unless (gethash token-id alive)
           (remhash token-id disco-room--attachment-token-table)))
       disco-room--attachment-token-table))))

(defun disco-room--apply-input-text-properties ()
  "Normalize current draft text properties after redraws and edits."
  (disco-chatbuf-input-apply-text-properties)
  (when-let* ((bounds (disco-chatbuf-input-region-bounds)))
    (with-silent-modifications
      (add-text-properties
       (car bounds) (cdr bounds)
       '(disco-room-input t)))))

(defun disco-room--update-context-mode ()
  "Enable timeline bindings only when point is outside the room draft."
  (let ((timeline-p (not (disco-chatbuf-point-in-input-p))))
    (unless (eq disco-room-timeline-mode timeline-p)
      (disco-room-timeline-mode (if timeline-p 1 -1)))))

(defun disco-room--post-command ()
  "Keep point out of prompt glyphs and hide revealed spoilers when leaving a row."
  (unless (disco-chatbuf-rendering-p)
    (disco-chatbuf-post-command-clamp-point)
    (let ((current-message-id (or (get-text-property (point) 'disco-message-id)
                                  (get-text-property (line-beginning-position)
                                                     'disco-message-id))))
      (when (and disco-room--revealed-spoiler-message-id
                 (not (equal current-message-id
                             disco-room--revealed-spoiler-message-id)))
        (let ((previous disco-room--revealed-spoiler-message-id))
          (setq disco-room--revealed-spoiler-message-id nil)
          (disco-room--invalidate-message-node previous))))
    (disco-room--update-context-mode)))

(defun disco-room--sync-draft-from-buffer ()
  "Sync shared chatbuf draft cache from editable input region."
  (let ((text (plist-get (disco-chatbuf-input-state-sync)
                         :value)))
    (disco-room--prune-unused-attachment-tokens text)
    (disco-room--sync-pending-attachments-from-draft text)))

(defun disco-room--after-change (beg end _old-len)
  "Keep draft state synced after editable-region changes from BEG to END."
  (disco-chatbuf-after-change
   beg end
   :rendering-p (disco-chatbuf-rendering-p)
   :prune-broken-objects t
   :sync-function #'disco-room--sync-draft-from-buffer))

(cl-defun disco-room--apply-draft-state (text &key reset-history-p)
  "Apply draft TEXT to cache/live input and return update metadata.

When a visible tail input exists and attachment-derived footer state is
unchanged, update the live input directly in telega-like fashion.  Otherwise,
callers can use the returned metadata to decide whether a frame refresh is
needed."
  (let ((old-attachments (copy-tree disco-room--pending-attachments))
        (live-input-p (and (not (disco-chatbuf-rendering-p))
                           (disco-chatbuf-input-start-position))))
    (let ((draft
           (disco-chatbuf-input-state-set
            text
            :reset-history-p reset-history-p)))
      (disco-room--prune-unused-attachment-tokens draft)
      (disco-room--sync-pending-attachments-from-draft draft)
      (let ((attachments-changed-p
             (not (equal old-attachments disco-room--pending-attachments))))
        (when (and live-input-p (not attachments-changed-p))
          (disco-chatbuf-with-structural-update
            (disco-chatbuf-input-replace draft)
            (disco-room--apply-input-text-properties)))
        (list :draft (disco-chatbuf-copy-string draft)
              :attachments-changed-p attachments-changed-p
              :live-input-updated-p (and live-input-p
                                         (not attachments-changed-p)))))))

(defun disco-room--set-draft (text)
  "Set room draft TEXT and refresh composer surfaces as needed."
  (let ((result (disco-room--apply-draft-state text)))
    (when (plist-get result :attachments-changed-p)
      (disco-room--update-frame))))

(defun disco-room--clear-draft ()
  "Clear room draft and reset draft history navigation state."
  (let ((result (disco-room--apply-draft-state "" :reset-history-p t)))
    (when (plist-get result :attachments-changed-p)
      (disco-room--update-frame))))

(defun disco-room-draft-prev (&optional n)
  "Replace draft with N previous entries from draft history."
  (interactive "p")
  (let ((result (disco-chatbuf-input-history-prev-value
                 (disco-room--current-draft)
                 n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-room--set-draft (plist-get result :value)))
      (_
       (message "disco: draft history is empty")))))

(defun disco-room-draft-next (&optional n)
  "Replace draft with N newer entries from draft history."
  (interactive "p")
  (let ((result (disco-chatbuf-input-history-next-value n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-room--set-draft (plist-get result :value)))
      (_
       (message "disco: already at latest draft")))))

(defun disco-room-edit-draft ()
  "Edit current room draft in minibuffer and re-render room."
  (interactive)
  (when (disco-chatbuf-string-has-objects-p (disco-room--current-draft))
    (user-error "disco: minibuffer draft editing is unavailable for structured input objects"))
  (let ((updated (read-from-minibuffer
                  "Draft: "
                  (disco-chatbuf-string-plain-text (disco-room--current-draft)))))
    (disco-chatbuf-input-history-reset)
    (disco-room--set-draft updated)))

(defun disco-room--read-state-snapshot-fields (state)
  "Return writable read-state fields copied from STATE."
  (let (fields)
    (dolist (field '(last_message_id
                     mention_count
                     last_pin_timestamp
                     flags
                     last_viewed
                     version))
      (when (assq field state)
        (push (cons field (alist-get field state)) fields)))
    (nreverse fields)))

(defun disco-room--restore-channel-read-state (channel-id state ack-token)
  "Restore CHANNEL-ID read STATE and ACK-TOKEN snapshot."
  (if state
      (disco-state--upsert-read-state
       disco-read-state-type-channel
       channel-id
       (disco-room--read-state-snapshot-fields state))
    (progn
      (disco-state--delete-read-state disco-read-state-type-channel channel-id)
      (when ack-token
        (disco-state-set-channel-ack-token channel-id ack-token)))))

(defun disco-room--optimistic-read-ack-begin (channel-id target-id ack-fields)
  "Apply optimistic read ACK for CHANNEL-ID/TARGET-ID and return op seq."
  (let ((seq (1+ disco-room--optimistic-read-ack-seq)))
    (setq disco-room--optimistic-read-ack-seq seq)
    (setq disco-room--pending-optimistic-read-ack
          (list :seq seq
                :channel-id channel-id
                :target-id target-id
                :previous-state (disco-state-read-state
                                 disco-read-state-type-channel channel-id)
                :previous-token (disco-state-channel-ack-token channel-id)))
    (disco-state-apply-message-ack
     channel-id
     target-id
     0
     (plist-get ack-fields :flags)
     (plist-get ack-fields :last-viewed))
    (disco-room--apply-read-state-change)
    seq))

(defun disco-room--optimistic-read-ack-clear (seq)
  "Clear pending optimistic read ACK when it matches SEQ."
  (when (and (listp disco-room--pending-optimistic-read-ack)
             (= (or (plist-get disco-room--pending-optimistic-read-ack :seq) -1)
                seq))
    (setq disco-room--pending-optimistic-read-ack nil)
    t))

(defun disco-room--optimistic-read-ack-confirm (message-id)
  "Confirm pending optimistic read ACK using MESSAGE-ID from gateway/server."
  (when (and (listp disco-room--pending-optimistic-read-ack)
             (stringp message-id))
    (let ((target-id (plist-get disco-room--pending-optimistic-read-ack :target-id)))
      (when (and (stringp target-id)
                 (or (equal message-id target-id)
                     (disco-state-snowflake< target-id message-id)))
        (setq disco-room--pending-optimistic-read-ack nil)
        t))))

(defun disco-room--optimistic-read-ack-rollback (seq)
  "Rollback pending optimistic read ACK when it still matches SEQ."
  (when (and (listp disco-room--pending-optimistic-read-ack)
             (= (or (plist-get disco-room--pending-optimistic-read-ack :seq) -1)
                seq))
    (let ((channel-id (plist-get disco-room--pending-optimistic-read-ack :channel-id))
          (previous-state (plist-get disco-room--pending-optimistic-read-ack :previous-state))
          (previous-token (plist-get disco-room--pending-optimistic-read-ack :previous-token)))
      (setq disco-room--pending-optimistic-read-ack nil)
      (disco-room--restore-channel-read-state channel-id previous-state previous-token)
      (disco-room--apply-read-state-change)
      t)))

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
    (if should-ack
        (let* ((ack-fields (disco-state-channel-ack-request-fields channel-id))
               (optimistic-seq (disco-room--optimistic-read-ack-begin
                                channel-id target-id ack-fields)))
          (disco-api-ack-message-async
           channel-id
           target-id
           :token (plist-get ack-fields :token)
           :flags (plist-get ack-fields :flags)
           :last-viewed (plist-get ack-fields :last-viewed)
           :on-success
           (lambda (response)
             (when (disco-room--callback-active-p room-buffer channel-id generation)
               (with-current-buffer room-buffer
                 (disco-room--optimistic-read-ack-clear optimistic-seq)
                 (disco-state-apply-message-ack channel-id target-id 0)
                 (disco-state-apply-channel-ack-response channel-id response)
                 (disco-room--apply-read-state-change))))
           :on-error
           (lambda (err)
             (when (disco-room--callback-active-p room-buffer channel-id generation)
               (with-current-buffer room-buffer
                 (disco-room--optimistic-read-ack-rollback optimistic-seq)))
             (message "disco: read-state ack failed for %s: %s"
                      channel-id
                      (disco-room--async-error-message err)))))
      (disco-state-apply-message-ack channel-id nil 0))))

(defun disco-room-ack-channel-pins ()
  "Acknowledge currently pinned messages in the active room channel."
  (interactive)
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (generation disco-room--refresh-generation)
         (channel (disco-room--channel-object))
         (last-pin-timestamp (and channel (alist-get 'last_pin_timestamp channel))))
    (cond
     ((not channel-id)
      (user-error "disco: room is not bound to a channel"))
     ((not (stringp last-pin-timestamp))
      (message "disco: channel %s has no pinned messages" channel-id))
     ((not (disco-state-channel-has-unread-pins-p channel))
      (message "disco: pins already acknowledged for %s" channel-id))
     (t
      (disco-api-ack-channel-pins-async
       channel-id
       :on-success
       (lambda (_response)
         (when (disco-room--callback-active-p room-buffer channel-id generation)
           (disco-state-apply-channel-pins-ack channel-id last-pin-timestamp)
           (message "disco: acknowledged pins for %s" channel-id)))
       :on-error
       (lambda (err)
         (message "disco: pins ack failed for %s: %s"
                  channel-id
                  (disco-room--async-error-message err))))))))

(defun disco-room--update-message-window-state (messages)
  "Update pagination cursors from MESSAGES (newest-first list)."
  (setq disco-room--newest-message-id (and messages (alist-get 'id (car messages))))
  (setq disco-room--oldest-message-id
        (and messages (alist-get 'id (car (last messages))))))

(defun disco-room--message-id-at-point ()
  "Return message ID at point, or signal a user error.

Message lines carry the `disco-message-id' text property."
  (or (get-text-property (point) 'disco-message-id)
      (get-text-property (line-beginning-position) 'disco-message-id)
      (user-error "disco: point is not on a message line")))

(defun disco-room--message-spoilers-revealed-p (message-id)
  "Return non-nil when MESSAGE-ID currently shows revealed spoilers."
  (and (stringp message-id)
       (equal message-id disco-room--revealed-spoiler-message-id)))

(defun disco-room--invalidate-message-node (message-id)
  "Invalidate the rendered node for MESSAGE-ID when present."
  (when (and message-id
             (disco-chat-timeline-live-p)
             (disco-chat-timeline-node message-id))
    (disco-chat-timeline-invalidate (list message-id))))

(defun disco-room--redisplay-msg (msg)
  "Force MSG to be rerendered in the current room."
  (let ((message-id (and (listp msg) (alist-get 'id msg))))
    (unless (and (stringp message-id) (not (string-empty-p message-id)))
      (user-error "disco: message has no id to redisplay"))
    (disco-room--invalidate-message-node message-id)))

(defun disco-room-toggle-message-spoilers (message-id)
  "Toggle all rendered spoilers for MESSAGE-ID, telega-style."
  (interactive (list (disco-room--message-id-at-point)))
  (unless (stringp message-id)
    (user-error "disco: invalid spoiler message id"))
  (let ((previous disco-room--revealed-spoiler-message-id))
    (setq disco-room--revealed-spoiler-message-id
          (unless (equal previous message-id)
            message-id))
    (when (and previous (not (equal previous message-id)))
      (disco-room--invalidate-message-node previous))
    (disco-room--invalidate-message-node message-id)))

(defun disco-room--filtered-message-by-id (message-id)
  "Return filtered room message object for MESSAGE-ID, or nil."
  (when-let* ((items (and (listp disco-room--msg-filter)
                          (plist-get disco-room--msg-filter :items))))
    (seq-find (lambda (message)
                (equal (alist-get 'id message) message-id))
              items)))

(defun disco-room--message-by-id (message-id)
  "Return room message object for MESSAGE-ID, or nil."
  (or (disco-msg-find-in-channel disco-room--channel-id message-id)
      (disco-room--filtered-message-by-id message-id)))

(defun disco-room--channel-message-by-id (channel-id message-id)
  "Return cached MESSAGE-ID from CHANNEL-ID, or nil."
  (disco-msg-find-in-channel channel-id message-id))

(defun disco-room--resolve-message (message-id &optional channel-id _position)
  "Resolve MESSAGE-ID in current room context, optionally using CHANNEL-ID."
  (let ((target-channel-id (disco-msg-normalize-id (or channel-id disco-room--channel-id))))
    (if (and target-channel-id
             (equal target-channel-id
                    (disco-msg-normalize-id disco-room--channel-id)))
        (disco-room--message-by-id message-id)
      (disco-room--channel-message-by-id target-channel-id message-id))))

(defun disco-room--msg-filter-active-p ()
  "Return non-nil when a room message filter is currently active."
  (and (listp disco-room--msg-filter)
       (plist-get disco-room--msg-filter :active)))

(defun disco-room--display-messages ()
  "Return message list currently displayed in the room buffer."
  (if (disco-room--msg-filter-active-p)
      (or (plist-get disco-room--msg-filter :items) '())
    (or (disco-state-messages disco-room--channel-id) '())))

(defun disco-room--message-position (message-id)
  "Return buffer position for MESSAGE-ID in current room render, or nil."
  (when (and (stringp message-id)
             (not (string-empty-p message-id))
             (disco-chat-timeline-live-p))
    (disco-chat-timeline-key-position message-id)))

(defcustom disco-room-jump-context-limit 50
  "Number of messages to request around a jump target.

Used by `disco-room-jump-to-message' to center the timeline around the
requested message instead of linearly paginating backward from the latest
page."
  :type 'integer
  :group 'disco)

(defun disco-room--message-list-contains-id-p (messages message-id)
  "Return non-nil when MESSAGES contains MESSAGE-ID."
  (seq-some (lambda (message)
              (equal (alist-get 'id message) message-id))
            (or messages '())))

(defun disco-room--sort-messages-newest-first (messages)
  "Return MESSAGES sorted newest-first by Discord snowflake id."
  (sort (copy-sequence (or messages '()))
        (lambda (left right)
          (let ((left-id (alist-get 'id left))
                (right-id (alist-get 'id right)))
            (cond
             ((equal left-id right-id)
              nil)
             ((null left-id)
              nil)
             ((null right-id)
              t)
             (t
              (disco-state-snowflake< right-id left-id)))))))

(defun disco-room--merge-message-sets (&rest message-lists)
  "Merge MESSAGE-LISTS into one newest-first list without duplicates."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (messages message-lists)
      (dolist (message (or messages '()))
        (let ((message-id (alist-get 'id message)))
          (unless (and message-id (gethash message-id seen))
            (when message-id
              (puthash message-id t seen))
            (push message merged)))))
    (disco-room--sort-messages-newest-first (nreverse merged))))

(defun disco-room--fetch-around-pending-jump ()
  "Fetch one message page around current pending jump target."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (target-id disco-room--pending-jump-message-id)
         (generation (1+ disco-room--refresh-generation))
         (request-revision (disco-state-message-revision channel-id))
         (limit (max 1 (or disco-room-jump-context-limit 50))))
    (unless (and (stringp target-id) (not (string-empty-p target-id)))
      (user-error "disco: pending jump target is empty"))
    (setq disco-room--refresh-generation generation)
    (setq disco-room--jump-in-flight t)
    (setq disco-room--refresh-in-flight t)
    (disco-room--update-frame)
    (disco-api-channel-messages-around-async
     channel-id
     target-id
     :limit limit
     :on-success
     (lambda (messages)
       (when (disco-room--callback-active-p room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq disco-room--jump-in-flight nil)
           (setq disco-room--refresh-in-flight nil)
           (setq disco-room--history-exhausted nil)
           (if (disco-room--message-list-contains-id-p messages target-id)
               (progn
                 (let ((merged (disco-state-merge-message-page
                                channel-id messages request-revision)))
                   (disco-room--update-message-window-state merged))
                 (disco-room-render)
                 (setq disco-room--pending-jump-message-id nil)
                 (if (disco-room--jump-to-visible-message target-id)
                     (message "disco: jumped to message %s" target-id)
                   (message "disco: failed to render jump target %s" target-id)))
             (setq disco-room--pending-jump-message-id nil)
             (disco-room--update-frame)
             (message "disco: message %s not found in around fetch" target-id)))))
     :on-error
     (lambda (err)
       (when (disco-room--callback-active-p room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq disco-room--jump-in-flight nil)
           (setq disco-room--refresh-in-flight nil)
           (setq disco-room--pending-jump-message-id nil)
           (disco-room--update-frame)
           (message "disco: jump fetch failed: %s"
                    (disco-room--async-error-message err))))))))

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
      (unless disco-room--jump-in-flight
        (disco-room--fetch-around-pending-jump)))))

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
      (if (disco-permission-channel-known-p channel)
          (disco-permission-ensure-channel
           channel
           required
           :unknown-value nil
           :action (format "jump target channel %s" channel-id))
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

When MESSAGE-ID is not currently visible, fetch one page centered around the
message id and render that context before jumping."
  (interactive
   (list (read-string "Jump to message ID: "
                      (or (ignore-errors (disco-room--message-id-at-point))
                          ""))
         nil))
  (let* ((target-id (disco-msg-normalize-id message-id))
         (target-channel (disco-msg-normalize-id (or channel-id disco-room--channel-id)))
         (current-channel (disco-msg-normalize-id disco-room--channel-id)))
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

(defun disco-room--open-thread-from-message (msg)
  "Open starter thread associated with MSG."
  (let* ((message-id (alist-get 'id msg))
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

(defun disco-room-open-thread-from-message-at-point ()
  "Open starter thread associated with message at point.

Discord starter threads reuse source message ID as thread channel ID."
  (interactive)
  (disco-room--open-thread-from-message (disco-room--message-at-point)))

(defun disco-room--message-at-point ()
  "Return message object at point, or signal user error."
  (or (disco-msg-at)
      (user-error "disco: message not found in local room cache")))

(defun disco-room--read-optional-nonnegative-int (prompt)
  "Read optional non-negative integer using PROMPT.

Returns nil when left blank."
  (let ((raw (read-string prompt)))
    (unless (string-empty-p raw)
      (let ((n (string-to-number raw)))
        (when (< n 0)
          (user-error "disco: value must be >= 0"))
        n))))

(defun disco-room--resolve-thread-update (updated)
  "Store complete UPDATED thread channel response."
  (disco-thread-resolve-update
   updated
   (lambda (channel)
     (when (alist-get 'name channel)
       (setq disco-room--channel-name (alist-get 'name channel))))))

(defun disco-room--buffer-name (channel-name channel-id)
  "Build room buffer name for CHANNEL-NAME and CHANNEL-ID."
  (format "*disco:%s (%s)*" channel-name channel-id))

(defun disco-room--render-window ()
  "Return best live window currently displaying this room buffer."
  (let ((best nil)
        (best-width -1))
    (dolist (win (get-buffer-window-list (current-buffer) nil t) best)
      (let ((width (if (window-live-p win)
                       (window-width win 'remap)
                     -1)))
        (when (> width best-width)
          (setq best win)
          (setq best-width width))))))

(defun disco-room--compute-chat-fill-column (&optional win)
  "Compute telega-like chat fill column for WIN.

When WIN is nil, use best room window from `disco-room--render-window'."
  (disco-view-window-fill-column
   (or win (disco-room--render-window))
   disco-room-auto-fill-margin-columns))

(defun disco-room--update-chat-fill-column (&optional win)
  "Refresh cached chat fill column from WIN or current room window."
  (let ((next (disco-room--compute-chat-fill-column win)))
    (when (numberp next)
      (setq-local disco-room--chat-fill-column next))
    next))

(defun disco-room--on-window-size-change (&optional _frame)
  "Recompute chat fill column and rerender on room window size changes."
  (when (eq major-mode 'disco-room-mode)
    (let ((old disco-room--chat-fill-column)
          (next (disco-room--update-chat-fill-column)))
      (when (and (numberp next)
                 (not (equal old next)))
        (disco-room-render)))))

(defun disco-room--line-fill-column ()
  "Return target fill column for current message line."
  (or (and (bound-and-true-p visual-fill-column-mode)
           (integerp disco-room-fill-column)
           (> disco-room-fill-column 0)
           disco-room-fill-column)
      (and (bound-and-true-p visual-fill-column-mode)
           (integerp fill-column)
           (> fill-column 0)
           fill-column)
      (and (integerp disco-room--chat-fill-column)
           (> disco-room--chat-fill-column 0)
           disco-room--chat-fill-column)
      (disco-room--update-chat-fill-column)
      80))

(defun disco-room--insert-right-aligned-text (text &optional face left-prefix-width)
  "Insert TEXT aligned to right edge on current line.

When FACE is non-nil, apply FACE to TEXT.  LEFT-PREFIX-WIDTH reserves
additional columns at line start (for future `line-prefix' application)."
  (disco-ins-insert-right-aligned-text
   text
   (disco-room--line-fill-column)
   :face face
   :right-align-p disco-room-right-align-timestamps
   :left-prefix-width left-prefix-width))

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
       ;; System divider messages break compact groups.
       (not (disco-room--message-system-divider-p previous))
       (not (disco-room--message-system-divider-p current))
       (disco-room--same-sender-p previous current)
       (let ((previous-time (disco-msg-time-epoch previous))
             (current-time (disco-msg-time-epoch current)))
         (and previous-time
              current-time
              (<= (abs (- current-time previous-time))
                  (max 0 disco-room-group-messages-timespan))))))

(defun disco-room--insert-divider-row (text face)
  "Insert read-only divider row TEXT with FACE, spanning full window width."
  (disco-ins-insert-divider-row
   text face (disco-room--line-fill-column)))

(defun disco-room--insert-date-separator-row (day-key)
  "Insert date separator row for DAY-KEY."
  (disco-room--insert-divider-row
   (disco-msg-day-label day-key)
   'disco-room-date-separator))

(defun disco-room--insert-unread-divider-row ()
  "Insert unread separator row."
  (disco-room--insert-divider-row
   "Unread Messages"
   'disco-room-unread-divider))

(defun disco-room--insert-system-divider-message (msg context)
  "Insert MSG with projected CONTEXT as a centered system divider line.

The message content is rendered as ────( avatar content )──── with
horizontal bars filling both sides to span the full line width.
The author name is propertized with its colour face and an inline
avatar image is prepended when available."
  (let* ((insert-date (plist-get context :insert-date))
         (insert-unread (disco-util-json-true-p (plist-get context :insert-unread)))
         (message-id (alist-get 'id msg))
         (content (disco-room--message-display-content msg))
         (author (disco-room--message-author msg))
         (author-face (disco-room--author-face msg))
         (avatar-str (disco-room--avatar-one-line-string msg))
         (label (concat avatar-str content)))
    (when (and (stringp insert-date) (not (string-empty-p insert-date)))
      (disco-room--insert-date-separator-row insert-date))
    (when insert-unread
      (disco-room--insert-unread-divider-row))
    (let ((span (disco-ins-insert-full-width-divider
                 label 'disco-room-system-divider
                 (disco-room--line-fill-column)
                 (list 'read-only t
                       'front-sticky '(read-only)
                       'rear-nonsticky '(read-only)
                       'disco-message-id message-id))))
      ;; Overlay author colour on top so the name stands out.
      (when (and (stringp author) (not (string-empty-p author)) author-face)
        (save-excursion
          (goto-char (car span))
          (when (search-forward author (line-end-position) t)
            (add-face-text-property (match-beginning 0) (match-end 0)
                                    author-face nil)))))))

(defun disco-room--message-effective-author (msg)
  "Return effective author object for MSG.

Type-21 thread starter rows should inherit author identity from the referenced
source message, not the synthetic starter row itself."
  (let ((author (and (listp msg) (alist-get 'author msg))))
    (if (= (disco-msg-type msg) 21)
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
  "Synchronize all open room buffers from current state."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'disco-room-mode)
          (disco-room-render))))))

(defun disco-room--invalidate-attachment-key-in-open-rooms (attachment-key)
  "Invalidate rendered rows referencing ATTACHMENT-KEY in open room buffers."
  (let ((updated nil))
    (dolist (buf (buffer-list) updated)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (eq major-mode 'disco-room-mode)
                     (stringp attachment-key)
                     (disco-chat-timeline-live-p))
            (let ((message-ids
                   (disco-chat-timeline-dependent-keys
                    (list (list :attachment attachment-key)))))
              (when message-ids
                (setq updated t)
                (disco-chat-timeline-invalidate message-ids)))))))))

(defun disco-room--handle-media-rerender ()
  "Handle media refreshes with targeted invalidation when possible."
  (pcase-let ((`(,kind . ,key) (disco-media-last-notification)))
    (pcase kind
      ((or 'audio 'download)
       (unless (disco-room--invalidate-attachment-key-in-open-rooms key)
         (disco-room--rerender-open-rooms)))
      (_
       (disco-room--rerender-open-rooms)))))

(defun disco-room--on-text-scale-change ()
  "Rerender room buffers after `text-scale-mode' changes."
  (when (eq major-mode 'disco-room-mode)
    ;; Text scale affects remapped widths; invalidate cached fill column.
    (setq-local disco-room--chat-fill-column nil)
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

(setq disco-media-rerender-function #'disco-room--handle-media-rerender)
(setq disco-media-preview-rerender-function #'disco-room--handle-media-rerender)

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

(defun disco-room--avatar-display-size ()
  "Return full avatar size in pixels for two-line avatar rendering.

`disco-room-avatar-image-size' is interpreted as baseline size ratio where
`28' maps to exactly two text lines at current scale."
  (let* ((line-height (disco-chat-avatar-line-pixel-height))
         (base-target (* 2 line-height))
         (size-factor (/ (float (max 1 disco-room-avatar-image-size)) 28.0)))
    (max 8 (round (* base-target size-factor)))))

(defun disco-room--avatar-factors (&optional cheight)
  "Return avatar (circle . margin) factors for CHEIGHT lines."
  (let* ((entry (alist-get (or cheight 2) disco-room-avatar-factors-alist))
         (circle (and (consp entry) (car entry)))
         (margin (and (consp entry) (cdr entry))))
    (cons (if (numberp circle) circle 0.8)
          (if (numberp margin) margin 0.1))))


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
           (line-height (disco-chat-avatar-line-pixel-height))
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
            (let ((image (disco-room--avatar-svg-image
                          svg
                          :scale 1.0
                          :width (disco-chat-avatar-column-width aw-chars)
                          :ascent 'center
                          :mask 'heuristic)))
              (when image
                (let* ((type (car image))
                       (props (copy-sequence (cdr image))))
                  (setq props
                        (plist-put props :disco-chat-avatar-char-width aw-chars))
                  (setq props
                        (plist-put props :disco-chat-avatar-slice-height
                                   line-height))
                  (setq image (cons type props))
                  (puthash cache-key image disco-room--avatar-round-image-cache)
                  image))))))))

(defun disco-room--avatar-one-line-image (msg)
  "Return avatar image sized for one text line for MSG, or nil."
  (when (disco-room--image-rendering-available-p)
    (let* ((cache-key (disco-room--avatar-cache-key msg))
           (cache-file (and cache-key
                            (disco-room--avatar-cache-existing-file cache-key)))
           (fallback (disco-room--avatar-placeholder msg))
           (svg-avatar (and disco-room-avatar-round-images
                            cache-file
                            (disco-room--avatar--create-svg
                             cache-file fallback 1))))
      (if (disco-media-image-object-valid-p svg-avatar)
          svg-avatar
        (let ((raw-image (disco-room--avatar-image msg)))
          (when (disco-media-image-object-valid-p raw-image)
            (let ((line-height (disco-chat-avatar-line-pixel-height)))
              (disco-chat-avatar-resize-image raw-image line-height))))))))

(defun disco-room--avatar-one-line-string (msg)
  "Return propertized string showing one-line inline avatar for MSG.
Returns empty string when no avatar is available."
  (let ((image (disco-room--avatar-one-line-image msg)))
    (if (disco-media-image-object-valid-p image)
        (let* ((char-width (disco-chat-avatar-image-char-width image))
               (text (make-string (max 1 char-width) ?\s)))
          (propertize text 'display image 'rear-nonsticky '(display)))
      "")))

(defun disco-room--avatar-prefixes (msg)
  "Return avatar-aware prefixes plist for MSG header/body lines."
  (let* ((image (disco-room--avatar-image msg))
         (fallback (disco-room--avatar-placeholder msg))
         (base-size (disco-room--avatar-display-size)))
    (if (disco-media-image-object-valid-p image)
        (let* ((pixel-size (if disco-room-avatar-round-images
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
                                 2))))
          (disco-chat-avatar-prefixes
           (or svg-avatar image)
           fallback
           :pixel-size pixel-size
           :resize (null svg-avatar)))
      (disco-chat-avatar-prefixes
       nil fallback :pixel-size base-size))))

(defun disco-room--attachment-meta-line (attachment)
  "Return compact metadata line for ATTACHMENT object."
  (disco-media-attachment-meta-line attachment t))

(defun disco-room--attachment-default-save-name (attachment)
  "Return default filename for saving ATTACHMENT locally."
  (disco-media-attachment-default-save-name attachment))

(defun disco-room--attachment-download-key (attachment)
  "Return stable download state key for ATTACHMENT."
  (disco-media-attachment-download-key attachment))

(defun disco-room--attachment-download-path (attachment)
  "Return default local download path for ATTACHMENT."
  (disco-media-attachment-download-path attachment))

(defun disco-room--attachment-download-state (attachment)
  "Return normalized download state plist for ATTACHMENT."
  (disco-media-attachment-download-state attachment))

(defun disco-room--start-attachment-download (attachment &optional open-after)
  "Start asynchronous default-location download for ATTACHMENT.

When OPEN-AFTER is non-nil, open downloaded file in Emacs after completion."
  (disco-media-start-attachment-download attachment open-after))

(defun disco-room--cancel-attachment-download (attachment)
  "Cancel active asynchronous download for ATTACHMENT."
  (disco-media-cancel-attachment-download attachment))

(defun disco-room--open-downloaded-attachment (attachment)
  "Open local downloaded file for ATTACHMENT."
  (disco-media-open-downloaded-attachment attachment))

(defun disco-room--play-attachment-video (attachment)
  "Play ATTACHMENT video preferring local file when available."
  (disco-media-play-attachment-video attachment))

(defun disco-room-download-attachment (attachment &optional target-path)
  "Download ATTACHMENT to TARGET-PATH.

When TARGET-PATH is nil, prompt interactively for destination path."
  (interactive)
  (disco-media-download-attachment attachment target-path))

(cl-defun disco-room--insert-attachment-card (attachment &key message-id spoiler-hidden)
  "Insert one typed rich attachment block for ATTACHMENT object."
  (let ((toggle-action (and spoiler-hidden
                            (stringp message-id)
                            (lambda ()
                              (disco-room-toggle-message-spoilers message-id)))))
    (pcase (disco-media-attachment-kind attachment)
      ('photo
       (disco-ins-insert-attachment-photo
        attachment
        :border-face 'disco-room-attachment-card-border
        :title-face 'disco-room-attachment-card-title
        :meta-face 'disco-room-attachment-card-meta
        :action-face 'disco-room-attachment-card-action
        :show-url disco-room-show-attachment-urls
        :spoiler-hidden spoiler-hidden
        :spoiler-toggle-action toggle-action))
      ('video
       (disco-ins-insert-attachment-video
        attachment
        :border-face 'disco-room-attachment-card-border
        :title-face 'disco-room-attachment-card-title
        :meta-face 'disco-room-attachment-card-meta
        :action-face 'disco-room-attachment-card-action
        :show-url disco-room-show-attachment-urls
        :spoiler-hidden spoiler-hidden
        :spoiler-toggle-action toggle-action))
      ('audio
       (disco-ins-insert-attachment-audio
        attachment
        :border-face 'disco-room-attachment-card-border
        :title-face 'disco-room-attachment-card-title
        :meta-face 'disco-room-attachment-card-meta
        :action-face 'disco-room-attachment-card-action
        :show-url disco-room-show-attachment-urls
        :spoiler-hidden spoiler-hidden
        :spoiler-toggle-action toggle-action))
      (_
       (disco-ins-insert-attachment-document
        attachment
        :border-face 'disco-room-attachment-card-border
        :title-face 'disco-room-attachment-card-title
        :meta-face 'disco-room-attachment-card-meta
        :action-face 'disco-room-attachment-card-action
        :show-url disco-room-show-attachment-urls
        :spoiler-hidden spoiler-hidden
        :spoiler-toggle-action toggle-action)))))

(defun disco-room--media-card-fallback-context ()
  "Return primary attachment context for the message at point.

An exact card context property wins before this function is called; this is
only the message-level fallback used by the shared media transient protocol."
  (when-let* ((message (ignore-errors (disco-room--message-at-point)))
              (attachment (car (disco-room--message-effective-attachments message))))
    (disco-media-attachment-card-context attachment)))

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

(defun disco-room--forward-private-channel-recipient-display-names (channel)
  "Return display names for CHANNEL recipients, excluding the current user when known."
  (let* ((self-id (disco-gateway-current-user-id))
         (recipients (and (listp channel) (alist-get 'recipients channel)))
         (filtered (if self-id
                       (seq-remove
                        (lambda (recipient)
                          (equal (format "%s" (alist-get 'id recipient))
                                 (format "%s" self-id)))
                        (or recipients '()))
                     (or recipients '())))
         (effective-recipients (if filtered filtered (or recipients '()))))
    (delq nil
          (mapcar #'disco-room--forward-recipient-display-name
                  effective-recipients))))

(defun disco-room--forward-private-channel-display-name (channel)
  "Return best display name for private CHANNEL."
  (let* ((channel-type (and (listp channel) (alist-get 'type channel)))
         (explicit-name (and (listp channel)
                             (stringp (alist-get 'name channel))
                             (not (string-empty-p (alist-get 'name channel)))
                             (alist-get 'name channel)))
         (recipient-names
          (disco-room--forward-private-channel-recipient-display-names channel)))
    (cond
     ((disco-channel-direct-message-p channel-type)
      (or (car recipient-names) explicit-name "direct-message"))
     ((disco-channel-group-dm-p channel-type)
      (or explicit-name
          (and recipient-names (mapconcat #'identity recipient-names ", "))
          "group-dm"))
     (t
      (or explicit-name "(no-name)")))))

(defun disco-room--forward-channel-name (channel)
  "Return display name for CHANNEL independent of badge prefixes."
  (if (and channel (listp channel) (disco-channel-private-p channel))
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
       ((disco-channel-private-p channel-type)
        (if (disco-channel-group-dm-p channel-type)
            (format "group:%s" name)
          (format "@%s" name)))
       ((disco-state-channel-thread-p channel)
        (let* ((parent-id (disco-msg-normalize-id (alist-get 'parent_id channel)))
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
  (let* ((ref-channel-id (disco-msg-reference-channel-id msg))
         (ref-guild-id (disco-msg-reference-guild-id msg))
         (channel (and ref-channel-id (disco-state-channel ref-channel-id)))
         (resolved-guild-id
          (or ref-guild-id
              (disco-msg-normalize-id
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
         (message-id (alist-get 'id msg))
         (text (and (listp snapshot) (alist-get 'content snapshot))))
    (when (and (stringp text) (not (string-empty-p text)))
      (disco-markdown-render text
                             :context 'forward-snapshot
                             :message snapshot
                             :spoiler-message-id message-id
                             :reveal-spoilers
                             (disco-room--message-spoilers-revealed-p message-id)))))

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
           (message-id (alist-get 'id msg))
           (text (and (listp snapshot) (alist-get 'content snapshot)))
           (display (and (stringp text)
                         (disco-markdown-render
                          text
                          :context 'forward-summary
                          :message snapshot
                          :spoiler-message-id message-id
                          :reveal-spoilers
                          (disco-room--message-spoilers-revealed-p message-id))))
           (trimmed (and (stringp display) (string-trim display))))
      (if (and (stringp trimmed) (not (string-empty-p trimmed)))
          (format "[forwarded] %s" trimmed)
        "[forwarded message]"))))

(defun disco-room--message-system-divider-p (msg)
  "Return non-nil when MSG should be rendered as a system divider line.

Types 0 (DEFAULT), 19 (REPLY), 20 (CHAT_INPUT_COMMAND), 21
(THREAD_STARTER_MESSAGE) and 23 (CONTEXT_MENU_COMMAND) are regular
messages; everything else is a system event shown as a centered divider."
  (not (memq (disco-msg-type msg) '(0 19 20 21 23))))

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
         (ref-id (disco-msg-reference-id msg))
         (ref-channel-id (or (disco-msg-reference-channel-id msg)
                             disco-room--channel-id))
         (self-id (disco-msg-normalize-id (alist-get 'id msg))))
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
                         (equal (disco-msg-normalize-id (alist-get 'id fallback))
                                self-id))
              fallback)))))))

(defun disco-room--thread-starter-reference-content (msg)
  "Return referenced message content for thread starter MSG, or nil."
  (let* ((resolved (disco-room--thread-starter-reference-message msg))
         (text (and (listp resolved) (alist-get 'content resolved)))
         (display (and (stringp text)
                       (disco-markdown-unescape-punctuation text))))
    (when (and (stringp display) (not (string-empty-p (string-trim display))))
      (string-trim display))))

(defun disco-room--message-guild-name (msg)
  "Return display guild name for MSG, or nil if unavailable."
  (let* ((msg-guild-id (disco-msg-normalize-id (alist-get 'guild_id msg)))
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
         (title (and (listp embed) (alist-get 'title embed)))
         (description (and (listp embed) (alist-get 'description embed)))
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
  (let* ((type (disco-msg-type msg))
         (author (disco-room--message-author msg))
         (content (string-trim (disco-markdown-unescape-punctuation
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
                    (alist-get 'role_subscription_data msg)))
              (tier-name (and (listp role-subscription)
                              (alist-get 'tier_name role-subscription)))
              (months (and (listp role-subscription)
                           (alist-get 'total_months_subscribed role-subscription)))
              (renewal (and (listp role-subscription)
                            (disco-util-json-true-p
                             (alist-get 'is_renewal role-subscription))))
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
                                (alist-get 'application msg)))
              (app-name (and (listp application)
                             (alist-get 'name application))))
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
                                          (alist-get 'purchase_notification msg)))
              (guild-product-purchase
               (and (listp purchase-notification)
                    (alist-get 'guild_product_purchase purchase-notification)))
              (product-name (and (listp guild-product-purchase)
                                 (alist-get 'product_name guild-product-purchase))))
         (if (and (stringp product-name) (not (string-empty-p product-name)))
             (format "%s has purchased %s!" author product-name)
           (format "%s completed a guild product purchase." author))))
      (46
       "A poll result was finalized.")
      (_ nil))))

(defun disco-room--active-highlight-query ()
  "Return active room search query string to highlight, or nil."
  (or (and (listp disco-room--inplace-search-filter)
           (plist-get disco-room--inplace-search-filter :query))
      (and (listp disco-room--msg-filter)
           (plist-get disco-room--msg-filter :query))))

(defun disco-room--highlight-search-query (text)
  "Return TEXT with active room search query highlighted."
  (let ((query (disco-room--active-highlight-query)))
    (if (or (not (stringp text))
            (string-empty-p text)
            (not (stringp query))
            (string-empty-p query))
        text
      (let ((copy (copy-sequence text))
            (start 0)
            (case-fold-search t))
        (while (and (< start (length copy))
                    (string-match (regexp-quote query) copy start))
          (add-face-text-property (match-beginning 0)
                                  (match-end 0)
                                  'disco-room-search-highlight
                                  'append
                                  copy)
          (setq start (match-end 0)))
        copy))))

(defun disco-room--message-display-content (msg)
  "Return human-readable content string for message MSG."
  (let* ((message-id (alist-get 'id msg))
         (raw-content (or (alist-get 'content msg) ""))
         (content (disco-room--highlight-search-query
                   (disco-markdown-render
                    raw-content
                    :context 'room-message
                    :message msg
                    :spoiler-message-id message-id
                    :reveal-spoilers
                    (disco-room--message-spoilers-revealed-p message-id))))
         (attachments (disco-room--message-effective-attachments msg))
         (embeds (disco-room--message-effective-embeds msg))
         (poll (disco-msg-poll msg))
         (attachment-count (length attachments))
         (embed-count (length embeds))
         (poll-count (if poll 1 0))
         (showing-attachments (and disco-room-show-attachments (> attachment-count 0)))
         (showing-embeds (and disco-embed-show-embeds (> embed-count 0)))
         (showing-poll (and disco-room-show-polls (> poll-count 0)))
         (msg-type (disco-msg-type msg))
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

(defun disco-room--message-copy-text (msg)
  "Return copy-ready visible text for MSG, or nil.

This keeps message-copy semantics close to room rendering while avoiding room
UI affordances such as timestamps, reaction rows and attachment cards." 
  (let* ((message-id (alist-get 'id msg))
         (system-content (disco-room--message-system-content msg))
         (raw-content (and (listp msg) (alist-get 'content msg)))
         (exported (and (stringp raw-content)
                        (disco-markdown-copy-export
                         raw-content
                         :context 'room-message-copy
                         :message msg
                         :spoiler-message-id message-id
                         :reveal-spoilers t))))
    (cond
     ((and (stringp system-content)
           (not (string-empty-p (string-trim system-content))))
      system-content)
     ((and (stringp exported)
           (not (string-empty-p (string-trim (substring-no-properties exported)))))
      exported)
     (t nil))))

(defun disco-room--attachment-kind (attachment)
  "Return short attachment kind string for ATTACHMENT object."
  (pcase (disco-media-attachment-kind attachment)
    ('photo "img")
    ('video "video")
    ('audio "audio")
    (_ "file")))

(defun disco-room--attachment-summary (attachment)
  "Return one-line attachment summary string for ATTACHMENT object."
  (disco-media-attachment-summary attachment))

(defun disco-room--insert-message-attachments (msg &optional prefix)
  "Insert attachment detail lines for MSG.

PREFIX can be a fixed prefix string or mutable prefix-state."
  (when disco-room-show-attachments
    (let* ((message-id (alist-get 'id msg))
           (reveal-spoilers (disco-room--message-spoilers-revealed-p message-id)))
      (dolist (attachment (or (disco-room--message-effective-attachments msg) '()))
        (let ((spoiler-hidden (and (stringp message-id)
                                   (disco-media-attachment-spoiler-p attachment)
                                   (not reveal-spoilers))))
          (if disco-room-use-rich-attachment-cards
              (disco-room--insert-attachment-card
               attachment
               :message-id message-id
               :spoiler-hidden spoiler-hidden)
            (if spoiler-hidden
                (disco-ins-insert-attachment-spoiler-placeholder
                 attachment
                 :prefix prefix
                 :line-face 'disco-room-message-meta
                 :button-face 'disco-room-message-meta
                 :toggle-action (lambda ()
                                  (disco-room-toggle-message-spoilers message-id))
                 :toggle-help-echo "Reveal spoiler attachment")
              (disco-ins-insert-attachment-lines
               (disco-room--attachment-summary attachment)
               :prefix prefix
               :url (and disco-room-show-attachment-urls
                         (or (alist-get 'url attachment)
                             (alist-get 'proxy_url attachment)))
               :summary-face 'disco-room-message-meta
               :url-face 'shadow))))))))

(defun disco-room--insert-message-embeds (msg)
  "Insert embed detail lines for MSG."
  (disco-embed-insert-message-embeds
   (disco-room--message-with-effective-embeds msg)))

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
  (let ((normalized (disco-msg-poll-normalize-answer-id-list answer-ids)))
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
      (disco-msg-poll-normalize-answer-id-list
       (disco-room--poll-draft-selection message-id))
    (disco-msg-poll-voted-answer-ids poll)))

(defun disco-room--poll-add-selection (message-id poll answer-id)
  "Return staged selection with ANSWER-ID added for MESSAGE-ID/POLL."
  (let ((current (copy-sequence (disco-room--poll-effective-selection message-id poll))))
    (if (disco-msg-poll-multiselect-p poll)
        (if (member answer-id current)
            current
          (append current (list answer-id)))
      (list answer-id))))

(defun disco-room--poll-toggle-draft-selection (message-id poll answer-id)
  "Return staged selection after toggling ANSWER-ID for MESSAGE-ID/POLL."
  (let* ((current (copy-sequence (disco-room--poll-effective-selection message-id poll)))
         (has (member answer-id current)))
    (if (disco-msg-poll-multiselect-p poll)
        (if has
            (delete answer-id current)
          (append current (list answer-id)))
      (if has
          '()
        (list answer-id)))))

(defun disco-room--poll-draft-differs-p (message-id poll)
  "Return non-nil when staged selection differs from committed vote state."
  (let ((draft (disco-room--poll-draft-selection message-id))
        (committed (disco-msg-poll-voted-answer-ids poll)))
    (and (disco-room--poll-draft-selection-present-p message-id)
         (not (equal (disco-msg-poll-normalize-answer-id-list draft)
                     (disco-msg-poll-normalize-answer-id-list committed))))))

(defun disco-room--poll-answer-selected-p (message-id poll answer-id)
  "Return non-nil when ANSWER-ID is selected in effective poll UI state."
  (member answer-id (disco-room--poll-effective-selection message-id poll)))

(defun disco-room--message-with-poll-vote-selection (msg selected-answer-ids)
  "Return MSG copy with current-user poll votes set to SELECTED-ANSWER-IDS."
  (let* ((updated (copy-tree msg))
         (poll (copy-tree (disco-msg-poll msg))))
    (if (not (listp poll))
        updated
      (let* ((results (copy-tree (or (disco-msg-poll-results poll) '())))
             (counts (copy-tree (or (alist-get 'answer_counts results) '())))
             (selected (delete-dups (copy-sequence (or selected-answer-ids '()))))
             (previous (disco-msg-poll-voted-answer-ids poll)))
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
         (poll (copy-tree (disco-msg-poll msg))))
    (if (not (and (listp poll) (integerp answer-id)))
        updated
      (let* ((results (copy-tree (or (disco-msg-poll-results poll) '())))
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

(defun disco-room--apply-live-poll-vote-event (event)
  "Apply poll vote EVENT to local room state and projected timeline."
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
    (let* ((poll (disco-msg-poll msg))
           (message-id (alist-get 'id msg))
           (question (and poll (disco-msg-poll-question-text poll)))
           (state (and poll (disco-msg-poll-state-label poll)))
           (expiry-label (and poll
                              (disco-msg-poll-expiry-label
                               poll disco-room-poll-date-format)))
           (answers (and poll (or (alist-get 'answers poll) '())))
           (committed-selection (and poll (disco-msg-poll-voted-answer-ids poll)))
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
            (when (disco-msg-poll-multiselect-p poll)
              (setq parts (append parts '("multi"))))
            (when (and disco-room-poll-show-total-votes
                       (disco-msg-poll-results poll))
              (setq parts
                    (append parts
                            (list (format "votes=%d"
                                          (disco-msg-poll-total-votes poll))))))
            (when expiry-label
              (setq parts (append parts (list (format "ends=%s" expiry-label)))))
            (insert (mapconcat #'identity parts "   ") "\n")
            (disco-ui-apply-line-prefix meta-start (point) prefix-state)
            (add-text-properties meta-start (point)
                                 `(disco-message-id ,message-id))
            (disco-ui-append-face
             meta-start (point) disco-room-poll-meta-face))
          (dolist (answer answers)
            (let* ((answer-id (disco-msg-poll-answer-id answer))
                   (selected (and answer-id
                                  (member answer-id effective-selection)))
                   (count (and answer-id
                               (disco-msg-poll-answer-count poll answer-id)))
                   (emoji (disco-msg-poll-answer-emoji answer))
                   (label (disco-msg-poll-answer-text answer))
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
         (reaction-name (disco-msg-reaction-emoji reaction)))
    (if target-id
        (and reaction-id (equal (format "%s" reaction-id) (format "%s" target-id)))
      (equal reaction-name target-name))))

(defun disco-room--message-has-own-reaction-p (msg emoji)
  "Return non-nil when MSG has current-user reaction EMOJI."
  (let ((found nil))
    (dolist (reaction (disco-msg-reactions msg))
      (when (and (disco-room--reaction-matches-input-p reaction emoji)
                 (disco-msg-reaction-selected-p reaction))
        (setq found t)))
    found))

(defun disco-room--message-with-reaction-delta (msg emoji addp)
  "Return MSG copy after applying one reaction delta for EMOJI.

When ADDP is non-nil, reaction count is increased and marked selected;
otherwise selected flag is cleared and count is decreased."
  (let* ((updated (copy-tree msg))
         (reactions (copy-tree (disco-msg-reactions msg)))
         (spec (disco-room--parse-reaction-input emoji))
         (target-id (plist-get spec :id))
         (target-name (plist-get spec :name))
         (found nil)
         (next '()))
    (dolist (reaction reactions)
      (if (disco-room--reaction-matches-input-p reaction emoji)
          (let* ((count (max 0 (or (disco-msg-reaction-count reaction) 0)))
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
      (disco-room--sync-timeline
       :changed-resources (list (list :message message-id))))
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
         (reactions (copy-tree (disco-msg-reactions msg)))
         (next '()))
    (dolist (reaction reactions)
      (unless (disco-room--reaction-matches-input-p reaction emoji)
        (push reaction next)))
    (setf (alist-get 'reactions updated nil 'remove) (nreverse next))
    updated))

(defun disco-room--apply-live-reaction-event (event)
  "Apply reaction EVENT to local room state and projected timeline.

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
  (when (disco-msg-reply-type-p msg)
    (or (and (listp (alist-get 'referenced_message msg))
             (alist-get 'id (alist-get 'referenced_message msg)))
        (disco-msg-reference-id msg))))

(defun disco-room--reply-preview (msg)
  "Return one-line preview string of MSG reply target, or nil."
  (when (disco-msg-reply-type-p msg)
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

(defun disco-room--insert-forward-card (msg)
  "Insert one rich forwarded-message card for MSG."
  (let* ((ref-id (disco-msg-reference-id msg))
         (ref-channel (disco-msg-reference-channel-id msg))
         (source (disco-room--forward-source-context msg))
         (guild (plist-get source :guild))
         (guild-label (or (plist-get source :guild-label) "direct message"))
         (channel-label (or (plist-get source :channel-label) "unknown-channel"))
         (sent-at (disco-room--forward-snapshot-time-label msg))
         (content (disco-room--forward-snapshot-content msg))
         (jump-help-echo
          (and (stringp ref-id)
               (not (string-empty-p ref-id))
               (if (and (stringp ref-channel)
                        (not (string-empty-p ref-channel))
                        (not (equal (disco-msg-normalize-id ref-channel)
                                    (disco-msg-normalize-id disco-room--channel-id))))
                   (format "Open channel %s and jump to message %s" ref-channel ref-id)
                 (format "Jump to message %s" ref-id)))))
    (disco-ins-insert-forward-card
     :source-text (format "%s / %s" guild-label channel-label)
     :sent-at sent-at
     :content content
     :insert-source-icon (and (listp guild)
                              (lambda ()
                                (disco-room--insert-forward-guild-icon guild)))
     :jump-label (and (stringp ref-id)
                      (not (string-empty-p ref-id))
                      "[Jump to source]")
     :jump-action (and (stringp ref-id)
                       (not (string-empty-p ref-id))
                       (lambda ()
                         (disco-room-jump-to-message ref-id ref-channel)))
     :jump-face 'disco-room-forward-card-action
     :jump-help-echo jump-help-echo
     :border-face 'disco-room-forward-card-border
     :title-face 'disco-room-forward-card-title
     :meta-face 'disco-room-forward-card-meta)))

(defun disco-room--insert-forward-section (msg &optional prefix)
  "Insert forwarded-message block for MSG when applicable.

When PREFIX is non-nil, use it for non-card fallback indentation."
  (when (disco-room--message-forwarded-p msg)
    (if disco-room-use-rich-forward-cards
        (disco-room--insert-forward-card msg)
      (let ((ref-id (disco-msg-reference-id msg))
            (ref-channel (disco-msg-reference-channel-id msg)))
        (when (and (stringp ref-id) (not (string-empty-p ref-id)))
          (disco-ins-insert-reference-line
           nil
           :prefix prefix
           :face 'shadow
           :button-label "[Jump to source]"
           :button-action (lambda ()
                            (disco-room-jump-to-message ref-id ref-channel))))))))

(defun disco-room--insert-message (msg context)
  "Insert one message MSG using projected render CONTEXT."
  (if (disco-room--message-system-divider-p msg)
      (disco-room--insert-system-divider-message msg context)
    (let* ((compact (disco-util-json-true-p (plist-get context :compact)))
           (insert-date (plist-get context :insert-date))
           (insert-unread (disco-util-json-true-p (plist-get context :insert-unread)))
           (timestamp (disco-util-format-time (or (alist-get 'timestamp msg) "")))
           (short-time (if (alist-get 'pending msg)
                           "sending…"
                         (disco-util-format-time-short
                          (or (alist-get 'timestamp msg) ""))))
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
                 (compact-prefix (or (plist-get avatar-prefixes :rest-body) "    "))
                 (compact-prefix-width (max 0 (string-width compact-prefix))))
            (setq section-prefix-state
                  (disco-ui-make-prefix-state compact-prefix compact-prefix))
            (when reply
              (let ((ref-id (disco-room--reply-reference-id msg))
                    (ref-channel (disco-msg-reference-channel-id msg)))
                (disco-ins-insert-reference-line
                 reply
                 :prefix section-prefix-state
                 :face 'shadow
                 :button-label (and (stringp ref-id)
                                    (not (string-empty-p ref-id))
                                    "[Jump]")
                 :button-action (and (stringp ref-id)
                                     (not (string-empty-p ref-id))
                                     (lambda ()
                                       (disco-room-jump-to-message ref-id ref-channel))))))
            (let ((content-start (point))
                  (time-span nil))
              (unless (string-empty-p content)
                (insert content))
              (setq time-span
                    (disco-room--insert-right-aligned-text
                     short-time
                     'disco-room-timestamp
                     compact-prefix-width))
              (when (and (stringp timestamp) (not (string-empty-p timestamp)))
                (add-text-properties
                 (car time-span)
                 (cdr time-span)
                 (list 'help-echo timestamp)))
              (insert "\n")
              (disco-ui-apply-line-prefix content-start (point) section-prefix-state)))
        (let* ((avatar-prefixes (disco-room--avatar-prefixes msg))
               (header-prefix (or (plist-get avatar-prefixes :header) ""))
               (header-prefix-width (max 0 (string-width header-prefix)))
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
                    'disco-room-timestamp
                    header-prefix-width)))
              (when (and (stringp timestamp) (not (string-empty-p timestamp)))
                (add-text-properties
                 (car time-span)
                 (cdr time-span)
                 (list 'help-echo timestamp))))
            (insert "\n")
            (disco-ui-apply-line-prefix
             header-start (point)
             (disco-ui-make-prefix-state header-prefix body-rest-prefix)))
          (when reply
            (let ((ref-id (disco-room--reply-reference-id msg))
                  (ref-channel (disco-msg-reference-channel-id msg)))
              (disco-ins-insert-reference-line
               reply
               :prefix section-prefix-state
               :face 'shadow
               :button-label (and (stringp ref-id)
                                  (not (string-empty-p ref-id))
                                  "[Jump]")
               :button-action (and (stringp ref-id)
                                   (not (string-empty-p ref-id))
                                   (lambda ()
                                     (disco-room-jump-to-message ref-id ref-channel))))))
          (unless (string-empty-p content)
            (disco-ins-insert-prefixed-lines section-prefix-state content))))
      (let ((disco-ui-card-indent-prefix-state section-prefix-state)
            (disco-ui-card-indent-prefix
             (disco-ins-prefix-string section-prefix-state nil "    ")))
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
      (when disco-room-show-reactions
        (disco-ins-insert-reaction-line
         (disco-msg-reactions msg)
         :prefix section-prefix-state
         :selected-face 'disco-room-reaction-selected
         :unselected-face 'disco-room-reaction
         :line-face 'disco-room-message-meta))
      (disco-msg-apply-command-map line-start (point))
      (add-text-properties
       line-start
       (point)
       (list 'read-only t
             'front-sticky '(read-only)
             'disco-message-id message-id
             'disco-message-channel-id
             (disco-msg-normalize-id
              (or (alist-get 'channel_id msg)
                  disco-room--channel-id))
             'disco-message-guild-id
             (disco-msg-normalize-id
              (or (alist-get 'guild_id msg)
                  disco-room--guild-id)))))))

(defun disco-room--ewoc-printer (row)
  "EWOC pretty-printer for one projected room ROW."
  (disco-room--insert-message
   (disco-chat-timeline-row-payload row)
   (or (disco-chat-timeline-row-context row) '())))

(defun disco-room--header-help-text (&optional channel)
  "Return header help text for room actions in CHANNEL."
  (concat
   "M-<: older/more   C-c g/s/n/p: refresh/search/next/prev   M-g s/n/p: inplace search"
   "   C-c /: filter search   C-c C-r/C-s: inplace query back/forward"
   "   C-c C-/: cancel filter   C-c C-g: jump msg-id   timeline c/l/n/p/o/r/f/e/d/i/L/!/+/-/T: message actions"
   "   C-c C-w: toggle breakline   C-c C-p s/+/-/t/v/c/e: poll actions"
   "   C-c C-P: ack pins   C-c C-a: attach menu   C-c C-f: attach file   C-c C-v: clipboard attach"
   "   C-c C-e/o: formatting/options   C-c C-x: clear attachments   C-c M-l/M-e/M-r: attachment ops"
   "   C-c C-t o: open message thread   C-c C-t: thread ops"
   (if (disco-room--composer-visible-p channel)
       (concat
        (format "   RET/C-c C-c: %s   M-RET: preview   TAB: @/# complete   C-c M-v: refetch avatars"
                (if (disco-room--composer-edit-active-p)
                    "save edit"
                  "send"))
        "   type at >>>   M-p/M-n/M-r: history")
     "   C-c M-v: refetch avatars   [composer hidden]")
   "   timeline q: quit   C-c ?: menu"))

(defun disco-room--input-footer-context-text ()
  "Return extra context lines shown above the room composer."
  (let* ((aux-state (disco-chatbuf-aux-state))
         (aux-type (plist-get aux-state :aux-type))
         (message-id (plist-get aux-state :message-id)))
    (concat
     (pcase aux-type
       ('edit
        (disco-room--composer-context-text "Editing" message-id))
       ('reply
        (disco-room--composer-context-text "Replying to" message-id))
       (_ ""))
     (if disco-room--pending-attachments
         (format "Queued attachments: %s\n"
                 (mapconcat #'identity
                            (disco-room--pending-attachment-labels)
                            ", "))
       ""))))

(defun disco-room--input-footer-text ()
  "Build read-only EWOC footer text shown above the room prompt."
  (let ((context-text (disco-room--input-footer-context-text))
        (typing-text (disco-room--typing-indicator-text)))
    (if (not (disco-room--composer-visible-p))
        ""
      (let ((text
             (concat
              "\n"
              (if (string-empty-p context-text)
                  ""
                context-text)
              (if (and (stringp typing-text)
                       (not (string-empty-p typing-text)))
                  (propertize (concat typing-text "\n")
                              'face 'disco-room-typing-indicator)
                ""))))
        (add-text-properties
         0 (length text)
         '(read-only t
           front-sticky (read-only)
           rear-nonsticky (read-only))
         text)
        text))))

(defun disco-room--prompt-text ()
  "Return visible prompt text for the current room buffer."
  ">>> ")

(defun disco-room--sync-shared-aux-state ()
  "Mirror room reply/edit context into shared chatbuf aux state."
  (if-let* ((aux-state (disco-room--current-composer-aux-state)))
      (disco-chatbuf-aux-set aux-state)
    (disco-chatbuf-aux-reset)))

(defun disco-room--set-composer-aux-state (pending-edit pending-reply-to)
  "Set room composer PENDING-EDIT and PENDING-REPLY-TO, then sync aux state."
  (setq disco-room--pending-edit pending-edit
        disco-room--pending-reply-to pending-reply-to)
  (disco-room--sync-shared-aux-state))

(defun disco-room--current-input-options-state ()
  "Return current room-local input-options plist for shared chatbuf state."
  (list :send-on-return disco-room-send-on-return
        :long-message-action disco-room-long-message-action
        :allowed-mentions (copy-tree disco-room-allowed-mentions)
        :reply-mention-replied-user disco-room-reply-mention-replied-user))

(defun disco-room--input-options-state ()
  "Return composer input-options plist from shared chatbuf state, or nil."
  (disco-chatbuf-input-options-state))

(defun disco-room--input-option-send-on-return ()
  "Return effective send-on-return option from shared chatbuf state."
  (eq t (plist-get (disco-room--input-options-state) :send-on-return)))

(defun disco-room--input-option-long-message-action ()
  "Return effective long-message action from shared chatbuf state."
  (plist-get (disco-room--input-options-state) :long-message-action))

(defun disco-room--input-option-allowed-mentions ()
  "Return effective allowed-mentions option from shared chatbuf state."
  (let ((state (disco-room--input-options-state)))
    (when (plist-member state :allowed-mentions)
      (copy-tree (plist-get state :allowed-mentions)))))

(defun disco-room--input-option-reply-mention-replied-user ()
  "Return effective reply-mention option from shared chatbuf state."
  (eq t (plist-get (disco-room--input-options-state)
                   :reply-mention-replied-user)))

(defun disco-room--sync-shared-input-options-state ()
  "Mirror room-local input option state into shared chatbuf state."
  (disco-chatbuf-input-options-set
   (disco-room--current-input-options-state)))

(defun disco-room--set-input-options-state (options)
  "Set room-local input OPTIONS and sync shared chatbuf state.

OPTIONS should be a plist using the same keys as
`disco-room--current-input-options-state'.  Missing keys fall back to the
current effective input-options state.  Return the normalized state plist."
  (let* ((current (or (disco-room--input-options-state)
                      (disco-room--current-input-options-state)))
         (send-on-return (if (plist-member options :send-on-return)
                             (plist-get options :send-on-return)
                           (plist-get current :send-on-return)))
         (long-message-action (if (plist-member options :long-message-action)
                                  (plist-get options :long-message-action)
                                (plist-get current :long-message-action)))
         (allowed-mentions (if (plist-member options :allowed-mentions)
                               (copy-tree (plist-get options :allowed-mentions))
                             (copy-tree (plist-get current :allowed-mentions))))
         (reply-mention-replied-user
          (if (plist-member options :reply-mention-replied-user)
              (plist-get options :reply-mention-replied-user)
            (plist-get current :reply-mention-replied-user))))
    (setq-local disco-room-send-on-return send-on-return
                disco-room-long-message-action long-message-action
                disco-room-allowed-mentions allowed-mentions
                disco-room-reply-mention-replied-user reply-mention-replied-user)
    (disco-room--sync-shared-input-options-state)
    (disco-room--current-input-options-state)))

(defun disco-room--update-input-options-state (updater)
  "Apply UPDATER to current effective input-options state and sync the result."
  (disco-room--set-input-options-state
   (funcall updater (or (disco-room--input-options-state)
                        (disco-room--current-input-options-state)))))

(defun disco-room--bind-input-region-from-footer ()
  "Ensure the persistent tail input region exists and matches current draft."
  (disco-chatbuf-init-state disco-room-input-history-size)
  (disco-room--sync-shared-aux-state)
  (disco-room--sync-shared-input-options-state)
  (disco-chatbuf-bind-input-region
   :visible-p (disco-room--composer-visible-p)
   :prompt (disco-room--prompt-text)
   :input-text (disco-room--current-draft)
   :post-bind-function #'disco-room--apply-input-text-properties))

(defun disco-room--header-text (&optional channel)
  "Build EWOC header text for the current room state."
  (let* ((channel (or channel (disco-room--channel-object)))
         (channel-name (or disco-room--channel-name ""))
         (channel-suffix (disco-room--channel-header-suffix channel))
         (help-text (disco-room--header-help-text channel))
         (composer-visible-p (disco-room--composer-visible-p channel))
         (context-text (and (not composer-visible-p)
                            (disco-room--input-footer-context-text)))
         (filter-line (disco-room--msg-filter-status-line))
         (composer-status-line (disco-room--composer-hidden-status-line channel))
         (text
          (with-temp-buffer
            (insert (format "Channel: %s%s\n" channel-name channel-suffix))
            (insert help-text)
            (when disco-room--refresh-in-flight
              (insert "   [refreshing...]"))
            (when disco-room--older-in-flight
              (insert "   [loading older...]"))
            (when disco-room--send-in-flight
              (insert "   [sending...]"))
            (insert "\n")
            (when (and (stringp context-text)
                       (not (string-empty-p context-text)))
              (insert context-text))
            (when disco-room--history-exhausted
              (insert "(older history exhausted)\n"))
            (when (stringp filter-line)
              (insert filter-line "\n"))
            (when (stringp composer-status-line)
              (insert composer-status-line "\n"))
            (insert "\n")
            (buffer-string))))
    (add-text-properties
     0 (length text)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only))
     text)
    text))

(defun disco-room--footer-text (&optional _draft)
  "Build EWOC footer text for the current room state."
  (disco-room--input-footer-text))

(defun disco-room--ensure-timeline (&optional channel draft)
  "Ensure current room buffer owns one shared projected timeline."
  (disco-chat-timeline-ensure
   :printer #'disco-room--ewoc-printer
   :anchor-property 'disco-message-id
   :header (disco-room--header-text channel)
   :footer (disco-room--footer-text draft)
   :after-mutation-function #'disco-room--update-context-mode))

(defun disco-room--update-frame (&optional channel draft)
  "Update current room header, footer, and composer in place."
  (disco-room--ensure-timeline channel draft)
  (disco-chat-timeline-set-frame
   (disco-room--header-text channel)
   (disco-room--footer-text draft)
   :bind-input-function #'disco-room--bind-input-region-from-footer))

(defun disco-room--first-unread-message-id (ordered-messages)
  "Return first unread message id in ORDERED-MESSAGES, or nil."
  (let ((last-read-id (disco-state-channel-last-read-message-id disco-room--channel-id))
        found)
    (dolist (msg ordered-messages)
      (let ((message-id (alist-get 'id msg)))
        (when (and (not found)
                   (or (null last-read-id)
                       (and (stringp message-id)
                            (stringp last-read-id)
                            (disco-state-snowflake< last-read-id message-id))))
          (setq found message-id))))
    found))

(defun disco-room--compute-message-render-context (previous-msg msg first-unread-id)
  "Return render context for MSG given PREVIOUS-MSG and FIRST-UNREAD-ID."
  (let* ((message-id (alist-get 'id msg))
         (day-key (disco-msg-day-key msg))
         (previous-day (and previous-msg (disco-msg-day-key previous-msg)))
         (insert-date (and disco-room-show-date-separators
                           (stringp day-key)
                           (not (equal day-key previous-day))
                           day-key))
         (compact (and previous-msg
                       (disco-room--messages-compact-group-p previous-msg msg)))
         (insert-unread (and disco-room-show-unread-divider
                             (stringp message-id)
                             (equal message-id first-unread-id))))
    (list :compact (and compact t)
          :insert-date insert-date
          :insert-unread (and insert-unread t))))

(defun disco-room--message-reference-targets-current-room-p (msg)
  "Return non-nil when MSG references a message in the current room."
  (let ((ref-channel-id (disco-msg-reference-channel-id msg)))
    (or (null ref-channel-id)
        (equal (disco-msg-normalize-id ref-channel-id)
               (disco-msg-normalize-id disco-room--channel-id)))))

(defun disco-room--message-dependency-keys (msg)
  "Return opaque resource keys that can change rendered MSG."
  (let ((reference-id (disco-msg-reference-id msg))
        (reference-channel-id (disco-msg-reference-channel-id msg))
        dependencies)
    (when (and (stringp reference-id)
               (disco-room--message-reference-targets-current-room-p msg)
               (or (disco-msg-reply-type-p msg)
                   (= (disco-msg-type msg) 21)))
      (push (list :message reference-id) dependencies))
    (when (disco-room--message-forwarded-p msg)
      (when-let* ((channel-id (disco-msg-normalize-id reference-channel-id)))
        (push (list :channel channel-id) dependencies))
      (when-let* ((guild-id
                   (or (disco-msg-reference-guild-id msg)
                       (and reference-channel-id
                            (disco-msg-normalize-id
                             (alist-get 'guild_id
                                        (disco-state-channel
                                         reference-channel-id)))))))
        (push (list :guild guild-id) dependencies)))
    (dolist (attachment (disco-room--message-effective-attachments msg))
      (when-let* ((key (disco-media-attachment-download-key attachment)))
        (push (list :attachment key) dependencies)))
    (delete-dups (delq nil dependencies))))

(defun disco-room--message-affects-composer-context-p (message-id)
  "Return non-nil when MESSAGE-ID is used by current composer context."
  (or (equal message-id (disco-room--composer-reply-message-id))
      (equal message-id (disco-room--composer-edit-message-id))))

(defun disco-room--project-timeline (ordered-messages)
  "Project ORDERED-MESSAGES into shared timeline rows."
  (let ((first-unread-id
         (disco-room--first-unread-message-id ordered-messages)))
    (disco-chat-timeline-project
     ordered-messages
     (lambda (message) (alist-get 'id message))
     :context-function
     (lambda (previous message)
       (disco-room--compute-message-render-context
        previous message first-unread-id))
     :dependencies-function #'disco-room--message-dependency-keys)))

(cl-defun disco-room--sync-timeline
    (&key ordered-messages force-keys changed-resources rekeys)
  "Synchronize projected room rows through the shared keyed controller."
  (disco-room--ensure-timeline)
  (let ((messages (or ordered-messages
                      (reverse (or (disco-room--display-messages) '())))))
    (disco-chat-timeline-sync
     (disco-room--project-timeline messages)
     :force-keys force-keys
     :changed-resources changed-resources
     :rekeys rekeys)))

(defun disco-room--apply-read-state-change ()
  "Synchronize projected unread-divider context after read-state changes."
  (when (and (disco-chat-timeline-live-p)
             (not (disco-room--msg-filter-active-p)))
    (disco-room--sync-timeline)
    t))

(defun disco-room--apply-forward-source-change (&optional source-channel-id
                                                          source-guild-id)
  "Synchronize rows depending on SOURCE-CHANNEL-ID or SOURCE-GUILD-ID."
  (when (and (disco-chat-timeline-live-p)
             (not (disco-room--msg-filter-active-p)))
    (let ((resources
           (delq nil
                 (list
                  (and source-channel-id
                       (list :channel
                             (disco-msg-normalize-id source-channel-id)))
                  (and source-guild-id
                       (list :guild
                             (disco-msg-normalize-id source-guild-id)))))))
      (when resources
        (disco-room--sync-timeline :changed-resources resources)
        t))))

(defun disco-room--apply-live-message-event (event)
  "Apply live message EVENT through canonical projected synchronization."
  (let* ((event-type (plist-get event :type))
         (event-message (plist-get event :message))
         (message-id (or (and (listp event-message) (alist-get 'id event-message))
                         (plist-get event :message-id)))
         (nonce (and (listp event-message)
                     (disco-msg-normalize-id
                      (alist-get 'nonce event-message))))
         (rekeys
          (and nonce message-id
               (not (equal nonce message-id))
               (disco-chat-timeline-node nonce)
               (list (cons nonce message-id)))))
    (cond
     ((disco-room--msg-filter-active-p)
      'filtered)
     ((not (memq event-type '(message-create message-update message-delete)))
      (error "disco: unsupported live message event: %S" event-type))
     ((not message-id)
      (error "disco: live message event has no message id: %S" event))
     (t
      (disco-room--sync-timeline
       :changed-resources (list (list :message message-id))
       :rekeys rekeys)
      (disco-room--update-message-window-state
       (or (disco-state-messages disco-room--channel-id) '()))
      (when (disco-room--message-affects-composer-context-p message-id)
        (disco-room--update-frame))
      'updated))))

(defun disco-room-render ()
  "Synchronize the room frame and projected timeline from local state."
  (let* ((channel (disco-room--channel-object))
         (messages (disco-room--display-messages))
         (draft (disco-room--current-draft))
         (initial-p (not (disco-chat-timeline-live-p)))
         (disco-room--avatar-fetch-budget
          (when (numberp disco-room-avatar-max-fetches-per-render)
            (max 0 disco-room-avatar-max-fetches-per-render)))
         (preview-fetch-budget
          (when (numberp disco-media-preview-max-fetches-per-render)
            (max 0 disco-media-preview-max-fetches-per-render))))
    (disco-media-set-preview-fetch-budget preview-fetch-budget)
    (unwind-protect
        (progn
          (disco-room--update-frame channel draft)
          ;; API returns newest-first by default; reverse for chat-like display.
          (disco-room--sync-timeline :ordered-messages (reverse messages))
          (when (and initial-p (disco-chatbuf-input-start-position))
            (let ((logical-end (disco-chatbuf-input-logical-end-position)))
              (when logical-end
                (goto-char logical-end)))))
      (disco-media-set-preview-fetch-budget nil)
      (disco-room--update-context-mode))))

(defun disco-room-refresh ()
  "Fetch and redraw latest messages for current room asynchronously."
  (interactive)
  (if (disco-room--msg-filter-active-p)
      (disco-room-filter-refresh)
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (generation (1+ disco-room--refresh-generation))
           (request-revision (disco-state-message-revision channel-id)))
      (setq disco-room--refresh-generation generation)
      (setq disco-room--refresh-in-flight t)
      (disco-room--update-frame)
      (disco-api-channel-messages-async
       channel-id
       :on-success
       (lambda (messages)
         (when (disco-room--callback-active-p room-buffer channel-id generation)
           (with-current-buffer room-buffer
             (setq disco-room--history-exhausted nil)
             (let ((merged (disco-state-merge-message-page
                            channel-id messages request-revision)))
               (disco-room--update-message-window-state merged))
             (disco-room--mark-read)
             (setq disco-room--refresh-in-flight nil)
             (disco-room-render)
             (disco-room--resolve-pending-jump)
             (message "disco: loaded %d messages" (length messages)))))
       :on-error
       (lambda (err)
         (when (disco-room--callback-active-p room-buffer channel-id generation)
           (with-current-buffer room-buffer
             (setq disco-room--refresh-in-flight nil)
             (disco-room--update-frame)
             (message "disco: room refresh failed: %s"
                      (disco-room--async-error-message err)))))))))

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
     ((and (memq event-type '(guild-update guild-delete))
           event-guild-id)
      (when (disco-room--apply-forward-source-change nil event-guild-id)
        t))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(channel-update
                              thread-update
                              channel-update-partial
                              channel-unread-update
                              channel-pins-update
                              channel-pins-ack)))
      (let ((channel (disco-room--channel-object)))
        (when (and channel (alist-get 'name channel))
          (setq disco-room--channel-name (alist-get 'name channel)))
        (disco-room--update-frame)
        (disco-room--apply-forward-source-change event-channel-id event-guild-id)))
     ((and event-channel-id
           (memq event-type '(channel-update thread-update channel-delete thread-delete)))
      (when (disco-room--apply-forward-source-change event-channel-id event-guild-id)
        t))
     ((and (equal event-channel-id disco-room--channel-id)
           (eq event-type 'message-ack))
      (disco-room--optimistic-read-ack-confirm (plist-get event :message-id))
      (unless (disco-room--apply-read-state-change)
        (disco-room--update-frame)))
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
             (message-id (or (and (listp message) (alist-get 'id message))
                             (plist-get event :message-id))))
        (when author-id
          ;; Message arrival implicitly ends visible typing state for sender.
          (disco-room--typing-stop-user author-id t))
        (disco-room--apply-live-message-event event)
        (when (and (eq event-type 'message-create)
                   (stringp message-id))
          (disco-room--mark-read message-id))))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-reaction-add
                              message-reaction-remove
                              message-reaction-remove-all
                              message-reaction-remove-emoji)))
      (disco-room--apply-live-reaction-event event))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-poll-vote-add
                              message-poll-vote-remove)))
      (disco-room--apply-live-poll-vote-event event)))))

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
                   (token-label (cond
                                 (token-id
                                  (disco-room--attachment-token-text token-id))
                                 ((disco-room--attachment-input-object-p item)
                                  "[file]")
                                 (t "[file:?]"))))
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

(defun disco-room--attachment-token-bounds-at-point ()
  "Return bounds of attachment token around point in input region, or nil."
  (let ((bounds (disco-chatbuf-input-region-bounds))
        (pos (point))
        found)
    (when (and bounds (disco-chatbuf-point-in-input-p pos))
      (save-excursion
        (goto-char (car bounds))
        (while (and (not found)
                    (re-search-forward disco-room--attachment-token-regexp (cdr bounds) t))
          (when (and (<= (match-beginning 0) pos)
                     (<= pos (match-end 0)))
            (setq found (cons (match-beginning 0) (match-end 0))))))
      found)))

(defun disco-room--attachment-object-bounds-at-point ()
  "Return bounds of attachment input object around point, or nil."
  (let ((object (disco-chatbuf-input-object-at-point)))
    (when (disco-room--attachment-input-object-p object)
      (disco-chatbuf-input-object-bounds-at-point))))

(defun disco-room--rewrite-draft-attachment-order (ordered-refs)
  "Rewrite current draft so ORDERED-REFS becomes the attachment sequence."
  (let* ((text-only (string-trim-right
                     (disco-room--draft-without-attachment-tokens
                      (disco-room--current-draft))))
         (parts (if (string-empty-p text-only)
                    nil
                  (list text-only))))
    (dolist (ref ordered-refs)
      (when parts
        (setq parts (append parts '(" "))))
      (setq parts (append parts (list (disco-room--attachment-ref-string ref)))))
    (disco-room--set-draft (if parts (apply #'concat parts) ""))))

(defun disco-room-remove-attachment-token-at-point ()
  "Remove queued attachment at point, or prompt for one when needed."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 1)
   "remove attachments")
  (let ((object-bounds (disco-room--attachment-object-bounds-at-point))
        (token-bounds (disco-room--attachment-token-bounds-at-point)))
    (cond
     (object-bounds
      (delete-region (car object-bounds) (cdr object-bounds))
      (disco-room--sync-draft-from-buffer)
      (disco-room--update-frame)
      (message "disco: removed attachment"))
     (token-bounds
      (let* ((token-text (buffer-substring-no-properties (car token-bounds) (cdr token-bounds)))
             (token-id (and (string-match disco-room--attachment-token-regexp token-text)
                            (match-string 1 token-text))))
        (delete-region (car token-bounds) (cdr token-bounds))
        (disco-room--sync-draft-from-buffer)
        (when token-id
          (remhash token-id disco-room--attachment-token-table))
        (disco-room--update-frame)
        (message "disco: removed attachment %s" (or token-id ""))))
     (t
      (let* ((ref (disco-room--choose-attachment-ref "Remove attachment: "))
             (type (plist-get ref :type))
             (start (plist-get ref :start))
             (end (plist-get ref :end))
             (draft (disco-room--current-draft))
             (updated (disco-room--draft-substring-delete draft start end)))
        (when (eq type 'token)
          (remhash (plist-get ref :token-id) disco-room--attachment-token-table))
        (disco-room--set-draft updated)
        (message "disco: removed %s" (plist-get ref :label)))))))

(defun disco-room-list-attachments ()
  "List queued attachments for current draft."
  (interactive)
  (disco-room--ensure-action-available
   (when (disco-room--composer-edit-active-p)
     "attachments are unavailable while editing a message")
   "list attachments")
  (let ((refs (disco-room--attachment-refs)))
    (if (null refs)
        (message "disco: no queued attachments")
      (message "disco: %s"
               (mapconcat (lambda (ref) (plist-get ref :label)) refs " | ")))))

(defun disco-room-edit-attachment-description ()
  "Edit description of one queued attachment."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 1)
   "edit attachment descriptions")
  (let* ((ref (disco-room--choose-attachment-ref "Edit attachment: "))
         (attachment (copy-tree (or (plist-get ref :attachment)
                                    (user-error "disco: attachment not found"))))
         (current (or (plist-get attachment :description) ""))
         (next-input (read-string
                      (format "Description for %s (empty clears): "
                              (plist-get ref :label))
                      current))
         (next (string-trim next-input)))
    (setq attachment
          (plist-put attachment :description (unless (string-empty-p next) next)))
    (pcase (plist-get ref :type)
      ('token
       (puthash (plist-get ref :token-id)
                (plist-put attachment :token-id (plist-get ref :token-id))
                disco-room--attachment-token-table)
       (disco-room--sync-pending-attachments-from-draft))
      ('object
       (let ((replacement
              (disco-room--attachment-input-object-string
               (disco-room--make-attachment-input-object
                (plist-get attachment :path)
                :filename (plist-get attachment :filename)
                :description (plist-get attachment :description)
                :content-type (plist-get attachment :content-type)))))
         (disco-room--set-draft
          (disco-room--draft-substring-replace
           (disco-room--current-draft)
           (plist-get ref :start)
           (plist-get ref :end)
           replacement)))))
    (disco-room--update-frame)
    (if (string-empty-p next)
        (message "disco: cleared description for %s" (plist-get ref :label))
      (message "disco: updated description for %s" (plist-get ref :label)))))

(defun disco-room-reorder-attachments ()
  "Reorder one queued attachment in the current draft."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--attachment-token-action-unavailable-reason 2)
   "reorder attachments")
  (let* ((refs (disco-room--attachment-refs))
         (count (length refs)))
    (when (< count 2)
      (user-error "disco: need at least two attachments to reorder"))
    (let* ((ref (disco-room--choose-attachment-ref "Move attachment: "))
           (current-index (or (cl-position ref refs :test #'equal)
                              (user-error "disco: attachment not found in draft")))
           (target-index-input
            (read-number
             (format "Move %s from %d to position (1-%d): "
                     (plist-get ref :label)
                     (1+ current-index)
                     count)
             (1+ current-index)))
           (target-index (max 0 (min (1- count) (1- target-index-input))))
           (without-ref (seq-remove (lambda (it) (equal it ref)) refs))
           (prefix (seq-take without-ref target-index))
           (suffix (seq-drop without-ref target-index))
           (next-order (append prefix (list ref) suffix)))
      (disco-room--rewrite-draft-attachment-order next-order)
      (message "disco: moved %s to position %d"
               (plist-get ref :label)
               (1+ target-index)))))

(defun disco-room--message-id-required-at-point ()
  "Return message ID at point, or signal user error."
  (or (disco-room--message-id-at-point)
      (user-error "disco: point is not on a message")))

(defun disco-room--default-reaction-emoji (msg)
  "Return best default reaction emoji suggestion from MSG."
  (let* ((reactions (disco-msg-reactions msg))
         (selected (seq-find #'disco-msg-reaction-selected-p reactions))
         (candidate (or selected (car reactions))))
    (or (and candidate (disco-msg-reaction-emoji candidate))
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

(defun disco-room--add-reaction-to-msg (msg)
  "Prompt for and add a reaction to MSG."
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason msg)
   "add reactions")
  (let* ((default (disco-room--default-reaction-emoji msg))
         (picked (disco-room--read-reaction-emoji "Add reaction" default)))
    (disco-room-add-reaction picked (alist-get 'id msg))))

(defun disco-room-add-reaction (&optional emoji message-id)
  "Add EMOJI reaction to MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message"))))
     (disco-room--ensure-action-available
      (disco-room--reaction-unavailable-reason msg)
      "add reactions")
     (let* ((default (disco-room--default-reaction-emoji msg))
            (picked (disco-room--read-reaction-emoji "Add reaction" default)))
       (list picked (alist-get 'id msg)))))
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason)
   "add reactions")
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

(defun disco-room--remove-reaction-from-msg (msg)
  "Prompt for and remove a reaction from MSG."
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason msg)
   "remove reactions")
  (let* ((default (disco-room--default-reaction-emoji msg))
         (picked (disco-room--read-reaction-emoji "Remove reaction" default)))
    (disco-room-remove-reaction picked (alist-get 'id msg))))

(defun disco-room-remove-reaction (&optional emoji message-id)
  "Remove current user's EMOJI reaction from MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message"))))
     (disco-room--ensure-action-available
      (disco-room--reaction-unavailable-reason msg)
      "remove reactions")
     (let* ((default (disco-room--default-reaction-emoji msg))
            (picked (disco-room--read-reaction-emoji "Remove reaction" default)))
       (list picked (alist-get 'id msg)))))
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason)
   "remove reactions")
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

(defun disco-room--toggle-reaction-on-msg (msg)
  "Prompt for and toggle a reaction on MSG."
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason msg)
   "toggle reactions")
  (let* ((default (disco-room--default-reaction-emoji msg))
         (picked (disco-room--read-reaction-emoji "Toggle reaction" default)))
    (disco-room-toggle-reaction picked (alist-get 'id msg))))

(defun disco-room-toggle-reaction (&optional emoji message-id)
  "Toggle current user's EMOJI reaction on MESSAGE-ID at point."
  (interactive
   (let* ((msg (or (disco-room--message-at-point)
                   (user-error "disco: point is not on a message"))))
     (disco-room--ensure-action-available
      (disco-room--reaction-unavailable-reason msg)
      "toggle reactions")
     (let* ((default (disco-room--default-reaction-emoji msg))
            (picked (disco-room--read-reaction-emoji "Toggle reaction" default)))
       (list picked (alist-get 'id msg)))))
  (disco-room--ensure-action-available
   (disco-room--reaction-unavailable-reason)
   "toggle reactions")
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
         (poll (disco-msg-poll msg)))
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
  (let* ((poll (or (disco-msg-poll msg) '()))
         (answers (or (alist-get 'answers poll) '()))
         out)
    (dolist (answer answers (nreverse out))
      (let ((answer-id (disco-msg-poll-answer-id answer)))
        (when answer-id
          (let* ((emoji (disco-msg-poll-answer-emoji answer))
                 (text (disco-msg-poll-answer-text answer))
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
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (target-id (alist-get 'id msg))
         (normalized (disco-msg-poll-normalize-answer-id-list selected-answer-ids)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "vote in polls")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
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
  (disco-room-render))

(defun disco-room-vote-poll-answer (&optional answer-id message-id)
  "Stage ANSWER-ID as selected for poll MESSAGE-ID.

In single-select polls, this replaces the staged selection."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "stage poll votes")
    (let ((picked (disco-room--pick-poll-answer-id msg answer-id)))
      (disco-room--stage-poll-selection
       target-id
       (disco-room--poll-add-selection target-id poll picked)))))

(defun disco-room-remove-poll-vote (&optional answer-id message-id)
  "Stage removal of ANSWER-ID vote from poll MESSAGE-ID."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg))
         (current (disco-room--poll-effective-selection target-id poll)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "stage poll vote removals")
    (let ((picked (disco-room--pick-poll-answer-id msg answer-id)))
      (unless (member picked current)
        (user-error "disco: answer %s is not selected" picked))
      (disco-room--stage-poll-selection
       target-id
       (delete picked (copy-sequence current))))))

(defun disco-room-toggle-poll-answer (&optional answer-id message-id)
  "Toggle staged poll ANSWER-ID in MESSAGE-ID.

This only updates local staged selection. Use `disco-room-submit-poll-vote' to
send votes to Discord."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg)))
    (disco-room--ensure-action-available
     (disco-room--poll-vote-unavailable-reason msg)
     "toggle staged poll votes")
    (let ((picked (disco-room--pick-poll-answer-id msg answer-id)))
      (disco-room--stage-poll-selection
       target-id
       (disco-room--poll-toggle-draft-selection target-id poll picked)))))

(defun disco-room-submit-poll-vote (&optional message-id)
  "Submit staged poll selection for MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg))
         (staged (disco-room--poll-effective-selection target-id poll))
         (committed (disco-msg-poll-voted-answer-ids poll)))
    (disco-room--ensure-action-available
     (disco-room--poll-submit-unavailable-reason msg)
     "submit poll votes")
    (when (null staged)
      (user-error "disco: select at least one answer before voting"))
    (when (equal (disco-msg-poll-normalize-answer-id-list staged)
                 (disco-msg-poll-normalize-answer-id-list committed))
      (user-error "disco: no pending poll vote changes"))
    (disco-room--submit-poll-vote target-id staged)))

(defun disco-room-clear-poll-votes (&optional message-id)
  "Remove all current-user votes for poll MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (poll (disco-msg-poll msg))
         (committed (disco-msg-poll-voted-answer-ids poll)))
    (disco-room--ensure-action-available
     (disco-room--poll-clear-unavailable-reason msg)
     "clear poll votes")
    (unless committed
      (user-error "disco: no existing poll vote to remove"))
    (disco-room--submit-poll-vote target-id '())))

(defun disco-room-expire-poll (&optional message-id)
  "End poll in MESSAGE-ID at point."
  (interactive)
  (let* ((msg (disco-room--poll-message-required message-id))
         (target-id (alist-get 'id msg))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id))
    (disco-room--ensure-action-available
     (disco-room--poll-expire-unavailable-reason msg)
     "end polls")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
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

(defun disco-room--forward-source-message (source-channel-id message-id)
  "Resolve SOURCE-CHANNEL-ID/MESSAGE-ID to a message object, or nil."
  (let ((channel-id (disco-msg-normalize-id source-channel-id))
        (target-id (disco-msg-normalize-id message-id)))
    (when (and channel-id target-id)
      (or (seq-find (lambda (msg)
                      (equal (disco-msg-normalize-id (alist-get 'id msg))
                             target-id))
                    (or (disco-state-messages channel-id) '()))
          (disco-api-channel-message channel-id target-id)))))

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
      (let* ((attachment-id (disco-msg-normalize-id (alist-get 'id attachment)))
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
      (unless source-message
        (user-error "disco: source message unavailable for forward-only"))
      (disco-room--read-forward-only-from-message source-message))))

(defun disco-room--send-allowed-mentions (&optional replying-p)
  "Return normalized allowed_mentions payload for outgoing message send/edit.

When REPLYING-P is non-nil and reply-mention is enabled, include
`replied_user'."
  (let* ((allowed-mentions (disco-room--input-option-allowed-mentions))
         (base
          (pcase allowed-mentions
            ('none '((parse . [])))
            ('all '((parse . ["users" "roles" "everyone"])))
            ((pred listp)
             (if (cl-every #'consp allowed-mentions)
                 (copy-tree allowed-mentions)
               (user-error "disco: disco-room-allowed-mentions custom value must be an alist")))
            (_ nil))))
    (when (and replying-p
               (disco-room--input-option-reply-mention-replied-user))
      (let ((value t))
        (if (listp base)
            (let ((cell (assq 'replied_user base)))
              (if cell
                  (setcdr cell value)
                (setq base (append base `((replied_user . ,value))))))
          (setq base `((replied_user . ,value))))))
    base))

(defun disco-room-draft-history-search (regexp)
  "Load one draft-history entry matching REGEXP."
  (interactive
   (list (read-regexp "Draft history search (regexp): "
                      nil
                      'disco-room-draft-history-search-history)))
  (let* ((entries (cl-delete-duplicates
                   (disco-chatbuf-input-history-elements)
                   :test #'equal))
         (matches (seq-filter (lambda (entry)
                                (and (stringp entry)
                                     (string-match-p regexp entry)))
                              entries)))
    (cond
     ((null matches)
      (message "disco: no draft history entry matches %s" regexp))
     (t
      (let ((picked (if (= 1 (length matches))
                        (car matches)
                      (completing-read "Matching draft: " matches nil t nil nil
                                       (car matches)))))
        (disco-chatbuf-input-history-reset)
        (disco-room--set-draft picked)
        (message "disco: loaded draft history match"))))))

(defun disco-room-input-preview ()
  "Show parsed preview of the current composer input."
  (interactive)
  (let* ((draft (disco-room--current-draft))
         (parsed (disco-room--parse-draft-input draft))
         (content (string-trim-right (or (plist-get parsed :content) "")))
         (attachments (or (plist-get parsed :attachments) '()))
         (buf (get-buffer-create "*disco-room-preview*"))
         (mode-label (pcase (plist-get (disco-chatbuf-aux-state) :aux-type)
                       ('edit "edit")
                       ('reply "reply")
                       (_ "message"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Room: %s\n" (or disco-room--channel-name disco-room--channel-id "(unknown)")))
        (insert (format "Composer mode: %s\n" mode-label))
        (insert (format "Structured objects: %d\n" (length (or (plist-get parsed :objects) '()))))
        (insert (format "Attachments: %d\n\n" (length attachments)))
        (insert "Content:\n")
        (insert (if (string-empty-p content)
                    "(empty)\n"
                  (concat content "\n")))
        (when attachments
          (insert "\nAttachments:\n")
          (dolist (attachment attachments)
            (insert (format "- %s\n"
                            (disco-room--attachment-label attachment "[file]")))))
        (special-mode)))
    (display-buffer buf)
    (message "disco: opened composer preview")))

(defun disco-room-attach-clipboard ()
  "Placeholder for telega-style clipboard attach entry point."
  (interactive)
  (user-error "disco: clipboard attach is not implemented yet"))

(defun disco-room-input-formatting-set ()
  "Placeholder for telega-style explicit input formatting."
  (interactive)
  (user-error "disco: explicit input formatting is not implemented yet"))

(defun disco-room-toggle-send-on-return ()
  "Toggle whether `RET' sends the current room draft."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :send-on-return
                             (not (plist-get current :send-on-return)))))))
    (message "disco: RET now %s"
             (if (plist-get state :send-on-return)
                 "sends messages"
               "opens draft editor"))))

(defun disco-room-cycle-long-message-action ()
  "Cycle long-message send behavior for current room buffer."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :long-message-action
                             (pcase (plist-get current :long-message-action)
                               ('split 'file)
                               (_ 'split)))))))
    (message "disco: long messages now %s"
             (pcase (plist-get state :long-message-action)
               ('file "send as file")
               (_ "split across messages")))))

(defun disco-room-cycle-allowed-mentions ()
  "Cycle allowed-mentions policy for current room buffer."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :allowed-mentions
                             (pcase (plist-get current :allowed-mentions)
                               ('none 'all)
                               ('all nil)
                               (_ 'none)))))))
    (message "disco: allowed mentions now %s"
             (pcase (plist-get state :allowed-mentions)
               ('none "disabled")
               ('all "explicitly enabled")
               (_ "Discord defaults")))))

(defun disco-room-toggle-reply-mention-replied-user ()
  "Toggle whether replies mention the replied user in current room."
  (interactive)
  (let ((state (disco-room--update-input-options-state
                (lambda (current)
                  (plist-put (copy-tree current)
                             :reply-mention-replied-user
                             (not (plist-get current :reply-mention-replied-user)))))))
    (message "disco: reply mention of replied user %s"
             (if (plist-get state :reply-mention-replied-user)
                 "enabled"
               "disabled"))))

(defun disco-room-reset-input-options ()
  "Reset room-local input option overrides back to global defaults."
  (interactive)
  (dolist (var '(disco-room-send-on-return
                 disco-room-long-message-action
                 disco-room-allowed-mentions
                 disco-room-reply-mention-replied-user))
    (kill-local-variable var))
  (disco-room--set-input-options-state (disco-room--current-input-options-state))
  (message "disco: room input options reset to global defaults"))

(transient-define-prefix disco-room-input-options-transient ()
  "Transient for telega-like room input options."
  [["Input Options"
    ("RET" "Toggle RET send/editor" disco-room-toggle-send-on-return)
    ("l" "Cycle long-message action" disco-room-cycle-long-message-action)
    ("m" "Cycle allowed mentions" disco-room-cycle-allowed-mentions)
    ("r" "Toggle reply mention" disco-room-toggle-reply-mention-replied-user)
    ("0" "Reset room-local options" disco-room-reset-input-options)]])

(defun disco-room-send-poll (question options &optional duration allow-multiselect content)
  "Create and send a poll with QUESTION and OPTIONS in current room.

DURATION is in hours. ALLOW-MULTISELECT toggles multi-select behavior.
CONTENT is optional extra text sent alongside the poll."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--poll-unavailable-reason)
      "send polls")
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
               content-input)))))
  (disco-room--ensure-action-available
   (disco-room--poll-unavailable-reason)
   "send polls")
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
    (disco-api--validate-message-content-length content "content")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     required-permissions
     :action "sending poll")
    (setq disco-room--send-in-flight t)
    (disco-room--update-frame)
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
           (disco-room--update-frame)
           (message "disco: send poll failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room-attach-file (path &optional description)
  "Queue attachment PATH for next room send.

DESCRIPTION is optional per-file description."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--attach-unavailable-reason)
      "attach files")
     (let* ((path (read-file-name "Attach file: " nil nil t))
            (description-input (string-trim (read-string "Attachment description (optional): ")))
            (description (unless (string-empty-p description-input)
                           description-input)))
       (list path description))))
  (disco-room--ensure-action-available
   (disco-room--attach-unavailable-reason)
   "attach files")
  (unless (file-readable-p path)
    (user-error "disco: file is not readable: %s" path))
  (let ((attachment (disco-room--make-attachment-input-object
                     path
                     :description description)))
    (if (disco-chatbuf-input-start-position)
        (progn
          (unless (disco-chatbuf-point-in-input-p)
            (goto-char (or (disco-chatbuf-input-logical-end-position) (point-max))))
          (disco-room--insert-attachment-input-object attachment)
          (disco-room--sync-draft-from-buffer))
      (let* ((draft (disco-room--current-draft))
             (separator (if (or (string-empty-p (disco-chatbuf-string-plain-text draft))
                                (string-match-p "[ \t\n]\\'"
                                                (disco-chatbuf-string-plain-text draft)))
                            ""
                          " ")))
        (disco-room--set-draft
         (concat draft
                 separator
                 (disco-room--attachment-input-object-string attachment)))))
    (message "disco: queued attachment %s"
             (file-name-nondirectory path))))

(defun disco-room-clear-attachments ()
  "Clear queued attachments for next send in current room."
  (interactive)
  (disco-room--ensure-action-available
   (when (disco-room--composer-edit-active-p)
     "attachments are unavailable while editing a message")
   "clear attachments")
  (setq disco-room--pending-attachments nil)
  (when disco-room--attachment-token-table
    (clrhash disco-room--attachment-token-table))
  (disco-room--set-draft
   (string-trim-right (disco-room--draft-without-attachment-tokens)))
  (message "disco: cleared queued attachments"))

(defun disco-room--clear-pending-attachment-state ()
  "Clear queued attachment state without touching the current draft text."
  (setq disco-room--pending-attachments nil)
  (when disco-room--attachment-token-table
    (clrhash disco-room--attachment-token-table)))

(defun disco-room--message-content-over-limit-p (content)
  "Return non-nil when CONTENT exceeds Discord's single-message limit."
  (and (stringp content)
       (> (length content) disco-api--message-content-limit)))

(defun disco-room--long-message-split-point (content)
  "Return preferred split point for CONTENT.

The split point is at most `disco-api--message-content-limit' and prefers
paragraph, line, and whitespace boundaries near the end of the chunk."
  (let* ((limit disco-api--message-content-limit)
         (len (length content))
         (max-end (min len limit))
         (min-acceptable (max 1 (/ limit 2))))
    (or (let ((pos (cl-search "\n\n" content :from-end t :end2 max-end)))
          (when (and pos (>= pos min-acceptable))
            (+ pos 2)))
        (let ((pos (cl-search "\n" content :from-end t :end2 max-end)))
          (when (and pos (>= pos min-acceptable))
            (1+ pos)))
        (let ((pos (cl-position-if (lambda (char)
                                     (memq char '(?\s ?\t)))
                                   content :from-end t :end max-end)))
          (when (and pos (>= pos min-acceptable))
            (1+ pos)))
        max-end)))

(defun disco-room--split-message-content (content)
  "Split CONTENT into Discord-sized message chunks."
  (let ((remaining (or content ""))
        (chunks nil))
    (while (disco-room--message-content-over-limit-p remaining)
      (let* ((split-point (disco-room--long-message-split-point remaining))
             (chunk (string-trim-right (substring remaining 0 split-point)))
             (rest (string-trim-left (substring remaining split-point))))
        (when (string-empty-p chunk)
          (setq split-point disco-api--message-content-limit
                chunk (substring remaining 0 split-point)
                rest (substring remaining split-point)))
        (push chunk chunks)
        (setq remaining rest)))
    (unless (string-empty-p remaining)
      (push remaining chunks))
    (nreverse chunks)))

(defun disco-room--write-long-message-temp-attachment (content)
  "Write CONTENT to a temporary text file attachment plist."
  (let ((path (make-temp-file "disco-message-" nil ".txt"))
        (coding-system-for-write 'utf-8))
    (with-temp-file path
      (insert (or content "")))
    (list :path path
          :filename disco-room-long-message-file-name
          :content-type "text/plain; charset=utf-8")))

(defun disco-room-send-message ()
  "Send current draft message to this room asynchronously.

When called with prefix argument, force draft edit in minibuffer first."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--send-message-unavailable-reason)
   (if (disco-room--composer-edit-active-p)
       "save edits"
     "send messages"))
  (cond
   (disco-room--send-in-flight
    (message "disco: send already in progress"))
   (t
    (let* ((current-draft (disco-room--current-draft))
           (current-draft-text (disco-chatbuf-string-plain-text current-draft))
           (initial-has-attachments
            (not (null (disco-room--attachments-from-draft current-draft))))
           (prompt-edit-p
            (or current-prefix-arg
                (and (string-empty-p (string-trim-right current-draft-text))
                     (not initial-has-attachments)))))
      (when (and current-prefix-arg
                 (disco-chatbuf-string-has-objects-p current-draft))
        (user-error "disco: minibuffer send-edit is unavailable for structured input objects"))
      (let* ((content (if prompt-edit-p
                          (read-from-minibuffer "Message: " current-draft-text)
                        current-draft))
             (parsed-input (disco-room--parse-draft-input content))
             (parsed-attachments (plist-get parsed-input :attachments))
             (has-attachments (not (null parsed-attachments)))
             (normalized (string-trim-right
                          (or (plist-get parsed-input :content) "")))
             (edit-message-id (disco-room--composer-edit-message-id))
             (over-limit-p (disco-room--message-content-over-limit-p normalized)))
        (if (and (string-empty-p normalized)
                 (not has-attachments)
                 (not edit-message-id))
            (message "disco: draft is empty")
          (let* ((room-buffer (current-buffer))
                 (channel-id disco-room--channel-id)
                 (reply-to (disco-room--composer-reply-message-id))
                 (allowed-mentions (disco-room--send-allowed-mentions
                                    (not (null reply-to))))
                 (attachments (copy-tree parsed-attachments))
                 (edit-message (and edit-message-id
                                    (disco-room--composer-context-message edit-message-id)))
                 (long-message-action
                  (and over-limit-p
                       (disco-room--input-option-long-message-action)))
                 (needs-attach-files-p (or has-attachments
                                           (eq long-message-action 'file)))
                 (required-permissions
                  (append
                   (disco-room--required-send-permissions)
                   (when needs-attach-files-p
                     '(attach-files))
                   (when reply-to
                     '(read-message-history)))))
            (if edit-message-id
                (progn
                  (disco-api--validate-message-content-length normalized "content")
                  (disco-room--ensure-action-available
                   (disco-room--edit-permission-reason edit-message)
                   "edit messages")
                  (when has-attachments
                    (user-error "disco: editing via composer does not support attachments yet"))
                  (disco-room--clear-draft)
                  (setq disco-room--send-in-flight t)
                  (disco-room--update-frame)
                  (disco-api-edit-message-async
                   channel-id
                   edit-message-id
                   normalized
                   :allowed-mentions (disco-room--send-allowed-mentions)
                   :on-success
                   (lambda (response)
                     (when (disco-room--channel-buffer-p room-buffer channel-id)
                       (with-current-buffer room-buffer
                         (unless (and (listp response) (alist-get 'id response))
                           (error "Discord edit-message returned no message"))
                         (disco-state-upsert-message channel-id response)
                         (setq disco-room--send-in-flight nil)
                         (disco-room--composer-edit-clear t)
                         (disco-room--render-send-state-change)
                         (message "disco: edited message %s" edit-message-id))))
                   :on-error
                   (lambda (err)
                     (when (disco-room--channel-buffer-p room-buffer channel-id)
                       (with-current-buffer room-buffer
                         (setq disco-room--send-in-flight nil)
                         (disco-room--apply-draft-state normalized)
                         (disco-room--update-frame)
                         (message "disco: edit failed for %s: %s"
                                  edit-message-id
                                  (disco-room--async-error-message err)))))))
              (disco-room--ensure-action-available
               (disco-room--room-send-restriction-reason
                (append (when needs-attach-files-p '(attach-files))
                        (when reply-to '(read-message-history))))
               "send messages")
              (disco-permission-ensure-channel
               (disco-room--channel-object)
               required-permissions
               :action "sending messages")
              (unless (string-empty-p normalized)
                (disco-chatbuf-input-history-push normalized))
              (disco-room--clear-draft)
              (setq disco-room--send-in-flight t)
              (disco-room--update-frame)
              (cl-labels
                  ((room-active-p ()
                     (disco-room--channel-buffer-p room-buffer channel-id))
                   (send-one (text reply attachments-list on-success on-error)
                     (let* ((nonce (disco-room--next-send-nonce))
                            (pending-content
                             (if (and attachments-list
                                      (or (not (stringp text)) (string-empty-p text)))
                                 (format "Uploading %d attachment%s…"
                                         (length attachments-list)
                                         (if (= (length attachments-list) 1) "" "s"))
                               text)))
                       (disco-state-insert-pending-message
                        channel-id nonce pending-content
                        (disco-gateway-current-user-id) reply)
                       (disco-room--render-send-state-change)
                       (let ((success
                              (lambda (response)
                                (if (and (listp response) (alist-get 'id response))
                                    (progn
                                      (disco-state-upsert-message channel-id response)
                                      (when (room-active-p)
                                        (with-current-buffer room-buffer
                                          (disco-room--render-send-state-change)))
                                      (funcall on-success response))
                                  (disco-state-remove-pending-message channel-id nonce)
                                  (funcall on-error
                                           (list 'error
                                                 "Discord create-message returned no message")))))
                             (failure
                              (lambda (err)
                                (disco-state-remove-pending-message channel-id nonce)
                                (when (room-active-p)
                                  (with-current-buffer room-buffer
                                    (disco-room--render-send-state-change)))
                                (funcall on-error err))))
                         (if attachments-list
                             (disco-api-send-message-with-attachments-async
                              channel-id
                              :content (and (stringp text) (not (string-empty-p text)) text)
                              :reply-to-message-id reply
                              :allowed-mentions (and (stringp text)
                                                     (not (string-empty-p text))
                                                     allowed-mentions)
                              :attachments attachments-list
                              :nonce nonce
                              :on-success success
                              :on-error failure)
                           (disco-api-send-message-async
                            channel-id
                            text
                            :reply-to-message-id reply
                            :allowed-mentions (and (stringp text)
                                                   (not (string-empty-p text))
                                                   allowed-mentions)
                            :nonce nonce
                            :on-success success
                            :on-error failure))))))
                (pcase long-message-action
                  ('split
                   (let* ((chunks (disco-room--split-message-content normalized))
                          (total (length chunks)))
                     (cl-labels
                         ((finish-success ()
                            (when (room-active-p)
                              (with-current-buffer room-buffer
                                (setq disco-room--send-in-flight nil)
                                (disco-room--set-composer-aux-state nil nil)
                                (when has-attachments
                                  (disco-room--clear-pending-attachment-state))
                                (message "disco: sent %d split messages" total))))
                          (finish-error (remaining sent-count err)
                            (when (room-active-p)
                              (with-current-buffer room-buffer
                                (setq disco-room--send-in-flight nil)
                                (if (> sent-count 0)
                                    (progn
                                      (disco-room--apply-draft-state
                                       (mapconcat #'identity remaining "\n\n"))
                                      (disco-room--set-composer-aux-state nil nil)
                                      (when has-attachments
                                        (disco-room--clear-pending-attachment-state))
                                      (disco-room--update-frame)
                                      (message "disco: sent %d/%d split messages; restored remaining draft: %s"
                                               sent-count total
                                               (disco-room--async-error-message err)))
                                  (disco-room--apply-draft-state content)
                                  (disco-room--update-frame)
                                  (message "disco: send failed: %s"
                                           (disco-room--async-error-message err))))))
                          (send-next (remaining sent-count)
                            (let ((chunk (car remaining))
                                  (rest (cdr remaining))
                                  (first-p (= sent-count 0)))
                              (send-one
                               chunk
                               (and first-p reply-to)
                               (and first-p attachments)
                               (lambda (_response)
                                 (if rest
                                     (send-next rest (1+ sent-count))
                                   (finish-success)))
                               (lambda (err)
                                 (finish-error remaining sent-count err))))))
                       (send-next chunks 0))))
                  ('file
                   (let* ((text-attachment
                           (disco-room--write-long-message-temp-attachment normalized))
                          (all-attachments (append attachments (list text-attachment))))
                     (unwind-protect
                         (send-one
                          nil
                          reply-to
                          all-attachments
                          (lambda (_response)
                            (when (room-active-p)
                              (with-current-buffer room-buffer
                                (setq disco-room--send-in-flight nil)
                                (disco-room--set-composer-aux-state nil nil)
                                (when has-attachments
                                  (disco-room--clear-pending-attachment-state))
                                (message "disco: long message sent as %s"
                                         disco-room-long-message-file-name))))
                          (lambda (err)
                            (when (room-active-p)
                              (with-current-buffer room-buffer
                                (setq disco-room--send-in-flight nil)
                                (disco-room--apply-draft-state content)
                                (disco-room--update-frame)
                                (message "disco: send failed: %s"
                                         (disco-room--async-error-message err))))))
                       (ignore-errors
                         (delete-file (plist-get text-attachment :path))))))
                  (_
                   (send-one
                    normalized
                    reply-to
                    attachments
                    (lambda (_response)
                      (when (room-active-p)
                        (with-current-buffer room-buffer
                          (setq disco-room--send-in-flight nil)
                          (disco-room--set-composer-aux-state nil nil)
                          (when has-attachments
                            (disco-room--clear-pending-attachment-state))
                          (message (if has-attachments
                                       "disco: message with attachment(s) sent"
                                     "disco: message sent")))))
                    (lambda (err)
                      (when (room-active-p)
                        (with-current-buffer room-buffer
                          (setq disco-room--send-in-flight nil)
                          (disco-room--apply-draft-state
                           (if has-attachments content normalized))
                          (disco-room--update-frame)
                          (message "disco: send failed: %s"
                                   (disco-room--async-error-message err)))))))))))))))))

(defun disco-room-load-older-messages ()
  "Load one older page for the current room view asynchronously."
  (interactive)
  (cond
   ((disco-room--msg-filter-active-p)
    (disco-room-filter-load-more))
   (disco-room--history-exhausted
    (message "disco: no older messages available"))
   (disco-room--older-in-flight
    (message "disco: older history load already in progress"))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (generation disco-room--refresh-generation)
           (request-revision (disco-state-message-revision channel-id))
           (before (or disco-room--oldest-message-id
                       (user-error "disco: no oldest message cursor; refresh first"))))
      (setq disco-room--older-in-flight t)
      (disco-room--update-frame)
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
                 (if (null older)
                     (progn
                       (setq disco-room--history-exhausted t)
                       (disco-room-render)
                       (disco-room--resolve-pending-jump)
                       (message "disco: reached beginning of history"))
                   (let ((merged (disco-state-merge-message-page
                                  channel-id older request-revision)))
                     (disco-room--update-message-window-state merged)
                     (disco-room-render)
                     (disco-room--resolve-pending-jump)
                     (message "disco: loaded %d older messages" (length older)))))))))
       :on-error
       (lambda (err)
         (when (buffer-live-p room-buffer)
           (with-current-buffer room-buffer
             (when (equal channel-id disco-room--channel-id)
               (setq disco-room--older-in-flight nil)
               (disco-room--update-frame)
               (message "disco: older history load failed: %s"
                        (disco-room--async-error-message err)))))))))))

(defun disco-room--reply-to-msg (msg)
  "Set pending reply target to MSG for the next send."
  (let ((message-id (alist-get 'id msg)))
    (unless (and (stringp message-id) (not (string-empty-p message-id)))
      (user-error "disco: message has no id to reply to"))
    (disco-room-reply-to-message message-id)))

(defun disco-room-reply-to-message (&optional message-id)
  "Set pending reply target MESSAGE-ID for next send.

When called interactively, defaults to message under point."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--reply-unavailable-reason)
      "start replies")
     (let* ((at-point (ignore-errors (disco-room--message-id-at-point)))
            (fallback (or at-point (disco-room--latest-message-id)))
            (raw (read-string
                  (if fallback
                      (format "Reply to message ID (default %s): " fallback)
                    "Reply to message ID: "))))
       (list (if (string-empty-p raw)
                 (or fallback
                     (user-error "disco: no target message available"))
               raw)))))
  (disco-room--ensure-action-available
   (disco-room--reply-unavailable-reason)
   "start replies")
  (when (disco-room--composer-edit-active-p)
    (disco-room--composer-edit-clear t))
  (disco-room--set-composer-aux-state nil message-id)
  (disco-room--update-frame)
  (message "disco: next message will reply to %s" message-id))

(defun disco-room--forward-msg (msg)
  "Forward MSG into the current room, prompting only for optional extras."
  (let* ((message-id (alist-get 'id msg))
         (source-channel-id (or (alist-get 'channel_id msg) disco-room--channel-id))
         (content-raw (string-trim (read-string "Optional forward comment: ")))
         (forward-only (disco-room--read-forward-only source-channel-id message-id)))
    (unless (and (stringp message-id) (not (string-empty-p message-id)))
      (user-error "disco: message has no id to forward"))
    (unless (and (stringp source-channel-id) (not (string-empty-p source-channel-id)))
      (user-error "disco: message has no source channel id to forward"))
    (disco-room-forward-message
     message-id
     source-channel-id
     (unless (string-empty-p content-raw)
       content-raw)
     forward-only)))

(defun disco-room-forward-message (&optional message-id source-channel-id content forward-only)
  "Forward MESSAGE-ID from SOURCE-CHANNEL-ID into current room.

CONTENT is optional text sent alongside the forwarded reference.
FORWARD-ONLY optionally narrows embeds/attachments included in the forward."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--forward-unavailable-reason)
      "forward messages")
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
             forward-only))))
  (disco-room--ensure-action-available
   (disco-room--forward-unavailable-reason)
   "forward messages")
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
    (disco-api--validate-message-content-length normalized-content "content")
    (unless (and message-id (not (string-empty-p (format "%s" message-id))))
      (user-error "disco: message id cannot be empty"))
    (unless (and source-channel-id
                 (not (string-empty-p (format "%s" source-channel-id))))
      (user-error "disco: source channel id cannot be empty"))
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     (disco-room--required-send-permissions)
     :action "forwarding messages")
    (disco-room--ensure-jump-permissions source-channel-id source-channel)
    (setq disco-room--send-in-flight t)
    (disco-room--update-frame)
    (cl-labels
        ((room-active-p ()
           (disco-room--channel-buffer-p room-buffer target-channel-id))
         (finish-success (response)
           (when (room-active-p)
             (with-current-buffer room-buffer
               (unless (and (listp response) (alist-get 'id response))
                 (error "disco: forward response has no message id"))
               (disco-state-upsert-message target-channel-id response)
               (setq disco-room--send-in-flight nil)
               (disco-room--render-send-state-change)
               (message "disco: forwarded message %s from channel %s"
                        message-id source-channel-id))))
         (finish-error (text)
           (when (room-active-p)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room--update-frame)
               (message "%s" text))))
         (send-forward (forward-content)
           (when (room-active-p)
             (disco-api-forward-message-async
              target-channel-id
              message-id
              source-channel-id
              :content forward-content
              :forward-only forward-only
              :allowed-mentions (and forward-content allowed-mentions)
              :on-success #'finish-success
              :on-error
              (lambda (err)
                (finish-error
                 (format "disco: forward failed: %s"
                         (disco-room--async-error-message err))))))))
      (send-forward normalized-content))))

(defun disco-room-cancel-reply ()
  "Cancel pending composer reply/edit context."
  (interactive)
  (cond
   ((disco-room--composer-edit-active-p)
    (disco-room--composer-edit-clear t)
    (disco-room--update-frame)
    (message "disco: edit target cleared"))
   ((disco-room--composer-reply-message-id)
    (disco-room--set-composer-aux-state nil nil)
    (disco-room--update-frame)
    (message "disco: reply target cleared"))
   (t
    (message "disco: no composer context to cancel"))))

(defun disco-room-return-dwim ()
  "RET behavior for room buffer.

When send-on-return is enabled, send current draft.  Otherwise open the draft
editor."
  (interactive)
  (if (disco-room--input-option-send-on-return)
      (disco-room-send-message)
    (disco-room-edit-draft)))

(defun disco-room-toggle-breakline ()
  "Toggle visual breakline wrapping in the current room buffer."
  (interactive)
  (setq-local disco-room-wrap-long-lines (not disco-room-wrap-long-lines))
  (disco-room--apply-breakline-settings)
  (message "disco: breakline wrapping %s"
           (if disco-room-wrap-long-lines "enabled" "disabled")))

(defun disco-room--edit-msg (msg)
  "Enter composer edit mode for MSG in current room."
  (disco-room--ensure-action-available
   (disco-room--edit-start-unavailable-reason nil)
   "edit messages")
  (disco-room--ensure-action-available
   (disco-room--edit-start-unavailable-reason msg)
   "edit messages")
  (disco-room--composer-enter-edit msg))

(defun disco-room-edit-message ()
  "Enter composer edit mode for message at point in current room."
  (interactive)
  (disco-room--edit-msg (disco-room--message-at-point)))

(defun disco-room--delete-msg (msg)
  "Delete MSG in current room."
  (let ((message-id (alist-get 'id msg)))
    (disco-room--ensure-action-available
     (disco-room--delete-message-unavailable-reason msg)
     "delete messages")
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

(defun disco-room-delete-message ()
  "Delete message at point in current room."
  (interactive)
  (disco-room--delete-msg (disco-room--message-at-point)))

(defun disco-room-create-thread-from-message (name message-id
                                                   &optional auto-archive-duration
                                                   rate-limit-per-user)
  "Create thread NAME from MESSAGE-ID in current channel.

AUTO-ARCHIVE-DURATION is optional minutes.
RATE-LIMIT-PER-USER is optional slowmode seconds."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--thread-create-from-message-unavailable-reason)
      "create threads from messages")
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
            (auto-archive-duration (disco-thread-read-auto-archive-duration nil nil))
            (rate-limit-per-user
             (disco-room--read-optional-nonnegative-int
              "Slowmode seconds (empty for none): ")))
       (list name message-id auto-archive-duration rate-limit-per-user))))
  (disco-room--ensure-action-available
   (disco-room--thread-create-from-message-unavailable-reason)
   "create threads from messages")
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
   (progn
     (disco-room--ensure-action-available
      (disco-room--thread-create-unavailable-reason :any)
      "create detached threads")
     (let* ((name (read-string "Thread name: "))
            (type (unless (disco-thread-forum-or-media-channel-p (disco-room--channel-object))
                    (disco-thread-read-detached-type)))
            (auto-archive-duration (disco-thread-read-auto-archive-duration nil nil))
            (invitable (when (equal type 12)
                         (y-or-n-p "Invitable by non-moderators? ")))
            (rate-limit-per-user
             (disco-room--read-optional-nonnegative-int
              "Slowmode seconds (empty for none): ")))
       (list name type auto-archive-duration invitable rate-limit-per-user))))
  (disco-room--ensure-action-available
   (disco-room--thread-create-unavailable-reason (or type :any))
   "create detached threads")
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
  (disco-room--ensure-action-available
   (disco-room--thread-join-unavailable-reason)
   "join threads")
  (disco-room--ensure-thread-channel)
  (disco-api-join-thread disco-room--channel-id)
  (when-let* ((self-id (disco-gateway-current-user-id)))
    (disco-state-upsert-thread-member disco-room--channel-id self-id))
  (message "disco: joined thread %s" disco-room--channel-name))

(defun disco-room-leave-thread ()
  "Leave current thread room as current user."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--thread-leave-unavailable-reason)
   "leave threads")
  (disco-room--ensure-thread-channel)
  (disco-api-leave-thread disco-room--channel-id)
  (when-let* ((self-id (disco-gateway-current-user-id)))
    (disco-state-delete-thread-member disco-room--channel-id self-id))
  (message "disco: left thread %s" disco-room--channel-name))

(defun disco-room-toggle-thread-archived ()
  "Toggle archived state for current thread."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--thread-toggle-archived-unavailable-reason)
   "toggle thread archived state")
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-archived (not (disco-thread-archived-p channel)))
         (updated (disco-api-set-thread-archived
                   disco-room--channel-id next-archived nil)))
    (disco-room--resolve-thread-update updated)
    (disco-room-render)
    (message "disco: thread %s" (if next-archived "archived" "unarchived"))))

(defun disco-room-rename-thread (name)
  "Rename current thread to NAME."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--thread-update-unavailable-reason)
      "rename threads")
     (let* ((channel (or (disco-room--channel-object)
                         (user-error "disco: unknown thread in state")))
            (current-name (or (alist-get 'name channel) ""))
            (name (string-trim
                   (read-string "Thread name: " current-name))))
       (list name))))
  (disco-room--ensure-action-available
   (disco-room--thread-update-unavailable-reason)
   "rename threads")
  (disco-room--ensure-thread-channel)
  (when (string-empty-p name)
    (user-error "disco: thread name cannot be empty"))
  (unless (disco-room--channel-object)
    (user-error "disco: unknown thread in state"))
  (let ((updated (disco-api-update-thread disco-room--channel-id :name name)))
    (disco-room--resolve-thread-update updated)
    (disco-room-render)
    (message "disco: thread renamed to %s" name)))

(defun disco-room-toggle-thread-locked ()
  "Toggle locked state for current thread."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--thread-update-unavailable-reason)
   "toggle thread locked state")
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (next-locked (not (disco-thread-locked-p channel)))
         (updated (disco-api-update-thread
                   disco-room--channel-id :locked next-locked)))
    (disco-room--resolve-thread-update updated)
    (disco-room-render)
    (message "disco: thread %s" (if next-locked "locked" "unlocked"))))

(defun disco-room-set-thread-slowmode (seconds)
  "Set current thread slowmode to SECONDS.

When called interactively, empty input clears slowmode (sets to 0)."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--thread-update-unavailable-reason)
      "set thread slowmode")
     (list (or (disco-room--read-optional-nonnegative-int
                "Slowmode seconds (empty clears to 0): ")
               0))))
  (disco-room--ensure-action-available
   (disco-room--thread-update-unavailable-reason)
   "set thread slowmode")
  (disco-room--ensure-thread-channel)
  (unless (disco-room--channel-object)
    (user-error "disco: unknown thread in state"))
  (let ((updated (disco-api-update-thread
                  disco-room--channel-id
                  :rate-limit-per-user seconds)))
    (disco-room--resolve-thread-update updated)
    (disco-room-render)
    (message "disco: thread slowmode -> %ss" seconds)))

(defun disco-room-set-thread-auto-archive-duration (minutes)
  "Set current thread auto archive duration to MINUTES."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--thread-update-unavailable-reason)
      "set thread auto archive duration")
     (let* ((channel (or (disco-room--channel-object)
                         (user-error "disco: unknown thread in state")))
            (meta (disco-thread-metadata channel))
            (current (or (alist-get 'auto_archive_duration meta)
                         (alist-get 'auto_archive_duration channel))))
       (list (disco-thread-read-auto-archive-duration t current)))))
  (disco-room--ensure-action-available
   (disco-room--thread-update-unavailable-reason)
   "set thread auto archive duration")
  (disco-room--ensure-thread-channel)
  (unless (disco-room--channel-object)
    (user-error "disco: unknown thread in state"))
  (let ((updated (disco-api-update-thread
                  disco-room--channel-id
                  :auto-archive-duration minutes)))
    (disco-room--resolve-thread-update updated)
    (disco-room-render)
    (message "disco: auto archive -> %s minutes" minutes)))

(defun disco-room-set-thread-muted (muted)
  "Set current user's muted state for current thread to MUTED."
  (interactive
   (progn
     (disco-room--ensure-action-available
      (disco-room--thread-mute-unavailable-reason)
      "set thread mute state")
     (list (y-or-n-p "Mute this thread? "))))
  (disco-room--ensure-action-available
   (disco-room--thread-mute-unavailable-reason)
   "set thread mute state")
  (disco-room--ensure-thread-channel)
  (disco-api-update-thread-member-settings disco-room--channel-id :muted muted)
  (message "disco: thread notifications %s" (if muted "muted" "unmuted")))

(defun disco-room-edit-thread-settings ()
  "Edit multiple thread settings in one PATCH request."
  (interactive)
  (disco-room--ensure-action-available
   (disco-room--thread-update-unavailable-reason)
   "edit thread settings")
  (disco-room--ensure-thread-channel)
  (let* ((channel (or (disco-room--channel-object)
                      (user-error "disco: unknown thread in state")))
         (meta (disco-thread-metadata channel))
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
          (disco-thread-read-tristate-bool
           "Archived"
           (disco-thread-archived-p channel)))
         (locked-choice
          (disco-thread-read-tristate-bool
           "Locked"
           (disco-thread-locked-p channel)))
         (archived (unless (eq archived-choice 'keep) archived-choice))
         (locked (unless (eq locked-choice 'keep) locked-choice))
         (has-change (or name
                         auto-archive-duration
                         (not (null rate-limit-per-user))
                         (not (eq archived-choice 'keep))
                         (not (eq locked-choice 'keep)))))
    (unless has-change
      (user-error "disco: no thread setting changes provided"))
    (let ((updated
           (disco-api-update-thread
            disco-room--channel-id
            :name name
            :auto-archive-duration auto-archive-duration
            :rate-limit-per-user rate-limit-per-user
            :archived archived
            :locked locked)))
      (disco-room--resolve-thread-update updated)
      (disco-room-render)
      (message "disco: updated thread settings"))))

(declare-function disco-root-list-archived-threads "disco-root" (&optional parent-channel-id))

(defun disco-room-open-parent-archived-threads ()
  "Open archived thread browser for current room's parent channel."
  (interactive)
  (let* ((channel (and disco-room--channel-id
                       (disco-state-channel disco-room--channel-id)))
         (parent-id (and channel (alist-get 'parent_id channel))))
    (unless parent-id
      (user-error "disco: current room has no parent channel"))
    (disco-root-list-archived-threads parent-id)))

(transient-define-prefix disco-room-message-transient ()
  "Transient for msg-centric room actions at point."
  [["Message"
    ("c" "Copy dwim" disco-msg-copy-dwim)
    ("l" "Copy link" disco-msg-copy-link)
    ("t" "Copy text" disco-msg-copy-text)
    ("i" "Describe" disco-msg-describe-message)
    ("L" "Redisplay" disco-msg-redisplay)
    ("r" "Reply" disco-msg-reply
     :inapt-if disco-room--reply-unavailable-reason)
    ("f" "Forward" disco-msg-forward
     :inapt-if disco-room--forward-unavailable-reason)
    ("e" "Edit" disco-msg-edit
     :inapt-if #'disco-room-menu--edit-inapt-reason)
    ("d" "Delete" disco-msg-delete
     :inapt-if #'disco-room-menu--delete-inapt-reason)
    ("!" "Add reaction" disco-msg-add-reaction
     :inapt-if #'disco-room-menu--reaction-inapt-reason)
    ("+" "Toggle reaction" disco-msg-toggle-reaction
     :inapt-if #'disco-room-menu--reaction-inapt-reason)
    ("-" "Remove reaction" disco-msg-remove-reaction
     :inapt-if #'disco-room-menu--reaction-inapt-reason)
    ("T" "Open thread" disco-msg-open-thread)]
   ["Media"
    ("o" "Open / play" disco-media-card-open
     :inapt-if (lambda () (disco-media-card-action-inapt-reason 'open)))
    ("D" "Download / retry" disco-media-card-download
     :inapt-if (lambda () (disco-media-card-action-inapt-reason 'download)))
    ("C" "Cancel download" disco-media-card-cancel-download
     :inapt-if (lambda () (disco-media-card-action-inapt-reason 'cancel)))
    ("s" "Save as" disco-media-card-save-as
     :inapt-if (lambda () (disco-media-card-action-inapt-reason 'save-as)))
    ("y" "Copy media URL" disco-media-card-copy-url
     :inapt-if (lambda () (disco-media-card-action-inapt-reason 'copy-url)))]])

(defun disco-room--operate-msg (_msg)
  "Open the message transient for the current room.

_MSG is ignored because the transient resolves availability from point."
  (disco-room-message-transient))

(defun disco-room-menu--attachment-action-inapt-reason (min-count)
  "Return inapt text for attachment actions requiring MIN-COUNT items."
  (disco-room--attachment-token-action-unavailable-reason min-count))

(defun disco-room-menu--message-at-point ()
  "Return message at point, suppressing user errors for menu checks."
  (ignore-errors (disco-room--message-at-point)))

(defun disco-room-menu--edit-inapt-reason ()
  "Return inapt text for editing the message at point."
  (disco-room--edit-start-unavailable-reason
   (disco-room-menu--message-at-point)))

(defun disco-room-menu--delete-inapt-reason ()
  "Return inapt text for deleting the message at point."
  (disco-room--delete-message-unavailable-reason
   (disco-room-menu--message-at-point)))

(defun disco-room-menu--reaction-inapt-reason ()
  "Return inapt text for reaction actions at point."
  (disco-room--reaction-unavailable-reason
   (disco-room-menu--message-at-point)))

(defun disco-room-menu--poll-vote-inapt-reason ()
  "Return inapt text for poll vote actions at point."
  (disco-room--poll-vote-unavailable-reason
   (disco-room-menu--message-at-point)))

(defun disco-room-menu--poll-submit-inapt-reason ()
  "Return inapt text for submitting a staged poll vote at point."
  (disco-room--poll-submit-unavailable-reason
   (disco-room-menu--message-at-point)))

(defun disco-room-menu--poll-clear-inapt-reason ()
  "Return inapt text for clearing a poll vote at point."
  (disco-room--poll-clear-unavailable-reason
   (disco-room-menu--message-at-point)))

(defun disco-room-menu--poll-expire-inapt-reason ()
  "Return inapt text for expiring a poll at point."
  (disco-room--poll-expire-unavailable-reason
   (disco-room-menu--message-at-point)))

(transient-define-prefix disco-room-attach-transient ()
  "Transient for telega-like room attachment commands."
  [["Attach"
    ("f" "Attach file" disco-room-attach-file
     :inapt-if disco-room--attach-unavailable-reason)
    ("p" "Send poll" disco-room-send-poll
     :inapt-if disco-room--poll-unavailable-reason)
    ("v" "Attach clipboard" disco-room-attach-clipboard)]])

(transient-define-prefix disco-room-transient ()
  "Room command menu for disco.el."
  [["Timeline"
    ("g" "Refresh room" disco-room-refresh)
    ("o" "Load older" disco-room-load-older-messages)
    ("c" "Send message" disco-room-send-message
     :inapt-if disco-room--send-message-unavailable-reason)
    ("f" "Attach file" disco-room-attach-file
     :inapt-if disco-room--attach-unavailable-reason)
    ("D" "Remove attachment" disco-room-remove-attachment-token-at-point
     :inapt-if (lambda ()
                 (disco-room-menu--attachment-action-inapt-reason 1)))
    ("x" "Clear attachments" disco-room-clear-attachments
     :inapt-if (lambda ()
                 (disco-room-menu--attachment-action-inapt-reason 1)))
    ("v" "List attachments" disco-room-list-attachments
     :inapt-if (lambda ()
                 (disco-room-menu--attachment-action-inapt-reason 1)))
    ("V" "Edit attachment desc" disco-room-edit-attachment-description
     :inapt-if (lambda ()
                 (disco-room-menu--attachment-action-inapt-reason 1)))
    ("O" "Reorder attachments" disco-room-reorder-attachments
     :inapt-if (lambda ()
                 (disco-room-menu--attachment-action-inapt-reason 2)))
    ("r" "Reply to message" disco-room-reply-to-message
     :inapt-if disco-room--reply-unavailable-reason)
    ("F" "Forward message" disco-room-forward-message
     :inapt-if disco-room--forward-unavailable-reason)
    ("k" "Cancel reply/edit" disco-room-cancel-reply
     :inapt-if (lambda () (not (disco-room--composer-aux-active-p))))
    ("e" "Edit at point" disco-room-edit-message
     :inapt-if #'disco-room-menu--edit-inapt-reason)
    ("d" "Delete at point" disco-room-delete-message
     :inapt-if #'disco-room-menu--delete-inapt-reason)
    ("!" "Add reaction" disco-room-add-reaction
     :inapt-if #'disco-room-menu--reaction-inapt-reason)
    ("+" "Toggle reaction" disco-room-toggle-reaction
     :inapt-if #'disco-room-menu--reaction-inapt-reason)
    ("-" "Remove reaction" disco-room-remove-reaction
     :inapt-if #'disco-room-menu--reaction-inapt-reason)
    ("p" "Send poll" disco-room-send-poll
     :inapt-if disco-room--poll-unavailable-reason)
    ("w" "Select answer" disco-room-vote-poll-answer
     :inapt-if #'disco-room-menu--poll-vote-inapt-reason)
    ("u" "Unselect answer" disco-room-remove-poll-vote
     :inapt-if #'disco-room-menu--poll-vote-inapt-reason)
    ("t" "Toggle staged answer" disco-room-toggle-poll-answer
     :inapt-if #'disco-room-menu--poll-vote-inapt-reason)
    ("W" "Submit staged vote" disco-room-submit-poll-vote
     :inapt-if #'disco-room-menu--poll-submit-inapt-reason)
    ("C" "Remove my vote" disco-room-clear-poll-votes
     :inapt-if #'disco-room-menu--poll-clear-inapt-reason)
    ("X" "End poll" disco-room-expire-poll
     :inapt-if #'disco-room-menu--poll-expire-inapt-reason)
    ("P" "Ack pinned msgs" disco-room-ack-channel-pins)]
   ["Thread"
    ("m" "Create from message" disco-room-create-thread-from-message
     :inapt-if disco-room--thread-create-from-message-unavailable-reason)
    ("o" "Open msg thread" disco-room-open-thread-from-message-at-point)
    ("n" "Create detached" disco-room-create-thread
     :inapt-if (lambda () (disco-room--thread-create-unavailable-reason :any)))
    ("R" "Rename thread" disco-room-rename-thread
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("L" "Toggle locked" disco-room-toggle-thread-locked
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("S" "Set slowmode" disco-room-set-thread-slowmode
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("U" "Set auto-archive" disco-room-set-thread-auto-archive-duration
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("E" "Edit thread settings" disco-room-edit-thread-settings
     :inapt-if disco-room--thread-update-unavailable-reason)
    ("M" "Set muted" disco-room-set-thread-muted
     :inapt-if disco-room--thread-mute-unavailable-reason)
    ("j" "Join thread" disco-room-join-thread
     :inapt-if disco-room--thread-join-unavailable-reason)
    ("l" "Leave thread" disco-room-leave-thread
     :inapt-if disco-room--thread-leave-unavailable-reason)
    ("a" "Toggle archived" disco-room-toggle-thread-archived
     :inapt-if disco-room--thread-toggle-archived-unavailable-reason)
    ("A" "Parent archived threads..." disco-room-open-parent-archived-threads)]
   ["Inspect"
    ("/" "Search channel..." disco-room-search-channel)
    ("f" "Filter search" disco-room-filter-search)
    ("F" "Cancel filter" disco-room-filter-cancel)
    ("v" "Refetch avatars" disco-room-refetch-avatars)
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Window"
    ("q" "Quit window" quit-window)]])

(defvar disco-room-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "TAB") #'disco-room-complete-mention)
    (define-key map (kbd "<tab>") #'disco-room-complete-mention)
    (define-key map (kbd "C-M-i") #'disco-room-complete-mention)
    (define-key map (kbd "C-c g") #'disco-room-refresh)
    (define-key map (kbd "C-c s") #'disco-room-search)
    (define-key map (kbd "C-c n") #'disco-room-search-next)
    (define-key map (kbd "C-c p") #'disco-room-search-prev)
    (define-key map (kbd "C-c m") disco-room-message-prefix-map)
    (define-key map (kbd "M-<") #'disco-room-load-older-messages)
    (define-key map (kbd "RET") #'disco-room-return-dwim)
    (define-key map (kbd "M-RET") #'disco-room-input-preview)
    (define-key map (kbd "C-c '") #'disco-room-edit-draft)
    (define-key map (kbd "M-p") #'disco-room-draft-prev)
    (define-key map (kbd "M-n") #'disco-room-draft-next)
    (define-key map (kbd "M-r") #'disco-room-draft-history-search)
    (define-key map (kbd "M-g s") #'disco-room-inplace-search)
    (define-key map (kbd "M-g n") #'disco-room-inplace-search-next)
    (define-key map (kbd "M-g p") #'disco-room-inplace-search-prev)
    (define-key map (kbd "C-c C-r") #'disco-room-inplace-search-query)
    (define-key map (kbd "C-c C-s") #'disco-room-inplace-search-query-forward)
    (define-key map (kbd "C-c /") #'disco-room-filter-search)
    (define-key map (kbd "C-c C-/") #'disco-room-filter-cancel)
    (define-key map (kbd "C-c M-/") #'disco-room-search-channel)
    (define-key map (kbd "C-c C-p s") #'disco-room-send-poll)
    (define-key map (kbd "C-c C-p +") #'disco-room-vote-poll-answer)
    (define-key map (kbd "C-c C-p -") #'disco-room-remove-poll-vote)
    (define-key map (kbd "C-c C-p t") #'disco-room-toggle-poll-answer)
    (define-key map (kbd "C-c C-p v") #'disco-room-submit-poll-vote)
    (define-key map (kbd "C-c C-p c") #'disco-room-clear-poll-votes)
    (define-key map (kbd "C-c C-p e") #'disco-room-expire-poll)
    (define-key map (kbd "C-c C-P") #'disco-room-ack-channel-pins)
    (define-key map (kbd "C-c C-c") #'disco-room-send-message)
    (define-key map (kbd "C-c C-a") #'disco-room-attach-transient)
    (define-key map (kbd "C-c C-f") #'disco-room-attach-file)
    (define-key map (kbd "C-c C-v") #'disco-room-attach-clipboard)
    (define-key map (kbd "C-c C-e") #'disco-room-input-formatting-set)
    (define-key map (kbd "C-c C-o") #'disco-room-input-options-transient)
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
    (define-key map (kbd "C-c C-t a") #'disco-room-toggle-thread-archived)
    (define-key map (kbd "C-c C-t A") #'disco-room-set-thread-auto-archive-duration)
    (define-key map (kbd "C-c C-t e") #'disco-room-edit-thread-settings)
    (define-key map (kbd "C-c C-t u") #'disco-room-set-thread-muted)
    (define-key map (kbd "C-c C-j") #'disco-room-join-thread)
    (define-key map (kbd "C-c C-l") #'disco-room-leave-thread)
    (define-key map (kbd "C-c M-v") #'disco-room-refetch-avatars)
    (define-key map (kbd "C-c ?") #'disco-room-transient)
    map)
  "Keymap for `disco-room-mode'.")

(define-derived-mode disco-room-mode nil "Disco-Room"
  "Major mode for disco.el room buffers."
  (disco-chatbuf-mode-setup)
  (disco-room--apply-breakline-settings)
  ;; Avoid visible seams between vertically sliced inline images.
  (setq-local line-spacing 0)
  ;; Strip visual-only line prefixes from copied text.
  (setq-local filter-buffer-substring-function
              #'disco-room--buffer-substring-filter)
  (disco-room--typing-cancel-expire-timer)
  (disco-chatbuf-reset-state disco-room-input-history-size)
  (disco-room--set-composer-aux-state nil nil)
  (disco-room--sync-shared-input-options-state)
  (setq-local disco-room--send-in-flight nil)
  (setq-local disco-room--pending-jump-message-id nil)
  (setq-local disco-room--jump-in-flight nil)
  (setq-local disco-room--last-search-query nil)
  (setq-local disco-room--msg-filter nil)
  (setq-local disco-msg-resolve-function #'disco-room--resolve-message)
  (setq-local disco-msg-content-text-function #'disco-room--message-copy-text)
  (setq-local disco-msg-reply-function #'disco-room--reply-to-msg)
  (setq-local disco-msg-forward-function #'disco-room--forward-msg)
  (setq-local disco-msg-operate-function #'disco-room--operate-msg)
  (setq-local disco-msg-edit-function #'disco-room--edit-msg)
  (setq-local disco-msg-delete-function #'disco-room--delete-msg)
  (setq-local disco-msg-open-thread-function #'disco-room--open-thread-from-message)
  (setq-local disco-msg-toggle-reaction-function #'disco-room--toggle-reaction-on-msg)
  (setq-local disco-msg-add-reaction-function #'disco-room--add-reaction-to-msg)
  (setq-local disco-msg-remove-reaction-function #'disco-room--remove-reaction-from-msg)
  (setq-local disco-msg-redisplay-function #'disco-room--redisplay-msg)
  (setq-local disco-media-card-fallback-context-function
              #'disco-room--media-card-fallback-context)
  (setq-local disco-room--filter-generation 0)
  (setq-local disco-room--filter-in-flight nil)
  (setq-local disco-room--inplace-search-filter nil)
  (setq-local disco-room--inplace-search-generation 0)
  (disco-chat-timeline-reset)
  (setq-local disco-room--chat-fill-column nil)
  (setq-local disco-room--pending-attachments nil)
  (setq-local disco-room--attachment-token-table (make-hash-table :test #'equal))
  (setq-local disco-room--attachment-token-seq 0)
  (setq-local disco-room--typing-users (make-hash-table :test #'equal))
  (setq-local disco-room--typing-expire-timer nil)
  (setq-local disco-room--poll-selection-drafts (make-hash-table :test #'equal))
  (setq-local disco-room--revealed-spoiler-message-id nil)
  (setq-local disco-room--optimistic-read-ack-seq 0)
  (setq-local disco-room--pending-optimistic-read-ack nil)
  (funcall #'disco-company-setup-room-buffer)
  (add-hook 'window-size-change-functions #'disco-room--on-window-size-change nil t)
  (add-hook 'display-line-numbers-mode-hook #'disco-room--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'disco-room--on-text-scale-change nil t)
  (add-hook 'after-change-functions #'disco-room--after-change nil t)
  (add-hook 'post-command-hook #'disco-room--post-command nil t)
  (disco-room--update-context-mode))

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
    (pop-to-buffer buf)
    (with-current-buffer buf
      (disco-room--on-window-size-change))))

(provide 'disco-room)

;;; disco-room.el ends here
