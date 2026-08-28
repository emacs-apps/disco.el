;;; disco-customize.el --- Customization for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors
;; Keywords: comm

;;; Commentary:

;; User customization options for disco.el.

;;; Code:

(require 'subr-x)

(defgroup disco nil
  "Discord client for Emacs."
  :group 'comm)

(defgroup disco-modes nil
  "Global presentation modes for disco.el."
  :group 'disco)

(defcustom disco-title-bracket-rules
  '((user "{" "}")
    (ephemeral-user "⦃" "⦄")
    (group "(" ")")
    (guild "[[" "]]")
    (channel "[" "]")
    (announcement "⟪" "⟫")
    (thread "⟨" "⟩")
    (private-thread "⦇" "⦈")
    (voice "「" "」")
    (stage "『" "』")
    (forum "⟦" "⟧")
    (media "【" "】")
    (directory "〔" "〕")
    (lobby "⌜" "⌝")
    (none "" "")
    (t "[" "]"))
  "Ordered rules selecting Discord presentation delimiters.

Each rule is (SELECTOR OPEN CLOSE); the first matching rule wins.  SELECTOR
may be a presentation kind symbol, t as a fallback, (channel-type TYPE...)
for exact numeric Discord channel types, or a function called with
(KIND SUBJECT).  OPEN may include a type prefix: `(channel \"#[\" \"]\")'
renders an ordinary channel as `#[name]'.  Appkit measures the complete
delimiters by display width."
  :type
  '(repeat
    (list :tag "Delimiter rule"
          (sexp :tag "Selector")
          (string :tag "Opening delimiter")
          (string :tag "Closing delimiter")))
  :group 'disco)

(defcustom disco-mode-line-string-format
  '("  " (:eval (disco-client-mode-line-icon))
    (:eval (disco-client-mode-line-unread))
    (:eval (disco-client-mode-line-mentions)))
  "Format used by `disco-client-mode-line-mode'."
  :type 'sexp
  :group 'disco-modes)

(defface disco-mode-line-unread
  '((t :inherit warning :weight bold))
  "Face for unread Discord channels in the mode line."
  :group 'disco-modes)

(defface disco-mode-line-mention
  '((t :inherit error :weight bold))
  "Face for unread Discord mentions in the mode line."
  :group 'disco-modes)

(defgroup disco-notifications nil
  "Desktop notifications for disco.el."
  :group 'disco)

(defcustom disco-notifications-delay 0.5
  "Seconds to delay a notification before rechecking room visibility."
  :type 'number :group 'disco-notifications)

(defcustom disco-notifications-timeout 4.0
  "Seconds before closing the current desktop notification.

Nil leaves notification lifetime to the desktop server."
  :type '(choice (const :tag "Desktop default" nil) number)
  :group 'disco-notifications)

(defcustom disco-notifications-max-message-age 60
  "Maximum incoming message age in seconds eligible for notification."
  :type 'integer :group 'disco-notifications)

(defcustom disco-notifications-body-limit 160
  "Maximum notification body width in characters."
  :type 'integer :group 'disco-notifications)

(defcustom disco-notifications-show-preview t
  "When non-nil, include a compact message preview."
  :type 'boolean :group 'disco-notifications)

(defcustom disco-notifications-history-ring-size 30
  "Number of recent desktop notifications retained."
  :type 'integer :group 'disco-notifications)

(defcustom disco-notifications-extra-args nil
  "Additional keyword arguments passed to `notifications-notify'."
  :type '(repeat sexp) :group 'disco-notifications)

;;; Room buffers

(defcustom disco-room-attach-commands
  '(("file" disco-room-attach-file)
    ("sticker" disco-room-send-sticker)
    ("poll" disco-room-send-poll))
  "Attachment commands offered by `disco-room-attach'.

Each entry has the form (NAME COMMAND).  NAME is the stable completion
candidate shown after `C-c C-a'; COMMAND must be interactive.  Users and
extensions may append Discord attachment kinds without replacing the
dispatcher."
  :type '(alist :key-type (string :tag "Attachment name")
                :value-type (list function))
  :group 'disco)

;;;; Controller

(defcustom disco-room-history-auto-load-threshold 2000
  "Character distance from a timeline edge that triggers history paging.

Nil disables automatic history paging.  Filtered search views never use this
gate because their result order is not a continuous channel history window."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'disco)

(defcustom disco-room-enable-company-backend t
  "When non-nil, register `disco-room-company-completion' for room buffers.

The backend is only used when `company' is loaded and `company-mode' is
active."
  :type 'boolean
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

(defface disco-room-typing-indicator
  '((t :inherit shadow :slant italic))
  "Face used for transient typing indicator text near the room prompt."
  :group 'disco)
(defcustom disco-room-jump-context-limit 50
  "Number of messages to request around a jump target.

Used by `disco-room-jump-to-message' to center the timeline around the
requested message instead of linearly paginating backward from the latest
page."
  :type 'integer
  :group 'disco)

;;;; Composer

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

;;;; Rendering

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

;;;; Search

(defcustom disco-room-search-filter-limit 25
  "Number of search hits to request per room filter-search page."
  :type 'integer
  :group 'disco)

(defface disco-room-search-highlight
  '((t :inherit isearch))
  "Face used to highlight active room search query matches."
  :group 'disco)

;;;; Reactions

(defcustom disco-room-show-reactions t
  "When non-nil, render reaction chips under each message."
  :type 'boolean
  :group 'disco)
(defface disco-room-reaction
  '((t :inherit mode-line-inactive))
  "Face used for unselected reaction chips."
  :group 'disco)

(defface disco-room-reaction-selected
  '((t :inherit success :weight bold))
  "Face used for reactions selected by the current user."
  :group 'disco)

;;;; Polls

(defcustom disco-room-poll-default-duration-hours 24
  "Default duration in hours used by `disco-room-send-poll'."
  :type 'integer
  :group 'disco)

(defcustom disco-room-poll-max-options 10
  "Maximum number of options collected by `disco-room-send-poll'."
  :type 'integer
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

;;; Authentication and API

(defcustom disco-token nil
  "Discord token used for authenticated API requests.

Use `disco-set-token' to set this in the current session.
When unset, disco falls back to environment variable `DISCO_TOKEN'."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'disco)

(defcustom disco-token-env-var "DISCO_TOKEN"
  "Environment variable used as token fallback when `disco-token' is unset."
  :type 'string
  :group 'disco)

(defcustom disco-api-base-url "https://discord.com/api/v10"
  "Base URL for Discord REST API."
  :type 'string
  :group 'disco)

(defcustom disco-http-timeout 30
  "Timeout in seconds for synchronous HTTP requests."
  :type 'integer
  :group 'disco)

(defcustom disco-message-fetch-limit 50
  "Default amount of messages fetched for room timeline."
  :type 'integer
  :group 'disco)

(defcustom disco-enable-live-updates t
  "If non-nil, enable periodic room updates while room buffers are open."
  :type 'boolean
  :group 'disco)

(defcustom disco-gateway-version 10
  "Discord Gateway API version used in websocket URL query."
  :type 'integer
  :group 'disco)

(defcustom disco-gateway-encoding "json"
  "Discord Gateway payload encoding used in websocket URL query."
  :type 'string
  :group 'disco)

(defcustom disco-gateway-transport-compression 'zlib-stream
  "Optional transport compression mode for Discord Gateway.

Supported values:
- nil: no transport compression
- zlib-stream: compressed binary frames with shared zlib context"
  :type '(choice (const :tag "Disabled" nil)
          (const :tag "zlib-stream" zlib-stream))
  :group 'disco)

(defcustom disco-gateway-zlib-max-buffer-bytes (* 64 1024 1024)
  "Maximum buffered compressed bytes for zlib-stream context.

When this threshold is reached, disco will reconnect to reset stream state
and avoid unbounded memory growth."
  :type 'integer
  :group 'disco)

(defcustom disco-fetch-guild-active-threads nil
  "If non-nil, query active threads endpoint during root refresh.

Discord docs mark this endpoint as bot-only; user accounts will receive
HTTP 403. Failures are logged and ignored, so enabling this is safe but
primarily useful for bot-token workflows."
  :type 'boolean
  :group 'disco)

(defcustom disco-thread-archive-fetch-limit 50
  "Default limit used when fetching archived thread lists.

Official Discord archived thread endpoints accept 2-100."
  :type 'integer
  :group 'disco)

(defcustom disco-gateway-reconnect-delay 3
  "Seconds to wait before reconnecting gateway after disconnect/error."
  :type 'integer
  :group 'disco)

(defcustom disco-gateway-max-reconnect-attempts 10
  "Maximum number of consecutive reconnect attempts before gateway stops.

Set to nil to allow unlimited reconnect attempts."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'disco)

(defcustom disco-gateway-reconnect-max-delay 60
  "Maximum delay in seconds used by reconnect backoff."
  :type 'integer
  :group 'disco)

(defcustom disco-gateway-reconnect-multiplier 2.0
  "Exponential multiplier applied to reconnect backoff delay."
  :type 'number
  :group 'disco)

(defcustom disco-gateway-reconnect-jitter 0.2
  "Jitter ratio applied to reconnect delay.

For example, 0.2 randomizes delay in the range of +/-20%."
  :type 'number
  :group 'disco)

(defcustom disco-gateway-invalid-session-min-delay 1.0
  "Minimum randomized delay in seconds for Opcode 9 Invalid Session reconnect."
  :type 'number
  :group 'disco)

(defcustom disco-gateway-invalid-session-max-delay 5.0
  "Maximum randomized delay in seconds for Opcode 9 Invalid Session reconnect."
  :type 'number
  :group 'disco)

(defcustom disco-gateway-identify-intents nil
  "Optional intents bitmask sent in Identify payload.

When nil, omit intents from Identify payload."
  :type '(choice (const :tag "Unset" nil) integer)
  :group 'disco)

(defcustom disco-gateway-identify-capabilities nil
  "Additional capabilities bitmask sent in Identify payload.

disco.el always enables the protocol capabilities required by its state model;
this option only adds capabilities to that baseline."
  :type '(choice (const :tag "Unset" nil) integer)
  :group 'disco)

(defcustom disco-gateway-enable-passive-guild-update-v2 t
  "When non-nil, opt into PASSIVE_GUILD_UPDATE_V2 gateway capability.

This keeps unread/activity deltas flowing for guilds without explicit
channel subscriptions, which is important for root/activity views that
primarily rely on passive updates."
  :type 'boolean
  :group 'disco)

(defcustom disco-gateway-identify-presence nil
  "Optional presence object sent in Identify payload.

Provide this as an alist matching Discord Gateway presence schema."
  :type '(choice (const :tag "Unset" nil) sexp)
  :group 'disco)

(defcustom disco-gateway-enable-lazy-channel-subscriptions t
  "When non-nil, send Gateway op 14 subscriptions for watched guild channels.

Discord user sessions generally need these per-channel subscriptions to
receive `TYPING_START` events in guild channels (DM typing does not need it)."
  :type 'boolean
  :group 'disco)

(defcustom disco-gateway-send-max-events-per-window 110
  "Maximum gateway events sent per connection window.

Discord documents a hard limit of 120 events per 60 seconds per connection.
This setting keeps a small safety margin for bursty timers and reconnect edges."
  :type 'integer
  :group 'disco)

(defcustom disco-gateway-send-window-seconds 60
  "Time window in seconds used by gateway send rate limiter."
  :type 'number
  :group 'disco)

(defcustom disco-gateway-send-queue-max-size 600
  "Maximum buffered gateway payload count waiting for rate-limit send slots."
  :type 'integer
  :group 'disco)

(defcustom disco-rate-limit-max-retries 2
  "Maximum retries for 429 responses in one API call."
  :type 'integer
  :group 'disco)

(defcustom disco-rate-limit-safety-margin 0.15
  "Extra seconds added after server-provided reset/retry windows."
  :type 'number
  :group 'disco)

(defcustom disco-user-agent
  (concat
   "Mozilla/5.0 (X11; Linux x86_64) "
   "AppleWebKit/537.36 (KHTML, like Gecko) "
   "discord/0.0.670 Chrome/134.0.6998.179 Electron/35.1.5 Safari/537.36")
  "User-Agent sent to Discord API.

Default uses a desktop-style Discord/Electron shape similar to oxicord,
which aligns with Discord user-account client property expectations."
  :type 'string
  :group 'disco)

(defcustom disco-locale "en-US"
  "Locale used in request headers."
  :type 'string
  :group 'disco)

(defun disco-set-token (token)
  "Set Discord TOKEN for current Emacs session."
  (interactive (list (read-passwd "Discord token: ")))
  (setq disco-token token)
  (message "disco: token set for current session"))

(defun disco-current-token ()
  "Return active Discord token from custom var or environment.

Preference order:
1) `disco-token' when non-empty.
2) environment variable named by `disco-token-env-var'."
  (let* ((custom-token (and (stringp disco-token)
                            (not (string-empty-p disco-token))
                            disco-token))
         (env-token (let ((raw (getenv disco-token-env-var)))
                      (and (stringp raw)
                           (not (string-empty-p raw))
                           raw))))
    (or custom-token env-token)))

(provide 'disco-customize)

;;; disco-customize.el ends here
