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
  "Optional capabilities bitmask sent in Identify payload.

When nil, omit capabilities from Identify payload."
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
