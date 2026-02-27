;;; disco-customize.el --- Customization for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors
;; Keywords: comm

;;; Commentary:

;; User customization options for disco.el.

;;; Code:

(defgroup disco nil
  "Discord client for Emacs."
  :group 'comm)

(defcustom disco-token nil
  "Discord token used for authenticated API requests.

Use `disco-set-token' to set this in the current session."
  :type '(choice (const :tag "Unset" nil) string)
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

This endpoint may not be available for all account types. Failures are
logged and ignored, so enabling this is safe but optional."
  :type 'boolean
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

(defcustom disco-gateway-identify-presence nil
  "Optional presence object sent in Identify payload.

Provide this as an alist matching Discord Gateway presence schema."
  :type '(choice (const :tag "Unset" nil) sexp)
  :group 'disco)

(defcustom disco-rate-limit-max-retries 2
  "Maximum retries for 429 responses in one API call."
  :type 'integer
  :group 'disco)

(defcustom disco-rate-limit-safety-margin 0.15
  "Extra seconds added after server-provided reset/retry windows."
  :type 'number
  :group 'disco)

(defcustom disco-user-agent "Mozilla/5.0 (X11; Linux x86_64) Emacs disco.el"
  "User-Agent sent to Discord API."
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

(provide 'disco-customize)

;;; disco-customize.el ends here
