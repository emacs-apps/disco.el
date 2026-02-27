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

(defcustom disco-live-update-interval 3
  "Polling interval in seconds for live room updates."
  :type 'integer
  :group 'disco)

(defcustom disco-live-update-message-limit 30
  "Per-poll message window size used by live update engine."
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
