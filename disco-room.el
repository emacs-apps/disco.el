;;; disco-room.el --- Channel room buffers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Per-channel room buffer with simple timeline rendering and message sending.

;;; Code:

(require 'subr-x)
(require 'time-date)
(require 'seq)
(require 'transient)
(require 'cl-lib)
(require 'ewoc)
(require 'plz)
(require 'svg nil t)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-media)
(require 'appkit-chat-avatar)
(require 'appkit-chat-history)
(require 'appkit-chat-ins)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-name-color)
(require 'disco-ins)
(require 'appkit-ui)
(require 'time-date)
(require 'disco-msg)
(require 'disco-thread)
(require 'disco-typing)
(require 'disco-markdown)
(require 'disco-media)
(require 'disco-avatar)
(require 'disco-emoji-image)
(require 'disco-sticker)
(require 'disco-embed)
(require 'appkit-view)
(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-permission)
(require 'disco-company)
(require 'disco-room-search)
(require 'disco-room-thread)
(require 'disco-room-poll)
(require 'disco-room-reaction)
(require 'disco-runtime)

(autoload 'disco-user-open "disco-user" nil t)
(declare-function disco-user-open "disco-user" (user-or-id &optional guild-id))

(declare-function disco-api--validate-message-content-length "disco-api-normalize"
                  (content field-name))
(declare-function disco-company--teardown-room-buffer "disco-company" ())
(defvar disco-api--message-content-limit)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--remote-latest-message-id nil
  "Newest canonical Discord message observed for this room.

This protocol frontier stays separate from AppKit's nil newer edge, whose
meaning is only that the projected window is attached to latest.")
(defvar-local disco-room--oldest-message-id nil
  "Oldest canonical message in the currently visible history window.

Kept for `disco-room-search' boundary compatibility; pagination ownership and
exhaustion live exclusively in `appkit-chat-history'.")
(defvar-local disco-room--newest-message-id nil
  "Newest canonical message in the currently visible history window.

This is a search boundary, not the remote/latest protocol frontier.")
(defvar-local disco-room--pending-reply-to nil)
(defvar-local disco-room--pending-edit nil)
(defvar-local disco-room--pending-jump-message-id nil)
(defvar-local disco-room--gateway-handler nil)
(defvar-local disco-room--live-update-handle nil
  "Appkit lifecycle handle owning this room's gateway hook and watch.")
(defvar-local disco-room--send-in-flight nil)
(defvar-local disco-room--sticker-picker-pending nil
  "Non-nil while this room is loading catalogs for an explicit Sticker pick.")

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

(defvar-local disco-room--last-search-query nil)
(defvar-local disco-room--msg-filter nil)
(defvar-local disco-room--filter-generation 0)
(defvar-local disco-room--filter-in-flight nil)
(defvar-local disco-room--inplace-search-filter nil)
(defvar-local disco-room--inplace-search-generation 0)
(defvar-local disco-room--pending-attachments nil)
(defvar-local disco-room--attachment-token-table nil)
(defvar-local disco-room--attachment-token-seq 0)
(defvar-local disco-room--typing-users nil)
(defvar-local disco-room--typing-expire-timer nil)
(defvar-local disco-room--pin-op-seq 0
  "Monotonic owner token for message pin requests in this room view.")
(defvar-local disco-room--pin-ops nil
  "Current message pin operation keyed by message id.")
(defvar-local disco-room--revealed-spoiler-message-id nil)
(defvar-local disco-room--optimistic-read-ack-seq 0)
(defvar-local disco-room--pending-optimistic-read-ack nil)
(defvar-local disco-room--pins-ack-seq 0
  "Monotonic owner token for pinned-message acknowledgements.")

(defvar-local disco-room--preview-buffer-owner-p nil
  "Non-nil when this buffer is a Disco composer-preview projection.")

(put 'disco-room--preview-buffer-owner-p 'permanent-local t)

(defconst disco-room--preview-buffer-name "*disco-room-preview*"
  "Preferred display name for the composer preview buffer.")

(defvar disco-room--preview-buffer nil
  "Live explicitly owned composer preview buffer, including after a rename.")

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

(defvar disco-room--session-cache-reset-in-progress nil
  "Non-nil while account-scoped room media state is being retired.

Fetch, timer, and projection callbacks must remain inert while this barrier is
set.  `disco-reset-session-state' binds it for the complete reset transaction,
including kill hooks which can run between the early and final cache drains.")

(defvar disco-room--avatar-round-image-cache (make-hash-table :test #'equal)
  "Global rounded avatar image cache keyed by file/size/mtime.")

(defvar disco-room--forward-guild-icon-image-cache (make-hash-table :test #'equal)
  "Global forwarded-source guild icon cache keyed by icon cache key.")

(defvar disco-room--forward-guild-icon-fetching (make-hash-table :test #'equal)
  "Forwarded guild icon request owners keyed by icon cache key.

Each owner is a unique plist containing its generation and exact `plz'
process, so a late callback cannot retire or overwrite a replacement request.")

(defvar disco-room--forward-guild-icon-fetch-generation 0
  "Generation used to revoke forwarded guild icon request callbacks.")

(defvar disco-room-draft-history-search-history nil
  "Minibuffer history for room draft-history searches.")

(defcustom disco-room-input-history-size 30
  "Maximum number of draft entries kept in room input history."
  :type 'integer
  :group 'disco)

(defcustom disco-room-history-auto-load-threshold 2000
  "Character distance from a timeline edge that triggers history paging.

Nil disables automatic history paging.  Filtered search views never use this
gate because their result order is not a continuous channel history window."
  :type '(choice (const :tag "Disabled" nil) integer)
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


(defcustom disco-room-poll-default-duration-hours 24
  "Default duration in hours used by `disco-room-send-poll'."
  :type 'integer
  :group 'disco)

(defcustom disco-room-poll-max-options 10
  "Maximum number of options collected by `disco-room-send-poll'."
  :type 'integer
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
  :type '(choice (const :tag "Use current fill-column" nil)
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

(defvar-keymap disco-room-timeline-mode-map
  :doc "Timeline-only keymap active when point is outside the room draft."
  "q" #'quit-window
  "c" #'disco-msg-copy-dwim
  "l" #'disco-msg-copy-link
  "n" #'disco-msg-next
  "p" #'disco-msg-previous
  "o" #'disco-msg-operate
  "t" #'disco-msg-copy-text
  "r" #'disco-msg-reply
  "f" #'disco-msg-forward
  "e" #'disco-msg-edit
  "d" #'disco-msg-delete
  "P" #'disco-msg-toggle-pin
  "i" #'disco-msg-describe-message
  "L" #'disco-msg-redisplay
  "!" #'disco-msg-add-reaction
  "+" #'disco-msg-toggle-reaction
  "-" #'disco-msg-remove-reaction
  "T" #'disco-msg-open-thread
  "?" #'disco-room-transient)

(defvar-keymap disco-room-message-prefix-map
  :doc "Prefix map for message actions at point in `disco-room-mode'."
  "c" #'disco-msg-copy-dwim
  "l" #'disco-msg-copy-link
  "n" #'disco-msg-next
  "p" #'disco-msg-previous
  "o" #'disco-msg-operate
  "t" #'disco-msg-copy-text
  "r" #'disco-msg-reply
  "f" #'disco-msg-forward
  "e" #'disco-msg-edit
  "d" #'disco-msg-delete
  "P" #'disco-msg-toggle-pin
  "i" #'disco-msg-describe-message
  "L" #'disco-msg-redisplay
  "!" #'disco-msg-add-reaction
  "+" #'disco-msg-toggle-reaction
  "-" #'disco-msg-remove-reaction
  "T" #'disco-msg-open-thread)

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

(defun disco-room--required-send-permissions (&optional channel)
  "Return permission list required to send message in CHANNEL.

When CHANNEL is nil, use current room channel."
  (if (disco-thread-channel-p (or channel (disco-room--channel-object)))
      '(send-messages-in-threads)
    '(send-messages)))

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
  (when (eq (plist-get (appkit-chatbuf-aux-state) :aux-type) 'reply)
    (plist-get (appkit-chatbuf-aux-state) :message-id)))

(defun disco-room--composer-edit-active-p ()
  "Return non-nil when room composer is editing an existing message."
  (eq (plist-get (appkit-chatbuf-aux-state) :aux-type) 'edit))

(defun disco-room--composer-edit-message-id ()
  "Return target message id for active composer edit, or nil."
  (and (disco-room--composer-edit-active-p)
       (plist-get (appkit-chatbuf-aux-state) :message-id)))

(defun disco-room--composer-aux-active-p ()
  "Return non-nil when room composer currently has reply/edit context."
  (not (null (appkit-chatbuf-aux-state))))

(defun disco-room--composer-aux-context-name ()
  "Return human-readable name for active composer aux context, or nil."
  (pcase (plist-get (appkit-chatbuf-aux-state) :aux-type)
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
  (or (disco-room--room-send-restriction-reason
       nil (disco-room--channel-object))
      (cond
       ((not (consp msg))
        "message ownership is unavailable")
       ((not (disco-room--message-owned-by-current-user-p msg nil))
        "only your own messages can be edited"))))

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


(defun disco-room--sticker-unavailable-reason ()
  "Return reason sending a Sticker is unavailable, or nil."
  (disco-room--room-send-restriction-reason))

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

(defun disco-room--pin-message-unavailable-reason (&optional msg)
  "Return reason toggling a message pin is unavailable for MSG, or nil."
  (let ((msg (or msg (ignore-errors (disco-room--message-at-point)))))
    (cond
     ((not (listp msg))
      "point is not on a message")
     ((not (alist-get 'id msg))
      "message has no id")
     ((not disco-room--channel-id)
      "current room has no channel")
     (t
      (disco-room--channel-permission-reason '(pin-messages))))))

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
  (list :draft (appkit-chatbuf-copy-string (disco-room--current-draft))
        :reply-to (disco-room--composer-reply-message-id)
        :attachment-token-seq disco-room--attachment-token-seq
        :attachment-token-entries (disco-room--copy-attachment-token-table)))

(defun disco-room--composer-edit-restore-state (state &optional defer-live-update-p)
  "Restore composer STATE captured by `disco-room--composer-edit-saved-state'.

When DEFER-LIVE-UPDATE-P is non-nil, update controller state only.  The owning
Appkit view will project the restored composer during its next sync."
  (let ((draft (appkit-chatbuf-copy-string (plist-get state :draft))))
    (disco-room--set-composer-aux-state nil (plist-get state :reply-to))
    (setq disco-room--attachment-token-seq
          (or (plist-get state :attachment-token-seq) 0))
    (disco-room--restore-attachment-token-table
     (plist-get state :attachment-token-entries))
    (disco-room--apply-draft-state
     draft
     :reset-history-p t
     :defer-live-update-p defer-live-update-p)))

(defun disco-room--composer-edit-clear (&optional restore-state defer-live-update-p)
  "Clear active composer edit.

When RESTORE-STATE is non-nil, also restore the saved draft/reply/attachment
state that was present before edit mode was entered.  DEFER-LIVE-UPDATE-P is
forwarded to the controller-only restore path used by asynchronous callbacks."
  (when (disco-room--composer-edit-active-p)
    (let ((saved-state (plist-get disco-room--pending-edit :saved-state)))
      (disco-room--set-composer-aux-state nil nil)
      (when restore-state
        (disco-room--composer-edit-restore-state
         saved-state defer-live-update-p)))
    t))

(defun disco-room--composer-operation-slot ()
  "Capture the complete client-owned composer slot for one mutation."
  (list :draft (appkit-chatbuf-copy-string (disco-room--current-draft))
        :pending-edit (copy-tree disco-room--pending-edit)
        :pending-reply-to disco-room--pending-reply-to
        :message-revision
        (disco-state-message-revision disco-room--channel-id)
        :attachment-token-seq disco-room--attachment-token-seq
        :attachment-token-entries (disco-room--copy-attachment-token-table)))

(defun disco-room--clear-composer-operation-slot ()
  "Clear input and aux as one mutation boundary and return its revision."
  (disco-room--clear-draft)
  (disco-room--set-composer-aux-state nil nil)
  (appkit-chatbuf-composer-revision))

(defun disco-room--restore-composer-operation-slot
    (revision slot &optional defer-live-update-p)
  "Restore SLOT only while Appkit composer REVISION remains pristine.

When DEFER-LIVE-UPDATE-P is non-nil, update controller state only.  Return
non-nil exactly when restoration wins.  Reply/edit targets deleted after SLOT
was captured are not restored, but their draft and attachments remain
recoverable."
  (when (= revision (appkit-chatbuf-composer-revision))
    (let* ((pending-edit (copy-tree (plist-get slot :pending-edit)))
           (pending-reply-to (plist-get slot :pending-reply-to))
           (message-revision (plist-get slot :message-revision)))
      (cl-labels
          ((deleted-after-capture-p
             (message-id)
             (and message-id
                  (disco-state-message-deleted-after-p
                   disco-room--channel-id message-id message-revision))))
        (when (deleted-after-capture-p
               (plist-get pending-edit :message-id))
          (setq pending-edit nil))
        (when (deleted-after-capture-p pending-reply-to)
          (setq pending-reply-to nil))
        (when-let* ((saved-state (plist-get pending-edit :saved-state))
                    (saved-reply-to (plist-get saved-state :reply-to)))
          (when (deleted-after-capture-p saved-reply-to)
            (setf (plist-get saved-state :reply-to) nil))))
      (setq disco-room--attachment-token-seq
            (or (plist-get slot :attachment-token-seq) 0))
      (disco-room--restore-attachment-token-table
       (plist-get slot :attachment-token-entries))
      (disco-room--set-composer-aux-state pending-edit pending-reply-to)
      (disco-room--apply-draft-state
       (appkit-chatbuf-copy-string (plist-get slot :draft))
       :reset-history-p t
       :defer-live-update-p defer-live-update-p)
      t)))

(defun disco-room--composer-context-message (message-id)
  "Return cached local message object for MESSAGE-ID, or nil."
  (and message-id (disco-room--message-by-id message-id)))

(defun disco-room--composer-context-text (aux-state)
  "Return the unified composer context card for AUX-STATE."
  (when aux-state
    (let* ((aux-type (plist-get aux-state :aux-type))
           (message-id (plist-get aux-state :message-id))
           (msg (or (disco-room--composer-context-message message-id)
                    (plist-get aux-state :aux-msg)))
           (author (and msg (disco-room--message-author msg)))
           (author-face (and msg (disco-room--author-face msg)))
           (subject
            (if (and (stringp author) (not (string-empty-p author)))
                (propertize author 'face author-face)
              "message"))
           (title
            (pcase aux-type
              ('edit "Editing message")
              ('reply (concat "Reply to " subject))
              (_ "")))
           (preview (string-trim
                     (or (and msg (disco-msg-preview-content msg)) ""))))
      (appkit-chatbuf-aux-render
       :title title
       :preview
       (disco-media-message-one-line-preview
        msg
        (if (string-empty-p preview)
            "Message preview unavailable"
          preview)
        :attachments
        (and msg (disco-room--message-effective-attachments msg)))
       :cancel-action #'disco-room-cancel-reply
       :cancel-help
       (format "Cancel %s (C-c C-k)"
               (if (eq aux-type 'edit) "edit" "reply"))
       :accent-face author-face
       :width (disco-room--line-fill-column)))))

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
    (appkit-chatbuf-focus-input)
    (message "disco: editing message %s in composer" message-id)))


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

(defun disco-room--typing-expire-timer-callback (buffer view)
  "Expire stale typing entries for room BUFFER owned by VIEW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; A room buffer can survive its Appkit view.  Never let the predecessor's
      ;; timer clear or redraw state belonging to a replacement view.
      (when (and (eq view (appkit-current-view))
                 (appkit-view-live-p view))
        (setq disco-room--typing-expire-timer nil)
        (when (disco-room--typing-prune-expired)
          (appkit-request-sync view :part 'frame))
        (disco-room--typing-reschedule-expire-timer)))))

(defun disco-room--typing-reschedule-expire-timer ()
  "Reschedule room-local timer for the next typing expiry."
  (disco-room--typing-cancel-expire-timer)
  (let ((next-expiry (disco-room--typing-next-expiry)))
    (when next-expiry
      (let ((delay (max 0.1 (- next-expiry (float-time))))
            (room-buffer (current-buffer))
            (view (appkit-current-view)))
        (when (appkit-view-live-p view)
          (setq disco-room--typing-expire-timer
                (run-at-time delay nil
                             #'disco-room--typing-expire-timer-callback
                             room-buffer view)))))))

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
              changed)))))))

(defun disco-room--typing-stop-user (user-id &optional _no-rerender)
  "Remove USER-ID from typing indicators and return non-nil when changed.

This helper is controller-only.  Its caller owns any Appkit invalidation."
  (let ((normalized-id (disco-room--typing-normalize-user-id user-id)))
    (when (and normalized-id
               (hash-table-p disco-room--typing-users)
               (gethash normalized-id disco-room--typing-users))
      (remhash normalized-id disco-room--typing-users)
      (disco-room--typing-reschedule-expire-timer)
      t)))

(defun disco-room--latest-message-id ()
  "Return newest message ID visible in the current room window, or nil."
  (disco-room--message-id
   (seq-find #'disco-room--canonical-message-p
             (disco-room--display-messages))))

(defun disco-room--plz-error-object (err)
  "Return the plz error object carried by ERR, or nil."
  (cond
   ((and (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p err)))
    err)
   ((and (consp err)
         (symbolp (car err))
         (fboundp 'plz-error-p)
         (ignore-errors (plz-error-p (cadr err))))
    (cadr err))
   (t nil)))

(defun disco-room--plz-error-http-status (err)
  "Return HTTP status from ERR, or nil if unavailable."
  (or (and (listp err) (plist-get err :status))
      (when-let* ((plz-error (disco-room--plz-error-object err)))
        (when (and (fboundp 'plz-error-response)
                   (fboundp 'plz-response-status))
          (let ((response (ignore-errors (plz-error-response plz-error))))
            (and response
                 (ignore-errors (plz-response-status response))))))))

(defun disco-room--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (when-let* ((plz-error (disco-room--plz-error-object err)))
        (let* ((msg (and (fboundp 'plz-error-message)
                         (ignore-errors (plz-error-message plz-error))))
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

(defun disco-room--callback-active-p (room-buffer channel-id view)
  "Return non-nil when ROOM-BUFFER is still bound to CHANNEL-ID and VIEW.

History callbacks additionally use AppKit request-owner identity.  Other
operations keep their own protocol-specific sequence/token validation."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (eq view (appkit-current-view))
              (appkit-view-live-p view)))))

(defun disco-room--channel-buffer-p (room-buffer channel-id view)
  "Return non-nil when ROOM-BUFFER is still bound to CHANNEL-ID and VIEW.

VIEW is the exact Appkit view captured when asynchronous work began.  A nil or
dead view never degrades this guard to channel identity alone."
  (and (buffer-live-p room-buffer)
       (appkit-view-live-p view)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (eq view (appkit-current-view))))))

(defun disco-room--current-draft ()
  "Return current room draft string, preserving text properties."
  (appkit-chatbuf-input-state))

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
  (let* ((path (plist-get attachment :path))
         (filename
          (or (plist-get attachment :filename)
              (and path (file-name-nondirectory path))
              "unnamed"))
         (content-type (plist-get attachment :content-type))
         (image-p
          (or (and (stringp content-type)
                   (string-prefix-p "image/" content-type))
              (appkit-media-image-file-name-p filename)))
         (image
          (and image-p
               (appkit-media-file-present-p path)
               (appkit-media-one-line-preview-image-from-file path)))
         (preview
          (and image (appkit-media-image-display-string image "▧")))
         (size
          (and (appkit-media-file-present-p path)
               (file-size-human-readable
                (file-attribute-size (file-attributes path))))))
    (concat (if image-p "[image] " "[file] ")
            (if preview (concat preview " ") "")
            (propertize filename 'help-echo path)
            (if size (format " (%s)" size) ""))))

(defun disco-room--attachment-input-object-string (attachment)
  "Return one propertized draft string representing ATTACHMENT."
  (let* ((object (copy-tree attachment))
         (text (disco-room--attachment-input-object-display-text object)))
    (appkit-chatbuf-input-object-string text object)))

(defun disco-room--insert-attachment-input-object (attachment)
  "Insert ATTACHMENT as one structured composer object at point."
  (let ((object (copy-tree attachment)))
    ;; Do not insert the leading separator into an existing atomic object when
    ;; point was placed unexpectedly inside its intangible display span.
    (when-let* ((bounds (appkit-chatbuf-input-object-bounds-at-point)))
      (unless (= (point) (car bounds))
        (goto-char (cdr bounds))))
    (when (and (appkit-chatbuf-point-in-input-p)
               (> (point) (or (appkit-chatbuf-input-start-position) (point-min)))
               (let ((before (char-before)))
                 (and before (not (memq before '(?\s ?\t ?\n))))))
      (appkit-chatbuf-input-insert " "))
    (appkit-chatbuf-input-insert
     (disco-room--attachment-input-object-display-text object)
     :object object)
    (when (let ((after (char-after)))
            (and after (not (memq after '(?\s ?\t ?\n)))))
      (appkit-chatbuf-input-insert " "))
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
      (let ((object (get-text-property pos appkit-chatbuf-input-object-property text)))
        (if (disco-room--attachment-input-object-p object)
            (let* ((end (appkit-chatbuf-next-input-object-change
                         pos text len))
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
          (let* ((next-object
                  (appkit-chatbuf-next-input-object-change pos text len))
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
      (let ((object (get-text-property pos appkit-chatbuf-input-object-property text)))
        (if object
            (let ((end (appkit-chatbuf-next-input-object-change
                        pos text len)))
              (push (copy-tree object) objects)
              (setq pos end))
          (setq pos (appkit-chatbuf-next-input-object-change
                     pos text len)))))
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
      (let ((object (get-text-property pos appkit-chatbuf-input-object-property text)))
        (if object
            (let* ((end (appkit-chatbuf-next-input-object-change
                         pos text len))
                   (object-copy (copy-tree object)))
              (push object-copy objects)
              (when-let* ((attachment
                           (disco-room--attachment-input-object-to-attachment object-copy)))
                (push attachment attachments))
              (setq pos end))
          (let* ((end (appkit-chatbuf-next-input-object-change
                       pos text len))
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
  (appkit-chatbuf-input-apply-text-properties)
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (with-silent-modifications
      (add-text-properties
       (car bounds) (cdr bounds)
       '(disco-room-input t)))))

(defun disco-room--maybe-auto-load-older ()
  "Load older channel history when point approaches the timeline top."
  (when (and disco-room--channel-id
             (not (disco-room--msg-filter-active-p))
             (not (appkit-chatbuf-point-in-input-p))
             (appkit-chat-history-autoload-older-p
              (point) (point-min) disco-room-history-auto-load-threshold))
    (disco-room-load-older-messages t)))

(defun disco-room--maybe-auto-load-newer (&optional position)
  "Load newer history when POSITION approaches a partial window's footer."
  (let ((position (or position (point)))
        (footer (or (appkit-chat-timeline-footer-start-position)
                    (appkit-chatbuf-input-start-position)
                    (point-max))))
    (when (and disco-room--channel-id
               (not (disco-room--msg-filter-active-p))
               (appkit-chat-history-autoload-newer-p
                position footer disco-room-history-auto-load-threshold
                (appkit-chatbuf-composer-idle-p)))
      (disco-room-load-newer-messages t))))

(defun disco-room--window-scroll (window _display-start)
  "Auto-load newer history from WINDOW's actual visible timeline edge.

`post-command-hook' covers keyboard motion through point.  Window scrolling
can move the viewport without moving point, so use AppKit's composer-clamped
visible end for both selected and inactive room windows."
  (when (and (window-live-p window)
             (buffer-live-p (window-buffer window)))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'disco-room-mode)
        (when-let* ((position
                     (appkit-chat-timeline-window-visible-end-position
                      window)))
          (disco-room--maybe-auto-load-newer position))))))

(defun disco-room--post-command ()
  "Maintain Disco-specific row and history behavior after each command."
  (unless (appkit-chatbuf-rendering-p)
    (let ((current-message-id (or (get-text-property (point) 'disco-message-id)
                                  (get-text-property (line-beginning-position)
                                                     'disco-message-id))))
      (when (and disco-room--revealed-spoiler-message-id
                 (not (equal current-message-id
                             disco-room--revealed-spoiler-message-id)))
        (let ((previous disco-room--revealed-spoiler-message-id))
          (setq disco-room--revealed-spoiler-message-id nil)
          (when-let* ((view (appkit-current-view)))
            (when (appkit-view-live-p view)
              (appkit-request-sync view :entry previous))))))
    (disco-room--maybe-auto-load-newer)
    (disco-room--maybe-auto-load-older)))

(defun disco-room--sync-draft-from-buffer ()
  "Sync shared chatbuf draft cache from editable input region."
  (let ((text (plist-get (appkit-chatbuf-input-state-sync)
                         :value)))
    (disco-room--prune-unused-attachment-tokens text)
    (disco-room--sync-pending-attachments-from-draft text)))

(cl-defun disco-room--apply-draft-state
    (text &key reset-history-p defer-live-update-p)
  "Apply draft TEXT to cache/live input and return update metadata.

When a visible tail input exists and attachment-derived footer state is
unchanged, update the live input directly in telega-like fashion.  Otherwise,
callers can use the returned metadata to decide whether a frame refresh is
needed.  When DEFER-LIVE-UPDATE-P is non-nil, only controller state changes;
an Appkit sync must project the resulting composer."
  (let ((old-attachments (copy-tree disco-room--pending-attachments))
        (live-input-p (and (not defer-live-update-p)
                           (not (appkit-chatbuf-rendering-p))
                           (appkit-chatbuf-input-start-position))))
    (let ((draft
           (appkit-chatbuf-input-state-set
            text
            :reset-history-p reset-history-p)))
      (disco-room--prune-unused-attachment-tokens draft)
      (disco-room--sync-pending-attachments-from-draft draft)
      (let ((attachments-changed-p
             (not (equal old-attachments disco-room--pending-attachments))))
        (when (and live-input-p (not attachments-changed-p))
          (appkit-chatbuf-with-generated-update
            (appkit-chatbuf-input-replace draft)
            (disco-room--apply-input-text-properties)))
        (list :draft (appkit-chatbuf-copy-string draft)
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
  (let ((result (appkit-chatbuf-input-history-prev-value
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
  (let ((result (appkit-chatbuf-input-history-next-value n)))
    (pcase (plist-get result :status)
      ('ok
       (disco-room--set-draft (plist-get result :value)))
      (_
       (message "disco: already at latest draft")))))

(defun disco-room-edit-draft ()
  "Edit current room draft in minibuffer and re-render room."
  (interactive)
  (when (appkit-chatbuf-string-has-objects-p (disco-room--current-draft))
    (user-error "disco: minibuffer draft editing is unavailable for structured input objects"))
  (let ((updated (read-from-minibuffer
                  "Draft: "
                  (appkit-chatbuf-string-plain-text (disco-room--current-draft)))))
    (appkit-chatbuf-input-history-reset)
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
      t)))

(defun disco-room--mark-read (&optional message-id defer-sync-p)
  "Mark current room as read and acknowledge MESSAGE-ID.

When MESSAGE-ID is nil, acknowledge the newest visible message in the room.
Unread counters are always cleared locally.  When DEFER-SYNC-P is non-nil, the
caller is already inside, or will request, an Appkit projection transaction."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-view))
         (channel (disco-room--channel-object))
         (target-id (or message-id
                        (disco-room--latest-message-id)
                        (and (not (appkit-chat-history-window-known-p))
                             channel
                             (alist-get 'last_message_id channel))))
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
             (when (disco-room--callback-active-p room-buffer channel-id view)
               (with-current-buffer room-buffer
                 ;; A later optimistic ACK supersedes this captured response.
                 ;; Gate both the message frontier and token update on the
                 ;; operation sequence so older successes cannot regress them.
                 (when (disco-room--optimistic-read-ack-clear optimistic-seq)
                   (disco-state-apply-message-ack channel-id target-id 0)
                   (disco-state-apply-channel-ack-response channel-id response)
                   (appkit-request-sync view :part 'timeline)))))
           :on-error
           (lambda (err)
             (when (disco-room--callback-active-p room-buffer channel-id view)
               (with-current-buffer room-buffer
                 (when (disco-room--optimistic-read-ack-rollback optimistic-seq)
                   (appkit-request-sync view :part 'timeline))
                 (message "disco: read-state ack failed for %s: %s"
                          channel-id
                          (disco-room--async-error-message err)))))))
      (disco-state-apply-message-ack channel-id nil 0))
    (unless defer-sync-p
      (appkit-request-sync view :part 'timeline)
      ;; This state transition is also used as an explicit local action in
      ;; tests/commands.  Consume its invalidation through Appkit, never by
      ;; calling the timeline projector directly.
      (appkit-sync-invalidations view))))

(defun disco-room-ack-channel-pins ()
  "Acknowledge currently pinned messages in the active room channel."
  (interactive)
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-view))
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
      (let ((ack-seq (cl-incf disco-room--pins-ack-seq)))
        (disco-api-ack-channel-pins-async
         channel-id
         :on-success
         (lambda (_response)
           (when (disco-room--callback-active-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               ;; A later local request owns the cursor.  A Gateway ACK may
               ;; also have advanced it while this request was in flight.  The
               ;; state merge owns version/timezone-aware monotonicity.
               (when (= ack-seq disco-room--pins-ack-seq)
                 (disco-state-apply-channel-pins-ack
                  channel-id last-pin-timestamp)
                 (appkit-request-sync view :part 'frame)
                 (message "disco: acknowledged pins for %s" channel-id)))))
         :on-error
         (lambda (err)
           (when (disco-room--callback-active-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (when (= ack-seq disco-room--pins-ack-seq)
                 (message "disco: pins ack failed for %s: %s"
                          channel-id
                          (disco-room--async-error-message err))))))))))))

(defvar-local disco-room-pinned-messages--channel-id nil
  "Channel whose pinned messages this buffer presents.")

(defvar-local disco-room-pinned-messages--channel-name nil
  "Best-known display name for `disco-room-pinned-messages--channel-id'.")

(defvar-local disco-room-pinned-messages--items nil
  "Accumulated Message Pin records in endpoint page order.")

(defvar-local disco-room-pinned-messages--next-before nil
  "Timestamp cursor for the next pinned-message page.")

(defvar-local disco-room-pinned-messages--has-more-p nil
  "Non-nil when the pinned-message endpoint reports another page.")

(defvar-local disco-room-pinned-messages--loading-p nil
  "Non-nil while this pinned-message view owns an active request.")

(defvar-local disco-room-pinned-messages--error nil
  "Most recent pinned-message request error.")

(defvar-local disco-room-pinned-messages--generation 0
  "Monotonic owner token for pinned-message page requests.")

(defun disco-room-pinned-messages--view-id (channel-id)
  "Return the stable Appkit view identity for CHANNEL-ID."
  (list 'room 'pinned-messages channel-id))

(defun disco-room-pinned-messages--buffer-name (channel-id channel-name)
  "Return a readable pinned-message buffer name for CHANNEL-ID."
  (format "*disco:pins:%s (%s)*" (or channel-name channel-id) channel-id))

(defun disco-room-pinned-messages--callback-active-p
    (buffer channel-id view generation)
  "Return non-nil when BUFFER still owns pinned request GENERATION."
  (and (buffer-live-p buffer)
       (appkit-view-live-p view)
       (with-current-buffer buffer
         (and (eq major-mode 'disco-room-pinned-messages-mode)
              (equal disco-room-pinned-messages--channel-id channel-id)
              (eq view (appkit-current-view))
              (eql generation disco-room-pinned-messages--generation)))))

(defun disco-room-pinned-messages--entry-message-id (entry)
  "Return the nested canonical message ID from Message Pin ENTRY."
  (disco-msg-normalize-id
   (and (listp entry)
        (listp (alist-get 'message entry))
        (alist-get 'id (alist-get 'message entry)))))

(defun disco-room-pinned-messages--merge-page (existing page)
  "Append unique usable Message Pin PAGE records to EXISTING.

Endpoint order is preserved; malformed records without a nested message ID are
not rendered or retained."
  (let ((seen (make-hash-table :test #'equal))
        additions)
    (dolist (entry existing)
      (when-let* ((message-id (disco-room-pinned-messages--entry-message-id entry)))
        (puthash message-id t seen)))
    (dolist (entry page)
      (when-let* ((message-id (disco-room-pinned-messages--entry-message-id entry))
                  ((not (gethash message-id seen))))
        (puthash message-id t seen)
        (push entry additions)))
    (append existing (nreverse additions))))

(defun disco-room-pinned-messages--page-from-response (response)
  "Normalize one pinned-message endpoint RESPONSE.

Return a plist containing `:items', `:has-more', and `:next-before'.  Discord
uses the final Message Pin's `pinned_at' timestamp as the continuation cursor."
  (let* ((items (alist-get 'items response))
         (has-more (eq (alist-get 'has_more response) t))
         (last-item (car (last items)))
         (next-before (and (listp last-item)
                           (alist-get 'pinned_at last-item))))
    (unless (listp items)
      (error "disco: malformed pinned-message response"))
    (when (and has-more
               (not (and (stringp next-before)
                         (not (string-empty-p next-before)))))
      (error "disco: pinned-message response has no continuation cursor"))
    (list :items items
          :has-more has-more
          :next-before next-before)))

(defun disco-room-pinned-messages--request-sync (view)
  "Schedule a structural render of pinned-message VIEW."
  (appkit-request-sync view :structure t))

(defun disco-room-pinned-messages--complete-error (view error)
  "Set current pinned-message VIEW request failure to ERROR."
  (setq-local disco-room-pinned-messages--loading-p nil
              disco-room-pinned-messages--error
              (disco-room--async-error-message error))
  (disco-room-pinned-messages--request-sync view))

(defun disco-room-pinned-messages--request-page (view reset)
  "Asynchronously request one pinned-message page for VIEW.

When RESET is non-nil, the returned page replaces the cached projection."
  (let ((channel-id disco-room-pinned-messages--channel-id))
    (unless (and (stringp channel-id) (not (string-empty-p channel-id)))
      (user-error "disco: pinned-message view has no channel"))
    (when (and (not reset) disco-room-pinned-messages--loading-p)
      (user-error "disco: pinned messages are already loading"))
    (when (and (not reset) (not disco-room-pinned-messages--has-more-p))
      (user-error "disco: no more pinned messages"))
    (let ((before (unless reset disco-room-pinned-messages--next-before)))
      (when (and (not reset)
                 (not (and (stringp before) (not (string-empty-p before)))))
        (setq-local disco-room-pinned-messages--has-more-p nil
                    disco-room-pinned-messages--error
                    "Pinned-message response has no continuation cursor")
        (disco-room-pinned-messages--request-sync view)
        (user-error "disco: pinned messages cannot continue without a cursor"))
      (let ((buffer (current-buffer))
            (generation (cl-incf disco-room-pinned-messages--generation)))
        (setq-local disco-room-pinned-messages--loading-p t
                    disco-room-pinned-messages--error nil)
        (disco-room-pinned-messages--request-sync view)
        (disco-api-channel-pins-async
         channel-id
         :before before
         :limit 50
         :on-success
         (lambda (response)
           (when (disco-room-pinned-messages--callback-active-p
                  buffer channel-id view generation)
             (with-current-buffer buffer
               (condition-case err
                   (let ((page (disco-room-pinned-messages--page-from-response
                                response)))
                     (setq-local
                      disco-room-pinned-messages--items
                      (if reset
                          (disco-room-pinned-messages--merge-page
                           nil (plist-get page :items))
                        (disco-room-pinned-messages--merge-page
                         disco-room-pinned-messages--items
                         (plist-get page :items)))
                      disco-room-pinned-messages--next-before
                      (plist-get page :next-before)
                      disco-room-pinned-messages--has-more-p
                      (plist-get page :has-more)
                      disco-room-pinned-messages--loading-p nil
                      disco-room-pinned-messages--error nil)
                     (disco-room-pinned-messages--request-sync view))
                 (error
                  (disco-room-pinned-messages--complete-error view err))))))
         :on-error
         (lambda (error)
           (when (disco-room-pinned-messages--callback-active-p
                  buffer channel-id view generation)
             (with-current-buffer buffer
               (disco-room-pinned-messages--complete-error view error)))))))))


(defun disco-room-pinned-messages-refresh ()
  "Refresh the pinned-message projection in the current browser buffer."
  (interactive)
  (disco-room-pinned-messages--request-page (appkit-current-view) t))

(defun disco-room-pinned-messages-load-more ()
  "Load the next pinned-message page in the current browser buffer."
  (interactive)
  (disco-room-pinned-messages--request-page (appkit-current-view) nil))

(defun disco-room-pinned-messages--open-entry (entry)
  "Open the nested message from Message Pin ENTRY."
  (let ((message-id (disco-room-pinned-messages--entry-message-id entry))
        (channel-id disco-room-pinned-messages--channel-id))
    (unless message-id
      (user-error "disco: pinned-message entry has no message ID"))
    (disco-room-jump-to-message message-id channel-id)))

(defun disco-room-pinned-messages--insert-entry (entry)
  "Insert one clickable Message Pin ENTRY."
  (let* ((message (alist-get 'message entry))
         (message-id (disco-room-pinned-messages--entry-message-id entry))
         (pinned-at (alist-get 'pinned_at entry))
         (label
          (format "%s  %s"
                  (if (and (stringp pinned-at) (not (string-empty-p pinned-at)))
                      (disco-room--format-time pinned-at)
                    "unknown-time")
                  (disco-msg-preview-line message)))
         (start (point)))
    (appkit-view-insert-label-line
     label
     :line-properties (list 'disco-message-id message-id))
    (appkit-ui-add-action
     start (1- (point))
     (lambda () (disco-room-pinned-messages--open-entry entry))
     :help-echo "Open pinned message")))

(defun disco-room-pinned-messages--list-spec ()
  "Return the current pinned-message browser list specification."
  (let* ((items (or disco-room-pinned-messages--items '()))
         (loading-note
          (cond
           (disco-room-pinned-messages--loading-p
            (if items "(refreshing pinned messages...)" "(loading pinned messages...)"))
           ((not disco-room-pinned-messages--has-more-p) "(no more pinned messages)")
           (t nil))))
    (appkit-view-list-spec-create
     :title (format "Pinned Messages: %s"
                    (or disco-room-pinned-messages--channel-name
                        disco-room-pinned-messages--channel-id))
     :summary (format "Loaded: %d%s"
                      (length items)
                      (if disco-room-pinned-messages--has-more-p
                          "  ·  more available"
                        ""))
     :loading-note loading-note
     :items items
     :item-inserter #'disco-room-pinned-messages--insert-entry
     :empty-text "(no visible pinned messages)"
     :footer-lines
     (when disco-room-pinned-messages--error
       (list (format "Error: %s" disco-room-pinned-messages--error))))))

(defun disco-room-pinned-messages--sync-invalidations (view _invalidations)
  "Synchronize pinned-message VIEW from its local controller state."
  (appkit-with-content-update view
    (appkit-view-render-list-spec-preserving-position
     (disco-room-pinned-messages--list-spec)
     :anchor-property 'disco-message-id
     :preserve-window-start t)))

(defvar-keymap disco-room-pinned-messages-mode-map
  :doc "Keymap for `disco-room-pinned-messages-mode'."
  "g" #'disco-room-pinned-messages-refresh
  "m" #'disco-room-pinned-messages-load-more
  "RET" #'appkit-ui-activate
  "<return>" #'appkit-ui-activate
  "q" #'quit-window)

(define-derived-mode disco-room-pinned-messages-mode special-mode "Disco-Pins"
  "Major mode for one channel's pinned-message browser."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun disco-room-list-pinned-messages (&optional channel-id)
  "Open the pinned-message browser for CHANNEL-ID or the current room."
  (interactive)
  (let* ((channel-id (or channel-id disco-room--channel-id))
         (channel (and channel-id (disco-state-channel channel-id)))
         (channel-name (or (and channel (alist-get 'name channel)) channel-id)))
    (unless (and (stringp channel-id) (not (string-empty-p channel-id)))
      (user-error "disco: room is not bound to a channel"))
    (let* ((app (disco-runtime-app))
           (view-id (disco-room-pinned-messages--view-id channel-id))
           (existing (appkit-view-for-id app view-id))
           (view
            (appkit-open-view
             :app app
             :id view-id
             :mode 'disco-room-pinned-messages-mode
             :buffer-name
             (disco-room-pinned-messages--buffer-name channel-id channel-name)
             :sync-function #'disco-room-pinned-messages--sync-invalidations
             :parts '(content header geometry)
             :setup
             (lambda (_view)
               (setq-local disco-room-pinned-messages--channel-id channel-id
                           disco-room-pinned-messages--channel-name channel-name
                           disco-room-pinned-messages--items nil
                           disco-room-pinned-messages--next-before nil
                           disco-room-pinned-messages--has-more-p nil
                           disco-room-pinned-messages--loading-p nil
                           disco-room-pinned-messages--error nil
                           disco-room-pinned-messages--generation 0))
             :select t))
           (buffer (appkit-view-buffer view)))
      (with-current-buffer buffer
        (setq-local disco-room-pinned-messages--channel-name channel-name)
        (unless existing
          (disco-room-pinned-messages-refresh)))
      buffer)))

(defun disco-room--pending-message-p (message)
  "Return non-nil when MESSAGE is a local optimistic row."
  (and (listp message) (alist-get 'pending message)))

(defun disco-room--message-id (message)
  "Return MESSAGE's normalized stable id, or nil."
  (disco-msg-normalize-id (and (listp message) (alist-get 'id message))))

(defun disco-room--canonical-message-p (message)
  "Return non-nil when MESSAGE has a canonical non-pending Discord id."
  (and (disco-room--message-id message)
       (not (disco-room--pending-message-p message))))

(defun disco-room--normalize-history-page (messages)
  "Return transport MESSAGES explicitly normalized newest-first."
  (disco-room--sort-messages-newest-first
   (seq-filter #'disco-room--canonical-message-p (or messages '()))))

(defun disco-room--history-page-bounds (messages)
  "Return (OLDEST . NEWEST) ids for newest-first MESSAGES."
  (cons (disco-room--message-id (car (last messages)))
        (disco-room--message-id (car messages))))

(defun disco-room--canonical-cache-newest-first (&optional channel-id)
  "Return CHANNEL-ID canonical cache explicitly sorted newest-first."
  (disco-room--normalize-history-page
   (disco-state-messages (or channel-id disco-room--channel-id))))

(defun disco-room--canonical-cache-oldest-first (&optional channel-id)
  "Return CHANNEL-ID canonical cache explicitly sorted oldest-first."
  (reverse (disco-room--canonical-cache-newest-first channel-id)))

(defun disco-room--history-page-retained-in-cache (page cache)
  "Return PAGE entries whose ids remain canonical in CACHE.

PAGE and CACHE use newest-first order.  The returned objects come from CACHE,
so a concurrent Gateway update wins over a stale REST copy while PAGE still
defines which response entries may move exact history edges."
  (let ((canonical-by-id (make-hash-table :test #'equal)))
    (dolist (message (disco-room--normalize-history-page cache))
      (puthash (disco-room--message-id message) message canonical-by-id))
    (delq nil
          (mapcar
           (lambda (message)
             (gethash (disco-room--message-id message) canonical-by-id))
           page))))

(defun disco-room--message-id-after-p (candidate-id reference-id messages)
  "Return non-nil when CANDIDATE-ID follows REFERENCE-ID in MESSAGES.

MESSAGES must be canonical oldest-first order.  Missing ids are unknown and
therefore never count as progress."
  (let ((candidate-index
         (seq-position messages candidate-id
                       (lambda (message id)
                         (equal (disco-room--message-id message) id))))
        (reference-index
         (seq-position messages reference-id
                       (lambda (message id)
                         (equal (disco-room--message-id message) id)))))
    (and candidate-index reference-index
         (> candidate-index reference-index))))

(defun disco-room--newest-canonical-id-among (ids messages)
  "Return newest member of IDS in newest-first canonical MESSAGES."
  (when-let* ((wanted (delq nil (copy-sequence ids)))
              (message
               (seq-find
                (lambda (candidate)
                  (member (disco-room--message-id candidate) wanted))
                messages)))
    (disco-room--message-id message)))

(defun disco-room--observe-live-create (message-id)
  "Observe canonical live MESSAGE-ID without widening a partial window."
  (when-let* ((id (disco-msg-normalize-id message-id)))
    (when (or (null disco-room--remote-latest-message-id)
              (disco-state-snowflake<
               disco-room--remote-latest-message-id id))
      (setq disco-room--remote-latest-message-id id)
      ;; A prior no-progress response at the same partial edge no longer
      ;; proves that another automatic attempt would stall.
      (appkit-chat-history-newer-stalled-clear))
    (when (and (equal id disco-room--remote-latest-message-id)
               (appkit-chat-history-window-empty-p))
      (appkit-chat-history-window-seed-live id))
    id))

(defun disco-room--repair-history-window-after-delete (message-id)
  "Move exact edges inward after canonical MESSAGE-ID was deleted.

The pre-event timeline keys are the only proved members of the old continuous
window; unrelated canonical cache islands are never used as replacements."
  (when (and (appkit-chat-history-window-known-p)
             (not (appkit-chat-history-window-empty-p))
             (appkit-chat-timeline-live-p))
    (let* ((id (disco-msg-normalize-id message-id))
           (first (appkit-chat-history-window-first-key))
           (last (appkit-chat-history-window-last-key))
           (cached-ids
            (mapcar #'disco-room--message-id
                    (disco-state-messages disco-room--channel-id)))
           (remaining
            (seq-filter
             (lambda (key)
               (and (not (equal key id)) (member key cached-ids)))
             (appkit-chat-timeline-keys))))
      (cond
       (remaining
        (appkit-chat-history-window-set
         (if (equal id first) (car remaining) first)
         (if (and last (equal id last)) (car (last remaining)) last))
        (when (equal id disco-room--remote-latest-message-id)
          (setq disco-room--remote-latest-message-id
                (and (null last) (car (last remaining))))))
       ((and (null last) (appkit-chat-history-older-loaded-p))
        (setq disco-room--remote-latest-message-id nil)
        (appkit-chat-history-window-establish-empty))
       (t
        (when (equal id disco-room--remote-latest-message-id)
          (setq disco-room--remote-latest-message-id nil))
        (appkit-chat-history-window-clear))))))

(defun disco-room--establish-latest-history-window
    (page frontier-at-start response-count request-limit)
  "Establish authoritative latest history from PAGE.

FRONTIER-AT-START distinguishes a live create delivered while the request was
in flight.  RESPONSE-COUNT is the raw transport page length; REQUEST-LIMIT
uses that count to prove completion without mistaking Gateway-conflict drops
for a short page.  PAGE contains only response entries retained in canonical
state and is normalized newest-first."
  (let* ((current-frontier disco-room--remote-latest-message-id)
         (live-frontier
          (and current-frontier
               (not (equal current-frontier frontier-at-start))
               current-frontier))
         (canonical (disco-room--canonical-cache-newest-first))
         (bounds (disco-room--history-page-bounds page))
         (oldest (car bounds))
         (page-newest (cdr bounds)))
    (cond
     (page-newest
      (setq disco-room--remote-latest-message-id
            (or (disco-room--newest-canonical-id-among
                 (list page-newest live-frontier) canonical)
                page-newest))
      (appkit-chat-history-older-loaded-set nil)
      (appkit-chat-history-window-set oldest nil)
      (when (< response-count request-limit)
        (appkit-chat-history-older-loaded-set t))
      'established)
     ((>= response-count request-limit)
      ;; Every full-page response entry lost a revision race.  It cannot prove
      ;; an empty channel.  A concurrently observed live frontier can still
      ;; establish a one-entry latest window whose older side remains open.
      (if live-frontier
          (progn
            (setq disco-room--remote-latest-message-id live-frontier)
            (appkit-chat-history-older-loaded-set nil)
            (appkit-chat-history-window-set live-frontier nil)
            'established)
        (appkit-chat-history-window-clear)
        'conflicted))
     (live-frontier
      ;; The REST snapshot was empty, then Gateway/API delivery created the
      ;; first canonical row before its callback completed.
      (setq disco-room--remote-latest-message-id live-frontier)
      (appkit-chat-history-window-establish-empty)
      (appkit-chat-history-window-seed-live live-frontier)
      'established)
     (t
      (setq disco-room--remote-latest-message-id nil)
      (appkit-chat-history-window-establish-empty)
      'empty))))

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
             (appkit-chat-timeline-live-p)
             (appkit-chat-timeline-node message-id))
    (appkit-chat-timeline-invalidate (list message-id))))

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
  "Return current room rows using the existing newest-first contract.

The canonical cache is first converted to oldest-first and strictly sliced by
AppKit.  Only then is the selected window converted back to Disco's historical
newest-first render contract.  Local pending rows join attached latest windows
(including authoritative empty) but never cross a partial around-window edge."
  (if (disco-room--msg-filter-active-p)
      (or (plist-get disco-room--msg-filter :items) '())
    (let* ((cache (or (disco-state-messages disco-room--channel-id) '()))
           (canonical-oldest
            (reverse
             (disco-room--normalize-history-page cache)))
           (slice
            (appkit-chat-history-window-slice
             canonical-oldest #'disco-room--message-id)))
      (if (not (plist-get slice :valid-p))
          nil
        (let* ((selected (plist-get slice :entries))
               (pending
                (and (not (appkit-chat-history-window-partial-p))
                     (seq-filter #'disco-room--pending-message-p cache))))
          (disco-room--sort-messages-newest-first
           (append selected pending)))))))

(defun disco-room--sync-visible-window-cursors (messages)
  "Update search-compatible cursors from newest-first visible MESSAGES."
  (unless (disco-room--msg-filter-active-p)
    (let ((canonical (seq-filter #'disco-room--canonical-message-p messages)))
      (setq disco-room--newest-message-id
            (disco-room--message-id (car canonical))
            disco-room--oldest-message-id
            (disco-room--message-id (car (last canonical)))))))

(defun disco-room--message-position (message-id)
  "Return buffer position for MESSAGE-ID in current room render, or nil."
  (when (and (stringp message-id)
             (not (string-empty-p message-id))
             (appkit-chat-timeline-live-p))
    (appkit-chat-timeline-key-position message-id)))

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
         (view (disco-room--ensure-view))
         (target-id disco-room--pending-jump-message-id)
         (request-revision (disco-state-message-revision channel-id))
         (limit (max 1 (or disco-room-jump-context-limit 50)))
         (owner (list :kind 'around-history
                      :channel-id channel-id
                      :target-id target-id
                      :frontier-at-start disco-room--remote-latest-message-id)))
    (unless (and (stringp target-id) (not (string-empty-p target-id)))
      (user-error "disco: pending jump target is empty"))
    (appkit-chat-history-request-begin 'around owner)
    (appkit-chat-history-older-loaded-set nil)
    (appkit-chat-history-newer-stalled-clear)
    (appkit-request-sync view :part 'frame)
    (disco-api-channel-messages-around-async
     channel-id
     target-id
     :limit limit
     :on-success
     (lambda (messages)
       (when (disco-room--callback-active-p room-buffer channel-id view)
         (with-current-buffer room-buffer
           (when (appkit-chat-history-request-current-p owner)
             (appkit-chat-history-request-end owner)
             (let* ((raw-page (disco-room--normalize-history-page messages))
                    (merged
                     (disco-state-merge-message-page
                      channel-id raw-page request-revision))
                    (page
                     (disco-room--history-page-retained-in-cache
                      raw-page merged)))
               (if (disco-room--message-list-contains-id-p page target-id)
                   (let* ((bounds (disco-room--history-page-bounds page))
                          (oldest (car bounds))
                          (newest (cdr bounds))
                          (canonical-oldest
                           (reverse
                            (disco-room--normalize-history-page merged)))
                          (canonical-newest
                           (disco-room--message-id
                            (car (disco-room--normalize-history-page merged))))
                          (frontier disco-room--remote-latest-message-id)
                          (at-latest
                           (and frontier
                                (equal frontier newest)
                                (equal canonical-newest newest)))
                          (stale-frontier
                           (and frontier
                                (not at-latest)
                                (disco-room--message-id-after-p
                                 canonical-newest frontier
                                 canonical-oldest))))
                     (when stale-frontier
                       (setq disco-room--remote-latest-message-id nil))
                     (appkit-chat-history-window-set
                      oldest (unless at-latest newest))
                     (appkit-chat-history-older-loaded-set nil)
                     (disco-room--request-render view))
                 (setq disco-room--pending-jump-message-id nil)
                 (disco-room--request-render view)
                 (message "disco: message %s not found in around fetch"
                          target-id)))))))
     :on-error
     (lambda (err)
       (when (disco-room--callback-active-p room-buffer channel-id view)
         (with-current-buffer room-buffer
           (when (appkit-chat-history-request-current-p owner)
             (appkit-chat-history-request-end owner)
             (setq disco-room--pending-jump-message-id nil)
             (disco-room--request-render view)
             (message "disco: jump fetch failed: %s"
                      (disco-room--async-error-message err)))))))))

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
      (unless (eq (appkit-chat-history-loading) 'around)
        (disco-room--fetch-around-pending-jump)))))

(defun disco-room--queue-jump (message-id view)
  "Record a jump to MESSAGE-ID and request positioning in originating VIEW."
  (when (and (appkit-view-live-p view)
             (eq view (appkit-current-view)))
    (setq disco-room--pending-jump-message-id
          (disco-msg-normalize-id message-id))
    (appkit-request-sync view :part 'timeline :position t)))

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
        (let ((view (disco-room--ensure-view)))
          (disco-room--queue-jump target-id view)
          ;; Explicit commands may consume the request immediately, while all
          ;; actual projection and positioning still runs through room sync.
          (appkit-sync-invalidations view))
      (let* ((target-chan-obj (disco-room--resolve-target-channel target-channel))
             (target-name (or (and (listp target-chan-obj)
                                   (alist-get 'name target-chan-obj))
                              target-channel)))
        (disco-room--ensure-jump-permissions target-channel target-chan-obj)
        ;; Appkit owns room identity.  A live room buffer may have been renamed,
        ;; so use the actual buffer returned by the view opener instead of
        ;; reconstructing and looking up its original display name.
        (when-let* ((target-buffer (disco-room-open target-channel target-name)))
          (with-current-buffer target-buffer
            (let ((view (disco-room--ensure-view)))
              (disco-room--queue-jump target-id view)
              (appkit-sync-invalidations view))))))))

(defun disco-room--message-flags (msg)
  "Return normalized integer flags value from message MSG."
  (let ((flags (alist-get 'flags msg)))
    (cond
     ((integerp flags) flags)
     ((and (stringp flags)
           (string-match-p "\\`[0-9]+\\'" flags))
      (string-to-number flags))
     (t 0))))


(defun disco-room--message-at-point ()
  "Return message object at point, or signal user error."
  (or (disco-msg-at)
      (user-error "disco: message not found in local room cache")))

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

(defun disco-room--line-fill-column ()
  "Return target fill column for the current message line."
  (or (and (bound-and-true-p visual-fill-column-mode)
           (integerp disco-room-fill-column)
           (> disco-room-fill-column 0)
           disco-room-fill-column)
      (and (bound-and-true-p visual-fill-column-mode)
           (integerp fill-column)
           (> fill-column 0)
           fill-column)
      (appkit-view-responsive-width disco-room-auto-fill-margin-columns)
      (and (integerp fill-column) (> fill-column 0) fill-column)
      80))

(defun disco-room--insert-right-aligned-text (text &optional face left-prefix-width)
  "Insert TEXT aligned to right edge on current line.

When FACE is non-nil, apply FACE to TEXT.  LEFT-PREFIX-WIDTH reserves
additional columns at line start (for future `line-prefix' application)."
  (appkit-chat-ins-insert-right-aligned-text
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
  (appkit-chat-ins-insert-divider-row
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
         (insert-unread (eq (plist-get context :insert-unread) t))
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
    (let ((span (appkit-chat-ins-insert-full-width-divider
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
                                    author-face nil))))
      (when (= (disco-msg-type msg) 6)
        (let ((channel-id disco-room--channel-id))
          (save-excursion
            (goto-char (car span))
            (when (search-forward "View all pinned messages." (cdr span) t)
              (appkit-ui-add-action
               (match-beginning 0) (match-end 0)
               (lambda () (disco-room-list-pinned-messages channel-id))
               :help-echo "Browse pinned messages"
               :face 'link))))))))


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
  (appkit-name-color-face
   (or (disco-room--message-author-id msg)
       (disco-room--message-author msg)
       "unknown")))

(defun disco-room--insert-message-author (msg label face)
  "Insert MSG sender LABEL using FACE as an actionable user title."
  (let* ((user (copy-tree (disco-room--message-effective-author msg)))
         (user-id (and (listp user)
                       (disco-msg-normalize-id (alist-get 'id user))))
         (guild-id
          (disco-msg-normalize-id
           (or (alist-get 'guild_id msg) disco-room--guild-id))))
    (if user-id
        (appkit-ui-insert-action-button
         label
         (lambda () (disco-user-open user guild-id))
         :face face
         :help-echo "Open sender profile"
         :properties
         (list 'read-only t
               'front-sticky '(read-only)
               'rear-nonsticky '(read-only)
               'disco-user-id user-id))
      (let ((start (point)))
        (insert label)
        (add-text-properties start (point) (list 'face face))))))

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
  (appkit-media-image-object-valid-p image))

(defun disco-room--forward-guild-icon-rendering-available-p ()
  "Return non-nil when forwarded-source guild icons can be rendered."
  (and disco-room-show-forward-guild-icons
       (not disco-room--session-cache-reset-in-progress)
       (appkit-media-inline-image-rendering-available-p)
       (fboundp 'plz)))

(defun disco-room--forward-guild-icon-owner-current-p (cache-key owner)
  "Return non-nil when OWNER still owns CACHE-KEY in this account session."
  (and (not disco-room--session-cache-reset-in-progress)
       (= (or (plist-get owner :generation) -1)
          disco-room--forward-guild-icon-fetch-generation)
       (eq owner
           (gethash cache-key disco-room--forward-guild-icon-fetching))))

(defun disco-room--cancel-icon-process (process)
  "Cancel PROCESS when it is live, isolating ordinary cancellation failures."
  (when process
    (condition-case nil
        (when (process-live-p process)
          (delete-process process))
      ((error quit) nil))))

(defun disco-room--cancel-icon-processes (processes)
  "Cancel PROCESSES while guaranteeing every remaining cancellation attempt."
  (let ((remaining processes))
    ;; The cleanup loop is the nonlocal-exit fallback.  Unlike one nested
    ;; `unwind-protect' per process, both passes are stack-safe for thousands
    ;; of concurrent icon owners.
    (unwind-protect
        (while remaining
          (disco-room--cancel-icon-process (pop remaining)))
      ;; Recursion occurs only after an arbitrary nonlocal transfer, not once
      ;; per item on the normal path.  A second transfer during cleanup gets
      ;; its own cleanup frame, so it still cannot skip later owners.
      (when remaining
        (disco-room--cancel-icon-processes remaining)))))

(defun disco-room--run-session-cleanup-actions (actions)
  "Run cleanup ACTIONS even when an earlier action exits nonlocally."
  (when actions
    (unwind-protect
        (funcall (car actions))
      (disco-room--run-session-cleanup-actions (cdr actions)))))

(defun disco-room--forward-guild-icon-finish
    (cache-key owner image valid-p guild-id)
  "Publish IMAGE for CACHE-KEY owned by OWNER and optionally sync GUILD-ID."
  (when (disco-room--forward-guild-icon-owner-current-p cache-key owner)
    (puthash cache-key (if valid-p image :missing)
             disco-room--forward-guild-icon-image-cache)
    ;; Revalidate after the cache mutation in case an instrumented cache or a
    ;; synchronous reset hook retired this owner.
    (when (disco-room--forward-guild-icon-owner-current-p cache-key owner)
      (remhash cache-key disco-room--forward-guild-icon-fetching)
      (when (and valid-p
                 (not disco-room--session-cache-reset-in-progress)
                 (= (plist-get owner :generation)
                    disco-room--forward-guild-icon-fetch-generation))
        (disco-room--sync-resource-changes-in-open-rooms
         (list (list :guild guild-id)))))))

(defun disco-room--forward-guild-icon-fail (cache-key owner)
  "Publish a missing icon for CACHE-KEY only when OWNER remains current."
  (when (disco-room--forward-guild-icon-owner-current-p cache-key owner)
    (puthash cache-key :missing disco-room--forward-guild-icon-image-cache)
    (when (disco-room--forward-guild-icon-owner-current-p cache-key owner)
      (remhash cache-key disco-room--forward-guild-icon-fetching))))

(defun disco-room--start-forward-guild-icon-fetch (cache-key guild-id url)
  "Start async guild icon fetch for CACHE-KEY and GUILD-ID from URL."
  (unless (or disco-room--session-cache-reset-in-progress
              (gethash cache-key disco-room--forward-guild-icon-fetching)
              (gethash cache-key disco-room--forward-guild-icon-image-cache))
    (let* ((generation disco-room--forward-guild-icon-fetch-generation)
           (owner (list :generation generation :process nil))
           process
           returned-p)
      (puthash cache-key owner disco-room--forward-guild-icon-fetching)
      (unwind-protect
          (progn
            (setq process
                  (plz 'get url
                    :as 'binary
                    :headers
                    '(("Accept" . "image/png,image/*;q=0.8,*/*;q=0.1"))
                    :then
                    (lambda (bytes)
                      (when (disco-room--forward-guild-icon-owner-current-p
                             cache-key owner)
                        (let* ((image
                                (ignore-errors
                                  (create-image
                                   bytes 'png t
                                   :width disco-room-forward-guild-icon-size
                                   :height disco-room-forward-guild-icon-size
                                   :ascent 'center)))
                               (valid-p
                                (disco-room--forward-guild-icon-image-valid-p
                                 image)))
                          (disco-room--forward-guild-icon-finish
                           cache-key owner image valid-p guild-id))))
                    :else
                    (lambda (_err)
                      (disco-room--forward-guild-icon-fail cache-key owner))))
            (setq returned-p t))
        (cond
         ((and returned-p
               (disco-room--forward-guild-icon-owner-current-p
                cache-key owner))
          (setf (plist-get owner :process) process))
         ((disco-room--forward-guild-icon-owner-current-p cache-key owner)
          (remhash cache-key disco-room--forward-guild-icon-fetching))
         (returned-p
          ;; A synchronous callback/reset retired OWNER before `plz' returned.
          ;; Do not leave the returned process unowned.
          (disco-room--cancel-icon-process process)))))))

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
        (let ((url (disco-room--forward-guild-icon-url guild))
              (guild-id (disco-msg-normalize-id (alist-get 'id guild))))
          (when (and (stringp url) (not (string-empty-p url)))
            (disco-room--start-forward-guild-icon-fetch
             cache-key guild-id url)))
        nil)))))

(defun disco-room--insert-forward-guild-icon (guild)
  "Insert one forwarded-source guild icon for GUILD."
  (let ((fallback (disco-room--forward-guild-icon-fallback guild))
        (image (disco-room--forward-guild-icon-image guild)))
    (if (disco-room--forward-guild-icon-image-valid-p image)
        (insert-image image fallback)
      (insert fallback))))

(defun disco-room--avatar-user (msg)
  "Return MSG's effective Discord author user alist, or nil."
  (let ((author (disco-room--message-effective-author msg)))
    (and (listp author) author)))

(defun disco-room--reset-forward-guild-icon-state ()
  "Revoke forwarded guild icon work and clear its account-scoped caches."
  (let ((disco-room--session-cache-reset-in-progress t)
        processes)
    ;; Generation is the revocation boundary.  Keep the owner table intact
    ;; until processes have been collected and cancellation has run, but old
    ;; callbacks are already inert before the first process is touched.
    (cl-incf disco-room--forward-guild-icon-fetch-generation)
    (maphash
     (lambda (_cache-key owner)
       (when-let* ((process (and (listp owner)
                                 (plist-get owner :process))))
         (push process processes)))
     disco-room--forward-guild-icon-fetching)
    (unwind-protect
        (disco-room--cancel-icon-processes processes)
      (clrhash disco-room--forward-guild-icon-fetching)
      (clrhash disco-room--forward-guild-icon-image-cache))))

(defun disco-room--clear-session-cache-memory ()
  "Clear account-scoped room cache bookkeeping without running callbacks."
  (setq disco-room--preview-buffer nil
        disco-room-draft-history-search-history nil
        disco-room-search-inplace-history nil)
  (clrhash disco-room--avatar-round-image-cache)
  (clrhash disco-room--forward-guild-icon-fetching)
  (clrhash disco-room--forward-guild-icon-image-cache))

(defun disco-room-reset-session-cache-state ()
  "Destructively clear account-scoped room media state without redrawing.

Shared user avatars are retired independently by `disco-avatar'.  This reset
owns only room presentation caches and exact forwarded-icon process owners.
No Appkit invalidation is requested."
  (let ((disco-room--session-cache-reset-in-progress t))
    (unwind-protect
        (disco-room--reset-forward-guild-icon-state)
      ;; Repeat the destructive clears after cancellation hooks: even an
      ;; instrumented hook which mutates these globals cannot retain old data.
      (disco-room--clear-session-cache-memory))))

(defun disco-room--refresh-open-rooms ()
  "Request geometry projection for all open room timelines."
  (unless disco-room--session-cache-reset-in-progress
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (eq major-mode 'disco-room-mode)
                     (appkit-view-live-p (appkit-current-view)))
            (appkit-request-sync (appkit-current-view) :part 'geometry)))))))

(defun disco-room--refresh-timeline-layout ()
  "Refresh every projected row after buffer display geometry changes."
  (when (appkit-chat-timeline-live-p)
    (appkit-chat-timeline-refresh)))

(defun disco-room--composer-one-line-resource-keys ()
  "Return media resources used by the current composer aux preview."
  (when-let* ((aux-state (appkit-chatbuf-aux-state))
              (message-id (plist-get aux-state :message-id))
              (message
               (or (disco-room--composer-context-message message-id)
                   (plist-get aux-state :aux-msg))))
    (disco-media-message-one-line-resource-keys
     message (disco-room--message-effective-attachments message))))

(defun disco-room--sync-resource-changes-in-open-rooms (resources)
  "Synchronize open room rows depending on opaque RESOURCES."
  (unless disco-room--session-cache-reset-in-progress
    (let ((resources (delete-dups (delq nil (copy-sequence resources)))))
      (when resources
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (when (and (eq major-mode 'disco-room-mode)
                         (appkit-view-live-p (appkit-current-view)))
                (let ((view (appkit-current-view)))
                  (appkit-request-sync view :resources resources)
                  (when (seq-some
                         (lambda (resource)
                           (member resource resources))
                         (disco-room--composer-one-line-resource-keys))
                    (disco-room--update-frame)))))))))))

(defun disco-room--handle-avatar-resources-updated (resources)
  "Synchronize room rows depending on changed avatar RESOURCES."
  (when (and resources
             (not disco-room--session-cache-reset-in-progress))
    (disco-room--sync-resource-changes-in-open-rooms resources)))

(add-hook 'disco-avatar-resources-updated-hook
          #'disco-room--handle-avatar-resources-updated)

(defun disco-room--handle-emoji-resources-updated (resources)
  "Synchronize room rows depending on changed custom emoji RESOURCES."
  (when (and resources
             (not disco-room--session-cache-reset-in-progress))
    (disco-room--sync-resource-changes-in-open-rooms resources)))

(add-hook 'disco-emoji-image-resources-updated-hook
          #'disco-room--handle-emoji-resources-updated)

(defun disco-room--handle-sticker-resources-updated (resources)
  "Synchronize room rows depending on changed sticker RESOURCES."
  (when (and resources
             (not disco-room--session-cache-reset-in-progress))
    (disco-room--sync-resource-changes-in-open-rooms resources)))

(add-hook 'disco-sticker-resources-updated-hook
          #'disco-room--handle-sticker-resources-updated)

(defun disco-room--handle-media-rerender (kind key)
  "Apply media state change KIND for KEY to dependent timeline rows."
  (pcase kind
    ('preview
     (when (stringp key)
       (disco-room--sync-resource-changes-in-open-rooms
        (list (list :preview key)))))
    ((or 'audio 'download)
     (when (stringp key)
       (disco-room--sync-resource-changes-in-open-rooms
        (list (list :attachment key)))))
    ('visual
     (disco-room--refresh-open-rooms))))

(defun disco-room--on-text-scale-change ()
  "Retire cached preview images after `text-scale-mode' changes."
  (when (eq major-mode 'disco-room-mode)
    ;; Appkit's responsive geometry hook owns the coalesced row redraw.
    (disco-media-clear-preview-memory-cache)
    (disco-sticker-clear-image-memory)))

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

(add-hook 'disco-media-rerender-hook #'disco-room--handle-media-rerender)

(defun disco-room--avatar-display-size ()
  "Return full avatar size in pixels for two-line avatar rendering.

`disco-avatar-image-size' is interpreted as baseline size ratio where
`28' maps to exactly two text lines at current scale."
  (let* ((line-height (appkit-chat-avatar-line-pixel-height))
         (base-target (* 2 line-height))
         (size-factor (/ (float (max 1 disco-avatar-image-size)) 28.0)))
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

(defun disco-room--avatar-svg-geometry (cheight)
  "Return derived round-avatar geometry for CHEIGHT text lines."
  (let* ((line-height (appkit-chat-avatar-line-pixel-height))
         (size-factor (/ (float (max 1 disco-avatar-image-size)) 28.0))
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
         (char-width (appkit-chat-avatar-column-pixel-width))
         (aw-chars (max 1 (ceiling (/ ch (float char-width)))))
         (svg-xw (* aw-chars char-width))
         (svg-xh (cond ((= cheight 1) cfull)
                       ((and (= cheight 2)
                             disco-room-avatar-extra-bottom-line)
                        (+ cfull line-height))
                       (t xh))))
    (list :line-height line-height
          :margin margin
          :circle-height ch
          :full-circle-height cfull
          :char-width char-width
          :char-columns aw-chars
          :svg-width svg-xw
          :svg-height svg-xh)))

(defun disco-room--avatar-svg-cache-key (file mtime cheight geometry)
  "Return cache key for FILE, MTIME, CHEIGHT, and derived GEOMETRY."
  (format "%S" (list file mtime cheight geometry)))

(defun disco-room--avatar--create-svg (file cheight)
  "Create telega-style circular avatar SVG image.

FILE is cached avatar image path.  CHEIGHT is avatar height in text lines,
typically 2."
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
           (geometry (disco-room--avatar-svg-geometry cheight))
           (line-height (plist-get geometry :line-height))
           (margin (plist-get geometry :margin))
           (ch (plist-get geometry :circle-height))
           (cfull (plist-get geometry :full-circle-height))
           (aw-chars (plist-get geometry :char-columns))
           (svg-xw (plist-get geometry :svg-width))
           (svg-xh (plist-get geometry :svg-height))
           (cache-key (disco-room--avatar-svg-cache-key
                       file mtime cheight geometry))
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
                          :width svg-xw
                          :ascent 'center
                          :mask 'heuristic)))
              (when image
                (let* ((type (car image))
                       (props (copy-sequence (cdr image))))
                  (setq props
                        (plist-put props :appkit-chat-avatar-char-width aw-chars))
                  (setq props
                        (plist-put props :appkit-chat-avatar-slice-height
                                   line-height))
                  (setq image (cons type props))
                  (puthash cache-key image disco-room--avatar-round-image-cache)
                  image))))))))

(defun disco-room--avatar-one-line-image (msg)
  "Return avatar image sized for one text line for MSG, or nil."
  (when-let* ((user (disco-room--avatar-user msg)))
    (let* ((raw-image (disco-avatar-image user))
           (cache-file (and (appkit-media-image-object-valid-p raw-image)
                            (disco-avatar-cached-file user)))
           (svg-avatar (and disco-room-avatar-round-images
                            cache-file
                            (disco-room--avatar--create-svg
                             cache-file 1))))
      (if (appkit-media-image-object-valid-p svg-avatar)
          svg-avatar
        (when (appkit-media-image-object-valid-p raw-image)
          (let ((line-height (appkit-chat-avatar-line-pixel-height)))
            (appkit-chat-avatar-resize-image raw-image line-height)))))))

(defun disco-room--avatar-one-line-string (msg)
  "Return propertized string showing one-line inline avatar for MSG.
Returns empty string when no avatar is available."
  (let ((image (disco-room--avatar-one-line-image msg)))
    (if (appkit-media-image-object-valid-p image)
        (let* ((char-width (appkit-chat-avatar-image-char-width image))
               (text (make-string (max 1 char-width) ?\s)))
          (propertize text 'display image 'rear-nonsticky '(display)))
      "")))

(defun disco-room--avatar-prefixes (msg)
  "Return avatar-aware prefixes plist for MSG header/body lines."
  (let* ((user (disco-room--avatar-user msg))
         (image (and user (disco-avatar-image user)))
         (fallback (disco-room--avatar-placeholder msg))
         (base-size (disco-room--avatar-display-size)))
    (if (appkit-media-image-object-valid-p image)
        (let* ((pixel-size (if disco-room-avatar-round-images
                               (max 8 (round (* base-size
                                                (max 0.1 disco-room-avatar-round-size-factor))))
                             base-size))
               (cache-file (and user (disco-avatar-cached-file user)))
               (svg-avatar (and disco-room-avatar-round-images
                                cache-file
                                (disco-room--avatar--create-svg
                                 cache-file 2))))
          (appkit-chat-avatar-prefixes
           (or svg-avatar image)
           fallback
           :pixel-size pixel-size
           :resize (null svg-avatar)))
      (appkit-chat-avatar-prefixes
       nil fallback :pixel-size base-size))))

(cl-defun disco-room--insert-attachment-card
    (attachment &key message-id spoiler-hidden owner)
  "Insert one typed rich attachment block for ATTACHMENT object.

OWNER is the exact Appkit app captured by video playback actions."
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
        :spoiler-toggle-action toggle-action
        :owner owner))
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
  (when-let* ((view (appkit-current-view))
              (_ (appkit-view-live-p view))
              (owner (appkit-view-app view))
              (message (ignore-errors (disco-room--message-at-point)))
              (attachment (car (disco-room--message-effective-attachments message))))
    (disco-media-attachment-card-context attachment owner)))

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

(defun disco-room--format-time (iso8601)
  "Format ISO8601 into a compact local timestamp."
  (condition-case nil
      (format-time-string "%Y-%m-%d %H:%M" (date-to-time iso8601))
    (error "unknown-time")))

(defun disco-room--format-time-short (iso8601)
  "Format ISO8601 into a local HH:MM timestamp."
  (condition-case nil
      (format-time-string "%H:%M" (date-to-time iso8601))
    (error "--:--")))

(defun disco-room--forward-snapshot-time-label (msg)
  "Return formatted snapshot timestamp for forwarded MSG, or nil."
  (let* ((snapshot (disco-room--message-forward-snapshot msg))
         (timestamp (and (listp snapshot) (alist-get 'timestamp snapshot))))
    (when (and (stringp timestamp) (not (string-empty-p timestamp)))
      (disco-room--format-time timestamp))))

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
  (let* ((message-id (alist-get 'id msg))
         (resolved (disco-room--thread-starter-reference-message msg))
         (text (and (listp resolved) (alist-get 'content resolved)))
         (display (and (stringp text)
                       (disco-markdown-render
                        text
                        :context 'thread-starter-reference
                        :message resolved
                        :spoiler-message-id message-id
                        :reveal-spoilers
                        (disco-room--message-spoilers-revealed-p message-id)))))
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
         (message-id (alist-get 'id msg))
         (content (string-trim
                   (disco-markdown-render
                    (or (alist-get 'content msg) "")
                    :context 'system-message
                    :message msg
                    :spoiler-message-id message-id
                    :reveal-spoilers
                    (disco-room--message-spoilers-revealed-p message-id))))
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
                            (eq (alist-get 'is_renewal role-subscription) t)))
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
         (stickers (disco-sticker-message-items msg))
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
           (stickers "")
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

(defun disco-room--attachment-summary (attachment)
  "Return one-line attachment summary string for ATTACHMENT object."
  (disco-media-attachment-summary attachment))

(defun disco-room--insert-message-stickers (msg &optional prefix)
  "Insert received sticker image blocks for MSG using PREFIX."
  (dolist (sticker (disco-sticker-message-items msg))
    (let* ((rows (disco-sticker-image-slice-rows sticker))
           (text
            (if rows
                (string-join rows "\n")
              (format "[Sticker: %s]" (disco-sticker-name sticker)))))
      (add-text-properties
       0 (length text)
       (list 'disco-sticker-object sticker
             'mouse-face 'highlight
             'help-echo
             (if (= (or (disco-sticker-format-type sticker) 0) 3)
                 "RET: play Lottie Sticker"
               "Animated Sticker"))
       text)
      (appkit-ui-insert-prefixed-lines prefix text))))

(defun disco-room--insert-message-attachments (msg &optional prefix owner)
  "Insert attachment detail lines for MSG.

PREFIX can be a fixed prefix string or mutable prefix-state.  OWNER is the
exact Appkit app captured by video playback actions."
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
               :spoiler-hidden spoiler-hidden
               :owner owner)
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

(defun disco-room--insert-message-embeds (msg &optional owner)
  "Insert embed detail lines for MSG with exact Appkit OWNER."
  (disco-embed-insert-message-embeds
   (disco-room--message-with-effective-embeds msg)
   owner))

(defun disco-room--same-user-id-p (left right)
  "Return non-nil when non-nil user ids LEFT and RIGHT are equal."
  (and left right
       (equal (format "%s" left) (format "%s" right))))

(defun disco-room--event-self-p (event)
  "Return frozen current-user identity for reaction/poll EVENT.

New Gateway events carry `:self-p' because queued events can outlive the READY
session that supplied `disco-gateway-current-user-id'.  The fallback keeps
directly constructed legacy events and tests compatible."
  (if (plist-member event :self-p)
      (and (plist-get event :self-p) t)
    (disco-room--same-user-id-p
     (disco-gateway-current-user-id)
     (plist-get event :user-id))))

(defun disco-room--forget-message-async-state (message-id)
  "Discard drafts and operation owners belonging to deleted MESSAGE-ID."
  (disco-room-poll-forget-message message-id)
  (disco-room-reaction-forget-message message-id)
  (disco-room--pin-ops-clear-message message-id))

(defun disco-room--pin-op-key (message-id)
  "Return normalized key for a message pin operation."
  (format "%s" message-id))

(defun disco-room--pin-op-begin (message-id pinned)
  "Begin and return an owner token setting MESSAGE-ID to PINNED."
  (unless (hash-table-p disco-room--pin-ops)
    (setq disco-room--pin-ops (make-hash-table :test #'equal)))
  (let ((token (cl-incf disco-room--pin-op-seq)))
    (puthash (disco-room--pin-op-key message-id)
             (list :token token :pinned (and pinned t))
             disco-room--pin-ops)
    token))

(defun disco-room--pin-op-for (message-id)
  "Return current pin operation for MESSAGE-ID, or nil."
  (and (hash-table-p disco-room--pin-ops)
       (gethash (disco-room--pin-op-key message-id) disco-room--pin-ops)))

(defun disco-room--pin-op-current-p (message-id token)
  "Return non-nil when TOKEN still owns MESSAGE-ID's pin operation."
  (let ((operation (disco-room--pin-op-for message-id)))
    (and (listp operation)
         (= (or (plist-get operation :token) -1) token))))

(defun disco-room--pin-op-finish (message-id token)
  "Finish MESSAGE-ID's pin operation when TOKEN still owns it."
  (when (disco-room--pin-op-current-p message-id token)
    (remhash (disco-room--pin-op-key message-id) disco-room--pin-ops)
    t))

(defun disco-room--pin-ops-clear-message (message-id)
  "Invalidate the pending pin operation for MESSAGE-ID."
  (when (hash-table-p disco-room--pin-ops)
    (remhash (disco-room--pin-op-key message-id) disco-room--pin-ops)))

(defun disco-room--update-message-locally (message-id updater)
  "Apply UPDATER to MESSAGE-ID in controller state and return the new message.

This helper never mutates the projected timeline.  External callbacks request
an Appkit entry sync; gateway events are projected by their enclosing room sync."
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
    updated-msg))

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
         (open-help-echo
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
     :open-action (and (stringp ref-id)
                       (not (string-empty-p ref-id))
                       (lambda ()
                         (disco-room-jump-to-message ref-id ref-channel)))
     :open-help-echo open-help-echo
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
           "Forwarded message"
           :prefix prefix
           :face 'shadow
           :action (lambda ()
                     (disco-room-jump-to-message ref-id ref-channel))
           :help-echo "Open forwarded source"))))))

(defun disco-room--insert-message (msg context &optional owner)
  "Insert one message MSG using projected render CONTEXT and Appkit OWNER."
  (if (disco-room--message-system-divider-p msg)
      (disco-room--insert-system-divider-message msg context)
    (let* ((compact (eq (plist-get context :compact) t))
           (insert-date (plist-get context :insert-date))
           (insert-unread (eq (plist-get context :insert-unread) t))
           (timestamp
            (disco-room--format-time (or (alist-get 'timestamp msg) "")))
           (short-time
            (if (alist-get 'pending msg)
                "sending…"
              (disco-room--format-time-short
               (or (alist-get 'timestamp msg) ""))))
           (author (disco-room--message-author msg))
           (author-face (disco-room--author-face msg))
           (content (disco-room--message-display-content msg))
           (reply (disco-room--reply-preview msg))
           (message-id (alist-get 'id msg))
           line-start
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
                  (appkit-ui-make-prefix-state compact-prefix compact-prefix))
            (when reply
              (let ((ref-id (disco-room--reply-reference-id msg))
                    (ref-channel (disco-msg-reference-channel-id msg)))
                (disco-ins-insert-reference-line
                 reply
                 :prefix section-prefix-state
                 :face 'shadow
                 :action (and (stringp ref-id)
                              (not (string-empty-p ref-id))
                              (lambda ()
                                (disco-room-jump-to-message ref-id ref-channel)))
                 :help-echo "Open replied-to message")))
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
              (appkit-ui-apply-line-prefix content-start (point) section-prefix-state)))
        (let* ((avatar-prefixes (disco-room--avatar-prefixes msg))
               (header-prefix (or (plist-get avatar-prefixes :header) ""))
               (header-prefix-width (max 0 (string-width header-prefix)))
               (body-first-prefix (or (plist-get avatar-prefixes :first-body) "    "))
               (body-rest-prefix (or (plist-get avatar-prefixes :rest-body) "    ")))
          (setq section-prefix-state
                (appkit-ui-make-prefix-state body-first-prefix body-rest-prefix))
          (let ((header-start (point)))
            (disco-room--insert-message-author msg author author-face)
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
            (appkit-ui-apply-line-prefix
             header-start (point)
             (appkit-ui-make-prefix-state header-prefix body-rest-prefix)))
          (when reply
            (let ((ref-id (disco-room--reply-reference-id msg))
                  (ref-channel (disco-msg-reference-channel-id msg)))
              (disco-ins-insert-reference-line
               reply
               :prefix section-prefix-state
               :face 'shadow
               :action (and (stringp ref-id)
                            (not (string-empty-p ref-id))
                            (lambda ()
                              (disco-room-jump-to-message ref-id ref-channel)))
               :help-echo "Open replied-to message")))
          (unless (string-empty-p content)
            (appkit-ui-insert-prefixed-lines section-prefix-state content))))
      (let ((appkit-ui-card-indent-prefix-state section-prefix-state)
            (appkit-ui-card-indent-prefix
             (appkit-ui-prefix-string section-prefix-state nil "    ")))
        (disco-room--insert-message-stickers msg section-prefix-state)
        (disco-room--insert-forward-section msg section-prefix-state)
        (disco-room-thread-insert-reference msg section-prefix-state)
        (disco-room--insert-message-attachments msg section-prefix-state owner)
        (disco-room--insert-message-embeds msg owner)
        (disco-room--insert-message-poll msg))
      (disco-room-reaction-insert msg section-prefix-state)
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
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (error "disco: cannot render media actions without an exact live view"))
    (disco-room--insert-message
     (appkit-chat-timeline-row-payload row)
     (or (appkit-chat-timeline-row-context row) '())
     (appkit-view-app view))))

(defun disco-room--input-footer-context-text ()
  "Return extra context lines shown above the room composer."
  (concat
   (or (disco-room--composer-context-text
        (appkit-chatbuf-aux-state))
       "")
   (if disco-room--pending-attachments
       (format "Queued attachments: %s\n"
               (mapconcat #'identity
                          (disco-room--pending-attachment-labels)
                          ", "))
     "")))

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
      (appkit-chatbuf-aux-set aux-state)
    (appkit-chatbuf-aux-reset)))

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
  (appkit-chatbuf-input-options-state))

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
  (appkit-chatbuf-input-options-set
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
  (appkit-chatbuf-init-state disco-room-input-history-size)
  (disco-room--sync-shared-aux-state)
  (disco-room--sync-shared-input-options-state)
  (appkit-chatbuf-bind-input-region
   :visible-p (disco-room--composer-visible-p)
   :prompt (disco-room--prompt-text)
   :input-text (disco-room--current-draft)
   :post-bind-function #'disco-room--apply-input-text-properties))

(defun disco-room--header-text (&optional channel)
  "Build EWOC header text for the current room state."
  (let* ((channel (or channel (disco-room--channel-object)))
         (channel-name (or disco-room--channel-name ""))
         (channel-suffix (disco-room--channel-header-suffix channel))
         (composer-visible-p (disco-room--composer-visible-p channel))
         (context-text (and (not composer-visible-p)
                            (disco-room--input-footer-context-text)))
         (filter-line (disco-room--msg-filter-status-line))
         (composer-status-line (disco-room--composer-hidden-status-line channel))
         (text
          (with-temp-buffer
            (insert (format "Channel: %s%s" channel-name channel-suffix))
            (when disco-room--send-in-flight
              (insert "   [sending...]"))
            (insert "\n")
            (when (and (stringp context-text)
                       (not (string-empty-p context-text)))
              (insert context-text))
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
  (concat
   (unless (disco-room--msg-filter-active-p)
     (concat
      (appkit-chat-history-delimiter-string
       (max 1 (disco-room--line-fill-column))
       :loading-text "loading…")
      "\n"))
   (disco-room--input-footer-text)))

(defun disco-room--ensure-timeline (&optional channel draft)
  "Ensure current room buffer owns one shared projected timeline."
  (disco-room--ensure-view)
  (appkit-chat-timeline-ensure
   :printer #'disco-room--ewoc-printer
   :anchor-property 'disco-message-id
   :header (disco-room--header-text channel)
   :footer (disco-room--footer-text draft)
   :after-mutation-function #'appkit-chatbuf-update-context-mode))

(defun disco-room--view-id ()
  "Return the opaque appkit view id for the current room."
  (unless disco-room--channel-id
    (error "disco: room buffer has no channel id"))
  (list 'room disco-room--channel-id))

(defun disco-room--request-render (view)
  "Request one coalesced full room projection for live VIEW.

Asynchronous callbacks use this boundary after updating canonical/controller
state.  Generated buffer content is mutated later by the Appkit sync function."
  (when (appkit-view-live-p view)
    (appkit-request-sync
     view
     :structure t
     :parts '(frame timeline composer))))

(defun disco-room--sync-invalidations (view invalidations)
  "Synchronize current room from coalesced appkit INVALIDATIONS."
  (let ((events (appkit-view-pending-events-snapshot view))
        (parts (appkit-invalidations-parts invalidations))
        (resources (appkit-invalidations-resource-keys invalidations))
        (entries (appkit-invalidations-entry-keys invalidations)))
    (dolist (event events)
      (when (appkit-view-live-p view)
        (disco-room--apply-gateway-event event)))
    (when (appkit-view-live-p view)
      (appkit-view-acknowledge-events view (length events)))
    (when (appkit-view-live-p view)
      (cond
       ((or (appkit-invalidations-structure-p invalidations)
            parts
            (appkit-invalidations-position-p invalidations))
        (disco-room-render)
        (when (memq 'geometry parts)
          (disco-room--refresh-timeline-layout))
        ;; History callbacks only record their new window and request this sync.
        ;; Resolve jumps after projection so message positions are current.
        (disco-room--resolve-pending-jump))
       ((or resources entries)
        (disco-room--sync-timeline
         :force-keys entries
         :changed-resources resources))))))

(defun disco-room--ensure-view ()
  "Return the live appkit view owning the current room buffer."
  (let* ((app (disco-runtime-app))
         (id (disco-room--view-id))
         (current (appkit-current-view))
         (view
          (cond
           ((and (appkit-view-live-p current)
                 (eq app (appkit-view-app current))
                 (equal id (appkit-view-id current)))
            (setf (appkit-view-state current) disco-room--channel-id
                  (appkit-view-sync-function current)
                  #'disco-room--sync-invalidations
                  (appkit-view-parts current)
                  '(frame timeline composer geometry))
            current)
           ((appkit-view-live-p current)
            (error "disco: room buffer belongs to a different appkit view"))
           (t
            (let* ((channel-id disco-room--channel-id)
                   (channel-name disco-room--channel-name)
                   (replacement-p
                    (and (boundp 'appkit--view-fingerprint)
                         appkit--view-fingerprint))
                   (attached
                    (appkit-attach-view
                     :app app
                     :id id
                     :state channel-id
                     :mode 'disco-room-mode
                     :sync-function #'disco-room--sync-invalidations
                     :parts '(frame timeline composer geometry))))
              ;; A same-mode buffer may outlive its previous Appkit view.
              ;; Replacements must not inherit dead controller state.
              (when replacement-p
                (disco-room--reset-view-local-state
                 channel-id channel-name))
              attached)))))
    (appkit-view-enable-responsive-geometry view)
    view))

(defun disco-room--update-frame (&optional channel draft)
  "Update current room header, footer, and composer in place."
  (disco-room--ensure-timeline channel draft)
  (appkit-chat-timeline-set-frame
   (disco-room--header-text channel)
   (disco-room--footer-text draft)
   :bind-input-function #'disco-room--bind-input-region-from-footer
   :composer-visible-p (disco-room--composer-visible-p channel)))

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
    (when-let* ((user (disco-room--avatar-user msg))
                (resource (disco-avatar-resource-key user)))
      (push resource dependencies))
    (dolist
        (identity
         (disco-markdown-custom-emoji-identities
          (and (listp msg) (alist-get 'content msg))))
      (when-let* ((resource
                   (disco-emoji-image-resource-key
                    (plist-get identity :id)
                    (plist-get identity :animated))))
        (push resource dependencies)))
    (dolist (sticker (disco-sticker-message-items msg))
      (when-let* ((resource (disco-sticker-resource-key sticker)))
        (push resource dependencies)))
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
        (push (list :attachment key) dependencies))
      (when-let* ((key (disco-media-attachment-preview-cache-key attachment)))
        (push (list :preview key) dependencies)))
    (dolist (key (disco-embed-message-preview-cache-keys msg))
      (push (list :preview key) dependencies))
    (delete-dups (delq nil dependencies))))

(defun disco-room--message-affects-composer-context-p (message-id)
  "Return non-nil when MESSAGE-ID is used by current composer context."
  (or (equal message-id (disco-room--composer-reply-message-id))
      (equal message-id (disco-room--composer-edit-message-id))))

(defun disco-room--retire-deleted-composer-context (message-id)
  "Retire aux state targeting deleted MESSAGE-ID without changing the draft."
  (when (disco-room--message-affects-composer-context-p message-id)
    (disco-room--set-composer-aux-state nil nil)
    t))

(defun disco-room--project-timeline (ordered-messages)
  "Project ORDERED-MESSAGES into shared timeline rows."
  (let ((first-unread-id
         (disco-room--first-unread-message-id ordered-messages)))
    (appkit-chat-timeline-project
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
    (appkit-chat-timeline-sync
     (disco-room--project-timeline messages)
     :force-keys force-keys
     :changed-resources changed-resources
     :rekeys rekeys)))

(defun disco-room--apply-read-state-change ()
  "Synchronize projected unread-divider context after read-state changes."
  (when (and (appkit-chat-timeline-live-p)
             (not (disco-room--msg-filter-active-p)))
    (disco-room--sync-timeline)
    t))

(defun disco-room--apply-forward-source-change (&optional source-channel-id
                                                          source-guild-id)
  "Synchronize rows depending on SOURCE-CHANNEL-ID or SOURCE-GUILD-ID."
  (when (and (appkit-chat-timeline-live-p)
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

(defun disco-room--apply-filtered-message-delete (message-id)
  "Remove MESSAGE-ID from the active filter and reject stale filter pages."
  (let* ((filter disco-room--msg-filter)
         (items (or (plist-get filter :items) '()))
         (remaining
          (seq-remove
           (lambda (message)
             (equal message-id (disco-room--message-id message)))
           items))
         (removed-p (< (length remaining) (length items)))
         (request-invalidated-p disco-room--filter-in-flight))
    (when request-invalidated-p
      ;; Load-more callbacks capture the old item list.  Invalidate them so a
      ;; response cannot resurrect a Gateway-deleted search result.
      (setq disco-room--filter-generation
            (1+ (or disco-room--filter-generation 0)))
      (setq disco-room--filter-in-flight nil))
    (when removed-p
      (let ((updated (copy-sequence filter))
            (total (plist-get filter :total-count)))
        (setq updated (plist-put updated :items remaining))
        (when (and (numberp total) (> total 0))
          (setq updated (plist-put updated :total-count (1- total))))
        (setq disco-room--msg-filter updated)))
    removed-p))

(defun disco-room--apply-live-message-event (event)
  "Apply live message EVENT through canonical projected synchronization."
  (let* ((event-type (plist-get event :type))
         (event-message (plist-get event :message))
         (message-id (or (and (listp event-message) (alist-get 'id event-message))
                         (plist-get event :message-id)))
         (composer-context-p
          (and message-id
               (disco-room--message-affects-composer-context-p message-id)))
         (nonce (and (listp event-message)
                     (disco-msg-normalize-id
                      (alist-get 'nonce event-message))))
         (rekeys
          (and nonce message-id
               (not (equal nonce message-id))
               (appkit-chat-timeline-node nonce)
               (list (cons nonce message-id)))))
    (cond
     ((not (memq event-type '(message-create message-update message-delete)))
      (error "disco: unsupported live message event: %S" event-type))
     ((not message-id)
      (error "disco: live message event has no message id: %S" event))
     (t
      (when (eq event-type 'message-create)
        (disco-room--observe-live-create message-id))
      (when (eq event-type 'message-delete)
        ;; The canonical message is already gone when the room consumes this
        ;; event.  Retire controller owners and matching composer aux state.
        (disco-room--forget-message-async-state message-id)
        (disco-room--retire-deleted-composer-context message-id))
      (when composer-context-p
        (disco-room--update-frame))
      (if (disco-room--msg-filter-active-p)
          (progn
            ;; Search results are a separate projection.  A deleted exact edge
            ;; invalidates the old continuous-window proof.
            (when (eq event-type 'message-delete)
              (when (equal message-id disco-room--remote-latest-message-id)
                (setq disco-room--remote-latest-message-id
                      (disco-room--message-id
                       (car (disco-room--canonical-cache-newest-first)))))
              (when (or (equal message-id
                               (appkit-chat-history-window-first-key))
                        (equal message-id
                               (appkit-chat-history-window-last-key)))
                (appkit-chat-history-request-cancel)
                (appkit-chat-history-window-clear))
              (disco-room--apply-filtered-message-delete message-id))
            'filtered)
        (when (eq event-type 'message-delete)
          (disco-room--repair-history-window-after-delete message-id))
        ;; This helper runs only while `disco-room--sync-invalidations' consumes
        ;; a gateway event.  Preserve keyed-node identity for optimistic sends
        ;; inside that projection transaction.
        (disco-room--sync-timeline
         :changed-resources (list (list :message message-id))
         :rekeys rekeys)
        (disco-room--sync-visible-window-cursors
         (disco-room--display-messages))
        'updated)))))

(defun disco-room-render ()
  "Synchronize the room frame and projected timeline from local state."
  (let* ((channel (disco-room--channel-object))
         (messages (disco-room--display-messages))
         (draft (disco-room--current-draft))
         (initial-p (not (appkit-chat-timeline-live-p)))
         (preview-fetch-budget
          (when (numberp disco-media-preview-max-fetches-per-render)
            (max 0 disco-media-preview-max-fetches-per-render))))
    (disco-room--sync-visible-window-cursors messages)
    (disco-media-set-preview-fetch-budget preview-fetch-budget)
    (unwind-protect
        (progn
          ;; API returns newest-first by default; reverse for chat-like display.
          (disco-room--sync-timeline :ordered-messages (reverse messages))
          ;; Bind the trailing composer only after first EWOC reconciliation;
          ;; before that the empty footer has no stable external tail boundary.
          (disco-room--update-frame channel draft)
          (when (and initial-p (appkit-chatbuf-input-start-position))
            (let ((logical-end (appkit-chatbuf-input-logical-end-position)))
              (when logical-end
                (goto-char logical-end)))))
      (disco-media-set-preview-fetch-budget nil)
      (appkit-chatbuf-update-context-mode))))

(defun disco-room-refresh ()
  "Fetch and redraw latest messages for current room asynchronously."
  (interactive)
  (if (disco-room--msg-filter-active-p)
      (disco-room-filter-refresh)
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-view))
           (request-revision (disco-state-message-revision channel-id))
           (request-limit (max 1 disco-message-fetch-limit))
           (owner (list :kind 'latest-history
                        :channel-id channel-id
                        :frontier-at-start
                        disco-room--remote-latest-message-id)))
      (appkit-chat-history-request-begin 'latest owner)
      (appkit-request-sync view :part 'frame)
      (disco-api-channel-messages-async
       channel-id
       :limit request-limit
       :on-success
       (lambda (messages)
         (when (disco-room--callback-active-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (appkit-chat-history-request-current-p owner)
               (let* ((raw-page (disco-room--normalize-history-page messages))
                      (merged
                       (disco-state-merge-message-page
                        channel-id raw-page request-revision))
                      (page
                       (disco-room--history-page-retained-in-cache
                        raw-page merged))
                      (result
                       (disco-room--establish-latest-history-window
                        page
                        (plist-get owner :frontier-at-start)
                        (length raw-page)
                        request-limit)))
                 (appkit-chat-history-request-end owner)
                 (unless (eq result 'conflicted)
                   (disco-room--mark-read nil t))
                 (disco-room--request-render view)
                 (if (eq result 'conflicted)
                     (message "disco: history changed concurrently; refresh again")
                   (message "disco: loaded %d messages" (length page))))))))
       :on-error
       (lambda (err)
         (when (disco-room--callback-active-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (appkit-chat-history-request-current-p owner)
               (appkit-chat-history-request-end owner)
               (disco-room--request-render view)
               (message "disco: room refresh failed: %s"
                        (disco-room--async-error-message err))))))))))

(defun disco-room--close-for-deleted-channel (reason)
  "Close current room because its backing channel is no longer valid.

REASON is shown in the minibuffer."
  (let ((buf (current-buffer)))
    (disco-room--detach-live-updates)
    (kill-buffer buf)
    (message "%s" reason)))

(defun disco-room--apply-gateway-event (event)
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
                   (stringp message-id)
                   (disco-room--message-list-contains-id-p
                    (disco-room--display-messages) message-id))
          (disco-room--mark-read message-id t))))
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
  "Attach this room's Appkit view to the live gateway event stream."
  (let ((view (disco-room--ensure-view)))
    (if (and (functionp disco-room--gateway-handler)
             (appkit-handle-p disco-room--live-update-handle)
             (appkit-handle-alive-p disco-room--live-update-handle)
             (eq view (appkit-handle-owner disco-room--live-update-handle)))
        view
      (disco-room--detach-live-updates)
      (let* ((buffer (current-buffer))
             (channel-id disco-room--channel-id)
             (handler
              (lambda (event)
                (when (appkit-view-live-p view)
                  (appkit-view-enqueue-event view event)
                  (appkit-request-sync view :part 'timeline))))
             (hook-installed-p nil)
             (watch-installed-p nil)
             (cleanup-active-p t)
             handle
             (cleanup
              (lambda ()
                ;; The handle and this guard jointly make cleanup idempotent.  The
                ;; captured identities also keep an old view from removing a
                ;; replacement view's handler or watch.
                (when cleanup-active-p
                  (setq cleanup-active-p nil)
                  (when hook-installed-p
                    (setq hook-installed-p nil)
                    (remove-hook 'disco-gateway-event-hook handler))
                  (when watch-installed-p
                    (setq watch-installed-p nil)
                    (disco-gateway-unwatch-channel channel-id))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (when (eq disco-room--gateway-handler handler)
                        (setq disco-room--gateway-handler nil))
                      (when (eq disco-room--live-update-handle handle)
                        (setq disco-room--live-update-handle nil))))))))
        (condition-case err
            (progn
              (add-hook 'disco-gateway-event-hook handler)
              (setq hook-installed-p t)
              (setq watch-installed-p t)
              (disco-gateway-watch-channel channel-id)
              (setq handle (appkit-register-handle view 'function cleanup))
              (setq disco-room--gateway-handler handler
                    disco-room--live-update-handle handle))
          (error
           (funcall cleanup)
           (signal (car err) (cdr err))))
        view))))

(defun disco-room--detach-live-updates ()
  "Detach this room buffer from the live update event stream exactly once."
  (let ((handle disco-room--live-update-handle)
        (handler disco-room--gateway-handler)
        (channel-id disco-room--channel-id))
    (setq disco-room--live-update-handle nil)
    (cond
     ((and (appkit-handle-p handle) (appkit-handle-alive-p handle))
      (appkit-cancel-handle handle))
     (handler
      ;; Compatibility cleanup for buffers attached before lifecycle ownership
      ;; was installed.  Clearing HANDLER makes repeated detach calls inert.
      (remove-hook 'disco-gateway-event-hook handler)
      (setq disco-room--gateway-handler nil)
      (when channel-id
        (disco-gateway-unwatch-channel channel-id)))))
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
  (let ((bounds (appkit-chatbuf-input-region-bounds))
        (pos (point))
        found)
    (when (and bounds (appkit-chatbuf-point-in-input-p pos))
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
  (let ((object (appkit-chatbuf-input-object-at-point)))
    (when (disco-room--attachment-input-object-p object)
      (appkit-chatbuf-input-object-bounds-at-point))))

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


(defun disco-room--message-pinned-p (msg)
  "Return non-nil when MSG is pinned."
  (eq (alist-get 'pinned msg) t))

(defun disco-room--message-with-pinned-state (msg pinned)
  "Return MSG copy with its pinned flag set to PINNED."
  (let ((updated (copy-tree msg)))
    (setf (alist-get 'pinned updated nil 'remove) (if pinned t :false))
    updated))

(defun disco-room--toggle-pin-on-msg (msg)
  "Toggle the pin state of MSG."
  (disco-room--ensure-action-available
   (disco-room--pin-message-unavailable-reason msg)
   "toggle message pins")
  (disco-room-toggle-pin (alist-get 'id msg)))

(defun disco-room-toggle-pin (&optional message-id)
  "Toggle whether MESSAGE-ID at point is pinned."
  (interactive)
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (msg (or (disco-room--message-by-id target-id)
                  (disco-room--message-at-point))))
    (disco-room--ensure-action-available
     (disco-room--pin-message-unavailable-reason msg)
     "toggle message pins")
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-view))
           (pending (disco-room--pin-op-for target-id))
           (pinned (not (if pending
                            (plist-get pending :pinned)
                          (disco-room--message-pinned-p msg))))
           (op-token (disco-room--pin-op-begin target-id pinned))
           (operation (if pinned
                          #'disco-api-pin-message-async
                        #'disco-api-unpin-message-async))
           (verb (if pinned "pinned" "unpinned")))
      (funcall
       operation channel-id target-id
       :on-success
       (lambda (_response)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--pin-op-current-p target-id op-token)
               (disco-room--update-message-locally
                target-id
                (lambda (message)
                  (disco-room--message-with-pinned-state message pinned)))
               (disco-room--pin-op-finish target-id op-token)
               (appkit-request-sync view :entry target-id)
               (message "disco: message %s" verb)))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--pin-op-finish target-id op-token)
               (message "disco: %s message failed: %s"
                        (if pinned "pin" "unpin")
                        (disco-room--async-error-message err))))))))))


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
                   (appkit-chatbuf-input-history-elements)
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
        (appkit-chatbuf-input-history-reset)
        (disco-room--set-draft picked)
        (message "disco: loaded draft history match"))))))

(defun disco-room--owned-preview-buffer ()
  "Return the explicitly owned composer preview buffer, creating it if needed."
  (or (and (buffer-live-p disco-room--preview-buffer)
           (buffer-local-value 'disco-room--preview-buffer-owner-p
                               disco-room--preview-buffer)
           disco-room--preview-buffer)
      (let* ((named (get-buffer disco-room--preview-buffer-name))
             (buffer
              (if (and (buffer-live-p named)
                       (buffer-local-value
                        'disco-room--preview-buffer-owner-p named))
                  named
                ;; A display-name collision never transfers ownership of an
                ;; ordinary user buffer to Disco.
                (generate-new-buffer disco-room--preview-buffer-name))))
        (with-current-buffer buffer
          (setq-local disco-room--preview-buffer-owner-p t))
        (setq disco-room--preview-buffer buffer))))

(defun disco-room-input-preview ()
  "Show parsed preview of the current composer input."
  (interactive)
  (let* ((draft (disco-room--current-draft))
         (parsed (disco-room--parse-draft-input draft))
         (content (string-trim-right (or (plist-get parsed :content) "")))
         (attachments (or (plist-get parsed :attachments) '()))
         (buf (disco-room--owned-preview-buffer))
         (mode-label (pcase (plist-get (appkit-chatbuf-aux-state) :aux-type)
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
        (special-mode)
        (setq-local disco-room--preview-buffer-owner-p t)))
    (display-buffer buf)
    (message "disco: opened composer preview")))

(defun disco-room-attach (attach-type)
  "Choose ATTACH-TYPE and invoke its configured attachment command."
  (interactive
   (list
    (completing-read
     "Attachment type: "
     (mapcar #'car disco-room-attach-commands)
     nil t)))
  (let ((command (cadr (assoc attach-type disco-room-attach-commands))))
    (unless (commandp command)
      (user-error "disco: invalid attachment command for %s" attach-type))
    (call-interactively command)))


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
                                       `((poll_media . ((text . ,option)))))
                                     options))
                 (duration . ,duration)
                 (allow_multiselect . ,(if allow-multiselect t :false))))
         (room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-view))
         request-revision
         (required-permissions
          (append (disco-room--required-send-permissions)
                  '(send-polls))))
    (disco-api--validate-message-content-length content "content")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     required-permissions
     :action "sending poll")
    (setq request-revision
          (disco-state-message-revision channel-id))
    (setq disco-room--send-in-flight t)
    (appkit-request-sync view :part 'frame)
    (disco-api-create-message-async
     channel-id
     :content content
     :poll poll
     :allowed-mentions (disco-room--send-allowed-mentions)
     :on-success
     (lambda (response)
       (when (and (listp response) (alist-get 'id response))
         (disco-state-merge-message-response
          channel-id response request-revision))
       (when (disco-room--channel-buffer-p room-buffer channel-id view)
         (with-current-buffer room-buffer
           (setq disco-room--send-in-flight nil)
           (cond
            ((and (listp response)
                  (alist-get 'id response)
                  (disco-room--channel-message-by-id
                   channel-id (alist-get 'id response)))
             (disco-room--observe-live-create (alist-get 'id response)))
            ((not (and (listp response) (alist-get 'id response)))
             (disco-room-refresh)))
           (disco-room--request-render view)
           (message "disco: poll sent"))))
     :on-error
     (lambda (err)
       (when (disco-room--channel-buffer-p room-buffer channel-id view)
         (with-current-buffer room-buffer
           (setq disco-room--send-in-flight nil)
           (disco-room--request-render view)
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
    (if (appkit-chatbuf-input-start-position)
        (progn
          (unless (appkit-chatbuf-point-in-input-p)
            (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
          (disco-room--insert-attachment-input-object attachment)
          (disco-room--sync-draft-from-buffer))
      (let* ((draft (disco-room--current-draft))
             (separator (if (or (string-empty-p (appkit-chatbuf-string-plain-text draft))
                                (string-match-p "[ \t\n]\\'"
                                                (appkit-chatbuf-string-plain-text draft)))
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

(defun disco-room--send-sticker-object (sticker)
  "Send one normalized STICKER in the current room."
  (let ((sticker-id (disco-sticker-id sticker)))
    (unless sticker-id
      (user-error "disco: selected Sticker has no valid ID"))
    (disco-room--ensure-action-available
     (disco-room--sticker-unavailable-reason)
     "send stickers")
    (disco-permission-ensure-channel
     (disco-room--channel-object)
     (disco-room--required-send-permissions)
     :action "sending stickers")
    (if disco-room--send-in-flight
        (message "disco: send already in progress")
      (let ((room-buffer (current-buffer))
            (channel-id disco-room--channel-id)
            (view (disco-room--ensure-view))
            (request-revision
             (disco-state-message-revision disco-room--channel-id)))
        (setq disco-room--send-in-flight t)
        (appkit-request-sync view :part 'frame)
        (disco-api-send-message-async
         channel-id nil
         :sticker-ids (list sticker-id)
         :on-success
         (lambda (response)
           (when (and (listp response) (alist-get 'id response))
             (disco-state-merge-message-response
              channel-id response request-revision))
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room--request-render view)
               (message
                (if (and (listp response) (alist-get 'id response))
                    "disco: sticker sent"
                  "disco: sticker send failed: Discord returned no message")))))
         :on-error
         (lambda (err)
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (setq disco-room--send-in-flight nil)
               (disco-room--request-render view)
               (message "disco: sticker send failed: %s"
                        (disco-room--async-error-message err))))))))))

(defun disco-room--read-and-send-sticker (&optional ranked-only)
  "Read and send a sticker, optionally restricting to account rankings."
  (when-let* ((sticker
               (disco-sticker-read disco-room--guild-id ranked-only)))
    (disco-room--send-sticker-object sticker)))

(defun disco-room-send-sticker (&optional ranked-only)
  "Select and send a sticker.

With prefix RANKED-ONLY, offer only Favorite and Frequently Used stickers."
  (interactive "P")
  (disco-room--ensure-action-available
   (disco-room--sticker-unavailable-reason)
   "send stickers")
  (cond
   (disco-room--send-in-flight
    (message "disco: send already in progress"))
   ((disco-sticker-ready-p disco-room--guild-id)
    (disco-sticker-ensure-ready disco-room--guild-id)
    (disco-room--read-and-send-sticker ranked-only))
   (disco-room--sticker-picker-pending
    (message "disco: sticker catalog is still loading"))
   (t
    (let ((room-buffer (current-buffer))
          (channel-id disco-room--channel-id)
          (guild-id disco-room--guild-id)
          (view (disco-room--ensure-view)))
      (setq disco-room--sticker-picker-pending t)
      (message "disco: loading sticker catalog…")
      (disco-sticker-ensure-ready
       guild-id
       :on-success
       (lambda ()
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (setq disco-room--sticker-picker-pending nil)
             (run-at-time
              0 nil
              (lambda ()
                (when (disco-room--channel-buffer-p
                       room-buffer channel-id view)
                  (with-current-buffer room-buffer
                    (disco-room--read-and-send-sticker ranked-only))))))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (setq disco-room--sticker-picker-pending nil)
             (message "disco: sticker catalog load failed: %s"
                      (disco-room--async-error-message err))))))))))

(defun disco-room-send-message ()
  "Send current draft message to this room asynchronously.

When called with prefix argument, force draft edit in minibuffer first."
  (interactive)
  (when (appkit-chatbuf-input-start-position)
    (appkit-chatbuf-input-prune-broken-objects)
    (disco-room--sync-draft-from-buffer))
  (disco-room--ensure-action-available
   (disco-room--send-message-unavailable-reason)
   (if (disco-room--composer-edit-active-p)
       "save edits"
     "send messages"))
  (if disco-room--send-in-flight
      (message "disco: send already in progress")
    (let* ((current-draft (disco-room--current-draft))
           (current-draft-text (appkit-chatbuf-string-plain-text current-draft))
           (initial-has-attachments
            (not (null (disco-room--attachments-from-draft current-draft))))
           (prompt-edit-p
            (or current-prefix-arg
                (and (string-empty-p (string-trim-right current-draft-text))
                     (not initial-has-attachments)))))
      (when (and current-prefix-arg
                 (appkit-chatbuf-string-has-objects-p current-draft))
        (user-error
         "disco: minibuffer send-edit is unavailable for structured input objects"))
      (let* ((content (if prompt-edit-p
                          (read-from-minibuffer "Message: " current-draft-text)
                        current-draft))
             (parsed-input (disco-room--parse-draft-input content))
             (parsed-attachments (plist-get parsed-input :attachments))
             (has-attachments (not (null parsed-attachments)))
             (normalized
              (string-trim-right (or (plist-get parsed-input :content) "")))
             (edit-message-id (disco-room--composer-edit-message-id))
             (over-limit-p
              (disco-room--message-content-over-limit-p normalized)))
        (if (and (string-empty-p normalized)
                 (not has-attachments)
                 (not edit-message-id))
            (message "disco: draft is empty")
          (let* ((room-buffer (current-buffer))
                 (channel-id disco-room--channel-id)
                 (view (disco-room--ensure-view))
                 (reply-to (disco-room--composer-reply-message-id))
                 (allowed-mentions
                  (disco-room--send-allowed-mentions (not (null reply-to))))
                 (attachments (copy-tree parsed-attachments))
                 (edit-message
                  (and edit-message-id
                       (disco-room--composer-context-message edit-message-id)))
                 (long-message-action
                  (and over-limit-p
                       (disco-room--input-option-long-message-action)))
                 (needs-attach-files-p
                  (or has-attachments (eq long-message-action 'file)))
                 (required-permissions
                  (append
                   (disco-room--required-send-permissions)
                   (when needs-attach-files-p '(attach-files))
                   (when reply-to '(read-message-history))))
                 (operation-slot (disco-room--composer-operation-slot))
                 (operation-settled-p nil)
                 cleared-revision)
            (setq operation-slot
                  (plist-put operation-slot :draft
                             (appkit-chatbuf-copy-string content)))
            (if edit-message-id
                (progn
                  (disco-api--validate-message-content-length
                   normalized "content")
                  (disco-room--ensure-action-available
                   (disco-room--edit-permission-reason edit-message)
                   "edit messages")
                  (when has-attachments
                    (user-error
                     "disco: editing via composer does not support attachments yet"))
                  (let ((request-revision
                         (disco-state-message-revision channel-id))
                        (saved-state
                         (copy-tree
                          (plist-get
                           (plist-get operation-slot :pending-edit)
                           :saved-state))))
                    (condition-case err
                        (progn
                          (setq cleared-revision
                                (disco-room--clear-composer-operation-slot)
                                disco-room--send-in-flight t)
                          (appkit-request-sync view :part 'frame)
                          (cl-labels
                              ((finish-error (error-data)
                                 (unless operation-settled-p
                                   (setq operation-settled-p t)
                                   (when (disco-room--channel-buffer-p
                                          room-buffer channel-id view)
                                     (with-current-buffer room-buffer
                                       (setq disco-room--send-in-flight nil)
                                       (disco-room--restore-composer-operation-slot
                                        cleared-revision operation-slot t)
                                       (disco-room--request-render view)
                                       (message
                                        "disco: edit failed for %s: %s"
                                        edit-message-id
                                        (disco-room--async-error-message
                                         error-data))))))
                               (finish-success (response)
                                 (if (not (and (listp response)
                                               (alist-get 'id response)))
                                     (finish-error
                                      (list 'error
                                            "Discord edit-message returned no message"))
                                   (unless operation-settled-p
                                     (setq operation-settled-p t)
                                     (disco-state-merge-message-response
                                      channel-id response request-revision)
                                     (when (disco-room--channel-buffer-p
                                            room-buffer channel-id view)
                                       (with-current-buffer room-buffer
                                         (setq disco-room--send-in-flight nil)
                                         (when (= cleared-revision
                                                  (appkit-chatbuf-composer-revision))
                                           (disco-room--composer-edit-restore-state
                                            saved-state t))
                                         (disco-room--request-render view)
                                         (message
                                          "disco: edited message %s"
                                          edit-message-id)))))))
                            (disco-api-edit-message-async channel-id edit-message-id normalized
                                                          :allowed-mentions
                                                          (disco-room--send-allowed-mentions)
                                                          :on-success #'finish-success
                                                          :on-error #'finish-error)))
                      (error
                       (unless operation-settled-p
                         (setq operation-settled-p t)
                         (when (disco-room--channel-buffer-p
                                room-buffer channel-id view)
                           (with-current-buffer room-buffer
                             (setq disco-room--send-in-flight nil)
                             (when (integerp cleared-revision)
                               (disco-room--restore-composer-operation-slot
                                cleared-revision operation-slot t))
                             (disco-room--request-render view))))
                       (signal (car err) (cdr err))))))
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
                (appkit-chatbuf-input-history-push normalized))
              (let ((recovery-slot operation-slot))
                (condition-case err
                    (progn
                      (setq cleared-revision
                            (disco-room--clear-composer-operation-slot)
                            disco-room--send-in-flight t)
                      (appkit-request-sync view :part 'frame)
                      (cl-labels
                          ((room-active-p ()
                             (disco-room--channel-buffer-p
                              room-buffer channel-id view))
                           (settle-success
                             (text)
                             (unless operation-settled-p
                               (setq operation-settled-p t)
                               (when (room-active-p)
                                 (with-current-buffer room-buffer
                                   (setq disco-room--send-in-flight nil)
                                   (disco-room--request-render view)
                                   (message "%s" text)))))
                           (settle-failure
                             (slot error-data text)
                             (unless operation-settled-p
                               (setq operation-settled-p t)
                               (when (room-active-p)
                                 (with-current-buffer room-buffer
                                   (setq disco-room--send-in-flight nil)
                                   (let ((restored
                                          (disco-room--restore-composer-operation-slot
                                           cleared-revision slot t)))
                                     (disco-room--request-render view)
                                     (message
                                      "%s%s: %s"
                                      text
                                      (if restored " (draft restored)" "")
                                      (disco-room--async-error-message
                                       error-data)))))))
                           (send-one
                             (text reply attachments-list on-success on-error)
                             (let* ((request-revision
                                     (disco-state-message-revision channel-id))
                                    (nonce (disco-room--next-send-nonce))
                                    (pending-content
                                     (if (and attachments-list
                                              (or (not (stringp text))
                                                  (string-empty-p text)))
                                         (format
                                          "Uploading %d attachment%s…"
                                          (length attachments-list)
                                          (if (= (length attachments-list) 1)
                                              ""
                                            "s"))
                                       text))
                                    leg-settled-p)
                               (disco-state-insert-pending-message
                                channel-id nonce pending-content
                                (disco-gateway-current-user-id) reply)
                               (disco-room--request-render view)
                               (cl-labels
                                   ((failure
                                      (error-data)
                                      (unless leg-settled-p
                                        (setq leg-settled-p t)
                                        (disco-state-remove-pending-message
                                         channel-id nonce)
                                        (funcall on-error error-data)))
                                    (success
                                      (response)
                                      (if (not (and (listp response)
                                                    (alist-get 'id response)))
                                          (failure
                                           (list
                                            'error
                                            "Discord create-message returned no message"))
                                        (unless leg-settled-p
                                          (setq leg-settled-p t)
                                          (disco-state-merge-message-response
                                           channel-id response request-revision
                                           nonce)
                                          (when (room-active-p)
                                            (with-current-buffer room-buffer
                                              (when (disco-room--channel-message-by-id
                                                     channel-id
                                                     (alist-get 'id response))
                                                (disco-room--observe-live-create
                                                 (alist-get 'id response)))))
                                          (funcall on-success response)))))
                                 (condition-case dispatch-error
                                     (if attachments-list
                                         (disco-api-send-message-with-attachments-async
                                          channel-id
                                          :content
                                          (and (stringp text)
                                               (not (string-empty-p text))
                                               text)
                                          :reply-to-message-id reply
                                          :allowed-mentions
                                          (and (stringp text)
                                               (not (string-empty-p text))
                                               allowed-mentions)
                                          :attachments attachments-list
                                          :nonce nonce
                                          :on-success #'success
                                          :on-error #'failure)
                                       (disco-api-send-message-async
                                        channel-id text
                                        :reply-to-message-id reply
                                        :allowed-mentions
                                        (and (stringp text)
                                             (not (string-empty-p text))
                                             allowed-mentions)
                                        :nonce nonce
                                        :on-success #'success
                                        :on-error #'failure))
                                   (error
                                    (failure dispatch-error)
                                    (signal (car dispatch-error)
                                            (cdr dispatch-error))))))))
                        (pcase long-message-action
                          ('split
                           (let* ((chunks
                                   (disco-room--split-message-content normalized))
                                  (total (length chunks)))
                             (cl-labels
                                 ((send-next
                                    (remaining sent-count)
                                    (let ((chunk (car remaining))
                                          (rest (cdr remaining))
                                          (first-p (= sent-count 0)))
                                      (send-one
                                       chunk
                                       (and first-p reply-to)
                                       (and first-p attachments)
                                       (lambda (_response)
                                         (if rest
                                             (progn
                                               (setq recovery-slot
                                                     (list
                                                      :draft
                                                      (mapconcat
                                                       #'identity rest "\n\n")
                                                      :pending-edit nil
                                                      :pending-reply-to nil
                                                      :attachment-token-seq 0
                                                      :attachment-token-entries
                                                      nil))
                                               (send-next
                                                rest (1+ sent-count)))
                                           (settle-success
                                            (format
                                             "disco: sent %d split messages"
                                             total))))
                                       (lambda (error-data)
                                         (settle-failure
                                          recovery-slot error-data
                                          (format
                                           "disco: sent %d/%d split messages"
                                           sent-count total)))))))
                               (send-next chunks 0))))
                          ('file
                           (let* ((text-attachment
                                   (disco-room--write-long-message-temp-attachment
                                    normalized))
                                  (all-attachments
                                   (append attachments
                                           (list text-attachment))))
                             (unwind-protect
                                 (send-one
                                  nil reply-to all-attachments
                                  (lambda (_response)
                                    (settle-success
                                     (format
                                      "disco: long message sent as %s"
                                      disco-room-long-message-file-name)))
                                  (lambda (error-data)
                                    (settle-failure
                                     recovery-slot error-data
                                     "disco: send failed")))
                               (ignore-errors
                                 (delete-file
                                  (plist-get text-attachment :path))))))
                          (_
                           (send-one
                            normalized reply-to attachments
                            (lambda (_response)
                              (settle-success
                               (if has-attachments
                                   "disco: message with attachment(s) sent"
                                 "disco: message sent")))
                            (lambda (error-data)
                              (settle-failure
                               recovery-slot error-data
                               "disco: send failed")))))))
                  (error
                   (unless operation-settled-p
                     (setq operation-settled-p t)
                     (when (disco-room--channel-buffer-p
                            room-buffer channel-id view)
                       (with-current-buffer room-buffer
                         (setq disco-room--send-in-flight nil)
                         (when (integerp cleared-revision)
                           (disco-room--restore-composer-operation-slot
                            cleared-revision recovery-slot t))
                         (disco-room--request-render view))))
                   (signal (car err) (cdr err))))))))))))

(defun disco-room-load-older-messages (&optional quiet)
  "Load one older page for the current room view asynchronously.

When QUIET is non-nil, suppress progress messages."
  (interactive)
  (cond
   ((disco-room--msg-filter-active-p)
    (disco-room-filter-load-more))
   ((not (appkit-chat-history-window-known-p))
    (unless quiet (message "disco: history window is not initialized")))
   ((appkit-chat-history-older-loaded-p)
    (unless quiet (message "disco: no older messages available")))
   ((appkit-chat-history-loading-p)
    (unless quiet (message "disco: history load already in progress")))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-view))
           (request-revision (disco-state-message-revision channel-id))
           (before (or (appkit-chat-history-window-first-key)
                       (user-error
                        "disco: no oldest message cursor; refresh first")))
           (request-limit (max 1 disco-message-fetch-limit))
           (owner (list :kind 'older-history
                        :channel-id channel-id
                        :cursor before)))
      (appkit-chat-history-request-begin 'older owner)
      (appkit-request-sync view :part 'frame)
      (disco-api-channel-messages-async
       channel-id
       :before before
       :limit request-limit
       :on-success
       (lambda (older)
         (when (disco-room--callback-active-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (appkit-chat-history-request-current-p owner)
               (let* ((raw-page (disco-room--normalize-history-page older))
                      (merged
                       (disco-state-merge-message-page
                        channel-id raw-page request-revision))
                      (page
                       (disco-room--history-page-retained-in-cache
                        raw-page merged))
                      (oldest (car (disco-room--history-page-bounds page)))
                      (canonical-oldest
                       (reverse (disco-room--normalize-history-page merged)))
                      (complete (< (length raw-page) request-limit))
                      (progressed
                       (and oldest
                            (disco-room--message-id-after-p
                             before oldest canonical-oldest))))
                 (appkit-chat-history-request-end owner)
                 (when progressed
                   (appkit-chat-history-window-set
                    oldest (appkit-chat-history-window-last-key)))
                 (when complete
                   (appkit-chat-history-older-loaded-set t))
                 (disco-room--request-render view)
                 (unless quiet
                   (cond
                    (progressed
                     (message "disco: loaded %d older messages"
                              (length page)))
                    (complete
                     (message "disco: reached beginning of history"))
                    (t
                     (message "disco: older history changed concurrently; retry"))))))))
         :on-error
         (lambda (err)
           (when (disco-room--callback-active-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (when (appkit-chat-history-request-current-p owner)
                 (appkit-chat-history-request-end owner)
                 (disco-room--request-render view)
                 (message "disco: older history load failed: %s"
                          (disco-room--async-error-message err))))))))))))

(defun disco-room-load-newer-messages (&optional quiet)
  "Extend a partial around-message window toward the live frontier.

When QUIET is non-nil, suppress progress messages."
  (interactive)
  (cond
   ((disco-room--msg-filter-active-p)
    (unless quiet (message "disco: newer paging is unavailable in filters")))
   ((not (appkit-chat-history-window-known-p))
    (if quiet
        nil
      (disco-room-refresh)))
   ((not (appkit-chat-history-window-partial-p))
    (unless quiet (message "disco: latest history is already loaded")))
   ((appkit-chat-history-loading-p)
    (unless quiet (message "disco: history load already in progress")))
   (t
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-view))
           (cursor (appkit-chat-history-window-last-key))
           (request-revision (disco-state-message-revision channel-id))
           (request-limit (max 1 disco-message-fetch-limit))
           (owner (list :kind 'newer-history
                        :channel-id channel-id
                        :cursor cursor
                        :frontier-at-start
                        disco-room--remote-latest-message-id)))
      (appkit-chat-history-request-begin 'newer owner)
      (appkit-request-sync view :part 'frame)
      (disco-api-channel-messages-async
       channel-id
       :after cursor
       :limit request-limit
       :on-success
       (lambda (newer)
         (when (disco-room--callback-active-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (appkit-chat-history-request-current-p owner)
               (let* ((raw-page (disco-room--normalize-history-page newer))
                      (merged
                       (disco-state-merge-message-page
                        channel-id raw-page request-revision))
                      (page
                       (disco-room--history-page-retained-in-cache
                        raw-page merged))
                      (bounds (disco-room--history-page-bounds page))
                      (newest (cdr bounds))
                      (edge (or newest cursor))
                      (canonical-oldest
                       (reverse (disco-room--normalize-history-page merged)))
                      (progressed
                       (and newest
                            (disco-room--message-id-after-p
                             newest cursor canonical-oldest)))
                      (frontier disco-room--remote-latest-message-id)
                      (edge-after-frontier
                       (and frontier edge
                            (disco-state-snowflake< frontier edge)))
                      (short-page (< (length raw-page) request-limit))
                      (finished
                       (or (equal edge frontier)
                           (and short-page
                                (or (null frontier)
                                    edge-after-frontier)))))
                 (appkit-chat-history-request-end owner)
                 (cond
                  (finished
                   (setq disco-room--remote-latest-message-id edge)
                   (appkit-chat-history-window-set
                    (appkit-chat-history-window-first-key) nil))
                  (progressed
                   (when edge-after-frontier
                     (setq disco-room--remote-latest-message-id nil))
                   (appkit-chat-history-window-set
                    (appkit-chat-history-window-first-key) newest))
                  (t
                   (appkit-chat-history-newer-stalled-set cursor)))
                 (disco-room--request-render view)
                 (unless quiet
                   (cond
                    (finished
                     (message "disco: newer history caught up"))
                    (progressed
                     (message "disco: loaded %d newer messages"
                              (length page)))
                    (t
                     (message "disco: newer history made no progress"))))))))
         :on-error
         (lambda (err)
           (when (disco-room--callback-active-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (when (appkit-chat-history-request-current-p owner)
                 (appkit-chat-history-request-end owner)
                 (disco-room--request-render view)
                 (message "disco: newer history load failed: %s"
                          (disco-room--async-error-message err))))))))))))

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
  (appkit-chatbuf-focus-input)
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
         (source-channel
          (and source-channel-id
               (disco-room--resolve-target-channel source-channel-id)))
         (normalized-content
          (and (stringp content)
               (let ((trimmed (string-trim content)))
                 (unless (string-empty-p trimmed)
                   trimmed))))
         (room-buffer (current-buffer))
         (view (disco-room--ensure-view))
         request-revision
         (allowed-mentions
          (and normalized-content (disco-room--send-allowed-mentions)))
         settled-p)
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
    (setq request-revision
          (disco-state-message-revision target-channel-id))
    (setq disco-room--send-in-flight t)
    (appkit-request-sync view :part 'frame)
    (cl-labels
        ((room-active-p
           ()
           (disco-room--channel-buffer-p room-buffer target-channel-id view))
         (finish-error
           (error-data)
           (unless settled-p
             (setq settled-p t)
             (when (room-active-p)
               (with-current-buffer room-buffer
                 (setq disco-room--send-in-flight nil)
                 (disco-room--request-render view)
                 (message "disco: forward failed: %s"
                          (disco-room--async-error-message error-data))))))
         (finish-success
           (response)
           (if (not (and (listp response) (alist-get 'id response)))
               (finish-error
                (list 'error "disco: forward response has no message id"))
             (unless settled-p
               (setq settled-p t)
               (disco-state-merge-message-response
                target-channel-id response request-revision)
               (when (room-active-p)
                 (with-current-buffer room-buffer
                   (setq disco-room--send-in-flight nil)
                   (disco-room--request-render view)
                   (message "disco: forwarded message %s from channel %s"
                            message-id source-channel-id)))))))
      (condition-case err
          (disco-api-forward-message-async
           target-channel-id
           message-id
           source-channel-id
           :content normalized-content
           :forward-only forward-only
           :allowed-mentions (and normalized-content allowed-mentions)
           :on-success #'finish-success
           :on-error #'finish-error)
        (error
         (finish-error err)
         (signal (car err) (cdr err)))))))

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

(defun disco-room--lottie-sticker-at-point ()
  "Return the native Lottie Sticker projected at point, or nil."
  (let ((sticker
         (or (get-text-property (point) 'disco-sticker-object)
             (and (> (point) (point-min))
                  (get-text-property (1- (point)) 'disco-sticker-object)))))
    (and (= (or (disco-sticker-format-type sticker) 0) 3)
         sticker)))

(defun disco-room-play-sticker-at-point ()
  "Play the native Lottie Sticker projected at point."
  (interactive)
  (if-let* ((sticker (disco-room--lottie-sticker-at-point)))
      (disco-sticker-play sticker)
    (user-error "disco: no Lottie Sticker at point")))

(defun disco-room-return-dwim ()
  "RET behavior for room buffer.

An unresolved composer token owns RET so completion cannot fall through into
an accidental send.  On a projected Lottie Sticker, RET streams its native
frames; elsewhere outside the composer, RET returns point to the draft.
Inside the composer, send-on-return sends the draft and the disabled setting
opens the draft editor."
  (interactive)
  (cond
   ((not (appkit-chatbuf-point-in-input-p))
    (if-let* ((sticker (disco-room--lottie-sticker-at-point)))
        (disco-sticker-play sticker)
      (goto-char
       (or (appkit-chatbuf-input-logical-end-position) (point-max)))))
   ((disco-company-completion-token-at-point)
    (disco-room-complete-mention))
   ((disco-room--input-option-send-on-return)
    (disco-room-send-message))
   (t
    (disco-room-edit-draft))))

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
            (channel-id disco-room--channel-id)
            (view (disco-room--ensure-view)))
        (disco-api-delete-message-async
         channel-id
         message-id
         :on-success
         (lambda (_response)
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (disco-state-delete-message channel-id message-id)
               (disco-room--repair-history-window-after-delete message-id)
               (disco-room--request-render view)
               (message "disco: deleted message %s" message-id))))
         :on-error
         (lambda (err)
           (when (disco-room--channel-buffer-p room-buffer channel-id view)
             (message "disco: delete failed for %s: %s"
                      message-id
                      (disco-room--async-error-message err)))))))))

(defun disco-room-delete-message ()
  "Delete message at point in current room."
  (interactive)
  (disco-room--delete-msg (disco-room--message-at-point)))


(transient-define-prefix disco-room-message-transient ()
  "Transient for msg-centric room actions at point."
  [["Message"
    ("c" "Copy dwim" disco-msg-copy-dwim)
    ("l" "Copy link" disco-msg-copy-link)
    ("t" "Copy text" disco-msg-copy-text)
    ("i" "Describe" disco-msg-describe-message)
    ("L" "Redisplay" disco-msg-redisplay)
    ("r" "Reply" disco-msg-reply
     :if-not disco-room--reply-unavailable-reason)
    ("f" "Forward" disco-msg-forward
     :if-not disco-room--forward-unavailable-reason)
    ("e" "Edit" disco-msg-edit
     :if-not (lambda ()
               (disco-room--edit-start-unavailable-reason
                (disco-room-menu--message-at-point))))
    ("d" "Delete" disco-msg-delete
     :if-not (lambda ()
               (disco-room--delete-message-unavailable-reason
                (disco-room-menu--message-at-point))))
    ("P" "Pin / unpin" disco-msg-toggle-pin
     :if-not (lambda ()
               (disco-room--pin-message-unavailable-reason
                (disco-room-menu--message-at-point))))
    ("!" "Add reaction" disco-msg-add-reaction
     :if-not disco-room--reaction-unavailable-reason)
    ("+" "Toggle reaction" disco-msg-toggle-reaction
     :if-not disco-room--reaction-unavailable-reason)
    ("-" "Remove reaction" disco-msg-remove-reaction
     :if-not disco-room--reaction-unavailable-reason)
    ("T" "Open thread" disco-msg-open-thread
     :if-not disco-room-thread--open-from-message-unavailable-reason)]
   ["Poll"
    ("p" "Poll actions…" disco-room-poll-transient
     :if disco-room-poll-actionable-at-point-p)]
   ["Media"
    ("o" "Open / play" appkit-media-card-open
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'open)))
    ("D" "Download / retry" appkit-media-card-download
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'download)))
    ("C" "Cancel download" appkit-media-card-cancel-download
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'cancel)))
    ("s" "Save as" appkit-media-card-save-as
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'save-as)))
    ("y" "Copy media URL" appkit-media-card-copy-url
     :if-not (lambda () (appkit-media-card-action-inapt-reason 'copy-url)))]])

(defun disco-room--operate-msg (_msg)
  "Open the message transient for the current room.

_MSG is ignored because the transient resolves availability from point."
  (call-interactively #'disco-room-message-transient))

(defun disco-room-menu--message-at-point ()
  "Return message at point, suppressing user errors for menu checks."
  (ignore-errors (disco-room--message-at-point)))

(transient-define-prefix disco-room-transient ()
  "Room command menu for disco.el."
  [["Timeline"
    ("g" "Refresh room" disco-room-refresh)
    ("o" "Message actions..." disco-room-message-transient
     :if disco-room-menu--message-at-point)
    ("c" "Send message" disco-room-send-message
     :if-not disco-room--send-message-unavailable-reason)
    ("f" "Attach file" disco-room-attach-file
     :if-not disco-room--attach-unavailable-reason)
    ("D" "Remove attachment" disco-room-remove-attachment-token-at-point
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("x" "Clear attachments" disco-room-clear-attachments
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("v" "List attachments" disco-room-list-attachments
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("V" "Edit attachment desc" disco-room-edit-attachment-description
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 1)))
    ("O" "Reorder attachments" disco-room-reorder-attachments
     :if-not (lambda ()
               (disco-room--attachment-token-action-unavailable-reason 2)))
    ("k" "Cancel reply/edit" disco-room-cancel-reply
     :if disco-room--composer-aux-active-p)
    ("p" "Send poll" disco-room-send-poll
     :if-not disco-room--poll-unavailable-reason)
    ("i" "Send Sticker" disco-room-send-sticker
     :if-not disco-room--sticker-unavailable-reason)
    ("B" "Browse pinned msgs" disco-room-list-pinned-messages)
    ("P" "Ack pinned msgs" disco-room-ack-channel-pins)]
   ["Thread"
    ("m" "Create from message" disco-room-thread-create-from-message
     :if-not disco-room-thread--create-from-message-unavailable-reason)
    ("n" "Create detached" disco-room-thread-create
     :if-not (lambda ()
               (disco-room-thread--create-unavailable-reason :any)))
    ("R" "Rename thread" disco-room-thread-rename
     :if-not disco-room-thread--update-unavailable-reason)
    ("L" "Toggle locked" disco-room-thread-toggle-locked
     :if-not disco-room-thread--update-unavailable-reason)
    ("S" "Set slowmode" disco-room-thread-set-slowmode
     :if-not disco-room-thread--update-unavailable-reason)
    ("U" "Set auto-archive" disco-room-thread-set-auto-archive-duration
     :if-not disco-room-thread--update-unavailable-reason)
    ("E" "Edit thread settings" disco-room-thread-edit-settings
     :if-not disco-room-thread--update-unavailable-reason)
    ("M" "Set muted" disco-room-thread-set-muted
     :if-not disco-room-thread--mute-unavailable-reason)
    ("j" "Join thread" disco-room-thread-join
     :if-not disco-room-thread--join-unavailable-reason)
    ("l" "Leave thread" disco-room-thread-leave
     :if-not disco-room-thread--leave-unavailable-reason)
    ("a" "Toggle archived" disco-room-thread-toggle-archived
     :if-not disco-room-thread--toggle-archived-unavailable-reason)
    ("A" "Parent archived threads..." disco-room-thread-open-parent-archived
     :if (lambda ()
           (alist-get 'parent_id (disco-room--channel-object))))]
   ["Inspect"
    ("/" "Structured search..." disco-room-search-channel)
    ("f" "Filter search" disco-room-filter-search)
    ("F" "Cancel filter" disco-room-filter-cancel)
    ("v" "Refetch avatars" disco-avatar-refetch)
    ("H" "HTTP queue" disco-http-describe-queue)
    ("R" "Rate limits" disco-api-describe-rate-limits)
    ("G" "Gateway status" disco-gateway-describe-status)]
   ["Window"
    ("q" "Quit window" quit-window)]])

(defvar-keymap disco-room-mode-map
  :doc "Keymap for `disco-room-mode'."
  "C-l" #'recenter-top-bottom
  "TAB" #'disco-room-complete-mention
  "<tab>" #'disco-room-complete-mention
  "C-M-i" #'disco-room-complete-mention
  "C-c g" #'disco-room-refresh
  "C-c m" disco-room-message-prefix-map
  "RET" #'disco-room-return-dwim
  "M-RET" #'disco-room-input-preview
  "C-c '" #'disco-room-edit-draft
  "M-p" #'disco-room-draft-prev
  "M-n" #'disco-room-draft-next
  "M-r" #'disco-room-draft-history-search
  "M-g s" #'disco-room-inplace-search
  "M-g n" #'disco-room-inplace-search-next
  "M-g p" #'disco-room-inplace-search-prev
  "C-c C-r" #'disco-room-inplace-search-query
  "C-c C-s" #'disco-room-inplace-search-query-forward
  "C-c /" #'disco-room-filter-search
  "C-c C-c" #'disco-room-filter-cancel
  "C-c M-/" #'disco-room-search-channel
  "C-c C-p s" #'disco-room-send-poll
  "C-c C-p +" #'disco-room-vote-poll-answer
  "C-c C-p -" #'disco-room-remove-poll-vote
  "C-c C-p t" #'disco-room-toggle-poll-answer
  "C-c C-p v" #'disco-room-submit-poll-vote
  "C-c C-p c" #'disco-room-clear-poll-votes
  "C-c C-p e" #'disco-room-expire-poll
  "C-c M-p" #'disco-room-list-pinned-messages
  "C-c C-P" #'disco-room-ack-channel-pins
  "C-c RET" #'disco-room-send-message
  "C-c C-a" #'disco-room-attach
  "C-c C-f" #'disco-room-attach-file
  "C-c C-i" #'disco-room-send-sticker
  "C-c C-o" #'disco-room-input-options-transient
  "C-c C-d" #'disco-room-remove-attachment-token-at-point
  "C-c C-x" #'disco-room-clear-attachments
  "C-c M-l" #'disco-room-list-attachments
  "C-c M-e" #'disco-room-edit-attachment-description
  "C-c M-r" #'disco-room-reorder-attachments
  "C-c C-k" #'disco-room-cancel-reply
  "ESC ESC" #'disco-room-cancel-reply
  "C-M-c" #'disco-room-cancel-reply
  "C-c C-g" #'disco-room-jump-to-message
  "C-c C-w" #'disco-room-toggle-breakline
  "C-c C-t m" #'disco-room-thread-create-from-message
  "C-c C-t o" #'disco-room-thread-open-from-message-at-point
  "C-c C-t c" #'disco-room-thread-create
  "C-c C-t r" #'disco-room-thread-rename
  "C-c C-t k" #'disco-room-thread-toggle-locked
  "C-c C-t s" #'disco-room-thread-set-slowmode
  "C-c C-t a" #'disco-room-thread-toggle-archived
  "C-c C-t A" #'disco-room-thread-set-auto-archive-duration
  "C-c C-t e" #'disco-room-thread-edit-settings
  "C-c C-t u" #'disco-room-thread-set-muted
  "C-c C-j" #'disco-room-thread-join
  "C-c C-l" #'disco-room-thread-leave
  "C-c M-v" #'disco-avatar-refetch
  "C-c ?" #'disco-room-transient)

(defun disco-room--reset-view-local-state (&optional channel-id channel-name)
  "Reset controller state owned by one room view.

CHANNEL-ID and CHANNEL-NAME bind a newly attached replacement view.  This is
separate from major-mode initialization because an Appkit view can die while
its same-mode buffer survives."
  (disco-room--detach-live-updates)
  (when (fboundp 'disco-company--teardown-room-buffer)
    (disco-company--teardown-room-buffer))
  (disco-room--typing-cancel-expire-timer)
  (appkit-chatbuf-reset-state disco-room-input-history-size)
  (appkit-chat-history-reset-state)
  (setq-local disco-room--channel-id channel-id)
  (setq-local disco-room--channel-name channel-name)
  (let ((channel (and channel-id (disco-state-channel channel-id))))
    (setq-local disco-room--guild-id
                (and channel (alist-get 'guild_id channel))))
  (setq-local disco-room--remote-latest-message-id nil)
  (setq-local disco-room--oldest-message-id nil)
  (setq-local disco-room--newest-message-id nil)
  (disco-room--set-composer-aux-state nil nil)
  (disco-room--sync-shared-input-options-state)
  (setq-local disco-room--send-in-flight nil)
  (setq-local disco-room--sticker-picker-pending nil)
  (setq-local disco-room--pending-jump-message-id nil)
  (setq-local disco-room--last-search-query nil)
  (setq-local disco-room--msg-filter nil)
  (setq-local disco-msg-resolve-function #'disco-room--resolve-message)
  (setq-local disco-msg-content-text-function #'disco-room--message-copy-text)
  (setq-local disco-msg-reply-function #'disco-room--reply-to-msg)
  (setq-local disco-msg-forward-function #'disco-room--forward-msg)
  (setq-local disco-msg-operate-function #'disco-room--operate-msg)
  (setq-local disco-msg-edit-function #'disco-room--edit-msg)
  (setq-local disco-msg-delete-function #'disco-room--delete-msg)
  (setq-local disco-msg-toggle-pin-function #'disco-room--toggle-pin-on-msg)
  (setq-local disco-msg-open-thread-function #'disco-room-thread-open-from-message)
  (setq-local disco-msg-toggle-reaction-function #'disco-room--toggle-reaction-on-msg)
  (setq-local disco-msg-add-reaction-function #'disco-room--add-reaction-to-msg)
  (setq-local disco-msg-remove-reaction-function #'disco-room--remove-reaction-from-msg)
  (setq-local disco-msg-redisplay-function #'disco-room--redisplay-msg)
  (setq-local appkit-media-card-fallback-context-function
              #'disco-room--media-card-fallback-context)
  (setq-local disco-room--filter-generation 0)
  (setq-local disco-room--filter-in-flight nil)
  (setq-local disco-room--inplace-search-filter nil)
  (setq-local disco-room--inplace-search-generation 0)
  (setq-local disco-room--pending-attachments nil)
  (setq-local disco-room--attachment-token-table (make-hash-table :test #'equal))
  (setq-local disco-room--attachment-token-seq 0)
  (setq-local disco-room--typing-users (make-hash-table :test #'equal))
  (setq-local disco-room--typing-expire-timer nil)
  (disco-room-poll-reset)
  (disco-room-reaction-reset)
  (setq-local disco-room--pin-op-seq 0)
  (setq-local disco-room--pin-ops (make-hash-table :test #'equal))
  (setq-local disco-room--revealed-spoiler-message-id nil)
  (setq-local disco-room--optimistic-read-ack-seq 0)
  (setq-local disco-room--pending-optimistic-read-ack nil)
  (setq-local disco-room--pins-ack-seq 0)
  (setq-local disco-room--gateway-handler nil)
  (setq-local disco-room--live-update-handle nil)
  (funcall #'disco-company-setup-room-buffer)
  (when (disco-current-token)
    (disco-sticker-ensure-ready disco-room--guild-id)))

(define-derived-mode disco-room-mode appkit-chatbuf-mode "Disco-Room"
  "Major mode for disco.el room buffers."
  (disco-room--apply-breakline-settings)
  ;; Avoid visible seams between vertically sliced inline images.
  (setq-local line-spacing 0)
  ;; Strip visual-only line prefixes from copied text.
  (setq-local filter-buffer-substring-function
              #'disco-room--buffer-substring-filter)
  (disco-room--reset-view-local-state)
  (setq-local appkit-chatbuf-input-sync-function
              #'disco-room--sync-draft-from-buffer)
  (add-hook 'text-scale-mode-hook #'disco-room--on-text-scale-change nil t)
  (add-hook 'post-command-hook #'disco-room--post-command t t)
  (add-hook 'window-scroll-functions #'disco-room--window-scroll nil t)
  (appkit-chatbuf-use-timeline-mode #'disco-room-timeline-mode))

(defun disco-room-open (channel-id channel-name)
  "Open room for CHANNEL-ID with CHANNEL-NAME and return its actual buffer."
  (let* ((app (disco-runtime-app))
         (view-id (list 'room channel-id))
         (existing (appkit-view-for-id app view-id))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode 'disco-room-mode
           :buffer-name (disco-room--buffer-name channel-name channel-id)
           :state channel-id
           :sync-function #'disco-room--sync-invalidations
           :parts '(frame timeline composer geometry)
           :setup
           (lambda (_view)
             ;; The buffer may outlive a killed predecessor view.  SETUP runs
             ;; only for a new attachment, so live view reuse keeps its draft,
             ;; history window, controller generations, and request ownership.
             (disco-room--reset-view-local-state channel-id channel-name))))
         (buf (appkit-view-buffer view)))
    (with-current-buffer buf
      (setq disco-room--channel-id channel-id)
      (setq disco-room--channel-name channel-name)
      (let ((channel (disco-state-channel channel-id)))
        (setq disco-room--guild-id (and channel (alist-get 'guild_id channel))))
      (disco-room--attach-live-updates)
      (unless existing
        (disco-room-refresh)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (appkit-view-refresh-responsive-geometry))
    buf))

(provide 'disco-room)

;;; disco-room.el ends here
