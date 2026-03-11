;;; disco-msg.el --- Message model helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared helpers for Discord message identity/reference access.

;;; Code:

(require 'pp)
(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'thingatpt)
(require 'disco-markdown)
(require 'disco-state)
(require 'disco-util)

(defconst disco-msg--reference-field-map
  '((id . message_id)
    (channel-id . channel_id)
    (guild-id . guild_id))
  "Declarative mapping from reference field role to payload key.")

(defvar-local disco-msg-resolve-function nil
  "Buffer-local function resolving a message by id at point.

When non-nil it is called as (FUNCTION MESSAGE-ID CHANNEL-ID POSITION) and
should return the latest cached message object for that buffer context.")

(defvar-local disco-msg-content-text-function nil
  "Buffer-local function returning copy-ready text for a message.

When non-nil it is called with one argument, the message object, and should
return a string or nil.")

(defvar-local disco-msg-reply-function nil
  "Buffer-local function beginning a reply for a message.")

(defvar-local disco-msg-forward-function nil
  "Buffer-local function beginning a forward flow for a message.")

(defvar-local disco-msg-operate-function nil
  "Buffer-local function opening a message actions menu for a message.")

(defvar-local disco-msg-edit-function nil
  "Buffer-local function beginning an edit flow for a message.")

(defvar-local disco-msg-delete-function nil
  "Buffer-local function deleting a message.")

(defvar-local disco-msg-open-thread-function nil
  "Buffer-local function opening a starter thread for a message.")

(defvar-local disco-msg-toggle-reaction-function nil
  "Buffer-local function toggling a reaction on a message.")

(defvar-local disco-msg-add-reaction-function nil
  "Buffer-local function adding a reaction to a message.")

(defvar-local disco-msg-remove-reaction-function nil
  "Buffer-local function removing a reaction from a message.")

(defvar-local disco-msg-redisplay-function nil
  "Buffer-local function forcing a message redisplay in the current view.")

(defvar-local disco-msg--inspect-message-id nil
  "Message id shown by the current msg inspect buffer.")

(defvar-local disco-msg--inspect-channel-id nil
  "Channel id shown by the current msg inspect buffer.")

(defvar-local disco-msg--inspect-guild-id nil
  "Guild id shown by the current msg inspect buffer.")

(defvar disco-msg-command-map
  (let ((map (make-sparse-keymap)))
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
    (define-key map (kbd "t") #'disco-msg-copy-text)
    (define-key map (kbd "L") #'disco-msg-redisplay)
    (define-key map (kbd "!") #'disco-msg-add-reaction)
    (define-key map (kbd "+") #'disco-msg-toggle-reaction)
    (define-key map (kbd "-") #'disco-msg-remove-reaction)
    (define-key map (kbd "T") #'disco-msg-open-thread)
    map)
  "Default command map applied to rendered message spans.")

(defun disco-msg-apply-command-map (start end)
  "Apply `disco-msg-command-map' between START and END where no keymap exists."
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-char-property-change pos 'keymap nil end)
                      end)))
        (unless (get-text-property pos 'keymap)
          (add-text-properties pos next (list 'keymap disco-msg-command-map)))
        (setq pos next)))))

(defun disco-msg--message-start-positions ()
  "Return visible message start positions in the current buffer."
  (let ((pos (point-min))
        starts)
    (while (< pos (point-max))
      (when (get-text-property pos 'disco-message-id)
        (push pos starts))
      (setq pos (or (next-single-char-property-change
                     pos 'disco-message-id nil (point-max))
                    (point-max))))
    (nreverse starts)))

(defun disco-msg--message-start-at-point (&optional pos)
  "Return message start position containing POS, or nil."
  (let ((position (or pos (point)))
        found)
    (dolist (start (disco-msg--message-start-positions) found)
      (when (<= start position)
        (setq found start)))))

(defun disco-msg--goto-message-start (position)
  "Move point to POSITION and ensure it is visible."
  (goto-char position)
  (when-let* ((win (get-buffer-window (current-buffer) t)))
    (set-window-point win position)))

(defun disco-msg--call-adapter (adapter message action)
  "Call ADAPTER with MESSAGE or signal an ACTION-specific user error."
  (unless (functionp adapter)
    (user-error "disco: %s is unavailable in this buffer" action))
  (funcall adapter message))

(defun disco-msg--event-point (event)
  "Return buffer position encoded by mouse EVENT, or nil."
  (when event
    (condition-case nil
        (and (mouse-event-p event)
             (posn-point (event-start event)))
      (error nil))))

(defun disco-msg--property-probe-positions (&optional pos)
  "Return candidate positions to probe message properties around POS."
  (let* ((position (or pos (point)))
         (line-beg (save-excursion
                     (goto-char position)
                     (line-beginning-position))))
    (delete-dups
     (delq nil
           (list position
                 (and (> position (point-min)) (1- position))
                 line-beg)))))

(defun disco-msg--text-property-any (property &optional pos)
  "Return PROPERTY found around POS, or nil."
  (seq-some (lambda (probe)
              (get-text-property probe property))
            (disco-msg--property-probe-positions pos)))

(defun disco-msg-ref-at (&optional pos)
  "Return message reference plist at POS, or nil.

The returned plist contains `:message-id', `:channel-id' and `:guild-id'
when available from buffer text properties."
  (when-let* ((message-id
               (disco-msg-normalize-id
                (disco-msg--text-property-any 'disco-message-id pos))))
    (list :message-id message-id
          :channel-id
          (disco-msg-normalize-id
           (disco-msg--text-property-any 'disco-message-channel-id pos))
          :guild-id
          (disco-msg-normalize-id
           (disco-msg--text-property-any 'disco-message-guild-id pos)))))

(defun disco-msg-at (&optional pos msg-predicate)
  "Return current message at POS, or nil.

If MSG-PREDICATE is non-nil, return the message only when it satisfies the
predicate."
  (when-let* ((ref (disco-msg-ref-at pos))
              (message-id (plist-get ref :message-id)))
    (let* ((channel-id (plist-get ref :channel-id))
           (msg (or (and (functionp disco-msg-resolve-function)
                         (funcall disco-msg-resolve-function
                                  message-id channel-id pos))
                    (and channel-id
                         (disco-msg-find-in-channel channel-id message-id)))))
      (when (or (null msg-predicate)
                (and msg (funcall msg-predicate msg)))
        msg))))

(defun disco-msg-for-interactive ()
  "Return message at mouse event or current point, or signal a user error."
  (or (disco-msg-at (disco-msg--event-point last-input-event))
      (disco-msg-at (point))
      (user-error "disco: point is not on a message")))

(defun disco-msg-next (&optional n)
  "Move point to the Nth next visible message."
  (interactive "p")
  (let* ((count (max 1 (or n 1)))
         (starts (disco-msg--message-start-positions))
         (current (disco-msg--message-start-at-point))
         (target (if current
                     (nth count (member current starts))
                   (seq-find (lambda (start)
                               (> start (point)))
                             starts))))
    (unless target
      (user-error "disco: no next message"))
    (disco-msg--goto-message-start target)))

(defun disco-msg-previous (&optional n)
  "Move point to the Nth previous visible message."
  (interactive "p")
  (let* ((count (max 1 (or n 1)))
         (starts (disco-msg--message-start-positions))
         (current (disco-msg--message-start-at-point))
         (candidates (if current
                         (seq-take-while (lambda (start)
                                           (< start current))
                                         starts)
                       (seq-take-while (lambda (start)
                                         (< start (point)))
                                       starts)))
         (target (nth (1- count) (reverse candidates))))
    (unless target
      (user-error "disco: no previous message"))
    (disco-msg--goto-message-start target)))

(defun disco-msg-normalize-id (value)
  "Return normalized snowflake-like ID string from VALUE, or nil.

String IDs are trimmed; integer IDs are stringified. Empty values are
rejected."
  (let ((normalized
         (cond
          ((stringp value) (string-trim value))
          ((integerp value) (number-to-string value))
          (t nil))))
    (when (and (stringp normalized)
               (not (string-empty-p normalized)))
      normalized)))

(defun disco-msg-id (message)
  "Return normalized message ID for MESSAGE, or nil."
  (disco-msg-normalize-id (and (listp message) (alist-get 'id message))))

(defun disco-msg-channel-id (message)
  "Return normalized channel ID for MESSAGE, or nil."
  (disco-msg-normalize-id (and (listp message) (alist-get 'channel_id message))))

(defun disco-msg-guild-id (message)
  "Return normalized guild ID for MESSAGE, or nil.

When MESSAGE itself has no `guild_id', infer it from the cached channel when
possible."
  (or (disco-msg-normalize-id (and (listp message) (alist-get 'guild_id message)))
      (when-let* ((channel-id (disco-msg-channel-id message))
                  (channel (disco-state-channel channel-id)))
        (disco-msg-normalize-id (alist-get 'guild_id channel)))))

(defun disco-msg-link (message &optional channel-id guild-id)
  "Return Discord permalink for MESSAGE, or nil when unavailable."
  (when-let* ((message-id (disco-msg-id message))
              (resolved-channel-id
               (disco-msg-normalize-id (or channel-id
                                           (disco-msg-channel-id message)))))
    (format "https://discord.com/channels/%s/%s/%s"
            (or (disco-msg-normalize-id
                 (or guild-id (disco-msg-guild-id message)))
                "@me")
            resolved-channel-id
            message-id)))

(defun disco-msg-reference (message)
  "Return message_reference object from MESSAGE, or nil."
  (let ((reference (and (listp message) (alist-get 'message_reference message))))
    (and (listp reference) reference)))

(defun disco-msg-reference-field (message field-role)
  "Return normalized reference field FIELD-ROLE from MESSAGE.

FIELD-ROLE is one of `id', `channel-id', or `guild-id'."
  (let* ((reference (disco-msg-reference message))
         (field-key (alist-get field-role disco-msg--reference-field-map)))
    (disco-msg-normalize-id
     (and reference field-key (alist-get field-key reference)))))

(defun disco-msg-reference-id (message)
  "Return referenced message ID from MESSAGE, or nil."
  (disco-msg-reference-field message 'id))

(defun disco-msg-reference-channel-id (message)
  "Return referenced channel ID from MESSAGE, or nil."
  (disco-msg-reference-field message 'channel-id))

(defun disco-msg-reference-guild-id (message)
  "Return referenced guild ID from MESSAGE, or nil."
  (disco-msg-reference-field message 'guild-id))

(defun disco-msg-find-in-messages (messages message-id)
  "Return message object with MESSAGE-ID from MESSAGES, or nil.

ID comparison is normalized for snowflake strings."
  (let ((normalized-target-id (disco-msg-normalize-id message-id)))
    (when normalized-target-id
      (seq-find
       (lambda (message)
         (equal (disco-msg-id message) normalized-target-id))
       (or messages '())))))

(defun disco-msg-find-in-channel (channel-id message-id)
  "Return cached MESSAGE-ID from CHANNEL-ID, or nil."
  (let ((normalized-channel-id (disco-msg-normalize-id channel-id)))
    (when normalized-channel-id
      (disco-msg-find-in-messages
       (disco-state-messages normalized-channel-id)
       message-id))))

(defun disco-msg-author-display-name (message)
  "Return best-effort author display name for MESSAGE."
  (let* ((author (and (listp message) (alist-get 'author message)))
         (member (and (listp message) (alist-get 'member message))))
    (or (and (listp member)
             (stringp (alist-get 'nick member))
             (not (string-empty-p (alist-get 'nick member)))
             (alist-get 'nick member))
        (and (listp author)
             (stringp (alist-get 'global_name author))
             (not (string-empty-p (alist-get 'global_name author)))
             (alist-get 'global_name author))
        (and (listp author)
             (stringp (alist-get 'username author))
             (not (string-empty-p (alist-get 'username author)))
             (alist-get 'username author))
        (and (listp author)
             (disco-msg-normalize-id (alist-get 'id author))))))

(defun disco-msg-preview-content (message)
  "Return compact one-line content preview for MESSAGE."
  (let* ((content (and (listp message)
                       (stringp (alist-get 'content message))
                       (string-trim (alist-get 'content message))))
         (attachments (or (and (listp message)
                               (alist-get 'attachments message))
                          '()))
         (embeds (or (and (listp message)
                          (alist-get 'embeds message))
                     '()))
         (poll (and (listp message)
                    (alist-get 'poll message)))
         (sticker-items (or (and (listp message)
                                 (alist-get 'sticker_items message))
                            '())))
    (cond
     ((and (stringp content)
           (not (string-empty-p content)))
      (replace-regexp-in-string "[\n\r\t ]+" " " content))
     ((and (listp poll) poll)
      "(poll)")
     ((> (length attachments) 0)
      (format "(%d attachment%s)"
              (length attachments)
              (if (= (length attachments) 1) "" "s")))
     ((> (length embeds) 0)
      "(embed)")
     ((> (length sticker-items) 0)
      "(sticker)")
     (t
      "(message)"))))

(defun disco-msg-preview-line (message)
  "Return compact single-line preview label for MESSAGE."
  (let ((author (disco-msg-author-display-name message))
        (content (disco-msg-preview-content message)))
    (if (and author (not (string-empty-p author)))
        (format "%s> %s" author content)
      content)))

(defun disco-msg-channel-last-cached-message (channel)
  "Return last cached message object for CHANNEL, or nil."
  (let* ((channel-id (and (listp channel)
                          (disco-msg-normalize-id (alist-get 'id channel))))
         (last-message-id (and (listp channel)
                               (disco-msg-normalize-id
                                (alist-get 'last_message_id channel))))
         (messages (and channel-id
                        (disco-state-messages channel-id))))
    (or (and last-message-id
             (disco-msg-find-in-messages messages last-message-id))
        (car messages))))

(defun disco-msg-channel-preview-line (channel)
  "Return best-effort cached preview line for CHANNEL, or nil."
  (when-let* ((message (disco-msg-channel-last-cached-message channel)))
    (disco-msg-preview-line message)))

(defun disco-msg-content-text (message)
  "Return copy-ready text content for MESSAGE, or nil."
  (or (and (functionp disco-msg-content-text-function)
           (funcall disco-msg-content-text-function message))
      (let* ((raw-content (and (listp message) (alist-get 'content message)))
             (exported (and (stringp raw-content)
                            (disco-markdown-copy-export
                             raw-content
                             :context 'message-copy
                             :message message
                             :spoiler-message-id (disco-msg-id message)
                             :reveal-spoilers t))))
        (when (and (stringp exported)
                   (not (string-empty-p
                         (string-trim (substring-no-properties exported)))))
          exported))))

(defun disco-msg--property-value-at-point (property &optional pos)
  "Return PROPERTY value around POS, or nil."
  (let ((positions (disco-msg--property-probe-positions pos))
        value)
    (while (and positions (null value))
      (setq value (get-text-property (car positions) property))
      (setq positions (cdr positions)))
    value))

(defun disco-msg--bounds-of-property-at-point (property &optional pos)
  "Return contiguous bounds for PROPERTY around POS, or nil."
  (let* ((position (or pos (point)))
         (probe (seq-find (lambda (candidate)
                            (get-text-property candidate property))
                          (disco-msg--property-probe-positions position))))
    (when probe
      (cons (or (previous-single-char-property-change
                 (1+ probe) property nil (point-min))
                (point-min))
            (or (next-single-char-property-change probe property nil (point-max))
                (point-max))))))

(defun disco-msg--url-at-point (&optional pos)
  "Return URL under POS, preferring rendered Markdown link properties."
  (or (disco-msg--property-value-at-point 'disco-markdown-url pos)
      (save-excursion
        (goto-char (or pos (point)))
        (thing-at-point-url-at-point))))

(defun disco-msg--code-bounds-at-point (&optional pos)
  "Return code span bounds around POS, or nil."
  (disco-msg--bounds-of-property-at-point 'disco-markdown-code pos))

(defun disco-msg-copy-link (message)
  "Copy Discord permalink for MESSAGE into the kill ring."
  (interactive (list (disco-msg-for-interactive)))
  (let ((link (disco-msg-link message)))
    (unless (and (stringp link) (not (string-empty-p link)))
      (user-error "disco: message link is unavailable"))
    (kill-new link)
    (message "disco: copied message link %s" link)))

(defun disco-msg-copy-text (message &optional no-properties)
  "Copy copy-ready text for MESSAGE into the kill ring.

With NO-PROPERTIES non-nil, strip text properties before copying."
  (interactive (list (disco-msg-for-interactive)
                     current-prefix-arg))
  (let ((text (disco-msg-content-text message)))
    (unless text
      (user-error "disco: nothing to copy"))
    (kill-new (if no-properties
                  (substring-no-properties text)
                text))
    (message "disco: copied message text (%d chars)" (length text))))

(defun disco-msg-copy-dwim (message &optional no-properties)
  "Copy text at point in a telega-style DWIM manner.

If the region is active, copy it.  If point is on a URL, copy the URL.  If
point is inside a code span or code block, copy that code.  Otherwise copy the
message text for MESSAGE.  With NO-PROPERTIES non-nil, strip text properties
before copying."
  (interactive (list (disco-msg-for-interactive)
                     current-prefix-arg))
  (let* ((code-bounds (and (not (region-active-p))
                           (disco-msg--code-bounds-at-point)))
         (url (and (not (region-active-p))
                   (not code-bounds)
                   (disco-msg--url-at-point)))
         (text (cond
                ((region-active-p)
                 (prog1
                     (buffer-substring (region-beginning) (region-end))
                   (deactivate-mark)))
                ((and (stringp url) (not (string-empty-p url)))
                 url)
                (code-bounds
                 (buffer-substring (car code-bounds) (cdr code-bounds)))
                (t nil))))
    (if text
        (let ((copied (if no-properties
                          (substring-no-properties text)
                        text)))
          (kill-new copied)
          (message "disco: copied text (%d chars)" (length copied)))
      (disco-msg-copy-text message no-properties))))

(defun disco-msg-reply (message)
  "Begin replying to MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-reply-function message "replying to messages"))

(defun disco-msg-forward (message)
  "Begin forwarding MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-forward-function message "forwarding messages"))

(defun disco-msg-operate (message)
  "Open message actions menu for MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-operate-function message "message actions"))

(defun disco-msg--inspect-buffer-name (message)
  "Return inspect buffer name for MESSAGE."
  (format "*disco-message:%s*"
          (or (disco-msg-id message) "unknown")))

(defun disco-msg--inspect-buffer-message ()
  "Return current inspect-buffer message object, or nil."
  (and disco-msg--inspect-message-id
       disco-msg--inspect-channel-id
       (disco-msg-find-in-channel disco-msg--inspect-channel-id
                                  disco-msg--inspect-message-id)))

(defun disco-msg--render-inspect-buffer ()
  "Render current message inspect buffer."
  (let* ((message (or (disco-msg--inspect-buffer-message)
                      (user-error "disco: inspect buffer has no message context")))
         (message-id (disco-msg-id message))
         (channel-id (or (disco-msg-channel-id message) disco-msg--inspect-channel-id))
         (guild-id (or (disco-msg-guild-id message) disco-msg--inspect-guild-id))
         (title (or (disco-msg-preview-line message)
                    message-id
                    "(unknown message)"))
         (copy-text (ignore-errors (disco-msg-content-text message)))
         (inhibit-read-only t))
    (erase-buffer)
    (insert title "\n")
    (insert (make-string (length title) ?=) "\n\n")
    (when message-id
      (insert (format "Message ID: %s\n" message-id)))
    (when channel-id
      (insert (format "Channel ID: %s\n" channel-id)))
    (when guild-id
      (insert (format "Guild ID: %s\n" guild-id)))
    (insert (format "Type: %s\n" (disco-msg-type message)))
    (when-let* ((timestamp (alist-get 'timestamp message)))
      (insert (format "Timestamp: %s\n" timestamp)))
    (when-let* ((link (disco-msg-link message channel-id guild-id)))
      (insert (format "Link: %s\n" link)))
    (when (and (stringp copy-text)
               (not (string-empty-p (substring-no-properties copy-text))))
      (insert "\nCopy Text:\n\n")
      (insert (substring-no-properties copy-text) "\n"))
    (insert "\nRaw message object:\n\n")
    (insert (pp-to-string message))
    (goto-char (point-min))))

(defun disco-msg-inspect-refresh ()
  "Refresh the current message inspect buffer from local state."
  (interactive)
  (unless disco-msg--inspect-message-id
    (user-error "disco: inspect buffer has no message context"))
  (disco-msg--render-inspect-buffer)
  (message "disco: refreshed message inspect"))

(defvar disco-msg-inspect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'disco-msg-inspect-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `disco-msg-inspect-mode'.")

(define-derived-mode disco-msg-inspect-mode special-mode "Disco-Message"
  "Major mode for message inspect buffers."
  (setq buffer-read-only t)
  (setq truncate-lines nil))

(defun disco-msg-describe-message (message)
  "Open an inspect buffer describing MESSAGE.

Return the inspect buffer."
  (interactive (list (disco-msg-for-interactive)))
  (let ((buf (get-buffer-create (disco-msg--inspect-buffer-name message))))
    (with-current-buffer buf
      (disco-msg-inspect-mode)
      (setq disco-msg--inspect-message-id (disco-msg-id message))
      (setq disco-msg--inspect-channel-id (disco-msg-channel-id message))
      (setq disco-msg--inspect-guild-id (disco-msg-guild-id message))
      (disco-msg--render-inspect-buffer))
    (pop-to-buffer buf)
    buf))

(defun disco-msg-edit (message)
  "Begin editing MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-edit-function message "editing messages"))

(defun disco-msg-delete (message)
  "Delete MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-delete-function message "deleting messages"))

(defun disco-msg-open-thread (message)
  "Open starter thread associated with MESSAGE."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-open-thread-function message "opening threads"))

(defun disco-msg-toggle-reaction (message)
  "Toggle a reaction on MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-toggle-reaction-function
                           message
                           "toggling reactions"))

(defun disco-msg-add-reaction (message)
  "Add a reaction to MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-add-reaction-function
                           message
                           "adding reactions"))

(defun disco-msg-remove-reaction (message)
  "Remove a reaction from MESSAGE in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-remove-reaction-function
                           message
                           "removing reactions"))

(defun disco-msg-redisplay (message)
  "Force MESSAGE to be redisplayed in the current buffer context."
  (interactive (list (disco-msg-for-interactive)))
  (disco-msg--call-adapter disco-msg-redisplay-function message "redisplaying messages"))

(defun disco-msg-time (message)
  "Return decoded timestamp for MESSAGE, or nil when unavailable."
  (let ((raw (and (listp message) (alist-get 'timestamp message))))
    (when (and (stringp raw)
               (not (string-empty-p raw)))
      (condition-case _
          (date-to-time raw)
        (error nil)))))

(defun disco-msg-time-epoch (message)
  "Return float epoch seconds for MESSAGE timestamp, or nil."
  (let ((time (disco-msg-time message)))
    (and time (float-time time))))

(defun disco-msg-day-key (message)
  "Return local calendar day key for MESSAGE timestamp, or nil."
  (let ((time (disco-msg-time message)))
    (and time (format-time-string "%Y-%m-%d" time))))

(defun disco-msg-day-label (day-key)
  "Return pretty date label for DAY-KEY in YYYY-MM-DD form."
  (if (not (stringp day-key))
      "Unknown date"
    (condition-case _
        (format-time-string "%A, %Y-%m-%d"
                            (date-to-time (concat day-key "T00:00:00")))
      (error day-key))))

(defun disco-msg-type (message)
  "Return numeric message type for MESSAGE, defaulting to 0."
  (let ((raw (and (listp message) (alist-get 'type message))))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t 0))))

(defun disco-msg-reply-type-p (message)
  "Return non-nil when MESSAGE is a standard reply message."
  (= (disco-msg-type message) 19))

(defun disco-msg-poll (message)
  "Return poll object from MESSAGE, or nil when absent."
  (let ((poll (and (listp message) (alist-get 'poll message))))
    (and (listp poll) poll)))

(defun disco-msg-poll-results (poll)
  "Return poll results object from POLL, or nil when unknown."
  (let ((results (and (listp poll) (alist-get 'results poll))))
    (and (listp results) results)))

(defun disco-msg-poll-answer-id (answer)
  "Return normalized integer answer id from poll ANSWER, or nil."
  (let ((raw (and (listp answer) (alist-get 'answer_id answer))))
    (cond
     ((integerp raw) raw)
     ((and (stringp raw)
           (string-match-p "\\`[0-9]+\\'" raw))
      (string-to-number raw))
     (t nil))))

(defun disco-msg-poll-answer-media (answer)
  "Return poll media object for ANSWER."
  (let ((media (and (listp answer) (alist-get 'poll_media answer))))
    (and (listp media) media)))

(defun disco-msg-poll-answer-text (answer)
  "Return display text for poll ANSWER."
  (let* ((media (disco-msg-poll-answer-media answer))
         (text (and media (alist-get 'text media))))
    (if (and (stringp text)
             (not (string-empty-p (string-trim text))))
        (string-trim text)
      "(no text)")))

(defun disco-msg-poll-answer-emoji (answer)
  "Return emoji label for poll ANSWER, or nil."
  (let* ((media (disco-msg-poll-answer-media answer))
         (emoji (and media (alist-get 'emoji media)))
         (name (and (listp emoji) (alist-get 'name emoji))))
    (and (stringp name)
         (not (string-empty-p name))
         name)))

(defun disco-msg-poll-question-text (poll)
  "Return normalized question text for POLL."
  (let* ((question (and (listp poll) (alist-get 'question poll)))
         (text (cond
                ((stringp question) question)
                ((listp question) (alist-get 'text question))
                (t nil))))
    (if (and (stringp text)
             (not (string-empty-p (string-trim text))))
        (string-trim text)
      "(untitled poll)")))

(defun disco-msg-poll-answer-count-entry (poll answer-id)
  "Return result count entry from POLL for ANSWER-ID, or nil."
  (let ((counts (and (disco-msg-poll-results poll)
                     (alist-get 'answer_counts (disco-msg-poll-results poll)))))
    (seq-find
     (lambda (entry)
       (let ((entry-id (alist-get 'id entry)))
         (or (and (integerp entry-id)
                  (= entry-id answer-id))
             (and (stringp entry-id)
                  (string-match-p "\\`[0-9]+\\'" entry-id)
                  (= (string-to-number entry-id) answer-id)))))
     (or counts '()))))

(defun disco-msg-poll-answer-count (poll answer-id)
  "Return vote count for ANSWER-ID in POLL."
  (let* ((entry (disco-msg-poll-answer-count-entry poll answer-id))
         (count (and (listp entry) (alist-get 'count entry))))
    (if (numberp count)
        (max 0 count)
      0)))

(defun disco-msg-poll-answer-me-voted-p (poll answer-id)
  "Return non-nil when current user voted ANSWER-ID in POLL."
  (let* ((entry (disco-msg-poll-answer-count-entry poll answer-id))
         (me-voted (and (listp entry) (alist-get 'me_voted entry))))
    (disco-util-json-true-p me-voted)))

(defun disco-msg-poll-total-votes (poll)
  "Return aggregate vote count from POLL results."
  (let ((counts (and (disco-msg-poll-results poll)
                     (alist-get 'answer_counts (disco-msg-poll-results poll))))
        (total 0))
    (dolist (entry (or counts '()) total)
      (let ((count (and (listp entry) (alist-get 'count entry))))
        (when (numberp count)
          (setq total (+ total (max 0 count))))))))

(defun disco-msg-poll-multiselect-p (poll)
  "Return non-nil when POLL allows multiple answers."
  (disco-util-json-true-p (alist-get 'allow_multiselect poll)))

(defun disco-msg-poll-expired-p (poll)
  "Return non-nil when POLL expiry is in the past."
  (let ((expiry (and (listp poll) (alist-get 'expiry poll))))
    (when (and (stringp expiry)
               (not (string-empty-p expiry)))
      (condition-case _
          (<= (float-time (date-to-time expiry)) (float-time))
        (error nil)))))

(defun disco-msg-poll-expiry-label (poll &optional format)
  "Return formatted expiry text for POLL, or nil.

FORMAT defaults to `%Y-%m-%d %H:%M'."
  (let ((expiry (and (listp poll) (alist-get 'expiry poll))))
    (when (and (stringp expiry)
               (not (string-empty-p expiry)))
      (condition-case _
          (format-time-string (or format "%Y-%m-%d %H:%M")
                              (date-to-time expiry))
        (error nil)))))

(defun disco-msg-poll-state-label (poll)
  "Return short status label for POLL."
  (let* ((results (disco-msg-poll-results poll))
         (finalized (and (listp results)
                         (disco-util-json-true-p (alist-get 'is_finalized results))))
         (expired (disco-msg-poll-expired-p poll)))
    (cond
     (finalized "finalized")
     (expired "closed")
     (t "open"))))

(defun disco-msg-poll-voted-answer-ids (poll)
  "Return list of answer IDs voted by current user in POLL."
  (let ((answers (or (alist-get 'answers poll) '()))
        out)
    (dolist (answer answers (nreverse out))
      (let ((answer-id (disco-msg-poll-answer-id answer)))
        (when (and answer-id
                   (disco-msg-poll-answer-me-voted-p poll answer-id))
          (push answer-id out))))))

(defun disco-msg-poll-normalize-answer-id-list (answer-ids)
  "Return ANSWER-IDS normalized as a deduped integer list."
  (let (out)
    (dolist (it (or answer-ids '()) (nreverse out))
      (let ((id (cond
                 ((integerp it) it)
                 ((and (stringp it)
                       (string-match-p "\\`[0-9]+\\'" it))
                  (string-to-number it))
                 (t nil))))
        (when (and (integerp id)
                   (> id 0)
                   (not (member id out)))
          (push id out))))))

(defun disco-msg-reaction-emoji (reaction)
  "Extract display emoji string from REACTION object."
  (let* ((emoji (alist-get 'emoji reaction))
         (name (and (listp emoji) (alist-get 'name emoji))))
    (or name
        (and (stringp emoji) emoji)
        (alist-get 'emoji_name reaction)
        "?")))

(defun disco-msg-reaction-count (reaction)
  "Return integer count for REACTION object."
  (or (alist-get 'count reaction)
      (alist-get 'total_count reaction)
      0))

(defun disco-msg-reaction-selected-p (reaction)
  "Return non-nil when REACTION is selected by current user."
  (or (disco-util-json-true-p (alist-get 'me reaction))
      (disco-util-json-true-p (alist-get 'is_chosen reaction))))

(defun disco-msg-reactions (message)
  "Return normalized reactions list for MESSAGE."
  (or (and (listp message) (alist-get 'reactions message))
      (and (listp message) (alist-get 'reaction_counts message))
      '()))

(provide 'disco-msg)

;;; disco-msg.el ends here
