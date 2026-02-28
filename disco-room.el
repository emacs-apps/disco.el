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
(require 'url)
(require 'url-http)
(require 'disco-api)
(require 'disco-gateway)
(require 'disco-state)
(require 'disco-transient)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar url-http-content-type)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--channel-name nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--oldest-message-id nil)
(defvar-local disco-room--newest-message-id nil)
(defvar-local disco-room--history-exhausted nil)
(defvar-local disco-room--pending-reply-to nil)
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
(defvar-local disco-room--rendering nil)
(defvar-local disco-room--ewoc nil)
(defvar-local disco-room--message-node-table nil)

(defvar disco-room--avatar-image-cache (make-hash-table :test #'equal)
  "Global avatar image cache keyed by avatar cache key.

Values are either image objects or the symbol `:missing'.")

(defvar disco-room--avatar-fetching (make-hash-table :test #'equal)
  "Global set of avatar cache keys currently being fetched.")

(defvar disco-room--avatar-fetch-budget nil
  "Dynamic cap for number of avatar fetches started in current render pass.")

(defcustom disco-room-input-history-size 30
  "Maximum number of draft entries kept in room input history."
  :type 'integer
  :group 'disco)

(defcustom disco-room-send-on-return t
  "When non-nil, `RET' in room buffer sends current draft."
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

(defcustom disco-room-avatar-cache-directory
  (locate-user-emacs-file "disco-avatar-cache/")
  "Directory used to cache downloaded avatar images."
  :type 'directory
  :group 'disco)

(defcustom disco-room-avatar-max-fetches-per-render 6
  "Maximum avatar fetches started during one room render pass."
  :type 'integer
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

(defvar disco-room-input-map
  (let ((map (make-sparse-keymap)))
    ;; Keep normal text editing in draft region, then layer room actions.
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
    map)
  "Keymap active when point is inside the room draft region.")

(defun disco-room--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-room--json-true-p (value)
  "Return non-nil when VALUE semantically represents JSON true."
  (or (eq value t)
      (eq value 'true)
      (equal value "true")))

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
    (or (disco-room--json-true-p (alist-get 'archived meta))
        (disco-room--json-true-p (alist-get 'archived target)))))

(defun disco-room--thread-locked-p (&optional channel)
  "Return non-nil when CHANNEL thread is locked."
  (let* ((target (or channel (disco-room--channel-object)))
         (meta (disco-room--thread-metadata target)))
    (or (disco-room--json-true-p (alist-get 'locked meta))
        (disco-room--json-true-p (alist-get 'locked target)))))

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

(defun disco-room--latest-message-id ()
  "Return newest known message ID for current room, or nil."
  (alist-get 'id (car (disco-state-messages disco-room--channel-id))))

(defun disco-room--async-error-message (err)
  "Return user-facing error message extracted from async ERR payload."
  (or (and (listp err) (plist-get err :message))
      (and (listp err)
           (plist-get err :status)
           (format "HTTP %s" (plist-get err :status)))
      (format "%S" err)))

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

(defun disco-room--input-region-bounds ()
  "Return current writable draft region as (START . END), or nil."
  (when (and (markerp disco-room--input-marker)
             (eq (marker-buffer disco-room--input-marker) (current-buffer)))
    (let ((start (marker-position disco-room--input-marker)))
      (when (<= start (point-max))
        ;; Draft input is rendered at the end of buffer. Using point-max
        ;; avoids depending on field-property propagation edge cases.
        (cons start (point-max))))))

(defun disco-room--point-in-input-p (&optional position)
  "Return non-nil when POSITION (or point) is inside draft input region."
  (let* ((bounds (disco-room--input-region-bounds))
         (pos (or position (point))))
    (and bounds
         (<= (car bounds) pos)
         (<= pos (cdr bounds)))))

(defun disco-room--sync-draft-from-buffer ()
  "Sync `disco-room--draft-input' from editable input region, when present."
  (let ((bounds (disco-room--input-region-bounds)))
    (when bounds
      (let* ((raw (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (text (replace-regexp-in-string "[\n\r]+\\'" "" raw)))
        (unless (equal text disco-room--draft-input)
          (setq disco-room--draft-input text)
          (setq disco-room--input-index nil)
          (setq disco-room--input-pending nil))))))

(defun disco-room--after-change (beg end _old-len)
  "Keep draft state synced after editable-region changes from BEG to END."
  (unless disco-room--rendering
    (let ((bounds (disco-room--input-region-bounds)))
      (when (and bounds
                 (< beg (cdr bounds))
                 (> end (car bounds)))
        (disco-room--sync-draft-from-buffer)))))

(defun disco-room--set-draft (text)
  "Set room draft TEXT and re-render room."
  (setq disco-room--draft-input (or text ""))
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

(defun disco-room--mention-candidates ()
  "Return mention completion candidates from loaded room messages.

Each element is a cons cell (DISPLAY . USER-ID)."
  (let ((seen-ids (make-hash-table :test #'equal))
        (seen-labels (make-hash-table :test #'equal))
        out)
    (dolist (msg (or (disco-state-messages disco-room--channel-id) '()))
      (let* ((author (alist-get 'author msg))
             (user-id (and (listp author) (alist-get 'id author)))
             (name (or (and (listp author) (alist-get 'global_name author))
                       (and (listp author) (alist-get 'username author))
                       "unknown")))
        (when (and user-id (not (gethash user-id seen-ids)))
          (puthash user-id t seen-ids)
          (let ((label (format "@%s" name)))
            (when (gethash label seen-labels)
              (setq label (format "%s (%s)"
                                  label
                                  (substring user-id (max 0 (- (length user-id) 4))))))
            (puthash label t seen-labels)
            (push (cons label user-id) out)))))
    (sort out (lambda (a b)
                (string-lessp (downcase (car a))
                              (downcase (car b)))))))

(defun disco-room--mention-token-bounds ()
  "Return bounds of @mention token at point as (START . END), or nil."
  (save-excursion
    (let ((end (point)))
      (skip-chars-backward "A-Za-z0-9._-")
      (when (eq (char-before) ?@)
        (let* ((start (1- (point)))
               (left (char-before start)))
          ;; Trigger mention completion only at token boundaries.
          (when (or (null left)
                    (eq left ?\s)
                    (eq left ?\t)
                    (eq left ?\n)
                    (eq left ?\r))
            (cons start end)))))))

(defun disco-room-complete-mention ()
  "Complete @mention at point using authors in loaded room history.

Completion inserts Discord mention syntax `<@USER-ID>`."
  (interactive)
  (let ((bounds (disco-room--mention-token-bounds)))
    (if (not bounds)
        (message "disco: point is not on an @mention token")
      (let* ((start (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties (1+ start) end))
             (search-prefix (if (string-prefix-p "@" prefix)
                                (substring prefix 1)
                              prefix))
             (all (disco-room--mention-candidates))
             (matches (seq-filter (lambda (it)
                                    (string-prefix-p (downcase search-prefix)
                                                     (downcase (substring (car it) 1))))
                                  all)))
        (if (null matches)
            (message "disco: no mention candidates for @%s" prefix)
          (let* ((choice-label
                  (if (= (length matches) 1)
                      (car (car matches))
                    (completing-read (format "Mention @%s: " prefix)
                                     (mapcar #'car matches)
                                     nil t nil nil search-prefix)))
                 (choice-id (cdr (assoc choice-label matches))))
            (unless choice-id
              (user-error "disco: invalid mention selection"))
            (delete-region start end)
            (insert (format "<@%s>" choice-id))
            (unless (memq (char-after) '(?\s ?\t))
              (insert " "))
            (disco-room--sync-draft-from-buffer)))))))

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

(defun disco-room--message-by-id (message-id)
  "Return room message object for MESSAGE-ID, or nil."
  (seq-find (lambda (msg)
              (equal (alist-get 'id msg) message-id))
            (or (disco-state-messages disco-room--channel-id) '())))

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

(defun disco-room--buffer-name (channel-name channel-id)
  "Build room buffer name for CHANNEL-NAME and CHANNEL-ID."
  (format "*disco:%s (%s)*" channel-name channel-id))

(defun disco-room--format-time (iso8601)
  "Format ISO8601 into a compact local string."
  (condition-case _
      (format-time-string "%Y-%m-%d %H:%M"
                          (date-to-time iso8601))
    (error "unknown-time")))

(defun disco-room--message-author (msg)
  "Extract author name from message MSG alist."
  (let* ((author (alist-get 'author msg))
         (global-name (and (listp author) (alist-get 'global_name author)))
         (username (and (listp author) (alist-get 'username author))))
    (or global-name username "unknown")))

(defun disco-room--message-author-id (msg)
  "Extract author ID string from message MSG alist."
  (let ((author (alist-get 'author msg)))
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

(defun disco-room--image-rendering-available-p ()
  "Return non-nil when avatar images can be shown in current frame."
  (and disco-room-show-avatar-images
       (display-images-p)
       (or (image-type-available-p 'png)
           (image-type-available-p 'imagemagick))))

(defun disco-room--author-avatar-hash (msg)
  "Extract Discord avatar hash string from MSG author, or nil."
  (let ((author (alist-get 'author msg)))
    (and (listp author) (alist-get 'avatar author))))

(defun disco-room--author-default-avatar-index (msg)
  "Return Discord default avatar index for MSG author."
  (let* ((author (alist-get 'author msg))
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

(defun disco-room--avatar-cache-file (cache-key)
  "Return cache file path for avatar CACHE-KEY."
  (expand-file-name
   (format "%s.png" (md5 cache-key))
   disco-room-avatar-cache-directory))

(defun disco-room--avatar-image-from-file (file)
  "Create inline avatar image from FILE, or nil when unsupported."
  (when (disco-room--file-looks-like-png-p file)
    (let ((image
           (ignore-errors
             (create-image file nil nil
                           :width disco-room-avatar-image-size
                           :height disco-room-avatar-image-size
                           :ascent 'center))))
      (when (and image (disco-room--image-object-valid-p image))
        image))))

(defun disco-room--file-looks-like-png-p (file)
  "Return non-nil when FILE starts with the PNG signature."
  (and (file-exists-p file)
       (>= (nth 7 (file-attributes file)) 8)
       (with-temp-buffer
         (let ((coding-system-for-read 'binary))
           (insert-file-contents-literally file nil 0 8))
         (string= (buffer-string)
                  (string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n)))))

(defun disco-room--image-object-valid-p (image)
  "Return non-nil when IMAGE can be rendered by Emacs.

This filters out broken image specs that would otherwise render as tofu boxes."
  (and image
       (condition-case _
           (progn
             (image-size image t)
             t)
         (error nil))))

(defun disco-room-clear-avatar-cache ()
  "Clear in-memory avatar cache and rerender all room buffers."
  (interactive)
  (clrhash disco-room--avatar-image-cache)
  (clrhash disco-room--avatar-fetching)
  (disco-room--rerender-open-rooms)
  (message "disco: avatar cache cleared"))

(defun disco-room-refetch-avatars ()
  "Drop avatar caches (memory + disk) and refetch in open room buffers."
  (interactive)
  (clrhash disco-room--avatar-image-cache)
  (clrhash disco-room--avatar-fetching)
  (when (file-directory-p disco-room-avatar-cache-directory)
    (delete-directory disco-room-avatar-cache-directory t))
  (disco-room--rerender-open-rooms)
  (message "disco: avatar cache reset; refetching"))

(defun disco-room--rerender-open-rooms ()
  "Rerender all open room buffers while preserving point line/column."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'disco-room-mode)
          (let ((line (line-number-at-pos))
                (col (current-column)))
            (disco-room-render)
            (goto-char (point-min))
            (forward-line (max 0 (1- line)))
            (move-to-column col)))))))

(defun disco-room--start-avatar-fetch (cache-key url file)
  "Start asynchronous avatar fetch for CACHE-KEY from URL into FILE."
  (unless (or (gethash cache-key disco-room--avatar-fetching)
              (gethash cache-key disco-room--avatar-image-cache)
              (and (numberp disco-room--avatar-fetch-budget)
                   (<= disco-room--avatar-fetch-budget 0)))
    (when (numberp disco-room--avatar-fetch-budget)
      (cl-decf disco-room--avatar-fetch-budget))
    (puthash cache-key t disco-room--avatar-fetching)
    (url-retrieve
     url
     (lambda (status key target-file)
       (unwind-protect
           (let* ((status-code (and (boundp 'url-http-response-status)
                                    url-http-response-status))
                  (body-start (and (boundp 'url-http-end-of-headers)
                                   url-http-end-of-headers))
                  image)
             (when (and (not (plist-get status :error))
                        (numberp status-code)
                        (<= 200 status-code)
                        (< status-code 300)
                        (or (not (boundp 'url-http-content-type))
                            (null url-http-content-type)
                            (string-match-p "\\`image/" url-http-content-type)))
               (unless body-start
                 (goto-char (point-min))
                 (when (re-search-forward "\\r?\\n\\r?\\n" nil t)
                   (setq body-start (point))))
               (when body-start
                 (make-directory (file-name-directory target-file) t)
                 (let ((coding-system-for-write 'binary))
                   (write-region body-start (point-max) target-file nil 'silent))
                 (setq image (disco-room--avatar-image-from-file target-file))))
             (when (and (null image)
                        (file-exists-p target-file))
               ;; Drop corrupted cache files so later refresh can retry cleanly.
               (ignore-errors (delete-file target-file)))
             (puthash key
                      (or image :missing)
                      disco-room--avatar-image-cache)
             (remhash key disco-room--avatar-fetching)
             (disco-room--rerender-open-rooms))
         (when (buffer-live-p (current-buffer))
           (kill-buffer (current-buffer)))))
     (list cache-key file)
     t)))

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
        (if (disco-room--image-object-valid-p cached)
            cached
          (remhash cache-key disco-room--avatar-image-cache)
          nil))
       (t
        (let* ((url (disco-room--avatar-url msg))
               (cache-file (and cache-key (disco-room--avatar-cache-file cache-key)))
               (file-image (and cache-file
                                (file-exists-p cache-file)
                                (disco-room--avatar-image-from-file cache-file))))
          (cond
           (file-image
            (puthash cache-key file-image disco-room--avatar-image-cache)
            file-image)
           ((and url cache-file)
            (when (and (file-exists-p cache-file) (not file-image))
              (ignore-errors (delete-file cache-file)))
            (disco-room--start-avatar-fetch cache-key url cache-file)
            nil)
           (t nil))))))))

(defun disco-room--insert-avatar (msg)
  "Insert avatar presentation for MSG at point."
  (let ((image (disco-room--avatar-image msg))
        (fallback (disco-room--avatar-placeholder msg)))
    (if image
        (condition-case _
            (insert-image image fallback)
          (error
           (insert fallback)))
      (insert fallback))))

(defun disco-room--message-display-content (msg)
  "Return human-readable content string for message MSG."
  (let ((content (or (alist-get 'content msg) ""))
        (attachments (or (alist-get 'attachments msg) '()))
        (embeds (or (alist-get 'embeds msg) '())))
    (if (string-empty-p content)
        (cond
         ((> (length attachments) 0)
          (format "[attachment x%d]" (length attachments)))
         ((> (length embeds) 0)
          (format "[embed x%d]" (length embeds)))
         (t "[empty]"))
      content)))

(defun disco-room--reply-reference-id (msg)
  "Return referenced message ID for MSG, or nil."
  (or (and (listp (alist-get 'referenced_message msg))
           (alist-get 'id (alist-get 'referenced_message msg)))
      (and (listp (alist-get 'message_reference msg))
           (alist-get 'message_id (alist-get 'message_reference msg)))))

(defun disco-room--reply-preview (msg)
  "Return one-line preview string of MSG reply target, or nil."
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
        (format "Original message unavailable (%s)" ref-id)))))

(defun disco-room--insert-message (msg)
  "Insert one message MSG in current buffer."
  (let* ((timestamp (disco-room--format-time (or (alist-get 'timestamp msg) "")))
         (author (disco-room--message-author msg))
         (author-face (disco-room--author-face msg))
         (content (disco-room--message-display-content msg))
         (reply (disco-room--reply-preview msg))
         (message-id (alist-get 'id msg))
         line-start
         author-start)
    (when reply
      (let ((reply-start (point)))
        (insert (format "  ↪ %s\n" reply))
        (add-text-properties
         reply-start
         (point)
         (list 'read-only t
               'face 'shadow
               'front-sticky '(read-only)
               'disco-message-id message-id))))
    (setq line-start (point))
    (insert (format "[%s] " timestamp))
    (disco-room--insert-avatar msg)
    (insert " ")
    (setq author-start (point))
    (insert author)
    (add-text-properties author-start (point) (list 'face author-face))
    (insert (format ": %s\n" content))
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
  (let ((prompt (propertize "\n>>> "
                            'read-only t
                            'front-sticky '(read-only)
                            'rear-nonsticky '(read-only disco-room-input)))
        (input (if (string-empty-p draft)
                   "\n"
                 (concat draft "\n"))))
    (concat prompt
            (propertize input
                        'disco-room-input t
                        'read-only nil))))

(defun disco-room--bind-input-region-from-footer ()
  "Locate and bind editable input region from EWOC footer properties."
  (let ((input-start (text-property-any (point-min) (point-max) 'disco-room-input t)))
    (when input-start
      (let ((input-end (or (next-single-property-change
                            input-start 'disco-room-input nil (point-max))
                           (point-max))))
        (add-text-properties
         input-start input-end
         (list 'read-only nil
               'field 'disco-room-input
               'local-map disco-room-input-map
               'rear-nonsticky '(read-only field local-map)))
        (setq disco-room--input-marker (copy-marker input-start nil))))))

(defun disco-room--insert-message-node (msg)
  "Insert one message node for MSG at the end of room EWOC."
  (when disco-room--ewoc
    (let ((disco-room--rendering t)
          (inhibit-read-only t))
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
            (let ((disco-room--rendering t)
                  (inhibit-read-only t))
              (ewoc-set-data node msg)
              (ewoc-invalidate disco-room--ewoc node)
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
            (inhibit-read-only t))
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
        (messages (disco-state-messages disco-room--channel-id))
        (draft (disco-room--current-draft))
        header-end
        (disco-room--avatar-fetch-budget
         (max 0 (or disco-room-avatar-max-fetches-per-render 0))))
    (setq disco-room--rendering t)
    (unwind-protect
        (progn
          (erase-buffer)
          (insert (format "Channel: %s%s\n"
                          disco-room--channel-name
                          (disco-room--thread-header-suffix)))
          (insert "g: refresh   M-<: older   s/n/p: search   r/e/d: reply/edit/delete   RET/C-c C-c: send   TAB: @mention   C-c C-v: refetch avatars   type at >>>   M-p/M-n: history   q: quit")
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
          (when disco-room--history-exhausted
            (insert "(older history exhausted)\n"))
          (insert "\n")
          (setq header-end (point))
          (put-text-property (point-min) header-end 'read-only t)
          (setq disco-room--input-marker nil)
          (setq disco-room--message-node-table (make-hash-table :test #'equal))
          (setq disco-room--ewoc
                (ewoc-create
                 #'disco-room--ewoc-printer
                 nil
                 (disco-room--input-footer-text draft)
                 t))
          ;; API returns newest-first by default; reverse for chat-like display.
          (dolist (msg (reverse messages))
            (disco-room--insert-message-node msg))
          (disco-room--bind-input-region-from-footer)
          (when (markerp disco-room--input-marker)
            ;; Keep typing position at end of current draft.
            (goto-char (point-max))
            (disco-room--sync-draft-from-buffer)))
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
           (disco-room-render)
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
      (let ((at-bottom (= (point) (point-max)))
            (channel (disco-room--channel-object)))
        (when (and channel (alist-get 'name channel))
          (setq disco-room--channel-name (alist-get 'name channel)))
        (disco-room-render)
        (when at-bottom
          (goto-char (point-max)))))
     ((and (equal event-channel-id disco-room--channel-id)
           (memq event-type '(message-create message-update message-delete)))
      (let ((at-bottom (= (point) (point-max))))
        (unless (disco-room--apply-live-message-event-partially event)
          (disco-room-render))
        (when (eq event-type 'message-create)
          (let* ((message (plist-get event :message))
                 (message-id (and (listp message) (alist-get 'id message))))
            (disco-room--mark-read message-id)))
        (when at-bottom
          (goto-char (point-max))))))))

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
    (disco-gateway-unwatch-channel disco-room--channel-id)))

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
           (content (if (or current-prefix-arg
                            (string-empty-p (string-trim-right current-draft)))
                        (read-from-minibuffer "Message: " current-draft)
                      current-draft))
           (normalized (string-trim-right (or content ""))))
      (if (string-empty-p normalized)
          (message "disco: draft is empty")
        (let ((room-buffer (current-buffer))
              (channel-id disco-room--channel-id)
              (reply-to disco-room--pending-reply-to))
          (disco-room--input-history-push normalized)
          (setq disco-room--draft-input "")
          (setq disco-room--send-in-flight t)
          (disco-room-render)
          (disco-api-send-message-async
           channel-id
           normalized
           :reply-to-message-id reply-to
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
                          (disco-room--async-error-message err))))))))))))

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
                         (disco-room-render)
                         (message "disco: reached beginning of history"))
                     (let ((merged (disco-room--merge-message-pages existing older)))
                       (disco-state-put-messages channel-id merged)
                       (disco-room--update-message-window-state merged)
                       (disco-room-render)
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
    (disco-room-render)
    (message "disco: thread %s" (if next-archived "archived" "unarchived"))))

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
    (define-key map (kbd "C-c C-c") #'disco-room-send-message)
    (define-key map (kbd "C-c C-k") #'disco-room-cancel-reply)
    (define-key map (kbd "C-c C-t m") #'disco-room-create-thread-from-message)
    (define-key map (kbd "C-c C-t c") #'disco-room-create-thread)
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
  (setq truncate-lines t)
  (setq-local disco-room--draft-input "")
  (setq-local disco-room--input-ring (make-ring (max 1 disco-room-input-history-size)))
  (setq-local disco-room--input-index nil)
  (setq-local disco-room--input-pending nil)
  (setq-local disco-room--send-in-flight nil)
  (setq-local disco-room--last-search-query nil)
  (setq-local disco-room--input-marker nil)
  (setq-local disco-room--rendering nil)
  (setq-local disco-room--ewoc nil)
  (setq-local disco-room--message-node-table (make-hash-table :test #'equal))
  (add-hook 'after-change-functions #'disco-room--after-change nil t))

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
