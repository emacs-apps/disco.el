;;; disco-room-render.el --- Timeline rendering for Disco rooms -*- lexical-binding: t; -*-

;;; Commentary:

;; Room presentation policy, avatar/media resources, message formatting, and
;; timeline row insertion.  The room facade retains lifecycle, history,
;; Gateway dispatch, and projection transaction ownership.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg nil t)
(require 'time-date)
(require 'plz)

(require 'appkit-core)
(require 'appkit-media)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'disco-api)
(require 'disco-avatar)
(require 'disco-channel-type)
(require 'disco-customize)
(require 'disco-embed)
(require 'disco-emoji-image)
(require 'disco-gateway)
(require 'disco-ins)
(require 'disco-markdown)
(require 'disco-media)
(require 'disco-msg)
(require 'disco-room-compose)
(require 'disco-room-pin)
(require 'disco-room-poll)
(require 'disco-room-reaction)
(require 'disco-room-search)
(require 'disco-room-thread)
(require 'disco-runtime)
(require 'disco-state)
(require 'disco-sticker)
(require 'disco-thread)

(autoload 'disco-user-open "disco-user" nil t)
(defvar visual-fill-column-width)

(declare-function disco-room--channel-message-by-id
                  "disco-room" (channel-id message-id))
(declare-function disco-room--channel-object "disco-room" ())
(declare-function disco-room--ensure-surface "disco-room" ())
(declare-function disco-room--message-by-id "disco-room" (message-id))
(declare-function disco-room--message-at-point "disco-room" ())
(declare-function disco-room--message-flags "disco-room" (message))
(declare-function disco-room--message-spoilers-revealed-p
                  "disco-room" (message-id))
(declare-function disco-room-toggle-message-spoilers
                  "disco-room" (&optional message-id))
(declare-function disco-room--request-render "disco-room" (view))
(declare-function disco-room-jump-to-message
                  "disco-room" (message-id &optional channel-id))
(declare-function disco-room-list-pinned-messages
                  "disco-room-pin" (&optional channel-id))
(declare-function disco-room--update-frame
                  "disco-room" (&optional channel draft))
(declare-function appkit-translate-insert "appkit-translate"
                  (source &optional prefix prefix-face surface))

(defvar disco-room--channel-id)
(defvar disco-room--channel-name)
(defvar disco-room--guild-id)
(defvar disco-room--revealed-spoiler-message-id)

;;; Presentation state

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

;;; Layout and message identity

(defun disco-room--line-fill-column ()
  "Return target fill column for the current message line."
  (or (and (bound-and-true-p visual-fill-column-mode)
           (boundp 'visual-fill-column-width)
           (integerp visual-fill-column-width)
           (> visual-fill-column-width 0)
           visual-fill-column-width)
      (and (bound-and-true-p visual-fill-column-mode)
           (integerp fill-column)
           (> fill-column 0)
           fill-column)
      (when-let* ((surface (appkit-current-surface)))
        (appkit-surface-responsive-width
         surface disco-room-auto-fill-margin-columns))
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
      (when (= (disco-msg-type msg) 18)
        (when-let* ((thread-id (disco-msg-reference-channel-id msg)))
          (let ((thread-name (or (alist-get 'name (disco-state-channel thread-id))
                                 (alist-get 'content msg)
                                 thread-id)))
            (appkit-ui-add-action
             (car span) (cdr span)
             (lambda () (disco-room-open thread-id thread-name))
             :help-echo "Open thread"))))
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

;;; Forward guild icon resources

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

;;; Session cache and resource invalidation

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

(defun disco-room-render--clear-session-cache-memory ()
  "Clear account-scoped room presentation cache bookkeeping."
  (clrhash disco-room--avatar-round-image-cache)
  (clrhash disco-room--forward-guild-icon-fetching)
  (clrhash disco-room--forward-guild-icon-image-cache))

(defun disco-room-render-reset-session-cache-state ()
  "Destructively clear account-scoped room media state without redrawing.

Shared user avatars are retired independently by `disco-avatar'.  This reset
owns only room presentation caches and exact forwarded-icon process owners.
No Appkit invalidation is requested."
  (let ((disco-room--session-cache-reset-in-progress t))
    (unwind-protect
        (disco-room--reset-forward-guild-icon-state)
      ;; Repeat the destructive clears after cancellation hooks: even an
      ;; instrumented hook which mutates these globals cannot retain old data.
      (disco-room-render--clear-session-cache-memory))))

(defun disco-room--responsive-geometry-changed (surface _width)
  "Request one geometry redraw after SURFACE's presentation width changes."
  (disco-room--queue-update surface 'geometry))

(defun disco-room--refresh-open-rooms ()
  "Request geometry projection for all open room timelines."
  (unless disco-room--session-cache-reset-in-progress
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (eq major-mode 'disco-room-mode)
                     (appkit-surface-live-p (appkit-current-surface)))
            (disco-room--queue-update (appkit-current-surface) 'geometry)))))))

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
                         (appkit-surface-live-p (appkit-current-surface)))
                (let ((view (appkit-current-surface)))
                  (disco-room--queue-update view (list 'resources-changed resources))

                  (when (seq-some
                         (lambda (resource)
                           (member resource resources))
                         (disco-room--composer-one-line-resource-keys))
                    (disco-room--queue-update view 'frame)))))))))))

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

(add-hook 'disco-media-rerender-hook #'disco-room--handle-media-rerender)

;;; Avatar presentation

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
       nil fallback
       :pixel-size base-size))))

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
        :spoiler-toggle-action toggle-action
        :owner owner))
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
  (when-let* ((view (appkit-current-surface))
              (_ (appkit-surface-live-p view))
              (owner (appkit-surface-app view))
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

;;; Forwarded message presentation

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

;;; Attachment and snapshot projection

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

;;; Message content formatting

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

;;; Timeline insertion

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

(defconst disco-room--semantic-system-message-types
  '(6 7 8 9 10 11 12 14 15 16 17 18 21 22 24 25 26 27 28 29 30 31
    32 36 37 38 39 44 46)
  "Message types whose body is synthesized rather than rendered as Markdown.")

(defun disco-room--semantic-message-document (msg)
  "Return MSG's native semantic content Document, or nil for synthetic rows."
  (let ((captured (and (listp msg) (alist-get 'appkit_document msg)))
        (source (and (listp msg) (alist-get 'content msg))))
    (cond
     ((appkit-markup-document-p captured)
      (appkit-markup-normalize captured))
     ((and (stringp source)
           (not (string-empty-p source))
           (not (memq (disco-msg-type msg)
                      disco-room--semantic-system-message-types)))
      (let ((message-id (alist-get 'id msg)))
        (disco-markdown-document
         source
         :context 'room-message
         :message msg
         :spoiler-message-id message-id)))
     (t nil))))

(defun disco-room--translation-source (msg &optional include-text)
  "Return a scoped translation source for MSG's displayed message body.
INCLUDE-TEXT extracts semantic text only for an explicit request.  Forward
snapshots and thread starters use their source message, not UI summaries.
Attachments, embeds, reply previews and spoiler bodies are never sent."
  (let* ((body (cond
                ((= (disco-msg-type msg) 21)
                 (disco-room--thread-starter-reference-message msg))
                ((and (string-empty-p (or (alist-get 'content msg) ""))
                      (disco-room--message-forwarded-p msg))
                 (disco-room--message-forward-snapshot msg))
                (t msg)))
         (source
          (list :key (list 'disco
                           (or (alist-get 'channel_id msg) disco-room--channel-id)
                           (alist-get 'id msg))
                :version (list (disco-msg-type msg)
                               (alist-get 'id body)
                               (alist-get 'content body)
                               (alist-get 'appkit_document body)
                               (alist-get 'mentions body)
                               (alist-get 'mention_channels body)
                               (alist-get 'resolved body)
                               'concealed-spoilers))))
    (when include-text
      (setq source
            (plist-put
             source :text
             (if-let* ((document (disco-room--semantic-message-document body)))
                 (disco-markdown-translation-text document)
               ""))))
    source))

(defun disco-room--highlight-search-region (start end)
  "Apply the active room search highlight between START and END."
  (when-let* ((query (and (fboundp 'disco-room--active-highlight-query)
                          (funcall 'disco-room--active-highlight-query))))
    (when (and (stringp query) (not (string-empty-p query)))
      (save-excursion
        (goto-char start)
        (let ((case-fold-search t))
          (while (re-search-forward (regexp-quote query) end t)
            (add-face-text-property
             (match-beginning 0) (match-end 0)
             'disco-room-search-highlight 'append)))))))

(cl-defun disco-room--insert-semantic-message-content
    (document msg &key prefix (final-newline-p t))
  "Insert MSG's semantic DOCUMENT natively and return its exact bounds."
  (let* ((message-id (alist-get 'id msg))
         (span
          (disco-markdown-insert-document
           document
           :context 'room-message
           :spoiler-message-id message-id
           :reveal-spoilers
           (disco-room--message-spoilers-revealed-p message-id)
           :prefix prefix
           :final-newline-p final-newline-p)))
    (disco-room--highlight-search-region (car span) (cdr span))
    span))

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
           (message-id (alist-get 'id msg))
           (semantic-document (disco-room--semantic-message-document msg))
           (content
            (if semantic-document
                (appkit-markup-plain-text semantic-document)
              (disco-room--message-display-content msg)))
           (reply (disco-room--reply-preview msg))
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
                (if semantic-document
                    (disco-room--insert-semantic-message-content
                     semantic-document msg
                     :final-newline-p nil)
                  (insert content)))
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
            (if semantic-document
                (disco-room--insert-semantic-message-content
                 semantic-document msg
                 :prefix section-prefix-state)
              (appkit-ui-insert-prefixed-lines
               section-prefix-state content)))))
      (let ((appkit-ui-card-indent-prefix-state section-prefix-state)
            (appkit-ui-card-indent-prefix
             (appkit-ui-prefix-string section-prefix-state nil "    ")))
        (disco-room--insert-message-stickers msg section-prefix-state)
        (disco-room--insert-forward-section msg section-prefix-state)
        (when (featurep 'appkit-translate)
          (appkit-translate-insert
           (disco-room--translation-source msg) section-prefix-state))
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
  "Print ROW, projecting its mark from the committed Surface model."
  (let* ((msg (appkit-chat-timeline-row-payload row))
         (start (point)))
    (disco-room--insert-message msg (appkit-chat-timeline-row-context row)
                               (appkit-current-surface))
    (when (member (disco-msg-id msg) (disco-msg-marked-ids))
      ;; Exclude date and unread dividers preceding the message.
      (when-let* ((message-start
                  (text-property-not-all start (point) 'disco-message-id nil)))
        (appkit-chat-ins-apply-message-selection message-start (point))))))

(provide 'disco-room-render)

;;; disco-room-render.el ends here
