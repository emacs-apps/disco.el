;;; disco-user.el --- Discord user profile buffers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; A Telega-style user page backed by Discord's full user-profile endpoint.
;; The page follows the Appkit ownership and stale-callback model used by the
;; rest of Disco while retaining the guild context that shapes Discord member
;; profiles.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-chat-avatar)
(require 'appkit-position)
(require 'appkit-ui)
(require 'appkit-view)
(require 'disco-api)
(require 'disco-avatar)
(require 'disco-channel-type)
(require 'disco-markdown)
(require 'disco-runtime)
(require 'disco-state)

(declare-function disco-gateway-current-user "disco-gateway" ())
(declare-function disco-gateway-current-user-id "disco-gateway" ())
(declare-function disco-room-open "disco-room" (channel-id channel-name))
(declare-function disco-channel-directory-open
                  "disco-channel-directory" (guild-id))

(defun disco-user--surface-id (user-id guild-id)
  "Return the Generated Surface identity for USER-ID in GUILD-ID."
  (list 'user-profile user-id guild-id))

(defun disco-user--buffer-name (user-id guild-id)
  "Return the profile buffer name for USER-ID in GUILD-ID context."
  (format "*disco-user:%s%s*"
          user-id
          (if guild-id (format "@%s" guild-id) "")))

(defface disco-user-action-button
  '((t :inherit button :weight semibold))
  "Face for primary actions on a Disco user page."
  :group 'disco)

(defface disco-user-card-title
  '((t :inherit variable-pitch :weight bold :height 1.2))
  "Face for the title on a Disco user page."
  :group 'disco)

(defvar-local disco-user--user-id nil
  "Discord user ID selected in the current profile buffer.")

(defvar-local disco-user--guild-id nil
  "Optional guild context selected in the current profile buffer.")

(defvar-local disco-user--seed-user nil
  "Partial user object available before the full profile response.")

(defvar-local disco-user--profile nil
  "Full Discord user-profile response displayed by this buffer.")

(defvar-local disco-user--loading nil
  "Non-nil while a full profile request is active.")

(defvar-local disco-user--error nil
  "Last full-profile request error, or nil.")

(defvar-local disco-user--request-owner nil
  "Exact owner object for the current full-profile request.")

(defvar-local disco-user--message-owner nil
  "Exact owner object for an active open-DM request.")

(defvar-local disco-user--message-error nil
  "Last open-DM error, or nil.")

(defvar-local disco-user--avatar-hook-function nil
  "View-owned shared-avatar update hook.")

(defun disco-user--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun disco-user--normalize-id (value)
  "Return VALUE as a decimal Discord ID string, or nil."
  (let ((normalized
         (cond ((stringp value) value)
               ((integerp value) (number-to-string value))
               (t nil))))
    (and normalized
         (string-match-p "\\`[0-9]+\\'" normalized)
         (not (string-match-p "\\`0+\\'" normalized))
         normalized)))

(defun disco-user--guild-by-id (guild-id)
  "Return the cached Guild object identified by GUILD-ID, or nil."
  (when-let* ((guild-id (disco-user--normalize-id guild-id)))
    (seq-find
     (lambda (guild)
       (equal
        guild-id (disco-user--normalize-id (alist-get 'id guild))))
     (disco-state-guilds))))

(defun disco-user--guild ()
  "Return the current guild object, or nil."
  (disco-user--guild-by-id disco-user--guild-id))

(defun disco-user--guild-member ()
  "Return the current profile's guild-member object, or cached fallback."
  (or (and (listp disco-user--profile)
           (alist-get 'guild_member disco-user--profile))
      (and disco-user--guild-id
           (disco-state-guild-member disco-user--guild-id disco-user--user-id))))

(defun disco-user--user ()
  "Return the best partial user object for the current profile."
  (or (and (listp disco-user--profile)
           (alist-get 'user disco-user--profile))
      (and (listp (disco-user--guild-member))
           (alist-get 'user (disco-user--guild-member)))
      disco-user--seed-user
      (and (fboundp 'disco-gateway-current-user-id)
           (equal disco-user--user-id (disco-gateway-current-user-id))
           (fboundp 'disco-gateway-current-user)
           (disco-gateway-current-user))))

(defun disco-user--display-name ()
  "Return the best display name for the current profile."
  (let ((member (disco-user--guild-member))
        (user (disco-user--user)))
    (or (disco-user--present-string (and member (alist-get 'nick member)))
        (disco-user--present-string (and user (alist-get 'global_name user)))
        (disco-user--present-string (and user (alist-get 'username user)))
        disco-user--user-id
        "Discord user")))

(defun disco-user--username-label ()
  "Return the current user's Discord username label, or nil."
  (let* ((user (disco-user--user))
         (username (and user (disco-user--present-string
                              (alist-get 'username user))))
         (discriminator (and user (alist-get 'discriminator user))))
    (when username
      (if (and discriminator
               (not (equal (format "%s" discriminator) "0")))
          (format "%s#%s" username discriminator)
        (concat "@" username)))))

(defun disco-user--self-p ()
  "Return non-nil when the current profile belongs to this account."
  (and disco-user--user-id
       (fboundp 'disco-gateway-current-user-id)
       (equal disco-user--user-id (disco-gateway-current-user-id))))

(defun disco-user--profile-key ()
  "Return the current profile's stable presentation key."
  (list 'user-profile disco-user--user-id disco-user--guild-id))

(defun disco-user--header-line ()
  "Return the dynamic header line for the current user page."
  (format " Disco User · %s (%s)%s"
          (disco-user--display-name)
          (or disco-user--user-id "unknown")
          (if disco-user--loading " · loading" "")))

(defun disco-user--avatar-placeholder ()
  "Return a compact text avatar for the current profile."
  (let* ((parts (split-string (disco-user--display-name)
                              "[^[:alnum:]]+" t))
         (first (if parts (substring (car parts) 0 1) "?"))
         (second (if (> (length parts) 1)
                     (substring (cadr parts) 0 1)
                   "")))
    (format "[%s]" (upcase (concat first second)))))

(defun disco-user--avatar-prefixes ()
  "Return Telega-style two-line prefixes for the current user's avatar."
  (let* ((user (disco-user--user))
         (fallback (disco-user--avatar-placeholder))
         (pixel-size (appkit-chat-avatar-two-line-pixel-size))
         (image (and user
                     (disco-avatar-rounded-image user pixel-size))))
    (appkit-chat-avatar-prefixes
     image fallback :pixel-size pixel-size :resize t)))

(defun disco-user--insert-field (label value &optional face)
  "Insert profile LABEL and VALUE when VALUE is present."
  (when (and value (not (equal value "")))
    (let ((start (point)))
      (insert (format "%-18s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (let ((start (point)))
      (insert (format "%s" value) "\n")
      (when face
        (add-text-properties start (point) (list 'face face))))))

(defun disco-user--insert-paragraph (heading text)
  "Insert HEADING and Markdown TEXT when TEXT is present."
  (when-let* ((text (disco-user--present-string text)))
    (insert "\n")
    (appkit-view-insert-heading-line heading :face 'bold)
    (insert (disco-markdown-render text :context 'user-profile) "\n")))

(defun disco-user--snowflake-date (user-id)
  "Return USER-ID's Discord creation date, or nil."
  (when-let* ((user-id (disco-user--normalize-id user-id)))
    (condition-case nil
        (let* ((discord-epoch 1420070400000)
               (milliseconds (+ discord-epoch
                                (ash (string-to-number user-id) -22))))
          (format-time-string "%Y-%m-%d"
                              (seconds-to-time (/ milliseconds 1000.0))))
      (error nil))))

(defun disco-user--guild-member-count (guild)
  "Return GUILD's cached member count, or nil when unavailable."
  (let ((count
         (and (listp guild)
              (or (alist-get 'member_count guild)
                  (alist-get 'approximate_member_count guild)))))
    (and (integerp count) (>= count 0) count)))

(defun disco-user--mutual-guilds ()
  "Return the current profile's mutual Guild objects."
  (let ((guilds (and (listp disco-user--profile)
                     (alist-get 'mutual_guilds disco-user--profile))))
    (and (listp guilds) guilds)))

(defun disco-user--open-mutual-guild (button)
  "Open the channel directory identified by mutual Guild BUTTON."
  (disco-channel-directory-open (button-get button 'disco-guild-id)))

(defun disco-user--insert-mutual-guilds ()
  "Insert each mutual Guild as an independently navigable Telega-style row."
  (when-let* ((guilds (disco-user--mutual-guilds)))
    (disco-user--insert-field "Mutual servers" (length guilds))
    (dolist (mutual-guild guilds)
      (when-let* ((guild-id
                   (disco-user--normalize-id
                    (alist-get 'id mutual-guild))))
        (let* ((guild (disco-user--guild-by-id guild-id))
               (name
                (or (and guild
                         (disco-user--present-string
                          (alist-get 'name guild)))
                    guild-id))
               (nick
                (disco-user--present-string
                 (alist-get 'nick mutual-guild)))
               (member-count (disco-user--guild-member-count guild))
               (count-label
                (and member-count
                     (disco-title-compact-count member-count)))
               (brackets (disco-title-brackets 'guild)))
          (insert "  " (car brackets))
          (insert-text-button name
                              'follow-link
                              t
                              'action
                              #'disco-user--open-mutual-guild
                              'disco-guild-id
                              guild-id
                              'help-echo
                              "Open this server's channel directory")
          (when nick
            (insert (propertize (format " · %s" nick) 'face 'shadow)))
          (when count-label
            (let* ((width
                    (or (appkit-view-window-fill-column nil 2)
                        fill-column
                        80))
                   (target
                    (- width (string-width count-label)
                       (string-width (cadr brackets)))))
              (if (> target (appkit-view-current-column))
                  (appkit-view-move-to-column target)
                (insert " "))
              (insert (propertize count-label 'face 'shadow))))
          (insert (cadr brackets) "\n"))))))

(defun disco-user--mutual-friends ()
  "Return partial User objects for the current profile's mutual friends."
  (let ((friends
         (and (listp disco-user--profile)
              (alist-get 'mutual_friends disco-user--profile))))
    (and (listp friends) friends)))

(defun disco-user--mutual-friend-label (friend)
  "Return a readable display label for partial User object FRIEND."
  (let ((display-name
         (disco-user--present-string (alist-get 'global_name friend)))
        (username
         (disco-user--present-string (alist-get 'username friend)))
        (user-id (disco-user--normalize-id (alist-get 'id friend))))
    (or display-name
        (and username (concat "@" username))
        user-id
        "Unknown user")))

(defun disco-user--open-mutual-friend (button)
  "Open the partial User object stored on mutual-friend BUTTON."
  (when-let* ((friend (button-get button 'disco-user-object)))
    (disco-user-open friend)))

(defun disco-user--insert-mutual-friends ()
  "Insert mutual-friend count and available user navigation rows."
  (let* ((friends (disco-user--mutual-friends))
         (reported-count
          (and (listp disco-user--profile)
               (alist-get 'mutual_friends_count disco-user--profile)))
         (count
          (if (integerp reported-count)
              reported-count
            (and friends (length friends)))))
    (when (integerp count)
      (disco-user--insert-field "Mutual friends" count))
    (dolist (friend friends)
      (when (and (listp friend)
                 (disco-user--normalize-id (alist-get 'id friend)))
        (let* ((display-name
                (disco-user--present-string
                 (alist-get 'global_name friend)))
               (username
                (disco-user--present-string
                 (alist-get 'username friend)))
               (label
                (concat
                 (disco-user--mutual-friend-label friend)
                 (if (and display-name
                          username
                          (not (equal display-name username)))
                     (format " · @%s" username)
                   ""))))
          (insert "  {")
          (insert-text-button label
                              'follow-link
                              t
                              'action
                              #'disco-user--open-mutual-friend
                              'disco-user-object
                              friend
                              'help-echo
                              "Open this mutual friend's profile")
          (insert "}\n"))))))

(defun disco-user--badge-label ()
  "Return descriptions for global and guild profile badges."
  (let ((badges (append (and (listp disco-user--profile)
                             (alist-get 'badges disco-user--profile))
                        (and (listp disco-user--profile)
                             (alist-get 'guild_badges disco-user--profile)))))
    (when badges
      (string-join
       (delete-dups
        (delq nil
              (mapcar (lambda (badge)
                        (or (disco-user--present-string
                             (alist-get 'description badge))
                            (disco-user--present-string
                             (alist-get 'id badge))))
                      badges)))
       " · "))))

(defun disco-user--connections-label ()
  "Return a concise label for public connected accounts."
  (when-let* ((connections
               (and (listp disco-user--profile)
                    (alist-get 'connected_accounts disco-user--profile)))
              ((listp connections))
              ((not (null connections))))
    (string-join
     (delq nil
           (mapcar
            (lambda (connection)
              (let ((type (disco-user--present-string
                           (alist-get 'type connection)))
                    (name (disco-user--present-string
                           (alist-get 'name connection))))
                (cond ((and type name) (format "%s: %s" type name))
                      (name name)
                      (type type))))
            connections))
     " · ")))

(defun disco-user--presence-status ()
  "Return the current user's cached presence status, or nil."
  (when-let* ((presence (disco-state-presence
                         disco-user--user-id disco-user--guild-id)))
    (disco-user--present-string (alist-get 'status presence))))

(defun disco-user--insert-action-buttons ()
  "Insert the primary user action row."
  (insert "  ")
  (unless (disco-user--self-p)
    (appkit-ui-insert-action-button
     (if disco-user--message-owner " Opening DM… " " Message ")
     #'disco-user-open-chat
     :face 'disco-user-action-button
     :help-echo "Open a direct message (m)")
    (insert "  "))
  (appkit-ui-insert-action-button
   " Copy ID " #'disco-user-copy-id
   :face 'disco-user-action-button
   :help-echo "Copy Discord user ID (Y)")
  (insert "\n"))

(defun disco-user-render ()
  "Render the current Discord user profile."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t)
           (user (disco-user--user)))
       (erase-buffer)
       (setq-local header-line-format '(:eval (disco-user--header-line)))
       (if (and disco-user--loading (null user))
           (appkit-view-insert-note-line "Loading user profile…")
         (let* ((prefixes (disco-user--avatar-prefixes))
                (header-prefix (plist-get prefixes :header))
                (status-prefix (plist-get prefixes :first-body))
                (status (disco-user--presence-status)))
           (insert header-prefix
                   (propertize (disco-user--display-name)
                               'face 'disco-user-card-title))
           (when-let* ((username (disco-user--username-label)))
             (insert (propertize (format " · %s" username) 'face 'shadow)))
           (insert "\n" status-prefix)
           (when status
             (insert (propertize (capitalize status) 'face 'shadow)))
           (insert "\n"))
         (disco-user--insert-action-buttons)
         (when disco-user--loading
           (appkit-view-insert-note-line
            "Loading full user profile…" :face 'shadow))
         (when disco-user--error
           (appkit-view-insert-note-line disco-user--error :face 'error))
         (when disco-user--message-error
           (appkit-view-insert-note-line disco-user--message-error :face 'error))
         (appkit-view-insert-note-line
          "g refresh · m message · Y copy ID · q quit" :face 'shadow)
         (insert "\n")
         (disco-user--insert-field "User ID" disco-user--user-id)
         (disco-user--insert-field
          "Account created" (disco-user--snowflake-date disco-user--user-id))
         (when (eq t (and user (alist-get 'bot user)))
           (disco-user--insert-field "Account type" "Bot"))
         (let* ((member (disco-user--guild-member))
                (member-profile
                 (and (listp disco-user--profile)
                      (alist-get 'guild_member_profile disco-user--profile))))
           (when (or disco-user--guild-id member member-profile)
             (insert "\n")
             (appkit-view-insert-heading-line
              "Server profile"
              :face 'bold)
             (disco-user--insert-field
              "Server"
              (or (and (disco-user--guild)
                       (alist-get 'name (disco-user--guild)))
                  disco-user--guild-id))
             (disco-user--insert-field
              "Nickname" (and member (alist-get 'nick member)))
             (disco-user--insert-field
              "Pronouns" (and member-profile
                              (alist-get 'pronouns member-profile)))
             (disco-user--insert-field "Joined" (and member (alist-get 'joined_at member)))
             (disco-user--insert-field
              "Boosting since" (and member (alist-get 'premium_since member)))
             (disco-user--insert-paragraph
              "Server about me"
              (or (and member-profile (alist-get 'bio member-profile))
                  (and member (alist-get 'bio member))))))
         (let* ((user-profile
                 (and (listp disco-user--profile)
                      (alist-get 'user_profile disco-user--profile)))
                (global-bio
                 (or (and user-profile (alist-get 'bio user-profile))
                     (and user (alist-get 'bio user)))))
           (when (or user-profile global-bio)
             (insert "\n")
             (appkit-view-insert-heading-line "User profile" :face 'bold)
             (disco-user--insert-field
              "Pronouns" (and user-profile
                              (alist-get 'pronouns user-profile)))
             (disco-user--insert-paragraph "About me" global-bio)))
         (disco-user--insert-field "Badges" (disco-user--badge-label))
         (disco-user--insert-mutual-guilds)
         (disco-user--insert-mutual-friends)
         (disco-user--insert-field
          "Connections" (disco-user--connections-label))
         (when (eq t (and (listp disco-user--profile)
                          (alist-get 'private disco-user--profile)))
           (insert "\n")
           (appkit-view-insert-note-line
            "This user has a private extended profile." :face 'shadow))
         (when-let* ((application
                      (and (listp disco-user--profile)
                           (alist-get 'application disco-user--profile)))
                     (description
                      (disco-user--present-string
                       (alist-get 'description application))))
           (disco-user--insert-paragraph "Bot application" description))
         (insert "\n"))
       (add-text-properties
        (point-min) (point-max)
        (list 'disco-user-profile-key (disco-user--profile-key)
              'rear-nonsticky '(disco-user-profile-key)))
       (goto-char (point-min))))
   :anchor-property 'disco-user-profile-key
   :preserve-window-start t))

(defun disco-user--surface-current-p (surface)
  "Return non-nil when SURFACE owns this user-profile context."
  (and (appkit-surface-live-p surface)
       (with-current-buffer (appkit-surface-buffer surface)
         (and (derived-mode-p 'disco-user-mode)
              (eq surface (appkit-current-surface))
              disco-user--user-id
              (equal (appkit-surface-identity surface)
                     (disco-user--surface-id
                      disco-user--user-id disco-user--guild-id))))))

(defun disco-user--live-current-surface ()
  "Return the live Generated Surface attached to this user buffer."
  (let ((surface (appkit-current-surface)))
    (and (disco-user--surface-current-p surface) surface)))

(cl-defun disco-user--request-sync (&optional surface &key resource)
  "Queue one coalesced render for live SURFACE."
  (ignore resource)
  (when-let* ((surface (or surface (disco-user--live-current-surface))))
    (appkit-surface-post surface 'render)))

(defun disco-user--sync-now (surface)
  "Synchronously commit queued profile renders for SURFACE."
  (when (disco-user--surface-current-p surface)
    (appkit-surface-send surface 'flush)))

(defun disco-user--render-surface (_surface)
  "Render the current user profile in its exact Surface host."
  (disco-user-render))

(defun disco-user--request-current-p
    (surface buffer user-id guild-id owner)
  "Return non-nil when OWNER still loads USER-ID for SURFACE."
  (and (disco-user--surface-current-p surface)
       (eq (appkit-surface-buffer surface) buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (equal disco-user--user-id user-id)
              (equal disco-user--guild-id guild-id)
              (eq disco-user--request-owner owner)))))

(defun disco-user-refresh ()
  "Refresh the current Discord user profile."
  (interactive)
  (unless disco-user--user-id
    (user-error "disco: this buffer has no user identity"))
  (let* ((surface (or (disco-user--live-current-surface)
                   (error "Disco: user buffer has no live Appkit Surface")))
         (buffer (current-buffer))
         (user-id disco-user--user-id)
         (guild-id disco-user--guild-id)
         (owner (list 'user-profile user-id guild-id)))
    (setq disco-user--loading t
          disco-user--error nil
          disco-user--request-owner owner)
    (disco-user--request-sync surface)
    (condition-case error-data
        (disco-api-user-profile-async
         user-id
         :guild-id guild-id
         :on-success
         (lambda (profile)
           (when (disco-user--request-current-p
                  surface buffer user-id guild-id owner)
             (with-current-buffer buffer
               (setq disco-user--profile profile
                     disco-user--loading nil
                     disco-user--error nil
                     disco-user--request-owner nil)
               (disco-user--request-sync surface))))
         :on-error
         (lambda (error-info)
           (when (disco-user--request-current-p
                  surface buffer user-id guild-id owner)
             (with-current-buffer buffer
               (setq disco-user--loading nil
                     disco-user--error
                     (format "Unable to load profile: %s"
                             (or (plist-get error-info :message)
                                 "unknown error"))
                     disco-user--request-owner nil)
               (disco-user--request-sync surface)))))
      (error
       (when (disco-user--request-current-p
              surface buffer user-id guild-id owner)
         (setq disco-user--loading nil
               disco-user--error
               (format "Unable to load profile: %s"
                       (error-message-string error-data))
               disco-user--request-owner nil)
         (disco-user--request-sync surface))))
    (disco-user--sync-now surface)))

(defun disco-user--message-current-p
    (surface buffer user-id guild-id owner)
  "Return non-nil when OWNER still opens USER-ID's DM for SURFACE."
  (and (disco-user--surface-current-p surface)
       (eq (appkit-surface-buffer surface) buffer)
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (equal disco-user--user-id user-id)
              (equal disco-user--guild-id guild-id)
              (eq disco-user--message-owner owner)))))

(defun disco-user-open-chat ()
  "Create or open a direct-message channel with the current user."
  (interactive)
  (unless disco-user--user-id
    (user-error "disco: this buffer has no user identity"))
  (when (disco-user--self-p)
    (user-error "disco: cannot open a direct message with yourself"))
  (when disco-user--message-owner
    (user-error "disco: direct message is already opening"))
  (let* ((surface (or (disco-user--live-current-surface)
                   (error "Disco: user buffer has no live Appkit Surface")))
         (buffer (current-buffer))
         (user-id disco-user--user-id)
         (guild-id disco-user--guild-id)
         (channel-name (disco-user--display-name))
         (owner (list 'open-private-channel user-id guild-id)))
    (setq disco-user--message-owner owner
          disco-user--message-error nil)
    (disco-user--request-sync surface)
    (condition-case error-data
        (disco-api-create-private-channel-async
         user-id
         :on-success
         (lambda (channel)
           (when (disco-user--message-current-p
                  surface buffer user-id guild-id owner)
             (with-current-buffer buffer
               (setq disco-user--message-owner nil
                     disco-user--message-error nil)
               (disco-user--request-sync surface))
             (disco-state-upsert-channel channel)
             (if-let* ((channel-id
                        (disco-user--normalize-id (alist-get 'id channel))))
                 (disco-room-open channel-id channel-name)
               (message "disco: private-channel response has no channel ID"))))
         :on-error
         (lambda (error-info)
           (when (disco-user--message-current-p
                  surface buffer user-id guild-id owner)
             (with-current-buffer buffer
               (setq disco-user--message-owner nil
                     disco-user--message-error
                     (format "Unable to open direct message: %s"
                             (or (plist-get error-info :message)
                                 "unknown error")))
               (disco-user--request-sync surface)))))
      (error
       (when (disco-user--message-current-p
              surface buffer user-id guild-id owner)
         (setq disco-user--message-owner nil
               disco-user--message-error
               (format "Unable to open direct message: %s"
                       (error-message-string error-data)))
         (disco-user--request-sync surface))))
    (disco-user--sync-now surface)))

(defun disco-user-copy-id ()
  "Copy the current profile's Discord user ID."
  (interactive)
  (unless disco-user--user-id
    (user-error "disco: this buffer has no user identity"))
  (kill-new disco-user--user-id)
  (message "disco: copied user ID %s" disco-user--user-id))

(defun disco-user-button-backward ()
  "Move point to the previous user-page button."
  (interactive)
  (forward-button -1))

(defun disco-user--clear-surface-data ()
  "Clear profile data and request ownership from this Surface host."
  (setq disco-user--user-id nil
        disco-user--guild-id nil
        disco-user--profile nil
        disco-user--seed-user nil
        disco-user--loading nil
        disco-user--error nil
        disco-user--request-owner nil
        disco-user--message-owner nil
        disco-user--message-error nil))

(defun disco-user--detach-surface ()
  "Remove avatar observation and clear this user Surface host."
  (when disco-user--avatar-hook-function
    (remove-hook 'disco-avatar-resources-updated-hook
                 disco-user--avatar-hook-function)
    (setq disco-user--avatar-hook-function nil))
  (disco-user--clear-surface-data))

(defun disco-user--handle-avatar-updates (surface resources)
  "Request a render when RESOURCES include SURFACE's avatar."
  (when (and (disco-user--surface-current-p surface) (listp resources))
    (with-current-buffer (appkit-surface-buffer surface)
      (when-let* ((user (disco-user--user))
                  (resource (disco-avatar-resource-key user))
                  ((member resource resources)))
        (disco-user--request-sync surface :resource resource)))))

(defun disco-user--attach-surface (surface)
  "Attach avatar resource observation to SURFACE's lifecycle."
  (let ((hook (apply-partially
               #'disco-user--handle-avatar-updates surface)))
    (setq-local disco-user--avatar-hook-function hook)
    (add-hook 'disco-avatar-resources-updated-hook hook)))

(defun disco-user--bind-context (user-id guild-id seed-user)
  "Bind this Surface host to USER-ID, GUILD-ID, and SEED-USER."
  (when (and disco-user--user-id
             (not (and (equal disco-user--user-id user-id)
                       (equal disco-user--guild-id guild-id))))
    (error "Disco: user Surface identity does not match its context"))
  (setq disco-user--user-id user-id
        disco-user--guild-id guild-id)
  (when seed-user
    (setq disco-user--seed-user (copy-tree seed-user))))

(defvar-keymap disco-user-mode-map
  :doc "Keymap for `disco-user-mode'."
  "g" #'disco-user-refresh
  "m" #'disco-user-open-chat
  "Y" #'disco-user-copy-id
  "TAB" #'forward-button
  "<backtab>" #'disco-user-button-backward
  "q" #'quit-window)

(defun disco-user--surface-init (_context input)
  "Initialize a user profile Surface from INPUT."
  (disco-user--clear-surface-data)
  (disco-user--bind-context
   (plist-get input :user-id)
   (plist-get input :guild-id)
   (plist-get input :seed-user))
  (appkit-next
   :model (list (plist-get input :user-id) (plist-get input :guild-id))
   :render 'full))

(defun disco-user--surface-update (_context model message)
  "Return the render disposition requested by MESSAGE."
  (pcase message
    ('render (appkit-next :model model :render 'full))
    ('flush (appkit-next :model model :render appkit-render-none))
    (_ (appkit-next-reject 'invalid-user-surface-message))))

(defun disco-user--surface-renderer (_surface)
  "Create one Generated Renderer for a user profile."
  (appkit-generated-renderer-create
   :mount (lambda (surface _app-read-view _model)
            (disco-user--attach-surface surface))
   :merge (lambda (_left _right) 'full)
   :render (lambda (surface _app-read-view _model _request)
             (disco-user--render-surface surface))
   :recover nil
   :unmount (lambda (_surface) (disco-user--detach-surface))))

(define-derived-mode disco-user-mode special-mode "Disco-User"
  "Major mode for a Discord user profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (disco-user--header-line)))
  (buffer-disable-undo)
  (setq-local buffer-undo-list t))

(defconst disco-user--surface-type
  (appkit-surface-type-create
   :name 'disco-user-profile
   :mode #'disco-user-mode
   :init #'disco-user--surface-init
   :update #'disco-user--surface-update
   :renderer-factory #'disco-user--surface-renderer)
  "Generated Surface type for Discord user profiles.")

;;;###autoload
(defun disco-user-open (user-or-id &optional guild-id)
  "Open USER-OR-ID's Generated profile Surface in GUILD-ID context."
  (interactive (list (read-string "Discord user ID: " ) nil))
  (let* ((seed-user (and (listp user-or-id) user-or-id))
         (user-id
          (disco-user--normalize-id
           (if seed-user (alist-get 'id seed-user) user-or-id)))
         (guild-id (and guild-id (disco-user--normalize-id guild-id))))
    (unless user-id
      (user-error "disco: user profile requires a decimal Discord user ID"))
    (let* ((app (disco-runtime-app))
           (identity (disco-user--surface-id user-id guild-id))
           (existing (appkit-app-surface app identity))
           (surface
            (cond
             ((appkit-surface-live-p existing)
              (pop-to-buffer (appkit-surface-buffer existing))
              existing)
             (existing
              (error "Disco: user profile is unavailable: %S"
                     (appkit-surface-status existing)))
             (t
              (appkit-open-generated-surface
               disco-user--surface-type
               :app app :identity identity
               :input (list :user-id user-id :guild-id guild-id
                            :seed-user seed-user)
               :buffer-name (disco-user--buffer-name user-id guild-id)
               :select t))))
           (buffer (appkit-surface-buffer surface)))
      (when (and (not existing)
                 (with-current-buffer buffer
                   (and (null disco-user--profile)
                        (not disco-user--loading))))
        (with-current-buffer buffer (disco-user-refresh)))
      buffer)))

(provide 'disco-user)

;;; disco-user.el ends here
