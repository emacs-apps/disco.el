;;; disco-company.el --- Composer completion backends for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Completion helpers for room composer tokens (`@' and `#').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'disco-state)

(defvar company-mode)
(defvar company-backends)
(defvar completion-at-point-functions)
(defvar disco-room-enable-company-backend)
(defvar disco-room-show-avatar-images)

(defcustom disco-company-show-user-avatars t
  "When non-nil, show user avatars in company completion annotations."
  :type 'boolean
  :group 'disco)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--typing-users nil)
(defvar-local disco-room--input-marker nil)
(defvar-local disco-room--draft-input "")

(defun disco-company--as-list (value)
  "Return VALUE normalized as a proper list."
  (cond
   ((null value) nil)
   ((listp value) value)
   (t (list value))))

(defun disco-company-setup-room-buffer ()
  "Install CAPF/company completion hooks for current room buffer."
  (let* ((existing-capf (disco-company--as-list completion-at-point-functions))
         (filtered-capf (cl-remove #'disco-room-complete-at-point
                                   existing-capf
                                   :test #'eq)))
    (setq-local completion-at-point-functions
                (cons #'disco-room-complete-at-point filtered-capf)))
  (when (and disco-room-enable-company-backend
             (featurep 'company)
             (boundp 'company-backends))
    (let* ((existing-backends (disco-company--as-list company-backends))
           (filtered-backends (cl-remove 'disco-room-company-completion
                                         existing-backends
                                         :test #'equal)))
      (setq-local company-backends
                  (cons 'disco-room-company-completion filtered-backends)))))

(defun disco-company--input-region-bounds ()
  "Return writable room draft region as (START . END), or nil."
  (when (and (markerp disco-room--input-marker)
             (eq (marker-buffer disco-room--input-marker) (current-buffer)))
    (let ((start (marker-position disco-room--input-marker)))
      (when (<= start (point-max))
        (cons start (point-max))))))

(defun disco-company--point-in-input-p (&optional position)
  "Return non-nil when POSITION (or point) is inside the room draft input."
  (let* ((bounds (disco-company--input-region-bounds))
         (pos (or position (point))))
    (and bounds
         (<= (car bounds) pos)
         (<= pos (cdr bounds)))))

(defun disco-company--normalize-id (value)
  "Return normalized snowflake-like ID string from VALUE, or nil."
  (let ((normalized
         (cond
          ((stringp value) (string-trim value))
          ((integerp value) (number-to-string value))
          (t nil))))
    (when (and (stringp normalized)
               (not (string-empty-p normalized)))
      normalized)))

(defun disco-company--normalize-list-sequence (value)
  "Normalize VALUE into a list, preserving list/vector elements."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun disco-company--channel-object ()
  "Return current room channel object from state."
  (disco-state-channel disco-room--channel-id))

(defun disco-company--thread-channel-p (&optional channel)
  "Return non-nil when CHANNEL (or current room channel) is a thread."
  (let ((target (or channel (disco-company--channel-object))))
    (and target (disco-state-channel-thread-p target))))

(defun disco-company--guild-by-id (guild-id)
  "Return guild object for GUILD-ID, or nil."
  (when (and (stringp guild-id) (not (string-empty-p guild-id)))
    (seq-find (lambda (guild)
                (equal (alist-get 'id guild) guild-id))
              (or (disco-state-guilds) '()))))

(defun disco-company--sync-room-draft ()
  "Sync room draft state after completion insertion."
  (if (fboundp 'disco-room--sync-draft-from-buffer)
      (funcall #'disco-room--sync-draft-from-buffer)
    (let ((bounds (disco-company--input-region-bounds)))
      (when bounds
        (setq-local disco-room--draft-input
                    (replace-regexp-in-string
                     "[\n\r]+\\'"
                     ""
                     (buffer-substring-no-properties (car bounds) (cdr bounds))))))))

(defun disco-company--completion-token-boundary-p (char)
  "Return non-nil when CHAR is a valid left boundary for token completion."
  (or (null char)
      (let ((syntax (char-syntax char)))
        (not (memq syntax '(?w ?_))))))

(defun disco-company--completion-token-bounds ()
  "Return completion token metadata at point, or nil.

Returned plist contains :start, :end, :trigger, :raw and :query.
Supported trigger characters are `@' and `#'."
  (when (disco-company--point-in-input-p)
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward "A-Za-z0-9._-")
        (let ((trigger (char-before)))
          (when (memq trigger '(?@ ?#))
            (let* ((start (1- (point)))
                   (left (char-before start)))
              (when (disco-company--completion-token-boundary-p left)
                (let ((raw (buffer-substring-no-properties start end)))
                  (list :start start
                        :end end
                        :trigger trigger
                        :raw raw
                        :query (buffer-substring-no-properties (1+ start) end)))))))))))

(defun disco-company--completion-string-present-p (value)
  "Return non-nil when VALUE is a non-empty trimmed string."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun disco-company--completion-first-present (&rest values)
  "Return first non-empty trimmed string from VALUES, or nil."
  (seq-find #'disco-company--completion-string-present-p
            (mapcar (lambda (value)
                      (and (stringp value)
                           (string-trim value)))
                    values)))

(defun disco-company--completion-short-id (id)
  "Return short suffix label for snowflake-like ID."
  (let ((text (or (disco-company--normalize-id id) "????")))
    (if (> (length text) 4)
        (substring text (- (length text) 4))
      text)))

(defun disco-company--completion-disambiguate-label (base hint seen-labels)
  "Return unique completion label from BASE using HINT against SEEN-LABELS."
  (let* ((seed (or base ""))
         (suffix (if (disco-company--completion-string-present-p hint)
                     (format " (%s)" hint)
                   ""))
         (index 0)
         candidate)
    (setq candidate seed)
    (while (gethash candidate seen-labels)
      (setq index (1+ index))
      (setq candidate
            (if (= index 1)
                (format "%s%s" seed suffix)
              (format "%s%s#%d" seed suffix index))))
    (puthash candidate t seen-labels)
    candidate))

(defun disco-company--completion-make-candidate (label insert annotation sort-key &rest props)
  "Build one completion candidate plist.

PROPS is appended as additional plist metadata."
  (append (list :label label :insert insert :annotation annotation :sort-key sort-key)
          props))

(cl-defun disco-company--completion--merge-user
    (table user-id &key username global-name nick avatar display-name)
  "Merge one user record into TABLE by USER-ID."
  (let ((id (disco-company--normalize-id user-id)))
    (when id
      (let ((entry (copy-sequence (or (gethash id table)
                                      (list :id id)))))
        (dolist (field-value (list (cons :username username)
                                   (cons :global-name global-name)
                                   (cons :nick nick)
                                   (cons :avatar avatar)))
          (let* ((field (car field-value))
                 (raw-value (cdr field-value))
                 (value (and (stringp raw-value) (string-trim raw-value))))
            (when (disco-company--completion-string-present-p value)
              (setq entry (plist-put entry field value)))))
        (let ((display-rank (or (plist-get entry :display-rank) 0))
              (display (plist-get entry :display-name)))
          (dolist (choice `((40 . ,nick)
                            (30 . ,global-name)
                            (20 . ,display-name)
                            (10 . ,username)))
            (let* ((rank (car choice))
                   (raw-value (cdr choice))
                   (value (and (stringp raw-value) (string-trim raw-value))))
              (when (and (disco-company--completion-string-present-p value)
                         (> rank display-rank))
                (setq display-rank rank)
                (setq display value))))
          (setq entry (plist-put entry :display-rank display-rank))
          (when (disco-company--completion-string-present-p display)
            (setq entry (plist-put entry :display-name display))))
        (puthash id entry table)))))

(defun disco-company--completion-user-annotation-text (username user-id)
  "Return annotation text for a user completion row."
  (format " user %s | id:%s"
          (if (disco-company--completion-string-present-p username)
              (format "@%s" username)
            "@unknown")
          user-id))

(defun disco-company--completion-collect-user-name-map ()
  "Return hash table of user-id -> metadata plist for current room context."
  (let ((table (make-hash-table :test #'equal)))
    (let* ((channel (disco-company--channel-object))
           (recipients (and (listp channel)
                            (disco-company--normalize-list-sequence
                             (alist-get 'recipients channel)))))
      (dolist (recipient recipients)
        (when (listp recipient)
          (disco-company--completion--merge-user
           table
           (alist-get 'id recipient)
           :username (alist-get 'username recipient)
           :global-name (alist-get 'global_name recipient)
           :avatar (alist-get 'avatar recipient)))))
    (when (hash-table-p disco-room--typing-users)
      (maphash
       (lambda (user-id entry)
         (disco-company--completion--merge-user
          table
          user-id
          :display-name (and (listp entry)
                             (plist-get entry :display-name))))
       disco-room--typing-users))
    (dolist (msg (or (disco-state-messages disco-room--channel-id) '()))
      (let* ((author (alist-get 'author msg))
             (member (alist-get 'member msg)))
        (when (listp author)
          (disco-company--completion--merge-user
           table
           (alist-get 'id author)
           :username (alist-get 'username author)
           :global-name (alist-get 'global_name author)
           :nick (and (listp member) (alist-get 'nick member))
           :avatar (alist-get 'avatar author)))))
    (when (disco-company--thread-channel-p)
      (dolist (user-id (or (disco-state-thread-member-ids disco-room--channel-id) '()))
        (disco-company--completion--merge-user table user-id)))
    table))

(defun disco-company--completion-user-candidates ()
  "Return `@' user mention candidates for current room context."
  (let ((user-map (disco-company--completion-collect-user-name-map))
        (seen-labels (make-hash-table :test #'equal))
        out)
    (maphash
     (lambda (user-id entry)
       (let* ((display-name (or (and (listp entry) (plist-get entry :display-name))
                                (format "user-%s" (disco-company--completion-short-id user-id))))
              (username (and (listp entry) (plist-get entry :username)))
              (avatar-hash (and (listp entry) (plist-get entry :avatar)))
              (label (disco-company--completion-disambiguate-label
                      (format "@%s" display-name)
                      (disco-company--completion-short-id user-id)
                      seen-labels)))
         (push (disco-company--completion-make-candidate
                label
                (format "<@%s>" user-id)
                (disco-company--completion-user-annotation-text username user-id)
                (downcase display-name)
                :kind 'user
                :user-id user-id
                :display-name display-name
                :username username
                :avatar-hash avatar-hash)
               out)))
     user-map)
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-company--completion-role-candidates ()
  "Return `@' role mention candidates from current guild state."
  (let* ((guild-id (disco-company--normalize-id disco-room--guild-id))
         (guild (and guild-id (disco-company--guild-by-id guild-id)))
         (roles (and (listp guild)
                     (disco-company--normalize-list-sequence
                      (alist-get 'roles guild))))
         (seen-labels (make-hash-table :test #'equal))
         out)
    (dolist (role roles)
      (let* ((role-id (disco-company--normalize-id (and (listp role) (alist-get 'id role))))
             (role-name (and (listp role) (alist-get 'name role)))
             (name (and (stringp role-name) (string-trim role-name))))
        (when (and role-id
                   (disco-company--completion-string-present-p name)
                   (not (equal role-id guild-id)))
          (let ((label (disco-company--completion-disambiguate-label
                        (format "@%s" name)
                        (disco-company--completion-short-id role-id)
                        seen-labels)))
            (push (disco-company--completion-make-candidate
                   label
                   (format "<@&%s>" role-id)
                   " role"
                   (downcase name)
                   :kind 'role
                   :role-id role-id)
                  out)))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-company--completion-special-candidates ()
  "Return special `@everyone' and `@here' candidates for guild channels."
  (when (disco-company--normalize-id disco-room--guild-id)
    (list (disco-company--completion-make-candidate
           "@everyone" "@everyone" " special" "@everyone"
           :kind 'special)
          (disco-company--completion-make-candidate
           "@here" "@here" " special" "@here"
           :kind 'special))))

(defun disco-company--completion-channel-type-label (channel)
  "Return human-readable channel type label for CHANNEL."
  (pcase (and (listp channel) (alist-get 'type channel))
    (0 " text")
    (2 " voice")
    (5 " announcement")
    ((or 10 11 12) " thread")
    (13 " stage")
    (15 " forum")
    (16 " media")
    (_ " channel")))

(defun disco-company--completion-mentionable-channel-p (channel)
  "Return non-nil when CHANNEL should be offered for `#' completion."
  (let ((type (and (listp channel) (alist-get 'type channel))))
    (and (listp channel)
         (not (eq type 4))
         (disco-company--completion-string-present-p (alist-get 'name channel))
         (disco-company--normalize-id (alist-get 'id channel)))))

(defun disco-company--completion-channel-candidates ()
  "Return `#' channel mention candidates from current guild state."
  (let* ((guild-id (disco-company--normalize-id disco-room--guild-id))
         (channels (and guild-id
                        (disco-state-guild-channels guild-id)))
         (seen-ids (make-hash-table :test #'equal))
         (seen-labels (make-hash-table :test #'equal))
         out)
    (dolist (channel (or channels '()))
      (when (disco-company--completion-mentionable-channel-p channel)
        (let* ((channel-id (disco-company--normalize-id (alist-get 'id channel)))
               (name (string-trim (alist-get 'name channel))))
          (unless (gethash channel-id seen-ids)
            (puthash channel-id t seen-ids)
            (let ((label (disco-company--completion-disambiguate-label
                          (format "#%s" name)
                          (disco-company--completion-short-id channel-id)
                          seen-labels)))
              (push (disco-company--completion-make-candidate
                     label
                     (format "<#%s>" channel-id)
                     (disco-company--completion-channel-type-label channel)
                     (downcase name)
                     :kind 'channel
                     :channel-id channel-id)
                    out))))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-company--completion-uniquify-candidates (candidates)
  "Return CANDIDATES with globally unique display labels."
  (let ((seen-labels (make-hash-table :test #'equal))
        out)
    (dolist (candidate candidates)
      (let* ((copy (copy-sequence candidate))
             (label (or (plist-get copy :label) ""))
             (hint (or (and (plist-get copy :user-id)
                            (disco-company--completion-short-id
                             (plist-get copy :user-id)))
                       (and (plist-get copy :role-id)
                            (disco-company--completion-short-id
                             (plist-get copy :role-id)))
                       (and (plist-get copy :channel-id)
                            (disco-company--completion-short-id
                             (plist-get copy :channel-id)))
                       (string-trim (or (plist-get copy :annotation) ""))))
             (unique (disco-company--completion-disambiguate-label
                      label
                      hint
                      seen-labels)))
        (setq copy (plist-put copy :label unique))
        (push copy out)))
    (nreverse out)))

(defun disco-company--completion-candidates-for-trigger (trigger)
  "Return completion candidates for TRIGGER in current room context."
  (disco-company--completion-uniquify-candidates
   (pcase trigger
     (?@ (append (or (disco-company--completion-special-candidates) '())
                 (disco-company--completion-role-candidates)
                 (disco-company--completion-user-candidates)))
     (?# (disco-company--completion-channel-candidates))
     (_ nil))))

(defun disco-company--completion-filter-candidates (candidates prefix)
  "Return CANDIDATES matching PREFIX case-insensitively."
  (let ((needle (downcase (or prefix ""))))
    (if (string-empty-p needle)
        candidates
      (seq-filter
       (lambda (candidate)
         (let* ((label (downcase (or (plist-get candidate :label) "")))
                (username (downcase (or (plist-get candidate :username) "")))
                (user-id (downcase (or (plist-get candidate :user-id) "")))
                (username-with-at (if (string-empty-p username)
                                      ""
                                    (format "@%s" username)))
                (user-id-with-at (if (string-empty-p user-id)
                                     ""
                                   (format "@%s" user-id))))
           (or (string-prefix-p needle label)
               (and (not (string-empty-p username-with-at))
                    (string-prefix-p needle username-with-at))
               (and (not (string-empty-p user-id))
                    (or (string-prefix-p needle user-id)
                        (string-prefix-p needle user-id-with-at))))))
       candidates))))

(defun disco-company--completion-apply-candidate (completed replacement)
  "Replace COMPLETED text before point with mention REPLACEMENT.

Return non-nil when replacement was applied."
  (let ((choice (and (stringp completed)
                     (substring-no-properties completed))))
    (when (and (disco-company--completion-string-present-p choice)
               (disco-company--completion-string-present-p replacement))
      (let ((end (point))
            (start (- (point) (length choice))))
        (when (and (>= start (point-min))
                   (<= end (point-max))
                   (string= (buffer-substring-no-properties start end)
                            choice))
          (delete-region start end)
          (insert replacement)
          (unless (or (eobp)
                      (memq (char-after) '(?\s ?\t ?\n ?\r)))
            (insert " "))
          (disco-company--sync-room-draft)
          t)))))

(defun disco-company--completion-user-initials (candidate)
  "Return text avatar initials for user CANDIDATE."
  (let* ((name (or (plist-get candidate :display-name)
                   (plist-get candidate :username)
                   "?"))
         (parts (split-string (or name "") "[^[:alnum:]]+" t))
         (first (if parts (substring (car parts) 0 1) "?"))
         (second (if (> (length parts) 1)
                     (substring (cadr parts) 0 1)
                   "")))
    (upcase (concat first second))))

(defun disco-company--completion-user-avatar-image (candidate)
  "Return avatar image object for user CANDIDATE when available."
  (when (and disco-company-show-user-avatars
             (boundp 'disco-room-show-avatar-images)
             disco-room-show-avatar-images
             (fboundp 'disco-room--avatar-image))
    (let* ((user-id (plist-get candidate :user-id))
           (avatar-hash (plist-get candidate :avatar-hash)))
      (when user-id
        (let ((author `((id . ,user-id))))
          (when (disco-company--completion-string-present-p avatar-hash)
            (setq author (append author `((avatar . ,avatar-hash)))))
          (ignore-errors
            (funcall #'disco-room--avatar-image
                     `((author . ,author)))))))))

(defun disco-company--completion-company-user-annotation (candidate)
  "Return company annotation string for user CANDIDATE."
  (let* ((username (plist-get candidate :username))
         (user-id (plist-get candidate :user-id))
         (image (disco-company--completion-user-avatar-image candidate))
         (icon (if image
                   (propertize " " 'display image)
                 (format "[%s]" (disco-company--completion-user-initials candidate)))))
    (format "  %s %s | id:%s"
            icon
            (if (disco-company--completion-string-present-p username)
                (format "@%s" username)
              "@unknown")
            (or user-id "?"))))

(defun disco-company--completion-company-annotation (candidate)
  "Return company annotation string for CANDIDATE plist."
  (if (eq (plist-get candidate :kind) 'user)
      (disco-company--completion-company-user-annotation candidate)
    (or (plist-get candidate :annotation) "")))

(defun disco-company--completion-company-meta (candidate)
  "Return company metadata string for CANDIDATE plist."
  (if (eq (plist-get candidate :kind) 'user)
      (format "display:%s username:%s id:%s"
              (or (plist-get candidate :display-name) "")
              (or (plist-get candidate :username) "")
              (or (plist-get candidate :user-id) ""))
    (or (plist-get candidate :label) "")))

(defun disco-room-complete-at-point ()
  "Return completion data for `@' and `#' tokens at point.

This function is suitable for `completion-at-point-functions'."
  (let ((token (disco-company--completion-token-bounds)))
    (when token
      (let* ((trigger (plist-get token :trigger))
             (start (plist-get token :start))
             (end (plist-get token :end))
             (all-candidates (disco-company--completion-candidates-for-trigger trigger))
             (candidate-table (make-hash-table :test #'equal)))
        (dolist (candidate all-candidates)
          (puthash (plist-get candidate :label) candidate candidate-table))
        (list
         start
         end
         (lambda (string pred action)
           (if (eq action 'metadata)
               '(metadata (category . unicode-name)
                          (display-sort-function . identity)
                          (cycle-sort-function . identity))
             (let* ((matches (disco-company--completion-filter-candidates
                              all-candidates
                              string))
                    (labels (mapcar (lambda (candidate)
                                      (plist-get candidate :label))
                                    matches)))
               (complete-with-action action labels string pred))))
         :affixation-function
         (lambda (labels)
           (mapcar
            (lambda (label)
              (let* ((key (and (stringp label)
                               (substring-no-properties label)))
                     (candidate (and key
                                     (gethash key candidate-table))))
                (list label ""
                      (if candidate
                          (disco-company--completion-company-annotation candidate)
                        ""))))
            labels))
         :annotation-function
         (lambda (label)
           (let* ((key (and (stringp label)
                            (substring-no-properties label)))
                  (candidate (and key
                                  (gethash key candidate-table))))
             (if candidate
                 (disco-company--completion-company-annotation candidate)
               "")))
         :exit-function
         (lambda (label status)
           (when (memq status '(finished sole exact))
             (let* ((key (and (stringp label)
                              (substring-no-properties label)))
                    (candidate (and key
                                    (gethash key candidate-table)))
                    (replacement (and candidate (plist-get candidate :insert))))
               (when replacement
                 (disco-company--completion-apply-candidate key replacement)))))
         :exclusive 'no)))))

(defun disco-company--company-prefix ()
  "Return current completion token for company backend, or nil."
  (let ((token (disco-company--completion-token-bounds)))
    (when token
      (plist-get token :raw))))

(defun disco-company--company-candidates (prefix)
  "Return company candidate strings for PREFIX."
  (when (and (stringp prefix) (> (length prefix) 0))
    (let* ((trigger (aref prefix 0))
           (matches (disco-company--completion-filter-candidates
                     (disco-company--completion-candidates-for-trigger trigger)
                     prefix)))
      (mapcar (lambda (candidate)
                (propertize (plist-get candidate :label)
                            'disco-room-completion-candidate candidate
                            'disco-room-completion-insert
                            (plist-get candidate :insert)
                            'disco-room-completion-annotation
                            (plist-get candidate :annotation)))
              matches))))

(defun disco-room-company-completion (command &optional arg &rest _ignored)
  "Company backend for room `@' and `#' token completion."
  (interactive (list 'interactive))
  (pcase command
    ('interactive
     (when (fboundp 'company-begin-backend)
       (funcall 'company-begin-backend 'disco-room-company-completion)))
    ('prefix
     (when (and (derived-mode-p 'disco-room-mode)
                (disco-company--point-in-input-p))
       (disco-company--company-prefix)))
    ('sorted t)
    ('require-match 'never)
    ('candidates
     (disco-company--company-candidates arg))
    ('annotation
     (let ((candidate (and (stringp arg)
                           (get-text-property 0 'disco-room-completion-candidate arg))))
       (if candidate
           (disco-company--completion-company-annotation candidate)
         (or (and (stringp arg)
                  (get-text-property 0 'disco-room-completion-annotation arg))
             ""))))
    ('meta
     (let ((candidate (and (stringp arg)
                           (get-text-property 0 'disco-room-completion-candidate arg))))
       (if candidate
           (disco-company--completion-company-meta candidate)
         (or arg ""))))
    ('post-completion
     (let ((replacement (and (stringp arg)
                             (get-text-property 0 'disco-room-completion-insert arg))))
       (when replacement
         (disco-company--completion-apply-candidate arg replacement))))))

(defun disco-company--complete-with-capf ()
  "Try CAPF completion for mention/channel token at point."
  (let ((completion-ignore-case t)
        (completion-at-point-functions
         (cons #'disco-room-complete-at-point completion-at-point-functions)))
    (completion-at-point)))

(defun disco-company--complete-with-company ()
  "Try company backend completion for mention/channel token at point."
  (when (and (bound-and-true-p company-mode)
             (fboundp 'company-begin-backend)
             (fboundp 'company-complete)
             (disco-company--completion-token-bounds))
    (funcall 'company-begin-backend 'disco-room-company-completion)
    (funcall 'company-complete)))

(defun disco-room-complete-mention ()
  "Complete `@' mention or `#' channel token at point.

This command prefers company completion when available and falls back to CAPF."
  (interactive)
  (let ((token (disco-company--completion-token-bounds)))
    (if (not token)
        (message "disco: point is not on @mention or #channel token")
      (or (disco-company--complete-with-company)
          (disco-company--complete-with-capf)
          (message "disco: no completion candidates for %s"
                   (plist-get token :raw))))))

(provide 'disco-company)

;;; disco-company.el ends here
