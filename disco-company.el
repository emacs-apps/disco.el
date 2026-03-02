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
(defvar completion-at-point-functions)

(defvar-local disco-room--channel-id nil)
(defvar-local disco-room--guild-id nil)
(defvar-local disco-room--typing-users nil)
(defvar-local disco-room--input-marker nil)
(defvar-local disco-room--draft-input "")

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

(defun disco-room--completion-token-boundary-p (char)
  "Return non-nil when CHAR is a valid left boundary for token completion."
  (or (null char)
      (let ((syntax (char-syntax char)))
        (not (memq syntax '(?w ?_))))))

(defun disco-room--completion-token-bounds ()
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
              (when (disco-room--completion-token-boundary-p left)
                (let ((raw (buffer-substring-no-properties start end)))
                  (list :start start
                        :end end
                        :trigger trigger
                        :raw raw
                        :query (buffer-substring-no-properties (1+ start) end)))))))))))

(defun disco-room--completion-string-present-p (value)
  "Return non-nil when VALUE is a non-empty trimmed string."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun disco-room--completion-first-present (&rest values)
  "Return first non-empty trimmed string from VALUES, or nil."
  (seq-find #'disco-room--completion-string-present-p
            (mapcar (lambda (value)
                      (and (stringp value)
                           (string-trim value)))
                    values)))

(defun disco-room--completion-short-id (id)
  "Return short suffix label for snowflake-like ID."
  (let ((text (or (disco-company--normalize-id id) "????")))
    (if (> (length text) 4)
        (substring text (- (length text) 4))
      text)))

(defun disco-room--completion-disambiguate-label (base hint seen-labels)
  "Return unique completion label from BASE using HINT against SEEN-LABELS."
  (let* ((seed (or base ""))
         (suffix (if (disco-room--completion-string-present-p hint)
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

(defun disco-room--completion-make-candidate (label insert annotation sort-key)
  "Build one completion candidate plist."
  (list :label label :insert insert :annotation annotation :sort-key sort-key))

(defun disco-room--completion-collect-user-name-map ()
  "Return hash table of user-id -> display name for current room context."
  (let ((name-map (make-hash-table :test #'equal)))
    (cl-labels
        ((remember (user-id display-name)
           (let* ((id (disco-company--normalize-id user-id))
                  (name (and (stringp display-name) (string-trim display-name)))
                  (has-name (disco-room--completion-string-present-p name))
                  (existing (and id (gethash id name-map))))
             (when id
               (when (or (null existing)
                         (and has-name
                              (not (disco-room--completion-string-present-p existing))))
                 (puthash id (if has-name name "") name-map))))))
      (let* ((channel (disco-company--channel-object))
             (recipients (and (listp channel)
                              (disco-company--normalize-list-sequence
                               (alist-get 'recipients channel)))))
        (dolist (recipient recipients)
          (when (listp recipient)
            (remember (alist-get 'id recipient)
                      (disco-room--completion-first-present
                       (alist-get 'global_name recipient)
                       (alist-get 'username recipient))))))
      (when (hash-table-p disco-room--typing-users)
        (maphash (lambda (user-id entry)
                   (remember user-id
                             (and (listp entry)
                                  (plist-get entry :display-name))))
                 disco-room--typing-users))
      (dolist (msg (or (disco-state-messages disco-room--channel-id) '()))
        (let* ((author (alist-get 'author msg))
               (member (alist-get 'member msg)))
          (when (listp author)
            (remember (alist-get 'id author)
                      (disco-room--completion-first-present
                       (and (listp member) (alist-get 'nick member))
                       (alist-get 'global_name author)
                       (alist-get 'username author))))))
      (when (disco-company--thread-channel-p)
        (dolist (user-id (or (disco-state-thread-member-ids disco-room--channel-id) '()))
          (remember user-id nil))))
    name-map))

(defun disco-room--completion-user-candidates ()
  "Return `@' user mention candidates for current room context."
  (let ((name-map (disco-room--completion-collect-user-name-map))
        (seen-labels (make-hash-table :test #'equal))
        out)
    (maphash
     (lambda (user-id display-name)
       (let* ((short-id (disco-room--completion-short-id user-id))
              (name (if (disco-room--completion-string-present-p display-name)
                        display-name
                      (format "user-%s" short-id)))
              (label (disco-room--completion-disambiguate-label
                      (format "@%s" name)
                      short-id
                      seen-labels)))
         (push (disco-room--completion-make-candidate
                label
                (format "<@%s>" user-id)
                " user"
                (downcase name))
               out)))
     name-map)
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-room--completion-role-candidates ()
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
                   (disco-room--completion-string-present-p name)
                   (not (equal role-id guild-id)))
          (let ((label (disco-room--completion-disambiguate-label
                        (format "@%s" name)
                        (disco-room--completion-short-id role-id)
                        seen-labels)))
            (push (disco-room--completion-make-candidate
                   label
                   (format "<@&%s>" role-id)
                   " role"
                   (downcase name))
                  out)))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-room--completion-special-candidates ()
  "Return special `@everyone' and `@here' candidates for guild channels."
  (when (disco-company--normalize-id disco-room--guild-id)
    (list (disco-room--completion-make-candidate
           "@everyone" "@everyone" " special" "@everyone")
          (disco-room--completion-make-candidate
           "@here" "@here" " special" "@here"))))

(defun disco-room--completion-channel-type-label (channel)
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

(defun disco-room--completion-mentionable-channel-p (channel)
  "Return non-nil when CHANNEL should be offered for `#' completion."
  (let ((type (and (listp channel) (alist-get 'type channel))))
    (and (listp channel)
         (not (eq type 4))
         (disco-room--completion-string-present-p (alist-get 'name channel))
         (disco-company--normalize-id (alist-get 'id channel)))))

(defun disco-room--completion-channel-candidates ()
  "Return `#' channel mention candidates from current guild state."
  (let* ((guild-id (disco-company--normalize-id disco-room--guild-id))
         (channels (and guild-id
                        (disco-state-guild-channels guild-id)))
         (seen-ids (make-hash-table :test #'equal))
         (seen-labels (make-hash-table :test #'equal))
         out)
    (dolist (channel (or channels '()))
      (when (disco-room--completion-mentionable-channel-p channel)
        (let* ((channel-id (disco-company--normalize-id (alist-get 'id channel)))
               (name (string-trim (alist-get 'name channel))))
          (unless (gethash channel-id seen-ids)
            (puthash channel-id t seen-ids)
            (let ((label (disco-room--completion-disambiguate-label
                          (format "#%s" name)
                          (disco-room--completion-short-id channel-id)
                          seen-labels)))
              (push (disco-room--completion-make-candidate
                     label
                     (format "<#%s>" channel-id)
                     (disco-room--completion-channel-type-label channel)
                     (downcase name))
                    out))))))
    (sort out (lambda (left right)
                (string-lessp (or (plist-get left :sort-key) "")
                              (or (plist-get right :sort-key) ""))))))

(defun disco-room--completion-uniquify-candidates (candidates)
  "Return CANDIDATES with globally unique display labels."
  (let ((seen-labels (make-hash-table :test #'equal))
        out)
    (dolist (candidate candidates)
      (let* ((copy (copy-sequence candidate))
             (label (or (plist-get copy :label) ""))
             (hint (string-trim (or (plist-get copy :annotation) "")))
             (unique (disco-room--completion-disambiguate-label
                      label
                      hint
                      seen-labels)))
        (setq copy (plist-put copy :label unique))
        (push copy out)))
    (nreverse out)))

(defun disco-room--completion-candidates-for-trigger (trigger)
  "Return completion candidates for TRIGGER in current room context."
  (disco-room--completion-uniquify-candidates
   (pcase trigger
     (?@ (append (or (disco-room--completion-special-candidates) '())
                 (disco-room--completion-role-candidates)
                 (disco-room--completion-user-candidates)))
     (?# (disco-room--completion-channel-candidates))
     (_ nil))))

(defun disco-room--completion-filter-candidates (candidates prefix)
  "Return CANDIDATES matching PREFIX case-insensitively."
  (let ((needle (downcase (or prefix ""))))
    (if (string-empty-p needle)
        candidates
      (seq-filter
       (lambda (candidate)
         (let ((label (or (plist-get candidate :label) "")))
           (string-prefix-p needle (downcase label))))
       candidates))))

(defun disco-room--completion-apply-candidate (completed replacement)
  "Replace COMPLETED text before point with mention REPLACEMENT.

Return non-nil when replacement was applied."
  (let ((choice (and (stringp completed)
                     (substring-no-properties completed))))
    (when (and (disco-room--completion-string-present-p choice)
               (disco-room--completion-string-present-p replacement))
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

(defun disco-room-complete-at-point ()
  "Return completion data for `@' and `#' tokens at point.

This function is suitable for `completion-at-point-functions'."
  (let ((token (disco-room--completion-token-bounds)))
    (when token
      (let* ((trigger (plist-get token :trigger))
             (start (plist-get token :start))
             (end (plist-get token :end))
             (all-candidates (disco-room--completion-candidates-for-trigger trigger))
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
             (let* ((matches (disco-room--completion-filter-candidates
                              all-candidates
                              string))
                    (labels (mapcar (lambda (candidate)
                                      (plist-get candidate :label))
                                    matches)))
               (complete-with-action action labels string pred))))
         :annotation-function
         (lambda (label)
           (let* ((key (and (stringp label)
                            (substring-no-properties label)))
                  (candidate (and key
                                  (gethash key candidate-table))))
             (or (and candidate (plist-get candidate :annotation))
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
                 (disco-room--completion-apply-candidate key replacement)))))
         :exclusive 'no)))))

(defun disco-room--company-prefix ()
  "Return current completion token for company backend, or nil."
  (let ((token (disco-room--completion-token-bounds)))
    (when token
      (plist-get token :raw))))

(defun disco-room--company-candidates (prefix)
  "Return company candidate strings for PREFIX."
  (when (and (stringp prefix) (> (length prefix) 0))
    (let* ((trigger (aref prefix 0))
           (matches (disco-room--completion-filter-candidates
                     (disco-room--completion-candidates-for-trigger trigger)
                     prefix)))
      (mapcar (lambda (candidate)
                (propertize (plist-get candidate :label)
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
       (disco-room--company-prefix)))
    ('sorted t)
    ('require-match 'never)
    ('candidates
     (disco-room--company-candidates arg))
    ('annotation
     (or (and (stringp arg)
              (get-text-property 0 'disco-room-completion-annotation arg))
         ""))
    ('post-completion
     (let ((replacement (and (stringp arg)
                             (get-text-property 0 'disco-room-completion-insert arg))))
       (when replacement
         (disco-room--completion-apply-candidate arg replacement))))))

(defun disco-room--complete-with-capf ()
  "Try CAPF completion for mention/channel token at point."
  (let ((completion-ignore-case t)
        (completion-at-point-functions
         (cons #'disco-room-complete-at-point completion-at-point-functions)))
    (completion-at-point)))

(defun disco-room--complete-with-company ()
  "Try company backend completion for mention/channel token at point."
  (when (and (bound-and-true-p company-mode)
             (fboundp 'company-begin-backend)
             (fboundp 'company-complete)
             (disco-room--completion-token-bounds))
    (funcall 'company-begin-backend 'disco-room-company-completion)
    (funcall 'company-complete)))

(defun disco-room-complete-mention ()
  "Complete `@' mention or `#' channel token at point.

This command prefers company completion when available and falls back to CAPF."
  (interactive)
  (let ((token (disco-room--completion-token-bounds)))
    (if (not token)
        (message "disco: point is not on @mention or #channel token")
      (or (disco-room--complete-with-company)
          (disco-room--complete-with-capf)
          (message "disco: no completion candidates for %s"
                   (plist-get token :raw))))))

(provide 'disco-company)

;;; disco-company.el ends here
