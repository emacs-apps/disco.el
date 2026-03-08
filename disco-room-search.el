;;; disco-room-search.el --- Search and filter flows for disco room buffers -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Search, filter, and jump helpers extracted from `disco-room.el'.  This file
;; owns room search behavior while leaving room state and render primitives in
;; the room facade.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'disco-api)
(require 'disco-channel-type)
(require 'disco-gateway)
(require 'disco-msg)
(require 'disco-state)

(defvar disco-room--channel-id)
(defvar disco-room--guild-id)
(defvar disco-room--oldest-message-id)
(defvar disco-room--newest-message-id)
(defvar disco-room--last-search-query)
(defvar disco-room--msg-filter)
(defvar disco-room--filter-generation)
(defvar disco-room--filter-in-flight)
(defvar disco-room--inplace-search-filter)
(defvar disco-room--inplace-search-generation)
(defvar disco-room-filter-search-limit)
(defvar disco-room-inplace-search-history)

(declare-function disco-room--active-highlight-query "disco-room")
(declare-function disco-room--async-error-message "disco-room" (err))
(declare-function disco-room--at-message-bottom-p "disco-room")
(declare-function disco-room--channel-object "disco-room")
(declare-function disco-room--display-messages "disco-room")
(declare-function disco-room--message-at-point "disco-room")
(declare-function disco-room--message-author "disco-room" (msg))
(declare-function disco-room--message-author-id "disco-room" (msg))
(declare-function disco-room--message-by-id "disco-room" (message-id))
(declare-function disco-room--message-id-at-point "disco-room")
(declare-function disco-room--msg-filter-active-p "disco-room")
(declare-function disco-room--render-preserving-point "disco-room")
(declare-function disco-room--update-frame-preserving-point "disco-room")
(declare-function disco-room-jump-to-message "disco-room"
                  (message-id &optional channel-id))
(declare-function disco-room-render "disco-room")
(declare-function disco-root-search-channel-transient "disco-root"
                  (&optional channel))

(defun disco-room-search--message-text (msg)
  "Return best-effort searchable text extracted from MSG."
  (string-join
   (delq nil
         (list (and (stringp (alist-get 'content msg))
                    (string-trim (alist-get 'content msg)))
               (disco-msg-preview-content msg)
               (disco-room--message-author msg)))
   " "))

(defun disco-room-search--message-match-p (msg filter)
  "Return non-nil when MSG matches inplace FILTER plist."
  (let ((query (plist-get filter :query))
        (author-id (plist-get filter :author-id)))
    (and (or (null author-id)
             (equal (format "%s" author-id)
                    (format "%s" (disco-room--message-author-id msg))))
         (or (null query)
             (string-empty-p query)
             (string-match-p (regexp-quote query)
                             (downcase (disco-room-search--message-text msg)))))))

(defun disco-room-search--message-line-positions ()
  "Return ordered list of rendered message line positions in current buffer."
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (get-text-property (line-beginning-position) 'disco-message-id)
          (push (line-beginning-position) positions))
        (forward-line 1)))
    (nreverse positions)))

(defun disco-room-search--message-line (filter forward &optional origin)
  "Return next local search hit for FILTER from ORIGIN.

When FORWARD is non-nil, search forward; otherwise backward."
  (let ((cursor (or origin (point)))
        found)
    (if forward
        (setq found
              (seq-find
               (lambda (pos)
                 (and (> pos cursor)
                      (let* ((message-id (get-text-property pos 'disco-message-id))
                             (msg (and message-id
                                       (disco-room--message-by-id message-id))))
                        (and msg
                             (disco-room-search--message-match-p msg filter)))))
               (disco-room-search--message-line-positions)))
      (dolist (pos (disco-room-search--message-line-positions))
        (when (and (< pos cursor)
                   (let* ((message-id (get-text-property pos 'disco-message-id))
                          (msg (and message-id
                                    (disco-room--message-by-id message-id))))
                     (and msg
                          (disco-room-search--message-match-p msg filter))))
          (setq found pos))))
    found))

(defun disco-room-search--move-local (filter forward &optional count)
  "Move point to COUNTth local hit for FILTER.

When FORWARD is non-nil move forward, otherwise backward.  Wrap once inside
currently rendered room messages."
  (let ((steps (max 1 (or count 1)))
        (origin (point))
        (cursor (point))
        (moved t))
    (while (and moved (> steps 0))
      (let ((hit (or (disco-room-search--message-line filter forward cursor)
                     (disco-room-search--message-line
                      filter forward (if forward (point-min) (point-max))))))
        (if hit
            (progn
              (setq cursor hit)
              (goto-char hit)
              (setq steps (1- steps)))
          (setq moved nil)
          (goto-char origin))))
    (and moved (= steps 0))))

(defun disco-room-search--register-candidate (label value seen result)
  "Register LABEL -> VALUE in RESULT using SEEN hash table."
  (let ((text (and label (string-trim (format "%s" label)))))
    (when (and (stringp text)
               (not (string-empty-p text))
               (not (gethash text seen)))
      (puthash text t seen)
      (push (cons text value) result)))
  result)

(defun disco-room-search--user-candidates ()
  "Return completion candidates for room search user prompts."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (when-let* ((current-user-id (and (fboundp 'disco-gateway-current-user-id)
                                      (disco-gateway-current-user-id))))
      (dolist (label (list "me" current-user-id))
        (setq result (disco-room-search--register-candidate
                      label current-user-id seen result))))
    (when disco-room--guild-id
      (dolist (member (disco-state-guild-members disco-room--guild-id))
        (let* ((user (alist-get 'user member))
               (user-id (or (and (listp user) (alist-get 'id user))
                            (alist-get 'user_id member)))
               (nick (alist-get 'nick member)))
          (when user-id
            (dolist (label (delq nil (list nick
                                           (and (listp user)
                                                (alist-get 'global_name user))
                                           (and (listp user)
                                                (alist-get 'username user))
                                           (and (listp user)
                                                (alist-get 'username user)
                                                (format "@%s"
                                                        (alist-get 'username user)))
                                           user-id)))
              (setq result (disco-room-search--register-candidate
                            label user-id seen result))))))
      (dolist (presence (disco-state-presences disco-room--guild-id))
        (let* ((user (alist-get 'user presence))
               (user-id (and (listp user) (alist-get 'id user))))
          (when user-id
            (dolist (label (delq nil (list (alist-get 'global_name user)
                                           (alist-get 'username user)
                                           (and (alist-get 'username user)
                                                (format "@%s"
                                                        (alist-get 'username user)))
                                           user-id)))
              (setq result (disco-room-search--register-candidate
                            label user-id seen result)))))))
    (when-let* ((channel (disco-room--channel-object))
                (recipients (alist-get 'recipients channel)))
      (dolist (recipient recipients)
        (when-let* ((user-id (alist-get 'id recipient)))
          (dolist (label (delq nil (list (alist-get 'global_name recipient)
                                         (alist-get 'username recipient)
                                         (and (alist-get 'username recipient)
                                              (format "@%s"
                                                      (alist-get 'username recipient)))
                                         user-id)))
            (setq result (disco-room-search--register-candidate
                          label user-id seen result))))))
    (dolist (msg (append (disco-room--display-messages)
                         (or (disco-state-messages disco-room--channel-id) '())))
      (let ((author-id (disco-room--message-author-id msg)))
        (when author-id
          (dolist (label (delq nil (list (disco-room--message-author msg)
                                         author-id)))
            (setq result (disco-room-search--register-candidate
                          label author-id seen result))))))
    (nreverse result)))

(defun disco-room--search-user-label (user-id)
  "Return best-effort display label for USER-ID in current room context."
  (or (car (seq-find (lambda (cell)
                       (equal (cdr cell) user-id))
                     (disco-room-search--user-candidates)))
      (format "%s" user-id)))

(defun disco-room--read-search-user-id (prompt)
  "Prompt for one room search user id with PROMPT."
  (let* ((candidates (disco-room-search--user-candidates))
         (default-id (and (ignore-errors (disco-room--message-id-at-point))
                          (ignore-errors
                            (disco-room--message-author-id
                             (disco-room--message-at-point)))))
         (default-label (and default-id
                             (car (seq-find (lambda (cell)
                                              (equal (cdr cell) default-id))
                                            candidates))))
         (choice (completing-read prompt (mapcar #'car candidates)
                                  nil t nil nil default-label)))
    (or (cdr (assoc choice candidates))
        (and (string-match-p "\\`[0-9]+\\'" choice) choice)
        (user-error "disco: unknown room search user %s" choice))))

(defun disco-room-search--flatten-messages (messages)
  "Flatten Discord nested search MESSAGES array into a list of messages."
  (let (result)
    (dolist (group (or messages '()))
      (if (listp group)
          (dolist (message group)
            (when (listp message)
              (push message result)))
        (when (listp group)
          (push group result))))
    (nreverse result)))

(defun disco-room-search--merge-message-lists (existing new-items)
  "Append NEW-ITEMS to EXISTING, deduping by message id while preserving order."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (message (append (or existing '()) (or new-items '())))
      (let ((message-id (alist-get 'id message)))
        (unless (and message-id (gethash message-id seen))
          (when message-id
            (puthash message-id t seen))
          (push message result))))
    (nreverse result)))

(defun disco-room--searchable-channel-type-p (&optional channel)
  "Return non-nil when CHANNEL supports server-side room message search."
  (let ((target (or channel (disco-room--channel-object))))
    (or (null (disco-channel-type-value target))
        (disco-channel-searchable-p target))))

(defun disco-room--searchable-channel-type-name (&optional channel)
  "Return descriptive channel type name for CHANNEL."
  (disco-channel-type-name (or channel (disco-room--channel-object))))

(defun disco-room--ensure-searchable-channel (&optional action)
  "Signal user error when current room channel cannot be searched remotely.

ACTION is optional text describing the attempted search action."
  (unless (disco-room--searchable-channel-type-p)
    (user-error
     "disco: %s is not supported for %s channels"
     (or action "server-side room search")
     (disco-room--searchable-channel-type-name))))

(cl-defun disco-room--search-current-channel-async
    (&key query author-id limit offset max-id min-id sort-order on-success on-error)
  "Search the current room channel asynchronously."
  (disco-room--ensure-searchable-channel "server-side room search")
  (let* ((channel (disco-room--channel-object))
         (include-nsfw (and disco-room--guild-id
                            (disco-state-channel-age-restricted-p channel))))
    (if disco-room--guild-id
        (disco-api-guild-search-messages-async
         disco-room--guild-id
         :channel-ids (list disco-room--channel-id)
         :include-nsfw include-nsfw
         :content query
         :author-ids (and author-id (list author-id))
         :limit limit
         :offset offset
         :max-id max-id
         :min-id min-id
         :sort-by 'timestamp
         :sort-order sort-order
         :on-success on-success
         :on-error on-error)
      (disco-api-channel-search-messages-async
       disco-room--channel-id
       :content query
       :author-ids (and author-id (list author-id))
       :limit limit
       :offset offset
       :max-id max-id
       :min-id min-id
       :sort-by 'timestamp
       :sort-order sort-order
       :on-success on-success
       :on-error on-error))))

(defun disco-room--msg-filter-title (filter)
  "Return human-readable title string for room FILTER plist."
  (let ((query (plist-get filter :query))
        (author-id (plist-get filter :author-id)))
    (cond
     ((and query author-id)
      (format "search \"%s\" from %s" query (disco-room--search-user-label author-id)))
     (author-id
      (format "from %s" (disco-room--search-user-label author-id)))
     (query
      (format "search \"%s\"" query))
     (t
      "filter"))))

(defun disco-room--msg-filter-has-more-p ()
  "Return non-nil when current room filter likely has more results."
  (when (disco-room--msg-filter-active-p)
    (let* ((items (or (plist-get disco-room--msg-filter :items) '()))
           (total (plist-get disco-room--msg-filter :total-count))
           (loaded (length items)))
      (if (numberp total)
          (< loaded total)
        (plist-get disco-room--msg-filter :has-more)))))

(defun disco-room--msg-filter-status-line ()
  "Return one status line describing the active room message filter."
  (when (disco-room--msg-filter-active-p)
    (let* ((filter disco-room--msg-filter)
           (loaded (length (or (plist-get filter :items) '())))
           (total (plist-get filter :total-count))
           (parts (list (format "Filter: %s"
                                (disco-room--msg-filter-title filter))
                        (if (numberp total)
                            (format "[%d/%d]" loaded total)
                          (format "[%d]" loaded)))))
      (when disco-room--filter-in-flight
        (setq parts (append parts '("[searching...]"))))
      (when (disco-room--msg-filter-has-more-p)
        (setq parts (append parts '("[M-< more]"))))
      (setq parts (append parts '("[C-c C-/ cancel]")))
      (string-join parts "  "))))

(defun disco-room-search--filter-callback-active-p (room-buffer channel-id generation)
  "Return non-nil when filter callback still matches ROOM-BUFFER state."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (= disco-room--filter-generation generation)))))

(defun disco-room-search--inplace-search-callback-active-p
    (room-buffer channel-id generation)
  "Return non-nil when inplace search callback still matches ROOM-BUFFER state."
  (and (buffer-live-p room-buffer)
       (with-current-buffer room-buffer
         (and (eq major-mode 'disco-room-mode)
              (equal disco-room--channel-id channel-id)
              (= disco-room--inplace-search-generation generation)))))

(defun disco-room-search--run-filter (filter &optional append)
  "Run room message FILTER asynchronously.

When APPEND is non-nil, load the next page of matching messages."
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (generation (1+ disco-room--filter-generation))
         (existing (if append
                       (or (plist-get disco-room--msg-filter :items) '())
                     '()))
         (offset (if append (length existing) 0)))
    (setq-local disco-room--filter-generation generation)
    (setq-local disco-room--filter-in-flight t)
    (setq-local disco-room--msg-filter
                (plist-put (copy-sequence filter) :active t))
    (when append
      (setq-local disco-room--msg-filter
                  (plist-put disco-room--msg-filter :items existing)))
    (disco-room--render-preserving-point)
    (disco-room--search-current-channel-async
     :query (plist-get filter :query)
     :author-id (plist-get filter :author-id)
     :limit disco-room-filter-search-limit
     :offset offset
     :sort-order 'desc
     :on-success
     (lambda (body)
       (when (disco-room-search--filter-callback-active-p
              room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (let* ((page (disco-room-search--flatten-messages
                         (alist-get 'messages body)))
                  (items (if append
                             (disco-room-search--merge-message-lists existing page)
                           page))
                  (total (alist-get 'total_results body)))
             (setq-local disco-room--filter-in-flight nil)
             (setq-local disco-room--msg-filter
                         (list :active t
                               :query (plist-get filter :query)
                               :author-id (plist-get filter :author-id)
                               :items items
                               :total-count total
                               :has-more (if (numberp total)
                                             (< (length items) total)
                                           (= (length page)
                                              disco-room-filter-search-limit))))
             (if (disco-room--at-message-bottom-p)
                 (disco-room-render)
               (disco-room--render-preserving-point))
             (message "disco: filter -> %s"
                      (disco-room--msg-filter-title disco-room--msg-filter))))))
     :on-error
     (lambda (err)
       (when (disco-room-search--filter-callback-active-p
              room-buffer channel-id generation)
         (with-current-buffer room-buffer
           (setq-local disco-room--filter-in-flight nil)
           (disco-room--update-frame-preserving-point)
           (message "disco: filter search failed: %s"
                    (disco-room--async-error-message err))))))))

(defun disco-room-filter-search (&optional query by-sender-p)
  "Filter current room messages by QUERY.

With BY-SENDER-P, also prompt for a sender and restrict matches to that user."
  (interactive
   (list (read-string (format "Filter messages%s: "
                              (if current-prefix-arg " by sender" "")))
         current-prefix-arg))
  (disco-room--ensure-searchable-channel "filter search")
  (let ((sender-id (when by-sender-p
                     (disco-room--read-search-user-id "Sent by: ")))
        (trimmed (string-trim (or query ""))))
    (unless (or sender-id (not (string-empty-p trimmed)))
      (user-error "disco: search query is empty"))
    (when (not (string-empty-p trimmed))
      (setq-local disco-room--last-search-query trimmed))
    (disco-room-search--run-filter (list :query (unless (string-empty-p trimmed)
                                                  trimmed)
                                         :author-id sender-id)
                                   nil)))

(defun disco-room-filter-by-sender ()
  "Filter current room messages by sender only."
  (interactive)
  (disco-room-filter-search "" 'by-sender))

(defun disco-room-filter-refresh ()
  "Refresh the currently active room message filter."
  (interactive)
  (unless (disco-room--msg-filter-active-p)
    (user-error "disco: no active message filter"))
  (disco-room--ensure-searchable-channel "filter search")
  (disco-room-search--run-filter disco-room--msg-filter nil))

(defun disco-room-filter-load-more ()
  "Load the next page of results for the current room message filter."
  (interactive)
  (disco-room--ensure-searchable-channel "filter search")
  (cond
   ((not (disco-room--msg-filter-active-p))
    (user-error "disco: no active message filter"))
   (disco-room--filter-in-flight
    (message "disco: filter search already in progress"))
   ((not (disco-room--msg-filter-has-more-p))
    (message "disco: no more filtered messages available"))
   (t
    (disco-room-search--run-filter disco-room--msg-filter t))))

(defun disco-room-filter-cancel ()
  "Cancel current room message filter and restore the normal timeline."
  (interactive)
  (unless (disco-room--msg-filter-active-p)
    (user-error "disco: no active message filter"))
  (let ((message-id (ignore-errors (disco-room--message-id-at-point))))
    (setq-local disco-room--msg-filter nil)
    (setq-local disco-room--filter-in-flight nil)
    (if (or (null message-id)
            (disco-msg-find-in-channel disco-room--channel-id message-id))
        (disco-room--render-preserving-point)
      (disco-room-render)
      (disco-room-jump-to-message message-id))
    (message "disco: message filter canceled")))

(defconst disco-room-search--inplace-entry-points
  '(("query" . disco-room-inplace-search-query)
    ("by-sender" . disco-room-inplace-search-by-sender))
  "Supported room inplace search entry points.")

(defun disco-room-search--title (filter)
  "Return human-readable title string for inplace FILTER plist."
  (let ((query (plist-get filter :query))
        (author-id (plist-get filter :author-id)))
    (cond
     ((and query author-id)
      (format "query \"%s\" from %s" query (disco-room--search-user-label author-id)))
     (author-id
      (format "sent by %s" (disco-room--search-user-label author-id)))
     (query
      (format "query \"%s\"" query))
     (t
      "search"))))

(defun disco-room-search--current-message-id (forward)
  "Return starting message id for inplace search in direction FORWARD."
  (or (ignore-errors (disco-room--message-id-at-point))
      (if forward
          disco-room--oldest-message-id
        disco-room--newest-message-id)))

(defun disco-room--inplace-search-dispatch (filter &optional forward from-message-id)
  "Continue inplace searching using FILTER.

When FORWARD is non-nil, search toward newer messages.  FROM-MESSAGE-ID, when
non-nil, overrides the message id at point as the search boundary."
  (when (disco-room--msg-filter-active-p)
    (user-error "disco: can't search inplace while message filter is applied"))
  (let ((title (disco-room-search--title filter))
        (old-query (disco-room--active-highlight-query)))
    (if (disco-room-search--move-local filter forward 1)
        (progn
          (setq-local disco-room--inplace-search-filter filter)
          (when-let* ((query (plist-get filter :query)))
            (setq-local disco-room--last-search-query query))
          (unless (equal old-query (disco-room--active-highlight-query))
            (disco-room--render-preserving-point))
          (message "disco: %s" title))
      (let* ((room-buffer (current-buffer))
             (channel-id disco-room--channel-id)
             (generation (1+ disco-room--inplace-search-generation))
             (cursor-id (or from-message-id
                            (disco-room-search--current-message-id forward))))
        (unless cursor-id
          (user-error "disco: no message anchor available for search"))
        (if (not (disco-room--searchable-channel-type-p))
            (message "disco: no loaded message matches '%s' and remote search is not supported for %s channels"
                     title
                     (disco-room--searchable-channel-type-name))
          (setq-local disco-room--inplace-search-generation generation)
          (setq-local disco-room--inplace-search-filter filter)
          (when-let* ((query (plist-get filter :query)))
            (setq-local disco-room--last-search-query query))
          (unless (equal old-query (disco-room--active-highlight-query))
            (disco-room--render-preserving-point))
          (message "disco: searching %s..." title)
          (disco-room--search-current-channel-async
           :query (plist-get filter :query)
           :author-id (plist-get filter :author-id)
           :limit 2
           :max-id (unless forward cursor-id)
           :min-id (when forward cursor-id)
           :sort-order (if forward 'asc 'desc)
           :on-success
           (lambda (body)
             (when (disco-room-search--inplace-search-callback-active-p
                    room-buffer channel-id generation)
               (with-current-buffer room-buffer
                 (let* ((messages (disco-room-search--flatten-messages
                                   (alist-get 'messages body)))
                        (match (seq-find (lambda (msg)
                                           (not (equal (alist-get 'id msg)
                                                       cursor-id)))
                                         messages)))
                   (if-let* ((message-id (and (listp match)
                                             (alist-get 'id match))))
                       (progn
                         (message "")
                         (disco-room-jump-to-message message-id channel-id)
                         (message "disco: %s" title))
                     (message "disco: no message matches '%s'" title))))))
           :on-error
           (lambda (err)
             (when (disco-room-search--inplace-search-callback-active-p
                    room-buffer channel-id generation)
               (with-current-buffer room-buffer
                 (message "disco: inplace search failed: %s"
                          (disco-room--async-error-message err)))))))))))

(defun disco-room-inplace-search ()
  "Prompt for a room inplace search flavor and start searching."
  (interactive)
  (let ((choice (completing-read "Room search: "
                                 (mapcar #'car disco-room-search--inplace-entry-points)
                                 nil t)))
    (call-interactively (or (cdr (assoc choice disco-room-search--inplace-entry-points))
                            #'disco-room-inplace-search-query))))

(defun disco-room-inplace-search-query (query &optional by-sender-p forward-p)
  "Search current room for QUERY, optionally constrained by sender.

With BY-SENDER-P, also prompt for a sender.  When FORWARD-P is non-nil,
search toward newer messages."
  (interactive
   (list (read-string (format "Search messages%s: "
                              (if current-prefix-arg " by sender" ""))
                      nil 'disco-room-inplace-search-history)
         current-prefix-arg
         nil))
  (let* ((trimmed (string-trim (or query "")))
         (sender-id (when by-sender-p
                      (disco-room--read-search-user-id "Sent by: "))))
    (unless (or sender-id (not (string-empty-p trimmed)))
      (user-error "disco: search query is empty"))
    (disco-room--inplace-search-dispatch
     (list :query (unless (string-empty-p trimmed) trimmed)
           :author-id sender-id)
     forward-p)))

(defun disco-room-inplace-search-query-forward (query &optional by-sender-p)
  "Search forward in the current room for QUERY."
  (interactive
   (list (read-string (format "Search forward%s: "
                              (if current-prefix-arg " by sender" ""))
                      nil 'disco-room-inplace-search-history)
         current-prefix-arg))
  (disco-room-inplace-search-query query by-sender-p t))

(defun disco-room-inplace-search-by-sender (sender-id &optional forward-p)
  "Search current room by SENDER-ID only.

When called interactively, prompt for one sender in current room context."
  (interactive (list (disco-room--read-search-user-id "Sent by: ") nil))
  (disco-room--inplace-search-dispatch
   (list :author-id sender-id)
   forward-p))

(defun disco-room-inplace-search-prev (&optional forward-p)
  "Continue room inplace search.

When FORWARD-P is non-nil, continue toward newer messages."
  (interactive)
  (if disco-room--inplace-search-filter
      (disco-room--inplace-search-dispatch disco-room--inplace-search-filter
                                           forward-p)
    (call-interactively #'disco-room-inplace-search)))

(defun disco-room-inplace-search-next ()
  "Continue room inplace search toward newer messages."
  (interactive)
  (disco-room-inplace-search-prev t))

(defun disco-room-search ()
  "Prompt for room inplace search query and jump to the next hit."
  (interactive)
  (call-interactively #'disco-room-inplace-search-query))

(defun disco-room-search-next (&optional _n)
  "Jump to the next room inplace search result."
  (interactive "p")
  (disco-room-inplace-search-next))

(defun disco-room-search-prev (&optional _n)
  "Jump to the previous room inplace search result."
  (interactive "p")
  (disco-room-inplace-search-prev))

(defun disco-room-search-channel ()
  "Open root search transient scoped to the current room channel."
  (interactive)
  (let ((channel (or (disco-room--channel-object)
                     (user-error "disco: unknown current room channel"))))
    (disco-root-search-channel-transient channel)))

(provide 'disco-room-search)

;;; disco-room-search.el ends here
