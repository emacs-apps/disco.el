;;; disco-api-normalize.el --- API payload normalizers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared request payload/query normalization helpers for Discord API calls.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'disco-read-state)
(require 'disco-util)

(defun disco-api--json-true-p (value)
  "Return non-nil if VALUE semantically represents JSON true."
  (or (eq value t)
      (equal value "true")
      (eq value 'true)))

(defun disco-api--query-bool-string (value)
  "Return VALUE as API query boolean string.

JSON-like true values map to the true string and everything else maps to
the false string."
  (if (disco-api--json-true-p value) "true" "false"))

(defconst disco-api--thread-search-tag-setting-alist
  '((match-some . "match_some")
    (match_some . "match_some")
    (match-all . "match_all")
    (match_all . "match_all"))
  "Declarative map for thread search tag setting values.")

(defconst disco-api--thread-search-sort-by-alist
  '((last-message-time . "last_message_time")
    (last_message_time . "last_message_time")
    (archive-time . "archive_time")
    (archive_time . "archive_time")
    (relevance . "relevance")
    (creation-time . "creation_time")
    (creation_time . "creation_time"))
  "Declarative map for thread search sort-by values.")

(defconst disco-api--thread-search-sort-order-alist
  '((asc . "asc")
    (desc . "desc"))
  "Declarative map for thread search sort-order values.")

(defun disco-api--thread-search-tag-setting-value (value)
  "Normalize thread search tag setting VALUE to API representation."
  (cond
   ((stringp value) value)
   ((symbolp value)
    (alist-get value disco-api--thread-search-tag-setting-alist))
   (t nil)))

(defun disco-api--thread-search-sort-by-value (value)
  "Normalize thread search sort-by VALUE to API representation."
  (cond
   ((stringp value) value)
   ((symbolp value)
    (alist-get value disco-api--thread-search-sort-by-alist))
   (t nil)))

(defun disco-api--thread-search-sort-order-value (value)
  "Normalize thread search sort-order VALUE to API representation."
  (cond
   ((and (stringp value)
         (member value '("asc" "desc")))
    value)
   ((symbolp value)
    (alist-get value disco-api--thread-search-sort-order-alist))
   (t nil)))

(defconst disco-api--message-content-limit 2000
  "Maximum allowed Discord message content length in characters.")

(defun disco-api--validate-message-content-length (content field-name)
  "Signal `user-error' when CONTENT exceeds Discord's message limit.

FIELD-NAME is used to describe the failing payload field."
  (when (and (stringp content)
             (> (length content) disco-api--message-content-limit))
    (user-error "disco: %s exceeds Discord's %d character limit"
                field-name
                disco-api--message-content-limit))
  content)

(defconst disco-api--message-search-sort-by-alist
  '((timestamp . "timestamp")
    (relevance . "relevance"))
  "Declarative map for message search sort-by values.")

(defun disco-api--message-search-sort-by-value (value)
  "Normalize message search sort-by VALUE to API representation."
  (cond
   ((and (stringp value)
         (member value '("timestamp" "relevance")))
    value)
   ((symbolp value)
    (alist-get value disco-api--message-search-sort-by-alist))
   (t nil)))

(defun disco-api--normalize-string-sequence (value field-name)
  "Normalize VALUE into vector of non-empty strings for FIELD-NAME."
  (let* ((source (cond
                  ((null value) nil)
                  ((vectorp value) (append value nil))
                  ((listp value) value)
                  (t (list value))))
         (normalized
          (mapcar (lambda (item)
                    (let ((text (string-trim (format "%s" item))))
                      (unless (not (string-empty-p text))
                        (user-error "disco: %s cannot contain empty entries" field-name))
                      text))
                  source)))
    (vconcat normalized)))

(defun disco-api--normalize-string-list (value field-name)
  "Normalize VALUE into a list of non-empty strings for FIELD-NAME."
  (append (disco-api--normalize-string-sequence value field-name) nil))

(cl-defun disco-api--message-search-query
    (&key limit offset max-id min-id slop content author-types author-ids mentions
          mention-everyone has pinned sort-by sort-order channel-ids include-nsfw)
  "Build query alist for Discord message search GET endpoints."
  (let (query)
    (when (numberp limit)
      (push `("limit" . ,(number-to-string (max 1 (min 25 limit)))) query))
    (when (numberp offset)
      (push `("offset" . ,(number-to-string (max 0 (min 9975 offset)))) query))
    (when max-id
      (push `("max_id" . ,(format "%s" max-id)) query))
    (when min-id
      (push `("min_id" . ,(format "%s" min-id)) query))
    (when (numberp slop)
      (push `("slop" . ,(number-to-string (max 0 (min 100 slop)))) query))
    (when (and (stringp content)
               (not (string-empty-p (string-trim content))))
      (push `("content" . ,(string-trim content)) query))
    (dolist (value (disco-api--normalize-string-list author-types "message search author_type"))
      (push `("author_type" . ,value) query))
    (dolist (value (append (disco-api--normalize-id-sequence author-ids "message search author_id") nil))
      (push `("author_id" . ,value) query))
    (dolist (value (append (disco-api--normalize-id-sequence mentions "message search mentions") nil))
      (push `("mentions" . ,value) query))
    (when (not (null mention-everyone))
      (push `("mention_everyone" . ,(disco-api--query-bool-string mention-everyone)) query))
    (dolist (value (disco-api--normalize-string-list has "message search has"))
      (push `("has" . ,value) query))
    (when (not (null pinned))
      (push `("pinned" . ,(disco-api--query-bool-string pinned)) query))
    (let ((sort-by-value (disco-api--message-search-sort-by-value sort-by)))
      (when sort-by-value
        (push `("sort_by" . ,sort-by-value) query)))
    (let ((sort-order-value (disco-api--thread-search-sort-order-value sort-order)))
      (when sort-order-value
        (push `("sort_order" . ,sort-order-value) query)))
    (dolist (value (append (disco-api--normalize-id-sequence channel-ids "message search channel_id") nil))
      (push `("channel_id" . ,value) query))
    (when (not (null include-nsfw))
      (push `("include_nsfw" . ,(disco-api--query-bool-string include-nsfw)) query))
    (nreverse query)))

(cl-defun disco-api--message-search-tab-payload
    (&key limit offset cursor max-id min-id slop content author-types author-ids mentions
          mention-everyone has pinned sort-by sort-order)
  "Build one message search tab payload object."
  (when (and cursor (numberp offset))
    (user-error "disco: message search tab cannot use both cursor and offset"))
  (let (payload)
    (when (numberp limit)
      (push `(limit . ,(max 1 (min 25 limit))) payload))
    (when (numberp offset)
      (push `(offset . ,(max 0 (min 9975 offset))) payload))
    (when cursor
      (push `(cursor . ,cursor) payload))
    (when max-id
      (push `(max_id . ,(format "%s" max-id)) payload))
    (when min-id
      (push `(min_id . ,(format "%s" min-id)) payload))
    (when (numberp slop)
      (push `(slop . ,(max 0 (min 100 slop))) payload))
    (when (and (stringp content)
               (not (string-empty-p (string-trim content))))
      (push `(content . ,(string-trim content)) payload))
    (when author-types
      (push `(author_type . ,(disco-api--normalize-string-sequence
                              author-types
                              "message search author_type"))
            payload))
    (when author-ids
      (push `(author_id . ,(disco-api--normalize-id-sequence
                            author-ids
                            "message search author_id"))
            payload))
    (when mentions
      (push `(mentions . ,(disco-api--normalize-id-sequence
                           mentions
                           "message search mentions"))
            payload))
    (when (not (null mention-everyone))
      (push `(mention_everyone
              . ,(if (disco-api--json-true-p mention-everyone) t :false))
            payload))
    (when has
      (push `(has . ,(disco-api--normalize-string-sequence has "message search has"))
            payload))
    (when (not (null pinned))
      (push `(pinned . ,(if (disco-api--json-true-p pinned) t :false)) payload))
    (let ((sort-by-value (disco-api--message-search-sort-by-value sort-by)))
      (when sort-by-value
        (push `(sort_by . ,sort-by-value) payload)))
    (let ((sort-order-value (disco-api--thread-search-sort-order-value sort-order)))
      (when sort-order-value
        (push `(sort_order . ,sort-order-value) payload)))
    (nreverse payload)))

(cl-defun disco-api--message-search-tabs-payload
    (&key tabs channel-ids include-nsfw track-exact-total-hits)
  "Build payload for Discord message search tabs endpoints."
  (unless (listp tabs)
    (user-error "disco: message search tabs must be an alist"))
  (let ((payload nil)
        (tab-payloads nil))
    (dolist (tab tabs)
      (let* ((name (car tab))
             (spec (cdr tab))
             (payload-value
              (apply #'disco-api--message-search-tab-payload
                     (cond
                      ((null spec) nil)
                      ((and (listp spec)
                            (keywordp (car spec)))
                       spec)
                      ((listp spec)
                       spec)
                      (t
                       (user-error
                        "disco: message search tab `%s' spec must be a plist"
                        name))))))
        (push (cons name payload-value) tab-payloads)))
    (push (cons 'tabs (nreverse tab-payloads)) payload)
    (when channel-ids
      (push `(channel_ids . ,(disco-api--normalize-id-sequence
                              channel-ids
                              "message search channel_ids"))
            payload))
    (when (not (null include-nsfw))
      (push `(include_nsfw . ,(if (disco-api--json-true-p include-nsfw) t :false))
            payload))
    (when (not (null track-exact-total-hits))
      (push `(track_exact_total_hits
              . ,(if (disco-api--json-true-p track-exact-total-hits) t :false))
            payload))
    (nreverse payload)))

(cl-defun disco-api--thread-search-query (&key name slop tags tag-setting archived
                                               sort-by sort-order limit offset
                                               max-id min-id)
  "Build query alist for `/channels/{channel.id}/threads/search'."
  (let ((query nil))
    (when (and (stringp name) (not (string-empty-p name)))
      (push `("name" . ,name) query))
    (when (numberp slop)
      (push `("slop" . ,(number-to-string (max 0 (min 100 slop)))) query))
    (when (listp tags)
      (dolist (tag tags)
        (when tag
          (push `("tag" . ,(format "%s" tag)) query))))
    (let ((tag-setting-value (disco-api--thread-search-tag-setting-value tag-setting)))
      (when tag-setting-value
        (push `("tag_setting" . ,tag-setting-value) query)))
    (when (not (null archived))
      (push `("archived" . ,(disco-api--query-bool-string archived)) query))
    (let ((sort-by-value (disco-api--thread-search-sort-by-value sort-by)))
      (when sort-by-value
        (push `("sort_by" . ,sort-by-value) query)))
    (let ((sort-order-value (disco-api--thread-search-sort-order-value sort-order)))
      (when sort-order-value
        (push `("sort_order" . ,sort-order-value) query)))
    (when (numberp limit)
      (push `("limit" . ,(number-to-string (max 1 (min 25 limit)))) query))
    (when (numberp offset)
      (push `("offset" . ,(number-to-string (max 0 (min 9975 offset)))) query))
    (when max-id
      (push `("max_id" . ,(format "%s" max-id)) query))
    (when min-id
      (push `("min_id" . ,(format "%s" min-id)) query))
    (nreverse query)))

(defun disco-api--thread-archive-query (before limit)
  "Build query alist for thread archive endpoints."
  (let* ((raw-limit (or limit 50))
         ;; Discord archived thread endpoints accept 2-100.
         (normalized-limit (max 2 (min 100 raw-limit)))
         (query `(("limit" . ,(number-to-string normalized-limit)))))
    (when before
      (setq query (append query `(("before" . ,before)))))
    query))

(cl-defun disco-api--thread-members-query (&key with-member after limit)
  "Build query alist for thread member listing endpoints."
  (let (query)
    (when (not (null with-member))
      (push `("with_member" . ,(disco-api--query-bool-string with-member)) query))
    (when after
      (push `("after" . ,(format "%s" after)) query))
    (when (numberp limit)
      ;; Discord thread member listing endpoint accepts 1-100.
      (push `("limit" . ,(number-to-string (max 1 (min 100 limit)))) query))
    (nreverse query)))

(cl-defun disco-api--thread-update-payload (&key name archived locked auto-archive-duration
                                                 rate-limit-per-user invitable applied-tags)
  "Build thread channel PATCH payload from keyword arguments."
  (let (payload)
    (when (and (stringp name) (not (string-empty-p name)))
      (push `(name . ,name) payload))
    (when (not (null archived))
      (push `(archived . ,(if (disco-api--json-true-p archived) t :false)) payload))
    (when (not (null locked))
      (push `(locked . ,(if (disco-api--json-true-p locked) t :false)) payload))
    (when auto-archive-duration
      (push `(auto_archive_duration . ,auto-archive-duration) payload))
    (when (not (null rate-limit-per-user))
      (push `(rate_limit_per_user . ,rate-limit-per-user) payload))
    (when (not (null invitable))
      (push `(invitable . ,(if (disco-api--json-true-p invitable) t :false)) payload))
    (when (listp applied-tags)
      (push `(applied_tags . ,applied-tags) payload))
    (nreverse payload)))

(defconst disco-api--attachment-content-type-alist
  '(("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp")
    ("bmp" . "image/bmp")
    ("svg" . "image/svg+xml")
    ("mp4" . "video/mp4")
    ("mov" . "video/quicktime")
    ("webm" . "video/webm")
    ("mkv" . "video/x-matroska")
    ("mp3" . "audio/mpeg")
    ("wav" . "audio/wav")
    ("ogg" . "audio/ogg")
    ("pdf" . "application/pdf")
    ("zip" . "application/zip")
    ("json" . "application/json")
    ("txt" . "text/plain")
    ("md" . "text/plain")
    ("log" . "text/plain"))
  "Best-effort MIME type map by file extension.")

(defun disco-api--guess-content-type (filename)
  "Best-effort MIME type for FILENAME."
  (or (alist-get (downcase (or (file-name-extension (or filename "")) ""))
                 disco-api--attachment-content-type-alist
                 nil
                 #'string=)
      "application/octet-stream"))

(defun disco-api--read-file-bytes (path)
  "Return file contents of PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun disco-api--multipart-boundary ()
  "Generate multipart boundary token for one request."
  (format "----disco-%s-%06d"
          (format-time-string "%Y%m%d%H%M%S" (current-time) t)
          (random 1000000)))

(defun disco-api--multipart-write-string (string)
  "Insert STRING into current buffer as UTF-8 unibyte bytes."
  (insert (encode-coding-string (or string "") 'utf-8 t)))

(defun disco-api--json-encode-payload (payload)
  "Encode PAYLOAD as JSON string, preserving empty objects."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false :false)
        (json-null :null))
    (if (eq payload :empty-object)
        "{}"
      (json-encode payload))))

(defun disco-api--multipart-write-payload-json (boundary payload)
  "Insert PAYLOAD as multipart payload_json part using BOUNDARY."
  (disco-api--multipart-write-string (format "--%s\r\n" boundary))
  (disco-api--multipart-write-string
   "Content-Disposition: form-data; name=\"payload_json\"\r\n")
  (disco-api--multipart-write-string "Content-Type: application/json\r\n\r\n")
  (disco-api--multipart-write-string (disco-api--json-encode-payload payload))
  (disco-api--multipart-write-string "\r\n"))

(defun disco-api--multipart-write-file (boundary index attachment)
  "Insert one ATTACHMENT part under multipart BOUNDARY with INDEX."
  (let* ((path (plist-get attachment :path))
         (filename (plist-get attachment :filename))
         (content-type (plist-get attachment :content-type))
         (bytes (disco-api--read-file-bytes path)))
    (disco-api--multipart-write-string (format "--%s\r\n" boundary))
    (disco-api--multipart-write-string
     (format
      "Content-Disposition: form-data; name=\"files[%d]\"; filename=\"%s\"\r\n"
      index
      (replace-regexp-in-string "\"" "_" filename)))
    (disco-api--multipart-write-string
     (format "Content-Type: %s\r\n\r\n" content-type))
    (insert bytes)
    (disco-api--multipart-write-string "\r\n")))

(defun disco-api--normalize-send-attachment (attachment)
  "Normalize ATTACHMENT into plist with :path/:filename/:description/:content-type.

ATTACHMENT may be a file path string or a plist containing :path."
  (let* ((path (cond
                ((stringp attachment) attachment)
                ((and (listp attachment) (plist-get attachment :path))
                 (plist-get attachment :path))
                (t nil)))
         (description (and (listp attachment) (plist-get attachment :description)))
         (filename (and (listp attachment) (plist-get attachment :filename)))
         (content-type (and (listp attachment) (plist-get attachment :content-type))))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "disco: attachment must include a file path"))
    (unless (file-readable-p path)
      (user-error "disco: attachment file is not readable: %s" path))
    (let ((resolved-filename (or filename (file-name-nondirectory path))))
      (list :path path
            :filename resolved-filename
            :description (and (stringp description)
                              (not (string-empty-p (string-trim description)))
                              (string-trim description))
            :content-type (or content-type
                              (disco-api--guess-content-type resolved-filename))))))

(defun disco-api--build-message-multipart-body (payload attachments)
  "Build multipart body for message PAYLOAD and ATTACHMENTS.

Return cons cell (BOUNDARY . BODY)."
  (let ((boundary (disco-api--multipart-boundary)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (disco-api--multipart-write-payload-json boundary payload)
      (cl-loop for attachment in attachments
               for idx from 0
               do (disco-api--multipart-write-file boundary idx attachment))
      (disco-api--multipart-write-string (format "--%s--\r\n" boundary))
      (cons boundary (buffer-string)))))

(defun disco-api--normalize-poll-answer-media (answer index)
  "Normalize one poll ANSWER into create-request answer object.

INDEX is used for user-facing validation messages."
  (let* ((source-media
          (cond
           ((stringp answer)
            `((text . ,answer)))
           ((listp answer)
            (or (alist-get 'poll_media answer)
                (alist-get 'poll-media answer)
                (and (assq 'text answer) answer)))
           (t nil)))
         (text (and (listp source-media)
                    (alist-get 'text source-media)))
         (normalized-text (and (stringp text)
                               (string-trim text)))
         (emoji (and (listp source-media)
                     (alist-get 'emoji source-media)))
         (emoji-id (and (listp emoji) (alist-get 'id emoji)))
         (emoji-name (and (listp emoji) (alist-get 'name emoji)))
         (media nil))
    (unless (and (stringp normalized-text)
                 (not (string-empty-p normalized-text)))
      (user-error "disco: poll answer %d text cannot be empty" (1+ index)))
    (push `(text . ,normalized-text) media)
    (when (or emoji-id emoji-name)
      (push `(emoji . ((id . ,emoji-id)
                       (name . ,emoji-name)))
            media))
    `((poll_media . ,(nreverse media)))))

(defun disco-api--normalize-poll-request (poll)
  "Normalize POLL payload into Discord poll create-request object.

When POLL is nil, return nil."
  (when poll
    (unless (listp poll)
      (user-error "disco: poll payload must be an alist"))
    (let* ((raw-question (alist-get 'question poll))
           (question-text
            (cond
             ((stringp raw-question) (string-trim raw-question))
             ((listp raw-question)
              (let ((text (alist-get 'text raw-question)))
                (and (stringp text) (string-trim text))))
             (t nil)))
           (raw-answers (or (alist-get 'answers poll) '()))
           (normalized-answers
            (cl-loop for answer in raw-answers
                     for idx from 0
                     collect (disco-api--normalize-poll-answer-media answer idx)))
           (duration (alist-get 'duration poll))
           (layout-type (alist-get 'layout_type poll))
           (allow-multiselect-pair
            (or (assq 'allow_multiselect poll)
                (assq 'allow-multiselect poll)))
           payload)
      (unless (and (stringp question-text)
                   (not (string-empty-p question-text)))
        (user-error "disco: poll question cannot be empty"))
      (when (or (< (length normalized-answers) 2)
                (> (length normalized-answers) 10))
        (user-error "disco: poll must have between 2 and 10 answers"))
      (push `(question . ((text . ,question-text))) payload)
      (push `(answers . ,normalized-answers) payload)
      (when duration
        (unless (and (numberp duration)
                     (>= duration 1)
                     (<= duration (* 32 24)))
          (user-error "disco: poll duration must be 1..768 hours"))
        (push `(duration . ,(truncate duration)) payload))
      (when allow-multiselect-pair
        (push `(allow_multiselect . ,(if (disco-api--json-true-p (cdr allow-multiselect-pair))
                                         t
                                       :false))
              payload))
      (when layout-type
        (unless (integerp layout-type)
          (user-error "disco: poll layout_type must be an integer"))
        (push `(layout_type . ,layout-type) payload))
      (nreverse payload))))

(defun disco-api--normalize-id-string (value field-name)
  "Normalize VALUE into non-empty ID string for FIELD-NAME."
  (let ((normalized
         (cond
          ((null value) nil)
          ((stringp value) (string-trim value))
          ((numberp value) (format "%.0f" value))
          (t (string-trim (format "%s" value))))))
    (unless (and (stringp normalized)
                 (not (string-empty-p normalized)))
      (user-error "disco: %s cannot be empty" field-name))
    normalized))

(defun disco-api--normalize-id-sequence (value field-name)
  "Normalize VALUE into vector of non-empty ID strings for FIELD-NAME."
  (let* ((source (cond
                  ((null value) nil)
                  ((vectorp value) (append value nil))
                  ((listp value) value)
                  (t (list value))))
         (normalized
          (mapcar (lambda (item)
                    (disco-api--normalize-id-string item field-name))
                  source)))
    (vconcat normalized)))

(defun disco-api--normalize-allowed-mentions-parse-types (parse)
  "Normalize PARSE mention-type list/vector into vector."
  (let* ((source (cond
                  ((null parse) nil)
                  ((vectorp parse) (append parse nil))
                  ((listp parse) parse)
                  (t (list parse))))
         (allowed '("users" "roles" "everyone"))
         (normalized
          (mapcar
           (lambda (item)
             (let ((name (downcase (string-trim (format "%s" item)))))
               (unless (member name allowed)
                 (user-error "disco: allowed_mentions.parse entry `%s' is invalid" item))
               name))
           source)))
    (vconcat normalized)))

(defun disco-api--normalize-allowed-mentions (allowed-mentions)
  "Normalize ALLOWED-MENTIONS payload to Discord structure or nil."
  (cond
   ((null allowed-mentions) nil)
   ((eq allowed-mentions 'none)
    '((parse . [])))
   ((eq allowed-mentions 'all)
    '((parse . ["users" "roles" "everyone"])))
   ((not (listp allowed-mentions))
    (user-error "disco: allowed_mentions must be nil, symbol, or alist/plist"))
   (t
    (let* ((parse (disco-util-object-get allowed-mentions 'parse))
           (roles (disco-util-object-get allowed-mentions 'roles))
           (users (disco-util-object-get allowed-mentions 'users))
           (replied-user (disco-util-object-get allowed-mentions
                                                'replied_user
                                                'replied-user))
           payload)
      (when parse
        (push `(parse . ,(disco-api--normalize-allowed-mentions-parse-types parse)) payload))
      (when roles
        (push `(roles . ,(disco-api--normalize-id-sequence
                          roles
                          "allowed_mentions.roles"))
              payload))
      (when users
        (push `(users . ,(disco-api--normalize-id-sequence
                          users
                          "allowed_mentions.users"))
              payload))
      (when (not (null replied-user))
        (push `(replied_user . ,(if (disco-api--json-true-p replied-user)
                                    t
                                  :false))
              payload))
      (nreverse payload)))))

(defun disco-api--normalize-message-forward-only (forward-only)
  "Normalize FORWARD-ONLY payload for message reference forwarding."
  (when forward-only
    (unless (listp forward-only)
      (user-error "disco: message_reference.forward_only must be an alist/plist"))
    (let* ((embed-indices (disco-util-object-get forward-only
                                                 'embed_indices
                                                 'embed-indices))
           (attachment-ids (disco-util-object-get forward-only
                                                  'attachment_ids
                                                  'attachment-ids))
           payload)
      (when embed-indices
        (let* ((source (cond
                        ((vectorp embed-indices) (append embed-indices nil))
                        ((listp embed-indices) embed-indices)
                        (t (list embed-indices))))
               (normalized
                (mapcar
                 (lambda (index)
                   (unless (integerp index)
                     (user-error "disco: forward_only.embed_indices values must be integers"))
                   (when (< index 0)
                     (user-error "disco: forward_only.embed_indices cannot be negative"))
                   index)
                 source)))
          (push `(embed_indices . ,(vconcat normalized)) payload)))
      (when attachment-ids
        (push `(attachment_ids . ,(disco-api--normalize-id-sequence
                                   attachment-ids
                                   "message_reference.forward_only.attachment_ids"))
              payload))
      (nreverse payload))))

(defconst disco-api--message-reference-type-alist
  '((default . 0)
    (reply . 0)
    (forward . 1))
  "Map symbolic message reference types to Discord API integer values.")

(defconst disco-api--message-reference-type-string-alist
  '(("default" . 0)
    ("reply" . 0)
    ("forward" . 1))
  "Map string message reference types to Discord API integer values.")

(defun disco-api--normalize-message-reference-type (value)
  "Normalize message reference type VALUE into integer or nil."
  (cond
   ((null value) nil)
   ((integerp value) value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))
   ((symbolp value)
    (or (alist-get value disco-api--message-reference-type-alist)
        (user-error "disco: unsupported message_reference.type `%s'" value)))
   ((stringp value)
    (or (alist-get (downcase value)
                   disco-api--message-reference-type-string-alist
                   nil
                   #'string=)
        (user-error "disco: unsupported message_reference.type `%s'" value)))
   (t
    (user-error "disco: unsupported message_reference.type `%s'" value))))

(defun disco-api--normalize-message-reference (message-reference reply-to-message-id)
  "Normalize message reference from MESSAGE-REFERENCE/REPLY-TO-MESSAGE-ID.

REPLY-TO-MESSAGE-ID remains as backwards-compatible shorthand for replies."
  (when (and message-reference reply-to-message-id)
    (user-error "disco: use either reply-to-message-id or message-reference, not both"))
  (cond
   (reply-to-message-id
    `((message_id . ,(disco-api--normalize-id-string
                      reply-to-message-id
                      "message_reference.message_id"))))
   ((null message-reference)
    nil)
   ((stringp message-reference)
    `((message_id . ,(disco-api--normalize-id-string
                      message-reference
                      "message_reference.message_id"))))
   ((not (listp message-reference))
    (user-error "disco: message-reference must be nil, string, or alist/plist"))
   (t
    (let* ((type (disco-api--normalize-message-reference-type
                  (disco-util-object-get message-reference 'type)))
           (message-id (disco-util-object-get message-reference
                                              'message_id
                                              'message-id))
           (channel-id (disco-util-object-get message-reference
                                              'channel_id
                                              'channel-id))
           (guild-id (disco-util-object-get message-reference
                                            'guild_id
                                            'guild-id))
           (fail-if-not-exists (disco-util-object-get message-reference
                                                      'fail_if_not_exists
                                                      'fail-if-not-exists))
           (forward-only
            (disco-api--normalize-message-forward-only
             (disco-util-object-get message-reference
                                    'forward_only
                                    'forward-only)))
           payload)
      (push `(message_id . ,(disco-api--normalize-id-string
                             message-id
                             "message_reference.message_id"))
            payload)
      (when (not (null type))
        (push `(type . ,type) payload))
      (when channel-id
        (push `(channel_id . ,(disco-api--normalize-id-string
                               channel-id
                               "message_reference.channel_id"))
              payload))
      (when guild-id
        (push `(guild_id . ,(disco-api--normalize-id-string
                             guild-id
                             "message_reference.guild_id"))
              payload))
      (when (not (null fail-if-not-exists))
        (push `(fail_if_not_exists . ,(if (disco-api--json-true-p fail-if-not-exists)
                                          t
                                        :false))
              payload))
      (when forward-only
        (push `(forward_only . ,forward-only) payload))
      (when (and (eq type 1)
                 (not channel-id))
        (user-error "disco: forward message_reference requires channel_id"))
      (when (and forward-only
                 (not (eq type 1)))
        (user-error "disco: forward_only can only be used with FORWARD references"))
      (nreverse payload)))))

(defun disco-api--message-send-payload (content reply-to-message-id message-reference attachments poll allowed-mentions)
  "Build message create payload.

CONTENT is optional message text. REPLY-TO-MESSAGE-ID and MESSAGE-REFERENCE
select attribution metadata. ATTACHMENTS is normalized attachment plist list,
POLL is optional poll object. ALLOWED-MENTIONS controls mention parsing."
  (let* ((normalized-message-reference
          (disco-api--normalize-message-reference message-reference reply-to-message-id))
         (normalized-content
          (and (stringp content)
               (let ((trimmed (string-trim-right content)))
                 (unless (string-empty-p trimmed)
                   (disco-api--validate-message-content-length
                    trimmed
                    "content")))))
         (normalized-allowed-mentions
          (disco-api--normalize-allowed-mentions allowed-mentions))
         payload)
    (when normalized-content
      (push `(content . ,normalized-content) payload))
    (when normalized-message-reference
      (push `(message_reference . ,normalized-message-reference) payload))
    (when attachments
      (let ((attachment-objects nil))
        (cl-loop for attachment in attachments
                 for idx from 0
                 do (let ((entry `((id . ,idx)
                                   (filename . ,(plist-get attachment :filename)))))
                      (let ((description (plist-get attachment :description)))
                        (when description
                          (setq entry (append entry `((description . ,description))))))
                      (push entry attachment-objects)))
        (push `(attachments . ,(nreverse attachment-objects)) payload)))
    (when poll
      (push `(poll . ,poll) payload))
    (when normalized-allowed-mentions
      (push `(allowed_mentions . ,normalized-allowed-mentions) payload))
    (nreverse payload)))

(defun disco-api--normalize-non-negative-integer (value field-name)
  "Normalize VALUE into a non-negative integer for FIELD-NAME.

Return nil when VALUE is nil."
  (cond
   ((null value)
    nil)
   ((and (integerp value) (>= value 0))
    value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))
   (t
    (user-error "disco: %s must be a non-negative integer" field-name))))

(defun disco-api--normalize-read-state-type (value field-name &optional default)
  "Normalize read-state type VALUE for FIELD-NAME.

When VALUE is nil, return DEFAULT."
  (cond
   ((null value)
    default)
   ((integerp value)
    value)
   ((symbolp value)
    (or (alist-get value disco-read-state-type-alist)
        (user-error "disco: unsupported %s `%s'" field-name value)))
   (t
    (user-error "disco: unsupported %s `%s'" field-name value))))

(defun disco-api--normalize-ack-token (token)
  "Normalize read-state ack TOKEN.

Return nil, string token, or :null."
  (cond
   ((null token)
    nil)
   ((or (stringp token)
        (eq token :null))
    token)
   (t
    (user-error "disco: token must be a string, :null, or nil"))))

(defun disco-api--token-payload (&optional token)
  "Build token-only payload object for read-state ACK endpoints.

When TOKEN is omitted, return `:empty-object'."
  (let ((normalized-token (disco-api--normalize-ack-token token)))
    (if normalized-token
        `((token . ,normalized-token))
      :empty-object)))

(defun disco-api--normalize-read-state-update-entry (entry)
  "Normalize one bulk read-state update ENTRY payload."
  (let* ((read-state-type
          (disco-api--normalize-read-state-type
           (alist-get 'read_state_type entry)
           "read_states[].read_state_type"
           0))
         (normalized-channel-id
          (disco-api--normalize-id-string
           (alist-get 'channel_id entry)
           "read_states[].channel_id"))
         (normalized-message-id
          (disco-api--normalize-id-string
           (alist-get 'message_id entry)
           "read_states[].message_id"))
         payload)
    (when (string-match-p "\\`0+\\'" normalized-message-id)
      (user-error "disco: read_states[].message_id must be greater than 0"))
    (when (/= read-state-type 0)
      (push `(read_state_type . ,read-state-type) payload))
    (push `(channel_id . ,normalized-channel-id) payload)
    (push `(message_id . ,normalized-message-id) payload)
    (nreverse payload)))

(defun disco-api--read-states-bulk-payload (read-states)
  "Build payload for bulk read-state update endpoint."
  (let* ((source (cond
                  ((null read-states) nil)
                  ((vectorp read-states) (append read-states nil))
                  ((listp read-states) read-states)
                  (t (list read-states))))
         (normalized (mapcar #'disco-api--normalize-read-state-update-entry source)))
    (unless normalized
      (user-error "disco: read_states cannot be empty"))
    `((read_states . ,(vconcat normalized)))))

(cl-defun disco-api--delete-read-state-payload (&key read-state-type version)
  "Build payload for delete read-state endpoint."
  (let ((normalized-read-state-type
         (disco-api--normalize-read-state-type
          read-state-type
          "read_state_type"
          nil))
        (normalized-version
         (disco-api--normalize-non-negative-integer version "version"))
        payload)
    (when (not (null normalized-read-state-type))
      (push `(read_state_type . ,normalized-read-state-type) payload))
    (when (not (null normalized-version))
      (push `(version . ,normalized-version) payload))
    (nreverse payload)))

(defun disco-api--message-edit-payload (content &optional allowed-mentions)
  "Build payload for message edit endpoints.

ALLOWED-MENTIONS is normalized using `disco-api--normalize-allowed-mentions'."
  (let ((payload `((content . ,(disco-api--validate-message-content-length
                                 content
                                 "content")))))
    (when allowed-mentions
      (let ((normalized (disco-api--normalize-allowed-mentions allowed-mentions)))
        (when normalized
          (setq payload (append payload `((allowed_mentions . ,normalized)))))))
    payload))

(defconst disco-api--ack-message-field-order
  '(token manual mention_count flags last_viewed)
  "Canonical output order for message ACK payload fields.")

(defun disco-api--ack-message-payload (token manual mention-count flags last-viewed)
  "Build payload for message read-state ACK endpoint.

`mention_count' implies `manual=true' following Discord read-state docs.
When all fields are omitted, return `:empty-object'."
  (let* ((manual-value (or (disco-api--json-true-p manual)
                           (not (null mention-count))))
         (normalized-token
          (disco-api--normalize-ack-token token))
         (normalized-mention-count
          (disco-api--normalize-non-negative-integer mention-count "mention_count"))
         (normalized-flags
          (disco-api--normalize-non-negative-integer flags "flags"))
         (normalized-last-viewed
          (disco-api--normalize-non-negative-integer last-viewed "last_viewed"))
         (field-values `((token . ,normalized-token)
                         (manual . ,(and manual-value t))
                         (mention_count . ,normalized-mention-count)
                         (flags . ,normalized-flags)
                         (last_viewed . ,normalized-last-viewed)))
         payload)
    (dolist (field disco-api--ack-message-field-order)
      (let ((value (cdr (assq field field-values))))
        (when (not (null value))
          (push (cons field value) payload))))
    (setq payload (nreverse payload))
    (if payload payload :empty-object)))

(defun disco-api--normalize-reaction-emoji (emoji)
  "Normalize user-provided EMOJI string for Discord reaction endpoints."
  (let* ((raw (or emoji ""))
         (trimmed (string-trim raw))
         (custom
          (cond
           ((string-match "^<a?:\\([^:>]+\\):\\([0-9]+\\)>$" trimmed)
            (format "%s:%s" (match-string 1 trimmed) (match-string 2 trimmed)))
           ((string-match "^[^:]+:[0-9]+$" trimmed)
            trimmed)
           (t trimmed))))
    (unless (and (stringp custom) (not (string-empty-p custom)))
      (user-error "disco: emoji cannot be empty"))
    custom))

(defun disco-api--encode-reaction-emoji (emoji)
  "Return URL path component for reaction EMOJI."
  (url-hexify-string (disco-api--normalize-reaction-emoji emoji)))

(defun disco-api--normalize-poll-answer-id (answer-id)
  "Normalize poll ANSWER-ID to an integer."
  (let ((value
         (cond
          ((integerp answer-id) answer-id)
          ((and (stringp answer-id)
                (string-match-p "\\`[0-9]+\\'" answer-id))
           (string-to-number answer-id))
          (t nil))))
    (unless (and (integerp value) (> value 0))
      (user-error "disco: poll answer id must be a positive integer"))
    value))

(defun disco-api--normalize-poll-answer-ids (answer-ids)
  "Normalize ANSWER-IDS into a deduped integer vector.

Vector return type avoids JSON nil/list ambiguity for empty arrays."
  (let* ((source
          (cond
           ((null answer-ids) '())
           ((vectorp answer-ids) (append answer-ids nil))
           ((listp answer-ids) answer-ids)
           (t (list answer-ids))))
         (normalized (delete-dups (mapcar #'disco-api--normalize-poll-answer-id source))))
    (vconcat normalized)))

(provide 'disco-api-normalize)

;;; disco-api-normalize.el ends here
