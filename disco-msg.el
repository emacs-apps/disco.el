;;; disco-msg.el --- Message model helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared helpers for Discord message identity/reference access.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'disco-state)
(require 'disco-util)

(defconst disco-msg--reference-field-map
  '((id . message_id)
    (channel-id . channel_id)
    (guild-id . guild_id))
  "Declarative mapping from reference field role to payload key.")

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
