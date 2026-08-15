;;; disco-channel-type.el --- Channel type helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared Discord channel type and title metadata used by state, root, room,
;; and profile layers.

;;; Code:
(require 'disco-customize)

(defconst disco-channel-flag-obfuscated (ash 1 17)
  "Discord channel flag `OBFUSCATED'.

Discord uses this flag on the limited channel objects included by the
`CHANNEL_OBFUSCATION' Gateway capability.")

(defconst disco-channel-type-spec-alist
  '((0
     :name "text"
     :title-kind channel
     :root-visible t
     :open-mode timeline
     :searchable t
     :thread-parent t)
    (1
     :name "dm"
     :title-kind user
     :root-visible t
     :open-mode timeline
     :searchable t
     :private t
     :dm-like t
     :direct-message t)
    (2
     :name "voice"
     :title-kind voice
     :root-visible t
     :open-mode timeline)
    (3
     :name "group-dm"
     :title-kind group
     :root-visible t
     :open-mode timeline
     :searchable t
     :private t
     :dm-like t
     :group-dm t)
    (4 :name "category" :title-kind none :structural t)
    (5
     :name "announcement"
     :title-kind announcement
     :root-visible t
     :open-mode timeline
     :searchable t
     :thread-parent t)
    (6 :name "store" :title-kind media)
    (10
     :name "announcement-thread"
     :title-kind thread
     :root-visible t
     :open-mode timeline
     :searchable t
     :thread t)
    (11
     :name "public-thread"
     :title-kind thread
     :root-visible t
     :open-mode timeline
     :searchable t
     :thread t)
    (12
     :name "private-thread"
     :title-kind private-thread
     :root-visible t
     :open-mode timeline
     :searchable t
     :thread t)
    (13
     :name "stage"
     :title-kind stage
     :root-visible t
     :open-mode timeline)
    (14
     :name "directory"
     :title-kind directory
     :root-visible t
     :open-mode inspect
     :inspect-note "Directory channel browsing is not implemented yet. Use this view to inspect the raw channel metadata.")
    (15
     :name "forum"
     :title-kind forum
     :root-visible t
     :open-mode thread-directory
     :thread-parent t
     :thread-only-parent t
     :forum-or-media t)
    (16
     :name "media"
     :title-kind media
     :root-visible t
     :open-mode thread-directory
     :thread-parent t
     :thread-only-parent t
     :forum-or-media t)
    (17
     :name "lobby"
     :title-kind lobby
     :root-visible t
     :open-mode inspect
     :inspect-note "Lobby channel timelines are not implemented yet. Use this view to inspect the channel and any linked lobby metadata.")
    (18
     :name "ephemeral-dm"
     :title-kind ephemeral-user
     :root-visible t
     :open-mode timeline
     :searchable t
     :private t
     :dm-like t
     :direct-message t))
  "Declarative map of Discord channel type to capability plist.")


(defun disco-title--bracket-selector-match-p (selector kind subject)
  "Return non-nil when SELECTOR matches presentation KIND and SUBJECT."
  (cond
   ((eq selector t)
    t)
   ((eq selector kind)
    t)
   ((and (consp selector) (eq (car selector) 'channel-type))
    (let ((channel-type
           (cond
            ((integerp subject)
             subject)
            ((listp subject)
             (alist-get 'type subject)))))
      (and (integerp channel-type)
           (memq channel-type (cdr selector)))))
   ((functionp selector)
    (funcall selector kind subject))
   (t
    nil)))

(defun disco-title-brackets (kind &optional subject)
  "Return delimiters selected for presentation KIND and optional SUBJECT."
  (catch 'brackets
    (dolist (rule disco-title-bracket-rules)
      (unless (and (consp rule)
                   (consp (cdr rule))
                   (stringp (cadr rule))
                   (consp (cddr rule))
                   (stringp (caddr rule))
                   (null (cdddr rule)))
        (error "Invalid Disco title delimiter rule: %S" rule))
      (when (disco-title--bracket-selector-match-p
             (car rule) kind subject)
        (throw 'brackets (list (cadr rule) (caddr rule)))))
    (error "No Disco title delimiter rule matches %S" kind)))

(defun disco-title-format (kind title &optional subject)
  "Wrap TITLE in delimiters selected for KIND and optional SUBJECT."
  (unless (stringp title)
    (error "Disco title must be a string: %S" title))
  (let ((brackets (disco-title-brackets kind subject)))
    (concat (car brackets) title (cadr brackets))))

(defun disco-title-compact-count (value)
  "Return non-negative numeric VALUE in compact title-trail form."
  (let ((n (max 0 (or value 0))))
    (cond
     ((>= n 1000000)
      (replace-regexp-in-string
       "\\.0m\\'" "m" (format "%.1fm" (/ n 1000000.0))))
     ((>= n 1000)
      (replace-regexp-in-string
       "\\.0k\\'" "k" (format "%.1fk" (/ n 1000.0))))
     (t
      (number-to-string n)))))

(defun disco-channel-type-value (channel-or-type)
  "Return numeric channel type from CHANNEL-OR-TYPE."
  (cond
   ((listp channel-or-type)
    (alist-get 'type channel-or-type))
   ((integerp channel-or-type)
    channel-or-type)
   (t nil)))

(defun disco-channel-type-spec (channel-or-type)
  "Return capability plist for CHANNEL-OR-TYPE."
  (alist-get (disco-channel-type-value channel-or-type)
             disco-channel-type-spec-alist))

(defun disco-channel-type-get (channel-or-type property)
  "Return PROPERTY from CHANNEL-OR-TYPE capability spec."
  (plist-get (disco-channel-type-spec channel-or-type) property))

(defun disco-channel-type-name (channel-or-type)
  "Return descriptive type name for CHANNEL-OR-TYPE."
  (let* ((type (disco-channel-type-value channel-or-type))
         (name (disco-channel-type-get type :name)))
    (or name
        (and type (format "type-%s" type))
        "unknown")))

(defun disco-channel-private-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE is a private channel."
  (eq t (disco-channel-type-get channel-or-type :private)))

(defun disco-channel-dm-like-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE behaves like a DM in UI."
  (eq t (disco-channel-type-get channel-or-type :dm-like)))

(defun disco-channel-direct-message-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE is a one-to-one private channel."
  (eq t (disco-channel-type-get channel-or-type :direct-message)))

(defun disco-channel-group-dm-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE is a group DM channel."
  (eq t (disco-channel-type-get channel-or-type :group-dm)))

(defun disco-channel-title-kind (channel-or-type)
  "Return the stable presentation title kind for CHANNEL-OR-TYPE."
  (or (disco-channel-type-get channel-or-type :title-kind) 'channel))

(defun disco-channel-title-brackets (channel-or-type)
  "Return the presentation bracket pair for CHANNEL-OR-TYPE."
  (disco-title-brackets (disco-channel-title-kind channel-or-type)
                        channel-or-type))

(defun disco-channel-format-title (channel-or-type title)
  "Wrap TITLE according to the Discord CHANNEL-OR-TYPE domain."
  (disco-title-format (disco-channel-title-kind channel-or-type) title
                      channel-or-type))

(defun disco-channel-thread-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE is a thread channel."
  (eq t (disco-channel-type-get channel-or-type :thread)))

(defun disco-channel-thread-parent-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE can expose threads in UI."
  (eq t (disco-channel-type-get channel-or-type :thread-parent)))

(defun disco-channel-thread-only-parent-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE is a thread-only parent channel."
  (eq t (disco-channel-type-get channel-or-type :thread-only-parent)))

(defun disco-channel-forum-or-media-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE is a forum/media parent channel."
  (eq t (disco-channel-type-get channel-or-type :forum-or-media)))

(defun disco-channel-root-visible-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE should appear as a root row."
  (eq t (disco-channel-type-get channel-or-type :root-visible)))

(defun disco-channel-open-mode (channel-or-type)
  "Return open mode symbol for CHANNEL-OR-TYPE, or nil when unsupported."
  (disco-channel-type-get channel-or-type :open-mode))

(defun disco-channel-searchable-p (channel-or-type)
  "Return non-nil when CHANNEL-OR-TYPE supports remote message search."
  (eq t (disco-channel-type-get channel-or-type :searchable)))

(defun disco-channel-inspect-note (channel-or-type)
  "Return inspector note string for CHANNEL-OR-TYPE, or nil."
  (disco-channel-type-get channel-or-type :inspect-note))

(defun disco-channel-obfuscated-p (channel)
  "Return non-nil when CHANNEL explicitly carries `OBFUSCATED'.

Only a non-negative integer `flags' value is accepted.  In particular, a
missing or malformed field is not evidence that a channel is obfuscated."
  (let ((flags (and (listp channel) (alist-get 'flags channel))))
    (and (integerp flags)
         (>= flags 0)
         (not (zerop (logand flags disco-channel-flag-obfuscated))))))

(provide 'disco-channel-type)

;;; disco-channel-type.el ends here
