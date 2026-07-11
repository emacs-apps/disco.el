;;; disco-channel-type.el --- Channel type helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared Discord channel type metadata used by state, root, and room layers.

;;; Code:

(defconst disco-channel-type-spec-alist
  '((0 :name "text"
       :root-visible t
       :open-mode timeline
       :searchable t
       :thread-parent t)
    (1 :name "dm"
       :root-visible t
       :open-mode timeline
       :searchable t
       :private t
       :dm-like t
       :direct-message t)
    (2 :name "voice"
       :root-visible t
       :open-mode timeline)
    (3 :name "group-dm"
       :root-visible t
       :open-mode timeline
       :searchable t
       :private t
       :dm-like t
       :group-dm t)
    (4 :name "category"
       :structural t)
    (5 :name "announcement"
       :root-visible t
       :open-mode timeline
       :searchable t
       :thread-parent t)
    (6 :name "store")
    (10 :name "announcement-thread"
        :root-visible t
        :open-mode timeline
        :searchable t
        :thread t)
    (11 :name "public-thread"
        :root-visible t
        :open-mode timeline
        :searchable t
        :thread t)
    (12 :name "private-thread"
        :root-visible t
        :open-mode timeline
        :searchable t
        :thread t)
    (13 :name "stage"
        :root-visible t
        :open-mode timeline)
    (14 :name "directory"
        :root-visible t
        :open-mode inspect
        :inspect-note "Directory channel browsing is not implemented yet. Use this view to inspect the raw channel metadata.")
    (15 :name "forum"
        :root-visible t
        :open-mode thread-directory
        :thread-parent t
        :thread-only-parent t
        :forum-or-media t)
    (16 :name "media"
        :root-visible t
        :open-mode thread-directory
        :thread-parent t
        :thread-only-parent t
        :forum-or-media t)
    (17 :name "lobby"
        :root-visible t
        :open-mode inspect
        :inspect-note "Lobby channel timelines are not implemented yet. Use this view to inspect the channel and any linked lobby metadata.")
    (18 :name "ephemeral-dm"
        :root-visible t
        :open-mode timeline
        :searchable t
        :private t
        :dm-like t
        :direct-message t))
  "Declarative map of Discord channel type to capability plist.")

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

(provide 'disco-channel-type)

;;; disco-channel-type.el ends here
