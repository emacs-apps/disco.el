;;; disco-read-state.el --- Shared read-state constants for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared declarative read-state model used by API/state/gateway layers.

;;; Code:

(defconst disco-read-state-type-alist
  '((channel . 0)
    (guild-event . 1)
    (notification-center . 2)
    (guild-home . 3)
    (guild-onboarding-question . 4)
    (message-requests . 5))
  "Declarative map of Discord read-state type names to integer values.")

(defconst disco-read-state-type-channel
  (alist-get 'channel disco-read-state-type-alist)
  "Read-state type value for channel message unreads.")

(defconst disco-read-state-type-guild-event
  (alist-get 'guild-event disco-read-state-type-alist)
  "Read-state type value for guild event feature.")

(defconst disco-read-state-type-notification-center
  (alist-get 'notification-center disco-read-state-type-alist)
  "Read-state type value for notification center feature.")

(defconst disco-read-state-type-guild-home
  (alist-get 'guild-home disco-read-state-type-alist)
  "Read-state type value for guild home feature.")

(defconst disco-read-state-type-guild-onboarding-question
  (alist-get 'guild-onboarding-question disco-read-state-type-alist)
  "Read-state type value for guild onboarding question feature.")

(defconst disco-read-state-type-message-requests
  (alist-get 'message-requests disco-read-state-type-alist)
  "Read-state type value for message request feature.")

(defconst disco-read-state-spec-alist
  `((,disco-read-state-type-channel
     :entity-field last_message_id
     :counter-field mention_count)
    (,disco-read-state-type-guild-event
     :entity-field last_acked_id
     :counter-field badge_count)
    (,disco-read-state-type-notification-center
     :entity-field last_acked_id
     :counter-field badge_count)
    (,disco-read-state-type-guild-home
     :entity-field last_acked_id
     :counter-field badge_count)
    (,disco-read-state-type-guild-onboarding-question
     :entity-field last_acked_id
     :counter-field badge_count)
    (,disco-read-state-type-message-requests
     :entity-field last_acked_id
     :counter-field badge_count))
  "Declarative map of read-state type to state field specs.")

(defun disco-read-state-spec (read-state-type)
  "Return read-state spec plist for READ-STATE-TYPE."
  (alist-get read-state-type disco-read-state-spec-alist))

(defun disco-read-state-entity-field (read-state-type)
  "Return entity cursor field symbol for READ-STATE-TYPE."
  (plist-get (disco-read-state-spec read-state-type) :entity-field))

(defun disco-read-state-counter-field (read-state-type)
  "Return unread counter field symbol for READ-STATE-TYPE."
  (plist-get (disco-read-state-spec read-state-type) :counter-field))

(defconst disco-read-state-flag-alist
  '((is-guild-channel . 1)
    (is-thread . 2)
    (is-mention-low-importance . 4))
  "Declarative map of read-state channel flag names to bit values.")

(defconst disco-read-state-flag-is-guild-channel
  (alist-get 'is-guild-channel disco-read-state-flag-alist)
  "Flag bit for guild channel read-state.")

(defconst disco-read-state-flag-is-thread
  (alist-get 'is-thread disco-read-state-flag-alist)
  "Flag bit for thread channel read-state.")

(defconst disco-read-state-flag-is-mention-low-importance
  (alist-get 'is-mention-low-importance disco-read-state-flag-alist)
  "Flag bit indicating that all counted mentions are non-ping notifications.")

(provide 'disco-read-state)

;;; disco-read-state.el ends here
