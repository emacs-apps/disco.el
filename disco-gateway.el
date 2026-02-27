;;; disco-gateway.el --- Live update engine for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; This module provides a gateway-like event stream for MVP using polling.
;;
;; Why polling first:
;; - keeps dependency and protocol complexity low while REST layer stabilizes;
;; - still models discordo/oxicord event flow as create/update/delete dispatch.

;;; Code:

(require 'cl-lib)
(require 'disco-api)
(require 'disco-customize)
(require 'disco-state)

(defvar disco-gateway-event-hook nil
  "Hook called with one event plist argument.

Event schema:
- :type one of `message-create' `message-update' `message-delete'
- :channel-id string
- :message message object for create/update
- :message-id string for delete")

(defvar disco-gateway--timer nil)
(defvar disco-gateway--watch-counts (make-hash-table :test #'equal))
(defvar disco-gateway--polling-busy nil)

(defun disco-gateway-running-p ()
  "Return non-nil when live update loop is active."
  (timerp disco-gateway--timer))

(defun disco-gateway-start ()
  "Start live update polling loop."
  (interactive)
  (unless (disco-gateway-running-p)
    (setq disco-gateway--timer
          (run-at-time 0 disco-live-update-interval #'disco-gateway--poll-once))
    (message "disco: live updates started")))

(defun disco-gateway-stop ()
  "Stop live update polling loop."
  (interactive)
  (when (timerp disco-gateway--timer)
    (cancel-timer disco-gateway--timer)
    (setq disco-gateway--timer nil)
    (setq disco-gateway--polling-busy nil)
    (message "disco: live updates stopped")))

(defun disco-gateway-watch-channel (channel-id)
  "Start watching CHANNEL-ID for live message changes."
  (let ((count (gethash channel-id disco-gateway--watch-counts 0)))
    (puthash channel-id (1+ count) disco-gateway--watch-counts))
  (when disco-enable-live-updates
    (disco-gateway-start)))

(defun disco-gateway-unwatch-channel (channel-id)
  "Decrease watch count for CHANNEL-ID and stop watching if no users left."
  (let ((count (gethash channel-id disco-gateway--watch-counts 0)))
    (cond
     ((<= count 1)
      (remhash channel-id disco-gateway--watch-counts))
     (t
      (puthash channel-id (1- count) disco-gateway--watch-counts))))
  (when (and (= (hash-table-count disco-gateway--watch-counts) 0)
             (disco-gateway-running-p))
    (disco-gateway-stop)))

(defun disco-gateway--message-id (msg)
  "Extract message ID string from MSG."
  (alist-get 'id msg))

(defun disco-gateway--message-signature (msg)
  "Build message signature used to detect updates."
  (list (alist-get 'content msg)
        (alist-get 'edited_timestamp msg)
        (alist-get 'timestamp msg)))

(defun disco-gateway--index-messages (messages)
  "Build hash map message-id -> message object from MESSAGES list."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (msg messages)
      (let ((id (disco-gateway--message-id msg)))
        (when id
          (puthash id msg table))))
    table))

(defun disco-gateway--emit (event)
  "Emit EVENT plist to `disco-gateway-event-hook'."
  (run-hook-with-args 'disco-gateway-event-hook event))

(defun disco-gateway--emit-diff (channel-id old-messages new-messages)
  "Emit create/update/delete events by diffing OLD-MESSAGES and NEW-MESSAGES."
  (let ((old-index (disco-gateway--index-messages old-messages))
        (new-index (disco-gateway--index-messages new-messages)))
    (dolist (msg new-messages)
      (let* ((id (disco-gateway--message-id msg))
             (old (and id (gethash id old-index))))
        (when id
          (cond
           ((null old)
            (disco-gateway--emit
             (list :type 'message-create :channel-id channel-id :message msg)))
           ((not (equal (disco-gateway--message-signature old)
                        (disco-gateway--message-signature msg)))
            (disco-gateway--emit
             (list :type 'message-update :channel-id channel-id :message msg)))))))

    (dolist (msg old-messages)
      (let ((id (disco-gateway--message-id msg)))
        (when (and id (null (gethash id new-index)))
          (disco-gateway--emit
           (list :type 'message-delete :channel-id channel-id :message-id id)))))))

(defun disco-gateway--poll-channel (channel-id)
  "Poll one CHANNEL-ID and emit gateway-like events." 
  (let* ((old-messages (or (disco-state-messages channel-id) '()))
         (new-messages (or (disco-api-channel-messages
                            channel-id nil disco-live-update-message-limit)
                           '())))
    (disco-state-put-messages channel-id new-messages)
    (disco-gateway--emit-diff channel-id old-messages new-messages)))

(defun disco-gateway--watched-channels ()
  "Return list of currently watched channel IDs."
  (let (acc)
    (maphash (lambda (channel-id _count)
               (push channel-id acc))
             disco-gateway--watch-counts)
    acc))

(defun disco-gateway--poll-once ()
  "Run one polling iteration for watched channels." 
  (when (and disco-enable-live-updates
             (not disco-gateway--polling-busy)
             (> (hash-table-count disco-gateway--watch-counts) 0))
    (setq disco-gateway--polling-busy t)
    (unwind-protect
        (dolist (channel-id (disco-gateway--watched-channels))
          (condition-case err
              (disco-gateway--poll-channel channel-id)
            (error
             (message "disco: live update failed for channel %s: %s"
                      channel-id (error-message-string err)))))
      (setq disco-gateway--polling-busy nil))))

(provide 'disco-gateway)

;;; disco-gateway.el ends here
