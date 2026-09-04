;;; disco-room-pin.el --- Pin interaction for Disco rooms -*- lexical-binding: t; -*-

;;; Commentary:

;; Room-local pin mutations, pin acknowledgements, and the independently owned
;; pinned-message Generated Surface.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-invalidation)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'disco-api)
(require 'disco-msg)
(require 'disco-state)
(require 'disco-runtime)
(require 'disco-room-compose)

(declare-function disco-room--async-error-message "disco-room" (err))
(declare-function disco-room--callback-active-p "disco-room" (buffer channel-id view))
(declare-function disco-room--channel-buffer-p "disco-room" (buffer channel-id view))
(declare-function disco-room--channel-object "disco-room" ())
(declare-function disco-room--ensure-surface "disco-room" ())
(declare-function disco-room--message-at-point "disco-room" ())
(declare-function disco-room--message-by-id "disco-room" (message-id))
(declare-function disco-room--update-message-locally "disco-room" (message-id function))
(declare-function disco-room--format-time "disco-room-render" (timestamp))
(declare-function disco-room-jump-to-message
                  "disco-room" (message-id &optional channel-id))

(defvar disco-room--channel-id)
(defvar-local disco-room--pin-op-seq 0
  "Monotonic owner token for message pin requests in this room view.")
(defvar-local disco-room--pin-ops nil
  "Current message pin operation keyed by message id.")
(defvar-local disco-room--pins-ack-seq 0
  "Monotonic owner token for pinned-message acknowledgements.")
(defun disco-room-pin-reset ()
  "Reset pin-local state in the current room buffer."
  (setq-local disco-room--pin-op-seq 0
              disco-room--pin-ops (make-hash-table :test #'equal)
              disco-room--pins-ack-seq 0))

(defun disco-room-pin-forget-message (message-id)
  "Discard pending pin state belonging to deleted MESSAGE-ID."
  (disco-room--pin-ops-clear-message message-id))
(defun disco-room--pin-message-unavailable-reason (&optional msg)
  "Return reason toggling a message pin is unavailable for MSG, or nil."
  (let ((msg (or msg (ignore-errors (disco-room--message-at-point)))))
    (cond
     ((not (listp msg))
      "point is not on a message")
     ((not (alist-get 'id msg))
      "message has no id")
     ((not disco-room--channel-id)
      "current room has no channel")
     (t
      (disco-room--channel-permission-reason '(pin-messages))))))
(defun disco-room-ack-channel-pins ()
  "Acknowledge currently pinned messages in the active room channel."
  (interactive)
  (let* ((room-buffer (current-buffer))
         (channel-id disco-room--channel-id)
         (view (disco-room--ensure-surface))
         (channel (disco-room--channel-object))
         (last-pin-timestamp (and channel (alist-get 'last_pin_timestamp channel))))
    (cond
     ((not channel-id)
      (user-error "disco: room is not bound to a channel"))
     ((not (stringp last-pin-timestamp))
      (message "disco: channel %s has no pinned messages" channel-id))
     ((not (disco-state-channel-has-unread-pins-p channel))
      (message "disco: pins already acknowledged for %s" channel-id))
     (t
      (let ((ack-seq (cl-incf disco-room--pins-ack-seq)))
        (disco-api-ack-channel-pins-async
         channel-id
         :on-success
         (lambda (_response)
           (when (disco-room--callback-active-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               ;; A later local request owns the cursor.  A Gateway ACK may
               ;; also have advanced it while this request was in flight.  The
               ;; state merge owns version/timezone-aware monotonicity.
               (when (= ack-seq disco-room--pins-ack-seq)
                 (disco-state-apply-channel-pins-ack
                  channel-id last-pin-timestamp)
                 (disco-room--queue-update view :part 'frame)
                 (message "disco: acknowledged pins for %s" channel-id)))))
         :on-error
         (lambda (err)
           (when (disco-room--callback-active-p room-buffer channel-id view)
             (with-current-buffer room-buffer
               (when (= ack-seq disco-room--pins-ack-seq)
                 (message "disco: pins ack failed for %s: %s"
                          channel-id
                          (disco-room--async-error-message err))))))))))))
(defun disco-room--pin-op-key (message-id)
  "Return normalized key for a message pin operation."
  (format "%s" message-id))

(defun disco-room--pin-op-begin (message-id pinned)
  "Begin and return an owner token setting MESSAGE-ID to PINNED."
  (unless (hash-table-p disco-room--pin-ops)
    (setq disco-room--pin-ops (make-hash-table :test #'equal)))
  (let ((token (cl-incf disco-room--pin-op-seq)))
    (puthash (disco-room--pin-op-key message-id)
             (list :token token :pinned (and pinned t))
             disco-room--pin-ops)
    token))

(defun disco-room--pin-op-for (message-id)
  "Return current pin operation for MESSAGE-ID, or nil."
  (and (hash-table-p disco-room--pin-ops)
       (gethash (disco-room--pin-op-key message-id) disco-room--pin-ops)))

(defun disco-room--pin-op-current-p (message-id token)
  "Return non-nil when TOKEN still owns MESSAGE-ID's pin operation."
  (let ((operation (disco-room--pin-op-for message-id)))
    (and (listp operation)
         (= (or (plist-get operation :token) -1) token))))

(defun disco-room--pin-op-finish (message-id token)
  "Finish MESSAGE-ID's pin operation when TOKEN still owns it."
  (when (disco-room--pin-op-current-p message-id token)
    (remhash (disco-room--pin-op-key message-id) disco-room--pin-ops)
    t))

(defun disco-room--pin-ops-clear-message (message-id)
  "Invalidate the pending pin operation for MESSAGE-ID."
  (when (hash-table-p disco-room--pin-ops)
    (remhash (disco-room--pin-op-key message-id) disco-room--pin-ops)))
(defun disco-room--message-pinned-p (msg)
  "Return non-nil when MSG is pinned."
  (eq (alist-get 'pinned msg) t))

(defun disco-room--message-with-pinned-state (msg pinned)
  "Return MSG copy with its pinned flag set to PINNED."
  (let ((updated (copy-tree msg)))
    (setf (alist-get 'pinned updated nil 'remove) (if pinned t :false))
    updated))

(defun disco-room--toggle-pin-on-msg (msg)
  "Toggle the pin state of MSG."
  (disco-room--ensure-action-available
   (disco-room--pin-message-unavailable-reason msg)
   "toggle message pins")
  (disco-room-toggle-pin (alist-get 'id msg)))

(defun disco-room-toggle-pin (&optional message-id)
  "Toggle whether MESSAGE-ID at point is pinned."
  (interactive)
  (let* ((target-id (or message-id (disco-room--message-id-required-at-point)))
         (msg (or (disco-room--message-by-id target-id)
                  (disco-room--message-at-point))))
    (disco-room--ensure-action-available
     (disco-room--pin-message-unavailable-reason msg)
     "toggle message pins")
    (let* ((room-buffer (current-buffer))
           (channel-id disco-room--channel-id)
           (view (disco-room--ensure-surface))
           (pending (disco-room--pin-op-for target-id))
           (pinned (not (if pending
                            (plist-get pending :pinned)
                          (disco-room--message-pinned-p msg))))
           (op-token (disco-room--pin-op-begin target-id pinned))
           (operation (if pinned
                          #'disco-api-pin-message-async
                        #'disco-api-unpin-message-async))
           (verb (if pinned "pinned" "unpinned")))
      (funcall
       operation channel-id target-id
       :on-success
       (lambda (_response)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--pin-op-current-p target-id op-token)
               (disco-room--update-message-locally
                target-id
                (lambda (message)
                  (disco-room--message-with-pinned-state message pinned)))
               (disco-room--pin-op-finish target-id op-token)
               (disco-room--queue-update view :entry target-id)
               (message "disco: message %s" verb)))))
       :on-error
       (lambda (err)
         (when (disco-room--channel-buffer-p room-buffer channel-id view)
           (with-current-buffer room-buffer
             (when (disco-room--pin-op-finish target-id op-token)
               (message "disco: %s message failed: %s"
                        (if pinned "pin" "unpin")
                        (disco-room--async-error-message err))))))))))

(defvar-local disco-room-pinned-messages--channel-id nil
  "Channel whose pinned messages this buffer presents.")

(defvar-local disco-room-pinned-messages--channel-name nil
  "Best-known display name for `disco-room-pinned-messages--channel-id'.")

(defvar-local disco-room-pinned-messages--items nil
  "Accumulated Message Pin records in endpoint page order.")

(defvar-local disco-room-pinned-messages--next-before nil
  "Timestamp cursor for the next pinned-message page.")

(defvar-local disco-room-pinned-messages--has-more-p nil
  "Non-nil when the pinned-message endpoint reports another page.")

(defvar-local disco-room-pinned-messages--loading-p nil
  "Non-nil while this pinned-message Surface owns an active request.")

(defvar-local disco-room-pinned-messages--error nil
  "Most recent pinned-message request error.")

(defvar-local disco-room-pinned-messages--generation 0
  "Monotonic owner token for pinned-message page requests.")

(defun disco-room-pinned-messages--surface-id (channel-id)
  "Return the stable Appkit Surface identity for CHANNEL-ID."
  (list 'room 'pinned-messages channel-id))

(defun disco-room-pinned-messages--buffer-name (channel-id channel-name)
  "Return a readable pinned-message buffer name for CHANNEL-ID."
  (format "*disco:pins:%s (%s)*" (or channel-name channel-id) channel-id))

(defun disco-room-pinned-messages--callback-active-p
    (buffer channel-id view generation)
  "Return non-nil when BUFFER still owns pinned request GENERATION."
  (and (buffer-live-p buffer)
       (appkit-surface-live-p view)
       (with-current-buffer buffer
         (and (eq major-mode 'disco-room-pinned-messages-mode)
              (equal disco-room-pinned-messages--channel-id channel-id)
              (eq view (appkit-current-surface))
              (eql generation disco-room-pinned-messages--generation)))))

(defun disco-room-pinned-messages--entry-message-id (entry)
  "Return the nested canonical message ID from Message Pin ENTRY."
  (disco-msg-normalize-id
   (and (listp entry)
        (listp (alist-get 'message entry))
        (alist-get 'id (alist-get 'message entry)))))

(defun disco-room-pinned-messages--merge-page (existing page)
  "Append unique usable Message Pin PAGE records to EXISTING.

Endpoint order is preserved; malformed records without a nested message ID are
not rendered or retained."
  (let ((seen (make-hash-table :test #'equal))
        additions)
    (dolist (entry existing)
      (when-let* ((message-id (disco-room-pinned-messages--entry-message-id entry)))
        (puthash message-id t seen)))
    (dolist (entry page)
      (when-let* ((message-id (disco-room-pinned-messages--entry-message-id entry))
                  ((not (gethash message-id seen))))
        (puthash message-id t seen)
        (push entry additions)))
    (append existing (nreverse additions))))

(defun disco-room-pinned-messages--page-from-response (response)
  "Normalize one pinned-message endpoint RESPONSE.

Return a plist containing `:items', `:has-more', and `:next-before'.  Discord
uses the final Message Pin's `pinned_at' timestamp as the continuation cursor."
  (let* ((items (alist-get 'items response))
         (has-more (eq (alist-get 'has_more response) t))
         (last-item (car (last items)))
         (next-before (and (listp last-item)
                           (alist-get 'pinned_at last-item))))
    (unless (listp items)
      (error "disco: malformed pinned-message response"))
    (when (and has-more
               (not (and (stringp next-before)
                         (not (string-empty-p next-before)))))
      (error "disco: pinned-message response has no continuation cursor"))
    (list :items items
          :has-more has-more
          :next-before next-before)))

(defun disco-room-pinned-messages--request-sync (surface)
  "Schedule one pinned-message render on SURFACE."
  (when (appkit-surface-live-p surface)
    (appkit-surface-post surface 'render)))

(defun disco-room-pinned-messages--complete-error (view error)
  "Set current pinned-message Surface request failure to ERROR."
  (setq-local disco-room-pinned-messages--loading-p nil
              disco-room-pinned-messages--error
              (disco-room--async-error-message error))
  (disco-room-pinned-messages--request-sync view))

(defun disco-room-pinned-messages--request-page (view reset)
  "Asynchronously request one pinned-message page for VIEW.

When RESET is non-nil, the returned page replaces the cached projection."
  (let ((channel-id disco-room-pinned-messages--channel-id))
    (unless (and (stringp channel-id) (not (string-empty-p channel-id)))
      (user-error "disco: pinned-message Surface has no channel"))
    (when (and (not reset) disco-room-pinned-messages--loading-p)
      (user-error "disco: pinned messages are already loading"))
    (when (and (not reset) (not disco-room-pinned-messages--has-more-p))
      (user-error "disco: no more pinned messages"))
    (let ((before (unless reset disco-room-pinned-messages--next-before)))
      (when (and (not reset)
                 (not (and (stringp before) (not (string-empty-p before)))))
        (setq-local disco-room-pinned-messages--has-more-p nil
                    disco-room-pinned-messages--error
                    "Pinned-message response has no continuation cursor")
        (disco-room-pinned-messages--request-sync view)
        (user-error "disco: pinned messages cannot continue without a cursor"))
      (let ((buffer (current-buffer))
            (generation (cl-incf disco-room-pinned-messages--generation)))
        (setq-local disco-room-pinned-messages--loading-p t
                    disco-room-pinned-messages--error nil)
        (disco-room-pinned-messages--request-sync view)
        (disco-api-channel-pins-async
         channel-id
         :before before
         :limit 50
         :owner view
         :on-success
         (lambda (response)
           (when (disco-room-pinned-messages--callback-active-p
                  buffer channel-id view generation)
             (with-current-buffer buffer
               (condition-case err
                   (let ((page (disco-room-pinned-messages--page-from-response
                                response)))
                     (setq-local
                      disco-room-pinned-messages--items
                      (if reset
                          (disco-room-pinned-messages--merge-page
                           nil (plist-get page :items))
                        (disco-room-pinned-messages--merge-page
                         disco-room-pinned-messages--items
                         (plist-get page :items)))
                      disco-room-pinned-messages--next-before
                      (plist-get page :next-before)
                      disco-room-pinned-messages--has-more-p
                      (plist-get page :has-more)
                      disco-room-pinned-messages--loading-p nil
                      disco-room-pinned-messages--error nil)
                     (disco-room-pinned-messages--request-sync view))
                 (error
                  (disco-room-pinned-messages--complete-error view err))))))
         :on-error
         (lambda (error)
           (when (disco-room-pinned-messages--callback-active-p
                  buffer channel-id view generation)
             (with-current-buffer buffer
               (disco-room-pinned-messages--complete-error view error)))))))))


(defun disco-room-pinned-messages-refresh ()
  "Refresh the pinned-message projection in the current browser buffer."
  (interactive)
  (disco-room-pinned-messages--request-page (appkit-current-surface) t))

(defun disco-room-pinned-messages-load-more ()
  "Load the next pinned-message page in the current browser buffer."
  (interactive)
  (disco-room-pinned-messages--request-page (appkit-current-surface) nil))

(defun disco-room-pinned-messages--open-entry (entry)
  "Open the nested message from Message Pin ENTRY."
  (let ((message-id (disco-room-pinned-messages--entry-message-id entry))
        (channel-id disco-room-pinned-messages--channel-id))
    (unless message-id
      (user-error "disco: pinned-message entry has no message ID"))
    (disco-room-jump-to-message message-id channel-id)))

(defun disco-room-pinned-messages--insert-entry (entry)
  "Insert one clickable Message Pin ENTRY."
  (let* ((message (alist-get 'message entry))
         (message-id (disco-room-pinned-messages--entry-message-id entry))
         (pinned-at (alist-get 'pinned_at entry))
         (label
          (format "%s  %s"
                  (if (and (stringp pinned-at) (not (string-empty-p pinned-at)))
                      (disco-room--format-time pinned-at)
                    "unknown-time")
                  (disco-msg-preview-line message)))
         (start (point)))
    (appkit-presentation-insert-label-line
     label
     :line-properties (list 'disco-message-id message-id))
    (appkit-ui-add-action
     start (1- (point))
     (lambda () (disco-room-pinned-messages--open-entry entry))
     :help-echo "Open pinned message")))

(defun disco-room-pinned-messages--list-spec ()
  "Return the current pinned-message browser list specification."
  (let* ((items (or disco-room-pinned-messages--items '()))
         (loading-note
          (cond
           (disco-room-pinned-messages--loading-p
            (if items "(refreshing pinned messages...)" "(loading pinned messages...)"))
           ((not disco-room-pinned-messages--has-more-p) "(no more pinned messages)")
           (t nil))))
    (appkit-presentation-list-spec-create
     :title (format "Pinned Messages: %s"
                    (or disco-room-pinned-messages--channel-name
                        disco-room-pinned-messages--channel-id))
     :summary (format "Loaded: %d%s"
                      (length items)
                      (if disco-room-pinned-messages--has-more-p
                          "  ·  more available"
                        ""))
     :loading-note loading-note
     :items items
     :item-inserter #'disco-room-pinned-messages--insert-entry
     :empty-text "(no visible pinned messages)"
     :footer-lines
     (when disco-room-pinned-messages--error
       (list (format "Error: %s" disco-room-pinned-messages--error))))))

(defun disco-room-pinned-messages--render (surface)
  "Render pinned messages in SURFACE's exact host buffer."
  (appkit-with-content-update surface
    (appkit-presentation-render-list-spec-preserving-position
     (disco-room-pinned-messages--list-spec)
     :anchor-property 'disco-message-id
     :preserve-window-start t)))

(defvar-keymap disco-room-pinned-messages-mode-map
  :doc "Keymap for `disco-room-pinned-messages-mode'."
  "g" #'disco-room-pinned-messages-refresh
  "m" #'disco-room-pinned-messages-load-more
  "RET" #'appkit-ui-activate
  "<return>" #'appkit-ui-activate
  "q" #'quit-window)

(define-derived-mode disco-room-pinned-messages-mode special-mode "Disco-Pins"
  "Major mode for one channel's pinned-message browser."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun disco-room-pinned-messages--surface-init (_context input)
  "Initialize a pinned-message Surface from INPUT."
  (setq-local disco-room-pinned-messages--channel-id
              (plist-get input :channel-id)
              disco-room-pinned-messages--channel-name
              (plist-get input :channel-name)
              disco-room-pinned-messages--items nil
              disco-room-pinned-messages--next-before nil
              disco-room-pinned-messages--has-more-p nil
              disco-room-pinned-messages--loading-p nil
              disco-room-pinned-messages--error nil
              disco-room-pinned-messages--generation 0)
  (appkit-next :model nil :render t))

(defun disco-room-pinned-messages--surface-update (_context model message)
  "Handle one pinned-message Surface MESSAGE."
  (if (eq message 'render)
      (appkit-next :model model :render t)
    (appkit-next-reject 'invalid-pinned-message-command)))

(defun disco-room-pinned-messages--renderer (_surface)
  "Create the pinned-message Generated Renderer."
  (appkit-generated-renderer-create
   :mount #'ignore
   :merge (lambda (_left right) right)
   :render (lambda (surface _app-read-view _model _request)
             (disco-room-pinned-messages--render surface))
   :recover nil
   :unmount #'ignore))

(defconst disco-room-pinned-messages--surface-type
  (appkit-surface-type-create
   :name 'disco-room-pinned-messages
   :mode #'disco-room-pinned-messages-mode
   :init #'disco-room-pinned-messages--surface-init
   :update #'disco-room-pinned-messages--surface-update
   :renderer-factory #'disco-room-pinned-messages--renderer)
  "Generated Surface type for pinned-message browsers.")

(defun disco-room-list-pinned-messages (&optional channel-id)
  "Open the pinned-message browser for CHANNEL-ID or the current room."
  (interactive)
  (let* ((channel-id (or channel-id disco-room--channel-id))
         (channel (and channel-id (disco-state-channel channel-id)))
         (channel-name (or (and channel (alist-get 'name channel)) channel-id)))
    (unless (and (stringp channel-id) (not (string-empty-p channel-id)))
      (user-error "disco: room is not bound to a channel"))
    (let* ((app (disco-runtime-app))
           (identity (disco-room-pinned-messages--surface-id channel-id))
           (existing (appkit-app-surface app identity))
           (surface
            (cond
             ((appkit-surface-live-p existing)
              (pop-to-buffer (appkit-surface-buffer existing))
              existing)
             (existing
              (error "disco: pinned-message Surface is unavailable: %S"
                     (appkit-surface-status existing)))
             (t
              (appkit-open-generated-surface
               disco-room-pinned-messages--surface-type
               :app app :identity identity
               :input (list :channel-id channel-id :channel-name channel-name)
               :buffer-name
               (disco-room-pinned-messages--buffer-name channel-id channel-name)
               :select t))))
           (buffer (appkit-surface-buffer surface)))
      (with-current-buffer buffer
        (setq-local disco-room-pinned-messages--channel-name channel-name)
        (unless existing
          (disco-room-pinned-messages-refresh)))
      buffer)))

(provide 'disco-room-pin)

;;; disco-room-pin.el ends here
