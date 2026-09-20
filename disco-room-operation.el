;;; disco-room-operation.el --- Surface-owned message batches -*- lexical-binding: t; -*-

;;; Commentary:

;; One ordered delete or forward batch per room.  Only the Surface transition
;; advances the batch.  Each request is a finite keyed Effect; transport
;; callbacks settle gates and cannot dispatch the next request or mutate UI.

;;; Code:

(require 'appkit-command)
(require 'appkit-effect)
(require 'appkit-selection)
(require 'disco-api)
(require 'disco-state)

(defvar disco-room--channel-id)

(defun disco-room-operation-current ()
  "Return this live Surface's in-progress message operation, or nil."
  (when-let* ((surface (appkit-current-surface)))
    (plist-get (appkit-surface-model surface) :message-operation)))

(defun disco-room-operation-forwarding-p ()
  "Return non-nil while a forwarding batch owns the room send slot."
  (eq (plist-get (disco-room-operation-current) :kind) 'forward))

(defun disco-room-operation-status (model)
  "Return the progress or terminal summary belonging to MODEL."
  (if-let* ((operation (plist-get model :message-operation)))
      (format "%s: %d/%d completed"
              (plist-get operation :kind)
              (+ (plist-get operation :succeeded)
                 (length (plist-get operation :failures)))
              (plist-get operation :total))
    (when-let* ((result (plist-get model :message-operation-result)))
      (format "%s: %d succeeded, %d failed%s"
              (plist-get result :kind) (plist-get result :succeeded)
              (length (plist-get result :failures))
              (if-let* ((failures (plist-get result :failures)))
                  (concat " — " (mapconcat (lambda (failure)
                                             (format "%s: %s" (car failure) (cdr failure)))
                                           (reverse failures) "; "))
                "")))))

(defun disco-room-operation-begin (kind specs)
  "Submit preflighted KIND and ordered, owned SPECS to the current Surface."
  (when (disco-room-operation-current)
    (user-error "disco: another message batch is still in progress"))
  (unless specs (user-error "disco: no messages selected"))
  (appkit-surface-send (disco-room--ensure-surface)
                       (list 'message-operation 'begin kind (copy-tree specs))))

(defun disco-room-operation--start (_context input _observe resolve reject)
  "Start one request described by INPUT, settling only RESOLVE or REJECT."
  (pcase-let ((`(,owner ,channel ,kind ,spec) input))
    (let ((handle
           (pcase kind
             ('delete
              (disco-api-delete-message-async
               channel (plist-get spec :id) :owner owner
               :on-success resolve :on-error reject))
             ('forward
              (disco-api-forward-message-async
               channel (plist-get spec :id) (plist-get spec :source-channel)
               :content (plist-get spec :content)
               :forward-only (plist-get spec :forward-only)
               :allowed-mentions (plist-get spec :allowed-mentions)
               :owner owner
               :on-success
               (lambda (response)
                 (if (and (listp response) (alist-get 'id response))
                     (funcall resolve response)
                   (funcall reject '(:message "forward response has no message id"))))
               :on-error reject)))))
      (when (appkit-handle-p handle)
        (appkit-cancellation-create
         :kind 'logical
         :cancel (lambda () (appkit-cancel-handle handle)))))))

(defun disco-room-operation--command (operation)
  "Return the Effect command for the first pending item of OPERATION."
  (let* ((kind (plist-get operation :kind))
         (spec (car (plist-get operation :pending)))
         (revision (disco-state-message-revision disco-room--channel-id)))
    (appkit-command-start-effect
     (appkit-effect-create
      :key 'message-operation
      :input (list (appkit-current-surface) disco-room--channel-id kind spec)
      :start #'disco-room-operation--start
      :success (lambda (_input response)
                 (list 'message-operation 'settled spec revision t response))
      :failure (lambda (_input reason)
                 (list 'message-operation 'settled spec revision nil reason))))))

(defun disco-room-operation-update (model message)
  "Return the room transition for a batch MESSAGE against MODEL."
  (pcase message
    (`(message-operation begin ,kind ,specs)
     (if (or (plist-get model :message-operation)
             (not (memq kind '(forward delete))) (null specs))
         (appkit-next-reject 'invalid-message-operation)
       (let ((next (copy-sequence model))
             (operation (list :kind kind :pending specs :total (length specs)
                              :succeeded 0 :failures nil)))
         (setf (plist-get next :message-operation) operation
               (plist-get next :message-operation-result) nil)
         (appkit-next
          :model next
          :render (disco-room--render-request-create
                   :change (appkit-projection-change-create :frame-p t))
          :commands (list (disco-room-operation--command operation))))))
    (`(message-operation settled ,spec ,revision ,success ,payload)
     (let ((current (plist-get model :message-operation)))
       (if (not (eq spec (car (plist-get current :pending))))
           (appkit-next-reject 'stale-message-operation)
         (let* ((next (copy-sequence model))
                (operation (copy-sequence current))
                (id (plist-get spec :id))
                (selection (plist-get model :selection)))
           (setf (plist-get operation :pending) (cdr (plist-get operation :pending)))
           (if success
               (progn
                 (cl-incf (plist-get operation :succeeded))
                 (setq selection (appkit-selection-forget selection (list id)))
                 (pcase (plist-get operation :kind)
                   ('forward
                    (disco-state-merge-message-response
                     disco-room--channel-id payload revision))
                   ('delete
                    (disco-state-delete-message disco-room--channel-id id)
                    (disco-room--repair-history-window-after-delete id)
                    (disco-room--forget-message-async-state id)
                    (disco-room--retire-deleted-composer-context id)
                    (disco-room--apply-filtered-message-delete id))))
             (push (cons id (disco-room--async-error-message payload))
                   (plist-get operation :failures))
             (when (disco-room--resolve-message id)
               (setq selection (appkit-selection-set selection id t))))
           (setf (plist-get next :selection) selection
                 (plist-get next :message-operation)
                 (and (plist-get operation :pending) operation)
                 (plist-get next :message-operation-result)
                 (unless (plist-get operation :pending) operation))
           (appkit-next
            :model next
            :render (disco-room--render-request-create
                     :change (appkit-projection-change-create :full-p t :frame-p t))
            :commands (when (plist-get operation :pending)
                        (list (disco-room-operation--command operation))))))))
    (_ (appkit-next-reject 'invalid-message-operation))))

(provide 'disco-room-operation)
;;; disco-room-operation.el ends here
