;;; disco-room-test-support.el --- Shared room test fixtures -*- lexical-binding: t; -*-

(require 'disco-room)
(require 'disco-state)

(defmacro disco-room-test-with-runtime (&rest body)
  "Run BODY with an isolated App and explicitly retire its Surfaces."
  (declare (indent 0) (debug (body)))
  `(let ((disco-runtime--app nil))
     (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
       (unwind-protect
           (progn ,@body)
         (disco-runtime-stop)))))

(defmacro disco-room-test-with-surface (channel-id &rest body)
  "Run BODY in an isolated Generated room Surface for CHANNEL-ID."
  (declare (indent 1) (debug (form body)))
  `(let ((disco-runtime--app nil)
         surface)
     (unwind-protect
         (progn
           (disco-state-reset)
           (disco-state-upsert-channel
            (list (cons 'id ,channel-id) '(type . 0) '(permissions . "2048")))
           (setq surface
                 (appkit-open-generated-surface
                  disco-room--surface-type
                  :app (disco-runtime-app)
                  :identity (list 'room ,channel-id)
                  :input (list :channel-id ,channel-id
                               :channel-name ,channel-id)))
           (with-current-buffer (appkit-surface-buffer surface)
             ,@body))
       (disco-runtime-stop)
       (when (and surface (buffer-live-p (appkit-surface-buffer surface)))
         (kill-buffer (appkit-surface-buffer surface))))))

(defun disco-room-test-drain (surface)
  "Run queued passes for SURFACE until its loop becomes idle."
  (let ((loop (appkit-surface-loop surface))
        (passes 0))
    (while (and (eq (appkit-loop-status loop) 'running)
                (> (appkit-loop-pending-count loop) 0))
      (when (> (cl-incf passes) 20)
        (ert-fail "Room Surface did not become idle"))
      (appkit-loop-run-pass loop))))

(defun disco-room-test-establish-latest-window (&optional channel-id)
  "Establish a known latest window for CHANNEL-ID's canonical fixture rows."
  (let* ((channel-id (or channel-id disco-room--channel-id))
         (messages
          (disco-room--normalize-history-page
           (disco-state-messages channel-id)))
         (newest (disco-room--message-id (car messages)))
         (oldest (disco-room--message-id (car (last messages)))))
    (setq disco-room--remote-latest-message-id newest)
    (if newest
        (appkit-chat-history-window-set oldest nil)
      (appkit-chat-history-window-establish-empty))))

(defun disco-room-test-setup-channel (&optional channel-id)
  "Reset state and bind the current room buffer to CHANNEL-ID."
  (let ((channel-id (or channel-id "chan")))
    (disco-state-reset)
    (setq-local disco-room--channel-id channel-id)
    (setq-local disco-room--channel-name channel-id)
    (disco-state-upsert-channel
     `((id . ,channel-id)
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    channel-id))

(provide 'disco-room-test-support)

;;; disco-room-test-support.el ends here
