;;; disco.el --- Discord client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: comm
;; URL: https://github.com/0WD0/disco.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (plz "0.8") (websocket "1.16") (transient "0.5.0") (appkit "0.2.0"))

;;; Commentary:

;; disco.el provides an MVP Discord client experience in Emacs:
;; - open a compact account root and lazy per-guild channel directories
;; - open a channel timeline
;; - send text messages

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-customize)
(require 'disco-runtime)
(require 'disco-state)
(require 'disco-directory)
(require 'disco-api)
(require 'disco-http)
(require 'disco-gateway)
(require 'disco-preview)
(require 'disco-markdown)
(require 'disco-media)
(require 'disco-avatar)
(require 'disco-company)
(require 'disco-room)
(require 'disco-root)
(require 'disco-modes)
(require 'disco-notifications)

(defconst disco--client-major-modes
  '(disco-channel-directory-mode
    disco-msg-inspect-mode
    disco-room-mode
    disco-root-archived-threads-mode
    disco-root-channel-inspect-mode
    disco-root-mode)
  "Major modes whose buffers contain account-scoped Disco client data.")

(defconst disco--reset-drain-limit 8
  "Maximum normal lifecycle drain passes during one destructive reset.")

(defvar disco--retired-app-identities nil
  "Appkit (KIND ID) pairs retired by the current destructive reset.")

(defun disco--retired-view-fingerprint-p (fingerprint)
  "Return non-nil when FINGERPRINT belongs to an app retired by this reset."
  (and (consp fingerprint)
       (consp (cdr fingerprint))
       (let ((identity (list (car fingerprint) (cadr fingerprint))))
         (or (equal identity '(disco default))
             (member identity disco--retired-app-identities)))))

(defun disco--owned-auxiliary-buffer-p ()
  "Return non-nil when the current buffer has explicit Disco ownership."
  (or disco-notifications--history-owner-p
      disco-root--debug-log-owner-p
      disco-api--rate-limit-buffer-owner-p
      disco-room--preview-buffer-owner-p
      disco-markdown--fontification-buffer-owner-p))

(defun disco--collect-client-buffers ()
  "Return live buffers owned by the current Disco client session.

The Appkit registry finds renamed views by ownership, while the explicit
major-mode list also finds truly legacy Disco buffers without a live Appkit
owner.  Explicit buffer-local ownership finds auxiliary projections even
after a user rename, without treating configurable name collisions as owned."
  (let ((app disco-runtime--app)
        buffers)
    (when (appkit-app-p app)
      (maphash
       (lambda (_id view)
         (when-let* ((buffer (and (appkit-view-p view)
                                  (eq app (appkit-view-app view))
                                  (appkit-view-buffer view))))
           (when (and (appkit-view-live-p view)
                      (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (eq (appkit-current-view) view))
                      (not (memq buffer buffers)))
             (push buffer buffers))))
       (appkit-app-view-registry app)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((view (appkit-current-view))
                         ;; Indirect buffers inherit the raw buffer-local view,
                         ;; while the reciprocal public accessor correctly
                         ;; rejects them because they are not the view buffer.
                         (raw-view appkit--current-view)
                         (fingerprint appkit--view-fingerprint))
                     (and
                      (apply #'derived-mode-p disco--client-major-modes)
                      ;; A live foreign Appkit view is never a legacy buffer,
                      ;; including when inherited by an indirect clone.
                      (cond
                       ((appkit-view-live-p view)
                        (eq app (appkit-view-app view)))
                       ((appkit-view-live-p raw-view)
                        (eq app (appkit-view-app raw-view)))
                       ;; Appkit keeps this identity after detachment.  Only a
                       ;; fingerprint whose app was retired by this exact reset
                       ;; is ours; an unknown detached Disco view is foreign.
                       (fingerprint
                        (disco--retired-view-fingerprint-p fingerprint))
                       ;; No live owner and no persistent identity means this
                       ;; is a genuinely legacy Disco projection.
                       (t t)))))
                 (not (memq buffer buffers)))
        (push buffer buffers)))
    (dolist (buffer (delq nil
                          (list disco-notifications--history-buffer
                                disco-root--debug-log-buffer)))
      (when (and (buffer-live-p buffer)
                 (not (memq buffer buffers)))
        (push buffer buffers)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (disco--owned-auxiliary-buffer-p))
                 (not (memq buffer buffers)))
        (push buffer buffers)))
    (nreverse buffers)))

(defun disco--retire-default-app ()
  "Stop the current default Appkit app without losing a reentrant successor."
  (when-let* ((app disco-runtime--app))
    ;; Revoke default ownership before Appkit invokes cancellation/shutdown
    ;; callbacks.  If one creates a successor, leave that successor visible to
    ;; the next drain pass instead of overwriting it after the callback.
    (cl-pushnew (list (appkit-app-kind app) (appkit-app-id app))
                disco--retired-app-identities :test #'equal)
    (setq disco-runtime--app nil)
    (appkit-stop-app app)))

(defun disco--drain-default-apps ()
  "Retire reentrant default applications until stable or bounded."
  (catch 'stable
    (dotimes (_ disco--reset-drain-limit)
      (unless disco-runtime--app
        (throw 'stable t))
      (condition-case err
          (disco--retire-default-app)
        (error
         (message "disco: Appkit retirement failed during reset: %s"
                  (error-message-string err)))
        (quit
         (message "disco: Appkit retirement was interrupted during reset"))))
    nil))

(defun disco--force-dispose-client-buffer (buffer)
  "Erase and close Disco BUFFER without running user or lifecycle hooks."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-disable-undo)
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (kill-buffer-hook nil)
            (kill-buffer-query-functions nil)
            (buffer-offer-save nil)
            (quit-flag nil))
        (widen)
        (erase-buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

(defun disco--kill-client-buffer (buffer)
  "Kill account-scoped Disco BUFFER without allowing a query to retain it.

Normal kill hooks run first so legacy buffer-owned work is cancelled.  If a
broken hook signals, force the already-selected Disco buffer closed so old
account data is not left visible."
  (when (buffer-live-p buffer)
    (let (complete-p)
      (unwind-protect
          (progn
            (condition-case error-data
                (with-current-buffer buffer
                  (let ((kill-buffer-query-functions nil)
                        (buffer-offer-save nil))
                    (set-buffer-modified-p nil)
                    (unless (kill-buffer buffer)
                      (error "Disco buffer refused normal closure"))))
              (error
               (message
                "disco: buffer cleanup failed for %s; forcing close: %s"
                (buffer-name buffer) (error-message-string error-data))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((kill-buffer-hook nil)
                         (kill-buffer-query-functions nil)
                         (buffer-offer-save nil))
                     (set-buffer-modified-p nil)
                     (unless (kill-buffer buffer)
                       (error "Disco buffer refused forced closure"))))))
              (quit
               (message
                "disco: buffer cleanup was interrupted for %s; forcing close"
                (buffer-name buffer))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((kill-buffer-hook nil)
                         (kill-buffer-query-functions nil)
                         (buffer-offer-save nil)
                         (quit-flag nil))
                     (set-buffer-modified-p nil)
                     (unless (kill-buffer buffer)
                       (error "Disco buffer refused forced closure")))))))
            (setq complete-p t))
        ;; A kill hook may throw to an active catch without signaling.  Erase
        ;; before forcing closure so even a second nonlocal transfer cannot
        ;; leave generated account text visible.
        (unless complete-p
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (buffer-disable-undo)
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t)
                    (kill-buffer-hook nil)
                    (kill-buffer-query-functions nil)
                    (buffer-offer-save nil)
                    (quit-flag nil))
                (widen)
                (erase-buffer)
                (set-buffer-modified-p nil)
                (kill-buffer buffer)))))))))

(defun disco--kill-client-buffers (buffers)
  "Kill every live account-scoped Disco buffer in BUFFERS."
  (let ((remaining buffers)
        complete-p)
    (unwind-protect
        (progn
          (while remaining
            (let ((buffer (pop remaining)))
              (condition-case error-data
                  (disco--kill-client-buffer buffer)
                (error
                 ;; Continue closing the remaining client buffers.  If Emacs
                 ;; refuses even after hooks and queries are disabled, at
                 ;; least remove the old account's generated contents.
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (buffer-disable-undo)
                     (let ((inhibit-read-only t)
                           (inhibit-modification-hooks t))
                       (widen)
                       (erase-buffer)
                       (set-buffer-modified-p nil))))
                 (message "disco: could not close %s: %s"
                          (if (buffer-live-p buffer)
                              (buffer-name buffer)
                            "Disco buffer")
                          (error-message-string error-data)))
                (quit
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (buffer-disable-undo)
                     (let ((inhibit-read-only t)
                           (inhibit-modification-hooks t)
                           (quit-flag nil))
                       (widen)
                       (erase-buffer)
                       (set-buffer-modified-p nil))))
                 (message "disco: closing %s was interrupted"
                          (if (buffer-live-p buffer)
                              (buffer-name buffer)
                            "Disco buffer"))))
              ;; A kill hook may have created a successor app.  Revoke it
              ;; before exposing later old-account buffers to their hooks.
              (disco--drain-default-apps)))
          (setq complete-p t))
      (unless complete-p
        (disco--drain-default-apps)
        (disco--kill-client-buffers remaining)))))

(defun disco--drain-client-projections (&optional initial-buffers)
  "Stop default apps and dispose INITIAL-BUFFERS plus reentrant projections.

Normal disposal runs lifecycle hooks on each pass.  If pathological hooks keep
creating account projections through every bounded pass, the final pass erases
and closes the remaining owned buffers with hooks disabled."
  (let ((pending (delete-dups (copy-sequence initial-buffers)))
        stable-p)
    (catch 'stable
      (dotimes (_ disco--reset-drain-limit)
        (disco--drain-default-apps)
        (setq pending
              (delete-dups
               (append pending (disco--collect-client-buffers))))
        (when (and (null pending) (null disco-runtime--app))
          (setq stable-p t)
          (throw 'stable t))
        (disco--kill-client-buffers pending)
        (setq pending nil)
        (disco--drain-default-apps)
        (when (and (null disco-runtime--app)
                   (null (disco--collect-client-buffers)))
          (setq stable-p t)
          (throw 'stable t))))
    (unless stable-p
      (message "disco: forcing final account projection cleanup")
      (disco--drain-default-apps)
      (disco--run-reset-actions
       (mapcar
        (lambda (buffer)
          (lambda () (disco--force-dispose-client-buffer buffer)))
        (disco--collect-client-buffers)))
      (disco--drain-default-apps))))

(defun disco--force-drain-client-projections ()
  "Erase and close every remaining owned projection with hooks disabled."
  (disco--run-reset-actions
   (mapcar
    (lambda (buffer)
      (lambda () (disco--force-dispose-client-buffer buffer)))
    (disco--collect-client-buffers))))

(defun disco--run-reset-actions (actions)
  "Run every zero-argument reset function in ACTIONS.

Errors and quits are logged and isolated.  If an action performs another
nonlocal transfer, the remaining privacy cleanup still runs while unwinding."
  (let ((remaining actions)
        complete-p)
    (unwind-protect
        (progn
          (while remaining
            (let ((action (pop remaining)))
              (condition-case err
                  (funcall action)
                (error
                 (message "disco: session cleanup failed: %s"
                          (error-message-string err)))
                (quit
                 (message "disco: session cleanup was interrupted")))))
          (setq complete-p t))
      (unless complete-p
        (disco--run-reset-actions remaining)))))

(defun disco--clear-session-memory ()
  "Clear account-scoped memory without hooks, cancellation, or rendering."
  (disco-notifications--clear-session-data)
  (disco-media--clear-session-memory)
  (disco-avatar--clear-session-memory)
  (disco-room--clear-session-cache-memory)
  (disco-root--clear-session-cache-memory)
  (disco-markdown-reset-session-state)
  (disco-preview--clear-session-data)
  (disco-directory-reset)
  (disco-state-clear-session-data)
  (disco-api--clear-rate-limit-memory)
  (disco-http--clear-queue-state)
  (disco-gateway--clear-session-data)
  (disco-root-reset-debug-log-owner)
  (setq disco-api--rate-limit-buffer nil))

;;;###autoload
(defun disco ()
  "Start disco.el and open root buffer.

If neither session token nor `DISCO_TOKEN' is available, prompt once."
  (interactive)
  (unless (disco-current-token)
    (call-interactively #'disco-set-token))
  (disco-runtime-app)
  (when disco-enable-live-updates
    (disco-gateway-start))
  (disco-root-open)
  (disco-root-refresh))

;;;###autoload
(defun disco-reset-session-state ()
  "Destructively clear the in-memory Disco account session.

Transport and Appkit ownership are stopped before account stores are reset.
Every account-scoped projection is then closed, including renamed Appkit
views, legacy Disco modes, notification history, composer preview, and root
debug output.  Cleanup failures are isolated so one broken hook or timer
cannot leave another old-account projection visible."
  (interactive)
  (let* ((disco--retired-app-identities nil)
         (buffers (disco--collect-client-buffers))
         ;; Keep callbacks unable to publish throughout all reset hooks, not
         ;; only while their own reset function is on the stack.
         (disco-notifications--reset-in-progress t)
         (disco-media--reset-in-progress t)
         (disco-avatar--reset-in-progress t)
         (disco-api--reset-in-progress t)
         (disco-http--reset-in-progress t)
         (disco-gateway--reset-in-progress t)
         (disco-room--session-cache-reset-in-progress t)
         (disco-root--session-cache-reset-in-progress t))
    ;; Client entry points must not recreate the default app while teardown is
    ;; running.  Directly constructed Appkit successors are still handled by
    ;; the bounded projection drain below.
    (cl-letf (((symbol-function 'disco-runtime-app)
               (lambda ()
                 (error "Disco default session is unavailable during reset"))))
      (disco--run-reset-actions
       (list
        #'disco--drain-default-apps
        #'disco-notifications-reset-session-state
        #'disco-media-reset-session-state
        #'disco-avatar-reset-session-state
        #'disco-room-reset-session-cache-state
        #'disco-root-reset-session-cache-state
        #'disco-markdown-reset-session-state
        #'disco-preview-reset
        #'disco-directory-reset
        #'disco-state-reset
        #'disco-api-reset-rate-limit-state
        #'disco-http-reset-queue-state
        (lambda ()
          (disco--drain-client-projections buffers)
          (setq buffers nil))
        ;; A kill hook can create a new app/view and perform an arbitrary
        ;; nonlocal exit.  This independent drain still runs while unwinding.
        #'disco--drain-client-projections
        #'disco-root-reset-debug-log-owner
        #'disco--drain-client-projections
        ;; No hookful projection disposal follows these terminal lifecycle
        ;; resets.  Any cancellation callback writes are destroyed by the
        ;; final pure memory sweep while every reset barrier remains raised.
        #'disco-media-reset-session-state
        #'disco-avatar-reset-session-state
        #'disco-room-reset-session-cache-state
        #'disco-root-reset-session-cache-state
        #'disco-api-reset-rate-limit-state
        #'disco-http-reset-queue-state
        #'disco--drain-default-apps
        #'disco-gateway-stop
        #'disco-preview-reset
        ;; A nonlocal exit or synchronous cancellation callback may create one
        ;; final owned projection.  Close a fresh snapshot without running
        ;; another generation of buffer hooks.
        #'disco--force-drain-client-projections
        #'disco--clear-session-memory))))
  (message "disco: session state reset; client buffers closed"))

;;;###autoload
(defun disco-describe-http-queue ()
  "Show current HTTP queue runtime status."
  (interactive)
  (disco-http-describe-queue))

;;;###autoload
(defun disco-describe-rate-limits ()
  "Show current in-memory Discord rate-limit windows."
  (interactive)
  (disco-api-describe-rate-limits))

;;;###autoload
(defun disco-describe-gateway ()
  "Show current gateway transport status."
  (interactive)
  (disco-gateway-describe-status))

(provide 'disco)

;;; disco.el ends here
