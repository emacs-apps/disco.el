;;; disco-directory.el --- Lazy Discord guild directory owner -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns REST request lifecycles for the guild/DM index and per-guild channel
;; snapshots.  Views consume state and directory events; they never issue these
;; requests directly.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'disco-api)
(require 'disco-customize)
(require 'disco-state)

(defvar disco-directory-event-hook nil
  "Hook run with one directory lifecycle event plist.")

(defvar disco-directory--index-generation 0)
(defvar disco-directory--guild-generation (make-hash-table :test #'equal))
(defvar disco-directory--guild-status (make-hash-table :test #'equal))

(defun disco-directory-reset ()
  "Reset directory request ownership state."
  (cl-incf disco-directory--index-generation)
  (clrhash disco-directory--guild-generation)
  (clrhash disco-directory--guild-status))

(defun disco-directory--emit (type &rest properties)
  "Emit directory event TYPE with PROPERTIES."
  (run-hook-with-args 'disco-directory-event-hook
                      (append (list :type type) properties)))

(defun disco-directory-guild-status (guild-id)
  "Return request status for GUILD-ID.

The result is `loading', `error', `loaded', or `unloaded'."
  (let ((request-status
         (gethash (format "%s" guild-id) disco-directory--guild-status)))
    (cond
     ((eq request-status 'loading) 'loading)
     ((disco-state-guild-channels-loaded-p guild-id) 'loaded)
     (request-status request-status)
     (t 'unloaded))))

(defun disco-directory--guild-known-p (guild-id)
  "Return non-nil when GUILD-ID remains in the guild index."
  (seq-some (lambda (guild)
              (equal (format "%s" (alist-get 'id guild))
                     (format "%s" guild-id)))
            (disco-state-guilds)))

(defun disco-directory--prune-guild-requests ()
  "Remove request metadata for guilds absent from the current index."
  (let (departed)
    (maphash
     (lambda (guild-id _status)
       (unless (disco-directory--guild-known-p guild-id)
         (push guild-id departed)))
     disco-directory--guild-status)
    (dolist (guild-id departed)
      (remhash guild-id disco-directory--guild-status)
      (remhash guild-id disco-directory--guild-generation))))

(cl-defun disco-directory-refresh-index-async (&key on-complete)
  "Refresh guild and private-channel indexes asynchronously.

Call ON-COMPLETE with a list of endpoint errors after both requests settle."
  (let ((generation (cl-incf disco-directory--index-generation))
        (pending 2)
        errors)
    (disco-directory--emit 'index-loading :generation generation)
    (cl-labels
        ((active-p () (= generation disco-directory--index-generation))
         (finish-one ()
           (cl-decf pending)
           (when (and (zerop pending) (active-p))
             (setq errors (nreverse errors))
             (disco-directory--emit
              'index-loaded :generation generation :errors errors)
             (when on-complete
               (funcall on-complete errors))))
         (fail (endpoint error-value)
           (when (active-p)
             (push (cons endpoint error-value) errors))
           (finish-one)))
      (disco-api-user-guilds-async
       :on-success
       (lambda (guilds)
         (when (active-p)
           (disco-state-set-guilds guilds)
           (disco-directory--prune-guild-requests))
         (finish-one))
       :on-error (lambda (error-value) (fail 'guilds error-value)))
      (disco-api-user-private-channels-async
       :on-success
       (lambda (channels)
         (when (active-p)
           (disco-state-set-private-channels channels))
         (finish-one))
       :on-error (lambda (error-value) (fail 'private-channels error-value)))
      generation)))

(defun disco-directory--load-active-threads-async (guild-id generation)
  "Load active threads for GUILD-ID under request GENERATION."
  (when disco-fetch-guild-active-threads
    (disco-api-guild-active-threads-async
     guild-id
     :on-success
     (lambda (active)
       (when (and (= generation
                     (gethash guild-id disco-directory--guild-generation 0))
                  (disco-directory--guild-known-p guild-id))
         (dolist (thread (or (alist-get 'threads active) '()))
           (disco-state-upsert-channel thread))
         (disco-directory--emit 'guild-enriched :guild-id guild-id)))
     :on-error
     (lambda (error-value)
       (when (= generation
                (gethash guild-id disco-directory--guild-generation 0))
         (disco-directory--emit
          'guild-enrichment-error
          :guild-id guild-id :error error-value))))))

(cl-defun disco-directory-load-guild-async (guild-id &key force)
  "Ensure GUILD-ID has a complete channel snapshot.

When FORCE is non-nil, refresh an already loaded snapshot.  Concurrent loads
for the same guild are coalesced."
  (setq guild-id (format "%s" guild-id))
  (let ((status (disco-directory-guild-status guild-id)))
    (cond
     ((eq status 'loading)
      'loading)
     ((and (eq status 'loaded) (not force))
      'loaded)
     ((not (disco-directory--guild-known-p guild-id))
      (error "disco: cannot load unknown guild %s" guild-id))
     (t
      (let ((generation
             (1+ (gethash guild-id disco-directory--guild-generation 0))))
        (puthash guild-id generation disco-directory--guild-generation)
        (puthash guild-id 'loading disco-directory--guild-status)
        (disco-directory--emit
         'guild-loading :guild-id guild-id :generation generation)
        (disco-api-guild-channels-async
         guild-id
         :on-success
         (lambda (channels)
           (when (and (= generation
                         (gethash guild-id disco-directory--guild-generation 0))
                      (disco-directory--guild-known-p guild-id))
             (disco-state-put-channels guild-id channels)
             (puthash guild-id 'loaded disco-directory--guild-status)
             (disco-directory--emit
              'guild-loaded :guild-id guild-id :generation generation)
             (disco-directory--load-active-threads-async guild-id generation)))
         :on-error
         (lambda (error-value)
           (when (= generation
                    (gethash guild-id disco-directory--guild-generation 0))
             (puthash guild-id 'error disco-directory--guild-status)
             (disco-directory--emit
              'guild-error :guild-id guild-id :generation generation
              :error error-value))))
        'loading)))))

(cl-defun disco-directory-refresh-all-async ()
  "Refresh the index, then explicitly hydrate every known guild."
  (disco-directory-refresh-index-async
   :on-complete
   (lambda (errors)
     (unless (assq 'guilds errors)
       (dolist (guild (disco-state-guilds))
         (when-let* ((guild-id (alist-get 'id guild)))
           (disco-directory-load-guild-async guild-id :force t)))))))

(provide 'disco-directory)

;;; disco-directory.el ends here
