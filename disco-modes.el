;;; disco-modes.el --- Global presentation modes for disco.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Telega-style global mode-line status backed by Discord read states.

;;; Code:

(require 'cl-lib)
(require 'disco-customize)
(require 'disco-directory)
(require 'disco-gateway)
(require 'appkit-mode-line)
(require 'disco-root)
(require 'disco-state)

(defvar disco-client-mode-line-string ""
  "Cached Discord mode-line string.")

(defvar disco-client-mode-line-mode nil
  "Non-nil when Discord mode-line status is enabled.")

(defvar disco-client-mode-line--cached-counts '(0 . 0)
  "Cached (KNOWN-UNREAD-MESSAGES . MENTIONS) modeline counts.")

(defconst disco-client-mode-line--count-event-types
  '(ready guild-sync guild-create guild-delete user-guild-settings-update
    channel-create channel-update channel-delete channel-update-partial
    channel-unread-update passive-update-v1 passive-update-v2
    channel-pins-update channel-pins-ack last-messages
    message-create message-delete message-ack
    thread-create thread-update thread-delete thread-list-sync)
  "Gateway event types which can change Discord mode-line counts.")

(defconst disco-client-mode-line--directory-count-event-types
  '(index-loaded guild-loaded guild-enriched
    parent-threads-page parent-threads-loaded)
  "Directory events which replace or extend the indexed channel set.")

(defcustom disco-client-mode-line-format
  '(disco-client-mode-line-mode ("" disco-client-mode-line-string))
  "Mode-line provider installed in `mode-line-misc-info'."
  :type 'sexp
  :group 'disco-modes
  :risky t)

(defun disco-client-mode-line--counts ()
  "Return (KNOWN-UNREAD-MESSAGES . MENTIONS) for current Discord state.

KNOWN-UNREAD-MESSAGES is a local-cache lower bound and excludes muted
channels.  MENTIONS is the exact sum of read-state `mention_count' values and
therefore remains visible for muted channels."
  (let ((unread-messages 0)
        (mentions 0))
    (dolist (channel (disco-state-channels))
      (let ((mention-count
             (disco-state-channel-unread-mention-count (alist-get 'id channel))))
        (cl-incf mentions mention-count)
        (unless (disco-state-channel-muted-p channel)
          (cl-incf unread-messages
                   (disco-state-channel-known-unread-message-count channel)))))
    (cons unread-messages mentions)))

(defun disco-client-mode-line-open-root ()
  "Open the disco root buffer."
  (interactive)
  (disco-root-open))

(defun disco-client-mode-line-open-unread ()
  "Open disco root and move to an unread channel."
  (interactive)
  (disco-root-open)
  (disco-root-next-unread))

(defun disco-client-mode-line-open-mentions ()
  "Open disco root and move to a channel with unread mentions."
  (interactive)
  (disco-root-open)
  (let* ((positions
          (disco-root--channel-line-positions
           (lambda (pos) (> (disco-root--line-unread-count pos) 0))))
         (origin (line-beginning-position))
         (found (or (disco-root--next-position-after positions origin)
                    (car positions))))
    (if (integerp found)
        (goto-char found)
      (message "disco: no unread mentions"))))

(defun disco-client-mode-line-icon ()
  "Return clickable Discord label for the mode line."
  (appkit-mode-line-indicator
   "Discord" :face 'mode-line-emphasis
   :command #'disco-client-mode-line-open-root :help-echo "Open disco"))

(defun disco-client-mode-line-unread ()
  "Return indicator for locally known unmuted unread Discord messages."
  (let ((count (car disco-client-mode-line--cached-counts)))
    (unless (zerop count)
      (appkit-mode-line-indicator
       (number-to-string count) :prefix " " :face 'disco-mode-line-unread
       :command #'disco-client-mode-line-open-unread
       :help-echo "Open unread Discord channels (count is locally known messages)"))))

(defun disco-client-mode-line-mentions ()
  "Return indicator for unread Discord mentions."
  (let ((count (cdr disco-client-mode-line--cached-counts)))
    (unless (zerop count)
      (appkit-mode-line-indicator
       (format "@%d" count) :prefix " " :face 'disco-mode-line-mention
       :command #'disco-client-mode-line-open-mentions
       :help-echo "Open Discord channels with unread mentions"))))

(defun disco-client-mode-line-update (&optional event)
  "Refresh cached Discord mode-line state after optional gateway EVENT."
  (when (and disco-client-mode-line-mode
             (or (null event)
                 (memq (plist-get event :type)
                       disco-client-mode-line--count-event-types)))
    (setq disco-client-mode-line--cached-counts
          (disco-client-mode-line--counts))
    (appkit-mode-line-update-cache
     'disco-client-mode-line-string disco-mode-line-string-format)))

(defun disco-client-mode-line--handle-directory-event (event)
  "Rebuild mode-line counts after channel-mutating directory EVENT."
  (when (memq (plist-get event :type)
              disco-client-mode-line--directory-count-event-types)
    (disco-client-mode-line-update)))

(defun disco-client-mode-line--handle-state-reset ()
  "Rebuild mode-line counts after the canonical state is reset."
  (disco-client-mode-line-update))

;;;###autoload
(define-minor-mode disco-client-mode-line-mode
  "Toggle Discord unread and mention status in the mode line."
  :init-value nil
  :global t
  :group 'disco-modes
  (if disco-client-mode-line-mode
      (progn
        (appkit-mode-line-install 'disco-client-mode-line-format)
        (add-hook 'disco-gateway-event-hook #'disco-client-mode-line-update)
        (add-hook 'disco-directory-event-hook
                  #'disco-client-mode-line--handle-directory-event)
        (add-hook 'disco-state-reset-hook
                  #'disco-client-mode-line--handle-state-reset)
        (disco-client-mode-line-update))
    (appkit-mode-line-uninstall 'disco-client-mode-line-format)
    (setq disco-client-mode-line-string ""
          disco-client-mode-line--cached-counts '(0 . 0))
    (remove-hook 'disco-gateway-event-hook #'disco-client-mode-line-update)
    (remove-hook 'disco-directory-event-hook
                 #'disco-client-mode-line--handle-directory-event)
    (remove-hook 'disco-state-reset-hook
                 #'disco-client-mode-line--handle-state-reset)
    (force-mode-line-update t)))

(provide 'disco-modes)

;;; disco-modes.el ends here
