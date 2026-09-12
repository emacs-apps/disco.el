;;; disco-evil.el --- Native Evil bindings for disco.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Disco's ordinary maps remain the Emacs-state interface.  This optional
;; integration keeps native Evil motions and defines only deliberate
;; application actions.  It does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
(require 'disco-customize)

(declare-function disco-channel-directory-clear-filter
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-next-channel
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-next-unread
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-open-archived-at-point
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-open-at-point
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-open-root
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-previous-channel
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-refresh
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-set-filter
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-tab-dwim
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-toggle-at-point
                  "disco-channel-directory" ())
(declare-function disco-channel-directory-toggle-unread-only
                  "disco-channel-directory" ())
(declare-function disco-msg-add-reaction "disco-msg" ())
(declare-function disco-msg-copy-dwim "disco-msg" ())
(declare-function disco-msg-copy-link "disco-msg" ())
(declare-function disco-msg-delete "disco-msg" ())
(declare-function disco-msg-describe-message "disco-msg" ())
(declare-function disco-msg-edit "disco-msg" ())
(declare-function disco-msg-forward "disco-msg" ())
(declare-function disco-msg-inspect-refresh "disco-msg" ())
(declare-function disco-msg-next "disco-msg" (&optional n))
(declare-function disco-msg-open-thread "disco-msg" ())
(declare-function disco-msg-operate "disco-msg" ())
(declare-function disco-msg-previous "disco-msg" (&optional n))
(declare-function disco-msg-remove-reaction "disco-msg" ())
(declare-function disco-msg-reply "disco-msg" ())
(declare-function disco-msg-toggle-reaction "disco-msg" ())
(declare-function disco-user-button-backward "disco-user" ())
(declare-function disco-user-copy-id "disco-user" ())
(declare-function disco-user-open-chat "disco-user" ())
(declare-function disco-user-refresh "disco-user" ())
(declare-function disco-room-inplace-search "disco-room-search" ())
(declare-function disco-room-refresh "disco-room" ())
(declare-function disco-room-search-next "disco-room-search" (&optional n))
(declare-function disco-room-search-prev "disco-room-search" (&optional n))
(declare-function disco-room-transient "disco-room" ())
(declare-function disco-root-archived-threads-load-more "disco-root-view" ())
(declare-function disco-root-archived-threads-refresh "disco-root-view" ())
(declare-function disco-root-button-backward "disco-root" ())
(declare-function disco-root-button-forward "disco-root" ())
(declare-function disco-root-channel-inspect-refresh "disco-root-view" ())
(declare-function disco-root-cycle-view-mode "disco-root" ())
(declare-function disco-root-list-archived-threads "disco-root" ())
(declare-function disco-root-next-unread "disco-root" ())
(declare-function disco-root-open-at-point "disco-root" ())
(declare-function disco-root-refresh "disco-root" (&optional full))
(declare-function disco-root-sync-gateway-context "disco-root" (&optional quiet))
(declare-function disco-root-tab-dwim "disco-root" ())
(declare-function disco-root-toggle-section-at-point "disco-root" ())
(declare-function disco-root-toggle-sort-mode "disco-root" ())
(declare-function disco-root-toggle-unread-lens "disco-root" ())
(declare-function disco-root-transient "disco-root" ())
(declare-function disco-root-search "disco-root" (query domain))
(declare-function disco-root-search-transient "disco-root" ())
(declare-function disco-root-view--transient "disco-root-view" ())

(defgroup disco-evil nil
  "Optional native Evil integration for disco.el."
  :group 'disco
  :prefix "disco-evil-")

(defcustom disco-evil-enable-integration t
  "If non-nil, install disco.el's Evil bindings automatically."
  :type 'boolean
  :group 'disco-evil)

(defcustom disco-evil-initial-state 'normal
  "Initial Evil state used for Disco application buffers.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'disco-evil)

(defconst disco-evil--application-modes
  '(disco-channel-directory-mode
    disco-msg-inspect-mode
    disco-user-mode
    disco-room-mode
    disco-room-pinned-messages-mode
    disco-root-archived-threads-mode
    disco-root-channel-inspect-mode
    disco-root-mode)
  "Major modes participating in Disco's Evil integration.")

(defconst disco-evil--readonly-maps
  '(disco-channel-directory-mode-map
    disco-msg-inspect-mode-map
    disco-user-mode-map
    disco-room-pinned-messages-mode-map
    disco-root-archived-threads-mode-map
    disco-root-channel-inspect-mode-map
    disco-root-mode-map)
  "Read-only Disco keymaps with standard modal quit semantics.")

(defun disco-evil--set-initial-states ()
  "Register `disco-evil-initial-state' for all Disco modes."
  (appkit-evil-set-initial-states
   disco-evil--application-modes disco-evil-initial-state))

(defun disco-evil--define-readonly-keys ()
  "Install shared and surface-specific read-only bindings."
  (dolist (map disco-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))

  (appkit-evil-map
    (:map disco-root-mode-map
     :nm
     "RET" #'disco-root-open-at-point
     "<return>" #'disco-root-open-at-point
     "g r" #'disco-root-refresh
     "g G" #'disco-root-sync-gateway-context
     "g s" #'disco-root-search
     "g S" #'disco-root-search-transient
     "g \\" #'disco-root-toggle-sort-mode
     "g v" #'disco-root-cycle-view-mode
     "U" #'disco-root-toggle-unread-lens
     "g A" #'disco-root-list-archived-threads
     "g t" #'disco-root-toggle-section-at-point
     "TAB" #'disco-root-tab-dwim
     "<backtab>" #'disco-root-button-backward
     "?" #'disco-root-transient)
    (:map disco-channel-directory-mode-map
     :nm
     "RET" #'disco-channel-directory-open-at-point
     "<return>" #'disco-channel-directory-open-at-point
     "g r" #'disco-channel-directory-refresh
     "g s" #'disco-channel-directory-set-filter
     "g S" #'disco-channel-directory-clear-filter
     "g b" #'disco-channel-directory-open-root
     "U" #'disco-channel-directory-toggle-unread-only
     "g t" #'disco-channel-directory-toggle-at-point
     "g A" #'disco-channel-directory-open-archived-at-point
     "TAB" #'disco-channel-directory-tab-dwim
     "<backtab>" #'disco-channel-directory-previous-channel)
    (:map disco-root-archived-threads-mode-map
     :nm
     "RET" #'disco-root-open-at-point
     "<return>" #'disco-root-open-at-point
     "g r" #'disco-root-archived-threads-refresh
     "g +" #'disco-root-archived-threads-load-more
     "?" #'disco-root-view--transient)
    (:map disco-room-pinned-messages-mode-map
     :nm
     "RET" #'appkit-ui-activate
     "<return>" #'appkit-ui-activate
     "g r" #'disco-room-pinned-messages-refresh
     "g +" #'disco-room-pinned-messages-load-more)
    (:map disco-root-channel-inspect-mode-map
     :nm
     "g r" #'disco-root-channel-inspect-refresh)
    (:map disco-msg-inspect-mode-map
     :nm
     "g r" #'disco-msg-inspect-refresh)
    (:map disco-user-mode-map
     :nm
     "g r" #'disco-user-refresh
     "g m" #'disco-user-open-chat
     "Y" #'disco-user-copy-id
     "TAB" #'forward-button
     "<backtab>" #'disco-user-button-backward)))

(defun disco-evil--define-room-keys ()
  "Install room-wide and timeline-only modal bindings."
  ;; Timeline mode is inactive in the composer.  Lowercase Evil operators and
  ;; motions remain native except for `i', which enters that composer.
  (appkit-evil-map
    (:map disco-room-mode-map
     :nm
     "g r" #'disco-room-refresh
     "g s" #'disco-room-inplace-search
     "g n" #'disco-room-search-next
     "g p" #'disco-room-search-prev)
    (:map disco-room-timeline-mode-map
     :nm
     "q" #'quit-window
     "R" #'disco-msg-reply
     "g F" #'disco-msg-forward
     "i" #'appkit-evil-chatbuf-enter-input
     "E" #'disco-msg-edit
     "Y" #'disco-msg-copy-dwim
     "g y" #'disco-msg-copy-link
     "!" #'disco-msg-add-reaction
     "+" #'disco-msg-toggle-reaction
     "-" #'disco-msg-remove-reaction
     "T" #'disco-msg-open-thread
     "?" #'disco-room-transient
     :n
     "D" #'disco-msg-delete
     :m
     "c" #'undefined
     "d" #'undefined
     "e" #'undefined
     "f" #'undefined
     "o" #'undefined
     "r" #'undefined
     "t" #'undefined
     "P" #'undefined
     "L" #'undefined)))

(defun disco-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing Disco application buffers."
  (appkit-evil-normalize-buffers disco-evil--application-modes))

;;;###autoload
(defun disco-evil-setup ()
  "Install disco.el's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) disco-evil-enable-integration)
    (disco-evil--set-initial-states)
    (disco-evil--define-readonly-keys)
    (disco-evil--define-room-keys)
    (disco-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (disco-evil-setup))

(provide 'disco-evil)

;;; disco-evil.el ends here
