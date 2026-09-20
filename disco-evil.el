;;; disco-evil.el --- Native Evil bindings for disco.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Disco's ordinary maps remain the Emacs-state interface.  This optional
;; integration follows Telega's modal message actions and keeps native motions
;; outside those deliberate application bindings.  It does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
(require 'disco)

(declare-function turn-off-evil-snipe-mode "evil-snipe" ())
(declare-function turn-off-evil-snipe-override-mode "evil-snipe" ())

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
    disco-root-channel-inspect-mode
    disco-root-list-mode)
  "Mode families participating in Disco's Evil integration.")

(defconst disco-evil--readonly-maps
  '(disco-channel-directory-mode-map
    disco-msg-inspect-mode-map
    disco-user-mode-map
    disco-room-pinned-messages-mode-map
    disco-root-channel-inspect-mode-map
    disco-root-list-mode-map)
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
    :map disco-root-list-mode-map
    :nm
    "RET" #'disco-root-open-at-point
    "<return>" #'disco-root-open-at-point
    :map disco-root-mode-map
    :nm
    "g r" #'disco-root-refresh
    "g G" #'disco-root-sync-gateway-context
    "g s" #'disco-root-search
    "g S" #'disco-root-search-transient
    "S" #'disco-root-toggle-sort-mode
    "g V" #'disco-root-cycle-view-mode
    "U" #'disco-root-toggle-unread-lens
    "g A" #'disco-root-list-archived-threads
    "g t" #'disco-root-toggle-section-at-point
    "TAB" #'disco-root-tab-dwim
    "<backtab>" #'disco-root-button-backward
    "?" #'disco-root-transient
    :map disco-channel-directory-mode-map
    :nm
    "RET" #'disco-channel-directory-open-at-point
    "<return>" #'disco-channel-directory-open-at-point
    "g r" #'disco-channel-directory-refresh
    "S" #'disco-channel-directory-set-filter
    "_" #'disco-channel-directory-clear-filter
    "g b" #'disco-channel-directory-open-root
    "U" #'disco-channel-directory-toggle-unread-only
    "g t" #'disco-channel-directory-toggle-at-point
    "g A" #'disco-channel-directory-open-archived-at-point
    "TAB" #'disco-channel-directory-tab-dwim
    "<backtab>" #'disco-channel-directory-previous-channel
    :map disco-root-archived-threads-mode-map
    :nm
    "g r" #'disco-root-archived-threads-refresh
    "g +" #'disco-root-archived-threads-load-more
    "?" #'disco-root-view--transient
    :map disco-room-pinned-messages-mode-map
    :nm
    "RET" #'appkit-ui-activate
    "<return>" #'appkit-ui-activate
    "g r" #'disco-room-pinned-messages-refresh
    "g +" #'disco-room-pinned-messages-load-more
    :map disco-root-channel-inspect-mode-map
    :nm
    "g r" #'disco-root-channel-inspect-refresh
    :map disco-msg-inspect-mode-map
    :nm
    "g r" #'disco-msg-inspect-refresh
    :map disco-user-mode-map
    :nm
    "g r" #'disco-user-refresh
    "m" #'disco-user-open-chat
    "Y" #'disco-user-copy-id
    "TAB" #'forward-button
    "<backtab>" #'disco-user-button-backward))

(defun disco-evil--define-room-keys ()
  "Install Telega-style room and message bindings outside the composer."
  ;; The timeline map leaves the composer and Emacs-state maps untouched.
  ;; Message actions deliberately replace normal-state editing commands.
  (appkit-evil-map
    :map disco-room-mode-map
    :nm
    "?" #'disco-room-transient
    "g r" #'disco-room-refresh
    "S" #'disco-room-filter-search
    "_" #'disco-room-filter-cancel
    "Z a" #'disco-room-attach
    "Z f" #'disco-room-attach-file
    :i
    "RET" #'newline
    "<return>" #'newline
    :map disco-room-timeline-mode-map
    :nm
    "q" #'quit-window
    "r" #'disco-msg-reply
    "R" #'disco-msg-forward
    "i" #'disco-msg-edit
    "D" #'disco-msg-delete
    "d d" #'disco-msg-delete
    "Z y" #'disco-msg-copy-text
    "Z l" #'disco-msg-copy-link
    "Z L" #'disco-msg-redisplay
    "P" #'disco-msg-toggle-pin
    "g r" #'disco-room-refresh
    "g ?" #'disco-msg-describe-message
    "!" #'disco-msg-add-reaction
    "a" #'disco-msg-mark-toggle
    "u" #'disco-msg-unmark
    "U" #'disco-msg-toggle-marks
    "o" #'disco-msg-operate
    :m
    "c" #'undefined
    "e" #'undefined
    "f" #'undefined
    "L" #'undefined))

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

(with-eval-after-load 'evil-snipe
  (dolist (mode (remq 'disco-room-mode disco-evil--application-modes))
    (let ((hook (intern (format "%s-hook" mode))))
      (add-hook hook #'turn-off-evil-snipe-mode)
      (add-hook hook #'turn-off-evil-snipe-override-mode))))

(with-eval-after-load 'disco-room
  (when disco-evil-enable-integration
    (appkit-evil-define-keys '(normal motion) 'disco-room-mode-map
      "g A" (lookup-key disco-room-mode-map (kbd "M-g")))))

(provide 'disco-evil)

;;; disco-evil.el ends here
