;;; disco-evil.el --- Native Evil bindings for disco.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Disco's ordinary maps remain the Emacs-state interface.  This optional
;; integration defines a separate modal vocabulary: application refresh and
;; navigation use `gr' and `gj'/`gk', preserving native Evil prefixes such as
;; `gg'.  It does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
(require 'disco-customize)

(declare-function appkit-evil-normalize-keymaps "appkit-evil" ())
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
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function turn-off-evil-snipe-mode "evil-snipe" ())
(declare-function turn-off-evil-snipe-override-mode "evil-snipe" ())

(eval-when-compile
  (unless (require 'evil nil t)
    (defun evil-set-initial-state (&rest _args) nil)))

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
    disco-room-mode
    disco-root-archived-threads-mode
    disco-root-channel-inspect-mode
    disco-root-mode)
  "Major modes participating in Disco's Evil integration.")

(defconst disco-evil--readonly-maps
  '(disco-channel-directory-mode-map
    disco-msg-inspect-mode-map
    disco-root-archived-threads-mode-map
    disco-root-channel-inspect-mode-map
    disco-root-mode-map)
  "Read-only Disco keymaps with standard modal quit semantics.")

(defconst disco-evil--application-states '(normal motion)
  "Evil states used by Disco application bindings.")

(defun disco-evil--set-initial-states ()
  "Register `disco-evil-initial-state' for all Disco modes."
  (when disco-evil-initial-state
    (dolist (mode disco-evil--application-modes)
      (evil-set-initial-state mode disco-evil-initial-state))))

(defun disco-evil--define-readonly-keys ()
  "Install shared and surface-specific read-only bindings."
  (dolist (map disco-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))

  (appkit-evil-define-keys disco-evil--application-states 'disco-root-mode-map
    (kbd "RET") #'disco-root-open-at-point
    (kbd "<return>") #'disco-root-open-at-point
    (kbd "g r") #'disco-root-refresh
    (kbd "g G") #'disco-root-sync-gateway-context
    (kbd "g j") #'disco-root-button-forward
    (kbd "g k") #'disco-root-button-backward
    (kbd "g u") #'disco-root-next-unread
    (kbd "s") #'disco-root-search
    (kbd "S") #'disco-root-search-transient
    ;; Release stale root shortcuts from Evil's auxiliary maps after reloads.
    (kbd "l") nil
    (kbd "L") nil
    (kbd "\\") #'disco-root-toggle-sort-mode
    (kbd "v") #'disco-root-cycle-view-mode
    (kbd "U") #'disco-root-toggle-unread-lens
    (kbd "A") #'disco-root-list-archived-threads
    (kbd "t") #'disco-root-toggle-section-at-point
    (kbd "TAB") #'disco-root-tab-dwim
    (kbd "<backtab>") #'disco-root-button-backward
    (kbd "?") #'disco-root-transient)

  (appkit-evil-define-keys
      disco-evil--application-states 'disco-channel-directory-mode-map
    (kbd "RET") #'disco-channel-directory-open-at-point
    (kbd "<return>") #'disco-channel-directory-open-at-point
    (kbd "g r") #'disco-channel-directory-refresh
    (kbd "g j") #'disco-channel-directory-next-channel
    (kbd "g k") #'disco-channel-directory-previous-channel
    (kbd "g u") #'disco-channel-directory-next-unread
    (kbd "g b") #'disco-channel-directory-open-root
    (kbd "s") #'disco-channel-directory-set-filter
    (kbd "S") #'disco-channel-directory-clear-filter
    (kbd "U") #'disco-channel-directory-toggle-unread-only
    (kbd "t") #'disco-channel-directory-toggle-at-point
    (kbd "A") #'disco-channel-directory-open-archived-at-point
    (kbd "TAB") #'disco-channel-directory-tab-dwim
    (kbd "<backtab>") #'disco-channel-directory-previous-channel)

  (appkit-evil-define-keys
      disco-evil--application-states 'disco-root-archived-threads-mode-map
    (kbd "RET") #'disco-root-open-at-point
    (kbd "<return>") #'disco-root-open-at-point
    (kbd "g r") #'disco-root-archived-threads-refresh
    (kbd "g j") #'disco-root-button-forward
    (kbd "g k") #'disco-root-button-backward
    (kbd "m") #'disco-root-archived-threads-load-more
    (kbd "?") #'disco-root-view--transient)

  (appkit-evil-define-keys
      disco-evil--application-states 'disco-root-channel-inspect-mode-map
    (kbd "g r") #'disco-root-channel-inspect-refresh)

  (appkit-evil-define-keys
      disco-evil--application-states 'disco-msg-inspect-mode-map
    (kbd "g r") #'disco-msg-inspect-refresh))

(defun disco-evil--define-room-keys ()
  "Install room-wide and timeline-only modal bindings."
  (appkit-evil-define-keys disco-evil--application-states 'disco-room-mode-map
    (kbd "g r") #'disco-room-refresh
    (kbd "g s") #'disco-room-inplace-search
    (kbd "g n") #'disco-room-search-next
    (kbd "g p") #'disco-room-search-prev)

  ;; Timeline mode is inactive in the composer, so these keys cannot steal
  ;; draft input.  Operators retain native Evil meanings unless an explicit
  ;; message action has a familiar modal spelling such as `dd'.
  (appkit-evil-define-keys
      disco-evil--application-states 'disco-room-timeline-mode-map
    (kbd "q") #'quit-window
    (kbd "r") #'disco-msg-reply
    (kbd "d d") #'disco-msg-delete
    (kbd "R") #'disco-msg-forward
    (kbd "E") #'disco-msg-edit
    (kbd "o") #'disco-msg-operate
    (kbd "i") #'disco-msg-describe-message
    (kbd "Y") #'disco-msg-copy-dwim
    (kbd "g y") #'disco-msg-copy-link
    (kbd "g j") #'disco-msg-next
    (kbd "g k") #'disco-msg-previous
    (kbd "!") #'disco-msg-add-reaction
    (kbd "+") #'disco-msg-toggle-reaction
    (kbd "-") #'disco-msg-remove-reaction
    (kbd "T") #'disco-msg-open-thread
    (kbd "?") #'disco-room-transient)
  (add-hook 'disco-room-timeline-mode-hook
            #'appkit-evil-normalize-keymaps))

(defun disco-evil--disable-snipe ()
  "Disable Evil Snipe in a read-only Disco application buffer."
  (when (fboundp 'turn-off-evil-snipe-mode)
    (turn-off-evil-snipe-mode))
  (when (fboundp 'turn-off-evil-snipe-override-mode)
    (turn-off-evil-snipe-override-mode)))

(defun disco-evil--install-snipe-hooks ()
  "Keep read-only Disco bindings above Evil Snipe local overrides."
  (when disco-evil-enable-integration
    (dolist (mode (delq 'disco-room-mode
                        (copy-sequence disco-evil--application-modes)))
      (add-hook (intern (format "%s-hook" mode))
                #'disco-evil--disable-snipe))))

(defun disco-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing Disco application buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (memq major-mode disco-evil--application-modes)
          (when (and (featurep 'evil-snipe)
                     (not (eq major-mode 'disco-room-mode)))
            (disco-evil--disable-snipe))
          (appkit-evil-normalize-keymaps))))))

;;;###autoload
(defun disco-evil-setup ()
  "Install disco.el's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) disco-evil-enable-integration)
    (disco-evil--set-initial-states)
    (disco-evil--define-readonly-keys)
    (disco-evil--define-room-keys)
    (when (featurep 'evil-snipe)
      (disco-evil--install-snipe-hooks))
    (disco-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (disco-evil-setup))

(with-eval-after-load 'evil-snipe
  (disco-evil--install-snipe-hooks))

(provide 'disco-evil)

;;; disco-evil.el ends here
