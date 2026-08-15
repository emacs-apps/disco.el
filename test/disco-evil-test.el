;;; disco-evil-test.el --- Tests for Disco Evil bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'disco)
(require 'evil)

(ert-deftest disco-evil-root-keeps-native-prefix-and-defines-actions ()
  (with-temp-buffer
    (disco-root-mode)
    (evil-normal-state)
    (should (eq (key-binding (kbd "RET")) #'disco-root-open-at-point))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "g r")) #'disco-root-refresh))
    (should (eq (key-binding (kbd "g j")) #'disco-root-button-forward))
    (should (eq (key-binding (kbd "g u")) #'disco-root-next-unread))
    (should (eq (key-binding (kbd "n")) #'evil-search-next))
    (should (eq (key-binding (kbd "s")) #'disco-root-search))
    (should (eq (key-binding (kbd "S")) #'disco-root-search-transient))
    (should (eq (key-binding (kbd "l")) #'evil-forward-char))
    (should (eq (key-binding (kbd "L")) #'evil-window-bottom))
    (evil-motion-state)
    (should (eq (key-binding (kbd "RET")) #'disco-root-open-at-point))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "s")) #'disco-root-search))
    (should (eq (key-binding (kbd "S")) #'disco-root-search-transient))))

(ert-deftest disco-evil-emacs-state-retains-ordinary-root-map ()
  (with-temp-buffer
    (disco-root-mode)
    (evil-emacs-state)
    (should (eq (key-binding (kbd "g")) #'disco-root-refresh))
    (should (eq (key-binding (kbd "n")) #'disco-root-button-forward))
    (should (eq (key-binding (kbd "s")) #'disco-root-search))
    (should (eq (key-binding (kbd "S")) #'disco-root-search-transient))
    (should (eq (key-binding (kbd "RET")) #'disco-root-open-at-point))))

(ert-deftest disco-evil-setup-releases-stale-root-shortcuts ()
  (appkit-evil-define-keys '(normal motion) 'disco-root-mode-map
    (kbd "l") #'ignore
    (kbd "L") #'ignore)
  (disco-evil-setup)
  (with-temp-buffer
    (disco-root-mode)
    (evil-normal-state)
    (should (eq (key-binding (kbd "l")) #'evil-forward-char))
    (should (eq (key-binding (kbd "L")) #'evil-window-bottom))))

(ert-deftest disco-evil-readonly-surfaces-use-modal-action-keys ()
  (dolist (case
           '((disco-channel-directory-mode-map
              "RET" disco-channel-directory-open-at-point)
             (disco-channel-directory-mode-map
              "g j" disco-channel-directory-next-channel)
             (disco-channel-directory-mode-map
              "s" disco-channel-directory-set-filter)
             (disco-root-archived-threads-mode-map
              "m" disco-root-archived-threads-load-more)
             (disco-root-channel-inspect-mode-map
              "g r" disco-root-channel-inspect-refresh)
             (disco-user-mode-map
              "g r" disco-user-refresh)
             (disco-user-mode-map
              "Y" disco-user-copy-id)
             (disco-msg-inspect-mode-map
              "g r" disco-msg-inspect-refresh)))
    (pcase-let ((`(,map-symbol ,key ,command) case))
      (with-temp-buffer
        (use-local-map (symbol-value map-symbol))
        (evil-normal-state)
        (should (eq (key-binding (kbd key)) command))
        (should (eq (key-binding (kbd "g g"))
                    #'evil-goto-first-line))))))

(ert-deftest disco-evil-room-keeps-prefixes-and-composer-boundary ()
  (with-temp-buffer
    (use-local-map disco-room-mode-map)
    (evil-normal-state)
    (should (eq (key-binding (kbd "RET")) #'evil-ret))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "g r")) #'disco-room-refresh))
    (should (eq (key-binding (kbd "g s")) #'disco-room-inplace-search))
    (disco-room-timeline-mode 1)
    (should (eq (key-binding (kbd "r")) #'disco-msg-reply))
    (should (eq (key-binding (kbd "d d")) #'disco-msg-delete))
    (should (eq (key-binding (kbd "R")) #'disco-msg-forward))
    (should (eq (key-binding (kbd "E")) #'disco-msg-edit))
    (should (eq (key-binding (kbd "Y")) #'disco-msg-copy-dwim))
    (should (eq (key-binding (kbd "g y")) #'disco-msg-copy-link))
    (should (eq (key-binding (kbd "g j")) #'disco-msg-next))
    (should (eq (key-binding (kbd "g k")) #'disco-msg-previous))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (disco-room-timeline-mode -1)
    (should-not (eq (key-binding (kbd "r")) #'disco-msg-reply))))

(ert-deftest disco-evil-room-emacs-state-retains-timeline-single-keys ()
  (with-temp-buffer
    (use-local-map disco-room-mode-map)
    (disco-room-timeline-mode 1)
    (evil-emacs-state)
    (should (eq (key-binding (kbd "c")) #'disco-msg-copy-dwim))
    (should (eq (key-binding (kbd "d")) #'disco-msg-delete))
    (should (eq (key-binding (kbd "f")) #'disco-msg-forward))
    (should-not (lookup-key disco-room-timeline-mode-map (kbd "R")))))

(provide 'disco-evil-test)

;;; disco-evil-test.el ends here
