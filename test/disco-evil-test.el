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
    (should (eq (key-binding (kbd "g s")) #'disco-root-search))
    (should (eq (key-binding (kbd "g S"))
                #'disco-root-search-transient))
    (should (eq (key-binding (kbd "g j")) #'evil-next-visual-line))
    (should (eq (key-binding (kbd "g u")) #'evil-downcase))
    (should (eq (key-binding (kbd "n")) #'evil-search-next))
    (should (eq (key-binding (kbd "s")) #'disco-root-search))
    (should (eq (key-binding (kbd "S")) #'disco-root-search-transient))
    (should (eq (key-binding (kbd "l")) #'evil-forward-char))
    (should (eq (key-binding (kbd "L")) #'evil-window-bottom))
    (evil-motion-state)
    (should (eq (key-binding (kbd "RET")) #'disco-root-open-at-point))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "s")) #'disco-root-search))
    (should (eq (key-binding (kbd "g s")) #'disco-root-search))
    (should (eq (key-binding (kbd "g S"))
                #'disco-root-search-transient))
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


(ert-deftest disco-evil-readonly-surfaces-use-modal-action-keys ()
  (dolist (case
           '((disco-channel-directory-mode-map
              "RET" disco-channel-directory-open-at-point)
             (disco-channel-directory-mode-map
              "s" disco-channel-directory-set-filter)
             (disco-channel-directory-mode-map
              "g s" disco-channel-directory-set-filter)
             (disco-channel-directory-mode-map
              "g S" disco-channel-directory-clear-filter)
             (disco-root-archived-threads-mode-map
              "m" disco-root-archived-threads-load-more)
             (disco-room-pinned-messages-mode-map
              "RET" appkit-ui-activate)
             (disco-room-pinned-messages-mode-map
              "g r" disco-room-pinned-messages-refresh)
             (disco-room-pinned-messages-mode-map
              "m" disco-room-pinned-messages-load-more)
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
    (appkit-chatbuf-use-timeline-mode #'disco-room-timeline-mode)
    (should (eq (key-binding (kbd "r")) #'disco-msg-reply))
    (should (eq (key-binding (kbd "R")) #'disco-msg-forward))
    (should (eq (key-binding (kbd "i")) #'disco-msg-edit))
    (should (eq (key-binding (kbd "Y")) #'disco-msg-copy-dwim))
    (should (eq (key-binding (kbd "D")) #'disco-msg-delete))
    (should (eq (key-binding (kbd "d d")) #'disco-msg-delete))
    (should-not (eq (key-binding (kbd "d")) #'evil-delete))
    (dolist (binding
             '(("e" . evil-forward-word-end)
               ("l" . evil-forward-char)
               ("n" . evil-search-next)
               ("p" . evil-paste-after)
               ("E" . evil-forward-WORD-end)
               ("o" . evil-open-below)
               ("g j" . evil-next-visual-line)
               ("g k" . evil-previous-visual-line)
               ("g u" . evil-downcase)
               ("g i" . evil-insert-resume)))
      (should (eq (key-binding (kbd (car binding))) (cdr binding))))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (evil-motion-state)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "E")) #'evil-forward-WORD-end))
    (should (eq (key-binding (kbd "o")) #'undefined))
    (evil-normal-state)
    (appkit-chatbuf-use-timeline-mode nil)
    (should (eq (key-binding (kbd "d")) #'evil-delete))
    (should-not (eq (key-binding (kbd "d d")) #'disco-msg-delete))
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
