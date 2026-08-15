;;; disco-user-test.el --- Tests for Discord user profiles -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'button)
(require 'disco-user)
(require 'disco-room)

(defun disco-user-test--profile (user-id name guild-id)
  "Return a full profile fixture for USER-ID, NAME, and GUILD-ID."
  `((user . ((id . ,user-id)
             (username . "alice")
             (global_name . ,name)
             (discriminator . "0")
             (avatar . :null)))
    (user_profile . ((pronouns . "they/them")
                     (bio . "Global **about**")))
    (guild_member . ((nick . "Server Alice")
                     (joined_at . "2024-01-02T03:04:05+00:00")
                     (premium_since . "2025-01-02T03:04:05+00:00")
                     (user . ((id . ,user-id)
                              (username . "alice")
                              (global_name . ,name)))))
    (guild_member_profile . ((guild_id . ,guild-id)
                             (pronouns . "she/her")
                             (bio . "Server about")))
    (badges . (((id . "staff") (description . "Discord Staff"))))
    (mutual_guilds . (((id . ,guild-id) (nick . "Server Alice"))))
    (mutual_friends_count . 3)
    (connected_accounts . (((type . "github") (name . "alice"))))))

(ert-deftest disco-user-render-separates-server-and-global-profile ()
  (with-temp-buffer
    (disco-user-mode)
    (setq disco-user--user-id "175928847299117063"
          disco-user--guild-id "99"
          disco-user--seed-user
          '((id . "175928847299117063")
            (username . "alice")
            (global_name . "Alice"))
          disco-user--profile
          (disco-user-test--profile
           "175928847299117063" "Alice" "99"))
    (cl-letf (((symbol-function 'disco-avatar-rounded-image)
               (lambda (&rest _args) nil))
              ((symbol-function 'disco-state-guilds)
               (lambda () '(((id . "99") (name . "Disco Guild")))))
              ((symbol-function 'disco-state-presence)
               (lambda (&rest _args) '((status . "online")))))
      (disco-user-render))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Server profile" text))
      (should (string-match-p "Disco Guild" text))
      (should (string-match-p "Server Alice" text))
      (should (string-match-p "Server about" text))
      (should (string-match-p "User profile" text))
      (should (string-match-p "Global about" text))
      (should (string-match-p "Discord Staff" text))
      (should (string-match-p "Mutual friends: *3" text))
      (should (string-match-p "github: alice" text)))))

(ert-deftest disco-user-render-keeps-inline-bio-fallbacks ()
  (with-temp-buffer
    (disco-user-mode)
    (setq disco-user--user-id "175928847299117063"
          disco-user--guild-id "99"
          disco-user--profile
          '((user . ((id . "175928847299117063")
                     (username . "alice")
                     (bio . "Inline global bio")))
            (guild_member . ((nick . "Server Alice")
                             (bio . "Inline server bio")))))
    (cl-letf (((symbol-function 'disco-avatar-rounded-image)
               (lambda (&rest _args) nil)))
      (disco-user-render))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "Inline server bio" text))
      (should (string-match-p "Inline global bio" text)))))

(ert-deftest disco-user-contexts-own-independent-profile-views ()
  (let ((disco-runtime--app nil)
        requests
        first-buffer
        second-buffer
        app)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'disco-api-user-profile-async)
                     (lambda (user-id &rest options)
                       (push (cons (list user-id
                                         (plist-get options :guild-id))
                                   (plist-get options :on-success))
                             requests)
                       'request))
                    ((symbol-function 'disco-avatar-rounded-image)
                     (lambda (&rest _args) nil)))
            (setq first-buffer
                  (disco-user-open
                   '((id . "100") (username . "first")) "10"))
            (setq second-buffer
                  (disco-user-open
                   '((id . "100") (username . "first-elsewhere")) "20"))
            (setq app disco-runtime--app)
            (should-not (eq first-buffer second-buffer))
            (should (equal "*disco-user:100@10*" (buffer-name first-buffer)))
            (should (equal "*disco-user:100@20*" (buffer-name second-buffer)))
            (with-current-buffer first-buffer
              (should
               (equal '(user-profile "100" "10")
                      (appkit-view-id (appkit-current-view)))))
            (with-current-buffer second-buffer
              (should
               (equal '(user-profile "100" "20")
                      (appkit-view-id (appkit-current-view)))))
            (funcall (cdr (assoc '("100" "10") requests))
                     (disco-user-test--profile "100" "First" "10"))
            (with-current-buffer first-buffer
              (should-not disco-user--loading)
              (should
               (equal "100"
                      (alist-get 'id (alist-get 'user disco-user--profile)))))
            (with-current-buffer second-buffer
              (should disco-user--loading)
              (should-not disco-user--profile))
            (funcall (cdr (assoc '("100" "20") requests))
                     (disco-user-test--profile "100" "First Elsewhere" "20"))
            (with-current-buffer second-buffer
              (should-not disco-user--loading)
              (should
               (equal "100"
                      (alist-get 'id (alist-get 'user disco-user--profile)))))
            (should
             (eq first-buffer
                 (disco-user-open
                  '((id . "100") (username . "first")) "10")))
            (should (= 2 (length requests)))))
      (when (appkit-app-p app)
        (appkit-stop-app app)))))

(ert-deftest disco-user-open-chat-publishes-channel-and-opens-room ()
  (let ((disco-runtime--app nil)
        app
        opened)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'disco-api-user-profile-async)
                     (lambda (&rest _args) 'profile-request))
                    ((symbol-function 'disco-api-create-private-channel-async)
                     (lambda (_user-id &rest options)
                       (funcall (plist-get options :on-success)
                                '((id . "300")
                                  (type . 1)
                                  (recipients . (((id . "200")
                                                  (username . "second"))))))
                       'dm-request))
                    ((symbol-function 'disco-avatar-rounded-image)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'disco-room-open)
                     (lambda (channel-id channel-name)
                       (setq opened (list channel-id channel-name)))))
            (let ((buffer
                   (disco-user-open
                    '((id . "200")
                      (username . "second")
                      (global_name . "Second User")))))
              (setq app disco-runtime--app)
              (with-current-buffer buffer
                (disco-user-open-chat))
              (should (equal '("300" "Second User") opened))
              (should (equal "300"
                             (alist-get 'id (disco-state-channel "300")))))))
      (when (appkit-app-p app)
        (appkit-stop-app app)))))

(ert-deftest disco-room-message-author-opens-guild-context-profile ()
  (with-temp-buffer
    (let ((disco-room--guild-id "99")
          captured)
      (cl-letf (((symbol-function 'disco-user-open)
                 (lambda (user &optional guild-id)
                   (setq captured (list user guild-id)))))
        (disco-room--insert-message-author
         '((guild_id . "99")
           (author . ((id . "42")
                      (username . "alice")
                      (global_name . "Alice"))))
         "Alice"
         'font-lock-keyword-face)
        (let ((button (button-at (point-min))))
          (should button)
          (button-activate button))
        (should (equal "42" (alist-get 'id (car captured))))
        (should (equal "99" (cadr captured)))
        (should (equal "42" (get-text-property (point-min) 'disco-user-id)))))))

(provide 'disco-user-test)

;;; disco-user-test.el ends here
