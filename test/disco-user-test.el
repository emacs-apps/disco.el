;;; disco-user-test.el --- Tests for Discord user profiles -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'button)
(require 'disco-user)
(require 'disco-room)

(defmacro disco-user-test--with-session-state (&rest body)
  "Run BODY with isolated profile state and without stopping user transport."
  (declare (indent 0) (debug t))
  `(let ((existing-buffers (buffer-list))
         (disco-runtime--app nil)
         (disco-avatar-resources-updated-hook nil)
         (disco-state-reset-hook nil)
         (disco-state--guilds nil)
         (disco-state--channels-by-guild (make-hash-table :test #'equal))
         (disco-state--guild-channels-loaded (make-hash-table :test #'equal))
         (disco-state--channels-by-id (make-hash-table :test #'equal))
         (disco-state--gateway-channel-access-by-id (make-hash-table :test #'equal))
         (disco-state--computed-channel-access-by-id (make-hash-table :test #'equal))
         (disco-state--private-channels nil)
         (disco-state--threads-by-parent (make-hash-table :test #'equal))
         (disco-state--thread-ids-by-guild (make-hash-table :test #'equal))
         (disco-state--messages-by-channel (make-hash-table :test #'equal))
         (disco-state--message-revision-by-channel (make-hash-table :test #'equal))
         (disco-state--message-revisions-by-channel (make-hash-table :test #'equal))
         (disco-state--read-states (make-hash-table :test #'equal))
         (disco-state--user-guild-settings (make-hash-table :test #'equal))
         (disco-state--channel-notification-overrides (make-hash-table :test #'equal))
         (disco-state--ack-token-by-read-state (make-hash-table :test #'equal))
         (disco-state--thread-member-ids-by-thread (make-hash-table :test #'equal))
         (disco-state--thread-member-count-by-thread (make-hash-table :test #'equal))
         (disco-state--presences-by-user (make-hash-table :test #'equal))
         (disco-state--presences-by-guild-user (make-hash-table :test #'equal))
         (disco-state--guild-members-by-guild-user (make-hash-table :test #'equal))
         (disco-state--guild-member-ids-by-guild (make-hash-table :test #'equal))
         (disco-state--emojis-by-guild (make-hash-table :test #'equal))
         (disco-state--guild-emojis-loaded (make-hash-table :test #'equal))
         (disco-state--guild-top-emojis-by-guild (make-hash-table :test #'equal))
         (disco-state--stickers-by-guild (make-hash-table :test #'equal))
         (disco-state--guild-stickers-loaded (make-hash-table :test #'equal))
         (disco-state--standard-sticker-packs nil)
         (disco-state--standard-sticker-packs-loaded-p nil)
         (disco-state--roles-by-guild (make-hash-table :test #'equal))
         (disco-state--guild-roles-loaded (make-hash-table :test #'equal))
         (disco-state--sessions nil)
         (disco-state--voice-states-by-key (make-hash-table :test #'equal))
         (disco-state--voice-state-keys-by-channel (make-hash-table :test #'equal))
         (disco-state--channel-member-counts-by-channel (make-hash-table :test #'equal))
         (disco-state--conversation-summaries-by-channel (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
       (unwind-protect
           (progn ,@body)
         (when (appkit-app-live-p disco-runtime--app)
           (appkit-app-close disco-runtime--app))
         (dolist (buffer (buffer-list))
           (when (and (not (memq buffer existing-buffers))
                      (buffer-live-p buffer))
             (kill-buffer buffer)))))))

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
    (mutual_friends
     .
     (((id . "200") (username . "bob") (global_name . "Bob"))
      ((id . "300") (username . "carol") (global_name . "Carol"))))
    (mutual_friends_count . 2)
    (connected_accounts . (((type . "github") (name . "alice"))))))

(ert-deftest disco-user-frame-only-skips-profile-render ()
  (disco-user-test--with-session-state
    (let ((invalidations (appkit-invalidations-create)))
      (setf (appkit-invalidations-parts invalidations) '(frame))
      (cl-letf (((symbol-function 'disco-user-render)
                 (lambda ()
                   (ert-fail "frame-only sync rendered user profile"))))
        (disco-user--sync-invalidations 'unused invalidations nil)))))

(ert-deftest disco-user-render-separates-server-and-global-profile ()
  (disco-user-test--with-session-state
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
        (should (string-match-p "Mutual friends: *2" text))
        (should (string-match-p "github: alice" text))))))

(ert-deftest disco-user-render-keeps-inline-bio-fallbacks ()
  (disco-user-test--with-session-state
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
        (should (string-match-p "Inline global bio" text))))))

(ert-deftest disco-user-contexts-own-independent-profile-views ()
  (disco-user-test--with-session-state
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
              (with-current-buffer first-buffer
                (should
                 (equal '(user-profile "100" "10")
                        (appkit-surface-identity (appkit-current-surface)))))
              (with-current-buffer second-buffer
                (should
                 (equal '(user-profile "100" "20")
                        (appkit-surface-identity (appkit-current-surface)))))
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
          (appkit-app-close app))))))

(ert-deftest disco-user-open-chat-publishes-channel-and-opens-room ()
  (disco-user-test--with-session-state
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
          (appkit-app-close app))))))

(ert-deftest disco-room-message-author-opens-guild-context-profile ()
  (disco-user-test--with-session-state
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
          (should (equal "42" (get-text-property (point-min) 'disco-user-id))))))))

(ert-deftest disco-user-render-compacts-identity-and-opens-mutual-guild ()
  (disco-user-test--with-session-state
    (with-temp-buffer
      (disco-user-mode)
      (setq disco-user--user-id "175928847299117063"
            disco-user--guild-id "99"
            disco-user--profile
            (disco-user-test--profile
             "175928847299117063" "Alice" "99"))
      (let (opened)
        (cl-letf (((symbol-function 'disco-avatar-rounded-image)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'disco-state-guilds)
                   (lambda ()
                     '(((id . "99")
                        (name . "Disco Guild")
                        (member_count . 1234)))))
                  ((symbol-function 'disco-state-presence)
                   (lambda (&rest _args) '((status . "online"))))
                  ((symbol-function 'disco-channel-directory-open)
                   (lambda (guild-id) (setq opened guild-id))))
          (disco-user-render)
          (let* ((text (buffer-substring-no-properties
                        (point-min) (point-max)))
                 (lines (split-string text "\n"))
                 (button (next-button (point-min))))
            (should (string-match-p "Server Alice.*@alice" (nth 0 lines)))
            (should (string-match-p "Online" (nth 1 lines)))
            (should-not (string-match-p "^Identity$" text))
            (should
             (string-match-p
              "\\[\\[Disco Guild · Server Alice .*1\\.2k\\]\\]" text))
            (goto-char (point-min))
            (search-forward "1.2k")
            (should
             (text-property-not-all
              (line-beginning-position)
              (line-end-position)
              'display
              nil))
            (while (and button
                        (not (button-get button 'disco-guild-id)))
              (setq button (next-button (button-end button))))
            (should button)
            (should (equal "99"
                           (button-get button 'disco-guild-id)))
            (button-activate button)
            (should (equal "99" opened))))))))

(ert-deftest disco-user-mutual-friends-open-independent-profiles ()
  (disco-user-test--with-session-state
    (with-temp-buffer
      (disco-user-mode)
      (setq
       disco-user--user-id "175928847299117063"
       disco-user--guild-id "99"
       disco-user--profile (disco-user-test--profile "175928847299117063" "Alice" "99"))
      (let (opened)
        (cl-letf (((symbol-function 'disco-avatar-rounded-image)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'disco-state-guilds)
                   (lambda () '(((id . "99") (name . "Disco Guild")))))
                  ((symbol-function 'disco-state-presence) #'ignore)
                  ((symbol-function 'disco-user-open)
                   (lambda (user &optional _guild-id)
                     (setq opened user))))
          (disco-user-render)
          (let ((button (next-button (point-min))))
            (while (and button
                        (not (button-get button 'disco-user-object)))
              (setq button (next-button (button-end button))))
            (should button)
            (should (equal "Bob · @bob" (button-label button)))
            (should
             (save-excursion
               (goto-char button)
               (string-match-p
                "{Bob · @bob}"
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))))
            (should
             (equal
              "200"
              (alist-get 'id (button-get button 'disco-user-object))))
            (button-activate button)
            (should (equal "200" (alist-get 'id opened)))))))))

(provide 'disco-user-test)

;;; disco-user-test.el ends here
