;;; disco-room-render-test.el --- Rendering tests for disco-room -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for room message formatting, media resources, and timeline
;; presentation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'disco-room)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest disco-room-author-face-uses-stable-discord-identity ()
  (disco-room-test-with-runtime
   (let ((original
          '((author . ((id . "42")
                       (global_name . "Original Name")))))
         (renamed
          '((author . ((id . "42")
                       (global_name . "Renamed User"))))))
     (should
      (eq (appkit-name-color-face "42")
          (disco-room--author-face original)))
     (should
      (eq (disco-room--author-face original)
          (disco-room--author-face renamed))))))

(ert-deftest disco-room-video-action-cannot-rebind-to-replacement-surface ()
  (disco-room-test-with-runtime
   (let ((disco-room-use-rich-attachment-cards t)
         (disco-media-show-previews nil)
         started)
     (cl-letf (((symbol-function 'appkit-media-video-presentation-start)
                (lambda (&rest _) (setq started t))))
       (disco-room-test-with-surface "video-owner"
                                     (disco-state-put-messages
                                      "video-owner"
                                      '(((id . "m-video")
                                         (channel_id . "video-owner")
                                         (content . "")
                                         (attachments . (((id . "a-video")
                                                          (filename . "clip.mp4")
                                                          (content_type . "video/mp4")
                                                          (url . "https://example.invalid/clip.mp4")))))))
                                     (disco-room-test-establish-latest-window)
                                     (disco-room-render)
                                     (goto-char (point-min))
                                     (search-forward "clip.mp4")
                                     (let ((old-surface (appkit-current-surface))
                                           (action (plist-get
                                                    (get-text-property (match-beginning 0)
                                                                       appkit-media-card-context-property)
                                                    :open-action)))
                                       (should (functionp action))
                                       (appkit-surface-stop old-surface)
                                       (let ((replacement (disco-room--ensure-surface)))
                                         (should (appkit-surface-live-p replacement))
                                         (should-not (eq replacement old-surface))
                                         (should-error (funcall action))
                                         (should-not started))))))))

(ert-deftest disco-room-thread-announcement-opens-referenced-channel ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "parent"
                                 (let ((message '((id . "notice") (type . 18)
                                                  (author . ((username . "Alice")))
                                                  (content . "Discussion")
                                                  (message_reference . ((channel_id . "thread")))))
                                       opened)
                                   (cl-letf (((symbol-function 'disco-room-open)
                                              (lambda (id name) (setq opened (list id name)))))
                                     (let ((inhibit-read-only t))
                                       (disco-room--insert-system-divider-message message nil))
                                     (goto-char (point-min))
                                     (search-forward "Discussion")
                                     (beginning-of-line)
                                     (appkit-ui-activate)
                                     (should (equal opened '("thread" "Discussion")))
                                     (setq opened nil)
                                     (search-forward "Discussion")
                                     (appkit-ui-activate)
                                     (should (equal opened '("thread" "Discussion"))))))))

(ert-deftest disco-room-pinned-system-message-links-to-captured-channel ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "pins"
                                 (setq-local disco-room--channel-id "pins")
                                 (let ((message
                                        '((id . "notice")
                                          (type . 6)
                                          (author . ((username . "Alice")))
                                          (content . "")))
                                       opened-channel)
                                   (cl-letf (((symbol-function 'disco-room-list-pinned-messages)
                                              (lambda (&optional channel-id)
                                                (setq opened-channel channel-id))))
                                     (let ((inhibit-read-only t))
                                       (disco-room--insert-system-divider-message message nil))
                                     (goto-char (point-min))
                                     (search-forward "View all pinned messages.")
                                     (should (appkit-ui-action-at (point)))
                                     (appkit-ui-activate-at (point))
                                     (should (equal "pins" opened-channel))
                                     (should
                                      (equal "Alice pinned a message to this channel. View all pinned messages."
                                             (disco-room--message-copy-text message))))))))

(ert-deftest disco-room-render-hides-composer-when-send-permission-missing ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "readonly"
                                 (appkit-chatbuf-input-set-text "hello")
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "readonly")
                                    (type . 0)
                                    (guild_id . "g1")
                                    (permissions . "0")))
                                 (disco-room-render)
                                 (should-not (appkit-chatbuf-input-start-position))
                                 (should (string-match-p "composer hidden: missing SEND_MESSAGES"
                                                         (buffer-string)))
                                 (should-not (string-match-p "type at >>>" (buffer-string))))))

(ert-deftest disco-room-render-shows-age-restricted-header-tag ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "adult"
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "adult")
                                    (type . 0)
                                    (guild_id . "g1")
                                    (permissions . "2048")
                                    (nsfw . t)))
                                 (disco-room-render)
                                 (should (string-match-p (regexp-quote "Channel: adult [18+]")
                                                         (buffer-string))))))

(ert-deftest disco-room-render-shows-reply-and-attachments-near-composer ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (appkit-chatbuf-input-set-text "hello")
                                 (disco-room--set-composer-aux-state nil "m42")
                                 (setq-local disco-room--pending-attachments
                                             '((:token-id 1 :path "/tmp/a.txt")
                                               (:token-id 2 :path "/tmp/b.png" :description "preview")))
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "chat")
                                    (type . 0)
                                    (guild_id . "g1")
                                    (permissions . "2048")))
                                 (disco-state-put-messages
                                  "chat"
                                  '(((id . "m42")
                                     (channel_id . "chat")
                                     (content . "hello reply")
                                     (author . ((id . "u1") (username . "alice"))))))
                                 (disco-room-render)
                                 (should (integerp (appkit-chatbuf-input-start-position)))
                                 (should (string-match-p "× ▏ Reply to alice\n  ▏ hello reply"
                                                         (buffer-string)))
                                 (should-not (string-match-p "\\[m42\\]" (buffer-string)))
                                 (goto-char (point-min))
                                 (search-forward "×")
                                 (should (eq (get-text-property (1- (point)) appkit-ui-action-property)
                                             #'disco-room-cancel-reply))
                                 (should (string-match-p "Queued attachments: \\\[file:1\\\] a.txt, \\\[file:2\\\] b.png - preview"
                                                         (buffer-string))))))

(ert-deftest disco-room-render-keeps-reply-context-when-composer-hidden ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "readonly"
                                 (disco-room--set-composer-aux-state nil "m42")
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "readonly")
                                    (type . 0)
                                    (guild_id . "g1")
                                    (permissions . "0")))
                                 (disco-state-put-messages
                                  "readonly"
                                  '(((id . "m42")
                                     (channel_id . "readonly")
                                     (content . "hello reply")
                                     (author . ((id . "u1") (username . "alice"))))))
                                 (disco-room-render)
                                 (should-not (appkit-chatbuf-input-start-position))
                                 (should (string-match-p "× ▏ Reply to alice\n  ▏ hello reply"
                                                         (buffer-string)))
                                 (should-not (string-match-p "\\[m42\\]" (buffer-string))))))

(ert-deftest disco-room-render-shows-composer-when-send-permission-present ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (appkit-chatbuf-input-set-text "hello")
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "chat")
                                    (type . 0)
                                    (guild_id . "g1")
                                    (permissions . "2048")))
                                 (disco-room-render)
                                 (should (integerp (appkit-chatbuf-input-start-position)))
                                 (should (string-match-p (regexp-quote ">>> hello")
                                                         (buffer-string))))))

(ert-deftest disco-room-render-reuses-existing-message-nodes ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "chat")
                                    (type . 0)
                                    (guild_id . "g1")
                                    (permissions . "2048")))
                                 (disco-state-put-messages
                                  "chat"
                                  '(((id . "m2") (channel_id . "chat") (content . "two"))
                                    ((id . "m1") (channel_id . "chat") (content . "one"))))
                                 (disco-room-test-establish-latest-window)
                                 (disco-room-render)
                                 (let ((ewoc (appkit-chat-timeline-ewoc))
                                       (node-m1 (appkit-chat-timeline-node "m1"))
                                       (node-m2 (appkit-chat-timeline-node "m2")))
                                   (disco-state-put-messages
                                    "chat"
                                    '(((id . "m3") (channel_id . "chat") (content . "three"))
                                      ((id . "m2") (channel_id . "chat") (content . "two updated"))
                                      ((id . "m1") (channel_id . "chat") (content . "one"))))
                                   (disco-room-render)
                                   (should (eq ewoc (appkit-chat-timeline-ewoc)))
                                   (should (eq node-m1 (appkit-chat-timeline-node "m1")))
                                   (should (eq node-m2 (appkit-chat-timeline-node "m2")))
                                   (should (appkit-chat-timeline-node "m3"))
                                   (should (equal '("m1" "m2" "m3")
                                                  (appkit-chat-timeline-keys)))
                                   (should (string-match-p "two updated" (buffer-string)))))))

(ert-deftest disco-room-render-shows-plain-attachment-lines-when-rich-cards-disabled ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (let ((disco-room-use-rich-attachment-cards nil)
                                       (disco-room-show-attachment-urls t))
                                   (disco-state-reset)
                                   (disco-state-upsert-channel
                                    '((id . "chat")
                                      (type . 0)
                                      (guild_id . "g1")
                                      (permissions . "2048")))
                                   (disco-state-put-messages
                                    "chat"
                                    '(((id . "m1")
                                       (channel_id . "chat")
                                       (content . "see file")
                                       (attachments . (((id . "a1")
                                                        (filename . "doc.txt")
                                                        (url . "https://example.invalid/doc.txt")))))))
                                   (disco-room-test-establish-latest-window)
                                   (disco-room-render)
                                   (should (string-match-p (regexp-quote "[file] doc.txt") (buffer-string)))
                                   (should (string-match-p (regexp-quote "https://example.invalid/doc.txt")
                                                           (buffer-string)))))))

(ert-deftest disco-room-insert-message-attachments-dispatches-rich-attachments-by-kind ()
  (disco-room-test-with-runtime
   (with-temp-buffer
     (let ((disco-room-use-rich-attachment-cards t)
           (seen nil))
       (cl-letf (((symbol-function 'disco-ins-insert-attachment-photo)
                  (lambda (_attachment &rest _args)
                    (push 'photo seen)
                    (insert "[photo-block]
")))
                 ((symbol-function 'disco-ins-insert-attachment-video)
                  (lambda (_attachment &rest _args)
                    (push 'video seen)
                    (insert "[video-block]
")))
                 ((symbol-function 'disco-ins-insert-attachment-audio)
                  (lambda (_attachment &rest _args)
                    (push 'audio seen)
                    (insert "[audio-block]
")))
                 ((symbol-function 'disco-ins-insert-attachment-document)
                  (lambda (_attachment &rest _args)
                    (push 'document seen)
                    (insert "[document-block]
"))))
         (disco-room--insert-message-attachments
          '((attachments . (((filename . "cat.png"))
                            ((filename . "clip.mp4"))
                            ((filename . "voice-message.ogg")
                             (content_type . "audio/ogg")
                             (duration_secs . 12.0)
                             (waveform . "AAAA"))
                            ((filename . "doc.txt"))))))
         (should (equal '(photo video audio document) (nreverse seen)))
         (should (string-match-p (regexp-quote "[photo-block]") (buffer-string)))
         (should (string-match-p (regexp-quote "[video-block]") (buffer-string)))
         (should (string-match-p (regexp-quote "[audio-block]") (buffer-string)))
         (should (string-match-p (regexp-quote "[document-block]") (buffer-string))))))))

(ert-deftest disco-room-insert-message-attachments-surfaces-render-error ()
  (disco-room-test-with-runtime
   (with-temp-buffer
     (let ((disco-room-use-rich-attachment-cards t))
       (cl-letf (((symbol-function 'disco-ins-insert-attachment-photo)
                  (lambda (&rest _args)
                    (error "boom"))))
         (should-error
          (disco-room--insert-message-attachments
           '((attachments . (((filename . "cat.png")
                              (url . "https://example.invalid/cat.png"))))))))))))

(ert-deftest disco-room-media-completion-redraws-only-dependent-rows ()
  (disco-room-test-with-runtime
   (let ((revision "old"))
     (cl-letf (((symbol-function 'disco-room--insert-message)
                (lambda (message _context &optional _owner)
                  (insert (format "%s:%s\n" (alist-get 'id message) revision)))))
       (disco-room-test-with-surface "media-rows"
                                     (disco-state-put-messages
                                      "media-rows"
                                      '(((id . "m2") (channel_id . "media-rows")
                                         (attachments . (((id . "a2") (filename . "two.ogg")))))
                                        ((id . "m1") (channel_id . "media-rows")
                                         (attachments . (((id . "a1") (filename . "one.ogg")))))))
                                     (disco-room-test-establish-latest-window)
                                     (disco-room-render)
                                     (setq revision "ready")
                                     (disco-room--handle-media-rerender 'audio "a2")
                                     (should (string-match-p "m2:old" (buffer-string)))
                                     (disco-room-test-drain (appkit-current-surface))
                                     (should (string-match-p "m2:ready" (buffer-string)))
                                     (should (string-match-p "m1:old" (buffer-string))))))))

(ert-deftest disco-room-insert-message-attachments-hides-spoiler-media-until-revealed ()
  (disco-room-test-with-runtime
   (with-temp-buffer
     (let ((disco-room-use-rich-attachment-cards t)
           (disco-room--revealed-spoiler-message-id nil)
           spoiler-hidden
           toggled-id)
       (cl-letf (((symbol-function 'disco-ins-insert-attachment-photo)
                  (lambda (_attachment &rest args)
                    (setq spoiler-hidden (plist-get args :spoiler-hidden))
                    (insert "[photo-card]\n")
                    (insert-text-button
                     "[Reveal spoiler]"
                     'action (lambda (_button)
                               (disco-room-toggle-message-spoilers "m1")))
                    (insert "\n")))
                 ((symbol-function 'disco-room-toggle-message-spoilers)
                  (lambda (message-id)
                    (setq toggled-id message-id))))
         (disco-room--insert-message-attachments
          '((id . "m1")
            (attachments . (((filename . "SPOILER_cat.png")
                             (flags . 8))))))
         (should spoiler-hidden)
         (should (string-match-p (regexp-quote "[photo-card]") (buffer-string)))
         (goto-char (point-min))
         (search-forward "[Reveal spoiler]")
         (button-activate (button-at (match-beginning 0)))
         (should (equal "m1" toggled-id)))))))

(ert-deftest disco-room-toggle-message-spoilers-reveals-spoiler-attachment-on-rerender ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (let ((disco-room-use-rich-attachment-cards nil)
                                       (disco-room-show-attachment-urls nil))
                                   (disco-state-reset)
                                   (disco-state-upsert-channel
                                    '((id . "chat")
                                      (type . 0)
                                      (guild_id . "g1")
                                      (permissions . "2048")))
                                   (disco-state-put-messages
                                    "chat"
                                    '(((id . "m1")
                                       (channel_id . "chat")
                                       (content . "")
                                       (attachments . (((id . "a1")
                                                        (filename . "SPOILER_cat.png")
                                                        (flags . 8)
                                                        (width . 640)
                                                        (height . 480)
                                                        (url . "https://example.invalid/cat.png")))))))
                                   (disco-room-test-establish-latest-window)
                                   (disco-room-render)
                                   (should (string-match-p (regexp-quote "[spoiler image hidden]")
                                                           (buffer-string)))
                                   (should-not (string-match-p (regexp-quote "cat.png") (buffer-string)))
                                   (disco-room-toggle-message-spoilers "m1")
                                   (should (string-match-p (regexp-quote "cat.png") (buffer-string)))
                                   (should-not (string-match-p (regexp-quote "[spoiler image hidden]")
                                                               (buffer-string)))))))

(ert-deftest disco-room-thread-entry-is-a-navigable-reference-not-a-button ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (disco-state-reset)
                                 (disco-state-upsert-channel
                                  '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
                                 (disco-state-put-messages
                                  "chat"
                                  '(((id . "m1")
                                     (channel_id . "chat")
                                     (timestamp . "2026-03-08T00:00:00.000000+00:00")
                                     (flags . 32)
                                     (content . "starter")
                                     (author . ((id . "u1") (username . "alice"))))))
                                 (disco-room-test-establish-latest-window)
                                 (disco-room-render)
                                 (should (string-match-p (regexp-quote "↪ Thread: thread:m1")
                                                         (buffer-string)))
                                 (should-not (string-match-p (regexp-quote "[Open thread]")
                                                             (buffer-string)))
                                 (goto-char (point-min))
                                 (search-forward "↪ Thread: thread:m1")
                                 (should (keymapp (get-text-property (match-beginning 0) 'keymap))))))

(ert-deftest disco-room-avatar-resource-hook-forwards-coalesced-resources ()
  (disco-room-test-with-runtime
   (let (changed-resources)
     (cl-letf (((symbol-function 'disco-room--sync-resource-changes-in-open-rooms)
                (lambda (resources)
                  (setq changed-resources resources))))
       (disco-room--handle-avatar-resources-updated
        '((:avatar "avatar-a") (:avatar "avatar-b")))
       (should (equal '((:avatar "avatar-a") (:avatar "avatar-b"))
                      changed-resources))))))

(ert-deftest disco-room-avatar-svg-cache-key-tracks-derived-factor-geometry ()
  (disco-room-test-with-runtime
   (let ((disco-avatar-image-size 28)
         (disco-room-avatar-round-size-factor 1.0)
         (disco-room-avatar-round-inset-ratio 0.0)
         (disco-room-avatar-extra-bottom-line t)
         (disco-room-avatar-factors-alist '((2 . (0.8 . 0.1)))))
     (cl-letf (((symbol-function 'appkit-chat-avatar-line-pixel-height)
                (lambda () 21))
               ((symbol-function 'appkit-chat-avatar-column-pixel-width)
                (lambda () 9)))
       (let* ((before (disco-room--avatar-svg-geometry 2))
              (before-key
               (disco-room--avatar-svg-cache-key "avatar.png" 'mtime 2 before)))
         (setq disco-room-avatar-factors-alist '((2 . (0.82 . 0.08))))
         (let* ((after (disco-room--avatar-svg-geometry 2))
                (after-key
                 (disco-room--avatar-svg-cache-key "avatar.png" 'mtime 2 after)))
           (should (= (plist-get before :char-columns)
                      (plist-get after :char-columns)))
           (should-not (= (plist-get before :circle-height)
                          (plist-get after :circle-height)))
           (should-not (equal before-key after-key))))))))

(ert-deftest disco-room-text-scale-reprints-existing-avatar-slices ()
  (disco-room-test-with-runtime
   (disco-room-test-with-surface "chat"
                                 (disco-state-upsert-channel
                                  '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
                                 (disco-state-put-messages
                                  "chat"
                                  '(((id . "m1")
                                     (channel_id . "chat")
                                     (timestamp . "2026-07-11T12:00:00.000000+00:00")
                                     (content . "hello")
                                     (author . ((id . "u1") (username . "Alice"))))))
                                 (disco-room-test-establish-latest-window)
                                 (let ((line-height 21)
                                       (disco-room-avatar-round-images nil))
                                   (cl-labels
                                       ((avatar-slice-height
                                          ()
                                          (goto-char (point-min))
                                          (search-forward "Alice")
                                          (let* ((prefix
                                                  (get-text-property (line-beginning-position) 'line-prefix))
                                                 (display (and (stringp prefix)
                                                               (get-text-property 0 'display prefix))))
                                            (nth 4 (car display)))))
                                     (cl-letf (((symbol-function 'disco-avatar-image)
                                                (lambda (_user)
                                                  '(image :type png :data "avatar" :width 16 :height 16)))
                                               ((symbol-function 'disco-avatar-cached-file)
                                                (lambda (_user) nil))
                                               ((symbol-function 'appkit-media-image-object-valid-p)
                                                (lambda (image) (and (consp image) (eq (car image) 'image))))
                                               ((symbol-function 'image-size)
                                                (lambda (image &rest _args)
                                                  (cons (or (plist-get (cdr image) :width) 16)
                                                        (or (plist-get (cdr image) :height) 16))))
                                               ((symbol-function 'appkit-chat-avatar-line-pixel-height)
                                                (lambda () line-height))
                                               ((symbol-function 'appkit-chat-avatar-column-pixel-width)
                                                (lambda () 9))
                                               ((symbol-function 'display-graphic-p)
                                                (lambda (&optional _frame) t))
                                               ((symbol-function 'disco-media-clear-preview-memory-cache)
                                                #'ignore)
                                               ((symbol-function 'appkit-geometry-display-window)
                                                (lambda (&rest _arguments) (selected-window)))
                                               ((symbol-function 'appkit-geometry-window-width)
                                                (lambda (&rest _arguments) 80)))
                                       (disco-room-render)
                                       (let ((node (appkit-chat-timeline-node "m1")))
                                         (should (= 21 (avatar-slice-height)))
                                         (setq line-height 35)
                                         (run-hooks 'text-scale-mode-hook)
                                         ;; Geometry notification is queued; the Surface pass owns redraw.
                                         (should (= 21 (avatar-slice-height)))
                                         (disco-room--flush-updates)
                                         (should (eq node (appkit-chat-timeline-node "m1")))
                                         (should (= 35 (avatar-slice-height))))))))))

(ert-deftest disco-room-session-cache-reset-revokes-icon-callbacks-without-sync ()
  (disco-room-test-with-runtime
   (let ((disco-room--session-cache-reset-in-progress nil)
         (disco-room--forward-guild-icon-fetch-generation 4)
         (disco-room--avatar-round-image-cache (make-hash-table :test #'equal))
         (disco-room--forward-guild-icon-image-cache
          (make-hash-table :test #'equal))
         (disco-room--forward-guild-icon-fetching
          (make-hash-table :test #'equal))
         (disco-room-draft-history-search-history
          '("OLD_ACCOUNT_SECRET-draft"))
         (disco-room-search-inplace-history
          '("OLD_ACCOUNT_SECRET-search"))
         then-callback
         else-callback
         canceled
         (plz-calls 0)
         (sync-count 0))
     (puthash "round-secret" "OLD_ACCOUNT_SECRET-round"
              disco-room--avatar-round-image-cache)
     (puthash "old-icon" "https://OLD_ACCOUNT_SECRET.invalid/icon.png"
              disco-room--forward-guild-icon-image-cache)
     (cl-letf (((symbol-function 'plz)
                (lambda (_method _url &rest args)
                  (cl-incf plz-calls)
                  (setq then-callback (plist-get args :then)
                        else-callback (plist-get args :else))
                  'old-icon-process))
               ((symbol-function 'process-live-p)
                (lambda (process) (eq process 'old-icon-process)))
               ((symbol-function 'delete-process)
                (lambda (process)
                  (setq canceled process)
                  ;; Cancellation can run sentinels synchronously.  Both the
                  ;; old callback and an attempted successor must stay inert.
                  (funcall then-callback "OLD_ACCOUNT_SECRET-bytes")
                  (disco-room--start-forward-guild-icon-fetch
                   "reentrant-icon" "new-guild"
                   "https://OLD_ACCOUNT_SECRET.invalid/reentrant.png")))
               ((symbol-function 'create-image)
                (lambda (&rest _args) :image))
               ((symbol-function 'disco-room--forward-guild-icon-image-valid-p)
                (lambda (image) (eq image :image)))
               ((symbol-function 'disco-room--sync-resource-changes-in-open-rooms)
                (lambda (&rest _args) (cl-incf sync-count)))
               ((symbol-function 'disco-room--refresh-open-rooms)
                (lambda () (ert-fail "session reset requested a redraw"))))
       (disco-room--start-forward-guild-icon-fetch
        "live-icon" "old-guild"
        "https://OLD_ACCOUNT_SECRET.invalid/live.png")
       (should (= 1 plz-calls))
       (should (eq 'old-icon-process
                   (plist-get
                    (gethash "live-icon"
                             disco-room--forward-guild-icon-fetching)
                    :process)))
       (disco-room-reset-session-cache-state)
       (should (eq 'old-icon-process canceled))
       (should (= 1 plz-calls))
       (should (= 0 sync-count))
       (dolist (table (list disco-room--avatar-round-image-cache
                            disco-room--forward-guild-icon-image-cache
                            disco-room--forward-guild-icon-fetching))
         (should (= 0 (hash-table-count table))))
       (should-not disco-room-draft-history-search-history)
       (should-not disco-room-search-inplace-history)
       ;; A response already queued by plz remains harmless after reset too.
       (funcall then-callback "OLD_ACCOUNT_SECRET-late-bytes")
       (funcall else-callback '(:message "OLD_ACCOUNT_SECRET-late-error"))
       (should (= 0 sync-count))
       (should (= 0 (hash-table-count
                     disco-room--forward-guild-icon-image-cache)))))))

(ert-deftest disco-room-session-cache-reset-clears-after-cancel-failures ()
  (disco-room-test-with-runtime
   (dolist (failure '(error quit throw))
     (let ((disco-room--forward-guild-icon-fetch-generation 1)
           (disco-room--forward-guild-icon-image-cache
            (make-hash-table :test #'equal))
           (disco-room--forward-guild-icon-fetching
            (make-hash-table :test #'equal))
           (disco-room--avatar-round-image-cache
            (make-hash-table :test #'equal)))
       (puthash "secret" "OLD_ACCOUNT_SECRET"
                disco-room--forward-guild-icon-image-cache)
       (puthash "secret" (list :generation 1 :process 'process)
                disco-room--forward-guild-icon-fetching)
       (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
                 ((symbol-function 'delete-process)
                  (lambda (_process)
                    (pcase failure
                      ('error (error "cancel failed"))
                      ('quit (signal 'quit nil))
                      ('throw (throw 'cancel-escape :escaped))))))
         (let ((result
                (catch 'cancel-escape
                  (disco-room-reset-session-cache-state)
                  :returned)))
           (if (eq failure 'throw)
               (should (eq result :escaped))
             (should (eq result :returned))))
         (should (= 0 (hash-table-count
                       disco-room--forward-guild-icon-image-cache)))
         (should (= 0 (hash-table-count
                       disco-room--forward-guild-icon-fetching))))))))

(ert-deftest disco-room-icon-process-cancel-drain-is-stack-safe ()
  (disco-room-test-with-runtime
   (let ((max-lisp-eval-depth 800)
         (canceled 0))
     (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
               ((symbol-function 'delete-process)
                (lambda (_process) (cl-incf canceled))))
       (disco-room--cancel-icon-processes (number-sequence 1 2000)))
     (should (= 2000 canceled)))
   (let (canceled)
     (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
               ((symbol-function 'delete-process)
                (lambda (process)
                  (push process canceled)
                  (throw 'cancel-escape process))))
       (should (eq 'third
                   (catch 'cancel-escape
                     (disco-room--cancel-icon-processes
                      '(escape second third))
                     :returned))))
     (should (equal '(third second escape) canceled)))))

(ert-deftest disco-room-insert-message-stickers-keeps-textual-fallback ()
  (disco-room-test-with-runtime
   (with-temp-buffer
     (cl-letf (((symbol-function 'disco-sticker-image-slice-rows)
                (lambda (_sticker) nil)))
       (disco-room--insert-message-stickers
        '((sticker_items . (((id . "11") (name . "Wave")
                             (format_type . 1))))))
       (should (equal (buffer-string) "[Sticker: Wave]\n"))))))

;;; disco-room-render-test.el ends here
