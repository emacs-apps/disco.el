;;; disco-translate-test.el --- Translation boundary tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-translate)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest disco-translate-conceals-spoilers-without-changing-copy-export ()
  (let* ((text "# Hello <@123>\n\n> public ||secret **nested**||\n\n- visible ||hidden||\n\n`||literal||`\n\n```\n||code||\n```")
         (document (disco-markdown-document
                    text :message '((mentions . (((id . "123") (username . "Alice")))))))
         (export (disco-markdown-translation-text document)))
    (should (string-match-p "Hello @Alice" export))
    (should (string-match-p "public" export))
    (should (string-match-p "visible" export))
    (should-not (string-match-p "secret\\|nested\\|hidden" export))
    (should (string-match-p (regexp-quote "||literal||") export))
    (should (string-match-p (regexp-quote "||code||") export))
    ;; Exporting for a remote service must not mutate the semantic document.
    (should (string-match-p "secret nested" (appkit-markup-plain-text document)))))

(ert-deftest disco-translate-uses-forward-and-thread-source-not-ui-summaries ()
  (let* ((disco-room--channel-id "chat")
         (body '((id . "100") (content . "**Source** ||secret||")))
         (forward `((id . "200") (content . "")
                    (message_snapshots . (((message . ,body))))))
         (starter `((id . "300") (type . 21) (referenced_message . ,body))))
    (should (equal "Source "
                   (plist-get (disco-room--translation-source forward t) :text)))
    (should (equal "Source "
                   (plist-get (disco-room--translation-source starter t) :text)))))

(ert-deftest disco-translate-edits-and-closed-views-reject-late-results ()
  (let ((disco-room-show-avatars nil)
        (disco-room-show-attachments nil)
        (disco-embed-show-embeds nil)
        callbacks cancelled sent)
    (disco-room-test-with-surface "chat"
      (let* ((message '((id . "100") (channel_id . "chat")
                        (content . "Hello ||secret||")
                        (author . ((id . "u1") (username . "Alice")))))
             (original (copy-tree message))
             (backend (list :id 'controlled :label "Controlled"
                            :start (lambda (source _language resolve _reject)
                                     (push (plist-get source :text) sent)
                                     (push resolve callbacks)
                                     (lambda () (setq cancelled t)))))
             (appkit-translate-backend-function (lambda () backend)))
        (disco-state-put-messages "chat" (list message))
        (disco-room-test-establish-latest-window)
        (appkit-chatbuf-input-replace "untouched draft")
        (disco-room--sync-timeline)
        (goto-char (point-min))
        (search-forward "Hello")
        (disco-room-translate-message)
        (disco-room-test-drain surface)
        (should (equal sent '("Hello ")))
        ;; An in-flight response to an edited message must never appear.
        (disco-state-upsert-message
         "chat" '((id . "100") (channel_id . "chat") (content . "Changed")))
        (disco-room--sync-timeline)
        (funcall (car callbacks) "obsolete translation")
        (disco-room-test-drain surface)
        (should-not (string-match-p "obsolete translation" (buffer-string)))
        (goto-char (point-min))
        (search-forward "Changed")
        (disco-room-translate-message)
        (funcall (car callbacks) "当前译文")
        (disco-room-test-drain surface)
        (should (string-match-p "当前译文" (buffer-string)))
        (should (equal "untouched draft" (appkit-chatbuf-input-string)))
        (should (equal message original))
        (should (equal "Changed" (alist-get 'content (disco-room--message-by-id "100"))))
        ;; A new request is owned by this exact Surface, not its room id.
        (disco-state-upsert-message
         "chat" '((id . "100") (channel_id . "chat") (content . "Third")))
        (disco-room--sync-timeline)
        (goto-char (point-min))
        (search-forward "Third")
        (disco-room-translate-message)
        (appkit-surface-stop surface)
        (should cancelled)
        (let ((before (buffer-string)))
          (funcall (car callbacks) "closed view translation")
          (should (equal before (buffer-string))))))))

(ert-deftest disco-translate-spoiler-only-message-does-not-start-backend ()
  (disco-room-test-with-surface "chat"
    (let ((appkit-translate-backend-function
           (lambda () (ert-fail "Empty text must not initialize the backend"))))
      (disco-state-put-messages
       "chat" '(((id . "100") (channel_id . "chat") (content . "||secret||"))))
      (disco-room-test-establish-latest-window)
      (disco-room--sync-timeline)
      (goto-char (point-min))
      (search-forward "secret")
      (should-error (disco-room-translate-message) :type 'user-error))))

(ert-deftest disco-translate-region-selects-whole-rows-not-boundaries-or-draft ()
  (let ((disco-room-show-avatars nil)
        (disco-room-show-attachments nil)
        (disco-embed-show-embeds nil)
        sent)
    (disco-room-test-with-surface "chat"
      (let ((appkit-translate-backend-function
             (lambda ()
               (list :id 'region :label "Region"
                     :start (lambda (source _language resolve _reject)
                              (push (plist-get source :text) sent)
                              (funcall resolve "译文")
                              nil)))))
        (disco-state-put-messages
         "chat" '(((id . "500") (channel_id . "chat") (content . "Outside"))
                  ((id . "400") (channel_id . "chat") (content . ""))
                  ((id . "300") (channel_id . "chat") (content . "Second"))
                  ((id . "200") (channel_id . "chat") (content . "||secret||"))
                  ((id . "100") (channel_id . "chat") (content . "First\ncontinued"))))
        (disco-room-test-establish-latest-window)
        (appkit-chatbuf-input-replace "private draft")
        (disco-room--sync-timeline)
        (let ((end (ewoc-location (appkit-chat-timeline-node "500"))))
          (goto-char (point-min))
          (search-forward "continued")
          (let ((transient-mark-mode t))
            (set-mark end)
            (activate-mark)
            (call-interactively #'disco-room-translate-message)))
        (disco-room-test-drain surface)
        (should (equal (reverse sent) '("First\ncontinued" "Second")))
        (should (equal "private draft" (appkit-chatbuf-input-string)))
        ;; A range starting exactly at the composer must not fall back to
        ;; the preceding message, nor export selected draft text.
        (goto-char (appkit-chatbuf-input-start-position))
        (let ((transient-mark-mode t))
          (set-mark (point-max))
          (activate-mark)
          (should-error (call-interactively #'disco-room-translate-message)
                        :type 'user-error))
        (should (equal (reverse sent) '("First\ncontinued" "Second")))))))

(ert-deftest disco-translate-filter-selections-use-displayed-snapshots ()
  (dolist (selection-kind '(point region marks visible menu))
    (let ((disco-room-show-avatars nil)
          (disco-room-show-attachments nil)
          (disco-embed-show-embeds nil)
          sent)
      (disco-room-test-with-surface "chat"
        (let ((appkit-translate-backend-function
               (lambda ()
                 (list :id 'filtered :label "Filtered"
                       :start (lambda (source _language resolve _reject)
                                (push (plist-get source :text) sent)
                                (funcall resolve "搜索译文")
                                nil)))))
          (disco-state-put-messages
           "chat" '(((id . "900") (channel_id . "chat")
                     (content . "Outside filter"))
                    ((id . "100") (channel_id . "chat")
                     (content . "New canonical message"))))
          (setq disco-room--msg-filter
                '(:active t :query "snapshot"
                  :items (((id . "100") (channel_id . "chat")
                           (content . "Old search snapshot")))))
          (disco-room--sync-timeline)
          (goto-char (point-min))
          (search-forward "Old search snapshot")
          (pcase selection-kind
            ('region
             (let ((transient-mark-mode t))
               (set-mark (- (point) 8))
               (activate-mark)
               (disco-room-translate-message)
               (deactivate-mark)))
            ('marks
             ;; Off-filter messages are eligible only when explicitly marked.
             (appkit-surface-send surface '(message-mark toggle "100"))
             (appkit-surface-send surface '(message-mark toggle "900"))
             (disco-room-translate-message))
            ('visible
             (save-window-excursion
               (switch-to-buffer (current-buffer))
               (set-window-start (selected-window) (point-min))
               (disco-room-translate-visible)))
            ('menu
             (let ((selection (disco-msg-capture-selection)))
               (goto-char (point-max))
               (let ((disco-msg-command-selection selection))
                 (disco-room-translate-message))))
            (_ (disco-room-translate-message)))
          (disco-room-test-drain surface)
          (should (equal (reverse sent)
                         (if (eq selection-kind 'marks)
                             '("Old search snapshot" "Outside filter")
                           '("Old search snapshot"))))
          (should (equal '("100") (appkit-chat-timeline-keys)))
          (should (string-match-p "搜索译文" (buffer-string)))
          (should-not (string-match-p "New canonical message\\|Outside filter"
                                      (buffer-string))))))))

(ert-deftest disco-translate-filter-live-edits-retire-results-and-refresh-row ()
  (let ((disco-room-show-avatars nil)
        (disco-room-show-attachments nil)
        (disco-embed-show-embeds nil)
        callbacks sent page-callback)
    (disco-room-test-with-surface "chat"
      (let* ((snapshot '((id . "100") (channel_id . "chat")
                         (content . "Old search snapshot")
                         (author . ((id . "u1") (username . "Alice")))))
             (appkit-translate-backend-function
              (lambda ()
                (list :id 'filter-edits :label "Filter edits"
                      :start (lambda (source _language resolve _reject)
                               (push (plist-get source :text) sent)
                               (push resolve callbacks)
                               nil)))))
        (disco-state-put-messages
         "chat" '(((id . "900") (channel_id . "chat")
                   (content . "Outside filter"))
                  ((id . "100") (channel_id . "chat")
                   (content . "New canonical message"))))
        (cl-letf (((symbol-function 'disco-room--search-current-channel-async)
                   (lambda (&rest args)
                     (setq page-callback (plist-get args :on-success)))))
          (disco-room-search--run-filter '(:query "snapshot"))
          (funcall page-callback
                   `((total_results . 2) (messages . ((,snapshot)))))
          (disco-room-test-drain surface)
          (goto-char (point-min))
          (search-forward "Old search snapshot")
          (let ((selection (disco-msg-capture-selection)))
            (disco-room-translate-message)
            (funcall (car callbacks) "旧译文")
            (disco-room-test-drain surface)
            (should (string-match-p "旧译文" (buffer-string)))
            ;; A pending next page must not restore the pre-edit snapshot.
            (disco-room-filter-load-more)
            (let ((edit '((id . "100") (channel_id . "chat")
                          (content . "Edited search snapshot ||secret||"))))
              (disco-gateway--upsert-message "chat" edit)
              (disco-room--queue-update
               surface (list 'gateway-event
                             (list :type 'message-update
                                   :channel-id "chat" :message edit))))
            (disco-room-test-drain surface)
            (should (string-match-p "Edited search snapshot" (buffer-string)))
            (should-not (string-match-p "Old search snapshot\\|旧译文"
                                        (buffer-string)))
            (should (equal "Alice"
                           (disco-room--message-author
                            (disco-room--message-by-id "100"))))
            ;; A captured menu keeps identities, not the previous body.
            (let ((disco-msg-command-selection selection))
              (disco-room-translate-message))
            (let ((edit '((id . "100") (channel_id . "chat")
                          (content . "Latest search body"))))
              (disco-gateway--upsert-message "chat" edit)
              (disco-room--queue-update
               surface (list 'gateway-event
                             (list :type 'message-update
                                   :channel-id "chat" :message edit))))
            (disco-room-test-drain surface)
            (funcall (car callbacks) "迟到译文")
            (disco-room-test-drain surface)
            (should-not (string-match-p "迟到译文" (buffer-string)))
            (funcall page-callback
                     `((total_results . 2)
                       (messages . ((,snapshot)
                                    (((id . "200") (channel_id . "chat")
                                      (content . "Another result")))))))
            (disco-room-test-drain surface)
            (goto-char (point-min))
            (search-forward "Latest search body")
            (disco-room-translate-message)
            (funcall (car callbacks) "当前搜索译文")
            (disco-room-test-drain surface)
            (should (equal (reverse sent)
                           '("Old search snapshot"
                             "Edited search snapshot "
                             "Latest search body")))
            (should (equal '("200" "100") (appkit-chat-timeline-keys)))
            (should (string-match-p "当前搜索译文" (buffer-string)))
            (goto-char (point-min))
            (search-forward "当前搜索译文")
            (should (equal "100" (get-text-property
                                  (match-beginning 0) 'disco-message-id)))
            (should-not (string-match-p
                         "Old search snapshot\\|Edited search snapshot\\|Outside filter\\|旧译文\\|迟到译文"
                         (buffer-string)))
            (should (equal "Old search snapshot" (alist-get 'content snapshot)))))))))

(ert-deftest disco-translate-thread-starter-loads-cached-parent-without-inline ()
  (let ((disco-room-show-avatars nil)
        (disco-room-show-attachments nil)
        (disco-embed-show-embeds nil)
        sent)
    (disco-room-test-with-surface "thread"
      (let ((appkit-translate-backend-function
             (lambda ()
               (list :id 'starter :label "Starter"
                     :start (lambda (source _language resolve _reject)
                              (push (plist-get source :text) sent)
                              (funcall resolve "父消息译文")
                              nil)))))
        (disco-state-put-messages
         "parent" '(((id . "100") (channel_id . "parent")
                     (content . "**Cached parent** ||secret||")
                     (author . ((id . "u1") (username . "Alice"))))))
        (disco-state-put-messages
         "thread" '(((id . "100") (channel_id . "thread") (type . 21)
                     (referenced_message . nil)
                     (message_reference . ((channel_id . "parent")
                                           (message_id . "100"))))))
        (disco-room-test-establish-latest-window)
        (disco-room--sync-timeline)
        (goto-char (point-min))
        (search-forward "Cached parent")
        (disco-room-translate-message)
        (disco-room-test-drain surface)
        (should (equal '("Cached parent ") sent))
        (should (string-match-p "父消息译文" (buffer-string)))
        (should (equal '("100") (appkit-chat-timeline-keys)))))))

(ert-deftest disco-translate-thread-starter-fallback-rejects-self-reference ()
  (disco-room-test-with-surface "thread"
    (let ((source '((id . "50") (channel_id . "thread")
                    (content . "Fallback parent")))
          (starter '((id . "100") (channel_id . "thread") (type . 21)
                     (content . "Synthetic starter"))))
      (disco-state-put-messages "thread" (list starter source))
      (dolist (channel '("parent" "thread"))
        (let ((self (append starter
                            `((message_reference . ((channel_id . ,channel)
                                                    (message_id . "100")))))))
          (should-not (disco-room--thread-starter-reference-message self))
          (should (equal "" (plist-get (disco-room--translation-source self t)
                                      :text)))))
      (let ((fallback
             (append starter
                     '((message_reference . ((channel_id . "parent")
                                             (message_id . "50")))))))
        (should (equal "Fallback parent"
                       (plist-get (disco-room--translation-source fallback t)
                                  :text)))))))

(provide 'disco-translate-test)
;;; disco-translate-test.el ends here
