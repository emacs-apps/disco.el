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
        (appkit-chatbuf-input-state-set "untouched draft")
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
        (should (equal "untouched draft" (disco-room--current-draft)))
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

(provide 'disco-translate-test)
;;; disco-translate-test.el ends here
