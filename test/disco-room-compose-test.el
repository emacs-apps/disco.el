;;; disco-room-compose-test.el --- Composer tests for disco-room -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for room composer state, structured attachments, and outgoing
;; message operations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'disco-room)
(require 'disco-room-test-support
         (expand-file-name
          "disco-room-test-support"
          (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest disco-room-attachment-send-error-callback-restores-controller-only ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "attachment-send-boundary")
            (disco-state-upsert-channel
             `((id . "attachment-send-boundary")
               (type . 0)
               (guild_id . "g1")
               (permissions . ,(number-to-string
                                (logior 2048 (ash 1 15))))))
            (let ((path (make-temp-file "disco-room-callback-attach")))
              (unwind-protect
                  (let* ((view (disco-room--ensure-view))
                         (draft
                          (concat
                           "hello "
                           (disco-room--attachment-input-object-string
                            (disco-room--make-attachment-input-object path)))))
                    (disco-room--set-draft draft)
                    (cl-letf (((symbol-function
                                'disco-api-send-message-with-attachments-async)
                               (lambda (_channel-id &rest args)
                                 (setq callback (plist-get args :on-error))))
                              ((symbol-function 'message) #'ignore))
                      (disco-room-send-message))
                    (should (functionp callback))
                    (should disco-room--send-in-flight)
                    (setq requests nil)
                    (cl-letf (((symbol-function 'disco-room--update-frame)
                               (lambda (&rest _args)
                                 (ert-fail "send callback updated frame directly")))
                              ((symbol-function 'disco-room-render)
                               (lambda ()
                                 (ert-fail "send callback rendered directly")))
                              ((symbol-function 'disco-room--sync-timeline)
                               (lambda (&rest _args)
                                 (ert-fail "send callback projected timeline directly")))
                              ((symbol-function 'appkit-chatbuf-input-replace)
                               (lambda (&rest _args)
                                 (ert-fail "send callback replaced live input directly")))
                              ((symbol-function 'appkit-sync-invalidations)
                               (lambda (&rest _args)
                                 (ert-fail "send callback synced directly")))
                              ((symbol-function 'appkit-request-sync)
                               (lambda (owner &rest args)
                                 (push (cons owner args) requests)))
                              ((symbol-function 'message) #'ignore))
                      (funcall callback '(:message "upload failed"))
                      (should-not disco-room--send-in-flight)
                      (should (= 1 (length requests)))
                      (should (eq view (caar requests)))
                      (should (plist-get (cdar requests) :structure))
                      (should (equal "hello "
                                     (disco-room--draft-without-attachment-tokens)))
                      (should (equal path
                                     (plist-get
                                      (car (disco-room--attachments-from-draft))
                                      :path)))))
                (delete-file path))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-edit-success-callback-restores-controller-only ()
  (let ((disco-runtime--app nil)
        callback
        requests)
    (cl-letf (((symbol-function 'disco-gateway-stop) #'ignore))
      (unwind-protect
          (with-temp-buffer
            (disco-room-mode)
            (disco-room-test-setup-channel "edit-callback-boundary")
            (let ((message
                   '((id . "m1")
                     (channel_id . "edit-callback-boundary")
                     (content . "old body")
                     (author . ((id . "self") (username . "Me"))))))
              (disco-state-put-messages "edit-callback-boundary" (list message))
              (let ((view (disco-room--ensure-view)))
                (disco-room--set-draft "saved draft")
                (disco-room--composer-enter-edit message)
                (disco-room--set-draft "updated body")
                (cl-letf (((symbol-function 'disco-room--edit-permission-reason)
                           (lambda (&optional _msg) nil))
                          ((symbol-function 'disco-api-edit-message-async)
                           (lambda (_channel-id _message-id _content &rest args)
                             (setq callback (plist-get args :on-success))))
                          ((symbol-function 'message) #'ignore))
                  (disco-room-send-message))
                (should (functionp callback))
                (should disco-room--send-in-flight)
                (setq requests nil)
                (cl-letf (((symbol-function 'disco-room--update-frame)
                           (lambda (&rest _args)
                             (ert-fail "edit callback updated frame directly")))
                          ((symbol-function 'disco-room-render)
                           (lambda ()
                             (ert-fail "edit callback rendered directly")))
                          ((symbol-function 'disco-room--sync-timeline)
                           (lambda (&rest _args)
                             (ert-fail "edit callback projected timeline directly")))
                          ((symbol-function 'appkit-chatbuf-input-replace)
                           (lambda (&rest _args)
                             (ert-fail "edit callback replaced live input directly")))
                          ((symbol-function 'appkit-sync-invalidations)
                           (lambda (&rest _args)
                             (ert-fail "edit callback synced directly")))
                          ((symbol-function 'appkit-request-sync)
                           (lambda (owner &rest args)
                             (push (cons owner args) requests)))
                          ((symbol-function 'message) #'ignore))
                  (funcall callback
                           '((id . "m1")
                             (channel_id . "edit-callback-boundary")
                             (content . "updated body")
                             (author . ((id . "self") (username . "Me")))))
                  (should-not disco-room--send-in-flight)
                  (should-not (disco-room--composer-edit-active-p))
                  (should (equal "saved draft"
                                 (appkit-chatbuf-string-plain-text
                                  (disco-room--current-draft))))
                  (should (equal "updated body"
                                 (alist-get
                                  'content
                                  (car (disco-state-messages
                                        "edit-callback-boundary")))))
                  (should (= 1 (length requests)))
                  (should (eq view (caar requests)))
                  (should (plist-get (cdar requests) :structure))))))
        (disco-runtime-stop)))))

(ert-deftest disco-room-draft-history-search-loads-match ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-chatbuf-input-history-push "deploy status")
    (appkit-chatbuf-input-history-push "hello world")
    (appkit-chatbuf-input-history-push "alpha beta")
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room-draft-history-search "hello"))
    (should (equal "hello world"
                   (appkit-chatbuf-string-plain-text
                    (disco-room--current-draft))))))

(ert-deftest disco-room-current-draft-tracks-live-input-through-state-sync ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (appkit-chatbuf-input-state-set "cached")
    (disco-room-render)
    (appkit-chatbuf-input-set-text "live")
    (should (equal "live"
                   (appkit-chatbuf-string-plain-text
                    (disco-room--current-draft))))))

(ert-deftest disco-room-return-completes-token-without-sending ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-chatbuf-install-prompt "> ")
    (insert "@ali")
    (let (completed sent)
      (cl-letf (((symbol-function 'disco-company-completion-token-at-point)
                 (lambda () '(:trigger ?@ :raw "@ali")))
                ((symbol-function 'disco-room-complete-mention)
                 (lambda () (setq completed t)))
                ((symbol-function 'disco-room-send-message)
                 (lambda () (setq sent t))))
        (disco-room-return-dwim))
      (should completed)
      (should-not sent)
      (should (equal "@ali" (appkit-chatbuf-input-string))))))

(ert-deftest disco-room-return-outside-composer-never-sends-draft ()
  (with-temp-buffer
    (disco-room-mode)
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt "> ")
    (insert "unsent draft")
    (goto-char (point-min))
    (let (sent)
      (cl-letf (((symbol-function 'disco-room-send-message)
                 (lambda () (setq sent t))))
        (disco-room-return-dwim))
      (should-not sent)
      (should (appkit-chatbuf-point-in-input-p))
      (should (= (point) (point-max)))
      (should (equal "unsent draft" (appkit-chatbuf-input-string))))))

(ert-deftest disco-room-image-attachment-uses-canonical-preview-object ()
  (let ((path (make-temp-file "disco-composer-preview" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "123456"))
          (cl-letf (((symbol-function
                      'appkit-media-one-line-preview-image-from-file)
                     (lambda (file &optional _max-width)
                       (should (equal file path))
                       '(:composer-preview)))
                    ((symbol-function 'appkit-media-image-display-string)
                     (lambda (image fallback)
                       (propertize fallback 'display image))))
            (let* ((object
                    (disco-room--make-attachment-input-object
                     path :filename "preview.png" :content-type "image/png"))
                   (text (disco-room--attachment-input-object-string object)))
              (should (string-match-p "\\[image\\]" text))
              (should (string-match-p "preview.png" text))
              (should (string-match-p "(6)" text))
              (should (equal object
                             (get-text-property
                              0 appkit-chatbuf-input-object-property text)))
              (should (equal
                       (substring-no-properties text 0 (1- (length text)))
                       (get-text-property
                        0 appkit-chatbuf-input-object-text-property text)))
              (should (get-text-property
                       (1- (length text))
                       appkit-chatbuf-input-object-end-property text)))))
      (ignore-errors (delete-file path)))))

(ert-deftest disco-room-equal-adjacent-objects-parse-as-two-attachments ()
  (let* ((object
          (disco-room--make-attachment-input-object
           "/tmp/reused.png" :filename "reused.png"))
         (label "[image] reused.png")
         (draft
          (concat (appkit-chatbuf-input-object-string label object)
                  (appkit-chatbuf-input-object-string label object)))
         (parsed (disco-room--parse-draft-input draft)))
    (should (= 2 (length (plist-get parsed :objects))))
    (should (= 2 (length (plist-get parsed :attachments))))
    (should (equal "" (plist-get parsed :content)))))

(ert-deftest disco-room-attachment-insertion-inside-object-keeps-both-atomic ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt "> ")
    (let ((first (disco-room--make-attachment-input-object
                  "/tmp/first.png" :filename "first.png"))
          (second (disco-room--make-attachment-input-object
                   "/tmp/second.png" :filename "second.png")))
      (disco-room--insert-attachment-input-object first)
      (goto-char (1+ (appkit-chatbuf-input-start-position)))
      (disco-room--insert-attachment-input-object second)
      (appkit-chatbuf-input-prune-broken-objects)
      (let ((parsed (disco-room--parse-draft-input
                     (appkit-chatbuf-input-string))))
        (should (= 2 (length (plist-get parsed :attachments))))
        (should (equal '("first.png" "second.png")
                       (mapcar (lambda (attachment)
                                 (plist-get attachment :filename))
                               (plist-get parsed :attachments))))))))

(ert-deftest disco-room-draft-history-prev-next-restores-structured-pending-draft ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (let ((path (make-temp-file "disco-room-history"))
          (render-calls 0))
      (unwind-protect
          (progn
            (appkit-chatbuf-input-history-push "older draft")
            (disco-room--set-draft
             (concat "pending "
                     (disco-room--attachment-input-object-string
                      (disco-room--make-attachment-input-object path :filename "a.txt"))))
            (let ((orig-update (symbol-function 'disco-room--update-frame)))
              (cl-letf (((symbol-function 'disco-room--update-frame)
                         (lambda (&rest args)
                           (setq render-calls (1+ render-calls))
                           (apply orig-update args))))
                (disco-room-draft-prev)
                (should (equal "older draft"
                               (appkit-chatbuf-string-plain-text
                                (disco-room--current-draft))))
                (should-not (appkit-chatbuf-string-has-objects-p
                             (disco-room--current-draft)))
                (disco-room-draft-next)
                (should (appkit-chatbuf-string-has-objects-p
                         (disco-room--current-draft)))
                (should (= 1 (length disco-room--pending-attachments)))
                (should (equal "a.txt"
                               (plist-get (car disco-room--pending-attachments)
                                          :filename)))
                (should (= 2 render-calls)))))
        (delete-file path)))))

(ert-deftest disco-room-reply-and-cancel-sync-shared-aux-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (cl-letf (((symbol-function 'disco-room--ensure-action-available)
               (lambda (&rest _args) nil))
              ((symbol-function 'disco-room--update-frame)
               (lambda (&rest _args) nil))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room-reply-to-message "m42")
      (should (equal "m42" disco-room--pending-reply-to))
      (should (eq 'reply (appkit-chatbuf-aux-type)))
      (should (equal "m42" (appkit-chatbuf-aux-message-id)))
      (disco-room-cancel-reply)
      (should-not disco-room--pending-reply-to)
      (should-not (appkit-chatbuf-aux-active-p)))))

(ert-deftest disco-room-input-options-use-shared-chatbuf-state-only ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-chatbuf-input-options-set
     '(:send-on-return t
		       :long-message-action file
		       :allowed-mentions none
		       :reply-mention-replied-user t))
    (setq-local disco-room-send-on-return nil)
    (setq-local disco-room-long-message-action 'split)
    (setq-local disco-room-allowed-mentions 'all)
    (setq-local disco-room-reply-mention-replied-user nil)
    (should (eq t (plist-get (disco-room--input-options-state) :send-on-return)))
    (should (disco-room--input-option-send-on-return))
    (should (eq 'file (disco-room--input-option-long-message-action)))
    (should (eq 'none (disco-room--input-option-allowed-mentions)))
    (should (disco-room--input-option-reply-mention-replied-user))))

(ert-deftest disco-room-input-options-write-path-syncs-shared-state ()
  (with-temp-buffer
    (disco-room-mode)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (disco-room-toggle-send-on-return)
      (should-not disco-room-send-on-return)
      (should-not (disco-room--input-option-send-on-return))
      (disco-room-cycle-long-message-action)
      (should (eq 'file disco-room-long-message-action))
      (should (eq 'file (disco-room--input-option-long-message-action)))
      (disco-room-cycle-allowed-mentions)
      (should (eq 'none disco-room-allowed-mentions))
      (should (eq 'none (disco-room--input-option-allowed-mentions)))
      (disco-room-toggle-reply-mention-replied-user)
      (should disco-room-reply-mention-replied-user)
      (should (disco-room--input-option-reply-mention-replied-user))
      (disco-room-reset-input-options)
      (should (equal (disco-room--current-input-options-state)
                     (disco-room--input-options-state))))))

(ert-deftest disco-room-composer-aux-state-uses-shared-chatbuf-state-only ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room--set-composer-aux-state nil "m1")
    (appkit-chatbuf-aux-set
     '(:aux-type reply :message-id "m1" :aux-msg ((id . "m1") (content . "shared"))))
    (let ((aux (appkit-chatbuf-aux-state)))
      (should (eq 'reply (plist-get aux :aux-type)))
      (should (equal "m1" (plist-get aux :message-id)))
      (should (equal "shared"
                     (alist-get 'content (plist-get aux :aux-msg)))))
    (setq-local disco-room--pending-reply-to "m2")
    (let ((aux (appkit-chatbuf-aux-state)))
      (should (equal "m1" (plist-get aux :message-id)))
      (should (equal "shared"
                     (alist-get 'content (plist-get aux :aux-msg)))))
    (appkit-chatbuf-aux-reset)
    (should-not (appkit-chatbuf-aux-state))))

(ert-deftest disco-room-input-preview-renders-parsed-attachments ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (let ((path (make-temp-file "disco-room-preview"))
          (preview-buf nil))
      (unwind-protect
          (progn
            (disco-room--set-draft
             (concat "hello "
                     (disco-room--attachment-input-object-string
                      (disco-room--make-attachment-input-object path :description "preview"))))
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buffer-or-name &rest _args)
                         (setq preview-buf (get-buffer buffer-or-name))
                         preview-buf))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-input-preview))
            (should (buffer-live-p preview-buf))
            (with-current-buffer preview-buf
              (should (string-match-p "Composer mode: message" (buffer-string)))
              (should (string-match-p "hello" (buffer-string)))
              (should (string-match-p "preview" (buffer-string)))))
        (when (buffer-live-p preview-buf)
          (kill-buffer preview-buf))
        (delete-file path)))))

(ert-deftest disco-room-capture-preserves-attachment-spoiler-side-channel ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat"
                disco-room--guild-id "guild")
    (let* ((object
            (disco-room--make-attachment-input-object
             "/tmp/secret.png" :description "hidden" :spoiler t))
           (draft (disco-room--attachment-input-object-string object)))
      (disco-room--set-draft draft)
      (let* ((capture (appkit-markup-compose-capture))
             (attachments (disco-room--capture-attachments capture)))
        (should (= 1 (length attachments)))
        (should (eq t (plist-get (car attachments) :is-spoiler)))
        (should
         (string-match-p
          "\\[spoiler\\]"
          (disco-room--attachment-input-object-display-text object)))))))

(ert-deftest disco-room-composer-visible-p-hides-read-only-guild-channel ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "readonly")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "readonly")
       (type . 0)
       (guild_id . "g1")
       (permissions . "0")))
    (should-not (disco-room--composer-visible-p))
    (should (equal '(send-messages)
                   (disco-room--composer-missing-permissions)))))

(ert-deftest disco-room-composer-visible-p-uses-thread-send-permission ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (permissions . "2048")))
    (should-not (disco-room--composer-visible-p))
    (should (equal '(send-messages-in-threads)
                   (disco-room--composer-missing-permissions)))))

(ert-deftest disco-room-composer-visible-p-hides-system-user-dm ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "sysdm")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "sysdm")
       (type . 1)
       (recipients . (((id . "643945264868098049")
                       (username . "Discord")
                       (system . t))))))
    (should-not (disco-room--composer-visible-p))
    (should (string-match-p "official Discord system DMs are read-only"
                            (disco-room--composer-hidden-status-line)))))

(ert-deftest disco-room-edit-message-enters-composer-edit-mode ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (appkit-chatbuf-input-state-set "saved draft")
    (let ((msg '((id . "m1")
                 (channel_id . "chat")
                 (content . "old body")
                 (author . ((id . "u1") (username . "alice"))))))
      (disco-state-reset)
      (disco-state-upsert-channel
       '((id . "chat")
         (type . 0)
         (guild_id . "g1")
         (permissions . "2048")))
      (disco-state-put-messages "chat" (list msg))
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg))
                ((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1"))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-edit-message))
      (should (disco-room--composer-edit-active-p))
      (should (equal "m1" (disco-room--composer-edit-message-id)))
      (should (eq 'edit (appkit-chatbuf-aux-type)))
      (should (equal "m1" (appkit-chatbuf-aux-message-id)))
      (should (equal "old body"
                     (appkit-chatbuf-string-plain-text
                      (disco-room--current-draft))))
      (should (string-match-p "× ▏ Editing message\n  ▏ old body"
                              (buffer-string)))
      (should-not (string-match-p "\\[m1\\]" (buffer-string)))
      (should (text-property-any
               (point-min) (point-max) 'disco-room-input t)))))

(ert-deftest disco-room-send-message-commits-composer-edit-and-restores-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (let ((msg '((id . "m1")
                 (channel_id . "chat")
                 (content . "old body")
                 (author . ((id . "u1") (username . "alice"))))))
      (disco-state-reset)
      (disco-state-upsert-channel
       '((id . "chat")
         (type . 0)
         (guild_id . "g1")
         (permissions . "2048")))
      (disco-state-put-messages "chat" (list msg))
      (appkit-chatbuf-input-state-set "saved draft [file:1]")
      (puthash "1" '(:token-id "1" :path "/tmp/a.txt") disco-room--attachment-token-table)
      (setq-local disco-room--attachment-token-seq 1)
      (disco-room--sync-pending-attachments-from-draft)
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg))
                ((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "u1"))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil)))
        (disco-room-edit-message))
      (should (eq 'edit (appkit-chatbuf-aux-type)))
      (should (equal "m1" (appkit-chatbuf-aux-message-id)))
      (disco-room--set-draft "updated body")
      (let (edit-call send-called)
        (cl-letf (((symbol-function 'disco-api-edit-message-async)
                   (lambda (channel-id message-id content &rest args)
                     (setq edit-call (list channel-id message-id content))
                     (funcall (plist-get args :on-success)
                              `((id . ,message-id) (channel_id . ,channel-id)
                                (content . ,content) (author (id . "u1"))))))
                  ((symbol-function 'disco-gateway-current-user-id)
                   (lambda () "u1"))
                  ((symbol-function 'disco-api-send-message-async)
                   (lambda (&rest _args)
                     (setq send-called t)))
                  ((symbol-function 'disco-room--channel-buffer-p)
                   (lambda (&rest _args) t))
                  ((symbol-function 'message)
                   (lambda (&rest _args) nil)))
          (disco-room-send-message))
        (should (equal '("chat" "m1" "updated body") edit-call))
        (should-not send-called)
        (should-not (disco-room--composer-edit-active-p))
        (should-not (appkit-chatbuf-aux-active-p))
        (let ((restored-draft (appkit-chatbuf-input-state)))
          (should (equal "saved draft [file:1]"
                         (appkit-chatbuf-string-plain-text restored-draft)))
          (should-not disco-room--pending-reply-to)
          (should (equal '("1")
                         (disco-room--attachment-token-ids-in-text restored-draft))))
        (should (= 1 (hash-table-count disco-room--attachment-token-table)))))))

(ert-deftest disco-room-set-draft-preserves-ewoc-and-composer-anchor ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (let ((ewoc (appkit-chat-timeline-ewoc))
          (input-start (appkit-chatbuf-input-start-position))
          (prompt-start (appkit-chatbuf-prompt-start-position))
          frame-update-called)
      (cl-letf (((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (setq frame-update-called t))))
        (disco-room--set-draft "updated body"))
      (should (eq ewoc (appkit-chat-timeline-ewoc)))
      (should (= input-start (appkit-chatbuf-input-start-position)))
      (should (= prompt-start (appkit-chatbuf-prompt-start-position)))
      (should-not frame-update-called)
      (should (string-match-p "> updated body" (buffer-string))))))

(ert-deftest disco-room-set-draft-rerenders-when-attachment-footer-state-changes ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (let ((frame-update-called nil))
      (cl-letf (((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (setq frame-update-called t))))
        (disco-room--set-draft
         (disco-room--attachment-input-object-string
          (disco-room--make-attachment-input-object "/tmp/a.txt"))))
      (should frame-update-called)
      (should (= 1 (length disco-room--pending-attachments))))))

(ert-deftest disco-room-sync-draft-from-buffer-preserves-attachment-input-objects ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "2048")))
    (disco-room-render)
    (goto-char (point-max))
    (appkit-chatbuf-input-insert "hello ")
    (disco-room--insert-attachment-input-object
     (disco-room--make-attachment-input-object
      "/tmp/a.txt"
      :description "preview"))
    (disco-room--sync-draft-from-buffer)
    (should (appkit-chatbuf-string-has-objects-p
             (disco-room--current-draft)))
    (should (= 1 (length (disco-room--draft-input-objects))))
    (should (equal "hello "
                   (disco-room--draft-without-attachment-tokens)))
    (should (equal '((:path "/tmp/a.txt"
			    :filename "a.txt"
			    :description "preview"))
                   (disco-room--attachments-from-draft)))))

(ert-deftest disco-room-send-message-parses-attachment-input-objects ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     `((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . ,(number-to-string (logior 2048 (ash 1 15))))))
    (disco-room-render)
    (let ((path (make-temp-file "disco-room-object-attach"))
          sent-content
          sent-attachments)
      (unwind-protect
          (progn
            (goto-char (point-max))
            (appkit-chatbuf-input-insert "hello ")
            (disco-room--insert-attachment-input-object
             (disco-room--make-attachment-input-object
              path
              :description "preview"))
            (cl-letf (((symbol-function 'disco-api-send-message-with-attachments-async)
                       (lambda (_channel-id &rest args)
                         (setq sent-content (plist-get args :content)
                               sent-attachments (plist-get args :attachments))
                         (funcall (plist-get args :on-success)
                                  `((id . "server-1")
                                    (nonce . ,(plist-get args :nonce))
                                    (channel_id . "chat")))))
                      ((symbol-function 'disco-room--channel-buffer-p)
                       (lambda (&rest _args) t))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-send-message))
            (should (equal "hello" sent-content))
            (should (equal `((:path ,path
				    :filename ,(file-name-nondirectory path)
				    :description "preview"))
                           sent-attachments))
            (should (equal ""
                           (appkit-chatbuf-string-plain-text
                            (disco-room--current-draft)))))
        (delete-file path)))))

(ert-deftest disco-room-composer-visible-p-hides-archived-thread ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "thread")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "thread")
       (type . 11)
       (guild_id . "g1")
       (permissions . "274877906944")
       (thread_metadata . ((archived . t)))))
    (should-not (disco-room--composer-visible-p))
    (should (string-match-p "current thread is archived"
                            (disco-room--composer-hidden-status-line)))))

(ert-deftest disco-room-attach-file-errors-while-editing ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (disco-room--set-composer-aux-state '(:type edit :message-id "m1" :saved-state nil) nil)
    (let ((path (make-temp-file "disco-room-attach")))
      (unwind-protect
          (should-error (disco-room-attach-file path) :type 'user-error)
        (delete-file path)))))

(ert-deftest disco-room-attach-file-inserts-attachment-input-object ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "34816")))
    (disco-room-render)
    (let ((path (make-temp-file "disco-room-attach")))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-attach-file path "preview"))
            (should (appkit-chatbuf-string-has-objects-p
                     (disco-room--current-draft)))
            (should (equal `((:path ,path
				    :filename ,(file-name-nondirectory path)
				    :description "preview"))
                           (disco-room--attachments-from-draft)))
            (should (string-match-p (regexp-quote (format "[file] %s"
                                                          (file-name-nondirectory path)))
                                    (buffer-string))))
        (delete-file path)))))

(ert-deftest disco-room-remove-attachment-token-removes-attachment-input-object ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (setq-local disco-room--channel-name "chat")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat")
       (type . 0)
       (guild_id . "g1")
       (permissions . "34816")))
    (disco-room-render)
    (let ((path (make-temp-file "disco-room-attach")))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (appkit-chatbuf-input-insert "hello ")
            (disco-room--insert-attachment-input-object
             (disco-room--make-attachment-input-object path :description "preview"))
            (goto-char (or (text-property-not-all (appkit-chatbuf-input-start-position)
                                                  (point-max)
                                                  appkit-chatbuf-input-object-property
                                                  nil)
                           (point-max)))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest _args) nil)))
              (disco-room-remove-attachment-token-at-point))
            (should-not (appkit-chatbuf-string-has-objects-p
                         (disco-room--current-draft)))
            (should (equal '() (disco-room--attachments-from-draft)))
            (should (equal "hello "
                           (appkit-chatbuf-string-plain-text
                            (disco-room--current-draft)))))
        (delete-file path)))))

(ert-deftest disco-room-remove-attachment-token-errors-while-editing ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room--set-composer-aux-state '(:type edit :message-id "m1" :saved-state nil) nil)
    (appkit-chatbuf-input-state-set "[file:1]")
    (should-error (disco-room-remove-attachment-token-at-point) :type 'user-error)))

(ert-deftest disco-room-forward-message-errors-while-replying ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-composer-aux-state nil "m1")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
    (should-error (disco-room-forward-message "m2" "src" nil nil)
                  :type 'user-error)))

(ert-deftest disco-room-send-message-errors-for-system-user-dm ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "sysdm")
    (appkit-chatbuf-input-state-set "hello")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "sysdm")
       (type . 1)
       (recipients . (((id . "643945264868098049")
                       (username . "Discord")
                       (system . t))))))
    (should-error (disco-room-send-message) :type 'user-error)))

(ert-deftest disco-room-edit-message-errors-while-replying ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-composer-aux-state nil "m1")
    (let ((msg '((id . "m2")
                 (channel_id . "chat")
                 (content . "body")
                 (author . ((id . "u1") (username . "alice"))))))
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chat") (type . 0) (guild_id . "g1") (permissions . "2048")))
      (cl-letf (((symbol-function 'disco-room--message-at-point)
                 (lambda () msg)))
        (should-error (disco-room-edit-message) :type 'user-error)))))

(ert-deftest disco-room-send-message-splits-overlong-content-by-default ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (appkit-chatbuf-input-state-set "first line\nsecond line\nthird line")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "2048")))
    (let ((disco-api--message-content-limit 12)
          sent)
      (cl-letf (((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chan"))))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-room-render)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'disco-api-send-message-async)
                 (lambda (_channel-id content &rest args)
                   (push content sent)
                   (funcall (plist-get args :on-success)
                            `((id . ,(format "server-%d" (length sent)))
                              (nonce . ,(plist-get args :nonce))
                              (channel_id . "chan"))))))
        (disco-room-send-message)
        (should (equal '("first line" "second line" "third line")
                       (nreverse sent)))
        (should-not disco-room--send-in-flight)
        (should (equal ""
                       (appkit-chatbuf-string-plain-text
                        (disco-room--current-draft))))))))

(ert-deftest disco-room-send-message-sends-overlong-content-as-file-when-configured ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (let* ((long-text (make-string 32 ?x))
           (disco-api--message-content-limit 10)
           (disco-room-long-message-action 'file)
           sent-content
           sent-attachments
           captured-file-body)
      (appkit-chatbuf-input-state-set long-text)
      (disco-room--sync-shared-input-options-state)
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "2048")))
      (cl-letf (((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chan"))))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _args) t))
                ((symbol-function 'disco-room-render)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'disco-api-send-message-with-attachments-async)
                 (lambda (_channel-id &rest args)
                   (setq sent-content (plist-get args :content)
                         sent-attachments (plist-get args :attachments))
                   (with-temp-buffer
                     (insert-file-contents (plist-get (car sent-attachments) :path))
                     (setq captured-file-body (buffer-string)))
                   (funcall (plist-get args :on-success)
                            `((id . "server-file")
                              (nonce . ,(plist-get args :nonce))
                              (channel_id . "chan"))))))
        (disco-room-send-message)
        (should-not sent-content)
        (should (= 1 (length sent-attachments)))
        (should (equal disco-room-long-message-file-name
                       (plist-get (car sent-attachments) :filename)))
        (should (equal long-text captured-file-body))
        (should-not disco-room--send-in-flight)
        (should (equal ""
                       (appkit-chatbuf-string-plain-text
                        (disco-room--current-draft))))))))

(ert-deftest disco-room-send-message-rejects-overlong-content-before-send-state ()
  (let ((disco-room-long-message-action 'file))
    (with-temp-buffer
      (disco-room-mode)
      (setq-local disco-room--channel-id "chan")
      (appkit-chatbuf-input-state-set
       (make-string (1+ disco-api--message-content-limit) ?a))
      (disco-room--sync-shared-input-options-state)
      (disco-state-reset)
      (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "0")))
      (should-error (disco-room-send-message) :type 'error)
      (should-not disco-room--send-in-flight)
      (should (= (1+ disco-api--message-content-limit)
                 (length (disco-room--current-draft)))))))

(ert-deftest disco-room-forward-message-rejects-overlong-comment-before-send-state ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chan")
    (disco-state-reset)
    (disco-state-upsert-channel '((id . "chan") (type . 0) (permissions . "2048")))
    (cl-letf (((symbol-function 'disco-room--ensure-jump-permissions)
               (lambda (&rest _args) t)))
      (should-error
       (disco-room-forward-message
        "m1" "chan"
        (make-string (1+ disco-api--message-content-limit) ?a)
        nil)
       :type 'error)
      (should-not disco-room--send-in-flight))))

(ert-deftest disco-room-forward-message-upserts-response-without-refresh ()
  (with-temp-buffer
    (disco-room-mode)
    (let ((disco-room--channel-id "target")
          requested
          refreshed)
      (disco-state-reset)
      (cl-letf (((symbol-function 'disco-room--forward-unavailable-reason)
                 (lambda () nil))
                ((symbol-function 'disco-room--resolve-target-channel)
                 (lambda (_channel-id) '((id . "source") (type . 0))))
                ((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "target") (type . 0))))
                ((symbol-function 'disco-permission-ensure-channel) #'ignore)
                ((symbol-function 'disco-room--ensure-jump-permissions) #'ignore)
                ((symbol-function 'disco-room--update-frame)
                 (lambda (&rest _args)
                   (ert-fail "forward callback updated frame directly")))
                ((symbol-function 'disco-room-render)
                 (lambda ()
                   (ert-fail "forward callback rendered directly")))
                ((symbol-function 'disco-room--render-send-state-change)
                 (lambda ()
                   (ert-fail "forward callback rendered directly")))
                ((symbol-function 'appkit-sync-invalidations)
                 (lambda (&rest _args)
                   (ert-fail "forward callback synced directly")))
                ((symbol-function 'appkit-request-sync)
                 (lambda (&rest _args) (setq requested t)))
                ((symbol-function 'disco-room-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'disco-api-forward-message-async)
                 (lambda (_target _message _source &rest args)
                   (funcall (plist-get args :on-success)
                            '((id . "100") (channel_id . "target")
                              (content . "forwarded")))))
                ((symbol-function 'message) #'ignore))
        (disco-room-forward-message "50" "source" nil nil)
        (should requested)
        (should-not refreshed)
        (should-not disco-room--send-in-flight)
        (should (equal "100"
                       (alist-get 'id (car (disco-state-messages "target")))))))))

(ert-deftest disco-room-preview-name-collision-preserves-ordinary-buffer ()
  (let* ((disco-room--preview-buffer nil)
         (disco-room--preview-buffer-name
          (generate-new-buffer-name "*disco-room-preview-collision*"))
         (ordinary (get-buffer-create disco-room--preview-buffer-name))
         owned)
    (unwind-protect
        (progn
          (with-current-buffer ordinary
            (insert "UNRELATED_PREVIEW_SENTINEL"))
          (setq owned (disco-room--owned-preview-buffer))
          (should (buffer-live-p owned))
          (should-not (eq ordinary owned))
          (should (buffer-local-value
                   'disco-room--preview-buffer-owner-p owned))
          (with-current-buffer owned
            (special-mode)
            (rename-buffer "*renamed-owned-room-preview*" t))
          (should (eq owned (disco-room--owned-preview-buffer)))
          (with-current-buffer ordinary
            (should (equal "UNRELATED_PREVIEW_SENTINEL" (buffer-string)))))
      (when (buffer-live-p owned)
        (kill-buffer owned))
      (when (buffer-live-p ordinary)
        (kill-buffer ordinary)))))

(ert-deftest disco-room-send-sticker-forwards-exact-snowflake ()
  (with-temp-buffer
    (let ((disco-room--channel-id "22")
          (disco-room--guild-id "33")
          (disco-room--send-in-flight nil)
          sent-ids)
      (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                 #'ignore)
                ((symbol-function 'disco-room--sticker-unavailable-reason)
                 (lambda () nil))
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "22"))))
                ((symbol-function 'disco-room--ensure-view)
                 (lambda () 'view))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'appkit-request-sync) #'ignore)
                ((symbol-function 'disco-state-merge-message-response) #'ignore)
                ((symbol-function 'disco-room--request-render) #'ignore)
                ((symbol-function 'disco-api-send-message-async)
                 (lambda (_channel-id _content &rest options)
                   (setq sent-ids (plist-get options :sticker-ids))
                   (funcall (plist-get options :on-success)
                            '((id . "server-message")))))
                ((symbol-function 'message) #'ignore))
        (disco-room--send-sticker-object
         '((id . "9007199254740993123") (name . "Wave")
           (format_type . 1)))
        (should (equal sent-ids '("9007199254740993123")))
        (should-not disco-room--send-in-flight)))))

(ert-deftest disco-room-edit-permission-is-author-only-and-fails-closed ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (cl-letf (((symbol-function 'disco-room--room-send-restriction-reason)
               (lambda (&rest _arguments) nil)))
      (cl-letf (((symbol-function 'disco-gateway-current-user-id)
                 (lambda () "self")))
        (should-not
         (disco-room--edit-permission-reason
          '((id . "own") (author . ((id . "self"))))))
        (should
         (equal "only your own messages can be edited"
                (disco-room--edit-permission-reason
                 '((id . "other") (author . ((id . "other"))))))))
      (cl-letf (((symbol-function 'disco-gateway-current-user-id)
                 (lambda () nil)))
        (should
         (equal "only your own messages can be edited"
                (disco-room--edit-permission-reason
                 '((id . "unknown") (author . ((id . "self"))))))))
      (should
       (equal "message ownership is unavailable"
              (disco-room--edit-permission-reason nil))))))

(ert-deftest disco-room-deleted-aux-target-preserves-current-draft ()
  (dolist (context '((nil "m1") ((:type edit :message-id "m1") nil)))
    (with-temp-buffer
      (disco-room-mode)
      (appkit-chatbuf-input-state-set "keep this draft")
      (disco-room--set-composer-aux-state (car context) (cadr context))
      (should
       (disco-room--retire-deleted-composer-context "m1"))
      (should-not (appkit-chatbuf-aux-state))
      (should (equal "keep this draft"
                     (appkit-chatbuf-string-plain-text
                      (disco-room--current-draft)))))))

(ert-deftest disco-room-synchronous-attachment-setup-error-settles-send-once ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (let* ((path (make-temp-file "disco-room-sync-attach"))
           (draft
            (concat
             "hello "
             (disco-room--attachment-input-object-string
              (disco-room--make-attachment-input-object path)))))
      (unwind-protect
          (progn
            (disco-room--set-draft draft)
            (disco-room--set-composer-aux-state nil "reply")
            (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                       #'ignore)
                      ((symbol-function 'disco-permission-ensure-channel)
                       (lambda (&rest _arguments) t))
                      ((symbol-function 'disco-room--channel-object)
                       (lambda () '((id . "chat"))))
                      ((symbol-function 'disco-room--ensure-view)
                       (lambda () 'view))
                      ((symbol-function 'disco-room--channel-buffer-p)
                       (lambda (&rest _arguments) t))
                      ((symbol-function 'appkit-request-sync) #'ignore)
                      ((symbol-function 'disco-room--request-render) #'ignore)
                      ((symbol-function 'disco-room--update-frame) #'ignore)
                      ((symbol-function 'disco-api-send-message-with-attachments-async)
                       (lambda (&rest _arguments)
                         (error "attachment normalization exploded")))
                      ((symbol-function 'message) #'ignore))
              (should-error (disco-room-send-message) :type 'error))
            (should-not disco-room--send-in-flight)
            (should (eq 'reply (appkit-chatbuf-aux-type)))
            (should (equal "hello "
                           (disco-room--draft-without-attachment-tokens)))
            (should (equal path
                           (plist-get
                            (car (disco-room--attachments-from-draft))
                            :path)))
            (should-not (disco-state-messages "chat")))
        (delete-file path)))))

(ert-deftest disco-room-send-callbacks-have-one-terminal-winner ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-draft "sent draft")
    (let (success error)
      (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                 #'ignore)
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chat"))))
                ((symbol-function 'disco-room--ensure-view)
                 (lambda () 'view))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'appkit-request-sync) #'ignore)
                ((symbol-function 'disco-room--request-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'disco-api-send-message-async)
                 (lambda (_channel-id _content &rest options)
                   (setq success (plist-get options :on-success)
                         error (plist-get options :on-error))))
                ((symbol-function 'message) #'ignore))
        (disco-room-send-message)
        (funcall success
                 '((id . "server") (channel_id . "chat")
                   (content . "sent draft")))
        (funcall error '(:message "late failure")))
      (should-not disco-room--send-in-flight)
      (should (equal '("server")
                     (mapcar (lambda (message) (alist-get 'id message))
                             (disco-state-messages "chat"))))
      (should (equal "" (disco-room--current-draft))))))

(ert-deftest disco-room-late-send-failure-never-overwrites-new-draft ()
  (with-temp-buffer
    (disco-state-reset)
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat")
    (disco-room--set-draft "old draft")
    (let (failure)
      (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                 #'ignore)
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chat"))))
                ((symbol-function 'disco-room--ensure-view)
                 (lambda () 'view))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'appkit-request-sync) #'ignore)
                ((symbol-function 'disco-room--request-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'disco-api-send-message-async)
                 (lambda (_channel-id _content &rest options)
                   (setq failure (plist-get options :on-error))))
                ((symbol-function 'message) #'ignore))
        (disco-room-send-message)
        (disco-room--set-draft "new draft")
        (funcall failure '(:message "late failure")))
      (should-not disco-room--send-in-flight)
      (should (equal "new draft" (disco-room--current-draft))))))

(ert-deftest disco-room-operation-slot-drops-reply-target-deleted-in-flight ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "chat")
    (disco-state-put-messages
     "chat" '(((id . "m1") (channel_id . "chat") (content . "target"))))
    (disco-room--set-draft "reply draft")
    (disco-room--set-composer-aux-state nil "m1")
    (let* ((slot (disco-room--composer-operation-slot))
           (revision (disco-room--clear-composer-operation-slot)))
      (disco-state-delete-message "chat" "m1")
      (should (disco-room--restore-composer-operation-slot revision slot t))
      (should (equal "reply draft" (disco-room--current-draft)))
      (should-not disco-room--pending-reply-to)
      (should-not (appkit-chatbuf-aux-active-p)))))

(ert-deftest disco-room-operation-slot-drops-edit-target-deleted-in-flight ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "chat")
    (disco-state-put-messages
     "chat" '(((id . "m1") (channel_id . "chat") (content . "target"))))
    (disco-room--set-draft "edited draft")
    (disco-room--set-composer-aux-state
     (list :type 'edit :message-id "m1"
           :saved-state (list :draft "older draft" :reply-to nil))
     nil)
    (let* ((slot (disco-room--composer-operation-slot))
           (revision (disco-room--clear-composer-operation-slot)))
      (disco-state-delete-message "chat" "m1")
      (should (disco-room--restore-composer-operation-slot revision slot t))
      (should (equal "edited draft" (disco-room--current-draft)))
      (should-not disco-room--pending-edit)
      (should-not (appkit-chatbuf-aux-active-p)))))

(ert-deftest disco-room-operation-slot-retains-unmutated-filter-only-reply ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "chat")
    (disco-room--set-draft "filtered reply")
    (disco-room--set-composer-aux-state nil "filter-only")
    (let* ((slot (disco-room--composer-operation-slot))
           (revision (disco-room--clear-composer-operation-slot)))
      (should (disco-room--restore-composer-operation-slot revision slot t))
      (should (equal "filter-only" disco-room--pending-reply-to))
      (should (eq 'reply (appkit-chatbuf-aux-type))))))

(ert-deftest disco-room-deleted-send-response-does-not-advance-live-frontier ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "chat")
    (setq disco-room--remote-latest-message-id "100")
    (disco-room--set-draft "sent draft")
    (let (success)
      (cl-letf (((symbol-function 'disco-room--ensure-action-available)
                 #'ignore)
                ((symbol-function 'disco-permission-ensure-channel)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'disco-room--channel-object)
                 (lambda () '((id . "chat"))))
                ((symbol-function 'disco-room--ensure-view)
                 (lambda () 'view))
                ((symbol-function 'disco-room--channel-buffer-p)
                 (lambda (&rest _arguments) t))
                ((symbol-function 'appkit-request-sync) #'ignore)
                ((symbol-function 'disco-room--request-render) #'ignore)
                ((symbol-function 'disco-room--update-frame) #'ignore)
                ((symbol-function 'disco-api-send-message-async)
                 (lambda (_channel-id _content &rest options)
                   (setq success (plist-get options :on-success))))
                ((symbol-function 'message) #'ignore))
        (disco-room-send-message)
        (disco-state-delete-message "chat" "200")
        (funcall success
                 '((id . "200") (channel_id . "chat")
                   (content . "stale response"))))
      (should (equal "100" disco-room--remote-latest-message-id))
      (should-not (disco-room--channel-message-by-id "chat" "200"))
      (should-not (disco-state-messages "chat")))))

(ert-deftest disco-room-forward-captures-revision-after-access-probe ()
  (with-temp-buffer
    (disco-room-mode)
    (disco-room-test-setup-channel "chat")
    (cl-letf (((symbol-function 'disco-room--ensure-action-available)
               #'ignore)
              ((symbol-function 'disco-permission-ensure-channel)
               (lambda (&rest _arguments) t))
              ((symbol-function 'disco-room--channel-object)
               (lambda () '((id . "chat"))))
              ((symbol-function 'disco-room--resolve-target-channel)
               (lambda (_channel-id) '((id . "source"))))
              ((symbol-function 'disco-room--ensure-jump-permissions)
               (lambda (&rest _arguments)
                 (disco-state-upsert-message
                  "chat"
                  '((id . "300") (channel_id . "chat")
                    (content . "updated during probe")))))
              ((symbol-function 'disco-room--ensure-view)
               (lambda () 'view))
              ((symbol-function 'disco-room--channel-buffer-p)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'appkit-request-sync) #'ignore)
              ((symbol-function 'disco-api-forward-message-async)
               (lambda (_target-channel-id _message-id _source-channel-id
                        &rest options)
                 (funcall
                  (plist-get options :on-success)
                  '((id . "300") (channel_id . "chat")
                    (content . "forward response"))))))
      (disco-room-forward-message "source-message" "source" nil t))
    (should
     (equal "forward response"
            (alist-get 'content
                       (disco-room--channel-message-by-id "chat" "300"))))))

(ert-deftest disco-room-compose-prefix-selection-is-semantic-and-nonpersistent ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat"
                disco-room--guild-id "guild")
    (disco-room--set-draft "*bold*")
    (should (eq 'discord-markdown appkit-markup-compose-active-codec))
    (let* ((capture (appkit-markup-compose-capture '(4)))
           (output
            (appkit-markup-compose-output capture 'discord-markdown)))
      (should (eq 'org (appkit-markup-compose-capture-codec capture)))
      (should (equal "bold"
                     (appkit-markup-plain-text
                      (appkit-markup-compose-document capture))))
      (should (equal "**bold**"
                     (appkit-markup-compose-output-source output)))
      (should-not (appkit-markup-compose-output-losses output))
      (should (eq 'discord-markdown appkit-markup-compose-active-codec)))))

(ert-deftest disco-room-send-org-capture-drives-wire-and-local-echo ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat"
                disco-room--channel-name "chat"
                disco-room--guild-id "guild")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat") (type . 0) (guild_id . "guild")
       (permissions . "2048")))
    (disco-room-render)
    (appkit-markup-compose-set-active-codec 'org)
    (disco-room--set-draft "*bold* /italic/ _under_")
    (let (wire pending-document)
      (cl-letf
          (((symbol-function 'disco-api-send-message-async)
            (lambda (channel-id content &rest args)
              (setq wire content
                    pending-document
                    (alist-get
                     'appkit_document
                     (car (disco-state-messages channel-id))))
              (funcall
               (plist-get args :on-success)
               `((id . "server-1")
                 (nonce . ,(plist-get args :nonce))
                 (channel_id . ,channel-id)
                 (content . ,content)))))
           ((symbol-function 'disco-room--channel-buffer-p)
            (lambda (&rest _arguments) t))
           ((symbol-function 'message) #'ignore))
        (disco-room-send-message))
      (should (equal "**bold** *italic* __under__" wire))
      (should (appkit-markup-document-p pending-document))
      (should (equal "bold italic under"
                     (appkit-markup-plain-text pending-document)))
      (should (eq 'org appkit-markup-compose-active-codec)))))

(ert-deftest disco-room-send-preserves-discord-spoiler-syntax ()
  (with-temp-buffer
    (disco-room-mode)
    (setq-local disco-room--channel-id "chat"
                disco-room--channel-name "chat"
                disco-room--guild-id "guild")
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "chat") (type . 0) (guild_id . "guild")
       (permissions . "2048")))
    (disco-room-render)
    (disco-room--set-draft "before ||**secret**|| after")
    (let (wire)
      (cl-letf
          (((symbol-function 'disco-api-send-message-async)
            (lambda (channel-id content &rest args)
              (setq wire content)
              (funcall
               (plist-get args :on-success)
               `((id . "server-spoiler")
                 (nonce . ,(plist-get args :nonce))
                 (channel_id . ,channel-id)
                 (content . ,content)))))
           ((symbol-function 'disco-room--channel-buffer-p)
            (lambda (&rest _arguments) t))
           ((symbol-function 'message) #'ignore))
        (disco-room-send-message))
      (should (equal "before ||**secret**|| after" wire)))))

(ert-deftest disco-room-split-send-keeps-semantic-delimiters-balanced ()
  (let ((disco-api--message-content-limit 20)
        (disco-room-long-message-action 'split))
    (with-temp-buffer
      (disco-room-mode)
      (setq-local disco-room--channel-id "chat"
                  disco-room--channel-name "chat"
                  disco-room--guild-id "guild")
      (disco-state-reset)
      (disco-state-upsert-channel
       '((id . "chat") (type . 0) (guild_id . "guild")
         (permissions . "2048")))
      (disco-room-render)
      (disco-room--set-draft
       (concat (make-string 18 ?x) "||secret||"))
      (let (wires documents)
        (cl-letf
            (((symbol-function 'disco-api-send-message-async)
              (lambda (channel-id content &rest args)
                (push content wires)
                (push
                 (alist-get
                  'appkit_document
                  (car (disco-state-messages channel-id)))
                 documents)
                (funcall
                 (plist-get args :on-success)
                 `((id . ,(format "server-%d" (length wires)))
                   (nonce . ,(plist-get args :nonce))
                   (channel_id . ,channel-id)
                   (content . ,content)))))
             ((symbol-function 'disco-room--channel-buffer-p)
              (lambda (&rest _arguments) t))
             ((symbol-function 'message) #'ignore))
          (disco-room-send-message))
        (setq wires (nreverse wires)
              documents (nreverse documents))
        (should (equal (list (make-string 18 ?x) "||secret||") wires))
        (should (= 2 (length documents)))
        (should (cl-every #'appkit-markup-document-p documents))))))

(ert-deftest disco-room-partial-split-failure-restores-balanced-provider-draft ()
  (let ((disco-api--message-content-limit 20)
        (disco-room-long-message-action 'split))
    (with-temp-buffer
      (disco-room-mode)
      (setq-local disco-room--channel-id "chat"
                  disco-room--channel-name "chat"
                  disco-room--guild-id "guild")
      (disco-state-reset)
      (disco-state-upsert-channel
       '((id . "chat") (type . 0) (guild_id . "guild")
         (permissions . "2048")))
      (disco-room-render)
      (appkit-markup-compose-set-active-codec 'org)
      (disco-room--set-draft
       (concat (make-string 17 ?x) " *secret*"))
      (let ((leg 0) wires)
        (cl-letf
            (((symbol-function 'disco-api-send-message-async)
              (lambda (channel-id content &rest args)
                (push content wires)
                (setq leg (1+ leg))
                (if (= leg 1)
                    (funcall
                     (plist-get args :on-success)
                     `((id . "server-first")
                       (nonce . ,(plist-get args :nonce))
                       (channel_id . ,channel-id)
                       (content . ,content)))
                  (funcall
                   (plist-get args :on-error)
                   '(:message "second leg failed")))))
             ((symbol-function 'disco-room--channel-buffer-p)
              (lambda (&rest _arguments) t))
             ((symbol-function 'message) #'ignore))
          (disco-room-send-message))
        (setq wires (nreverse wires))
        (should (equal (cadr wires) (disco-room--current-draft)))
        (should (eq 'discord-markdown
                    appkit-markup-compose-active-codec))
        (should-not disco-room--send-in-flight)))))

(ert-deftest disco-room-edit-restores-source-codec-with-rich-draft ()
  (with-temp-buffer
    (disco-room-mode)
    (appkit-markup-compose-set-active-codec 'org)
    (cl-letf (((symbol-function 'disco-room--update-frame) #'ignore)
              ((symbol-function 'appkit-chatbuf-focus-input) #'ignore))
      (disco-room--set-draft
       (concat "draft "
               (disco-room--attachment-input-object-string
                (disco-room--make-attachment-input-object "/tmp/a.txt"))))
      (disco-room--composer-enter-edit
       '((id . "m1") (content . "||server||")))
      (should (eq 'discord-markdown appkit-markup-compose-active-codec))
      (let* ((capture (appkit-markup-compose-capture))
             (output
              (appkit-markup-compose-output capture 'discord-markdown)))
        (should (equal "||server||"
                       (appkit-markup-compose-output-source output))))
      (should (disco-room--composer-edit-clear t))
      (should (eq 'org appkit-markup-compose-active-codec))
      (should (appkit-chatbuf-string-has-objects-p
               (disco-room--current-draft))))))

;;; disco-room-compose-test.el ends here
