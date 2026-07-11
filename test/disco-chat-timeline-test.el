;;; disco-chat-timeline-test.el --- Tests for projected chat timelines -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'disco-chat-timeline)

(defun disco-chat-timeline-test--printer (prints)
  "Return a row printer recording render counts in PRINTS."
  (lambda (row)
    (let* ((key (disco-chat-timeline-row-key row))
           (payload (disco-chat-timeline-row-payload row))
           (context (disco-chat-timeline-row-context row))
           (start (point)))
      (puthash key (1+ (gethash key prints 0)) prints)
      (insert (format "%s:%s:%s\n"
                      key payload (or (plist-get context :layout) "plain")))
      (add-text-properties start (point) (list 'test-message-key key)))))

(defun disco-chat-timeline-test--row (key payload &optional context dependencies)
  "Create one test row from KEY, PAYLOAD, CONTEXT, and DEPENDENCIES."
  (disco-chat-timeline-row-create
   :key key
   :payload payload
   :context context
   :dependencies dependencies))

(ert-deftest disco-chat-timeline-projects-context-and-dependencies ()
  (let ((rows
         (disco-chat-timeline-project
          '((a . "one") (b . "two"))
          #'car
          :context-function
          (lambda (previous current)
            (list :previous (car-safe previous)
                  :current (car current)))
          :dependencies-function
          (lambda (entry)
            (list (list :source (cdr entry)))))))
    (should (equal '(a b) (mapcar #'disco-chat-timeline-row-key rows)))
    (should (equal '(:previous a :current b)
                   (disco-chat-timeline-row-context (cadr rows))))
    (should (equal '((:source "two"))
                   (disco-chat-timeline-row-dependencies (cadr rows))))))

(ert-deftest disco-chat-timeline-sync-is-keyed-and-context-sensitive ()
  (with-temp-buffer
    (let ((prints (make-hash-table :test #'equal)))
      (disco-chat-timeline-ensure
       :printer (disco-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row 'a "A")
             (disco-chat-timeline-test--row 'b "B")))
      (let ((a-node (disco-chat-timeline-node 'a))
            (b-node (disco-chat-timeline-node 'b)))
        (disco-chat-timeline-sync
         (list (disco-chat-timeline-test--row 'a "A")
               (disco-chat-timeline-test--row 'b "B" '(:layout compact))))
        (should (eq a-node (disco-chat-timeline-node 'a)))
        (should (eq b-node (disco-chat-timeline-node 'b)))
        (should (= 1 (gethash 'a prints)))
        (should (= 2 (gethash 'b prints)))
        (should (equal '(a b) (disco-chat-timeline-keys)))))))

(ert-deftest disco-chat-timeline-sync-handles-arbitrary-reordering ()
  (with-temp-buffer
    (let ((prints (make-hash-table :test #'equal)))
      (disco-chat-timeline-ensure
       :printer (disco-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row 'a "A")
             (disco-chat-timeline-test--row 'b "B")
             (disco-chat-timeline-test--row 'c "C")))
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row 'c "C")
             (disco-chat-timeline-test--row 'a "A")
             (disco-chat-timeline-test--row 'd "D")))
      (should (equal '(c a d) (disco-chat-timeline-keys)))
      (should-not (disco-chat-timeline-node 'b))
      (should (string-match-p "c:C:plain\na:A:plain\nd:D:plain"
                              (buffer-string))))))

(ert-deftest disco-chat-timeline-invalidates-old-and-new-resource-dependents ()
  (with-temp-buffer
    (let ((prints (make-hash-table :test #'equal))
          (resource '(:message "source")))
      (disco-chat-timeline-ensure
       :printer (disco-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row 'source "source")
             (disco-chat-timeline-test--row 'reply "reply" nil
                                             (list resource))))
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row 'source "updated")
             (disco-chat-timeline-test--row 'reply "reply" nil
                                             (list resource)))
       :changed-resources (list resource))
      (should (= 2 (gethash 'source prints)))
      (should (= 2 (gethash 'reply prints)))
      (should (equal '(reply)
                     (disco-chat-timeline-dependent-keys (list resource)))))))

(ert-deftest disco-chat-timeline-rekey-preserves-node-and-semantic-point ()
  (with-temp-buffer
    (let ((prints (make-hash-table :test #'equal)))
      (disco-chat-timeline-ensure
       :printer (disco-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row "local-1" "pending")))
      (let ((node (disco-chat-timeline-node "local-1")))
        (goto-char (disco-chat-timeline-key-position "local-1"))
        (move-to-column 3)
        (disco-chat-timeline-sync
         (list (disco-chat-timeline-test--row
                "7467703692092974645" "sent"))
         :rekeys '(("local-1" . "7467703692092974645")))
        (should (eq node
                    (disco-chat-timeline-node "7467703692092974645")))
        (should-not (disco-chat-timeline-node "local-1"))
        (should (equal "7467703692092974645"
                       (disco-chat-timeline-key-at-point)))
        (should (= 3 (current-column)))))))

(ert-deftest disco-chat-timeline-frame-update-preserves-composer-and-undo ()
  (with-temp-buffer
    (let ((prints (make-hash-table :test #'equal)))
      (disco-chatbuf-init-state 8)
      (disco-chat-timeline-ensure
       :printer (disco-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key
       :header "old header\n"
       :footer "old footer\n")
      (disco-chat-timeline-sync
       (list (disco-chat-timeline-test--row 'a "A")))
      (disco-chat-timeline-set-frame
       "old header\n" "old footer\n"
       :bind-input-function
       (lambda ()
         (disco-chatbuf-bind-input-region
          :visible-p t :prompt ">>> " :input-text "draft")))
      (goto-char (+ (disco-chatbuf-input-start-position) 2))
      (setq buffer-undo-list nil)
      (let ((input-marker disco-chatbuf--input-marker)
            (prompt-marker disco-chatbuf--prompt-marker))
        (disco-chat-timeline-set-frame
         "new header\n" "new footer\n"
         :bind-input-function
         (lambda ()
           (disco-chatbuf-bind-input-region
            :visible-p t :prompt "qq> " :input-text "draft")))
        (should (eq input-marker disco-chatbuf--input-marker))
        (should (eq prompt-marker disco-chatbuf--prompt-marker))
        (should (= 2 (- (point) (disco-chatbuf-input-start-position))))
        (should (equal "draft" (disco-chatbuf-input-string)))
        (should-not buffer-undo-list)))))

(ert-deftest disco-chat-timeline-rejects-invalid-projections-before-mutation ()
  (with-temp-buffer
    (let ((prints (make-hash-table :test #'equal)))
      (disco-chat-timeline-ensure
       :printer (disco-chat-timeline-test--printer prints)
       :anchor-property 'test-message-key)
      (should-error
       (disco-chat-timeline-sync
        (list (disco-chat-timeline-test--row 'same "one")
              (disco-chat-timeline-test--row 'same "two"))))
      (should-not (disco-chat-timeline-keys)))))

(provide 'disco-chat-timeline-test)

;;; disco-chat-timeline-test.el ends here
