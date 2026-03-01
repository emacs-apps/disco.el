;;; disco-embed.el --- Embed rendering component for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Telega-inspired embed rendering extracted from room timeline logic.

;;; Code:

(require 'subr-x)
(require 'browse-url)
(require 'disco-ui)

(declare-function disco-room--object-get "disco-room" (object &rest keys))
(declare-function disco-room--embed-summary "disco-room" (embed))
(declare-function disco-room--embed-meta-line "disco-room" (embed))
(declare-function disco-room--embed-author-object "disco-room" (embed))
(declare-function disco-room--embed-provider-object "disco-room" (embed))
(declare-function disco-room--embed-author-url "disco-room" (msg embed))
(declare-function disco-room--embed-provider-url "disco-room" (msg embed))
(declare-function disco-room--embed-author-icon-url "disco-room" (msg embed))
(declare-function disco-room--embed-author-icon-image "disco-room" (msg embed embed-index))
(declare-function disco-room--embed-main-url "disco-room" (msg embed))
(declare-function disco-room--embed-media-entry "disco-room" (embed))
(declare-function disco-room--embed-media-url "disco-room" (msg embed))
(declare-function disco-room--embed-preview-attachment "disco-room" (msg embed embed-index))
(declare-function disco-room--attachment-preview-image "disco-room" (attachment &optional for-display))
(declare-function disco-room--format-time "disco-room" (timestamp))
(declare-function disco-room--inline-image-rendering-available-p "disco-room" ())
(declare-function disco-room--json-true-p "disco-room" (value))

(defvar disco-room-show-embeds)
(defvar disco-room-use-rich-embed-cards)
(defvar disco-room-show-embed-urls)
(defvar disco-room-show-embed-image-previews)
(defvar disco-room-embed-description-limit)

(defun disco-embed--url-present-p (url)
  "Return non-nil when URL is a non-empty string."
  (and (stringp url) (not (string-empty-p url))))

(defun disco-embed--normalize-text (value)
  "Normalize VALUE into trimmed non-empty string, or nil."
  (let ((text (and value (string-trim (format "%s" value)))))
    (when (and (stringp text) (not (string-empty-p text)))
      text)))

(defun disco-embed--truncate-text (text limit)
  "Truncate TEXT to LIMIT characters with ellipsis when needed."
  (cond
   ((not (and (stringp text) (not (string-empty-p text))))
    nil)
   ((eq limit 0)
    nil)
   ((and (integerp limit)
         (> limit 0)
         (> (length text) limit))
    (concat (substring text 0 limit) "..."))
   (t text)))

(defun disco-embed--description-line (embed)
  "Return one-line embed description for EMBED, respecting user limit." 
  (let* ((raw (disco-room--object-get embed 'description))
         (flat (and raw
                    (replace-regexp-in-string "[\n\r\t]+" " "
                                              (format "%s" raw))))
         (text (disco-embed--normalize-text flat)))
    (disco-embed--truncate-text text disco-room-embed-description-limit)))

(defun disco-embed--insert-action-button (label callback help-echo)
  "Insert one compact action button." 
  (disco-ui-insert-action-button
   label
   callback
   :face 'disco-room-embed-card-action
   :help-echo help-echo))

(defun disco-embed--insert-field-row (field)
  "Insert one field row for FIELD object." 
  (let* ((name (disco-embed--normalize-text
                (disco-room--object-get field 'name)))
         (raw-value (disco-room--object-get field 'value))
         (value (disco-embed--normalize-text raw-value))
         (inline (disco-room--json-true-p (disco-room--object-get field 'inline))))
    (when (or name value)
      (let ((field-start (point)))
        (insert "    | ")
        (insert (if name name "(unnamed field)"))
        (when inline
          (insert " [inline]"))
        (when value
          (insert ": ")
          (insert value))
        (insert "\n")
        (add-text-properties field-start (point)
                             '(face disco-room-embed-card-meta))))))

(defun disco-embed--insert-preview-row (msg embed embed-index media-kind media-url)
  "Insert media preview row for EMBED in MSG." 
  (let* ((preview-rendering-available
          (and disco-room-show-embed-image-previews
               (disco-room--inline-image-rendering-available-p)))
         (preview-attachment (disco-room--embed-preview-attachment msg embed embed-index))
         (preview (and preview-attachment
                       preview-rendering-available
                       (disco-room--attachment-preview-image preview-attachment t)))
         (preview-start (point)))
    (insert "    | ")
    (cond
     ((memq media-kind '(image thumbnail))
      (if preview
          (condition-case _
              (insert-image preview "[image]")
            (error
             (insert "[image unavailable]")))
        (cond
         ((not preview-rendering-available)
          (insert "[preview disabled]"))
         ((not (disco-embed--url-present-p media-url))
          (insert "[no preview URL]"))
         (t
          (insert "[loading preview]")))))
     ((eq media-kind 'video)
      (insert "[video embed]"))
     (t
      (insert "[no preview]")))
    (insert "\n")
    (add-text-properties preview-start (point)
                         '(face disco-room-embed-card-meta))))

(defun disco-embed--insert-action-row (main-url media-url author-url provider-url
                                                author-icon-url)
  "Insert compact action buttons row for one embed." 
  (let ((actions '()))
    (when (disco-embed--url-present-p main-url)
      (push (list "[Open]"
                  (lambda () (browse-url main-url t))
                  "Open embed URL")
            actions)
      (push (list "[Copy]"
                  (lambda ()
                    (kill-new main-url)
                    (message "disco: copied embed URL"))
                  "Copy embed URL")
            actions))
    (when (and (disco-embed--url-present-p media-url)
               (not (equal media-url main-url)))
      (push (list "[Media]"
                  (lambda () (browse-url media-url t))
                  "Open embed media URL")
            actions))
    (when (and (disco-embed--url-present-p author-url)
               (not (equal author-url main-url)))
      (push (list "[Author]"
                  (lambda () (browse-url author-url t))
                  "Open embed author URL")
            actions))
    (when (and (disco-embed--url-present-p provider-url)
               (not (equal provider-url main-url))
               (not (equal provider-url author-url)))
      (push (list "[Provider]"
                  (lambda () (browse-url provider-url t))
                  "Open embed provider URL")
            actions))
    (when (and (disco-embed--url-present-p author-icon-url)
               (not (equal author-icon-url main-url)))
      (push (list "[Icon]"
                  (lambda () (browse-url author-icon-url t))
                  "Open embed author icon URL")
            actions))
    (when actions
      (let ((action-start (point))
            (first t))
        (insert "    | ")
        (dolist (action (nreverse actions))
          (unless first
            (insert " "))
          (setq first nil)
          (disco-embed--insert-action-button
           (nth 0 action)
           (nth 1 action)
           (nth 2 action)))
        (insert "\n")
        (add-text-properties action-start (point)
                             '(face disco-room-embed-card-meta))))))

(defun disco-embed-insert-card (msg embed embed-index)
  "Insert one telega-inspired rich embed card for EMBED from MSG." 
  (let* ((summary (disco-room--embed-summary embed))
         (meta-fallback (disco-room--embed-meta-line embed))
         (description (disco-embed--description-line embed))
         (fields (or (disco-room--object-get embed 'fields) '()))
         (author (disco-room--embed-author-object embed))
         (author-name (and (listp author)
                           (disco-embed--normalize-text
                            (disco-room--object-get author 'name))))
         (author-url (disco-room--embed-author-url msg embed))
         (author-icon-url (disco-room--embed-author-icon-url msg embed))
         (author-icon-image (disco-room--embed-author-icon-image msg embed embed-index))
         (provider (disco-room--embed-provider-object embed))
         (provider-name (and (listp provider)
                             (disco-embed--normalize-text
                              (disco-room--object-get provider 'name))))
         (provider-url (disco-room--embed-provider-url msg embed))
         (main-url (disco-room--embed-main-url msg embed))
         (timestamp (disco-room--object-get embed 'timestamp))
         (timestamp-text (and (stringp timestamp)
                              (not (string-empty-p timestamp))
                              (disco-room--format-time timestamp)))
         (footer (disco-room--object-get embed 'footer))
         (footer-text (and (listp footer)
                           (disco-embed--normalize-text
                            (disco-room--object-get footer 'text))))
         (footer-line (let ((parts (delq nil (list footer-text timestamp-text))))
                        (and parts (string-join parts "  -  "))))
         (media-entry (disco-room--embed-media-entry embed))
         (media-kind (car media-entry))
         (media (cdr media-entry))
         (media-url (disco-room--embed-media-url msg embed))
         (media-width (and (listp media) (disco-room--object-get media 'width)))
         (media-height (and (listp media) (disco-room--object-get media 'height)))
         (media-dims (when (and (numberp media-width) (numberp media-height))
                       (format "%dx%d" media-width media-height)))
         (meta-parts (delq nil
                           (list provider-name
                                 (and author-name
                                      (not (equal author-name provider-name))
                                      author-name)
                                 timestamp-text))))
    (let ((top-start (point)))
      (insert "    +-- " summary "\n")
      (add-text-properties top-start (point)
                           '(face disco-room-embed-card-title)))
    (let ((meta-start (point)))
      (insert "    | ")
      (when author-icon-image
        (condition-case _
            (insert-image author-icon-image "[icon]")
          (error
           (insert "[icon]")))
        (insert " "))
      (insert (if meta-parts
                  (string-join meta-parts "  -  ")
                meta-fallback))
      (when media-dims
        (insert "  [" media-dims "]"))
      (insert "\n")
      (add-text-properties meta-start (point)
                           '(face disco-room-embed-card-meta)))
    (when description
      (let ((desc-start (point)))
        (insert "    | " description "\n")
        (add-text-properties desc-start (point)
                             '(face disco-room-embed-card-meta))))
    (when media-entry
      (disco-embed--insert-preview-row msg embed embed-index media-kind media-url))
    (dolist (field fields)
      (disco-embed--insert-field-row field))
    (when footer-line
      (let ((footer-start (point)))
        (insert "    | " footer-line "\n")
        (add-text-properties footer-start (point)
                             '(face disco-room-embed-card-meta))))
    (disco-embed--insert-action-row main-url media-url author-url provider-url
                                    author-icon-url)
    (let ((bottom-start (point)))
      (insert "    +--\n")
      (add-text-properties bottom-start (point)
                           '(face disco-room-embed-card-border)))
    (when disco-room-show-embed-urls
      (dolist (raw-url (delete-dups (delq nil (list main-url media-url author-url
                                                    provider-url author-icon-url))))
        (when (disco-embed--url-present-p raw-url)
          (let ((url-start (point)))
            (insert "      " raw-url "\n")
            (add-text-properties url-start (point) '(face shadow))))))))

(defun disco-embed-insert-message-embeds (msg)
  "Insert embed detail lines for MSG." 
  (when disco-room-show-embeds
    (let ((embed-index 0))
      (dolist (embed (or (alist-get 'embeds msg) '()))
        (setq embed-index (1+ embed-index))
        (condition-case _
            (if disco-room-use-rich-embed-cards
                (disco-embed-insert-card msg embed embed-index)
              (let* ((line-start (point))
                     (url (disco-room--embed-main-url msg embed)))
                (insert "    " (disco-room--embed-summary embed) "\n")
                (add-text-properties line-start (point) '(face disco-room-message-meta))
                (when (and disco-room-show-embed-urls
                           (disco-embed--url-present-p url))
                  (let ((url-start (point)))
                    (insert "      " url "\n")
                    (add-text-properties url-start (point) '(face shadow))))))
          (error
           (let ((line-start (point)))
             (insert "    [embed] [render fallback]\n")
             (add-text-properties line-start (point) '(face shadow)))))))))

(provide 'disco-embed)

;;; disco-embed.el ends here
