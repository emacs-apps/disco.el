;;; disco-settings.el --- Discord account expression settings -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Lossless protobuf field storage and read-only projections for Discord's
;; private frecency-and-favorites settings (settings-proto type 2).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'disco-api)

(defconst disco-settings--frecency-type 2)

(defvar disco-settings--fields nil
  "Raw parsed top-level protobuf fields for settings-proto type 2.")

(defvar disco-settings--loaded-p nil
  "Non-nil after a complete type-2 settings snapshot has been loaded.")

(defvar disco-settings--loading-p nil
  "Non-nil while a type-2 settings request is in flight.")

(defvar disco-settings--load-owner nil
  "Exact in-flight settings request owner and completion listeners.")


(defun disco-settings--read-varint (bytes position limit)
  "Read one protobuf varint from BYTES at POSITION before LIMIT.
Return (VALUE . NEXT-POSITION), or signal an error for malformed input."
  (let ((value 0)
        (shift 0)
        byte)
    (catch 'done
      (while (< position limit)
        (setq byte (aref bytes position)
              position (1+ position)
              value (logior value (ash (logand byte #x7f) shift)))
        (when (zerop (logand byte #x80))
          (throw 'done (cons value position)))
        (setq shift (+ shift 7))
        (when (> shift 63)
          (error "disco: malformed protobuf varint")))
      (error "disco: truncated protobuf varint"))))

(defun disco-settings--parse-fields (bytes)
  "Parse protobuf BYTES into lossless field plists.
Each field retains its number, wire type, decoded value, and exact raw bytes."
  (setq bytes (encode-coding-string bytes 'binary t))
  (let ((position 0)
        (limit (length bytes))
        fields)
    (while (< position limit)
      (let* ((start position)
             (key-result (disco-settings--read-varint bytes position limit))
             (key (car key-result))
             (number (ash key -3))
             (wire-type (logand key 7))
             value
             end)
        (setq position (cdr key-result))
        (when (zerop number)
          (error "disco: invalid protobuf field number 0"))
        (pcase wire-type
          (0
           (let ((result
                  (disco-settings--read-varint bytes position limit)))
             (setq value (car result)
                   end (cdr result))))
          (1
           (setq end (+ position 8))
           (when (> end limit)
             (error "disco: truncated protobuf fixed64 field"))
           (setq value (substring bytes position end)))
          (2
           (let* ((length-result
                   (disco-settings--read-varint bytes position limit))
                  (length (car length-result))
                  (body-start (cdr length-result)))
             (setq end (+ body-start length))
             (when (> end limit)
               (error "disco: truncated protobuf message field"))
             (setq value (substring bytes body-start end))))
          (5
           (setq end (+ position 4))
           (when (> end limit)
             (error "disco: truncated protobuf fixed32 field"))
           (setq value (substring bytes position end)))
          (_
           (error "disco: unsupported protobuf wire type %s" wire-type)))
        (push (list :number number
                    :wire-type wire-type
                    :value value
                    :raw (substring bytes start end))
              fields)
        (setq position end)))
    (nreverse fields)))

(defun disco-settings--message-field (fields number)
  "Return the last length-delimited field NUMBER from FIELDS."
  (let (value)
    (dolist (field fields value)
      (when (and (= (plist-get field :number) number)
                 (= (plist-get field :wire-type) 2))
        (setq value (plist-get field :value))))))

(defun disco-settings--varint-field (fields number)
  "Return the last varint field NUMBER from FIELDS."
  (let (value)
    (dolist (field fields value)
      (when (and (= (plist-get field :number) number)
                 (= (plist-get field :wire-type) 0))
        (setq value (plist-get field :value))))))

(defun disco-settings--decode-string (bytes)
  "Decode protobuf UTF-8 string BYTES."
  (decode-coding-string bytes 'utf-8 t))

(defun disco-settings--signed-int32 (value)
  "Interpret protobuf integer VALUE as a signed 32-bit integer."
  (let ((low (logand value #xffffffff)))
    (if (>= low #x80000000)
        (- low #x100000000)
      low)))

(defun disco-settings--decode-fixed64 (bytes)
  "Decode one little-endian protobuf fixed64 value from BYTES."
  (unless (= (length bytes) 8)
    (error "disco: malformed protobuf fixed64 value"))
  (let ((value 0)
        (index 0))
    (while (< index 8)
      (setq value
            (logior value
                    (ash (aref bytes index) (* index 8)))
            index (1+ index)))
    value))

(defun disco-settings--fixed64-field (fields number)
  "Return the last fixed64 field NUMBER from FIELDS."
  (let (value)
    (dolist (field fields value)
      (when (and (= (plist-get field :number) number)
                 (= (plist-get field :wire-type) 1))
        (setq value
              (disco-settings--decode-fixed64
               (plist-get field :value)))))))

(defun disco-settings--packed-fixed64s (bytes)
  "Decode packed little-endian fixed64 values from BYTES."
  (unless (zerop (% (length bytes) 8))
    (error "disco: malformed packed fixed64 values"))
  (let (values)
    (while (> (length bytes) 0)
      (push (disco-settings--decode-fixed64 (substring bytes 0 8))
            values)
      (setq bytes (substring bytes 8)))
    (nreverse values)))

(defun disco-settings--packed-varints (bytes)
  "Decode packed varints from BYTES."
  (let ((position 0)
        (limit (length bytes))
        values)
    (while (< position limit)
      (let ((result (disco-settings--read-varint bytes position limit)))
        (push (car result) values)
        (setq position (cdr result))))
    (nreverse values)))

(defun disco-settings--frecency-item (bytes)
  "Decode one FrecencyItem message from BYTES."
  (let* ((fields (disco-settings--parse-fields bytes))
         (total-uses (or (disco-settings--varint-field fields 1) 0))
         recent-uses)
    (dolist (field fields)
      (when (= (plist-get field :number) 2)
        (pcase (plist-get field :wire-type)
          (0 (push (plist-get field :value) recent-uses))
          (2 (setq recent-uses
                   (nconc (nreverse
                           (disco-settings--packed-varints
                            (plist-get field :value)))
                          recent-uses))))))
    (list :total-uses total-uses
          :recent-uses (nreverse recent-uses)
          :frecency (disco-settings--signed-int32
                     (or (disco-settings--varint-field fields 3) 0))
          :score (disco-settings--signed-int32
                  (or (disco-settings--varint-field fields 4) 0)))))

(defun disco-settings--frecency-map (field-number)
  "Return decoded frecency map from top-level FIELD-NUMBER."
  (let ((message (disco-settings--message-field
                  disco-settings--fields field-number))
        entries)
    (when message
      (dolist (field (disco-settings--parse-fields message))
        (when (and (= (plist-get field :number) 1)
                   (= (plist-get field :wire-type) 2))
          (let* ((entry-fields
                  (disco-settings--parse-fields (plist-get field :value)))
                 (key-bytes (disco-settings--message-field entry-fields 1))
                 (value-bytes (disco-settings--message-field entry-fields 2))
                 (key (and key-bytes
                           (disco-settings--decode-string key-bytes))))
            (when (and (stringp key) value-bytes)
              (setf (alist-get key entries nil nil #'equal)
                    (disco-settings--frecency-item value-bytes)))))))
    (copy-tree entries)))
(defun disco-settings--fixed64-frecency-map (field-number)
  "Return fixed64-keyed frecency map from top-level FIELD-NUMBER."
  (let ((message
         (disco-settings--message-field disco-settings--fields field-number))
        entries)
    (when message
      (dolist (field (disco-settings--parse-fields message))
        (when (and (= (plist-get field :number) 1)
                   (= (plist-get field :wire-type) 2))
          (let* ((entry-fields
                  (disco-settings--parse-fields (plist-get field :value)))
                 (key (disco-settings--fixed64-field entry-fields 1))
                 (value-bytes
                  (disco-settings--message-field entry-fields 2)))
            (when (and key value-bytes)
              (setf (alist-get (number-to-string key)
                               entries nil nil #'equal)
                    (disco-settings--frecency-item value-bytes)))))))
    (copy-tree entries)))

(defun disco-settings-favorite-emojis ()
  "Return current account favorite emoji identities in server order."
  (let ((message (disco-settings--message-field disco-settings--fields 5))
        favorites)
    (when message
      (dolist (field (disco-settings--parse-fields message))
        (when (and (= (plist-get field :number) 1)
                   (= (plist-get field :wire-type) 2))
          (push (disco-settings--decode-string (plist-get field :value))
                favorites))))
    (nreverse favorites)))

(defun disco-settings-favorite-stickers ()
  "Return current account favorite sticker IDs in server order."
  (let ((message (disco-settings--message-field disco-settings--fields 3))
        favorites)
    (when message
      (dolist (field (disco-settings--parse-fields message))
        (when (= (plist-get field :number) 1)
          (setq favorites
                (append
                 favorites
                 (pcase (plist-get field :wire-type)
                   (1
                    (list
                     (disco-settings--decode-fixed64
                      (plist-get field :value))))
                   (2
                    (disco-settings--packed-fixed64s
                     (plist-get field :value)))
                   (_ nil)))))))
    (mapcar #'number-to-string favorites)))

(defun disco-settings-sticker-frecency ()
  "Return a copy of current account sticker frecency entries."
  (disco-settings--fixed64-frecency-map 4))

(defun disco-settings-emoji-frecency ()
  "Return a copy of current account composer emoji frecency entries."
  (disco-settings--frecency-map 6))

(defun disco-settings-reaction-frecency ()
  "Return a copy of current account reaction emoji frecency entries."
  (disco-settings--frecency-map 13))

(defun disco-settings-data-version ()
  "Return current settings data version, or nil when absent."
  (when-let* ((versions
              (disco-settings--message-field disco-settings--fields 1)))
    (disco-settings--varint-field
     (disco-settings--parse-fields versions) 3)))

(defun disco-settings-loaded-p ()
  "Return non-nil after a complete account emoji settings load."
  disco-settings--loaded-p)

(defun disco-settings--merge-partial-fields (fields)
  "Merge protobuf patch FIELDS into the retained top-level snapshot."
  (let ((numbers (delete-dups
                  (mapcar (lambda (field) (plist-get field :number)) fields))))
    (setq disco-settings--fields
          (append
           (seq-remove
            (lambda (field)
              (memq (plist-get field :number) numbers))
            disco-settings--fields)
           fields))))

(defun disco-settings-apply-base64 (encoded &optional partial)
  "Apply base64 ENCODED type-2 settings protobuf.
When PARTIAL is non-nil, replace only top-level fields present in the payload."
  (unless (stringp encoded)
    (error "disco: settings protobuf must be base64 text"))
  (let ((fields (disco-settings--parse-fields
                 (base64-decode-string encoded))))
    (if partial
        (disco-settings--merge-partial-fields fields)
      (setq disco-settings--fields fields
            disco-settings--loaded-p t))))

(defun disco-settings--finish-load (owner success value)
  "Finish settings load OWNER, notifying listeners with SUCCESS and VALUE."
  (when (eq owner disco-settings--load-owner)
    (setq disco-settings--load-owner nil
          disco-settings--loading-p nil)
    (dolist (listener (nreverse (plist-get owner :listeners)))
      (when-let* ((callback (if success (car listener) (cdr listener))))
        (condition-case nil
            (funcall callback value)
          ((error quit) nil))))))

(defun disco-settings-apply-gateway-update (payload)
  "Apply USER_SETTINGS_PROTO_UPDATE PAYLOAD when it targets type 2."
  (let* ((settings (and (listp payload) (alist-get 'settings payload)))
         (type (and (listp settings) (alist-get 'type settings)))
         (encoded (and (listp settings) (alist-get 'proto settings)))
         (partial (eq (alist-get 'partial payload) t)))
    (when (and (= (or type -1) disco-settings--frecency-type)
               (stringp encoded))
      (condition-case err
          (progn
            (disco-settings-apply-base64 encoded partial)
            (unless partial
              (when-let* ((owner disco-settings--load-owner))
                (disco-settings--finish-load owner t nil))))
        (error
         (message "disco: ignored malformed expression settings update: %s"
                  (error-message-string err)))))))

(defun disco-settings-ensure-loaded (&optional on-success on-error)
  "Ensure account expression settings, then call ON-SUCCESS or ON-ERROR."
  (cond
   (disco-settings--loaded-p
    (when on-success (funcall on-success nil)))
   (disco-settings--load-owner
    (when (or on-success on-error)
      (push (cons on-success on-error)
            (plist-get disco-settings--load-owner :listeners))))
   (t
    (let ((owner
           (list :listeners
                 (and (or on-success on-error)
                      (list (cons on-success on-error))))))
      (setq disco-settings--load-owner owner
            disco-settings--loading-p t)
      (condition-case err
          (disco-api-user-settings-proto-async
           disco-settings--frecency-type
           :on-success
           (lambda (response)
             (when (eq owner disco-settings--load-owner)
               (condition-case decode-error
                   (progn
                     (disco-settings-apply-base64
                      (alist-get 'settings response))
                     (disco-settings--finish-load owner t nil))
                 (error
                  (let ((reason (error-message-string decode-error)))
                    (disco-settings--finish-load owner nil reason)
                    (message "disco: could not decode expression settings: %s"
                             reason))))))
           :on-error
           (lambda (reason)
             (disco-settings--finish-load owner nil reason)))
        ((error quit)
         (disco-settings--finish-load
          owner nil (error-message-string err))))))))

(defun disco-settings-reset ()
  "Forget account-scoped expression settings and retire pending loads."
  (setq disco-settings--fields nil
        disco-settings--loaded-p nil
        disco-settings--loading-p nil
        disco-settings--load-owner nil))

(provide 'disco-settings)

;;; disco-settings.el ends here
