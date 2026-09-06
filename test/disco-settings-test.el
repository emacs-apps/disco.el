;;; disco-settings-test.el --- Tests for account emoji settings -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-settings)

(defun disco-settings-test--varint (value)
  "Encode unsigned protobuf varint VALUE as a unibyte string."
  (let (bytes)
    (while (> value #x7f)
      (push (logior #x80 (logand value #x7f)) bytes)
      (setq value (ash value -7)))
    (push value bytes)
    (apply #'unibyte-string (nreverse bytes))))

(defun disco-settings-test--fixed64 (value)
  "Encode unsigned fixed64 VALUE as little-endian bytes."
  (let (bytes)
    (dotimes (index 8)
      (push (logand #xff (ash value (- (* index 8)))) bytes))
    (apply #'unibyte-string (nreverse bytes))))

(defun disco-settings-test--field (number wire-type value)
  "Encode protobuf field NUMBER with WIRE-TYPE and VALUE."
  (concat
   (disco-settings-test--varint (logior (ash number 3) wire-type))
   (pcase wire-type
     (0 (disco-settings-test--varint value))
     (1 value)
     (2 (concat (disco-settings-test--varint (length value)) value)))))

(defun disco-settings-test--message-field (number payload)
  "Encode length-delimited field NUMBER containing PAYLOAD."
  (disco-settings-test--field number 2 payload))

(defun disco-settings-test--string-field (number value)
  "Encode UTF-8 string VALUE as field NUMBER."
  (disco-settings-test--message-field
   number (encode-coding-string value 'utf-8 t)))

(defun disco-settings-test--frecency-map (key score)
  "Encode one frecency map entry for KEY with SCORE."
  (let* ((item
          (concat
           (disco-settings-test--field 1 0 4)
           (disco-settings-test--message-field
            2
            (concat (disco-settings-test--varint 1000)
                    (disco-settings-test--varint 2000)))
           (disco-settings-test--field 3 0 (1- (ash 1 64)))
           (disco-settings-test--field 4 0 score)))
         (entry
          (concat (disco-settings-test--string-field 1 key)
                  (disco-settings-test--message-field 2 item))))
    (disco-settings-test--message-field 1 entry)))

(ert-deftest disco-settings-decodes-favorites-and-reaction-frecency ()
  (unwind-protect
      (let* ((versions (disco-settings-test--field 3 0 7))
             (favorites
              (concat (disco-settings-test--string-field 1 "101")
                      (disco-settings-test--string-field
                       1 "skull_crossbones")))
             (payload
              (concat
               (disco-settings-test--message-field 1 versions)
               (disco-settings-test--message-field 5 favorites)
               (disco-settings-test--message-field
                13 (disco-settings-test--frecency-map "101" 12))
               (disco-settings-test--message-field 15 "unknown"))))
        (disco-settings-reset)
        (disco-settings-apply-base64 (base64-encode-string payload t))
        (should (disco-settings-loaded-p))
        (should (= 7 (disco-settings-data-version)))
        (should (equal '("101" "skull_crossbones")
                       (disco-settings-favorite-emojis)))
        (let ((item (cdr (assoc "101"
                                (disco-settings-reaction-frecency)))))
          (should (= 4 (plist-get item :total-uses)))
          (should (equal '(1000 2000) (plist-get item :recent-uses)))
          (should (= -1 (plist-get item :frecency)))
          (should (= 12 (plist-get item :score))))
        (should (member 15
                        (mapcar
                         (lambda (field) (plist-get field :number))
                         disco-settings--fields))))
    (disco-settings-reset)))

(ert-deftest disco-settings-partial-update-preserves-unmentioned-fields ()
  (unwind-protect
      (let* ((full
              (concat
               (disco-settings-test--message-field
                5 (disco-settings-test--string-field 1 "101"))
               (disco-settings-test--message-field
                13 (disco-settings-test--frecency-map "101" 9))))
             (partial
              (disco-settings-test--message-field
               5 (disco-settings-test--string-field 1 "202"))))
        (disco-settings-reset)
        (disco-settings-apply-base64 (base64-encode-string full t))
        (disco-settings-apply-base64
         (base64-encode-string partial t) t)
        (should (equal '("202") (disco-settings-favorite-emojis)))
        (should (= 9
                   (plist-get
                    (cdr (assoc "101"
                                (disco-settings-reaction-frecency)))
                    :score))))
    (disco-settings-reset)))

(ert-deftest disco-settings-coalesces-load-and-retires-reset-callback ()
  (let (success (calls 0))
    (disco-settings-reset)
    (cl-letf (((symbol-function 'disco-api-user-settings-proto-async)
               (lambda (_type &rest options)
                 (cl-incf calls)
                 (setq success (plist-get options :on-success)))))
      (disco-settings-ensure-loaded)
      (disco-settings-ensure-loaded)
      (should (= 1 calls))
      (disco-settings-reset)
      (funcall success `((settings . ,(base64-encode-string "" t))))
      (should-not (disco-settings-loaded-p)))))

(ert-deftest disco-settings-gateway-json-false-means-full-snapshot ()
  (unwind-protect
      (progn
        (disco-settings-reset)
        (disco-settings-apply-gateway-update
         `((settings
            . ((type . 2)
               (proto . ,(base64-encode-string "" t))))
           (partial . :false)))
        (should (disco-settings-loaded-p)))
    (disco-settings-reset)))

(ert-deftest disco-settings-malformed-payload-does-not-replace-snapshot ()
  (unwind-protect
      (progn
        (disco-settings-reset)
        (disco-settings-apply-base64
         (base64-encode-string
          (disco-settings-test--message-field
           5 (disco-settings-test--string-field 1 "101"))
          t))
        (should-error
         (disco-settings-apply-base64
          (base64-encode-string (unibyte-string #x2a #x05 #x41) t)))
        (should (equal '("101") (disco-settings-favorite-emojis))))
    (disco-settings-reset)))

(ert-deftest disco-settings-decodes-sticker-favorites-and-frecency ()
  (unwind-protect
      (let* ((favorite-list
              (disco-settings-test--message-field
               1
               (concat
                (disco-settings-test--fixed64 700)
                (disco-settings-test--fixed64 800))))
             (frecency-value
              (concat
               (disco-settings-test--field 1 0 6)
               (disco-settings-test--field 2 0 10)
               (disco-settings-test--field 2 0 20)
               (disco-settings-test--field 3 0 11)
               (disco-settings-test--field 4 0 17)))
             (frecency-entry
              (concat
               (disco-settings-test--field
                1 1 (disco-settings-test--fixed64 700))
               (disco-settings-test--message-field 2 frecency-value)))
             (payload
              (concat
               (disco-settings-test--message-field 3 favorite-list)
               (disco-settings-test--message-field
                4
                (disco-settings-test--message-field 1 frecency-entry)))))
        (disco-settings-reset)
        (disco-settings-apply-base64 (base64-encode-string payload t))
        (should (equal (disco-settings-favorite-stickers) '("700" "800")))
        (should
         (equal
          (disco-settings-sticker-frecency)
          '(("700" :total-uses 6 :recent-uses (10 20)
             :frecency 11 :score 17)))))
    (disco-settings-reset)))

;;; disco-settings-test.el ends here
