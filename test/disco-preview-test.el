;;; disco-preview-test.el --- Tests for shared preview hydration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(require 'disco-preview)

(defmacro disco-preview-test--with-state (&rest body)
  "Run BODY with isolated preview lifecycle state."
  (declare (indent 0) (debug t))
  `(let ((disco-preview-fetch-enabled t)
         (disco-preview-fetch-debounce 0.35)
         (disco-preview-response-timeout 15)
         (disco-preview--timer nil)
         (disco-preview--timer-deadline nil)
         (disco-preview--pending-by-guild (make-hash-table :test #'equal))
         (disco-preview--requested-message-id-by-channel
          (make-hash-table :test #'equal))
         (disco-preview--in-flight-by-guild
          (make-hash-table :test #'equal))
         (disco-preview--blocked-until-by-guild
          (make-hash-table :test #'equal)))
     ,@body))

(ert-deftest disco-preview-request-channel-dedupes-message-id ()
  (disco-preview-test--with-state
    (let ((channel '((id . "c1")
                     (guild_id . "g1")
                     (last_message_id . "m1")))
          (updated-channel '((id . "c1")
                             (guild_id . "g1")
                             (last_message_id . "m2")))
          (scheduled 0))
      (cl-letf (((symbol-function 'disco-msg-channel-last-cached-message)
                 (lambda (_channel) nil))
                ((symbol-function 'disco-preview--schedule)
                 (lambda () (cl-incf scheduled))))
        (should (disco-preview-request-channel channel))
        (should-not (disco-preview-request-channel channel))
        (should-not (disco-preview-request-channel updated-channel))
        (should (= 2 scheduled))
        (should (equal '("c1")
                       (gethash "g1" disco-preview--pending-by-guild)))
        (should (equal "m2"
                       (gethash
                        "c1"
                        disco-preview--requested-message-id-by-channel)))))))

(ert-deftest disco-preview-request-channel-waits-for-gateway-ready ()
  (disco-preview-test--with-state
    (let ((running nil)
          scheduled-delay)
      (cl-letf (((symbol-function 'disco-msg-channel-last-cached-message)
                 (lambda (_channel) nil))
                ((symbol-function 'disco-gateway-running-p)
                 (lambda () running))
                ((symbol-function 'run-with-timer)
                 (lambda (delay _repeat _function &rest _args)
                   (setq scheduled-delay delay)
                   'preview-timer)))
        (should
         (disco-preview-request-channel
          '((id . "c1") (guild_id . "g1") (last_message_id . "m1"))))
        (should-not scheduled-delay)
        (setq running t)
        (disco-preview--handle-gateway-event '(:type ready))
        (should (= disco-preview-fetch-debounce scheduled-delay))))))

(ert-deftest disco-preview-flush-serializes-batches-per-guild ()
  (disco-preview-test--with-state
    (let ((disco-preview--gateway-batch-limit 2)
          calls)
      (puthash "g1" '("c1" "c2" "c3")
               disco-preview--pending-by-guild)
      (cl-letf (((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-send-queue-slot-available-p)
                 (lambda (&optional _slots) t))
                ((symbol-function 'disco-gateway-request-last-messages)
                 (lambda (guild-id channel-ids)
                   (push (list guild-id channel-ids) calls)
                   t))
                ((symbol-function 'disco-preview--schedule) #'ignore))
        (disco-preview--flush)
        (disco-preview--flush)
        (should (equal '(("g1" ("c1" "c2"))) (nreverse calls)))
        (should (equal '("c3")
                       (gethash "g1" disco-preview--pending-by-guild)))
        (should
         (equal '("c1" "c2")
                (plist-get
                 (gethash "g1" disco-preview--in-flight-by-guild)
                 :channel-ids)))
        (disco-preview--handle-gateway-event
         '(:type last-messages :guild-id "g1"))
        (disco-preview--flush)
        (should (equal '(("g1" ("c1" "c2"))
                         ("g1" ("c3")))
                       (nreverse calls)))))))

(ert-deftest disco-preview-flush-preserves-work-when-send-queue-is-full ()
  (disco-preview-test--with-state
    (let (requested scheduled)
      (puthash "g1" '("c1" "c2") disco-preview--pending-by-guild)
      (cl-letf (((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-send-queue-slot-available-p)
                 (lambda (&optional _slots) nil))
                ((symbol-function 'disco-gateway-request-last-messages)
                 (lambda (&rest _args)
                   (setq requested t)))
                ((symbol-function 'disco-preview--schedule)
                 (lambda () (setq scheduled t))))
        (disco-preview--flush)
        (should-not requested)
        (should scheduled)
        (should (equal '("c1" "c2")
                       (gethash "g1" disco-preview--pending-by-guild)))))))

(ert-deftest disco-preview-flush-retries-expired-in-flight-batch ()
  (disco-preview-test--with-state
    (let (calls)
      (puthash "g1" '(:channel-ids ("c1") :sent-at 100.0)
               disco-preview--in-flight-by-guild)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 116.0))
                ((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-send-queue-slot-available-p)
                 (lambda (&optional _slots) t))
                ((symbol-function 'disco-gateway-request-last-messages)
                 (lambda (guild-id channel-ids)
                   (push (list guild-id channel-ids) calls)
                   t))
                ((symbol-function 'disco-preview--schedule) #'ignore))
        (disco-preview--flush)
        (should (equal '(("g1" ("c1"))) calls))
        (should (= 116.0
                   (plist-get
                    (gethash "g1" disco-preview--in-flight-by-guild)
                    :sent-at)))))))

(ert-deftest disco-preview-ready-requeues-in-flight-work ()
  (disco-preview-test--with-state
    (puthash "g1" '("c2") disco-preview--pending-by-guild)
    (puthash "g1" '(:channel-ids ("c1") :sent-at 100.0)
             disco-preview--in-flight-by-guild)
    (puthash "g1" 120.0 disco-preview--blocked-until-by-guild)
    (cl-letf (((symbol-function 'disco-preview--schedule) #'ignore))
      (disco-preview--handle-gateway-event '(:type ready))
      (should (equal '("c1" "c2")
                     (gethash "g1" disco-preview--pending-by-guild)))
      (should (= 0 (hash-table-count
                    disco-preview--in-flight-by-guild)))
      (should (= 0 (hash-table-count
                    disco-preview--blocked-until-by-guild))))))

(ert-deftest disco-preview-rate-limit-requeues-and-blocks-guild ()
  (disco-preview-test--with-state
    (puthash "g1" '(:channel-ids ("c1") :sent-at 100.0)
             disco-preview--in-flight-by-guild)
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 100.0))
              ((symbol-function 'disco-preview--schedule) #'ignore))
      (disco-preview--handle-gateway-event
       '(:type rate-limited
         :opcode 34
         :retry-after 7.5
         :meta ((guild_id . "g1"))))
      (should (equal '("c1")
                     (gethash "g1" disco-preview--pending-by-guild)))
      (should-not (gethash "g1" disco-preview--in-flight-by-guild))
      (should (= 107.5
                 (gethash "g1" disco-preview--blocked-until-by-guild)))
      (should (= 7.5 (disco-preview--retry-delay))))))

(ert-deftest disco-preview-schedules-response-timeout-after-sending ()
  (disco-preview-test--with-state
    (let (scheduled-delay)
      (puthash "g1" '("c1") disco-preview--pending-by-guild)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 100.0))
                ((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'disco-gateway-send-queue-slot-available-p)
                 (lambda (&optional _slots) t))
                ((symbol-function 'disco-gateway-request-last-messages)
                 (lambda (&rest _args) t))
                ((symbol-function 'run-with-timer)
                 (lambda (delay _repeat _function &rest _args)
                   (setq scheduled-delay delay)
                   'preview-timer)))
        (disco-preview--flush)
        (should (= disco-preview-response-timeout scheduled-delay))))))

(ert-deftest disco-preview-schedule-advances-an-obsolete-watchdog ()
  (disco-preview-test--with-state
    (let ((disco-preview--timer 'old-timer)
          (disco-preview--timer-deadline 115.0)
          cancelled
          scheduled-delay)
      (puthash "g1" '("c2") disco-preview--pending-by-guild)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _time) 100.0))
                ((symbol-function 'disco-gateway-running-p) (lambda () t))
                ((symbol-function 'timerp)
                 (lambda (value) (eq value 'old-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer)))
                ((symbol-function 'run-with-timer)
                 (lambda (delay _repeat _function &rest _args)
                   (setq scheduled-delay delay)
                   'new-timer)))
        (disco-preview--schedule)
        (should (eq 'old-timer cancelled))
        (should (= disco-preview-fetch-debounce scheduled-delay))
        (should (= 100.35 disco-preview--timer-deadline))
        (should (eq 'new-timer disco-preview--timer))))))

(ert-deftest disco-preview-last-messages-cancels-completed-watchdog ()
  (disco-preview-test--with-state
    (let ((disco-preview--timer 'watchdog)
          (disco-preview--timer-deadline 115.0)
          cancelled)
      (puthash "g1" '(:channel-ids ("c1") :sent-at 100.0)
               disco-preview--in-flight-by-guild)
      (cl-letf (((symbol-function 'timerp)
                 (lambda (value) (eq value 'watchdog)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer)))
                ((symbol-function 'disco-gateway-running-p) (lambda () t)))
        (disco-preview--handle-gateway-event
         '(:type last-messages :guild-id "g1"))
        (should (eq 'watchdog cancelled))
        (should-not disco-preview--timer)
        (should-not disco-preview--timer-deadline)))))

(provide 'disco-preview-test)

;;; disco-preview-test.el ends here
