;;; disco-runtime-test.el --- Disco vNext runtime tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-runtime)
(require 'disco-root)
(require 'disco-room)

(ert-deftest disco-runtime-owns-one-vnext-app-lifecycle ()
  (let ((disco-runtime--app nil)
        (shutdown-count 0))
    (cl-letf (((symbol-function 'disco-gateway-stop)
               (lambda () (cl-incf shutdown-count))))
      (unwind-protect
          (let* ((first (disco-runtime-app))
                 (second (disco-runtime-app))
                 (ticket (appkit-app-send first 'unexpected)))
            (should (eq first second))
            (should (eq disco-runtime--app first))
            (should (eq (appkit-app-type first) disco-runtime--app-type))
            (should (eq (appkit-app-model first) 'running))
            (should (eq (appkit-app-identity first) 'default))
            (should (eq (appkit-loop-ticket-state ticket) 'rejected))
            (should
             (equal (appkit-loop-ticket-outcome ticket)
                    '(disco-lifecycle-app-has-no-domain-messages unexpected)))
            (disco-runtime-stop)
            (should-not disco-runtime--app)
            (should (eq (appkit-app-status first) 'stopped))
            (should (= shutdown-count 1))
            (disco-runtime-stop)
            (should (= shutdown-count 1)))
        (when (appkit-app-p disco-runtime--app)
          (disco-runtime-stop))))))

(ert-deftest disco-root-opens-one-generated-surface ()
  (let ((disco-runtime--app nil)
        (disco-root-buffer-name " *disco-root-vnext-test*")
        (watch-count 0)
        (unwatch-count 0)
        app buffer surface)
    (cl-letf (((symbol-function 'disco-gateway-watch-global)
               (lambda () (cl-incf watch-count)))
              ((symbol-function 'disco-gateway-unwatch-global)
               (lambda () (cl-incf unwatch-count)))
              ((symbol-function 'disco-gateway-stop) #'ignore))
      (save-window-excursion
        (unwind-protect
            (progn
              (setq buffer (disco-root-open)
                    app disco-runtime--app
                    surface
                    (with-current-buffer buffer (appkit-current-surface)))
              (should (buffer-live-p buffer))
              (should (eq (buffer-local-value 'major-mode buffer)
                          'disco-root-mode))
              (should (appkit-surface-live-p surface))
              (should (equal (appkit-surface-identity surface) '(root main)))
              (should (eq buffer (disco-root-open)))
              (should (= watch-count 1))
              (with-current-buffer buffer
                (disco-root--queue-live-update '(:type refresh))
                (disco-root--flush-live-updates))
              (should (eq (appkit-surface-status surface) 'running))
              (kill-buffer buffer)
              (setq buffer nil)
              (should (eq (appkit-surface-status surface) 'stopped))
              (should (= unwatch-count 1)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (when (and (appkit-app-p app)
                     (not (eq (appkit-app-status app) 'stopped)))
            (appkit-app-close app)))))))

(ert-deftest disco-room-opens-one-generated-surface-and-fences-jumps ()
  (let ((disco-runtime--app nil)
        (disco-media-show-previews nil)
        around-success around-owner app buffer surface)
    (cl-letf (((symbol-function 'disco-state-channel)
               (lambda (_id)
                 '((id . "room") (name . "Room") (type . 0)
                   (permissions . "2048"))))
              ((symbol-function 'disco-state-messages) (lambda (&rest _) nil))
              ((symbol-function 'disco-api-channel-messages-async)
               (lambda (&rest args)
                 (when-let* ((success (plist-get args :on-success)))
                   (funcall success nil))
                 nil))
              ((symbol-function 'disco-api-channel-messages-around-async)
               (lambda (_channel-id _target-id &rest args)
                 (setq around-success (plist-get args :on-success)
                       around-owner (plist-get args :owner))
                 nil))
              ((symbol-function 'disco-gateway-watch-channel) #'ignore)
              ((symbol-function 'disco-gateway-unwatch-channel) #'ignore)
              ((symbol-function 'disco-gateway-current-user-id)
               (lambda () "self"))
              ((symbol-function 'disco-current-token) (lambda () nil))
              ((symbol-function 'disco-gateway-stop) #'ignore))
      (save-window-excursion
        (unwind-protect
            (progn
              (setq buffer (disco-room-open "room" "Room")
                    app disco-runtime--app
                    surface
                    (with-current-buffer buffer (appkit-current-surface)))
              (should (appkit-surface-live-p surface))
              (with-current-buffer buffer
                (disco-room--queue-jump "missing" surface)
                (disco-room--flush-updates surface))
              (should (eq (appkit-surface-status surface) 'running))
              (should (functionp around-success))
              (should (eq around-owner surface))
              (funcall around-success nil)
              (with-current-buffer buffer
                (disco-room--flush-updates surface)
                (should-not disco-room--pending-jump-message-id))
              (kill-buffer buffer)
              (setq buffer nil)
              (should (eq (appkit-surface-status surface) 'stopped)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (when (and (appkit-app-p app)
                     (not (eq (appkit-app-status app) 'stopped)))
            (appkit-app-close app)))))))

(ert-deftest disco-room-surface-geometry-drives-alignment-and-scale-redraw ()
  (let ((disco-runtime--app nil)
        (disco-media-show-previews nil)
        (disco-room-auto-fill-margin-columns 1)
        app buffer surface timestamp-display
        (preview-clears 0)
        (sticker-clears 0)
        (renders 0))
    (disco-state-reset)
    (disco-state-upsert-channel
     '((id . "geometry-room") (name . "Geometry") (type . 0)
       (permissions . "2048")))
    (disco-state-upsert-message
     "geometry-room"
     '((id . "m1") (channel_id . "geometry-room")
       (timestamp . "2026-09-04T12:00:00+00:00") (content . "hello")
       (author . ((id . "u1") (username . "Alice")))))
    (cl-letf (((symbol-function 'disco-api-channel-messages-async)
               (lambda (&rest _) nil))
              ((symbol-function 'disco-gateway-watch-channel) #'ignore)
              ((symbol-function 'disco-gateway-unwatch-channel) #'ignore)
              ((symbol-function 'disco-gateway-current-user-id)
               (lambda () "self"))
              ((symbol-function 'disco-current-token) (lambda () nil))
              ((symbol-function 'disco-gateway-stop) #'ignore)
              ((symbol-function 'appkit-geometry-window-width)
               (lambda (&rest _) 96)))
      (save-window-excursion
        (unwind-protect
            (progn
              (setq buffer (disco-room-open "geometry-room" "Geometry")
                    app disco-runtime--app
                    surface
                    (with-current-buffer buffer (appkit-current-surface)))
              (with-current-buffer buffer
                (setq-local visual-fill-column-mode nil)
                (appkit-chat-history-window-set "m1" nil)
                (disco-room--queue-update surface 'refresh)
                (disco-room--flush-updates surface)
                (should (= 95 (disco-room--line-fill-column)))
                (should (string-match-p "Alice" (buffer-string)))
                (let ((position (point-min)) spec)
                  (while
                      (and (< position (point-max))
                           (progn
                             (setq spec
                                   (get-text-property position 'display))
                             (not (and (consp spec)
                                       (eq (car spec) 'space)
                                       (eq (cadr spec) :align-to)))))
                    (setq position
                          (next-single-property-change
                           position 'display nil (point-max))))
                  (should (< position (point-max)))
                  (setq timestamp-display spec))
                (let ((target (nth 2 timestamp-display)))
                  (should
                   (= 90
                      (if (consp target)
                          (/ (car target)
                             (appkit-geometry-columns-pixel-width 1 (selected-window)))
                        target))))
                (cl-letf (((symbol-function
                            'disco-media-clear-preview-memory-cache)
                           (lambda () (cl-incf preview-clears)))
                          ((symbol-function 'disco-sticker-clear-image-memory)
                           (lambda () (cl-incf sticker-clears)))
                          ((symbol-function 'disco-room-render)
                           (lambda () (cl-incf renders))))
                  (text-scale-set 1)
                  (disco-room--flush-updates surface)))
              (should (= 1 preview-clears))
              (should (= 1 sticker-clears))
              (should (= 1 renders)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (when (and (appkit-app-p app)
                     (not (eq (appkit-app-status app) 'stopped)))
            (appkit-app-close app))
          (disco-state-reset))))))

(provide 'disco-runtime-test)

;;; disco-runtime-test.el ends here
