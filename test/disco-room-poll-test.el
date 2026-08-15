;;; disco-room-poll-test.el --- Tests for room poll interaction -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'disco-room)

(ert-deftest disco-room-poll-menu-hides-nonactionable-polls ()
  "The message menu exposes polls only when a poll action can run."
  (let ((message '((id . "p1")
                   (poll . ((question . ((text . "Question"))))))))
    (cl-letf (((symbol-function 'disco-room-menu--message-at-point)
               (lambda () message))
              ((symbol-function 'disco-room--poll-vote-unavailable-reason)
               (lambda (&optional _message) nil))
              ((symbol-function 'disco-room--poll-expire-unavailable-reason)
               (lambda (&optional _message) "only poll author can end this poll")))
      (should (disco-room-poll-actionable-at-point-p)))
    (cl-letf (((symbol-function 'disco-room-menu--message-at-point)
               (lambda () message))
              ((symbol-function 'disco-room--poll-vote-unavailable-reason)
               (lambda (&optional _message) "poll is closed"))
              ((symbol-function 'disco-room--poll-expire-unavailable-reason)
               (lambda (&optional _message) "poll is already closed")))
      (should-not (disco-room-poll-actionable-at-point-p)))))

;;; disco-room-poll-test.el ends here
