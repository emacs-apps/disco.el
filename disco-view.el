;;; disco-view.el --- Cursor/view preservation helpers for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Shared utilities for preserving cursor/viewport state around full buffer
;; rerenders. This keeps passive timeline/root updates from unexpectedly
;; snapping point to the input/footer.

;;; Code:

(require 'cl-lib)

(cl-defstruct (disco-view--snapshot
               (:constructor disco-view--snapshot-create))
  line
  column
  anchor-property
  anchor-value
  window-start-line)

(cl-defun disco-view-capture-position (&key anchor-property preserve-window-start)
  "Capture current position context for later restoration.

When ANCHOR-PROPERTY is non-nil, also capture its value at point (or line
beginning), so restore can anchor by semantic row identity.

When PRESERVE-WINDOW-START is non-nil, capture window-start as a 1-based line
index for the current buffer window."
  (let* ((anchor-value (and anchor-property
                            (or (get-text-property (point) anchor-property)
                                (get-text-property (line-beginning-position)
                                                   anchor-property))))
         (win (and preserve-window-start
                   (get-buffer-window (current-buffer))))
         (window-start-line (and win
                                 (save-excursion
                                   (goto-char (window-start win))
                                   (line-number-at-pos)))))
    (disco-view--snapshot-create
     :line (line-number-at-pos)
     :column (current-column)
     :anchor-property anchor-property
     :anchor-value anchor-value
     :window-start-line window-start-line)))

(defun disco-view-restore-position (snapshot)
  "Restore point/window state from SNAPSHOT.

If SNAPSHOT carries an anchor property/value and the anchor is still present,
restore by anchor first. Otherwise restore by line/column fallback."
  (let* ((anchor-property (disco-view--snapshot-anchor-property snapshot))
         (anchor-value (disco-view--snapshot-anchor-value snapshot))
         (line (max 1 (or (disco-view--snapshot-line snapshot) 1)))
         (column (max 0 (or (disco-view--snapshot-column snapshot) 0)))
         (target (and anchor-property
                      anchor-value
                      (text-property-any
                       (point-min)
                       (point-max)
                       anchor-property
                       anchor-value))))
    (if target
        (goto-char target)
      (goto-char (point-min))
      (forward-line (1- line)))
    (move-to-column column)
    (let* ((window-start-line (disco-view--snapshot-window-start-line snapshot))
           (win (and (integerp window-start-line)
                     (get-buffer-window (current-buffer)))))
      (when win
        (save-excursion
          (goto-char (point-min))
          (forward-line (max 0 (1- window-start-line)))
          (set-window-start win (point) 'noforce))))))

(cl-defun disco-view-render-preserving-position
    (render-fn &key anchor-property preserve-window-start)
  "Call RENDER-FN, then restore cursor/viewport context.

RENDER-FN must redraw current buffer. ANCHOR-PROPERTY and
PRESERVE-WINDOW-START are forwarded to `disco-view-capture-position'."
  (let ((snapshot (disco-view-capture-position
                   :anchor-property anchor-property
                   :preserve-window-start preserve-window-start)))
    (funcall render-fn)
    (disco-view-restore-position snapshot)))

(provide 'disco-view)

;;; disco-view.el ends here
