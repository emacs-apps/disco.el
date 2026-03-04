;;; disco-root-layout.el --- Root layout registry for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Layout definitions and customization entry points for root buffer views.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom disco-root-default-layout 'tree
  "Default root layout symbol used when opening the root buffer."
  :type 'symbol
  :group 'disco)

(defcustom disco-root-tree-default-show-unread-section t
  "When non-nil, tree layout renders the unread quick section by default."
  :type 'boolean
  :group 'disco)

(defcustom disco-root-tree-unread-section-limit 40
  "Maximum unread rows shown by the tree layout quick unread section.

When nil, show all unread rows without truncation."
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Limit"))
  :group 'disco)

(defcustom disco-root-custom-layouts nil
  "User-defined root layout specs.

Each element is (NAME . PLIST), where NAME is a symbol and PLIST accepts:
- `:label' string used in root header.
- `:render' function symbol called by `disco-root-render'.
- `:update-mode' symbol `incremental' or `full'.
- `:unread-mode' symbol such as `section', `summary', or `filter'.
- `:toggle-hint' short help text shown in root header for TAB/t behavior.
- `:refresh-headings' function called in incremental update mode.

Custom entries can override built-in layouts when NAME matches."
  :type 'sexp
  :group 'disco)

(defconst disco-root-layout-builtin-specs
  '((tree
     :label "Tree"
     :render disco-root--render-layout-tree
     :update-mode incremental
     :unread-mode section
     :toggle-hint "toggle section/guild/category or next channel"
     :refresh-headings disco-root--refresh-heading-nodes)
    (activity
     :label "Activity"
     :render disco-root--render-layout-activity
     :update-mode incremental
     :unread-mode filter
     :toggle-hint "next channel"))
  "Built-in root layout specs.")

(defun disco-root-layout-specs ()
  "Return merged built-in and custom root layout specs as an alist."
  (let ((specs (copy-tree disco-root-layout-builtin-specs)))
    (dolist (entry disco-root-custom-layouts)
      (when (and (consp entry)
                 (symbolp (car entry))
                 (listp (cdr entry)))
        (let ((name (car entry))
              (plist (cdr entry)))
          (if-let* ((cell (assq name specs)))
              (setcdr cell plist)
            (setq specs (append specs (list (cons name plist))))))))
    specs))

(defun disco-root-layout-names ()
  "Return ordered list of available root layout symbols."
  (mapcar #'car (disco-root-layout-specs)))

(defun disco-root-layout--active-layout (&optional layout)
  "Return explicit LAYOUT or currently active root layout symbol."
  (or layout
      (and (boundp 'disco-root--layout) disco-root--layout)
      disco-root-default-layout))

(defun disco-root-layout-spec (&optional layout)
  "Return merged layout plist for LAYOUT (or active layout)."
  (let* ((name (disco-root-layout--active-layout layout))
         (spec (alist-get name (disco-root-layout-specs) nil nil #'eq)))
    (or spec
        (alist-get 'tree (disco-root-layout-specs) nil nil #'eq)
        '(:label "Tree" :update-mode incremental))))

(defun disco-root-layout-label (&optional layout)
  "Return display label for LAYOUT (or active layout)."
  (let* ((name (disco-root-layout--active-layout layout))
         (label (plist-get (disco-root-layout-spec name) :label)))
    (or label (symbol-name name))))

(defun disco-root-layout-renderer (&optional layout)
  "Return renderer function symbol for LAYOUT (or active layout)."
  (plist-get (disco-root-layout-spec layout) :render))

(defun disco-root-layout-update-mode (&optional layout)
  "Return update mode for LAYOUT (or active layout)."
  (or (plist-get (disco-root-layout-spec layout) :update-mode)
      'incremental))

(defun disco-root-layout-unread-mode (&optional layout)
  "Return unread lens mode for LAYOUT (or active layout)."
  (or (plist-get (disco-root-layout-spec layout) :unread-mode)
      'summary))

(defun disco-root-layout-toggle-hint (&optional layout)
  "Return TAB/t behavior hint for LAYOUT (or active layout)."
  (or (plist-get (disco-root-layout-spec layout) :toggle-hint)
      "toggle row or move to next channel"))

(defun disco-root-layout-refresh-headings-function (&optional layout)
  "Return optional heading refresher for LAYOUT (or active layout)."
  (plist-get (disco-root-layout-spec layout) :refresh-headings))

(provide 'disco-root-layout)

;;; disco-root-layout.el ends here
