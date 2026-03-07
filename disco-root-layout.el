;;; disco-root-layout.el --- Root layout registry for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Layout definitions and customization entry points for root buffer views.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-view)

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
- `:build' function symbol returning a `disco-root-layout-view-spec'.
- `:render' legacy function symbol called by `disco-root-render'. It may
  either render directly or return a `disco-root-layout-view-spec'.
- `:update-mode' symbol `incremental' or `full'.
- `:unread-mode' symbol such as `section', `summary', or `filter'.
- `:toggle-hint' short help text shown in root header for TAB/t behavior.
- `:refresh-headings' function called in incremental update mode.

Custom entries can override built-in layouts when NAME matches."
  :type 'sexp
  :group 'disco)

(cl-defstruct (disco-root-layout-view-spec
               (:constructor disco-root-layout-view-spec-create))
  kind
  before-render
  render-function
  items
  item-inserter
  list-spec
  after-render)

(defconst disco-root-layout-builtin-specs
  '((tree
     :label "Tree"
     :build disco-root--render-layout-tree
     :update-mode incremental
     :unread-mode section
     :toggle-hint "toggle section/guild/category or next channel"
     :refresh-headings disco-root--refresh-heading-nodes)
    (activity
     :label "Activity"
     :build disco-root--render-layout-activity
     :update-mode incremental
     :unread-mode filter
     :toggle-hint "next channel")
    (search
     :label "Search"
     :build disco-root--render-layout-search
     :update-mode full
     :unread-mode summary
     :toggle-hint "next result or load more"))
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

(defun disco-root-layout-builder (&optional layout)
  "Return view builder function symbol for LAYOUT (or active layout)."
  (plist-get (disco-root-layout-spec layout) :build))

(defun disco-root-layout-renderer (&optional layout)
  "Return legacy renderer function symbol for LAYOUT (or active layout)."
  (plist-get (disco-root-layout-spec layout) :render))

(cl-defun disco-root-layout-list-spec-view-spec-create (list-spec &key after-render)
  "Return one root layout view spec wrapping LIST-SPEC."
  (disco-root-layout-view-spec-create
   :kind 'list-spec
   :list-spec list-spec
   :after-render after-render))

(cl-defun disco-root-layout-ewoc-items-view-spec-create
    (items &key before-render item-inserter after-render)
  "Return one EWOC-backed root layout view spec for ENTRY ITEMS.

When BEFORE-RENDER or ITEM-INSERTER are omitted, use the standard root EWOC
helpers so custom `:build' layouts can reuse the built-in tree/activity entry
pipeline without re-declaring private hooks."
  (disco-root-layout-view-spec-create
   :kind 'items
   :before-render (or before-render 'disco-root--prepare-ewoc-state)
   :items items
   :item-inserter (or item-inserter 'disco-root--ewoc-insert-entry)
   :after-render after-render))

(defun disco-root-layout-render-view-spec (view-spec)
  "Render VIEW-SPEC in current root buffer.

VIEW-SPEC is a `disco-root-layout-view-spec' object produced by a layout
builder."
  (when (disco-root-layout-view-spec-p view-spec)
    (let ((inhibit-read-only t))
      (when-let* ((before-render
                   (disco-root-layout-view-spec-before-render view-spec)))
        (funcall before-render))
      (pcase (disco-root-layout-view-spec-kind view-spec)
        ('list-spec
         (when-let* ((list-spec (disco-root-layout-view-spec-list-spec view-spec)))
           (disco-view-render-list-spec list-spec)))
        ('items
         (when-let* ((item-inserter
                      (disco-root-layout-view-spec-item-inserter view-spec)))
           (dolist (item (or (disco-root-layout-view-spec-items view-spec) '()))
             (funcall item-inserter item))))
        ('render-function
         (when-let* ((render-function
                      (disco-root-layout-view-spec-render-function view-spec)))
           (funcall render-function)))
        (_
         (error "Unknown root layout view spec kind: %S"
                (disco-root-layout-view-spec-kind view-spec))))
      (when-let* ((after-render
                   (disco-root-layout-view-spec-after-render view-spec)))
        (funcall after-render)))
    t))

(defun disco-root-layout-render (&optional layout)
  "Render LAYOUT (or active layout) in the current root buffer."
  (let ((builder (disco-root-layout-builder layout))
        (renderer (disco-root-layout-renderer layout)))
    (cond
     ((functionp builder)
      (when-let* ((view-spec (funcall builder)))
        (disco-root-layout-render-view-spec view-spec)))
     ((functionp renderer)
      (let ((result (funcall renderer)))
        (if (disco-root-layout-view-spec-p result)
            (disco-root-layout-render-view-spec result)
          t)))
     (t nil))))

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
