;;; disco-root-layout.el --- Root render specifications for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Render specifications shared by the composite root and temporary search.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-directory)
(require 'appkit-view)

(declare-function disco-root--build-search-layout-view-spec
                  "disco-root-view" ())
(declare-function disco-root--build-tree-layout-view-spec
                  "disco-root-view" ())

(defcustom disco-root-tree-unread-section-limit 40
  "Maximum unread rows shown by the home layout's quick unread section.

When nil, show all unread rows without truncation."
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Limit"))
  :group 'disco)

(defcustom disco-root-tree-default-expanded-sections '(unread private guilds)
  "Root tree sections expanded when a root buffer is first created."
  :type '(set (const unread) (const private) (const guilds))
  :group 'disco)

(defvar disco-root--search-active-p)

(cl-defstruct (disco-root-layout-view-spec
               (:constructor disco-root-layout-view-spec-create))
  kind
  entries
  list-spec
  directory-surface
  force-keys)

(cl-defstruct (disco-root-layout-entry
               (:constructor disco-root-layout-entry-create))
  key
  type
  title
  text
  face
  indent
  tab
  message
  label
  action
  loaded-count
  total-count
  loading)

(defun disco-root-layout-list-spec-view-spec-create (list-spec)
  "Wrap LIST-SPEC in a root view spec."
  (disco-root-layout-view-spec-create
   :kind 'list-spec
   :list-spec list-spec))

(cl-defun disco-root-layout-directory-view-spec-create
    (surface entries &key force-keys)
  "Return one Appkit directory VIEW-SPEC for SURFACE and ENTRIES.

FORCE-KEYS names retained directory rows whose rich renderers must run again."
  (unless (appkit-directory-surface-p surface)
    (error "Disco: root directory layout requires an Appkit surface"))
  (disco-root-layout-view-spec-create
   :kind 'directory
   :entries entries
   :directory-surface surface
   :force-keys force-keys))

(defun disco-root-layout-render-view-spec (view-spec)
  "Render VIEW-SPEC in the current root buffer."
  (when (disco-root-layout-view-spec-p view-spec)
    (let ((inhibit-read-only t))
      (pcase (disco-root-layout-view-spec-kind view-spec)
        ('list-spec
         (when-let* ((list-spec (disco-root-layout-view-spec-list-spec view-spec)))
           (appkit-view-render-list-spec list-spec)))
        ('directory
         (appkit-directory-reconcile
          (or (disco-root-layout-view-spec-directory-surface view-spec)
              (appkit-directory-surface))
          (or (disco-root-layout-view-spec-entries view-spec) '())
          :force-keys (disco-root-layout-view-spec-force-keys view-spec)))
        (_
         (error "Unknown root layout view spec kind: %S"
                (disco-root-layout-view-spec-kind view-spec))))
      t)))

(defun disco-root-layout-render ()
  "Render the composite root or its active temporary search projection."
  (let* ((builder (if disco-root--search-active-p
                      #'disco-root--build-search-layout-view-spec
                    #'disco-root--build-tree-layout-view-spec))
         (view-spec (funcall builder)))
    (unless (disco-root-layout-view-spec-p view-spec)
      (error "Disco: root builder returned an invalid view spec"))
    (disco-root-layout-render-view-spec view-spec)))

(provide 'disco-root-layout)

;;; disco-root-layout.el ends here
