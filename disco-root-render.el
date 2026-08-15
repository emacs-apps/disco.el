;;; disco-root-render.el --- Root render specifications for disco.el -*- lexical-binding: t; -*-

;; Author: disco.el contributors

;;; Commentary:

;; Render specifications shared by the composite root and temporary search.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-directory)
(require 'appkit-view)

(declare-function disco-root--build-search-render-spec
                  "disco-root-view" ())
(declare-function disco-root--build-composite-render-spec
                  "disco-root-view" ())

(defcustom disco-root-tree-unread-section-limit 40
  "Maximum unread rows shown by the composite root's quick unread section.

When nil, show all unread rows without truncation."
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Limit"))
  :group 'disco)

(defcustom disco-root-tree-default-expanded-sections '(unread private guilds)
  "Root tree sections expanded when a root buffer is first created."
  :type '(set (const unread) (const private) (const guilds))
  :group 'disco)

(defvar disco-root--search-active-p)

(cl-defstruct (disco-root-render-spec
               (:constructor disco-root-render-spec-create))
  kind
  entries
  list-spec
  directory-surface
  force-keys)

(cl-defstruct (disco-root-render-entry
               (:constructor disco-root-render-entry-create))
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

(defun disco-root-render-list-spec-create (list-spec)
  "Wrap LIST-SPEC in a root view spec."
  (disco-root-render-spec-create
   :kind 'list-spec
   :list-spec list-spec))

(cl-defun disco-root-render-directory-spec-create
    (surface entries &key force-keys)
  "Return one Appkit directory VIEW-SPEC for SURFACE and ENTRIES.

FORCE-KEYS names retained directory rows whose rich renderers must run again."
  (unless (appkit-directory-surface-p surface)
    (error "Disco: root directory render requires an Appkit surface"))
  (disco-root-render-spec-create
   :kind 'directory
   :entries entries
   :directory-surface surface
   :force-keys force-keys))

(defun disco-root-render-spec (view-spec)
  "Render VIEW-SPEC in the current root buffer."
  (when (disco-root-render-spec-p view-spec)
    (let ((inhibit-read-only t))
      (pcase (disco-root-render-spec-kind view-spec)
        ('list-spec
         (when-let* ((list-spec (disco-root-render-spec-list-spec view-spec)))
           (appkit-view-render-list-spec list-spec)))
        ('directory
         (appkit-directory-reconcile
          (or (disco-root-render-spec-directory-surface view-spec)
              (appkit-directory-surface))
          (or (disco-root-render-spec-entries view-spec) '())
          :force-keys (disco-root-render-spec-force-keys view-spec)))
        (_
         (error "Unknown root render spec kind: %S"
                (disco-root-render-spec-kind view-spec))))
      t)))

(defun disco-root-render-projection ()
  "Render the composite root or its active temporary search projection."
  (let* ((builder (if disco-root--search-active-p
                      #'disco-root--build-search-render-spec
                    #'disco-root--build-composite-render-spec))
         (view-spec (funcall builder)))
    (unless (disco-root-render-spec-p view-spec)
      (error "Disco: root builder returned an invalid view spec"))
    (disco-root-render-spec view-spec)))

(provide 'disco-root-render)

;;; disco-root-render.el ends here
