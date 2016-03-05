(in-package :cl-user)
(defpackage cl-project-manager
  (:use :cl))
(in-package :cl-project-manager)

(ql:quickload 'cl-fad)

(defun relative-load (filename)
  (load (merge-pathnames filename *load-truename*)))

(relative-load "variable-definitions.lisp")
(relative-load "document-functions.lisp")
(relative-load "file-management.lisp")
(relative-load "git-management.lisp")
(relative-load "project-manager.lisp")
(relative-load "utility.lisp")
(relative-load "config-management.lisp")
