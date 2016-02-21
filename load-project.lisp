(in-package :cl-user)
(defpackage cl-project-manager
  (:use :cl))
(in-package :cl-project-manager)

(ql:quickload 'cl-fad)

(load (merge-pathnames "document-functions.lisp" *load-truename*))
(load (merge-pathnames "file-management.lisp" *load-truename*))
(load (merge-pathnames "project-manager.lisp" *load-truename*))


