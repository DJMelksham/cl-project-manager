(in-package :cl-user)
(defpackage cl-project-manager
  (:use :cl))
(in-package :cl-project-manager)

;; body
(defparameter *active-project* nil)
(defparameter *active-project-path* nil)
(defparameter *active-module* nil)
(defparameter *module-names* nil)
(defparameter *module-paths* nil)

(defparameter *project-structure* '())

(defun make-project (pathname)

  

)

(defun valid-project-p (pathname)
)

(defun valid-module-p (pathname)
)

(defun set-active-project (pathname)

)

(defun get-active-project ()

)

(defun set-active-module (pathname)

)

(defun get-active-module ()

)

(defun list-modules ()

)

(defun list-module-paths ()

)

(defun test-project (pathname)

)

(defun test-module (pathname)

)

(defun test-file (pathname)

)

(defun make-module-load-file (module)

)

(defun make-project-load-file (module)

)

(defun make-git-ignore-file (pathname)
)

(defun make-module-load-order-file (module &optional load-order-list)

)

(defun create-test-directory (path)

)

(defun create-config-directory (path)

)

(defun create-licence-file (path)

)

(defun create-readme-file (path)

)

(defun create-test-file (path)

)

(defun make-pathname-from-string (path-string)

)
