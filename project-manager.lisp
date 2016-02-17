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


(defparameter *project-structure* '((commonlisp)
				    (config)
				    (tests)
				    .gitignore ""
				    LICENCE ""
				    README.md ""))
  
(defun get-project-structure ()
  default-structure))

(defun get-project-name (&optional active-project (get-active-project))

)

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

(defun make-test-directory (path)

)

(defun make-config-directory (path)

)

(defun make-git-ignore (&optional (additional-list-of-text nil))
  (let ((string (format nil "~{~a~%~}" 
			(append (list "#Ignore emacs editor temporary files"
				      "[#]*[#]"
				      "*~"
				      ""
				      "#Ignore cl-project-manager files")
				additional-list-of-text))))
    string))

(defun create-licence-file (&optional other-licence-file)
  (let ((string "All Rights Reserved."))
    string))

(defun create-readme-file (&optional 
			     (project-name (get-project-name) project-name-supplied-p)
			     ()
  (let ((string format nil "~{~a~%~}"
		(list (if project-name-supplied-p 
			  project-name 
			  "## Synopsis")
		      ""
		      "At the top of the file there should be a short introduction and/ or overview that explains **what** the project is."
		      ""
		      "## Motivation"
		      ""
		      "A short description of the motivation behind the creation and maintenance of the project. This should explain **why** the project exists."
		      ""
		      "## Installation"
		      ""
		      "Load the automatically generated file 'load-project.lisp' in your common lisp repl"
		      ""
		      "## Author"
		      ""
		      "Damien John Melksham"
		      ""
)))))

(defun create-test-file (path)

)

