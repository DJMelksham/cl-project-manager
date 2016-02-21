(defparameter *active-project-name* nil)
(defparameter *active-project-path* nil)
(defparameter *active-module-name* nil)
(defparameter *module-names-in-project* nil)
(defparameter *module-names-in-module nil)
(defparameter *module-paths* nil)

(defparameter *active-project-structure* nil)
  
(defun get-project-structure ()
  *active-project-structure*)

(defun get-project-name ()
  *active-project-name*)

(defun make-project (name path &optional (make-active t)
				         (default-structure t)
				         (authors (list "Damien John Melksham")))
  (let ((project-path (pathname path)))

    ;; create project directory if it doesn't exist
    (ensure-directories-exist project-path :verbose t)
    
    ;; create default project structure
    (create-default-project-structure project-path name authors)

    ;; set as active project if requested
    ;(when make-active
    ; (set-active-project project-path))

project-path))

(defun create-default-project-structure (project-path name authors)
  (ensure-directories-exist 
   (cl-fad:merge-pathnames-as-directory project-path (pathname "tests/")))

  (with-open-file (output-file (merge-pathnames project-path ".gitignore")
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists nil)
    (prin1 (make-git-ignore) output-file))

  (with-open-file (output-file (merge-pathnames project-path "LICENCE")
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists nil)
    (prin1 (create-licence-text) output-file))

  (with-open-file (output-file (merge-pathnames project-path "README.md")
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists nil)
    (prin1 (create-readme-text name authors) output-file)))

;(defun valid-project-p (pathname)
;nil)

;(defun valid-module-p (pathname)
;nil)

;(defun set-active-project (pathname)
;nil)

;(defun get-active-project ()
;nil)

;(defun set-active-module (pathname)
;nil)

;(defun get-active-module ()
;nil)

;(defun list-modules ()
;nil)

;(defun list-module-paths ()
;nil)

;(defun test-project (pathname)
;nil)

;(defun test-module (pathname)
;nil)

;(defun test-file (pathname)
;nil)

;(defun make-test-file ()
;nil)

;(defun make-module-load-file (module)
;nil)

;(defun make-git-ignore-file (pathname)
;nil)

;(defun make-module-load-order-file (module &optional load-order-list)
;nil)

;(defun make-test-directory (path)
;nil)

;(defun make-config-directory (path)
;nil)

;(defun create-test-file (path)
;nil)

;(defun make-module-structure-from-list (input-list)
;nil)

;(defun make-list-from-module-structure (project)
;nil)
