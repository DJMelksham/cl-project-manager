(defun get-project-structure ()
  *active-project-structure*)

(defun get-project-name ()
  *active-project-name*)

(defun set-active-project (pathname name)
  (setf *active-project-path* pathname) 
  (setf *active-project-name* name)
  *active-project-path*)

(defun set-active-module (name)
  name)
  
(defun create-default-project-structure (project-path project-name authors)
  (ensure-directories-exist 
   (cl-fad:merge-pathnames-as-directory project-path (pathname "tests/")))

  (with-open-file (output-file (merge-pathnames project-path ".gitignore")
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists nil)
    (unless (probe-file (merge-pathnames project-path ".gitignore"))
      (prin1 (make-git-ignore-text) output-file)))

  (with-open-file (output-file (merge-pathnames project-path "LICENCE")
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists nil)
    (unless (probe-file (merge-pathnames project-path "LICENCE"))
      (prin1 (create-licence-text) output-file)))

  (with-open-file (output-file (merge-pathnames project-path "README.md")
			       :direction :output
			       :if-does-not-exist :create
			       :if-exists nil)
    (unless (probe-file (merge-pathnames project-path "README.md"))
      (prin1 (create-readme-text project-name authors) output-file))))

(defun make-project (name path &optional (make-active t)
				         (structure nil)
				         (authors "Damien John Melksham"))
  (let ((project-path (cl-fad:pathname-as-directory path)))

    ;; create project directory if it doesn't exist
    (ensure-directories-exist project-path :verbose t)
    
    ;; create default project structure
    (create-default-project-structure project-path name authors)

    ;; set as active project if requested
    (when make-active
     (set-active-project project-path name))

    ;; unless structure is set, use the default structure
    (unless structure
      nil)

    ;; Initialise Git Repository in Project and estbalish first commit
    (when (not (path-in-git-project-p project-path))
       (git-init project-path)
       (git-add-all project-path)
       (git-commit project-path))

project-path))

;(defun valid-project-p (pathname)
;nil)

;(defun valid-module-p (pathname)
;nil)

;(defun get-active-project ()
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
