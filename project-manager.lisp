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

(defun apply-to-project-tree (path &optional (predicate-funcs (list (lambda (x) x))) (apply-func #'identity) (not-git t))
  (let* ((flat-tree (if not-git 
		       (remove-if #'git-folder-p (flatten-list (get-directory-structure path)))
		       (flatten-list (get-directory-structure path))))
	(filtered-tree flat-tree))

    (loop for predicate in predicate-funcs
	 do (setf filtered-tree (remove-if-not predicate filtered-tree)))
    
    (loop for thing in filtered-tree
	 do (funcall apply-func thing))))
