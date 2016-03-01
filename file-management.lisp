(defun get-subfolders (path &optional (inclusive nil))
  (let ((result (if inclusive (list (cl-fad:pathname-as-directory (truename (pathname path)))) nil))
	(dirs (remove-if-not #'cl-fad:directory-pathname-p
			     (cl-fad:list-directory (cl-fad:pathname-as-directory 
						     (truename (pathname path)))))))
    (loop for dir in dirs
	do (push dir result))
    (loop for dir in dirs
	  do (setf result (append (get-subfolders dir) result)))
    (nreverse result)))

(defun git-folder-p (path-to-be-searched)
 (let* ((true-path (truename (cl-fad:pathname-as-directory path-to-be-searched)))
	 (true-name-string (namestring true-path)))
   (if (and (cl-fad:directory-exists-p true-path)
	    (search "/.git/" true-name-string))
      T
      NIL)))

(defun get-path-folders-without-git (path &optional (inclusive nil))
  (remove-if #'git-folder-p (get-subfolders path inclusive)))

(defun get-project-folders ()
  (get-path-folders-without-git *active-project-path* t))

(defun get-module-folders ()
  (get-path-folders-without-git *active-module-path* t))

(defun get-directory-structure (path)
  (append (list (truename (cl-fad:pathname-as-directory path)))
	(loop for things in
		  (remove-if #'git-folder-p (cl-fad:list-directory path))
		collect (if (cl-fad:directory-pathname-p things)
			    (get-directory-structure things)
			    things))))

(defun flatten-list (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten-list structure))))

(defun get-type-structure (path type)
  (flet ((is-type-p (thing type-type) (if (string= (pathname-type thing) type-type)
					  thing
					  nil)))
   
    (loop for things in (remove-if #'git-folder-p (cl-fad:list-directory path))
	 collect (if (cl-fad:directory-pathname-p things)
		     (get-type-structure things type)
		     (if (is-type-p things type)
			 things
			 nil)))))
		     

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))
