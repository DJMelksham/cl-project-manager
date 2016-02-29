(defun get-subfolders (path &optional (inclusive nil))
  (let ((result (if inclusive (list (cl-fad:pathname-as-directory path) nil)))
	(dirs (remove-if-not #'cl-fad:directory-pathname-p
			     (cl-fad:list-directory (cl-fad:pathname-as-directory 
						     (truename (pathname path)))))))
    (loop for dir in dirs
	do (push dir result))
    (loop for dir in dirs
	  do (setf result (append (get-subfolders dir) result)))
    (nreverse result)))

(defun get-directory-structure (path)
  (append (list (truename (cl-fad:pathname-as-directory path)))
	(loop for things in
		  (remove-if #'is-git-folder-p (cl-fad:list-directory path))
		collect (if (cl-fad:directory-pathname-p things)
			    (get-directory-structure things)
			    things))))

(defun flatten-list (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten-list structure))))

(defun get-type-structure (path-or-list type)
  (if (list-p path-or-list)
      ()
      ()))

(defun get-endtextsearch-structure (path-or-list text)
  (if (list-p path-or-list)
      ()
      ()))
