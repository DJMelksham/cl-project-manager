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
