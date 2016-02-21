(defun get-subfolders (path &optional (inclusive nil))
  (let ((result (if inclusive (list (pathname path) nil)))
	(dirs (remove-if-not #'cl-fad:directory-pathname-p
			     (cl-fad:list-directory path))))
    (loop for dir in dirs
	do (push dir result))
    (loop for dir in dirs
	  do (setf result (append (get-subfolders dir) result)))
    (nreverse result)))

(defun truename-string (path)
  (namestring (truename path)))
