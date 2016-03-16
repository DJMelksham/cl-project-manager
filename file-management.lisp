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

(defun get-path-folders-without-git (path &optional (inclusive nil))
  (remove-if #'git-folder-p (get-subfolders path inclusive)))

(defun get-module-folders ()
  (get-path-folders-without-git *active-module-path* t))

(defun get-directory-structure (path)
  (append (list (truename (cl-fad:pathname-as-directory path)))
	(loop for things in
		  (remove-if #'git-folder-p (cl-fad:list-directory path))
		collect (if (cl-fad:directory-pathname-p things)
			    (get-directory-structure things)
			    things))))
		     
(defun last-directory-equals-string (path string)
  (let ((real-string string))

    (if (not (equal (char string 0) #\/)) 
	(setf real-string (concatenate 'string "/" real-string)))

    (if (not (equal (char string (max 0 
				      (- (length string) 1))) 
		    #\/))
	(setf real-string (concatenate 'string real-string "/")))       

  (ends-with-p (directory-namestring path) real-string))) 
	       
(defun ensure-folders-exist-in-tree (path folder-name)
 (let* ((valid-dirs (remove-if 
		     (lambda (x) (last-directory-equals-string x folder-name))	       
		     (flatten-list (get-path-folders-without-git
				   path t)))))

   (loop for dir in valid-dirs
      do (ensure-directories-exist 
	  (cl-fad:merge-pathnames-as-directory
	   dir
	   (cl-fad:pathname-as-directory folder-name))))))

    
(defun path-from-name (name &optional  
			      (path *active-module-path*))
  
  (if (or (null path)
	  (null name)
	  (and (not (stringp name))
	       (not (symbolp name))))
      (return-from path-from-name nil))

  (let ((search-space (sort (cl-fad:list-directory path)
			    (lambda (x y) (cond ((and (cl-fad:directory-pathname-p x)
						      (not (cl-fad:directory-pathname-p y))) t)
						(t nil)))))
	(search-term (string name))
	(result nil))
    
    (loop 
       for item in search-space
       do (if (string= search-term (tail-of-path item))
	      (setf result item))
	 until (string= search-term (tail-of-path item)))

    (if (null result)
	(loop for item in search-space
	   do (if (string= search-term (concatenate 'string 
						    (tail-of-path item)
						    "."
						    (pathname-type item)))
		  (setf result item))
	     until (string= search-term (concatenate 'string 
						    (tail-of-path item)
						    "."
						    (pathname-type item)))))
    result))
