(defun make-test-folder (&optional 
			   (module-path *active-module-path*) 
			   (test-dir-name *test-dir-name*))
  (ensure-directories-exist (merge-pathnames 
			     (cl-fad:pathname-as-directory test-dir-name) module-path)))
