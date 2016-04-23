(in-package #:peasant)

(defun get-git-path-string ()
  (if (string= (software-type) "Linux")
      (string-trim 
       '(#\Space #\Newline #\Backspace #\Tab 
	 #\Linefeed #\Page #\Return #\Rubout)
       (handler-case 
	   (with-output-to-string (s) 
	     (sb-ext:run-program "/bin/sh" (list "-c" "command -v git") :output s)) 
	 (simple-error () nil)))))

(defun path-in-git-project-p (path)
  (not (search "fatal: Not a git repository" 
	       (run-git (list "status") 
			(namestring (cl-fad:pathname-as-directory path))))))

(defun git-folder-p (path-to-be-searched)
 (let* ((true-path (truename (cl-fad:pathname-as-directory path-to-be-searched)))
	 (true-name-string (namestring true-path)))
   (if (and (cl-fad:directory-exists-p true-path)
	    (search "/.git/" true-name-string))
      T
      NIL)))
