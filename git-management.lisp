(defun get-git-path-string ()
  (if (string= (software-type) "Linux")
      (string-trim 
       '(#\Space #\Newline #\Backspace #\Tab 
	 #\Linefeed #\Page #\Return #\Rubout)
       (handler-case 
	   (with-output-to-string (s) 
	     (sb-ext:run-program "/bin/sh" (list "-c" "command -v git") :output s)) 
	 (simple-error () nil)))))

(defun run-git (&optional (list-of-argument-strings nil) 
			   (directory (truename "~/")))
  (let* ((true-path (cl-fad:pathname-as-directory (truename (pathname directory))))
	 (true-name-string (namestring true-path)))
    (string-trim 
     '(#\Space #\Newline #\Backspace #\Tab 
       #\Linefeed #\Page #\Return #\Rubout)
     (handler-case (with-output-to-string (s) 
		     (sb-ext:run-program (get-git-path-string) 
					 list-of-argument-strings 
					 :output s
					 :directory true-name-string))
       (simple-error () nil)))))

(defun git-init (path)
  (let* ((existing-dir (ensure-directories-exist (cl-fad:pathname-as-directory path)))
	 (true-path (truename existing-dir))
	 (true-name-string (namestring true-path)))
      (values (run-git (list "init" true-name-string))
	      true-path)))

(defun git-add-all (path)
  (let* ((true-path (truename (cl-fad:pathname-as-directory path)))
	 (true-name-string (namestring true-path)))
    (values (run-git (list "add" "-A") true-name-string)
	    true-path)))

(defun git-status (path)
  (let* ((true-path (truename (cl-fad:pathname-as-directory path)))
	 (true-name-string (namestring true-path)))
    (values (run-git (list "status") true-name-string)
	    true-path)))

(defun git-commit (path &optional 
			  (message "Initial commit made automatically by cl-project-management"))
  (let* ((true-path (truename (cl-fad:pathname-as-directory path)))
	 (true-name-string (namestring true-path)))
    (values (run-git (list "commit" "-m" message) true-name-string)
	    true-path)))

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
