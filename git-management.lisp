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
  (let* ((true-path (truename (pathname path)))
	 (true-name-string (namestring true-path)))
      (values (run-git (list "init" true-name-string))
	      true-path)))

(defun git-add-all (path)
  (let* ((true-path (truename (pathname path)))
	 (true-name-string (namestring true-path)))
    (values (run-git (list "add" "-A") true-name-string)
	    true-path)))

(defun git-status (path)
  (let* ((true-path (truename (pathname path)))
	 (true-name-string (namestring true-path)))
    (values (run-git (list "status") true-name-string)
	    true-path)))

(defun git-commit (path message)
  (let* ((true-path (truename (pathname path)))
	 (true-name-string (namestring true-path)))
    (values (run-git (list "commit" "-m") true-name-string)
	    true-path)))

(defun is-git-folder-p (path-to-be-searched)
 (let* ((true-path (cl-fad:pathname-as-directory (truename (pathname path-to-be-searched))))
	 (true-name-string (namestring true-path)))
   (if (and (cl-fad:directory-exists-p true-path)
	    (search "/.git/" true-name-string))
      T
      NIL)))

(defun get-subfolders-without-git (path)
  (remove-if #'is-git-folder-p (get-subfolders path)))

(defun is-git-project-p (path)
  (or (cl-fad:directory-exists-p 
       (cl-fad:merge-pathnames-as-directory 
	(cl-fad:pathname-as-directory path)
	(cl-fad:pathname-as-directory ".git/")))
      (string= (run-git (list "status") (path))
)

(defun is-in-git-project-p (path)

)
