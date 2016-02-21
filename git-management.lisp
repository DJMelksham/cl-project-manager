(defun is-git-project-p (path)

)

(defun is-in-git-project-p (path)

)


(defun run-git (list-of-argument-strings)
    (string-trim 
     '(#\Space #\Newline #\Backspace #\Tab 
       #\Linefeed #\Page #\Return #\Rubout)
     (handler-case (with-output-to-string (s) 
		     (sb-ext:run-program get-git-path-string 
					 list-of-argument-strings 
					 :output s))
       (simple-error () nil))))

(defun get-git-path-string ()
  (if (string= (software-type) "Linux")
      (string-trim 
       '(#\Space #\Newline #\Backspace #\Tab 
	 #\Linefeed #\Page #\Return #\Rubout)
       (handler-case 
	   (with-output-to-string (s) 
	     (sb-ext:run-program "/bin/sh" (list "-c" "command -v git") :output s)) 
	 (simple-error () nil)))))

(defun git-init (path)
  (let* ((true-path (truename (pathname path)))
	 (true-name-string (namestring true-path)))
  (prin1 (run-git (list "init" true-name-string)))
  true-path))

(defun git-add-all (path)
  (prin1 (run-get (list "add" "-A")))
  (true-path path)
)

(defun git-commit (path message)

)

(defun git-initialise-project (path &optional project-name)

)

(defun is-git-folder-p (path-to-be-searched)
 (let ((path-string (namestring path-to-be-searched))
       (path (pathname path-to-be-searched)))
   (if (and (cl-fad:directory-pathname-p path)
	    (search "/.git/" path-string))
      T
      NIL)))

(defun get-subfolders-without-git (path)
  (remove-if #'is-git-folder-p (get-subfolders path)))

