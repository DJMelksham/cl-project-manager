(defun make-git-ignore-text (&optional (additional-list-of-text nil))
  (let ((string (format nil "~{~a~%~}" 
			(append (list "#Ignore emacs editor temporary files"
				      "[#]*[#]"
				      "*~"
				      ""
				      "#Ignore cl-project-manager files"
				      *module-config-name*)
				additional-list-of-text))))
    string))

(defun create-licence-text ()
  (let ((string "All Rights Reserved."))
    string))

(defun create-readme-text (project-name authors) 
  (format nil "~{~a~%~}"
	  (list (concatenate 'string "## " (string-upcase project-name))
		""
		"At the top of the file there should be a short introduction and/ or overview that explains **what** the project is."
		""
		"## Motivation"
		""
		"A short description of the motivation behind the creation and maintenance of the project. This should explain **why** the project exists."
		""
		"## Installation"
		""
		"Load the generated file 'load-project.lisp' in your common lisp repl"
		""
		"## Author"
		""
		authors)))
