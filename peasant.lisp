(in-package #:peasant)

(defvar *active-project* nil)
(defvar *active-project-path* nil)
(defvar *test-folder-name* "tests/")
(defvar *test-path* nil)

(defun active-project (system-keyword &key test-path)
    ;;set active project
    ;;also make sure active-ness is set in gittest and testy?
  (if (and (symbolp *active-project*)
	   (pathnamep *active-project-path*)
	   (pathnamep *test-path*)
	   (not (= (testy:stat-number-tests (testy:all-tests)) 0))
	   (y-or-n-p (concatenate
		      'string
		      "Sir, you already have an active project "
		      (string *active-project*)
		      " with tests defined.  Write currently loaded tests to disk before changing projects?")))
      (progn
	(testy:delete-all-tests-in-dir *test-path*)
	(testy:serialise-tests *test-path*)))

      (testy:deregister-tests (testy:all-tests))

      (setf *active-project* system-keyword)
      (setf *active-project-path* (asdf:system-source-directory system-keyword))

      (if (null test-path)
	  (setf *test-path*
		(uiop:merge-pathnames*
		 (uiop:ensure-directory-pathname *test-folder-name*)
		 (asdf:system-source-directory system-keyword))))
      (asdf/cl:ensure-directories-exist *test-path*)

      (testy:set-testy-active-project *test-path* *active-project*)

      (gittest:set-gittest-active-directory *active-project-path*)

      (testy:load-tests)

      (format t "~&ACTIVE PROJECT: ~a~&PROJECT PATH: ~a~&TESTS PATH: ~a~&~a TESTS LOADED"
	      *active-project*
	      *active-project-path*
	      *test-path*
	      (testy:stat-number-tests (testy:all-tests))))
      

(defun make-project
    (pathname &key
		(init-git t)
		remote
		depends-on
		template-parameters
		(TEMPLATE-DIRECTORY *TEMPLATE-DIRECTORY*)
		(AUTHOR *AUTHOR*)
		LICENCE 
   		(INCLUDE-COPYRIGHT *INCLUDE-COPYRIGHT*))

  ;;make a project using quick-project
  (let* ((make-project-argument-call ())
	 (quicklisp-local-string (uiop:native-namestring (car quicklisp:*local-project-directories*)))
	 (pathname-dir (uiop:ensure-directory-pathname pathname))
	 (pathname-string (uiop:native-namestring pathname-dir))
	 (project-name (first (last (pathname-directory pathname-dir))))
	 (gittest:*git-project-path* pathname-dir))

    (print pathname-dir)
    (push pathname-string make-project-argument-call)
    (if depends-on
	(setf make-project-argument-call
	      (append (list depends-on :depends-on) make-project-argument-call)))
    (if template-parameters
	(setf make-project-argument-call
	      (append (list template-parameters :template-parameters) make-project-argument-call)))
    (if depends-on
	(setf make-project-argument-call
	      (append (list depends-on :depends-on) make-project-argument-call)))
    (if template-directory
	(setf make-project-argument-call
	      (append (list template-directory :template-directory) make-project-argument-call)))
    (if author
	(setf make-project-argument-call
	      (append (list author :author) make-project-argument-call)))
    (if licence
	(setf make-project-argument-call
	      (append (list licence :license) make-project-argument-call)))
    (if include-copyright
	(setf make-project-argument-call
	      (append (list include-copyright :include-copyright) make-project-argument-call)))
    
    (setf make-project-argument-call (reverse make-project-argument-call))
    
    (apply #'quickproject:make-project make-project-argument-call)
    
    ;;place a shortcut in the local-project's path of quick-lisp
    (inferior-shell:run/s (concatenate 'string "ln -s " "'" pathname-string "' '" quicklisp-local-string "Link to " project-name "'"))
    
    ;;initialise as a git repository and make an initial commit
    (if init-git
	(make-git))
    
    ;;add the remote URL to the git repository
    (if remote
	(git-add-remote remote))
    
    
    ;;ensure test folder exists
    
    (uiop:ensure-all-directories-exist (list (uiop:merge-pathnames*
					      (uiop:ensure-directory-pathname *test-folder-name*) pathname-dir)))))

(defun save-project (&key 
		       (write-tests t)
		       (message "Automatic peasant commit!")
		       (remote "origin")
		       (branch (git-branch)))

  (let ((tests-written 0)
	(git-commit-before-save (string-right-trim 
				 '(#\Space #\Newline #\Backspace #\Tab 
				   #\Linefeed #\Page #\Return #\Rubout)
				 (gittest:git "rev-parse HEAD")))
	(git-commit-after-save ""))
  
  ;; write tests
  (if write-tests
      (progn
	(testy:delete-all-tests-in-dir *test-path*) 
	(setf tests-written (testy:serialise-tests *test-path*))))

  ;; git add commit push
  (git-add-commit-push :message message :remote remote :branch branch)

  (setf git-commit-after-save (string-trim 
			       '(#\Space #\Newline #\Backspace #\Tab 
				 #\Linefeed #\Page #\Return #\Rubout)
			       (gittest:git "rev-parse HEAD")))
  ;;
  (format t "~&ACTIVE PROJECT: ~a~&PROJECT PATH: ~a~&TESTS PATH: ~a~&~a TESTS SAVED~&PREVIOUS GIT COMMIT: ~a~&LATEST GIT COMMIT: ~a~& "
	      *active-project*
	      *active-project-path*
	      *test-path*
	      tests-written
	      git-commit-before-save
	      git-commit-after-save)

  (if (equal git-commit-before-save git-commit-after-save)
      (format t "Something appears to have gone wrong with the commit.  Before and after commits are still the same!"))

  (if (not (search git-commit-after-save (gittest:git (concatenate 'string "ls-remote " branch))))
      (format t "Something appears to have gone wrong with pushing to the remote server. 
Commit hashes of local and remote are not lining up like I think they should."))))
							     
  
