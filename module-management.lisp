(in-package #:peasant)

(defun module-p (path &optional (module-config *module-config-name*))  
  (if (cl-fad:directory-pathname-p path) 
      (if (probe-file (merge-pathnames module-config path))
	  t
	  nil)))

(defmacro return-nil-if-no-active-module (function-name)
  `(if (null *active-module-path*)
       (progn
	 (format t "Currently no active module!")
	 (return-from ,function-name nil))))

(defun update-config (new-config)

  (return-nil-if-no-active-module update-config)

    (setf *active-module-config* new-config)
    (config-to-disk *active-module-path* *active-module-config*))

(defun make-module-config (path)
  (let ((config nil))

    (setf config 
	  (designate-test-folder *test-dir-name* config))
    
    (setf config 
	  (designate-load-file (concatenate 'string "load-" (tail-of-path path) ".lisp") config))

    (config-to-disk path config)))

(defun parent-modules (&optional (path *active-module-path*) (module-config *module-config-name*))
  
  (if (null path)
      (return-from parent-modules nil))

  (let ((result nil)
	(current-dir path)
	(parent-dir (cl-fad:pathname-parent-directory path)))
    
    (loop until (or (not (module-p parent-dir module-config))
		    (equal parent-dir current-dir))
       do (progn 
	    (push parent-dir result)
	    (setf current-dir parent-dir
		  parent-dir (cl-fad:pathname-parent-directory current-dir))))
    result))
 
(defun child-modules (&optional (path *active-module-path*))
  (loop for dir in (get-path-folders-without-git path)
       if (module-p dir)
       collect dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-export (symbol-ident)

  (return-nil-if-no-active-module add-export)  
  (let ((sym (string-upcase (string symbol-ident))))
  (update-config (add-value-to-config-key sym 'EXPORT *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-export (symbol-ident)
  (return-nil-if-no-active-module remove-export)
  (update-config (remove-value-from-config-key (string-upcase (string symbol-ident)) 'EXPORT *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun add-shadow (symbol-ident)

  (return-nil-if-no-active-module add-shadow)  
  (let ((sym (string-upcase (string symbol-ident))))
  (update-config (add-value-to-config-key sym 'SHADOW *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-shadow (symbol-ident)
  (return-nil-if-no-active-module remove-shadow)
  (update-config (remove-value-from-config-key (string-upcase (string symbol-ident)) 'SHADOW *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun add-use (library-name)

  (return-nil-if-no-active-module add-use)  
  (let ((lib (string-upcase (string library-name))))
  (update-config (add-value-to-config-key lib 'USE *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-use (library-name)
  (return-nil-if-no-active-module remove-use)
  (update-config (remove-value-from-config-key (string-upcase (string library-name)) 'USE *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun add-library-alias (library-name alias)

  (return-nil-if-no-active-module add-library-alias)  
  (let ((lib-alias-list (cons (string-upcase (string library-name))
			      (string-upcase (string alias)))))
    (update-config (add-value-to-config-key lib-alias-list 'LIBRARY-ALIAS *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-library-alias (library-name alias)
  (return-nil-if-no-active-module remove-library-alias)
  (update-config (remove-value-from-config-key (cons (string-upcase (string library-name))
						     (string-upcase (string alias))) 
					       'LIBRARY-ALIAS *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun add-import (library-name symbol-ident)

  (return-nil-if-no-active-module add-import)  
  (let ((lib (cons (string-upcase (string library-name))
		   (string-upcase (string symbol-ident)))))
    (update-config (add-value-to-config-key lib 'IMPORT *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-import (library-name symbol-ident)
  (return-nil-if-no-active-module remove-import)
  (update-config (remove-value-from-config-key (cons (string-upcase (string library-name))
						     (string-upcase (string symbol-ident))) 
					       'IMPORT *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun add-shadow-import (library-name symbol-ident)

  (return-nil-if-no-active-module add-shadow-import)  
  (let ((lib (cons (string-upcase (string library-name))
		   (string-upcase (string symbol-ident)))))
    (update-config (add-value-to-config-key lib 'SHADOW-IMPORT *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-shadow-import (library-name symbol-ident)
  (return-nil-if-no-active-module remove-shadow-import)
  (update-config (remove-value-from-config-key (cons (string-upcase (string library-name))
						     (string-upcase (string symbol-ident))) 
					       'SHADOW-IMPORT *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)


(defun add-quicklisp-lib (library-name)

  (return-nil-if-no-active-module add-quicklisp-lib)  
  (let ((lib (string-upcase (string library-name))))
  (update-config (add-value-to-config-key lib 'QUICKLISP *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T))

(defun remove-quicklisp-lib (library-name)
  (return-nil-if-no-active-module remove-quicklisp-lib)
  (update-config (remove-value-from-config-key (string-upcase (string library-name)) 'QUICKLISP *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun add-invisibility (identifier)
  (return-nil-if-no-active-module add-invisibility) 

  (let ((ident (path-from-name identifier)))
    
    (cond ((value-in-config-p identifier 'INVISIBLE *active-module-config*)
	   (progn
	     (format t "Identifier already invisible in ~a." (tail-of-path *active-module-path*))
	     nil))
	  ((not ident)
	   (progn
	     (format t "Unable to find identifier in ~a.  Cannot make ~a invisible." 
		     (tail-of-path *active-module-path*)
		     identifier)
	     (return-from add-invisibility nil)))
	  (t (progn 
	       (update-config (add-value-to-config-key identifier 'INVISIBLE *active-module-config*))
	       (format t "Object functionally invisible to cl-project-manager in ~a:~%~a." (tail-of-path *active-module-path*)
		     ident))))))

(defun remove-invisibility (identifier)
  (return-nil-if-no-active-module remove-invisibility)
  (update-config (remove-value-from-config-key identifier 'INVISIBLE *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  T)

(defun remove-priority (identifier)
  (return-nil-if-no-active-module remove-priority)
  (update-config (remove-value-from-config-key (string-upcase (string identifier)) 'PRIORITY *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work

  t)

(defun reset-priority ()
  (return-nil-if-no-active-module reset-priority)
  (update-config (remove-key-from-config-list 'PRIORITY *active-module-config*))
  
  ;;edit later to perform checks and things before adding to config
  ;;return nil if there is some reason it didn't work
  
  t)

(defun insert-priority (identifier &optional (priority-position 0))
  (return-nil-if-no-active-module insert-priority)
  
  (let ((ident (path-from-name identifier)))
    
    (cond ((null ident)
	   (progn
	     (format t "Unable to find identifier in ~a.  Unable to prioritise." 
		     (tail-of-path *active-module-path*))
	     (return-from insert-priority nil)))
	  (t (progn 
	       (update-config (insert-value-into-config-key priority-position identifier 'PRIORITY *active-module-config*))
	       (format t "~a prioritised at position ~a by cl-project-manager in ~a:~%" 
		       ident
		       (position identifier (values-from-config-list 'PRIORITY *active-module-config*) :test #'equal)
		       (tail-of-path *active-module-path*))
	       t)))))

(defun set-explicit-priority-list (&rest identifiers)
  (return-nil-if-no-active-module set-explicit-priority-list)
  
  (reset-priority)

  (let ((idents (loop for ident in identifiers
		     collect (path-from-name ident)))
	(items-to-insert nil))
    
    (loop 
       for item in identifiers
       for paths in idents
       do (cond ((null paths)
		 (format t "Unable to find identifier ~a in ~a.  Unable to prioritise.~%" 
			 item
			 (tail-of-path *active-module-path*)))
		(t (push item items-to-insert))))
    
    (loop for item in items-to-insert
	 do (insert-priority item))))

(defun priority? ()
  (return-nil-if-no-active-module priority?)
  (let ((priority-values (values-from-config-list 'PRIORITY *active-module-config*)))
    (format t "PRIORTIES FOR ~a MODULE:~%" (string-upcase (tail-of-path *active-module-path*)))
    (format t "~{~a~%~}" priority-values)))

(defun real-priority? ()
  (return-nil-if-no-active-module real-priority?)
  (let ((priority-values (loop 
			    for items in (values-from-config-list 'PRIORITY *active-module-config*)
			    collect (path-from-name items))))
    (format t "REAL PRIORTIES FOR ~a MODULE:~%" (string-upcase (tail-of-path *active-module-path*)))
    (format t "~{~a~%~}" priority-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-module ()
  (let ((result t))
    
    (if (not (validate-config-structure *active-module-config*))
	(setf result nil))
    
    (setf *active-module-validated* result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun active-module (&optional (module-identifier nil))

  (cond ((null module-identifier) 
	 *active-module-path*)
	((and (not (stringp module-identifier))
	      (not (symbolp module-identifier))
	      (not (pathnamep module-identifier)))
	 (return-from active-module *active-module-path*))
	((and (not (symbolp module-identifier))
	      (cl-fad:directory-exists-p module-identifier)
	      (module-p module-identifier))
	 (setf *active-module-path* (cl-fad:pathname-as-directory module-identifier)))
	((and (path-from-name module-identifier) 
	      (cl-fad:directory-pathname-p (path-from-name module-identifier))
	      (module-p (path-from-name module-identifier)))
	 (setf *active-module-path* (path-from-name module-identifier)))
	(t (progn (print "Unable to establish new active module.")
		  (return-from active-module nil))))
  
  (setf *active-module-config* (config-from-disk *active-module-path*))
  
  (validate-module)

*active-module-path*)


;;; Seems to be a bug in the .gitignore creation.  My file in my gittest folder had nothing in it.
;;;  Fix it yo' fool!
(defun make-module (path &key (make-active t) (git-ignore-file t) (template-documents t) (init-git t))

  (let* ((dir-path (cl-fad:pathname-as-directory path))
	 (test-path (merge-pathnames (cl-fad:pathname-as-directory *test-dir-name*) dir-path))
	 (git-ignore (merge-pathnames dir-path ".gitignore"))
	 (licence (merge-pathnames dir-path "LICENCE"))
	 (README (merge-pathnames dir-path "README.md")))
    
    (if (module-p dir-path)
	(progn
	  (format t "~a is already a managed module!" dir-path) 
	  (return-from make-module dir-path)))
    
    ;;Prompt the user to be sure as to their request
    (if (probe-file dir-path)
	(format t "Note: The path '~a' already exists.~%" dir-path)
	(format t "The path '~a' does not exist and will be created if you proceed." dir-path))
    
    (format t
	    "~%Common Lisp Project Manager will define a module in ~a~%with the following basic settings:~%
   Make the module active after being created: ~a
   Place a default .gitignore file in module: ~a
   Place some helper template documents (licence/Readme) in the module: ~a
   Author to be added in the Readme if created: ~a
   Initialise a new git repository and first commit in module: ~a~%"
	    dir-path
	    make-active
	    git-ignore-file
	    template-documents
	    *authors*
	    init-git)
    (format *query-io* 
	    "~%Are you sure you wish to make the resulting managed Common Lisp module (Y/N?): ")
    (force-output *query-io*)
    (if (not (string= (string-upcase (read-line *query-io*)) "Y"))
	(progn 
	  (format t "Aborting module creation.")
	  (return-from make-module)))
    
    ;;create the module directory
    (ensure-directories-exist dir-path :verbose t)
    ;;create the module test directory
    (ensure-directories-exist test-path :verbose t)
    
    (if git-ignore-file
	(if (probe-file git-ignore)
	    (format t "~&.gitignore file already exists.  I'm gonna leave it alone.~&")
	    (with-open-file (output-file git-ignore
					 :direction :output
					 :if-does-not-exist :create
					 :if-exists nil)
	      (prin1 (make-git-ignore-text) output-file))))
    
    (if template-documents
	(if (probe-file licence)
	    (format t "~&licence file already exists.  I'm gonna leave it alone.~&")
	    (with-open-file (output-file licence
				     :direction :output
				     :if-does-not-exist :create
				     :if-exists nil)
	      (prin1 (create-licence-text) output-file)))
	
	(if (probe-file README)
	    (format t "~&README.md file already exists.  I'm gonna leave it alone.~&")
	    (with-open-file (output-file readme
					 :direction :output
					 :if-does-not-exist :create
					 :if-exists nil)
	      (prin1 (create-readme-text (tail-of-path dir-path) *authors*) output-file))))
    
    ;; Create the module's basic configuration file - which identifies it as a managed module
    (make-module-config dir-path) 

    (if init-git
	(if (not (path-in-git-project-p dir-path))
	    (progn
	      (git-init dir-path)
	      (git-add-all dir-path)
	      (git-commit dir-path))
	    (format t "~&Module is already part of a .git repository.  I'm gonna leave the .git repository alone.~&")))
    
    (if (and make-active
	     (module-p dir-path))
	(active-module dir-path))
    
    dir-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun up-module ()
  (let ((parent-module (car (parent-modules))))
    (cond ((null *active-module-path*)
	   (progn
	     (print "No active module currently defined. No parent module available.")
	     (return-from up-module *active-module-path*)))
	  ((null parent-module)
	   (progn
	     (format t "~a is a top-level module." (tail-of-path *active-module-path*))
	     (return-from up-module *active-module-path*)))
	  (t (active-module parent-module)))))

(defun down-module (&optional (module-identifier nil))
  (cond ((null *active-module-path*)
	 (progn
	   (print "No active module currently defined. No sub-modules available.")
	   (return-from down-module *active-module-path*)))
	((null module-identifier)
	 (progn
	   (print "Please provide a module identifier. No sub-modules available. ")
	   (return-from down-module *active-module-path*)))
	((or (null (path-from-name module-identifier))
	     (not (module-p (path-from-name module-identifier))))
	 (progn
	   (format t "Identifier does not match any module in ~a" (tail-of-path *active-module-path*))
	   (return-from down-module *active-module-path*)))
	(t (active-module (path-from-name module-identifier)))))
