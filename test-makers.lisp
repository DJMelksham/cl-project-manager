(defun make-test (&key 
		    name
		    file-on-disk
		    description
		    expectation
		    tags
		    source
		    expected-value
		    before-function-source
		    after-function-source)
  (let* ((real-id (new-test-id))
	 (real-name nil)
	 (real-fod nil)
	 (real-desc nil)
	 (real-exp nil)
	 (real-tags nil)
	 (real-source nil)
	 (real-compiled-source nil)
	 (real-expected-value nil)
	 (real-before-function-source nil)
	 (real-compiled-bfs nil)
	 (real-compiled-afs nil))

;;producing test name

 (if (not name) 
     (setf real-name (concatenate 'string "TEST-" (write-to-string id)))
     (setf real-name (string-upcase name)))
 
 (if (gethash real-name *test-names*)
     (progn
       (format t "The name ~a is already registered to Test ID ~a~&" 
	       real-name
	       (id (gethash real-name *test-names*)))
       (return-from make-test nil)))

;;producing test file-name on disk 

 (if (not file-on-disk)
     (setf real-fod (concatenate 'string real-name ".test"))
     (setf real-fod (concatenate 'string file-on-disk ".test")))
 
 (if (probe (cl-fad:merge-pathnames-as-file *active-module-path* real-fod))
     (progn
       (format t "A file or test named ~a was already found at ~a~&" 
	       real-fod
	       (cl-fad:merge-pathnames-as-file *active-module-path* real-fod))
       (return-from make-test nil)))
 
;;producing test file description
 
 (if (or (not description)
	 (not (stringp description)))
     (setf real-desc "No valid description has been supplied for this test.~&")
     (setf real-desc description))

;;producing test expectation
;hardcoded test expectations for now...
 (cond ((null expectation) 
	(setf real-exp "EQUALP"))
       ((member (string-upcase expectation)
		(list "EQ" "EQL" "EQUAL" "EQUALP" "NULL" "NOTNULL" "T" "CONDITION-OF-TYPE"))
	(setf real-exp (string-upcase expectation)))
       (t (progn
	    (format t "Expectation ~a is not a valid kind of expectation.~&" exp)
	    (format t "Expectation must be one of ~S, ~S, ~S, ~S, ~S, ~S, ~S, or ~S.~&"
		    "EQ"
		    "EQL"
		    "EQUAL"
		    "EQUALP"
		    "NULL"
		    "NOTNULL"
		    "T"
		    "CONDITION-OF-TYPE")
	    (return-from make-test nil))))

;;producing test tags

 (if (or (not (listp tags))
	 (notevery #'stringp tags))
     (setf real-tags nil)
     (setf real-tags tags))

;;producing test source
 (cond ((null source)
	(progn
	  (format t "You must provide a valid lisp expression that can be used for the test.")
	  (return-from make-test nil)))
       ((and (listp source) (not equal (car source) 'lambda))
	(setf real-source (list 'lambda nil source)))
       ((and (listp source) (equal (car source) 'lambda))
	(setf real-source source))
       (t 
	(setf real-source (list 'lambda nil source))))

;;producing test compiled source
;;assumes SBCL-esque default behaviour where everything is compiled unless otherwise
;;stated.  May need to be changed if this lisp code is ever made purely portable.
 (setf real-compiled-source (eval real-source))

;;producing test expected value
 (if (null expected-value)
     (setf real-expected-value T)
     (setf real-expected-value expected-value))

;;producing test before-function-source
 (cond ((null before-function-source)
        (setf real-before-function-source nil))
       ((and (listp before-function-source) (not equal (car before-function-source) 'lambda))
	(setf real-before-function-source (list 'lambda nil before-function-source)))
       ((and (listp before-function-source) (equal (car before-function-source) 'lambda))
	(setf real-before-function-source before-function-source))
       (t 
	(setf real-before-function-source (list 'lambda nil before-function-source))))
;;producing test before-function-compiled
 (if (null real-before-function-source)
     nil
     (setf real-compiled-before-function-source (eval real-before-function-source)))

 
;;producing test after-function-source

;;producing test after-function-compiled

nil
)

(defun delete-test (identifier)
  (let ((test (cond ((integerp identifier) (gethash identifier *test-ids*))
		    ((stringp identifier) (gethash identifier *test-names*))
		    (t (return-from delete-test nil)))))
    (with-accessors ((id id)
		     (name name)
		     (tags tags)
		     (file-on-disk file-on-disk)) test

      (delete-file (gethash id *test-paths*))
      (deregister-test test))))

(defun config-structure-to-test (config-structure)

)

(defun test-to-config-structure (test)

)

(defun make-test-config-structure (&key 
				     id 
				     name
				     file-on-disk
				     description
				     expectation
				     tags
				     source
				     expected-value
				     before-function-source
				     after-function-source)
  (let ((result ()))
    (labels ((add-if (x y)
	       (if x 
		   (setf result (add-value-to-config-key
				  x
				  y
				  result)))))

      (add-if after-function-source 'after-function-source)
      (add-if before-function-source 'before-function-source)
      (add-if expected-value 'expected-value)
      (add-if source 'source)
      (add-if tags 'tags)
      (add-if expectation 'expectation)
      (add-if description 'description)
      (add-if file-on-disk 'file-on-disk)
      (add-if name 'name)
      (add-if id 'id)

      result)))
