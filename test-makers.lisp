(defun register-test (test)

  (if (gethash (id test) *test-ids*)
	(error (concatenate 'string "A test with ID " (write-to-string (id test)) " is already registered. Change the ID before registering again, or deregister the other test first.")))

  (if (gethash (string-upcase (name test)) *test-names*)
	(error (concatenate 'string "A test named " (string-upcase (name test)) " is already registered. Change the nameof the test before registering again, or deregister the other test first.")))

  (setf (gethash (id test) *test-ids*) test)
  (setf (gethash (name test) *test-names*) test)
  (loop for tag in (tags test)
     do (hash-ext-array-insert tag test *test-tags*))

  test)

(defun make-test (&key 
		    id
		    name
		    file-on-disk
		    description
		    expectation
		    tags
		    source
		    expected-value
		    run-value
		    run-time
		    result
		    before-function-source
		    after-function-source)
  (let ((real-id (if (null id) (new-test-id) id))
	(real-name nil)
	(real-fod nil)
	(real-desc nil)
	(real-exp nil)
	(real-tags nil)
	(real-source nil)
	(real-compiled-form nil)
	(real-expected-value nil)
	(real-before-function-source nil)
	(real-compiled-before-function-form nil)
	(real-after-function-source nil)
	(real-compiled-after-function-form nil))

    ;;producing test name

    (if (not name) 
	(setf real-name (concatenate 'string "TEST-" (write-to-string real-id)))
	(setf real-name (string-upcase name)))
    
    (if (gethash real-name *test-names*)
	(progn
	  (format t "The name ~a is already registered to Test ID ~a~&" 
		  real-name
		  (id (gethash real-name *test-names*)))
	  (return-from make-test nil)))

    ;;producing a potential file-name for the test on disk 
	  
    (cond ((not file-on-disk) (setf real-fod (concatenate 'string real-name ".test")))
	  ((and (stringp file-on-disk) (ends-with-p file-on-disk ".test"))
	   (setf real-fod file-on-disk))
	  (t (setf real-fod (concatenate 'string file-on-disk ".test"))))
    
    ;;producing test file description
    
    (if (or (not description)
	    (not (stringp description)))
	(setf real-desc "No valid description has been supplied for this test.")
	(setf real-desc description))
    
    ;;producing test expectation
    ;;hardcoded test expectations for now...
    (cond ((null expectation) 
	   (setf real-exp "EQUALP"))
	  ((member (string-upcase expectation)
		   (list "EQ" "EQL" "EQUAL" "EQUALP" "NULL" "NOTNULL" "T" "CONDITION-OF-TYPE") :test #'equal)
	   (setf real-exp (string-upcase expectation)))
	  (t (progn
	       (format t "Expectation ~a is not a valid kind of expectation.~&" expectation)
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
    
    (cond ((and (not (listp tags))
		(not (stringp tags))
		(notevery #'stringp tags)) 
	   (setf real-tags nil))
	  ((stringp tags) 
	   (setf real-tags (list (string-upcase tags))))
	  ((typep tags 'sequence)
	   (setf real-tags 
		 (remove-duplicates (map 'list #'string-upcase tags) :test #'equalp)))
	  (t (setf real-tags nil)))
    
    ;;producing test source
    (cond ((null source)
	   (progn
	     (format t "You must provide a valid lisp expression that can be used for the test.")
	     (return-from make-test nil)))
	  ((and (listp source) (not (equal (car source) 'lambda)))
	   (setf real-source (list 'lambda nil source)))
	  ((and (listp source) (equal (car source) 'lambda))
	   (setf real-source source))
	  (t 
	   (setf real-source (list 'lambda nil source))))
    
    ;;producing test compiled form
    ;;assumes SBCL-esque default behaviour where everything is compiled unless otherwise
    ;;stated.  May need to be changed if this lisp code is ever made purely portable.
    (setf real-compiled-form (eval real-source))
    
    ;;producing test expected value
    (if (null expected-value)
	(setf real-expected-value T)
	(setf real-expected-value expected-value))
    
    ;;producing test before-function-source
    (cond ((null before-function-source)
	   (setf real-before-function-source nil))
	  ((and (listp before-function-source) (not (equal (car before-function-source) 'lambda)))
	   (setf real-before-function-source (list 'lambda nil before-function-source)))
	  ((and (listp before-function-source) (equal (car before-function-source) 'lambda))
	   (setf real-before-function-source before-function-source))
	  (t 
	   (setf real-before-function-source (list 'lambda nil before-function-source))))
    ;;producing test before-function-compiled
    (if (null real-before-function-source)
	(setf real-compiled-before-function-form *test-empty-function*)
	(setf real-compiled-before-function-form (eval real-before-function-source)))
    
    ;;producing test after-function-source
    (cond ((null after-function-source)
	   (setf real-after-function-source nil))
	  ((and (listp after-function-source) (not (equal (car after-function-source) 'lambda)))
	   (setf real-after-function-source (list 'lambda nil after-function-source)))
	  ((and (listp after-function-source) (equal (car after-function-source) 'lambda))
	   (setf real-after-function-source after-function-source))
	  (t 
	   (setf real-after-function-source (list 'lambda nil after-function-source))))
    ;;producing test after-function-compiled
    (if (null real-after-function-source)
	(setf real-compiled-after-function-form *test-empty-function*)
	(setf real-compiled-after-function-form (eval real-after-function-source)))

      (register-test  (make-instance 'test
		   :id real-id
		   :name real-name
		   :file-on-disk real-fod
		   :description real-desc
		   :expectation real-exp
		   :tags real-tags
		   :source real-source
		   :compiled-form real-compiled-form
		   :expected-value real-expected-value
		   :run-value run-value
		   :run-time run-time
		   :result result
		   :before-function-source real-before-function-source
		   :before-function-compiled-form real-compiled-before-function-form
		   :after-function-source real-after-function-source
		   :after-function-compiled-form real-compiled-after-function-form))))
      



(defun config-structure-to-test (config-structure)
  (let ((id (values-from-config-list 'ID config-structure))
	(name (values-from-config-list 'NAME config-structure))
	(file-on-disk (values-from-config-list 'FILE-ON-DISK config-structure))
	(description (values-from-config-list 'DESCRIPTION config-structure))
	(expectation (values-from-config-list 'expectation config-structure))
	(tags (values-from-config-list 'TAGS config-structure))
	(source (values-from-config-list 'SOURCE config-structure))
	(expected-value (values-from-config-list 'EXPECTED-VALUE config-structure))
	(run-value (values-from-config-list 'RUN-VALUE config-structure))
	(run-time (values-from-config-list 'RUN-TIME config-structure))
	(result (values-from-config-list 'RESULT config-structure))
	(before-function-source (values-from-config-list 'BEFORE-FUNCTION-SOURCE config-structure))
	(after-function-source (values-from-config-list 'AFTER-FUNCTION-SOURCE config-structure)))

	(make-test  
	 :id (car id) 
	 :name (car name)
	 :file-on-disk (car file-on-disk)
	 :description (car description)
	 :expectation (car expectation)
	 :tags (car tags)
	 :source (car source)
	 :expected-value (car expected-value)
	 :run-value (car run-value)
	 :run-time (car run-time)
	 :result (car result)
	 :before-function-source (car before-function-source)
	 :after-function-source (car after-function-source))))

(defun test-to-config-structure (test)
  (let ((config-structure ()))

    (with-accessors ((id id)
		     (name name)
		     (file-on-disk file-on-disk)
		     (description description)
		     (expectation expectation)
		     (tags tags)
		     (source source)
		     (expected-value expected-value)
		     (run-value run-value)
		     (run-time run-time)
		     (result result)
		     (before-function-source before-function-source)
		     (after-function-source after-function-source)) test
		     
      (setf config-structure (add-value-to-config-key after-function-source 'after-function-source config-structure))
      (setf config-structure (add-value-to-config-key before-function-source 'before-function-source config-structure))
      (setf config-structure (add-value-to-config-key expected-value 'expected-value config-structure))
      (setf config-structure (add-value-to-config-key result 'result config-structure))
      (setf config-structure (add-value-to-config-key run-time 'run-time config-structure))
      (setf config-structure (add-value-to-config-key run-value 'run-value config-structure))
      (setf config-structure (add-value-to-config-key source 'source config-structure))
      (setf config-structure (add-value-to-config-key tags 'tags config-structure))
      (setf config-structure (add-value-to-config-key expectation 'expectation config-structure))
      (setf config-structure (add-value-to-config-key description 'description config-structure))
      (setf config-structure (add-value-to-config-key file-on-disk 'file-on-disk config-structure))
      (setf config-structure (add-value-to-config-key name 'name config-structure))
      (setf config-structure (add-value-to-config-key id 'id config-structure))

      config-structure)))

(defun deregister-test (identifier)
  (let ((test (cond ((typep identifier 'test) identifier) 
		    ((integerp identifier) (gethash identifier *test-ids*))
		    ((stringp identifier) (gethash (string-upcase identifier) *test-names*))
		    (t (return-from deregister-test nil)))))
    (with-accessors ((id id)
		     (name name)
		     (tags tags)
		     (file-on-disk file-on-disk)) test
      (if (null identifier)
        (error "Test identifier cannot be null!"))

      (remhash id *test-ids*)
      (remhash name *test-names*)
      (loop for tag in tags
	 do (hash-ext-array-remove tag id *test-tags*))
      (remhash id *test-ids-paths*)
  
    test)))
