(defclass test ()
  ((expectation-table
    :initform (let ((ex-table (make-hash-table :test #'equalp :size 8)))
		(setf (gethash "EQ" ex-table) #'eq
		      (gethash "EQL" ex-table) #'eql
		      (gethash "EQUAL" ex-table) #'equal
		      (gethash "EQUALP" ex-table) #'equalp
		      (gethash "NULL" ex-table) #'null
		      (gethash "NOTNULL" ex-table) (lambda (x) (NOT (NULL x)))
		      (gethash "T" ex-table) (lambda (x) (eq T x))
		      (gethash "CONDITION-OF-TYPE" ex-table) (lambda (type x)
							       (typep x type)))
		ex-table)
    :type 'hash-table
    :accessor expectation-table
    :allocation :class
    :documentation "The class-based lookup table for expectation functions ")
   (id
    :initarg :id
    :initform (error "A test needs an integer test-id")
    :type 'integer
    :accessor id
    :documentation "A unique integer identifying each test")
   (name
    :initarg :name
    :initform (error "A test must have a unique identifiable name.")
    :type 'string
    :accessor name
    :documentation "Textual name of a test.  Must be unique.")
   (file-on-disk
    :initarg :file-on-disk
    :initform nil
    :type 'string
    :accessor file-on-disk
    :documentation "The name of the test as written out to disk in the project's test folder")
   (description
    :initarg :description
    :initform "No description text has been entered for this test"
    :type 'string
    :accessor description
    :documentation "A long form textual description of the test.")
   (expectation
    :initarg :expectation
    :initform (error "A test must include an expectation string to compare the result of running the test function")
    :type 'string
    :accessor expectation
    :documentation "A string representing the expectation of what happens/what function to use when the test actually runs")
   (tags
    :initarg :tags
    :initform nil
    :type 'list
    :accessor tags
    :documentation "A list of tags applicable to the test")
   (source
    :initarg :source 
    :initform (error "A test must provide source code that defines the test")
    :type 'list
    :accessor source
    :documentation "Source code that defines the function of the test")
   (compiled-form
    :initarg :compiled-form
    :initform (error "A test must provide a compiled-form of the function applied when the test is called")
    :type 'function
    :accessor compiled-form
    :documentation "The compiled-form of the function applied when the test is called")
   (re-evaluate
    :initarg :re-evaluate
    :accessor re-evaluate
    :initform NIL
    :documentation "A T/NIL flag that determines whether a test should always re-evaluate its source code before running.  Helpful/necessary for testing code including user-defined macros.  However, comes with a performance hit.")
   (expected-value
    :initarg :expected-value
    :initform (error "A test object must include an expected value for the result of the test")
    :accessor expected-value
    :documentation "The value required by the test in order to result in a pass.")
   (run-value
    :initarg :run-value
    :initform nil
    :accessor run-value
    :documentation "The last value obtained (if any) when the test function was last applied successfully")
   (run-time
    :initarg :run-time
    :initform nil
    :accessor run-time
    :documentation "The time taken for the test to complete, including before/after functions.")
   (result
    :initarg :result
    :initform nil
    :accessor result
    :documentation "A T or NIL flag to determine whether the test passed or failed, respectively")
   (before-function-source
    :initarg :before-function-source
    :initform nil
    :type 'list
    :accessor before-function-source
    :documentation "Source for a zero argument function that will be funcall'd before the test.")
   (before-function-compiled-form
    :initarg :before-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor before-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd  before the test.")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :type 'list
    :accessor after-function-source
    :documentation "Source for a zero argument function that will be funcall'd after the test.")
   (after-function-compiled-form
    :initarg :after-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor after-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd after the test")))

(defgeneric serialise (pathname object)
  (:documentation "Serialse the given object to disk"))

(defmethod serialise (pathname (object test))
  (let ((local-pathname (if (cl-fad:directory-pathname-p pathname)
			    (cl-fad:merge-pathnames-as-file pathname (file-on-disk object))
			    pathname)))
    
    (with-open-file (stream local-pathname
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (print (list (cons 'NAME (name object))
		 (cons 'FILE-ON-DISK (concatenate 'string 
						  (pathname-name local-pathname)
						  (pathname-type local-pathname)))
		 (cons 'DESCRIPTION (description object))
		 (cons 'EXPECTATION (expectation object))
		 (cons 'TAGS (tags object))
		 (cons 'SOURCE (source object))
		 (cons 'RE-EVALUATE (re-evaluate object))
		 (cons 'EXPECTED-VALUE (expected-value object))
		 (cons 'RUN-VALUE (run-value object))
		 (cons 'RUN-TIME (run-time object))
		 (cons 'RESULT (result object))
		 (cons 'BEFORE-FUNCTION-SOURCE (before-function-source object))
		 (cons 'AFTER-FUNCTION-SOURCE (after-function-source object))) stream))
    
    local-pathname))

(defun load-test (pathname)
  (with-open-file (stream pathname
			  :direction :input
			  :if-does-not-exist :error)
    (let ((a-list (read stream)))
      (make-test :name (cdr (assoc 'NAME a-list))
		 :file-on-disk (cdr (assoc 'FILE-ON-DISK a-list))
		 :description (cdr (assoc 'DESCRIPTION a-list))
		 :expectation (cdr (assoc 'EXPECTATION a-list))
		 :tags (cdr (assoc 'TAGS a-list))
		 :source (cdr (assoc 'SOURCE a-list))
		 :expected-value (cdr (assoc 'EXPECTED-VALUE a-list))
		 :run-value (cdr (assoc 'RUN-VALUE a-list))
		 :run-time (cdr (assoc 'RUN-TIME a-list))
		 :result (cdr (assoc 'RESULT a-list))
		 :before-function-source (cdr (assoc 'BEFORE-FUNCTION-SOURCE a-list))
		 :after-function-source (cdr (assoc 'AFTER-FUNCTION-source a-list))))))

(defmethod print-object ((object test) stream)
  (print-unreadable-object (object stream)
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
		       (re-evaluate re-evaluate)
		       (status status)
		       (result result)
		       (before-function-source before-function-source)
		       (after-function-source after-function-source)) object

	(cond ((eq *print-verbosity* 'high)
	       (progn
		 (format stream "-------------------->~& ID: ~a~& NAME: ~a~& DESCRIPTION: ~a~& TAGS: ~a~& RE-EVALUATE EACH RUN: ~a"		id name description tags re-evaluate)
		 (format stream "~& SOURCE: ")
		 (format stream "~{~a~^~&~}" (cddr source))
		 (format stream "~& EXPECTATION: ~a" expectation)
		 (format stream "~& EXPECTED VALUE: ~a" expected-value)
		 (if run-value (format stream "~& RUN VALUE: ~a" run-value))
		 (format stream "~& TEST PASSED: ~a" result)
		 (if run-time (format stream "~& RUN-TIME IN SECONDS: ~a" run-time))
		 (if before-function-source (format stream "~& FUNCTION THAT RUNS BEFORE THE TEST: ~a" before-function-source))
		 (if after-function-source (format stream "~& FUNCTION THAT RUNS AFTER THE TEST: ~a" after-function-source))))
	      ((eq *print-verbosity* 'medium) 
	       (progn
		 (format stream "---------------------~& ***   ~a   *** ~& ID: ~a | NAME: ~a " (if result "PASSED!" "FAILED!") id name))
		 (format stream "~& SUMMARY: (~a ~a ~a)" expectation expected-value run-value))
	      ((eq *print-verbosity* 'low)
	       (format stream " TEST ~a ~a " id (if result "PASS" "FAIL")))))))
	    
