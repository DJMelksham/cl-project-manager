(defclass test ()
  (
   (expectation-table
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
    :initform (new-test-id)
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

(defmethod print-object ((object test) stream)
    (print-unreadable-object (object stream :type t)
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
		       (status status)
		       (result result)
		       (before-function-source before-function-source)
		       (after-function-source after-function-source)) object
	(format stream "~& TEST ID: ~a~& NAME: ~a~& FILE-ON-DISK: ~a~& TEST DESCRIPTION: ~a~& EXPECTATION TYPE: ~a~& TAGS: ~a~& TEST EXPECTED VALUE: ~a~&"		
		id name file-on-disk description expectation tags expected-value)

	(format stream "~& TEST SOURCE: ")
	(format stream "~{~a~^~&~}" (cddr source))
	(if run-value (format stream "~& LAST RUN VALUE OF TEST: ~a" run-value))
	(if run-time (format stream "~& RUN-TIME IN SECONDS: ~a" run-time))
	(if result (format stream "~& TEST PASSED ON LAST RUN?: ~a" result))
	(if before-function-source (format stream "~& SOURCE OF FUNCTION THAT RUNS BEFORE THE TEST: ~a" before-function-source))
	(if after-function-source (format stream "~& SOURCE OF FUNCTION THAT RUNS AFTER THE TEST: ~a" after-function-source)))))
