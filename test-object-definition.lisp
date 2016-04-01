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
		      (gethash "CONDITION-OF-TYPE" ex-table) (lambda (type function &rest arguments)
							       (multiple-value-bind (x y)
								   (ignore-errors (apply function arguments))
								 (if (null x)
								     (typep y type)))))
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
   (status
    :initarg :status
    :initform nil
    :accessor status
    :documentation "A T or NIL flag to determine whether the test completed successfully when called")
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
   (before-function-compiled
    :initarg :before-function-compiled
    :initform (lambda () nil)
    :type 'function
    :accessor before-function-compiled
    :documentation "A compiled zero argument function that will be funcall'd  before the test.")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :type 'list
    :accessor after-function-source
    :documentation "Source for a zero argument function that will be funcall'd after the test.")
   (after-function-compiled
    :initarg :after-function-compiled
    :initform (lambda () nil)
    :type 'function
    :accessor after-function-compiled
    :documentation "A compiled zero argument function that will be funcall'd after the test")))


;;; Simple example make-instance call, probably never to be used by user...
;;; Will put some interfacing macros and functions around it instead.

(make-instance 'test
	       :id 1
	       :name "Random Name"
	       :file-on-disk "test1"
	       :description "Creating a test test object"
	       :expectation "EQUALP"
	       :tags nil
	       :source '(lambda () 1)
	       :compiled-form (lambda () 1)
	       :expected-value T
	       :run-value nil
	       :status nil
	       :result nil
	       :before-function-source nil
	       :before-function-compiled (lambda () 1)
	       :after-function-source nil
	       :after-function-compiled (lambda () 1))
