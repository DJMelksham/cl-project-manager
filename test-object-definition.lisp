(defclass test ()
  (
   (expectation-table
    :initform (let ((ex-table (make-hash-table :test #'equalp)))
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
    :accessor expectation-table
    :allocation :class
    :documentation "The class-based lookup table for expectation functions ")
   (id
    :initarg :id
    :initform (error "Test must be assigned a unique integer identifier")
    :accessor id
    :documentation "A unique integer identifying each test")
   (name
    :initarg :name
    :initform (error "A test must have a unique identifiable name.")
    :accessor name
    :documentation "Textual name of a test.  Must be unique.")
   (description
    :initarg :description
    :initform "No description text has been entered for this test")
    :accessor description
    :documentation "A long form textual description of the test."
   (expectation
    :initarg :expectation
    :initform (error "A test must include an expectation string to compare the result of running the test function")
    :accessor :expectation
    :documentation "A string representing the expectation of what happens/what function to use when the test actually runs")
   (tags
    :initarg :tags
    :initform nil
    :accessor tags
    :documentation "A list of tags applicable to the test")
   (source
    :initarg :source 
    :initform (error "A test must provide source code that defines the test")
    :accessor source
    :documentation "Source code that defines the function of the test")
   (compiled-form
    :initarg :compiled-form
    :initform (error "A test must provide a compiled-form of the function applied when the test is called")
    :accessor compiled-form
    :documentation "The compiled-form of the function applied when the test is called")
   (expected-value
    :initarg expected-value
    :initform (error "A test object must include an expected value for the result of the test")
    :accessor expected-value
    :documentation "The value required by the test in order to result in a pass.")
   (last-run-value
    :initarg last-run-value
    :initform nil
    :accessor last-run-value
    :documentation "The last value obtained (if any) when the test function was last applied successfully")
   (last-run-status
    :initarg last-run-status
    :initform nil
    :accessor last-run-status
    :documentation "A T or NIL flag to determine whether the test completed successfully when called")
   (last-run-test-result
    :initarg last-run-test-result
    :initform nil
    :accessor last-run-value
    :documentation "A T or NIL flag to determine whether the test passed or failed, respectively")
   (before-function-source
    :initarg :before-function-source
    :initform nil
    :accessor before-function-source
    :documentation "Source for a function that will be funcall'd before the test.")
   (before-function-compiled
    :initarg :before-function-compiled
    :initform nil
    :accessor before-function-compiled
    :documentation "A compiled function that will be funcall'd  before the test.")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :accessor after-function-source
    :documentation "Source for a function that will be funcall'd after the test.")
   (after-function-compiled
    :initarg :after-function-compiled
    :initform nil
    :accessor after-function-compiled
    :documentation "A compiled function that will be funcall'd after the test")))
