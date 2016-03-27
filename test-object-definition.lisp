(defclass test ()
  ((name
    :initarg :name
    :initform (error "A test must have a unique identifiable name.")
    :accessor name
    :documentation "Textual name of a test.  Must be unique.")
   (description
    :initarg :description
    :initform "No description has been entered for this test")
    :accessor description
    :documentation "A long form description of the test."
   (tags
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (test-source
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (test-compiled-form
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")
   (expected-value
    )
   (last-run-value
    )
   (last-run-status
    )
   (last-run-test-result
    )
   (before-function-source
    )
   (before-function-compiled-form
    )
   (after-function-source
    )
   (after-function-compiled-form
    )))
