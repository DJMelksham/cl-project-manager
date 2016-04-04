(defclass test-tag ()
  ((name
    :initarg :name
    :initform (error "A tag must be created with a name")
    :type 'string
    :accessor name
    :documentation "The name of the test-tag")
   (description
    :initarg :description
    :initform "No description available"
    :type 'string
    :accessor description
    :documentation "A long form textual description of the tag and tests marked with it.")
   (before-function-source
    :initarg :before-function-source
    :initform nil
    :type 'list
    :accessor before-function-source
    :documentation "Source for a zero argument function that will be funcall'd before any tests accessed via this tag are run.")
   (before-function-compiled-form
    :initarg :before-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor before-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd  before any tests accessed via this tag are run.")
   (after-function-source
    :initarg :after-function-source
    :initform nil
    :type 'list
    :accessor after-function-source
    :documentation "Source for a zero argument function that will be funcall'd before any tests accessed via this tag are run.")
   (after-function-compiled-form
    :initarg :after-function-compiled-form
    :initform (lambda () nil)
    :type 'function
    :accessor after-function-compiled-form
    :documentation "A compiled zero argument function that will be funcall'd before any tests accessed via this tag are run")))

(defmethod print-object ((object test-tag) stream)
    (print-unreadable-object (object stream :type t)
      (with-accessors ((name name)
		       (description description)
		       (before-function-source before-function-source)
		       (after-function-source after-function-source)) object
	(format stream "~& NAME: ~a~& DESCRIPTION: ~a~&"		
		name description)
	(if before-function-source (format stream "~& SOURCE OF FUNCTION TO RUN BEFORE TAG: ~a"  before-function-source))
	(if after-function-source (format stream "~& SOURCE OF FUNCTION TO RUN AFTER TAG: ~a" after-function-source)))))

(defun make-test-tag (&key
			name
			description
			before-function-source
			after-function-source)

  (let ((real-name nil)
	(real-desc nil)
	(real-before-function-source nil)
	(real-compiled-before-function-form nil)
	(real-after-function-source nil)
	(real-compiled-after-function-form nil)
	(final-tag nil))

    ;;producing the test-tag name
    (if (not (stringp name))
	(progn
	  (format t "A test-tag must be created with a name, which must be a string.")
	   (return-from make-test-tag nil)))

    (setf real-name (string-upcase name))

    ;;producing description
    (if (or (not description)
	    (not (stringp description)))
	(setf real-desc "No valid description available.")
	(setf real-desc description))
    
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

	(setf final-tag (make-instance 'test-tag
				       :name real-name
				       :description real-desc 
				       :before-function-source real-before-function-source
				       :before-function-compiled-form real-compiled-before-function-form
				       :after-function-source real-after-function-source
				       :after-function-compiled-form real-compiled-after-function-form))
	
	(setf (gethash (name final-tag) *test-tags*) final-tag)
	
	final-tag))

(defun config-structure-to-test-tag (config-structure)
config-structure)

(defun test-tag-to-config-structure (config-structure)
  config-structure)

(defun test-tag-to-disk (test-tag)
test-tag)

(defun read-test-tag-from-disk (test-tag)
 test-tag)
