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
  (let ((name (values-from-config-list 'NAME config-structure))
	(description (values-from-config-list 'DESCRIPTION config-structure))
	(before-function-source (values-from-config-list 'BEFORE-FUNCTION-SOURCE config-structure))
	(after-function-source (values-from-config-list 'AFTER-FUNCTION-SOURCE config-structure)))
    
    (make-test-tag :name (car name)
		   :description (car description)
		   :before-function-source (car before-function-source)
		   :after-function-source (car after-function-source))))

(defun test-tag-to-config-structure (test-tag)
  (let ((config-structure ()))
    (with-accessors ((name name)
		     (description description)
		     (before-function-source before-function-source)
		     (after-function-source after-function-source)) test-tag

    (setf config-structure (add-value-to-config-key after-function-source 'after-function-source config-structure))
    (setf config-structure (add-value-to-config-key before-function-source 'before-function-source config-structure))
    (setf config-structure (add-value-to-config-key description 'description config-structure))
    (setf config-structure (add-value-to-config-key name 'name config-structure))
    
    config-structure)))

(defun test-tag-to-disk (file-pathname tag)
  (let* ((test-tag (cond ((typep tag 'test-tag) tag)
			 ((stringp tag) (gethash (string-upcase tag) *test-tags*))
			 (t (return-from test-tag-to-disk nil))))
	 (config (test-tag-to-config-structure test-tag)))

    (with-open-file (stream file-pathname
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "(")
    (loop for key in config
       do (format stream "~&(")
       do (format stream "~S~%" (first key))
	  (format stream "~{    ~S~^~%~}" (cdr key))
       do (format stream ")"))
    (format stream ")"))
    file-pathname))

(defun test-tag-from-disk (file-pathname)
  (let ((*read-eval* nil))
	(with-open-file (stream file-pathname
				:direction :input
				:if-does-not-exist nil)
	  
	 (config-structure-to-test-tag (read stream nil nil)))))

(defun add-before-function-to-tag (before-function-source tag)
  
  (let ((test-tag (cond ((typep tag 'test-tag) tag)
			((stringp tag) (gethash (string-upcase tag) *test-tags*))
			(t (return-from add-before-function-to-tag nil)))))
       
    (setf (before-function-source test-tag)
	  (cond ((null before-function-source) nil)
		((and (listp before-function-source) (not (equal (car before-function-source) 'lambda)))
		 (list 'lambda nil before-function-source))
		((and (listp before-function-source) (equal (car before-function-source) 'lambda))
		 before-function-source)
		(t 
		 (list 'lambda nil before-function-source))))

    (setf (before-function-compiled-form test-tag)
	  (eval (before-function-source test-tag)))

    test-tag))

(defun add-after-function-to-tag (after-function-source tag)  
  (let ((test-tag (cond ((typep tag 'test-tag) tag)
			((stringp tag) (gethash (string-upcase tag) *test-tags*))
			(t (return-from add-after-function-to-tag nil)))))
       
    (setf (after-function-source test-tag)
	  (cond ((null after-function-source) nil)
		((and (listp after-function-source) (not (equal (car after-function-source) 'lambda)))
		 (list 'lambda nil after-function-source))
		((and (listp after-function-source) (equal (car after-function-source) 'lambda))
		 after-function-source)
		(t 
		 (list 'lambda nil after-function-source))))

    (setf (after-function-compiled-form test-tag)
	  (eval (after-function-source test-tag)))

    test-tag))

(defun wrap-tag-in-functions (before-function after-function tag)
  (let ((test-tag (cond ((typep tag 'test-tag) tag)
			((stringp tag) (gethash (string-upcase tag) *test-tags*))
			(t (return-from wrap-tag-in-functions nil)))))
    (setf test-tag (add-before-function-to-tag before-function test-tag))
    (setf test-tag (add-after-function-to-tag after-function tag))
    
    test-tag))
