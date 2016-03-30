(defun expect-eq () 
 nil)

(defun expect-eql ()
 nil)

(defun expect-equal ()
  nil)

(defun expect-equalp ()
  nil)

(defun register-test (test)
  (setf (gethash (id test) *test-ids*) test)
  (setf (gethash (name test) *test-names*) test)
  (loop for tag in (tags test)
       with id = (id test)
     do (hash-ext-array-insert tag id *test-tag-ids*))
  (setf (gethash (id test) *test-paths*) 
	(cl-fad:merge-pathnames-as-file *active-module-path* (file-on-disk test))))

(defun deregister-test (test)
  (with-accessors ((id id)
		   (name name)
		   (tags tags)
		   (file-on-disk file-on-disk)) test
    (remhash is *test-ids*)
    (remhash name *test-names)
    (loop for tag in tags
	 do (hash-ext-array-remove tag id *test-tag-ids*))
    (remhash id *test-paths*)
    nil))

;;; This function is currently there as a copy/prototype of one in the test object's
;;; expectation table.
 
(defun condition-of-type (type function &rest arguments)
  (multiple-value-bind (x y)
      (ignore-errors (apply function arguments))
    (if (null x)
	(typep y type))))

(defgeneric serialise (path object 
		       &optional human-readible)
  (:documentation "Write an object out to disk."))

(defun deserialise (path)
  "Read a serialised object in from disk"
path)

