(defun hash-ext-array-insert (key value hash)
  (if (nth-value 1 (gethash key hash))
      (vector-push-extend value (gethash key hash))
      (setf (gethash key hash) (make-array 1 
					   :element-type 'integer 
					   :initial-element value
					   :adjustable t
					   :fill-pointer 1)))
      (fill-pointer (gethash key hash)))

(defun hash-ext-array-remove (key value hash)
  (if (nth-value 1 (gethash key hash))
      (progn 
	(setf (gethash key hash) (delete value (gethash key hash)))
	(if (= 0 (fill-pointer (gethash key hash)))
	    (remhash key hash)
	    T))))

(let ((x 0))
  (defun new-test-id ()
    (loop until (null (nth-value 1 (gethash x *test-ids*)))
	 do (incf x))
    x))

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
