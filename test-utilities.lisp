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

;;; This function is currently there as a copy/prototype of one in the test object's
;;; expectation table.
 
(defun condition-of-type (type function &rest arguments)
  (multiple-value-bind (x y)
      (ignore-errors (apply function arguments))
    (if (null x)
	(typep y type))))
