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
    (if (boundp '*test-ids*)
	(loop until (null (nth-value 1 (gethash x *test-ids*)))
	   do (incf x))
	(incf x))
    x)
  
  (defun set-test-id-counter (number)
    (if (and (not (integerp number))
	     (< 0 number))
	nil
	(setf x number))))

(defun test-cond (test-identifier)
  (cond ((integerp test-identifier) 
	 (gethash test-identifier *test-ids*))
	((or (stringp test-identifier) (symbolp test-identifier)) 
	 (gethash (string-upcase test-identifier) *test-names*))
	((typep test-identifier 'test)
	 test-identifier)
	(t nil)))
	      
(defun fetch-tests (test-identifier)
  (let* ((result nil))
    
    (if (null test-identifier)
	(return-from fetch-tests nil))

    (if (or (not (typep test-identifier 'sequence))
	    (typep test-identifier 'string))
	(setf result (make-array 1 :initial-element (test-cond test-identifier)))
	(setf result (map 'vector #'test-cond test-identifier)))
    result))
    
  
