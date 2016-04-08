(defun hash-ext-array-insert (key value hash)
  (if (nth-value 1 (gethash key hash))
      (vector-push-extend value (gethash key hash))
      (setf (gethash key hash) (make-array 1  
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

(defun tag-cond (tag-identifier)
  (cond ((and (not (listp tag-identifier))
	      (not (stringp tag-identifier))
	      (notevery #'stringp tag-identifier)) 
	   nil)
	  ((stringp tag-identifier) 
	   (list (string-upcase tag-identifier)))
	  ((typep tag-identifier 'sequence) 
	   (remove-duplicates (map 'list #'string-upcase tag-identifier) :test #'equalp))
	  (t nil)))
	      
(defun fetch-tests (test-identifier)
  (let* ((result nil))
    
    (if (null test-identifier)
	(return-from fetch-tests nil))

    (if (or (not (typep test-identifier 'sequence))
	    (typep test-identifier 'string))
	(setf result (make-array 1 :initial-element (test-cond test-identifier)))
	(setf result (map 'vector #'test-cond test-identifier)))
    (remove-duplicates result :test #'eq)))
    
(defun fetch-tests-from-tags (tag-identifiers)
  (let ((result (loop for tags in (tag-cond tag-identifiers)
		   unless (null (gethash tags *test-tags*))
		   collect (gethash tags *test-tags*))))

    (remove-duplicates (apply #'concatenate 'vector result) :test #'eq)))

(defun combine-test-sequences (&rest test-sequences)
 (remove nil 
	 (remove-duplicates 
	  (apply #'concatenate 'vector 
		 (map 'list #'fetch-tests test-sequences)) 
	  :test #'eq)))

(defun run-tests (tests)
  (map 'vector #'run-test (fetch-tests tests)))

(defun run-tags (tags)
  (map 'vector #'run-test (fetch-tests-from-tags tags)))

(defun tests-if (predicate-func test-sequence)
  (remove-if-not predicate-func (fetch-tests test-sequence)))

(defun tests-if-not (predicate-func test-sequence)
  (remove-if predicate-func (fetch-tests test-sequence)))

(defun map-tests (func test-sequence &optional (result-type 'vector))
  (map result-type func (fetch-tests test-sequence)))

(defun set-low-verbose ()
(setf *test-print-verbosity* 'low))

(defun set-high-verbose ()
(setf *test-print-verbosity* 'high))

(defun set-medium-verbose ()
(setf *test-print-verbosity* 'medium))

(defun all-tests ()
  (map 'vector #'identity (loop for tests being the hash-values in *test-ids*
			     collect tests)))
