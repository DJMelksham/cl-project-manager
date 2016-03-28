(defun expect-eq () 
 nil)

(defun expect-eql ()
 nil)

(defun expect-equal ()
  nil)

(defun expect-equalp ()
  nil)

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
