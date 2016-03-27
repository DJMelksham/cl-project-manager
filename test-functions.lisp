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
