(defun run-test (test &key (re-evaluate 'auto))
  
  (let ((test-time-start 0)
	(test-time-stop 0))
    
    (setf test-time-start (get-internal-real-time))

    (apply (before-function-compiled-form test) nil)

    (multiple-value-bind (value status)
	(ignore-errors (apply (compiled-form test) nil))
      (if (typep status 'condition)
	    (setf (run-value test) status)
	    (setf (run-value test) value)))
      
      (setf (result test)
	    (apply (gethash (expectation test) (expectation-table test))
		   (list (expected-value test) (run-value test))))
      
      (apply (after-function-compiled-form test) nil)
      
      (setf test-time-stop (get-internal-real-time))
      
      (setf (run-time test) (/ (- test-time-stop test-time-start)
				     1000.0))
      test))
