(defun make-test (&key 
		    name
		    file-on-disk
		    description
		    expectation
		    tags
		    source
		    expected-value
		    before-function-source
		    after-function-source)
     

)

(defun delete-test (&key
		      id
		      name)

)

(defun config-structure-to-test (config-structure)

)

(defun test-to-config-structure (test)

)

(defun make-test-config-structure (&key 
				     id 
				     name
				     file-on-disk
				     description
				     expectation
				     tags
				     source
				     expected-value
				     before-function-source
				     after-function-source)
  (let ((result ()))
    (labels ((add-if (x y)
	       (if x 
		   (setf result (add-value-to-config-key
				  x
				  y
				  result)))))

      (add-if after-function-source 'after-function-source)
      (add-if before-function-source 'before-function-source)
      (add-if expected-value 'expected-value)
      (add-if source 'source)
      (add-if tags 'tags)
      (add-if expectation 'expectation)
      (add-if description 'description)
      (add-if file-on-disk 'file-on-disk)
      (add-if name 'name)
      (add-if id 'id)

      result)))
