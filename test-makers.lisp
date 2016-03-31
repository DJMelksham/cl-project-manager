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
  (let* ((real-id (new-test-id))
	 (real-name nil)
	 (real-fod nil)
	 (real-desc nil)
	 (real-exp nil)
	 (real-tags nil)
	 (real-source nil)
	 (real-compiled-source nil)
	 (real-expected-value nil)
	 (real-bfs nil)
	 (real-compiled-bfs nil)
	 (real-compiled-afs nil))

;;producing test name

 (if (not name) 
     (setf real-name (concatenate 'string "TEST-" (write-to-string id)))
     (setf real-name (string-upcase name)))
 
 (if (gethash real-name *test-names*)
     (progn
       (format t "The name ~a is already registered to Test ID ~a~&" 
	       real-name
	       (id (gethash real-name *test-names*)))
       (return-from make-test nil)))

;;producing test file-name on disk 

 (if (not file-on-disk)
     (setf real-fod (concatenate 'string real-name ".test"))
     (setf real-fod (concatenate 'string file-on-disk ".test")))
 
 (if (probe (cl-fad:merge-pathnames-as-file *active-module-path* real-fod))
     (progn
       (format t "A file or test named ~a was already found at ~a~&" 
	       real-fod
	       (cl-fad:merge-pathnames-as-file *active-module-path* real-fod))
       (return-from make-test nil)))
 
;;producing test file description
 
 (if (or (not description)
	 (not (stringp description)))
     (setf real-desc "No valid description has been supplied for this test.")
     (setf real-desc description)


)

(defun delete-test (identifier)
  (let ((test (cond ((integerp identifier) (gethash identifier *test-ids*))
		    ((stringp identifier) (gethash identifier *test-names*))
		    (t (return-from delete-test nil)))))
    (with-accessors ((id id)
		     (name name)
		     (tags tags)
		     (file-on-disk file-on-disk)) test

      (delete-file (gethash id *test-paths*))
      (deregister-test test))))

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
