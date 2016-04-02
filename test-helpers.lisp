(defun register-test (test)
  (setf (gethash (id test) *test-ids*) test)
  (setf (gethash (name test) *test-names*) test)
  (loop for tag in (tags test)
       with id = (id test)
     do (hash-ext-array-insert tag id *test-tag-ids*))
  (setf (gethash (id test) *test-id-paths*) 
	(cl-fad:merge-pathnames-as-file *active-module-path* *test-dir-name* (file-on-disk test)))
  ;;need a function in here to add before and after tag functions
  ;;from their hashes if test is last member of tag
)

(defun deregister-test (test)
  (with-accessors ((id id)
		   (name name)
		   (tags tags)
		   (file-on-disk file-on-disk)) test
    (remhash id *test-ids*)
    (remhash name *test-names*)
    (loop for tag in tags
	 do (hash-ext-array-remove tag id *test-tag-ids*))
    (remhash id *test-id-paths*)

    ;;need a function in here to remove before and after tag functions
      ;;from their hashes if test is last member of tag
    nil))
