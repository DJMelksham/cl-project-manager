(defun config-to-disk (dir config &optional (config-name *module-config-name*))
  (with-open-file (stream (merge-pathnames config-name dir)
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "(")
    (loop for key in config
       do (format stream "~&(")
       do (format stream "~S~%" (first key))
	  (format stream "~{    ~S~^~%~}" (cdr key))
       do (format stream ")"))
    (format stream ")"))
    
  (merge-pathnames config-name dir))

(defun config-from-disk (dir &optional (config-name *module-config-name*))
  (let ((*read-eval* nil))
	(with-open-file (stream (merge-pathnames config-name dir)
				:direction :input
				:if-does-not-exist nil)
	  
	  (read stream nil nil))))

(defun values-from-config-list (key config-list)
  (rest (first (member key config-list :key #'car :test #'equalp))))

(defun remove-key-from-config-list (key config-list)
  (let ((result nil))
    (loop for item in config-list
       if (not (equalp (car item) key))
       do (push item result)
       finally (setf result (nreverse result)))
    result))

(defun add-value-to-config-key (value key config-list)
  (let ((key-list (values-from-config-list key config-list))
	(result (remove-key-from-config-list key config-list)))
	
	(setf key-list (append (list key value)
			       (remove-if (lambda (x) (equalp x value)) key-list)))
	(push key-list result)))

(defun remove-value-from-config-key (value key config-list)
  (let ((key-list (values-from-config-list key config-list))
	(result (remove-key-from-config-list key config-list)))
	
	(setf key-list (append (list key) 
			       (remove-if (lambda (x) (equalp x value)) key-list)))
	(if (= 1 (length key-list))
	    result
	    (push key-list result))))

(defun designate-test-folder (folder-name config-list)
  (add-value-to-config-key folder-name 'test-folder config-list))

(defun designate-load-file (load-file-name config-list)
  (add-value-to-config-key load-file-name 'LOAD-FILE config-list))

(defun value-in-config-p (value key config)
  (if (member value (values-from-config-list key config) :test #'equalp)
      t
      nil))

(defun insert-value-into-config-key (position value key config-list)
  (let* ((key-list (values-from-config-list key config-list))
	 (result (remove-key-from-config-list key config-list)))
      
	(setf key-list (append (list key)
			       (remove-if (lambda (x) (equalp x value)) key-list)))
	
	(setf key-list (insert-after key-list position value))
	(push key-list result)))


(defun key-in-config-p (key config)
  (if (member key config :key #'car :test #'equalp)
      t
      nil))

(defun validate-config-structure (config)
  
    (if (and (not (null config)) 
	     (listp config))
	(loop for item in config
	   if (or (not (listp item))
		  (null item)
		  (<= (length item) 1))
	   do (return-from validate-config-structure nil)
	   finally (return-from validate-config-structure t)))
    nil)
  
  

