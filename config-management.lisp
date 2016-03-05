(defun write-config-to-file (path config &optional (config-name "module-meta.lisp"))
  (with-open-file (stream (merge-pathnames config-name path)
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create)
    (prin1 config stream))
  (merge-pathnames config-name path))

(defun get-config-from-file (dir-or-file-path)
  (if (cl-fad:directory-pathname-p dir-or-file-path)
      (with-open-file (stream (merge-pathnames "module-meta.lisp" dir-or-file-path)
			      :direction :input
			      :if-does-not-exist nil)
	(read stream))
      (with-open-file (stream dir-or-file-path
			      :direction :input
			      :if-does-not-exist nil)
	(read stream))))

(defun get-values-from-config-list (key config-list)
  (rest (first (member key config-list :key #'car :test #'equal))))

(defun remove-key-from-config-list (key config-list)
  (let ((result nil))
    (loop for item in config-list
       if (not (equal (car item) key))
       do (push item result)
       finally (setf result (nreverse result)))
    result))

(defun add-value-to-config-key (value key config-list)
  (let ((key-list (get-values-from-config-list key config-list))
	(result (remove-key-from-config-list key config-list)))
	
	(setf key-list (append (list key value)
			       (remove-if (lambda (x) (equal x value)) key-list)))
	(push key-list result)))

(defun remove-value-from-config-key (value key config-list)
  (let ((key-list (get-values-from-config-list key config-list))
	(result (remove-key-from-config-list key config-list)))
	
	(setf key-list (append (list key) 
			       (remove-if (lambda (x) (equal x value)) key-list)))
	(push key-list result)))
