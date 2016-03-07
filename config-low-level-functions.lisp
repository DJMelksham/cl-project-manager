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

(defun designate-not-a-module (folder-path config-list 
				  &optional 
				    (not-a-module-marker 'NOT-A-MODULE))
  (add-value-to-config-key folder-path not-a-module-marker config-list))

(defun designate-test-folder (folder-path config-list
			          &optional
				    (test-folder-marker 'TEST-FOLDER))
  (add-value-to-config-key folder-path test-folder-marker config-list))

;(defun designate-content (path config-list
;			   &optional
;			     (content-marker 'CONTENT))
;  (let ((config-list-store (remove-key-from-config-list content-marker config-list)))
;    
;  (loop for value in (flatten-list (get-directory-structure path))
;     do (setf config-list-store
;	      (add-value-to-config-key value content-marker config-list-store)))
;  
;  config-list-store))

(defun designate-lisp-folder (path config-list
				   &optional
				     (lisp-folder-marker 'LISP-FOLDER))
  (add-value-to-config-key path lisp-folder-marker config-list))

(defun designate-load-priority (path config-list
				&optional
				  (load-priority-marker 'LOAD-PRIORITY))
  (add-value-to-config-key path load-priority-marker config-list))

(defun designate-quicklisp-library (library-designator config-list
				   &optional
				     (quicklisp-marker 'QUICKLISP))
  (add-value-to-config-key library-designator quicklisp-marker config-list))

(defun designate-rules-status (config-list
			   &optional 
			     (rules-status 'off)
			     (rules-status-marker 'rules))

  (if (string-equals rules-status 'off)
      (add-value-to-config-key 'off rules-status-marker config-list)
      (remove-key-from-config-list rules-status-marker config-list)))
