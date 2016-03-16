(defun flatten-list (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten-list structure))))

(defun func-list-applicator (func-or-list)
  (cond ((and (not (functionp func-or-list))
	      (not (listp func-or-list))) 
	 func-or-list)
	((functionp func-or-list) 
	 (funcall func-or-list))
	((= (length func-or-list) 1) 
	 (funcall (car func-or-list)))
	(t (apply (car func-or-list) 
		  (cdr func-or-list)))))

(defun tail-of-path (path)
  (if (cl-fad:directory-pathname-p path)
      (first (last (pathname-directory path)))
      (pathname-name path)))

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun insert-after (list index new-element)

  (let ((length (length list)))
    (cond ((null list) (return-from insert-after (list new-element)))
	  ((> index (- length 1))
	   (push new-element (cdr (nthcdr (- length 1) list)))) 
	  (t (push new-element (cdr (nthcdr index list)))))
    list))
