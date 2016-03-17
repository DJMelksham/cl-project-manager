(defun apply-to-project-tree (path &optional (predicate-funcs (list (lambda (x) x))) (apply-func #'identity) (not-git t))
  (let* ((flat-tree (if not-git 
		       (remove-if #'git-folder-p (flatten-list (get-directory-structure path)))
		       (flatten-list (get-directory-structure path))))
	(filtered-tree flat-tree))

    (loop for predicate in predicate-funcs
	 do (setf filtered-tree (remove-if-not predicate filtered-tree)))
    
    (loop for thing in filtered-tree
	 do (funcall apply-func thing))))
