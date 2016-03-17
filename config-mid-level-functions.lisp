(defun make-module (path)

)

(defun set-active-module (pathname)
  (setf *active-module-path* pathname))

(defun up-module (&optional (name (car *parent-modules*)))

)

(defun down-module (name)

)

(defun validate-module (&optional (module-path *active-module-path*))

)

