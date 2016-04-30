(in-package :cl-user)
(defpackage peasant
  (:use :cl
	:testy
	:quickproject
	:gittest)
  (:export #:active-project
	   #:make-project
	   #:save-project)
  (:shadow #:make-project))
