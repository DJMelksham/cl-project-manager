(in-package :cl-user)
(defpackage peasant
  (:use :cl
	:testy
	:quickproject
	:gittest)
  (:shadow :make-project))
