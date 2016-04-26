;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem #:peasant
  :name "peasant"
  :version "0.0.0"
  :maintainer "Damien John Melksham"
  :author "Damien John Melksham"
  :licence "All rights reserved"
  :description "Project Management for Common Lisp"
  :long-description "The humble peasant was written to do all the heavy work of my project management in Common Lisp, which I generally mean to involve version control, testing, system creation and continuous, while I sit on my ass and bask in the glory of hacking..."
  :serial t
  :components ((:file "package")
	       (:file "peasant"))
  :depends-on (#:testy #:gittest #:quickproject #:uiop #:quicklisp))
