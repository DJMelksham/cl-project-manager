;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem peasant
  :name "peasant"
  :version "0.0.0"
  :maintainer "Damien John Melksham"
  :author "Damien John Melksham"
  :licence "All rights reserved"
  :description "Project Management for Common Lisp"
  :long-description "Project Managment for Common Lisp: woo stands for 'waste of oxygen', which is to say, a summary of most management in the real world, rather than in software."
  :serial t
  :components ((:file "package")
	       (:file "git-management")
               (:file "utility")
               (:file "variable-definitions")
               (:file "document-functions")
	       (:file "file-management")
	       (:file "config-low-level-functions")
	       (:file "module-management"))

  :depends-on ("cl-fad" "testy" "gittest"))
