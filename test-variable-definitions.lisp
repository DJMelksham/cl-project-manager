(defparameter *test-ids* (make-hash-table :test 'eql))
(defparameter *test-names* (make-hash-table :test 'equal)) 
(defparameter *test-tag-ids* (make-hash-table :test 'equal))
(defparameter *test-tag-before-functions* (make-hash-table :test 'equal))
(defparameter *test-tag-after-functions* (make-hash-table :test 'equal))
(defparameter *test-directory* (cl-fad:merge-pathnames
(defparameter *test-paths* (make-hash-table :test 'equal))
(defparameter *test-results* nil)
(defparameter *test-statistics* nil) 
