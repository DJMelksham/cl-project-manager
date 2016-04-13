(defmacro qtest (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUALP"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest-EQ (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQ"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest= (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "="
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest-EQL (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest-EQUAL (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUAL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest-EQUALP (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "EQUALP"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest-NULL (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "NULL"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))

(defmacro qtest-T (&key 
		   name
		   description
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation "T"
	      :source ',source
	      :expected-value ',expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))
