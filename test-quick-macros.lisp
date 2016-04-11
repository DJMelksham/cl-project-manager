(defmacro qtest (&key 
		   name
		   description
		   (expectation "EQUALP")
		   (source +)
		   (expected-value *)
		    before-function-source
		    after-function-source)
  `(make-test :name ,name
	      :description ,description
	      :expectation ,expectation
	      :source ',source
	      :expected-value ,expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))
		      
