(defmacro generate-qtests ()
  (let ((function-list '("EQ" "=" "EQL" "EQUAL" "EQUALP" 
			 "NULL" "T" "CONDITION-OF-TYPE" "ERROR" 
			 "NOT-EQ" "/=" "NOT-=" "NOT-EQL" 
			 "NOT-EQUAL" "NOT-EQUALP" "NOT-NULL" "NOT-T" 
			 "NOT-CONDITION-OF-TYPE" "NOT-ERROR")))
    (append (list 'progn) 
	  (loop for func in function-list
	     for head-form = (list 'defmacro (intern (concatenate 'string "QTEST-" func)))
				   then (list 'defmacro (intern (concatenate 'string "QTEST-" func)))
	       for tail-form = nil then nil
	       do (push 
		   (append  '(&key 
			     name
			     description)
			   (list (list 'expectation func))
			   (list '(source +))
			   (list '(expected-value *))
			   (list 'before-function-source
				 'after-function-source)

		    `(make-test :name name
				      :description description
				      :expectation expectation
				      :source 'source
				      :expected-value expected-value
				      :before-function-source 'before-function-source
				      :after-function-source 'after-function-source))

		   tail-form)
	  collect (append head-form (reverse tail-form))))))

(defmacro generate-nwtests
    '("EQ" "=" "EQL" "EQUAL" "EQUALP" 
      "NULL" "T" "CONDITION-OF-TYPE" "ERROR" 
      "NOT-EQ" "/=" "NOT=" "NOT-EQL" 
      "NOT-EQUAL" "NOT-EQUALP" "NOTNULL" "NOT-T" 
      "NOT-CONDITION-OF-TYPE" "NOT-ERROR"))

(defmacro generate-tests
    '("EQ" "=" "EQL" "EQUAL" "EQUALP" 
      "NULL" "T" "CONDITION-OF-TYPE" "ERROR" 
      "NOT-EQ" "/=" "NOT=" "NOT-EQL" 
      "NOT-EQUAL" "NOT-EQUALP" "NOTNULL" "NOT-T" 
      "NOT-CONDITION-OF-TYPE" "NOT-ERROR")

)

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
		      
(defmacro wtest (&key 
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
	      :source ,source
	      :expected-value ,expected-value
	      :before-function-source ',before-function-source
	      :after-function-source ',after-function-source))


(defmacro qtest-eq (&key 
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
