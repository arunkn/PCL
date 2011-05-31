(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defmacro with-gensyms ((&rest names) &body body)
  "Generate gensym for the given 'names' and make it available within the form."
  `(let ,(loop for n in names
	       collect `(,n (gensym)))
     ,@body))