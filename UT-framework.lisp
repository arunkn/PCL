(load "utility.lisp")

(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
	       collect `(unless ,f (setf ,result nil)))
       ,result)))


(defmacro check-result (&body forms)
  `(combine-results
     ,@(loop for f in forms
	     collect `(report-result ,f ',f))))

(defun test-+ ()
  (check-result
   (= (+ 1 1) 2)
   (= (+ 2 2) 4)
   (= (+ -2 -5) -7)))

