(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form))

(defmacro check-result (&body forms)
  `(progn
     ,@(loop for f in forms
	    collect `(report-result ,f ',f))))

(defun test-+ ()
  (check-result
   (= (+ 1 1) 2)
   (= (+ 2 2) 4)
   (= (+ -2 -5) 7)))