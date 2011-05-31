(defun test-+ ()
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 1) 2) '(= (+ 1 1) 2))
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 2 2) 4) '(= (+ 2 2) 4))
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ -2 -5) 7) '(= (+ -2 -5) 7)))

