(defun report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form))

(defmacro check-result (form)
  `(report-result ,form ',form))

(defun test-+ ()
  (check-result (= (+ 1 1) 2))
  (check-result (= (+ 2 2) 4))
  (check-result (= (+ -2 -5) 7)))

