(declaim (optimize (speed 0) (safety 3) (debug 3)))

(load "utility.lisp")

(defvar *test-name* nil)

(defun report-result (result form)
  "Report the results of a single test case."
  (format t "~:[FAIL~;PASS~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
	       collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check-result (&body forms)
  "Run each expression in 'forms' as a test case"
  `(combine-results
     ,@(loop for f in forms
	     collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within each test function call other test functions
or use 'check' to run individual test case."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;;; Example usage of the framework

(deftest test-+ ()
  (check-result
    (= (+ 1 1) 2)
    (= (+ 2 2) 4)))

(deftest test-* ()
  (check-result
    (= (* 1 1) 1)
    (= (* 5 5) 10)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))