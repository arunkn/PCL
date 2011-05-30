(defvar *test-name* nil)

(defun test-+ ()
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ -1 -3) 4) '(= (+ -1 -3) 4)))

(defun my-report-result (result form)
  (format t "~:[FAIL~;PASS~] ... ~a~%" result form))

(defun test-+-v2 ()
  (my-report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (my-report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (my-report-result (= (+ -1 -3) 4) '(= (+ -1 -3) 4)))

(defmacro check (form)
  `(my-report-result ,form ',form))

(defun test-+-v3 ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -2) 4)))

(defmacro check-v2 (&body forms)
  `(progn ,@(loop for f in forms collect `(my-report-result ,f ',f))))

(defun test-+-v4 ()
  (check-v2
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ 1 2) 4)))

(defun my-report-result-v2 (result form)
  (format t "~:[FAIL~;PASS~] ... ~a: ~a~%" result *test-name* form)
  result)

;;; Usage of combine-results should be like

;; (combine-results
;;  (foo)
;;  (bar)
;;  (baz))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names
	      collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (gensym)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
	      collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check-v3 (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(my-report-result-v2 ,f ',f))))

(defun test-+-v5 ()
  (let ((*test-name* 'test-+-v5))
    (check-v3
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ 1 2) 3))))

(defun test-* ()
  (let ((*test-name* 'test-*))
    (check-v3
      (= (* 1 5) 5)
      (= (* 2 2) 4)
      (= (* 6 6) 12))))

;; (defun test-arithmetic ()
;;   (combine-results
;;     (test-*)
;;     (test-+-v5)))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest my-test-+ ()
  (check-v3
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ 1 2) 3)))

(deftest my-test-* ()
  (check-v3
    (= (* 1 5) 5)
    (= (* 2 2) 4)
    (= (* 6 6) 36)))

(deftest test-arithmetic ()
  (combine-results
    (my-test-*)
    (my-test-+)))

(deftest test-math ()
  (test-arithmetic))