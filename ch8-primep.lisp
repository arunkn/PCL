(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (when (> number 1)
    (loop for n from number when (primep n) return n)))

;;; Example usage of do-primes
;; (do-primes (p 0 19)
;;   (format t "~d " p))

;;; initial definition of do-primes
(do ((p 2 (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

(defmacro do-prime ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(do-prime (p 2 10)
  (format t "~d " p))

;;; The problem with the earlier do-prime is that the ,end form is evaluated multiple times.
;;; This might be okay in case it is pure (in which case it would only take up CPU time)
;;; The problem becomes serious if the form makes side-effects.

(defmacro do-primes-2 ((var start end) &body body)
  `(do ((ending-value ,end)
	(,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

;;; do-primes-2 also leaks abstraction.
;;; When the start and end parameters are simple numbers or pure functions then it is okay.
;;; Note that the end form is being evaluated before start.
;;; If the start and end forms have side-effects then evaluating them out of order
;;; would result in a surprise to the user

(defmacro do-primes-3 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;;; Here ending-value is intended to be used as an internal variable by the macro
;;; But when the same variable name is being used elsewhere it will result in confusion.

(defmacro do-primes-4 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	 (,ending-value-name ,end))
	((> ,var ,ending-value-name))
      ,@body)))

(do-primes-4 (prime 2 20)
  (format t "~d " prime))

;;; LESSONS to avoid surprises
;; Always evaluate the forms in the given order
;; Evaluate each form only once
;; Use GENSYM to create variables

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names
	       collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-5 ((var start end) &body body)
  (with-gensyms (ending-value-name)
   `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	 (,ending-value-name ,end))
	((> ,var ,ending-value-name))
      ,@body)))

(do-primes-4 (p 2 20)
  (format t "~d " p))