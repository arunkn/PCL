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