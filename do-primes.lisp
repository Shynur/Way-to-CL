(defun primep (n)
  (when (> n 1)
    (loop for fac from 2 to (isqrt n) never (zerop (mod n fac)))))

(defun next-prime (i)
  "return the minimum prime number which >= i"
  (loop for j from i when (primep j) return j))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

(defmacro do-primes ((var begin end) &body body)
  "iterate on the prime numbers in [begin, end)"
  (with-gensyms (end-name)
    `(do ((,var (next-prime ,begin) (next-prime (1+ ,var)))
          (,end-name ,end))
         ((>= ,var ,end-name))
       ,@body)))
