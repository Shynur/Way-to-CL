(defvar Y #'(lambda (f)
              ((lambda (g)
                 (funcall g g))
               #'(lambda (x)
                   (funcall f #'(lambda (&rest args)
                                  (apply (funcall x x) args)))))))

(defparameter fib (funcall Y #'(lambda (f)
                                 #'(lambda (n)
                                     (if (<= n 1)
                                         n
                                         (+ (funcall f (- n 1)) (funcall f (- n 2))))))))
