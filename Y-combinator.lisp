(defvar Y #'(lambda (f)
              ((lambda (g)
                 (funcall g g))
               #'(lambda (x)
                   (funcall f #'(lambda (&rest args)
                                  (apply (funcall x x) args)))))))

(defparameter fib (funcall Y #'(lambda (f)
                                 #'(lambda (n a b)
                                     (if (zerop n)
                                         a
                                         (funcall f (1- n) b (+ a b)))))))

(defun fib (n)
  (dotimes (i n)
    (format t "~a~%" (funcall fib i 0 1))))
