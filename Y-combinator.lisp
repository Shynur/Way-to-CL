(defconstant Y #'(lambda (f)
                   (let ((g #'(lambda (x)
                                (funcall f #'(lambda (&rest args)
                                               (apply (funcall x x) args))))))
                     (funcall g g))))

(defparameter fib (funcall Y #'(lambda (f)
                                 #'(lambda (n a b)
                                     (if (zerop n)
                                         a
                                         (funcall f (1- n) b (+ a b)))))))

(defun fib-n (n)
  (dotimes (i n)
    (format t "~a~%" (funcall fib i 0 1))))
