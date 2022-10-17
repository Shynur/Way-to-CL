(defun qsort (list &optional (< #'cl:<))
  "revive a classic bug: (qsort '(2 1 3)) ==> Recursive `Error'"
  (declare (list list))
  (let ((length (length list)))
    (if (cl:< length 2)
        list
        (let ((midian (nth (floor length 2) list)) ; BUG. correct: (floor len 2) -> (random len)
              (lessers  ())
              (greaters ()))
          (loop :for e :in list
                :do (when (funcall < e midian)
                      (push e lessers)
                      (push e greaters)))
          (concatenate 'list (qsort lessers) (qsort greaters))))))
