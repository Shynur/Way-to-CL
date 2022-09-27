(define-condition malformed-log-entry-error (error)
  ((text
    :initarg :text
    :reader  :text)))

(defun parse-log-entry (log-line)
  (if (wellformed-log-entry-p log-line)
      (make-instance 'log-entry ...)
      (restart-case (error 'malformed-log-entry-error
                           :text log-line)
        (use-value (value)
          value)
        (reparse-entry (fixed-text)
          (parse-log-entry fixed-text)))))

(defun parse-log-file (file)
  (with-open-file (fin file
                       :direction :input)
    (loop for line = (read-line fin nil nil) while line
          for entry = (restart-case (parse-log-entry line)
                        (skip-log-entry () nil))
          when entry collect it)))

(defun analyze-log (log-file)
  (dolist (entry (parse-log-file log-file))
    (analyze-entry entry)))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry #'(lambda (c)
                                          (use-value (make-instance 'malformed-log-entry
                                                                    :text (text c))))))
    (dolist (log (find-all-logs))
      (analyze-log log))))
