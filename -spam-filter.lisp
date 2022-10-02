;;;; Used Package: cl-ppcre <github.com/edicl/cl-ppcre>

(defpackage :shynur.spam-filter
  (:documentation
"
The main function to implement has a simple job:
  take the text of a message as an argument and classify the message as spam, ham, or unsure.
" )
  (:use :common-lisp
        :shynur.path)
  (:shadow "*features*")
  (:export))

(in-package :shynur.spam-filter)

;;; Key Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parameter: *max-ham-score* *min-spam-score* *features*

;; API
(defparameter *max-ham-score*  .4
  "ham-mail: [0, 0.4]")
(defparameter *min-spam-score* .6
  "spam-mail: [0.6, 1]")

;; API
(defvar *features* (make-hash-table :test #'equal)
  "a feature database. helper-function: clear-features, lookup-feature")

;; API
(defun clear-features ()
  (setf *features* (make-hash-table :test #'equal)))

(defun lookup-feature (word)
  "takes a word and returns the appropriate feature, creating it if necessary"
  (or (gethash word *features*)
      (setf (gethash word *features*) (make-instance 'word-feature :word word))))

;;; Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun classify (text)
  (let ((score (score (extra-features text))))
    (cond ((<= score *max-ham-score*)  :ham)
          ((>= score *min-spam-score*) :spam)
          (:unsure))))

(defclass word-feature ()
  ((word
    :initarg :word :initform (error "Must supply :word")
    :reader word
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count :initform 0
    :accessor spam-count
    :documentation "Number of spams seen this feature in.")
   (ham-count
    :initarg :ham-count :initform 0
    :accessor ham-count
    :documentation "Number of hams seen this feature in.")))

(defun extract-features (text)
  (mapcar #'lookup-feature (delete-duplicates (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
                                              :test #'string=)))

(defmethod print-object ((obj word-feature) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((word word) (hams ham-count) (spams spam-count)) obj
      (format stream "~s, hams: ~d, spams: ~d" word hams spams))))
