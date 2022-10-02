;;;; Used Package: cl-ppcre <github.com/edicl/cl-ppcre>
;;;; 1. Key Data:
;;;; 2. Filter Trainer

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

(defun classify (text)
  (let ((score (score (extract-features text))))
    (cond ((<= score *max-ham-score*)  'ham)
          ((>= score *min-spam-score*) 'spam)
          ('unsure))))

;;; Key Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parameter: *max-ham-score* *min-spam-score* *features*

;; API
(defparameter *max-ham-score*  .4
  "ham-mail: [0, 0.4]")
(defparameter *min-spam-score* .6
  "spam-mail: [0.6, 1]")

;; API
(defvar *features* (make-hash-table :test #'equal)
  "a word-feature database")

;; API
(defun clear-features ()
  "clear all the results of training, including:
     *features*, word frequency,
     *total-hams* and *total-spams*"
  (setf *features*    (make-hash-table :test #'equal)
        *total-hams*  0
        *total-spams* 0))

(defun lookup-feature (word)
  "takes a word and returns the appropriate feature, creating it if necessary"
  (or (gethash word *features*)
      (setf (gethash word *features*) (make-instance 'word-feature :word word))))

;;; Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod inc-count ((feature word-feature) type)
  (ecase type
    (ham  (incf (ham-count  feature)))
    (spam (incf (spam-count feature)))))

(defmethod P_ham ((feature word-feature))
  (with-accessors ((hams ham-count)) feature
    (/ hams (max 1 *total-hams*))))
(defmethod P_spam ((feature word-feature))
  (with-accessors ((spams spam-count)) feature
    (/ spams (max 1 *total-spams*))))

(defmethod print-object ((obj word-feature) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((word word) (hams ham-count) (spams spam-count)) obj
      (format stream "~s, hams: ~d, spams: ~d" word hams spams))))

;;; Filter Trainer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; these two var can be CLEARED by function clear-features
(defvar *total-hams*  0)
(defvar *total-spams* 0)

(defun train (text type)
  "update: the hash-table of word-features,
           the number of already counted hams or spams"
  (dolist (feature (extract-features text))
    (inc-count feature type))
  (ecase type
    (ham  (incf *total-hams*))
    (spam (incf *total-spams*))))

;;; Calculate Probability ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bayesian-P_spam (feature &optional (assumed-P 1/2) (weight 1))
  (let* ((P_ham  (P_ham  feature))
         (P_spam (P_spam feature))
         (P_ham+P_spam (+ P_ham P_spam))
         (basic-P_spam (if (zerop P_ham+P_spam) 1/2 (/ P_spam P_ham+P_spam)))
         (data-points (+ (ham-count feature) (spam-count feature))))
    (/ (+ (* weight assumed-P) (* basic-P_spam data-points)) (+ weight data-points))))
