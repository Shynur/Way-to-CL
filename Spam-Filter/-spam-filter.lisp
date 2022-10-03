(defpackage :shynur.spam-filter
  (:documentation
"
> To avoid having to think like a spammer,
> Graham decided to try distinguishing spam from nonspam, a.k.a. ham,
> based on statistics gathered about which words occur in which kinds of e-mails.
> The filter would keep track of how often specific words appear in both spam and ham messages
> and then use the frequencies associated with the words in a new message
> to compute a probability that it was either spam or ham.
> He called his approach Bayesian filtering after the statistical technique
> that he used to combine the individual word frequencies into an overall probability.

So, what is in this package is a Spam-Filter based on Paul Graham's Bayesian filtering approach.

Before using it, you need to train it well:
  1. Add sufficient classified mails to the folder `mail-corpus/`;
  2. Type (load-corpus). See this function's documentation.

To classify a text or file, type (classify some-string-or-pathname):
  It will return a primary result -- `ham`, `spam` or `unsure`;
  The result comes with a probability of the given text is a spam.

You can change the standard of judgment by setting two variables:
  *max-ham-score*
  *min-spam-score*

If you want to reset the filter, type (clear-features) to make it a brand new one.
" )
  (:use :common-lisp)
  (:import-from :shynur.path :ls
                :cl-ppcre    :all-matches-as-strings)
  (:export :classify
           :*max-ham-score*
           :*min-spam-score*
           :clear-features
           :load-corpus))

(in-package :shynur.spam-filter)

;; API
(defun classify (src)
  "called on a string or pathname"
  (if (typep src 'string)
    (let ((score (score (extract-features src))))
      (values (cond ((<= score *max-ham-score*)  'ham)
                    ((>= score *min-spam-score*) 'spam)
                    ('unsure))
              score))
    (classify (reduce #'(lambda (a b)
                          (concatenate 'string a b))
                      (with-open-file (fin src :direction :input)
                        (loop for line = (read-line fin nil)
                              while line
                              collecting line))))))

;;; Coer Data and Operations on it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; API
(defparameter *max-ham-score*  .4
  "ham-mail: [0, 0.4]")
(defparameter *min-spam-score* .6
  "spam-mail: [0.6, 1]")

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

;;; Feature of Text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (mapcar #'lookup-feature (delete-duplicates (all-matches-as-strings "[a-zA-Z]{3,}" text)
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

;;; Trainer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; API
(defun load-corpus (&key ignored-prefix)
  (dolist (dir&type `((,#P"mail-corpus/hams/" . ham) (,#P"mail-corpus/spams/" . spam)))
    (dolist (mail (ls (first dir&type)))
      (when (if ignored-prefix (untrained? mail ignored-prefix) t)
        (with-open-file (fin mail
                             :direction :input)
          (loop for line = (read-line fin nil)
                always line
                do (train line (rest dir&type))))
        (when ignored-prefix
          (rename-file mail (concatenate 'string ignored-prefix (pathname-name mail))))))))

(defun untrained? (mail ignored-prefix)
  (setf mail (pathname-name mail))
  (let ((prefix-length (length ignored-prefix)))
    (or (< (length mail) prefix-length)
        (string= mail ignored-prefix :end1 prefix-length :end2 prefix-length))))

;;; Calculate Probability ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bayesian-P_spam (feature &optional (assumed-P 1/2) (weight 1))
  (let* ((P_ham  (P_ham  feature))
         (P_spam (P_spam feature))
         (P_ham+P_spam (+ P_ham P_spam))
         (basic-P_spam (if (zerop P_ham+P_spam) 1/2 (/ P_spam P_ham+P_spam)))
         (data-points (+ (ham-count feature) (spam-count feature))))
    (/ (+ (* weight assumed-P) (* basic-P_spam data-points)) (+ weight data-points))))

(defun score (features)
  (let ((ham-probs  ())
        (spam-probs ())
        (number-of-probs 0))
    (dolist (feature features)
      (let ((spam-prob (float (bayesian-P_spam feature))))
        (push        spam-prob  spam-probs)
        (push (- 1.0 spam-prob)  ham-probs)
        (incf number-of-probs)))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher  ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0))))

(defun fisher (probs number-of-probs)
  (inverse-chi^2
    (* -2 (reduce #'+ probs :key #'log))
    (*  2 number-of-probs)))

(defun inverse-chi^2 (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min (loop with m = (/ value 2)
             for i below (/ degrees-of-freedom 2)
             for prob = (exp (- m)) then (* prob (/ m i))
             summing prob)
       1))
