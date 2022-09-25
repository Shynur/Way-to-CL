(defclass bank-account ()
  ((%%account-number
    :allocation    :class
    :initform      -1)
   (ID
    :reader        ID
    :documentation "Account number, unique within a bank.") 
   (customer-name
    :initform      (error "Must supply a customer's name.")
    :initarg       :name
    :accessor      customer-name
    :documentation "Customer's name.")
   (balance
    :initform      0
    :initarg       :balance
    :reader        balance
    :documentation "Current account balance.")
   (type
    :reader        account-type
    :documentation "Type of account, one of gold, silver, or bronze.")))

(defmethod initialize-instance :after ((account bank-account) &key opening-bonus-percent)
  (with-slots (id balance type) account
    (setf id (incf (slot-value account '%%account-number)))
    (when opening-bonus-percent
      (incf balance (* balance (/ opening-bonus-percent 100))))
    (setf type (cond
                 ((>= balance 100000) 'gold)
                 ((>= balance  50000) 'silver)
                 (T                   'bronze)))))

(defmethod %show ((x bank-account))
  (with-accessors ((id ID)
                   (name customer-name)
                   (balance balance)
                   (type account-type)) x
    (format t "ID:            ~a
customer-name: ~a
balance:       ~a
type:          ~a
" id name balance type)))
