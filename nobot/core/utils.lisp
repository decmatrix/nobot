(uiop:define-package :nobot/core/utils
    (:use :cl)
  (:nicknames :nobot-utils)
  (:export :lazy!
           :calc-it
           :determine-compare-func
           :equals))

(in-package :nobot/core/utils)

(defclass lazy-node ()
  ((executable-code
    :initarg :exe-code
    :accessor get-exe-code)))

(defmacro lazy! (&body body)
  `(make-instance 'lazy-node
                  :exe-code (lambda ()
                              ,@body)))

(defgeneric calc-it (obj))
(defgeneric equals (obj1 obj2))

(defmethod calc-it ((obj lazy-node))
  (funcall (get-exe-code obj)))



(defmethod equals :before ((obj1 t) (obj2 t))
  ;;empty
  )

(defmethod equals :before ((obj1 number) (obj2 number))
  (eql obj1 obj2))

(defmethod equals :before ((obj1 string) (obj2 string))
  (equal obj1 obj2))

(defmethod equals :before ((obj1 list) (obj2 list))
  (equalp obj1 obj2))

(defmethod equals :before ((obj1 symbol) (obj2 symbol))
  (eq obj1 obj2))
