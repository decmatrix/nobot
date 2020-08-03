(uiop:define-package :nobot/utils/common-utils
    (:use :cl)
  (:export #:equals
           #:set-<>))

(in-package :nobot/utils/common-utils)

(defgeneric equals (obj1 obj2))



(defmethod equals ((obj1 t) (obj2 t))
  nil)

(defmethod equals ((obj1 number) (obj2 number))
  (eql obj1 obj2))

(defmethod equals ((obj1 string) (obj2 string))
  (equal obj1 obj2))

(defmethod equals ((obj1 list) (obj2 list))
  (equalp obj1 obj2))

(defmethod equals ((obj1 symbol) (obj2 symbol))
  (eq obj1 obj2))

(defun set-<> (str)
  (concatenate 'string '(#\<) (string-upcase str) '(#\>)))
