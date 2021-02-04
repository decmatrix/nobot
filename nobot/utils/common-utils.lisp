;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/utils/common-utils
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:export #:equals
           #:split-list
           #:reintern
           #:let-when
           #:))

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

(defun reintern (sym &optional (package *package*))
  (intern (symbol-name sym) package))

(defmacro let-when (bindings &body body)
  (with-gensyms (block)
    `(block ,block
       (let ,(loop :for (symbol value) :in bindings
                :collect `(,symbol (or ,value
                                       (return-from ,block nil))))
         ,@body))))
