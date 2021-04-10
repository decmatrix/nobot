;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/utils/common-utils
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:export #:equals
           #:split-list
           #:reintern
           #:let-when
           #:to-symbol
           #:to-keyword
           #:to-string
           #:pos-of-str
           #:get-date-now))

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
       (let ,(loop
                :for (symbol value) :in bindings
                :collect `(,symbol (or ,value
                                       (return-from ,block nil))))
         ,@body))))

(defun to-symbol (str &key case-sensitive (to-package :cl-user))
  (let ((normal-str (if case-sensitive
                        str
                        (string-upcase str))))
    (intern normal-str to-package)))

(defun to-keyword (str &key case-sensitive)
  (to-symbol str
             :case-sensitive case-sensitive
             :to-package :keyword))

(defun to-string (sym &key case-sensitive)
  (let ((str (string sym)))
    (if case-sensitive
        str
        (string-downcase str))))

(defun pos-of-str (string)
  "pos: (x . y)"
  (let ((cur-pos-x 1)
        (cur-pos-y 1))
    (mapcar
     (lambda (ch)
       (prog2 nil
           (list
            ch
            (cons
             cur-pos-x
             (if (eq ch #\Newline)
                 (incf cur-pos-y)
                 cur-pos-y)))
         (incf cur-pos-x)))
     (coerce string 'list))))

(defun get-date-now ()
  (let ((decoded-time (multiple-value-list (get-decoded-time))))
    (format nil "~d.~2,'0d.~d"
            (nth 3 decoded-time)
            (nth 4 decoded-time)
            (nth 5 decoded-time))))
