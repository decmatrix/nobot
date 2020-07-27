(uiop:define-package :nobot/core/utils
    (:use :cl)
  (:export :with-gensyms))

(defmacro with-gensyms (args &body body)
  `(let ,(mapcar (lambda (arg)
                   `(,arg (gensym)))
                 args)
     ,@body))
