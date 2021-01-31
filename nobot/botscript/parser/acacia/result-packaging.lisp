;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/result-packaging
    (:use :cl)
  (:export #:get-parse-tree
           #:pack-parse-tree))

(in-package :nobot/botscript/parser/acacia/result-packaging)

(defclass acacia-packed-result ()
  ((parse-tree
    :type list
    :initarg :parse-tree
    :reader get-parse-tree)
   (source-type
    :type keyword
    :initarg :source-type
    :reader get-source-type)
   (source
    :type string
    :initarg :source
    :reader get-source)))

(defmethod initialize-instance :around ((res acacia-packed-result) &key parse-tree
                                                                     source-type
                                                                     source)
  (unless (or (eq source-type :string)
              (eq source-type :file))
    (error "unknown source type: ~a, expected :string or :file" type))
  (call-next-method res
                    :parse-tree parse-tree
                    :source-type source-type
                    :source source))

(defun pack-parse-tree ()
  (make-instance 'acacia-packed-result
                 :parse-tree parse-tree))
