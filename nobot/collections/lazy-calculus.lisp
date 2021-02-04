;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/collections/lazy-calculus
    (:use :cl
          :anaphora)
  (:export #:calc-lazy-node
           #:lazy!))

(in-package :nobot/collections/lazy-calculus)

(defclass lazy-node ()
  ((executable-code
    :initarg :exe-code
    :accessor get-exe-code)
   (node-info
    :initarg :node-info
    :accessor get-node-info)))

(defgeneric calc-lazy-node (obj))

(defmethod calc-lazy-node ((obj lazy-node))
  (funcall (get-exe-code obj)))


(defmacro lazy! ((&optional (info "node")) &body body)
  `(make-instance 'lazy-node
                  :node-info ,info
                  :exe-code (lambda ()
                              ,@body)))

(defmethod print-object ((obj lazy-node) stream)
  (format stream "{@lazy ~A}" (get-node-info obj)))

