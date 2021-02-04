;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/toplevel/context
    (:use :cl)
  (:export #:with-translator-context
           #:add-info-log
           #:add-debug-log
           #:add-warn-log
           #:add-error-log
           #:is-empty-info-stack-?
           #:is-empty-info-stack-?
           #:is-empty-warn-stack-?
           #:is-empty-error-stack-?
           #:get-source-from-context
           #:get-source-type-from-context))

(in-package :nobot/toplevel/context)

(defvar *context*)

(defclass context ()
  ((source-type
    :initarg :source-type
    :reader get-source-type
    :type keyword)
   (source
    :initarg :source
    :reader get-source
    :type string)
   (info-stack
    :initform nil
    :accessor get-info-stack)
   (debug-stack
    :initform nil
    :accessor get-debug-stack)
   (warn-stack
    :initform nil
    :accessor get-warn-stack)
   (error-stack
    :initform nil
    :accessor get-error-stack)))

(defmacro with-translator-context ((&key source-type source) &body body)
  `(let ((*context* (make-instance 'context
                                   :source-type (when (is-source-type-? ,source-type)
                                                  ,source-type)
                                   :source ,source)))
     ,@body))

(defun add-info-log (str)
  (when (boundp '*context*)
    (push str (get-info-stack *context*))))

(defun add-debug-log (str)
  (when (boundp '*context*)
    (push str (get-debug-stack *context*))))

(defun add-warn-log (str)
  (when (boundp '*context*)
    (push str (get-warn-stack *context*))))

(defun add-error-log (str)
  (when (boundp '*context*)
    (push str (get-error-stack *context*))))

(defun is-empty-info-stack-? ()
  (or
   (not (boundp '*context*))
   (null (get-info-stack *context*))))

(defun is-empty-debug-stack-? ()
  (or
   (not (boundp '*context*))
   (null (get-debug-stack *context*))))

(defun is-empty-warn-stack-? ()
  (or
   (not (boundp '*context*))
   (null (get-warn-stack *context*))))

(defun is-empty-error-stack-? ()
  (or
   (not (boundp '*context*))
   (null (get-error-stack *context*))))

(defun get-source-from-context ()
  (get-source '*context*))

(defun get-source-type-from-context ()
  (get-source-type '*context*))

(defun is-source-type-? (source-type)
  (if (or (eq source-type :file)
          (eq source-type :string))
      t
      (error "Unknown source type: ~a" source-type)))
