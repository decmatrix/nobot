(uiop:define-package :nobot/utils/context-variables
    (:use :cl)
  (:export #:define-context-var
           #:setf-context-var))

(in-package :nobot/utils/context-variables)

(defclass outside-context-node () ())

(defconstant +is-outside-context+ (make-instance 'outside-context-node))

(defmacro define-context-var (var-name)
  `(defparameter ,var-name ,+is-outside-context+))

;; what if is not context var ?
(defmacro setf-context-var (var-name value)
  `(if (is-outside-context-? ,var-name)
       (context-var-error)
       (setf ,var-name )))

(defun is-outside-context-? (var)
  (eq var +is-outside-context+))

(defun context-var-error ()
  (error "Using context macros outside context,
this operation was canceled"))
