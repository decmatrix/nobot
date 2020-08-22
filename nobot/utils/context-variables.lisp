(uiop:define-package :nobot/utils/context-variables
    (:use :cl)
  (:import-from :nobot/utils/common-utils
                #:define-constant-?)
  (:export #:defcontextvar
           #:setf-context-var))

(in-package :nobot/utils/context-variables)

(defclass outside-context-node () ())

(define-constant-? +is-outside-context+ (make-instance 'outside-context-node))

(defmacro defcontextvar (var-name)
  `(defparameter ,(intern (symbol-name var-name) *package*)
     +is-outside-context+))

;; what if is not context var ?
(defmacro setf-context-var (var-name value)
  `(if (is-outside-context-? ,var-name)
       (context-var-error)
       (setf ,var-name ,value)))

(defun is-outside-context-? (var)
  (eq var +is-outside-context+))

(defun context-var-error ()
  (error "Using context macros outside context,
this operation was canceled"))
