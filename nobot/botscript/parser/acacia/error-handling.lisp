;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/error-handling
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :nobot/toplevel/error-handling
                #:bs-parser-error
                #:get-error-msg
                #:raise-bs-parser-error)
  (:export #:acacia-unknown-parser-rule
           #:acacia-unknown-source-type
           #:acacia-empty-body-of-rule
           #:acacia-expected-empty-rule
           #:cond-parser-error-handle))

(in-package :nobot/botscript/parser/acacia/error-handling)

(defmacro cond-parser-error-handler ((condition-parameter) &body body)
  (with-gensyms (c)
    `(handler-case
         (,@body)
       (bs-parser-error (,c)
         (unless ,condition-parameter
           (error 'bs-parser-error
                  :error-msg (get-error-msg ,c)))))))

(define-condition acacia-unknown-parser-rule (error)
  ((unknown-rule
    :initarg :unknown-rule
    :initform nil
    :accessor get-unknown-rule))
  (:report
   (lambda (condition stream)
     (format stream "[ACACIA]: unknown parser rule: ~a.~&"
             (get-unknown-rule condition)))))

(define-condition acacia-empty-body-of-rule (error)
  ((rule
    :initarg :rule
    :initform nil
    :accessor get-rule))
  (:report
   (lambda (condition stream)
     (format stream "[ACACIA]: empty body of parser rule ~a,~&"
             (get-rule condition)))))

(define-condition acacia-unknown-source-type (error)
  ((unknown-source-type
    :initarg :unknown-source-type
    :initform nil
    :accessor get-unknown-source-type))
  (:report
   (lambda (condition stream)
     (format stream "[ACACIA]: unknown source type: ~a, expected :string or :file.~&"
             (get-unknown-source-type condition)))))

(define-condition acacia-expected-empty-rule (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "[ACACIA]: expected :empty rule as last rule in :or rule.~&"))))

