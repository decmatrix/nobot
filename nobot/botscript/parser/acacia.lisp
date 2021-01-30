;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser-generator
    (:use :cl)
  (:import-from)
  (:export #:with-generated-parser
           #:define-rule))

(in-package :nobot/botscript/parser-generator)

;; BIG TODO: move this package to separate lisp project:
;; parser generator - acacia: https://github.com/bohdan-sokolovskyi/acacia


(defvar *parser-configuration*)

(defclass parser-configuration ()
  ((start-rule
    :initarg :start-from
    :reader get-start-rule)
   (rule->sym-term
    :initarg :fun/rule->sym-term
    :reader :get-fun/rule->sym-term)))

(defmacro make-parser-config (&rest args)
  `(make-instance 'parser-configuration
                  ,@ args))

(defmacro with-generated-parser ((&rest configs-args) &body body)
  `(let ((*parser-configuration*
          (make-instance 'parser-configuration
                         .@config-args)))
     ,@body))

(defgeneric rule (rule-name)
  (:method (rule-name)
    (error "unknown rule: [~a]" rule-name)))

(defmacro define-rule (rule-name () body)
  (let ((rule-name (intern (string rule-name) :keyword)))
    `(defmethod rule ((rule-name (eql ,rule-name)))
       ,(build-rule-body `(,body)))))

(defun build-rule-body (quote-body-tree)
  (let* ((body-tree (car quote-body-tree))
         (root (car body-tree)))
    (case root
      (:and
       `(list ))
      (:rule)
      (:keyword)
      (:or)
      (:empty)
      (:id)
      (:delimiter)
      (:string)
      (:number-string)
      (:number-string))))
