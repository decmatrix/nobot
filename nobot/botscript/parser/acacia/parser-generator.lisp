;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/parser-generator
    (:use :cl
          :nobot/botscript/parser/acacia/configuration)
  (:import-from :nobot/utils
                #:reintern)
  (:export #:define-rule))

(in-package :nobot/botscript/parser/acacia/parser-generator)

(defgeneric rule (rule-name)
  (:method (rule-name)
    (error "unknown rule: [~a]" rule-name)))

(defmacro define-rule (rule-name () body)
  (let ((rule-name (intern (string rule-name) :keyword)))
    `(defmethod rule ((rule-name (eql ,rule-name)))
       ,(build-rule-body `(,body) ,rule-name))))

(defun build-rule-body (quote-body-tree
                        &optional rule-name
                        &key conditional)
  (let* ((body-tree (car quote-body-tree))
         (root (car body-tree)))
    (case root
      (:and
       `(list ,(funcall (get-fun/rule->term-sym *parser-configuration*)
                        rule-name)
              ,@(mapcar #'build-rule-body (cdr body-tree))))
      (:rule
       (destructuring-bind (kword rule-name)
           body-tree
         (declare (ignore kword))
         `(rule ,(to-kword rule-name))))
      (:keyword
       (destructuring-bind (kword real-kwrod)
           `(if )))
      (:or
       `)
      (:empty)
      (:id)
      (:delimiter)
      (:string)
      (:number-string)
      (:number-string))))

(defun to-kword (sym)
  (reintern sym :keyword))
