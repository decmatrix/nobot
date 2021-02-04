;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/parser-generator
    (:use :cl
          :nobot/botscript/parser/acacia/configuration
          :nobot/botscript/parser/acacia/construction)
  (:import-from :anaphora
                #:aif)
  (:import-from :nobot/utils
                #:reintern)
  (:export #:define-rule))

(in-package :nobot/botscript/parser/acacia/parser-generator)

(defmacro define-rule (rule-name () body)
  (let ((rule-name (intern (string rule-name) :keyword)))
    `(defmethod rule-> ((rule-name (eql ,rule-name)))
       ,(build-rule-body `(,body) `,rule-name))))

(defun build-rule-body (quote-body-tree &optional rule-name)
  (labels ((%build (body-tree)
             (let* ((root (car body-tree)))
               (case root
                 (:and
                  (assert rule-name)
                  `(list ($conf-rule->term-sym ,rule-name)
                         ,@(mapcar #'%build (cdr body-tree))))
                 (:rule
                  (destructuring-bind (kword rule-name)
                      body-tree
                    (declare (ignore kword))
                    `(rule-> ,(to-kword rule-name))))
                 (:keyword)
                 (:or)
                 (:empty
                  t)
                 (:id)
                 (:delimiter)
                 (:string)
                 (:number-string)))))
    (%build (car quote-body-tree))))

(defun to-kword (sym)
  (reintern sym :keyword))
