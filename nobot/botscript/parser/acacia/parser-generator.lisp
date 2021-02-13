;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/parser-generator
    (:use :cl
          :nobot/botscript/parser/acacia/configuration
          :nobot/botscript/parser/acacia/construction
          :nobot/botscript/parser/acacia/error-handling)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-parser-error)
  (:import-from :alexandria
                #:lastcar
                #:rcurry
                #:with-gensyms)
  (:import-from :anaphora
                #:aif
                #:awhen
                #:it)
  (:import-from :nobot/utils
                #:reintern
                #:to-symbol)
  (:import-from :nobot/botscript/lexer/token
                #:token-typep
                #:token-value-equal-to
                #:value-of-token
                #:convert-token
                #:get-token-type
                #:get-position)
  (:export #:define-rule))

(in-package :nobot/botscript/parser/acacia/parser-generator)

(defmacro with-next-token (() &body body)
  `(let ((next ($conf-next-token)))
     ,@body))

(defmacro define-rule (rule-name () body)
  (let ((rule-name (intern (string rule-name) :keyword)))
    `(defmethod rule-> ((rule-name (eql ,rule-name)) &key first-fail-no-error)
       (declare (ignorable first-fail-no-error))
       ,(build-rule-body `(,body) `,rule-name))))


(defun build-rule-body (quote-body-tree rule-name)
  (labels ((%build (body-tree &key first-fail-no-error)
             (let ((root (car body-tree)))
               (case root
                 (:rule
                  (destructuring-bind (rule-name)
                      (cdr body-tree)
                    `(rule-> ,(to-kword rule-name) :first-fail-no-error ,first-fail-no-error)))
                 (:and
                  (let* ((sub-rules (cdr body-tree))
                         (first-rule (car sub-rules)))
                    (unless sub-rules
                      (make-condition 'acacia-empty-body-of-rule
                                      :rule :and))
                    `(awhen ,(%build first-rule :first-fail-no-error first-fail-no-error)
                       (append
                        ($conf-rule->term-sym ,rule-name)
                        it
                        ,@ (mapcar #'%build (cdr sub-rules))))))
                 (:or
                  (let* ((sub-rules (cdr body-tree))
                         (last-rule (lastcar sub-rules)))
                    (unless sub-rules
                      (make-condition 'acacia-empty-body-of-rule
                                      :rule :or))
                    ;;TODO: see issue #4
                    `(append
                      ($conf-rule->term-sym ,rule-name)
                      (aif (or
                            ,@ (mapcar (rcurry #'%build :first-fail-no-error t)
                                       (butlast sub-rules))
                            ,(if (is-empty-rule last-rule)
                                 t
                                 (%build last-rule :first-fail-no-error t)))
                        (if (eq it t)
                            nil
                            it)
                        (raise-bs-parser-error
                         "error on rule ~a" ,rule-name)))))
                 (:no-term-sym
                  (destructuring-bind (sym &optional val)
                      (cdr body-tree)
                    (with-gensyms (converted-sym converted-val pos-list)
                      `(with-next-token ()
                         (let ((,converted-sym ($conf-token-rule->token-sym ',sym))
                               (,converted-val ',(to-symbol val)))
                           (declare (ignorable ,converted-val))
                           (if (and (token-typep next ,converted-sym)
                                    ,(if val
                                         `(token-value-equal-to next ,converted-val)
                                         t))
                               (convert-token next :with-pos nil)
                               (if ,(if first-fail-no-error
                                        t
                                        `first-fail-no-error)
                                   nil
                                   (let ((,pos-list (get-position next)))
                                     (raise-bs-parser-error
                                      "expected get: ~a, but got: ~a. File: ~a, line - ~a, column - ~a."
                                      ,(if val
                                           `($conf-no-term->description ',sym ,val)
                                           converted-sym)
                                      ,(if val
                                           `(value-of-token next)
                                           `(get-token-type next))
                                      (cdr ,pos-list)
                                      (1+ (car ,pos-list)))))))))))
                 ;;FIXME: temporary solution for `expr' rule
                 (:empty t)
                 (t (make-condition 'acacia-unknown-parser-rule
                                    :unknown-rule root))))))
    (%build (car quote-body-tree))))

(defun is-empty-rule (rule)
  (and
   (listp rule)
   (eq (car rule) :empty)
   (not (cdr rule))))

(defun to-kword (sym)
  (reintern sym :keyword))
