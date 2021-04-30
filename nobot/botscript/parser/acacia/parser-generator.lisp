;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/parser-generator
    (:use :cl
          :nobot/botscript/parser/acacia/configuration
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
  (:import-from :nobot/botscript/types
                #:type->keyword)
  (:import-from :nobot/botscript/lexer/token
                #:token-typep
                #:token-value-equal-to
                #:value-of-token
                #:convert-token
                #:get-token-type
                #:get-position)
  (:export #:define-rule
           #:rule->))

(in-package :nobot/botscript/parser/acacia/parser-generator)

(defgeneric rule-> (rule-name &key first-fail-no-error not-first)
  (:method (rule-name &key first-fail-no-error not-first)
    (declare (ignore first-fail-no-error not-first))
    (error 'acacia-undefined-rule
           :rule rule-name)))

(defmacro with-next-token (() &body body)
  `(let ((next ($conf-next-token)))
     ,@body))

(defmacro define-rule (rule-name () body)
  (let ((rule-name (intern (string rule-name) :keyword)))
    `(defmethod rule-> ((rule-name (eql ,rule-name)) &key first-fail-no-error not-first)
       (declare (ignorable first-fail-no-error not-first))
       ,(build-rule-body `(,body) `,rule-name))))

(defun build-rule-body (quote-body-tree rule-name)
  (labels ((%build (body-tree &key
                              first-fail-no-error
                              merge-sub-trees
                              not-first)
             (let ((root (car body-tree)))
               (case root
                 ((:rule :rule*)
                  (destructuring-bind (rule-name)
                      (cdr body-tree)
                    `(awhen (rule-> ,(to-kword rule-name)
                                    :first-fail-no-error ,(or first-fail-no-error
                                                              'first-fail-no-error)
                                    :not-first ,not-first)
                       ,(if (eq root :rule*)
                            '(cdr it)
                            'it))))
                 (:and
                  (let* ((sub-rules (cdr body-tree))
                         (first-rule (car sub-rules)))
                    (unless sub-rules
                      (error 'acacia-empty-body-of-rule
                             :rule :and))
                    (with-gensyms (res)
                      `(awhen ,(%build first-rule
                                       :first-fail-no-error (or first-fail-no-error
                                                                'first-fail-no-error)
                                       :merge-sub-trees t)
                         (let ((,res (list ,@ (mapcar
                                               (rcurry #'%build
                                                       :merge-sub-trees t
                                                       :not-first nil)
                                               (cdr sub-rules)))))
                           (unless (member nil ,res)
                             (remove
                              t
                              (append
                               (unless ,merge-sub-trees
                                 (list ($conf-rule->term-sym ,rule-name)))
                               (list it)
                               ,res))))))))
                 (:or
                  (let* ((sub-rules (cdr body-tree))
                         (last-rule (lastcar sub-rules)))
                    (unless sub-rules
                      (error 'acacia-empty-body-of-rule
                             :rule :or))
                    ;;TODO: see issue #4
                    (with-gensyms (head res)
                      `(let ((,res
                              (aif (or
                                    ,@ (mapcar (rcurry #'%build
                                                       :first-fail-no-error t
                                                       :merge-sub-trees t)
                                               (butlast sub-rules))
                                    ,(if (is-empty-rule last-rule)
                                         t
                                         (%build last-rule
                                                 :first-fail-no-error t
                                                 :merge-sub-trees t)))
                                ;;(print it)
                                it
                                (unless (and (not (or ,not-first
                                                      not-first))
                                             first-fail-no-error)
                                  (raise-parser-error ,rule-name)))))
                         (let ((,head ,(unless merge-sub-trees
                                         `(list ($conf-rule->term-sym ,rule-name)))))
                          ;;(print ,res)
                           (cond
                             ((eq ,res t) (list ,res))
                             (,res
                              (append ,head (list ,res)))))))))
                 ((:terminal :terminal*)
                  (destructuring-bind (sym &optional val)
                      (cdr body-tree)
                    (with-gensyms (converted-sym converted-val pos-list)
                      `(with-next-token ()
                         (let ((,converted-sym ($conf-token-rule->token-sym ',sym))
                               (,converted-val (if (stringp ,val)
                                                   ($conf-terminal->sym ',sym ,val)
                                                   ',(to-symbol val))))
                           (declare (ignorable ,converted-val))
                           (if (and
                                next
                                (token-typep next ,converted-sym)
                                ,(if val
                                     `(token-value-equal-to next ,converted-val)
                                     t))
                               (if ,(eq root :terminal*)
                                   t
                                   (convert-token next :with-pos nil))
                               (if ,(and (not not-first) first-fail-no-error)
                                   (progn
                                     ($conf-mv-ptr-to-prev-token)
                                     nil)
                                   (let ((,pos-list
                                          (if next
                                              (get-position next)
                                              (awhen (get-position ($conf-prev-token))
                                                (cons (car it)
                                                      (1+ (cdr it)))))))
                                     (raise-bs-parser-error
                                      "expected ~a, but got ~a, at position [~a:~a]~a"
                                      ,(if val
                                           `($conf-terminal->description ',sym ,val)
                                           `($conf-token-rule->description ',sym))
                                      (if next
                                          ,(if val
                                               `($conf-terminal->description
                                                 (type->keyword (get-token-type next))
                                                 (value-of-token next))
                                               `($conf-token-rule->description
                                                 (get-token-type next)))
                                          "end of source")
                                      (cdr ,pos-list)
                                      (car ,pos-list)
                                      (if (eq ($conf-get-source-type) :file)
                                          (format nil ", file: ~a"
                                                  ($conf-get-source))
                                          ""))))))))))
                 (t (error 'acacia-unknown-parser-rule
                           :unknown-rule root))))))
    (let* ((body-tree (car quote-body-tree))
           (first-rule (car body-tree)))
      (when (find first-rule '(:rule :empty))
        (error 'acacia-expected-empty-rule
               :rule first-rule))
      (%build body-tree))))

(defun is-empty-rule (rule)
  (and
   (listp rule)
   (eq (car rule) :empty)
   (not (cdr rule))))

;;TODO: use to-kword from nobot utils
(defun to-kword (sym)
  (reintern sym :keyword))

(defun raise-parser-error (rule-name)
  (with-next-token ()
    (let ((pos-list
           (if next
               (get-position next)
               (awhen (get-position ($conf-prev-token))
                 (cons (car it)
                       (1+ (cdr it)))))))
      (raise-bs-parser-error
       "expected ~a, but got ~a, at position [~a:~a]~a"
       ($conf-rule->description rule-name)
       (if next
           ($conf-terminal->description
            (type->keyword (get-token-type next))
            (value-of-token next))
           "end of source")
       (cdr pos-list)
       (car pos-list)
       (if (eq ($conf-get-source-type) :file)
           (format nil ", file: ~a"
                   ($conf-get-source))
           "")))))
