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
  (:import-from :nobot/botscript/lexer/token
                #:token-typep
                #:token-value-equal-to
                #:value-of-token
                #:convert-token
                #:get-token-type
                #:get-position
                )
  (:export #:define-rule
           #:rule->))

(in-package :nobot/botscript/parser/acacia/parser-generator)

(defgeneric rule-> (rule-name &key first-fail-no-error as-single-list)
  (:method (rule-name &key first-fail-no-error as-single-list)
    (declare (ignore first-fail-no-error as-single-list))
    (error 'acacia-undefined-rule
           :rule rule-name)))

(defmacro with-next-token (() &body body)
  `(let ((next ($conf-next-token)))
     ,@body))

(defmacro define-rule (rule-name () body)
  (let ((rule-name (intern (string rule-name) :keyword)))
    `(defmethod rule-> ((rule-name (eql ,rule-name)) &key first-fail-no-error as-single-list)
       (declare (ignorable first-fail-no-error as-single-list))
       ,(build-rule-body `(,body) `,rule-name))))

(defun build-rule-body (quote-body-tree rule-name)
  (labels ((%build (body-tree &key
                              first-fail-no-error
                              merge-sub-trees)
             (let ((root (car body-tree)))
               (case root
                 (:rule
                  (destructuring-bind (rule-name &optional as-single-list)
                      (cdr body-tree)
                    (unless (or (null as-single-list)
                                (eq as-single-list :as-single-list))
                      (error 'acacia-unknown-argument-of-rule
                             :unknown-arg as-single-list))
                    `(awhen (rule-> ,(to-kword rule-name)
                                    :first-fail-no-error ,first-fail-no-error)
                       (list it))))
                 (:and
                  (let* ((sub-rules (cdr body-tree))
                         (first-rule (car sub-rules)))
                    (unless sub-rules
                      (error 'acacia-empty-body-of-rule
                             :rule :and))
                    `(awhen ,(%build first-rule
                                     :first-fail-no-error first-fail-no-error
                                     :merge-sub-trees t)
                       (remove
                        nil
                        (append
                         (unless ,merge-sub-trees
                           (list ($conf-rule->term-sym ,rule-name)))
                         it
                         ,@ (mapcar (rcurry #'%build :merge-sub-trees t) (cdr sub-rules)))))))
                 (:or
                  (let* ((sub-rules (cdr body-tree))
                         (last-rule (lastcar sub-rules)))
                    (unless sub-rules
                      (error 'acacia-empty-body-of-rule
                             :rule :or))
                    ;;TODO: see issue #4
                    `(awhen
                         (append
                          (unless ,merge-sub-trees
                            (list ($conf-rule->term-sym ,rule-name)))
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
                            (if (eq it t)
                                nil
                                it)
                            (raise-bs-parser-error
                             "error on rule ~a" ,rule-name)))
                       (when (cdr it)
                         it))))
                 (:terminal
                  (destructuring-bind (sym &optional val exclude-from-tree)
                      (cdr body-tree)
                    (unless (or (null exclude-from-tree)
                                (eq exclude-from-tree :exclude-from-tree))
                      (error 'acacia-unknown-argument-of-rule
                             :unknown-arg exclude-from-tree))
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
                               (if ,exclude-from-tree
                                   (list nil)
                                   (list (convert-token next :with-pos nil)))
                               (if ,(if first-fail-no-error
                                        t
                                        `first-fail-no-error)
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
                                      "expected get: ~a, but got: ~a, at position line - ~a, column - ~a~a"
                                      ,(if val
                                           `($conf-terminal->description ',sym ,val)
                                           `($conf-token-rule->description ',sym))
                                      (if next
                                          ,(if val
                                               `($conf-terminal->description
                                                 ',sym
                                                 (value-of-token next))
                                               `($conf-token-rule->description
                                                 (get-token-type next)))
                                          "end of source")
                                      (cdr ,pos-list)
                                      (car ,pos-list)
                                      (if (eq ($conf-get-source-type) :file)
                                          (format nil ", file: ~a."
                                                  ($conf-get-source))
                                          "."))))))))))
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

(defun to-kword (sym)
  (reintern sym :keyword))
