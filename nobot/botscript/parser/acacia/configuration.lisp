;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/configuration
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :nobot/botscript/lexer/token
                #:get-next-token
                #:make-token-pointer
                ;; type
                #:token-pointer)
  (:import-from :nobot/botscript/lexer/lexer-nodes
                #:from-tokens-source-node)
  (:import-from :nobot/botscript/parser/acacia/result-packaging
                #:pack-parse-tree)
  (:export #:with-acacia-process
           ;; configs
           #:$conf-get-start-rule
           #:$conf-rule->term-sym
           #:$conf-rule->description
           #:$conf-next-token))

(in-package :nobot/botscript/parser/acacia/configuration)

(defvar *acacia-configuration*)

(defclass acacia-configuration ()
  ((start-rule
    :type keyword
    :initarg :start-from
    :reader get-start-rule)
   (rule->term-sym
    :type function
    :initarg :fun/rule->term-sym
    :reader get-fun/rule->term-sym)
   (rule->description
    :type function
    :initarg :fun/rule->description
    :reader get-fun/rule->description)
   (token-ptr
    :type token-pointer
    :initarg :token-pointer
    :reader get-token-pointer)
   (source-type
    :type keyword
    :initarg :source-type
    :reader get-source-type)
   (source
    :type string
    :initarg :source
    :reader get-source)))

(defmethod initialize-instance :around ((config acacia-configuration)
                                        &key
                                          start-from
                                          fun/rule->term-sym
                                          fun/rule->description
                                          tokens-source
                                          source-type
                                          source)
  (unless (or (eq source-type :string)
              (eq source-type :file))
    (error "unknown source type: ~a, expected :string or :file" source-type))
  (call-next-method config
                    :start-from start-from
                    :fun/rule->term-sym fun/rule->term-sym
                    :fun/rule->description fun/rule->description
                    :token-pointer (make-token-pointer token-source)
                    :source-type source-type
                    :source source))

(defmacro with-acacia-process ((&rest configs-args &key pack-result) &body body)
  (with-gensyms (parse-tree)
    `(let ((*acacia-configuration*
            (make-instance 'acacia-configuration
                           ,@config-args)))
       ,@body
       (let ((,parse-tree
              (nobot/botscript/parser/acacia/parser-generator:rule->
               ($conf-get-start-rule))))
         ,(if pack-result
              `(pack-parse-tree ,parse-tree)
              `,parse-tree)))))


;; configuration getters
(defun $conf-get-start-rule ()
  (get-start-rule *acacia-configuration*))

(defun $conf-rule->term-sym (rule-name)
  (funcall (get-fun/rule->term-sym *acacia-configuration*)
           rule-name))

(defun $conf-rule->description (rule-name)
  (funcall (get-fun/rule->description *acacia-configuration*)
           rule-name))

(defun $conf-next-token ()
  (get-next-token
   (get-token-pointer *acacia-configuration*)))
