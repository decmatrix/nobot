;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/configuration
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :nobot/botscript/parser/acacia/error-handling
                #:acacia-unknown-source-type)
  (:import-from :nobot/botscript/lexer/token
                #:get-next-token
                #:make-token-pointer
                #:pointer-at-end-?
                #:mv-ptr-to-prev-token
                ;; type
                #:token-pointer)
  (:import-from :nobot/botscript/lexer/lexer-nodes
                #:from-tokens-source-node)
  (:export #:make-configuration
           #:*acacia-configuration*
           ;; configs
           #:$conf-get-start-rule
           #:$conf-rule->term-sym
           #:$conf-rule->description
           #:$conf-token-rule->token-sym
           #:$conf-token-rule->description
           #:$conf-terminal->sym
           #:$conf-terminal->description
           #:$conf-next-token
           #:$conf-get-source-type
           #:$conf-get-source
           #:$conf-pointer-at-end-?
           #:$conf-mv-ptr-to-prev-token))

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
   (token-rule->token-sym
    :type function
    :initarg :fun/token-rule->token-sym
    :reader get-fun/token-rule->token-sym)
   (token-rule->description
    :type function
    :initarg :fun/token-rule->token->description
    :reader get-fun/token-rule->description)
   (terminal->sym
    :type function
    :initarg :fun/terminal->sym
    :reader get-fun/terminal->sym)
   (terminal->description
    :type function
    :initarg :fun/terminal->description
    :reader get-fun/terminal->description)
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

;;TODO: add initargs
(defmethod initialize-instance :around ((config acacia-configuration)
                                        &key
                                          start-from
                                          fun/rule->term-sym
                                          fun/rule->description
                                          fun/token-rule->token-sym
                                          fun/token-rule->description
                                          fun/terminal->sym
                                          fun/terminal->description
                                          tokens-source
                                          source-type
                                          source)
  (unless (or (eq source-type :string)
              (eq source-type :file))
    (make-condition 'acacia-unknown-source-type
                    :unknown-source-type source-type))
  (call-next-method config
                    :start-from start-from
                    :fun/rule->term-sym fun/rule->term-sym
                    :fun/rule->description fun/rule->description
                    :fun/token-rule->token-sym fun/token-rule->token-sym
                    :fun/token-rule->token->description fun/token-rule->description
                    :fun/terminal->sym fun/terminal->sym
                    :fun/terminal->description fun/terminal->description
                    :token-pointer (make-token-pointer tokens-source)
                    :source-type source-type
                    :source source))

(defmacro make-acacia-configuration (&rest config-args)
  `(make-instance 'acacia-configuration
                  ,@config-args))


;; configuration getters
(defun $conf-get-start-rule ()
  (get-start-rule *acacia-configuration*))

(defun $conf-rule->term-sym (rule-name)
  (funcall (get-fun/rule->term-sym *acacia-configuration*)
           rule-name))

(defun $conf-rule->description (rule-name)
  (funcall (get-fun/rule->description *acacia-configuration*)
           rule-name))

(defun $conf-token-rule->token-sym (token-rule)
  (funcall (get-fun/token-rule->token-sym *acacia-configuration*)
           token-rule))

(defun $conf-token-rule->description (token-rule)
  (funcall (get-fun/token-rule->description *acacia-configuration*)
           token-rule))

(defun $conf-terminal->sym (token-type val)
  (funcall (get-fun/terminal->sym *acacia-configuration*)
           token-type
           val))

(defun $conf-terminal->description (token-type val)
  (funcall (get-fun/terminal->description *acacia-configuration*)
           token-type
           val))

(defun $conf-next-token ()
  (get-next-token
   (get-token-pointer *acacia-configuration*)))

(defun $conf-mv-ptr-to-prev-token ()
  (mv-ptr-to-prev-token 
   (get-token-pointer *acacia-configuration*)))

(defun $conf-get-source-type ()
  (get-source-type *acacia-configuration*))

(defun $conf-get-source ()
  (get-source *acacia-configuration*))

(defun $conf-pointer-at-end-? ()
  (pointer-at-end-? (get-token-pointer *acacia-configuration*)))
