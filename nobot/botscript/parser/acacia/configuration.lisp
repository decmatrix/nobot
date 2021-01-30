;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/configuration
    (:use :cl)
  (:import-from :nobot/botscript/nodes
                ;; types
                #:from-tokens-source-node
                #:token-pointer)
  (:import-from :nobot/botscript/token-utils
                #:make-token-pointer
                #:get-next-token)
  (:export #:with-init-acacia
           ;; configs
           #:$conf-get-start-rule
           #:$conf-funcall/rule->term-sym
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
   (token-source
    :type from-tokens-source-node
    :initarg :tokens-source
    :reader get-tokens-source)
   (token-ptr
    :type token-pointer
    :writer set-token-pointer
    :reader get-token-pointer)))

(defmacro with-init-acacia ((&rest configs-args) &body body)
  `(let ((*acacia-configuration*
          (make-instance 'acacia-configuration
                         ,@config-args)))
     ($$conf-make-token-pointer)
     ,@body))


;; configuration getters
(defun $conf-get-start-rule ()
  (get-start-rule *acacia-configuration*))

(defun $conf-funcall/rule->term-sym (rule-name)
  (funcall (get-fun/rule->term-sym *acacia-configuration*)
           rule-name))

(defun $conf-next-token ()
  (get-next-token
   (get-token-pointer *acacia-configuration*)))

;; configurators
(defun $$conf-make-token-pointer ()
  (set-token-pointer (make-token-pointer
                      (get-tokens-source *acacia-configuration*))
                     *acacia-configuration*))

