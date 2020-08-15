(uiop:define-package :nobot/botscript/parser-utils
    (:use :cl
          :alexandria
          :nobot/utils
          :nobot/botscript/tree-utils
          :nobot/botscript/lexer
          :nobot/botscript/nodes
          :nobot/botscript/types)
  (:export #:with-disassembled-source
           #:defun-state))

(in-package :nobot/botscript/parser-utils)

(defparameter *from-tokens-source-node-instance* nil)
(defparameter *tokens-seq* nil)
(defparameter *token-pointer* nil)

(defmacro with-disassembled-source ((source type) &body body)
  `(let* ((*from-tokens-source-node-instance*
          (disassemble-source ,source
                              :type ,type
                              :return-instance t))
         (*tokens-seq* (get-tokens-seq *from-tokens-source-node-instance*)))
     ,@body))

(defmacro defun-state (sort-type (&rest args) &body body)
  (let ((symbol-of-sort-type (get-sort-symbol sort-type)))
    `(defun ,(intern (concatenate 'string "@" (string symbol-of-sort-type)))
         ,args
       (with-tree (,symbol-of-sort-type)
         ,@body))))
