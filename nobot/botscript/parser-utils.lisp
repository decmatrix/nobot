(uiop:define-package :nobot/botscript/parser-utils
    (:use :cl
          :alexandria
          :trivia
          :nobot-utils
          :nobot/botscript/tree-utils
          :nobot/botscript/lexer)
  (:export #:with-disassembled-source))

(in-package :nobot/botscript/parser-utils)

(defparameter *from-tokens-source-node-instance* nil)
(defparameter *tokens-seq* nil)

(defmacro with-disassembled-source ((source type) &body body)
  `(let* ((*from-tokens-source-node-instance*
          (disassemble-source ,source
                              :type ,type
                              :return-instance t))
         (*tokens-seq* (get-tokens-seq *from-tokens-source-node-instance*)))
     ,@body))
