(uiop:define-package :nobot/core/botscript
    (:use :cl
          :nobot/botscript/lexer)
  (:export
   ;; lexer
   #:disassemble-source
   #:disassemble-string
   #:disassemble-file))
