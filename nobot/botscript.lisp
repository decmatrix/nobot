(uiop:define-package :nobot/botscript
    (:use :cl
          :nobot/botscript/lexer)
  (:export
   ;; lexer
   #:disassemble-source
   #:disassemble-string
   #:disassemble-file))
