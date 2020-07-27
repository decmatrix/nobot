(uiop:define-package :nobot/core/botscript
    (:use :cl
          :nobot/core/botscript/lexer
          :nobot/core/botscript/parser)
  (:nicknames :botscript)
  (:export :parse-file))
