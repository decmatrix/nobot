(uiop:define-package :nobot/core/botscript
    (:use :cl
          :nobot/core/botscript/lexer)
  (:nicknames :botscript)
  (:export :parse-file))
