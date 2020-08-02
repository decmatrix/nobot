(uiop:define-package :nobot/tests/botscript-lexer
    (:use :cl
          :lisp-unit
          :nobot/core/botscript))


(in-package :nobot/tests/botscript-lexer)


;;TODO is bad tests
(define-test disassemble-source-from-stirng.1
  (disassemble-source
   "@def port 8081"
   :type :string)
  '((<KEYWORD> @DEF) (<ID> "PORT") (<NUMBER-STRING> 8081)))

(define-test disassemble-source-from-stirng.2
  (disassemble-source
   "$combo localhost-bot")
  '((<KEYWORD> $COMBO) (<ID> "LOCALHOST-BOT")))


