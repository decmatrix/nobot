;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/lexer
    (:use :cl
          :nobot/botscript/lexer/lexer-impl
          :nobot/botscript/lexer/token
          :nobot/botscript/lexer/lexer-nodes)
  (:reexport :nobot/botscript/lexer/lexer-impl
             :nobot/botscript/lexer/token
             :nobot/botscript/lexer/lexer-nodes))
