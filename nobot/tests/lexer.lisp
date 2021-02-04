;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests/lexer
    (:use :cl
          :lisp-unit
          :nobot/botscript/token-utils
          :nobot/botscript/lexer))

(in-package :nobot/tests/lexer)

(defmacro define-lexer-test (name input-string expected-tokens-seq &key with-pos)
  `(define-test ,name
     (assert-true (same-tokens-seq-?
                   (disassemble-string ,input-string
                                       :convert-with-pos ,with-pos)
                   ',expected-tokens-seq
                   :without-pos ,(not with-pos)))))


(define-lexer-test lexer.1
    "@def port 8081"
  ((<KEYWORD> @DEF) (<ID> "PORT") (<NUMBER-STRING> 8081)))

(define-lexer-test lexer.2
    "$combo bot-localhost"
  ((<KEYWORD> $COMBO) (<ID> "BOT-LOCALHOST")))

(define-lexer-test lexer.3
    "1234name"
  ((<number-string> 1234) (<id> "name")))

(define-lexer-test lexer.4
    "!use std-module"
  ((<keyword> !use) (<id> "std-module")))

(define-lexer-test lexer.5
    "#exe lang python"
  ((<keyword> |#exe|) (<id> "lang") (<id> "python")))

