;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests/botscript-parser
    (:use :cl
          :lisp-unit)
  (:import-from :nobot/botscript/parser
                #:parse-string)
  (:import-from :nobot/botscript/parser/acacia
                #:same-parse-tree-?))

(in-package :nobot/tests/botscript-parser)

(defmacro define-parser-test (name
                              input-string
                              expected-tree
                              &optional (sort-type :script))
  `(define-test ,name
     (assert-true
      (same-parse-tree-?
       (parse-string ,input-string :sort-type ,sort-type)
       ',expected-tree))))

(define-parser-test bs-parser.start-stmt
    "start from a"
  (<START-STMT> (<KEYWORD> START) (<KEYWORD> FROM) (<ID> "a"))
  :start-stmt)

(define-parser-test bs-parser.literal.1
    "\"test\""
  (<LITERAL> (<CHAR-STRING> "\"TEST\""))
  :literal)

(define-parser-test bs-parser.literal.2
    "8080"
  (<LITERAL> (<NUMBER-STRING> 8080))
  :literal)

(define-parser-test bs-parser.vertex-option-val.1
    "in"
  (<VERTEX-OPTION-VAL> (<KEYWORD> IN))
  :vertex-option-val)

(define-parser-test bs-parser.vertex-option-val.2
    "out"
  (<VERTEX-OPTION-VAL> (<KEYWORD> OUT))
  :vertex-option-val)

(define-parser-test bs-parser.vertex-option-val.3
    "label"
  (<VERTEX-OPTION-VAL> (<ID> "label"))
  :vertex-option-val)

(define-parser-test bs-parser.vertex-option-name.1
    "act"
  (<VERTEX-OPTION-NAME> (<KEYWORD> ACT))
  :vertex-option-name)

(define-parser-test bs-parser.vertex-option-name.1
    "act"
  (<VERTEX-OPTION-NAME> (<KEYWORD> ACT))
  :vertex-option-name)

(define-parser-test bs-parser.vertex-option-name.2
    "type"
  (<VERTEX-OPTION-NAME> (<KEYWORD> TYPE))
  :vertex-option-name)




