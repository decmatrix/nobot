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

(define-parser-test bs-parser.vertex-option-name.2
    "type"
  (<VERTEX-OPTION-NAME> (<KEYWORD> TYPE))
  :vertex-option-name)

(define-parser-test bs-parser.vertex-option.1
    "act = in"
  (<VERTEX-OPTION> (<VERTEX-OPTION-NAME> (<KEYWORD> ACT)) (<DELIMITER> ASSIGN)
                   (<VERTEX-OPTION-VAL> (<KEYWORD> IN)))
  :vertex-option)

(define-parser-test bs-parser.item.1
    "8080"
  (<ITEM> (<LITERAL> (<NUMBER-STRING> 8080)))
  :item)

(define-parser-test bs-parser.item-tail-list.1
    ", 1,3, \"js\""
  (<ITEM-TAIL-LIST>
   (<DELIMITER> COMMA) (<ITEM> (<LITERAL> (<NUMBER-STRING> 1)))
   (<ITEM-TAIL-LIST>
    (<DELIMITER> COMMA) (<ITEM> (<LITERAL> (<NUMBER-STRING> 3)))
    (<ITEM-TAIL-LIST>
     (<DELIMITER> COMMA)
     (<ITEM> (<LITERAL> (<CHAR-STRING> "\"JS\""))))))
  :item-tail-list)

(define-parser-test bs-parser.item-list.1
    "1, 2, \"js\", 5"
  (<ITEM-LIST>
   (<ITEM> (<LITERAL> (<NUMBER-STRING> 1)))
   (<ITEM-TAIL-LIST>
    (<DELIMITER> COMMA) (<ITEM> (<LITERAL> (<NUMBER-STRING> 2)))
    (<ITEM-TAIL-LIST>
     (<DELIMITER> COMMA)
     (<ITEM> (<LITERAL> (<CHAR-STRING> "\"JS\"")))
     (<ITEM-TAIL-LIST>
      (<DELIMITER> COMMA)
      (<ITEM> (<LITERAL> (<NUMBER-STRING> 5)))))))
  :item-list)

;;TODO: issue #14
;; (define-parser-test bs-parser.)

(define-parser-test bs-parser.data-seq.1
    "[1, 2, \"hi\", 3]"
  (<DATA-SEQ>
   (<DELIMITER> O-SQ-BRACKET)
   (<ITEM-LIST> (<ITEM> (<LITERAL> (<NUMBER-STRING> 1)))
                (<ITEM-TAIL-LIST>
                 (<DELIMITER> COMMA)
                 (<ITEM> (<LITERAL> (<NUMBER-STRING> 2)))
                 (<ITEM-TAIL-LIST>
                  (<DELIMITER> COMMA)
                  (<ITEM> (<LITERAL> (<CHAR-STRING> "\"HI\"")))
                  (<ITEM-TAIL-LIST>
                   (<DELIMITER> COMMA)
                   (<ITEM> (<LITERAL> (<NUMBER-STRING> 3)))))))
   (<DELIMITER> C-SQ-BRACKET))
  :data-seq)

;;TODO: stmt, stmt-list -> need grammar for expr
;;TODO: vertex-options, see issue #14
;;TODO: data-expr, see issue #15
;;TODO: string ot nume, see issue #16








