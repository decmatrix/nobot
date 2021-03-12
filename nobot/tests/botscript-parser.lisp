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

(define-parser-test bs-parser.literal.1
    "\"hello\""
  (<LITERAL> (<CHAR-STRING> "\"hello\""))
  :literal)

(define-parser-test bs-parser.literal.2
    "8081"
  (<LITERAL> (<NUMBER-STRING> 8081))
  :literal)

(define-parser-test bs-parser.literal.3
    "none"
  (<LITERAL> (<KEYWORD> NONE))
  :literal)

(define-parser-test bs-parser.literal.4
    "[1, none, \"aa\", 4, 5]"
  (<LITERAL>
   (<ITEM-LIST> (<LITERAL> (<NUMBER-STRING> 1)) (<LITERAL> (<KEYWORD> NONE))
                (<LITERAL> (<CHAR-STRING> "\"aa\"")) (<LITERAL> (<NUMBER-STRING> 4))
                (<LITERAL> (<NUMBER-STRING> 5))))
  :literal)

(define-parser-test bs-parser.rest-literal-list.1
    ", 1, none, \"hello\""
  (<REST-LITERAL-LIST> (<LITERAL> (<NUMBER-STRING> 1))
                       (<LITERAL> (<KEYWORD> NONE)) (<LITERAL> (<CHAR-STRING> "\"hello\"")))
  :rest-literal-list)

;; TODO: impl test, [] ?
(define-parser-test bs-parser.item-list.1
    "[1, [], [1, [1]], \"1\"]"
  ()
  :item-list)

(define-parser-test bs-parser.string-or-number.1
    "\"hello\""
  (<STRING-OR-NUMBER> (<CHAR-STRING> "\"hello\""))
  :string-or-number)

(define-parser-test bs-parser.string-or-number.2
    "8081"
  (<STRING-OR-NUMBER> (<NUMBER-STRING> 8081))
  :string-or-number)

(define-parser-test bs-parser.literal-or-id.1
    "11"
  (<LITERAL-OR-ID> (<LITERAL> (<NUMBER-STRING> 11)))
  :literal-or-id)

;; TODO: impl test
(define-parser-test bs-parser.literal-or-id.2
    "greetings"
  ()
  :literal-or-id)

(define-parser-test bs-parser.eql-expr.1
    "11"
  (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 11)))
  :eql-expr)

(define-parser-test bs-parser.eql-expr.2
    "greetings"
  (<EQL-EXPR> (<ID> "greetings"))
  :eql-expr)

(define-parser-test bs-parser.comparison-expr.1
    "11 == greetings"
  (<COMPARISON-EXPR> (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 11)))
                     (<EQL-EXPR> (<ID> "greetings")))
  :comparison-expr)

(define-parser-test bs-parser.logic-expr.1
    "11 == greetings"
  (<LOGIC-EXPR>
   (<COMPARISON-EXPR> (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 11)))
                      (<EQL-EXPR> (<ID> "greetings"))))
  :logic-expr)

(define-parser-test bs-parser.cond-expr.1
    "11 == greetings"
  (<COND-EXPR>
   (<LOGIC-EXPR>
    (<COMPARISON-EXPR> (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 11)))
                       (<EQL-EXPR> (<ID> "greetings")))))
  :cond-expr)

(define-parser-test bs-parser.save-to-expr.1
    "save 21 to user-age"
  (<SAVE-TO-EXPR> (<LITERAL-OR-ID> (<LITERAL> (<NUMBER-STRING> 21)))
                  (<ID> "user-age"))
  :save-to-expr)

(define-parser-test bs-parser.say-expr-arg.1
    "\"hello\""
  (<SAY-EXPR-ARG> (<CHAR-STRING> "\"hello\""))
  :say-expr-arg)

(define-parser-test bs-parser.say-expr-arg.2
    "21"
  (<SAY-EXPR-ARG> (<NUMBER-STRING> 21))
  :say-expr-arg)

(define-parser-test bs-parser.say-expr-arg.3
    "user-name"
  (<SAY-EXPR-ARG> (<ID> "user-name"))
  :say-expr-arg)

;; TODO: impl test
(define-parser-test bs-parser.rest-say-expr-args.1
    "\"Hello,\" user-name \". Your age is\" 21"
  ()
  :rest-say-expr-args)

;; TODO: impl test
(define-parser-test bs-parser.rest-say-expr-args.2
    ""
  ()
  :rest-say-expr-args)

;; TODO: impl test
(define-parser-test bs-parser.say-expr-args.1
    "\"Hi, \" user-name"
  ()
  :say-expr-args)

;; TODO: impl test
(define-parser-test bs-parser.say-expr.1
    "say \"Hello, world!\""
  ()
  :say-expr)

(define-parser-test bs-parser.gotov-expr.1
    "gotov a"
  (<GOTOV-EXPR> (<ID> "a"))
  :gotov-expr)

;; TODO: impl test
(define-parser-test bs-parser.else-block.1
    "else { gotov a; }"
  :else-block)

;; TODO: impl test
(define-parser-test bs-parser.if-stmt.1
    :if-stmt)

(define-parser-test bs-parser.stmt.1
    "gotov a;"
  (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))
  :stmt)

(define-parser-test bs-parser.expr.1
    "gotov a"
  (<EXPR> (<GOTOV-EXPR> (<ID> "a")))
  :expr)

;; TODO: impl test
(define-parser-test bs-parser.stmt-list.1
    "gotov a; save 1 to b;"
  ()
  :stmt-list)

;; TODO: impl test
(define-parser-test bs-parser.state-decl.1
    "act-a : {}"
  ()
  :state-decl)



