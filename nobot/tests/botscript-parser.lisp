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

(define-parser-test bs-parser.item-list.1
    "[1, [], [1, [1]], \"1\"]"
  (<ITEM-LIST> (<LITERAL> (<NUMBER-STRING> 1)) (<LITERAL> (<ITEM-LIST>))
               (<LITERAL>
                (<ITEM-LIST> (<LITERAL> (<NUMBER-STRING> 1))
                             (<LITERAL> (<ITEM-LIST> (<LITERAL> (<NUMBER-STRING> 1))))))
               (<LITERAL> (<CHAR-STRING> "\"1\"")))
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

(define-parser-test bs-parser.literal-or-id.2
    "greetings"
  (<LITERAL-OR-ID> (<ID> "greetings"))
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

(define-parser-test bs-parser.rest-say-expr-args.1
    "\"Hello,\" user-name \". Your age is\" 21"
  (<REST-SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello,\""))
                        (<SAY-EXPR-ARG> (<ID> "user-name"))
                        (<SAY-EXPR-ARG> (<CHAR-STRING> "\". Your age is\""))
                        (<SAY-EXPR-ARG> (<NUMBER-STRING> 21)))
  :rest-say-expr-args)

(define-parser-test bs-parser.rest-say-expr-args.2
    ""
  (<REST-SAY-EXPR-ARGS>)
  :rest-say-expr-args)

(define-parser-test bs-parser.say-expr-args.1
    "\"Hi, \" user-name"
  (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hi, \""))
                   (<SAY-EXPR-ARG> (<ID> "user-name")))
  :say-expr-args)

(define-parser-test bs-parser.say-expr.1
    "say \"Hello, world!\""
  (<SAY-EXPR>
   (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello, world!\""))))
  :say-expr)

(define-parser-test bs-parser.say-expr.2
    "say \"Hello, \" user"
  (<SAY-EXPR>
   (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello, \""))
                    (<SAY-EXPR-ARG> (<ID> "user"))))
  :say-expr)

(define-parser-test bs-parser.gotov-expr.1
    "gotov a"
  (<GOTOV-EXPR> (<ID> "a"))
  :gotov-expr)

(define-parser-test bs-parser.else-block.1
    "else { gotov a; }"
  (<ELSE-BLOCK> (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))))
  :else-block)

(define-parser-test bs-parser.if-stmt.1
    "if a == 1 {} else { gotov a; }"
  (<IF-STMT>
   (<COND-EXPR>
    (<LOGIC-EXPR>
     (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "a"))
                        (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 1))))))
   (<STMT-LIST>)
   (<ELSE-BLOCK> (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a")))))))
  :if-stmt)

(define-parser-test bs-parser.if-stmt.2
    "if a == 1 { gotov b; }"
  (<IF-STMT>
   (<COND-EXPR>
    (<LOGIC-EXPR>
     (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "a"))
                        (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 1))))))
   (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "b"))))) (<ELSE-BLOCK>))
  :if-stmt)

(define-parser-test bs-parser.stmt.1
    "gotov a;"
  (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))
  :stmt)

(define-parser-test bs-parser.expr.1
    "gotov a"
  (<EXPR> (<GOTOV-EXPR> (<ID> "a")))
  :expr)

(define-parser-test bs-parser.stmt-list.1
    "gotov a; save 1 to b; say \"Hello\";"
  (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))
               (<STMT>
                (<EXPR>
                 (<SAVE-TO-EXPR> (<LITERAL-OR-ID> (<LITERAL> (<NUMBER-STRING> 1)))
                                 (<ID> "b"))))
               (<STMT>
                (<EXPR>
                 (<SAY-EXPR>
                  (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello\"")))))))
  :stmt-list)

(define-parser-test bs-parser.state-decl.1
    "act-a : {}"
  (<STATE-DECL> (<ID> "act-a") (<STMT-LIST>))
  :state-decl)

(define-parser-test bs-parser.state-decl.2
    "act-a : { if a == 1 {} else { gotov a; } say name; }"
  (<STATE-DECL> (<ID> "act-a")
                (<STMT-LIST>
                 (<STMT>
                  (<IF-STMT>
                   (<COND-EXPR>
                    (<LOGIC-EXPR>
                     (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "a"))
                                        (<EQL-EXPR> (<LITERAL> (<NUMBER-STRING> 1))))))
                   (<STMT-LIST>)
                   (<ELSE-BLOCK> (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))))))
                 (<STMT>
                  (<EXPR> (<SAY-EXPR> (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<ID> "name"))))))))
  :state-decl)

(define-parser-test bs-parser.state-actions-decls.1
    "a : {} b: {} c: {}"
  (<STATE-ACTIONS-DECLS> (<STATE-DECL> (<ID> "a") (<STMT-LIST>))
                         (<STATE-DECL> (<ID> "b") (<STMT-LIST>))
                         (<STATE-DECL> (<ID> "c") (<STMT-LIST>)))
  :state-actions-decls)

(define-parser-test bs-parser.state-actions-declarations.1
    "state-actions {a : {} b: {} c: {} }"
  (<STATE-ACTIONS-DECLARATIONS> (<STATE-DECL> (<ID> "a") (<STMT-LIST>))
                                (<STATE-DECL> (<ID> "b") (<STMT-LIST>))
                                (<STATE-DECL> (<ID> "c") (<STMT-LIST>)))
  :state-actions-declarations)

(define-parser-test bs-parser.state-actions-declarations.2
    "state-actions {}"
  (<STATE-ACTIONS-DECLARATIONS>)
  :state-actions-declarations)

(define-parser-test bs-parser.state-point-option.1
    "type: in;"
  (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in"))
  :state-point-option)

(define-parser-test bs-parser.state-point-options.1
    "act:act-a; type: in;"
  (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-a"))
                         (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in")))
  :state-point-options)

(define-parser-test bs-parser.state-point-decl.1
    "a: {act:act-a; type: in;}"
  (<STATE-POINT-DECL>
   (<ID> "a")
   (<STATE-POINT-OPTIONS>
    (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-a"))
    (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in"))))
  :state-point-decl)

(define-parser-test bs-parser.state-points-decls.1
    "a: {act:act-a; type: in;} b: {act:act-b; type: out;}"
  (<STATE-POINTS-DECLS>
   (<STATE-POINT-DECL>
    (<ID> "a")
    (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-a"))
                           (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in"))))
   (<STATE-POINT-DECL>
    (<ID> "b")
    (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-b"))
                           (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out")))))
  :state-points-decls)

(define-parser-test bs-parser.state-points-declarations.1
    "state-points { a: {act:act-a; type: in;} b: {act:act-b; type: out;} }"
  (<STATE-POINTS-DECLARATIONS>
   (<STATE-POINT-DECL>
    (<ID> "a")
    (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-a"))
                           (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in"))))
   (<STATE-POINT-DECL>
    (<ID> "b")
    (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-b"))
                           (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out")))))
  :state-points-declarations)

(define-parser-test bs-parser.start-from-stmt.1
    "start from a;"
  (<START-FROM-STMT> (<ID> "a"))
  :start-from-stmt)

(define-parser-test bs-parser.var-decl.1
    "greetengs : [\"Hi\", \"Hello\"];" 
  (<VAR-DECL> (<ID> "greetengs")
              (<LITERAL>
               (<ITEM-LIST> (<LITERAL> (<CHAR-STRING> "\"Hi\""))
                            (<LITERAL> (<CHAR-STRING> "\"Hello\"")))))
  :var-decl)

;;TODO: ; sem not check
(define-parser-test bs-parser.var-decls-list.1
    "greetengs : [\"Hi\", \"Hello\"]; start-command: \"/start\";"
  (<VAR-DECLS-LIST>
   (<VAR-DECL> (<ID> "greetengs")
               (<LITERAL>
                (<ITEM-LIST> (<LITERAL> (<CHAR-STRING> "\"Hi\""))
                             (<LITERAL> (<CHAR-STRING> "\"Hello\"")))))
   (<VAR-DECL> (<ID> "start-command") (<LITERAL> (<CHAR-STRING> "\"/start\""))))
  :var-decls-list)

;;TODO: nil if vars keyword not present
(define-parser-test bs-parser.var-declrations.1
    "vars { greetengs : [\"Hi\", \"Hello\"]; start-command: \"/start\"; }"
  (<VAR-DECLARATIONS>
   (<VAR-DECL> (<ID> "greetengs")
               (<LITERAL>
                (<ITEM-LIST> (<LITERAL> (<CHAR-STRING> "\"Hi\""))
                             (<LITERAL> (<CHAR-STRING> "\"Hello\"")))))
   (<VAR-DECL> (<ID> "start-command") (<LITERAL> (<CHAR-STRING> "\"/start\""))))
  :var-declarations)

(define-parser-test bs-parser.bot-option.1
    "port: 8082;"
  (<BOT-OPTION> (<ID> "port") (<STRING-OR-NUMBER> (<NUMBER-STRING> 8082)))
  :bot-option)

(define-parser-test bs-parser.bot-options-list.1
    "port: 8082; host: \"localhost\";"
  (<BOT-OPTIONS-LIST>
   (<BOT-OPTION> (<ID> "port") (<STRING-OR-NUMBER> (<NUMBER-STRING> 8082)))
   (<BOT-OPTION> (<ID> "host")
                 (<STRING-OR-NUMBER> (<CHAR-STRING> "\"localhost\""))))
  :bot-options-list)

(define-parser-test bs-parser.bot-options.1
    "options { port: 8082; host: \"localhost\"; }"
  (<BOT-OPTIONS>
   (<BOT-OPTION> (<ID> "port") (<STRING-OR-NUMBER> (<NUMBER-STRING> 8082)))
   (<BOT-OPTION> (<ID> "host")
                 (<STRING-OR-NUMBER> (<CHAR-STRING> "\"localhost\""))))
  :bot-options)

(define-parser-test bs-parser.compiler-option-name.1
    "@codegen"
  (<COMPILER-OPTION-NAME> (<KEYWORD> @CODEGEN))
  :compiler-option-name)

(define-parser-test bs-parser.compiler-option.1
    "@codegen \"js\";"
  (<COMPILER-OPTION> (<COMPILER-OPTION-NAME> (<KEYWORD> @CODEGEN))
                     (<STRING-OR-NUMBER> (<CHAR-STRING> "\"js\"")))
  :compiler-option)

(define-parser-test bs-parser.compiler-option.1
    "@codegen \"js\"; @codegen \"js\";"
  (<COMPILER-OPTIONS>
   (<COMPILER-OPTION>
    (<COMPILER-OPTION-NAME> (<KEYWORD> @CODEGEN))
    (<STRING-OR-NUMBER> (<CHAR-STRING> "\"js\"")))
   (<COMPILER-OPTION>
    (<COMPILER-OPTION-NAME> (<KEYWORD> @CODEGEN))
    (<STRING-OR-NUMBER> (<CHAR-STRING> "\"js\""))))
  :compiler-options)

(define-parser-test bs-parser.bot-declaration.1
    "
bot {
    options {
        name: \"Test\";
        port: 8082;
        host: \"localhost\";
    }

    vars {
        start-command: \"/start\";
        greetings: [\"Hi\", \"Hello\", \"Hey you\"];
        user-name: none;
    }

    start from a;

    state-points {
        a: {
            act: act-a;
            type: in;
        }

        b: {
            act: act-b;
            type: out;
        }

        c: {
            act: act-c;
            type: out;
        }

        default: {
            act: act-defualt;
            type: out;
        }
    }

    state-actions {
        act-a: {
            if input == start-command {
                say \"Hello\";
                gotov b;
            } else {
                gotov default;
            }

            if input == greetings {
                gotov a;
            } else {
                gotov default;
            }
        }

        act-b: {
            say \"What is your name ?\";
            gotov c;
        }

        act-c: {
            save input to user-name;
            say \"Hello\" input;
            gotov a;
        }

        act-default {
            say \"Sorry, i can't understand you\";
            gotov a;
        }
    }
}"
  (<BOT-DECLARATION>
   (<BOT-OPTIONS>
    (<BOT-OPTION> (<ID> "name") (<STRING-OR-NUMBER> (<CHAR-STRING> "\"Test\"")))
    (<BOT-OPTION> (<ID> "port") (<STRING-OR-NUMBER> (<NUMBER-STRING> 8082)))
    (<BOT-OPTION> (<ID> "host")
                  (<STRING-OR-NUMBER> (<CHAR-STRING> "\"localhost\""))))
   (<VAR-DECLARATIONS>
    (<VAR-DECL> (<ID> "start-command") (<LITERAL> (<CHAR-STRING> "\"/start\"")))
    (<VAR-DECL> (<ID> "greetings")
                (<LITERAL>
                 (<ITEM-LIST> (<LITERAL> (<CHAR-STRING> "\"Hi\""))
                              (<LITERAL> (<CHAR-STRING> "\"Hello\""))
                              (<LITERAL> (<CHAR-STRING> "\"Hey you\"")))))
    (<VAR-DECL> (<ID> "user-name") (<LITERAL> (<KEYWORD> NONE))))
   (<START-FROM-STMT> (<ID> "a"))
   (<STATE-POINTS-DECLARATIONS>
    (<STATE-POINT-DECL>
     (<ID> "a")
     (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-a"))
                            (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in"))))
    (<STATE-POINT-DECL>
     (<ID> "b")

     (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-b"))
                            (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out"))))
    (<STATE-POINT-DECL>
     (<ID> "c")
     (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-c"))
                            (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out"))))
    (<STATE-POINT-DECL> (<ID> "default")
                        (<STATE-POINT-OPTIONS>
                         (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-defualt"))
                         (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out")))))
   (<STATE-ACTIONS-DECLARATIONS>
    (<STATE-DECL>
     (<ID> "act-a")
     (<STMT-LIST>
      (<STMT>
       (<IF-STMT>
        (<COND-EXPR>
         (<LOGIC-EXPR>
          (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "input"))
                             (<EQL-EXPR> (<ID> "start-command")))))
        (<STMT-LIST>
         (<STMT>
          (<EXPR>
           (<SAY-EXPR>
            (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello\""))))))
         (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "b")))))
        (<ELSE-BLOCK>
         (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "default"))))))))
      (<STMT>
       (<IF-STMT>
        (<COND-EXPR>
         (<LOGIC-EXPR>
          (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "input"))
                             (<EQL-EXPR> (<ID> "greetings")))))
        (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a")))))
        (<ELSE-BLOCK>
         (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "default"))))))))))
    (<STATE-DECL>
     (<ID> "act-b")
     (<STMT-LIST>
      (<STMT>
       (<EXPR>
        (<SAY-EXPR>
         (<SAY-EXPR-ARGS>
          (<SAY-EXPR-ARG> (<CHAR-STRING> "\"What is your name ?\""))))))
      (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "c"))))))
    (<STATE-DECL>
     (<ID> "act-c")
     (<STMT-LIST>
      (<STMT>
       (<EXPR>
        (<SAVE-TO-EXPR> (<LITERAL-OR-ID> (<ID> "input")) (<ID> "user-name"))))
      (<STMT>
       (<EXPR>
        (<SAY-EXPR>
         (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello\""))
                          (<SAY-EXPR-ARG> (<ID> "input"))))))
      (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))))
    (<STATE-DECL>
     (<ID> "act-default")
     (<STMT-LIST>
      (<STMT>
       (<EXPR>
        (<SAY-EXPR>
         (<SAY-EXPR-ARGS>
          (<SAY-EXPR-ARG>
           (<CHAR-STRING> "\"Sorry, i can't understand you\""))))))
      (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))))))
  :bot-declaration)

(define-parser-test bs-parser.script.1
    "
@codegen \"js\";

bot {
    options {
        name: \"Test\";
        port: 8082;
        host: \"localhost\";
    }

    vars {
        start-command: \"/start\";
        greetings: [\"Hi\", \"Hello\", \"Hey you\"];
        user-name: none;
    }

    start from a;

    state-points {
        a: {
            act: act-a;
            type: in;
        }

        b: {
            act: act-b;
            type: out;
        }

        c: {
            act: act-c;
            type: out;
        }

        default: {
            act: act-defualt;
            type: out;
        }
    }

    state-actions {
        act-a: {
            if input == start-command {
                say \"Hello\";
                gotov b;
            } else {
                gotov default;
            }

            if input == greetings {
                gotov a;
            } else {
                gotov default;
            }
        }

        act-b: {
            say \"What is your name ?\";
            gotov c;
        }

        act-c: {
            save input to user-name;
            say \"Hello\" input;
            gotov a;
        }

        act-default {
            say \"Sorry, i can't understand you\";
            gotov a;
        }
    }
}"
  (<SCRIPT>
   (<COMPILER-OPTIONS>
    (<COMPILER-OPTION> (<COMPILER-OPTION-NAME> (<KEYWORD> @CODEGEN))
                       (<STRING-OR-NUMBER> (<CHAR-STRING> "\"js\""))))
   (<BOT-DECLARATION>
    (<BOT-OPTIONS>
     (<BOT-OPTION> (<ID> "name") (<STRING-OR-NUMBER> (<CHAR-STRING> "\"Test\"")))
     (<BOT-OPTION> (<ID> "port") (<STRING-OR-NUMBER> (<NUMBER-STRING> 8082)))
     (<BOT-OPTION> (<ID> "host")
                   (<STRING-OR-NUMBER> (<CHAR-STRING> "\"localhost\""))))
    (<VAR-DECLARATIONS>
     (<VAR-DECL> (<ID> "start-command") (<LITERAL> (<CHAR-STRING> "\"/start\"")))
     (<VAR-DECL> (<ID> "greetings")
                 (<LITERAL>
                  (<ITEM-LIST> (<LITERAL> (<CHAR-STRING> "\"Hi\""))
                               (<LITERAL> (<CHAR-STRING> "\"Hello\""))
                               (<LITERAL> (<CHAR-STRING> "\"Hey you\"")))))
     (<VAR-DECL> (<ID> "user-name") (<LITERAL> (<KEYWORD> NONE))))
    (<START-FROM-STMT> (<ID> "a"))
    (<STATE-POINTS-DECLARATIONS>
     (<STATE-POINT-DECL>
      (<ID> "a")
      (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-a"))
                             (<STATE-POINT-OPTION> (<ID> "type") (<ID> "in"))))
     (<STATE-POINT-DECL>
      (<ID> "b")
      (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-b"))
                             (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out"))))
     (<STATE-POINT-DECL>
      (<ID> "c")
      (<STATE-POINT-OPTIONS> (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-c"))
                             (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out"))))
     (<STATE-POINT-DECL>
      (<ID> "default")
      (<STATE-POINT-OPTIONS>
       (<STATE-POINT-OPTION> (<ID> "act") (<ID> "act-defualt"))
       (<STATE-POINT-OPTION> (<ID> "type") (<ID> "out")))))
    (<STATE-ACTIONS-DECLARATIONS>
     (<STATE-DECL> (<ID> "act-a")
                   (<STMT-LIST>
                    (<STMT>
                     (<IF-STMT>
                      (<COND-EXPR>
                       (<LOGIC-EXPR>
                        (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "input"))
                                           (<EQL-EXPR> (<ID> "start-command")))))
                      (<STMT-LIST>
                       (<STMT>
                        (<EXPR>
                         (<SAY-EXPR>
                          (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello\""))))))
                       (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "b")))))
                      (<ELSE-BLOCK>
                       (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "default"))))))))
                    (<STMT>
                     (<IF-STMT>
                      (<COND-EXPR>
                       (<LOGIC-EXPR>
                        (<COMPARISON-EXPR> (<EQL-EXPR> (<ID> "input"))
                                           (<EQL-EXPR> (<ID> "greetings")))))
                      (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a")))))
                      (<ELSE-BLOCK>
                       (<STMT-LIST> (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "default"))))))))))
     (<STATE-DECL> (<ID> "act-b")
                   (<STMT-LIST>
                    (<STMT>
                     (<EXPR>
                      (<SAY-EXPR>
                       (<SAY-EXPR-ARGS>
                        (<SAY-EXPR-ARG> (<CHAR-STRING> "\"What is your name ?\""))))))
                    (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "c"))))))
     (<STATE-DECL> (<ID> "act-c")
                   (<STMT-LIST>
                    (<STMT>
                     (<EXPR>
                      (<SAVE-TO-EXPR> (<LITERAL-OR-ID> (<ID> "input")) (<ID> "user-name"))))
                    (<STMT>
                     (<EXPR>
                      (<SAY-EXPR>
                       (<SAY-EXPR-ARGS> (<SAY-EXPR-ARG> (<CHAR-STRING> "\"Hello\""))
                                        (<SAY-EXPR-ARG> (<ID> "input"))))))
                    (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a"))))))
     (<STATE-DECL> (<ID> "act-default")
                   (<STMT-LIST>
                    (<STMT>
                     (<EXPR>
                      (<SAY-EXPR>
                       (<SAY-EXPR-ARGS>
                        (<SAY-EXPR-ARG>
                         (<CHAR-STRING> "\"Sorry, i can't understand you\""))))))
                    (<STMT> (<EXPR> (<GOTOV-EXPR> (<ID> "a")))))))))
  :script)
