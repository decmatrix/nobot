;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests/botscript-lexer
    (:use :cl
          :lisp-unit
          :nobot/botscript/lexer
          :nobot/botscript/lexer/token))

(in-package :nobot/tests/botscript-lexer)

(defmacro define-lexer-test (name input-string expected-tokens-seq &key with-pos)
  `(define-test ,name
     (assert-true
         (same-tokens-seq-?
          (disassemble-string ,input-string
                              :convert-with-pos ,with-pos)
          ',expected-tokens-seq
          :without-pos ,(not with-pos)))))


(define-lexer-test bs-lexer.simple.1
    "lang:1, op2: \"test\" // 12 3 4 * & ^"
  ((<ID> "lang" (1 . 1)) (<DELIMITER> COLON (5 . 1)) (<NUMBER-STRING> 1 (6 . 1))
   (<DELIMITER> COMMA (7 . 1)) (<ID> "op2" (9 . 1)) (<DELIMITER> COLON (12 . 1))
   (<CHAR-STRING> "\"TEST\"" (14 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.2
    "@codegen \"js\"; bot { options { name: \"Tobi\"; } }"
  ((<KEYWORD> @CODEGEN (1 . 1)) (<CHAR-STRING> "\"js\"" (10 . 1))
   (<DELIMITER> SEMICOLON (15 . 1)) (<KEYWORD> BOT (17 . 1))
   (<DELIMITER> O-FIG-BRACKET (21 . 1)) (<KEYWORD> OPTIONS (23 . 1))
   (<DELIMITER> O-FIG-BRACKET (31 . 1)) (<ID> "name" (33 . 1))
   (<DELIMITER> COLON (37 . 1)) (<CHAR-STRING> "\"Tobi\"" (39 . 1))
   (<DELIMITER> SEMICOLON (46 . 1)) (<DELIMITER> C-FIG-BRACKET (48 . 1))
   (<DELIMITER> C-FIG-BRACKET (50 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.3
    "if input == start-command { say \"Hello\"; } else { save input to buff; }"
  ((<KEYWORD> IF (1 . 1)) (<ID> "input" (4 . 1))
   (<DELIMITER> LOGIC-EQUAL (10 . 1)) (<ID> "start-command" (13 . 1))
   (<DELIMITER> O-FIG-BRACKET (27 . 1)) (<KEYWORD> SAY (29 . 1))
   (<CHAR-STRING> "\"Hello\"" (33 . 1)) (<DELIMITER> SEMICOLON (41 . 1))
   (<DELIMITER> C-FIG-BRACKET (43 . 1)) (<KEYWORD> ELSE (45 . 1))
   (<DELIMITER> O-FIG-BRACKET (50 . 1)) (<KEYWORD> SAVE (52 . 1))
   (<ID> "input" (57 . 1)) (<KEYWORD> TO (63 . 1)) (<ID> "buff" (66 . 1))
   (<DELIMITER> SEMICOLON (70 . 1)) (<DELIMITER> C-FIG-BRACKET (72 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.4
    "gotov a; a: {}"
  ((<KEYWORD> GOTOV (1 . 1)) (<ID> "a" (7 . 1)) (<DELIMITER> SEMICOLON (8 . 1))
   (<ID> "a" (10 . 1)) (<DELIMITER> COLON (11 . 1))
   (<DELIMITER> O-FIG-BRACKET (13 . 1)) (<DELIMITER> C-FIG-BRACKET (14 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.5
    "options {
name: \"Tobi\";
port: 8082;
host: \"localhost\";
author: \"Bohdan Sokolovskyi\";
version: \"0.0.1\";
}"
  ((<KEYWORD> OPTIONS (1 . 1)) (<DELIMITER> O-FIG-BRACKET (9 . 1))
   (<ID> "name" (10 . 2)) (<DELIMITER> COLON (14 . 2))
   (<CHAR-STRING> "\"Tobi\"" (16 . 2)) (<DELIMITER> SEMICOLON (23 . 2))
   (<ID> "port" (24 . 3)) (<DELIMITER> COLON (28 . 3))
   (<NUMBER-STRING> 8082 (30 . 3)) (<DELIMITER> SEMICOLON (34 . 3))
   (<ID> "host" (35 . 4)) (<DELIMITER> COLON (39 . 4))
   (<CHAR-STRING> "\"localhost\"" (41 . 4)) (<DELIMITER> SEMICOLON (53 . 4))
   (<ID> "author" (54 . 5)) (<DELIMITER> COLON (60 . 5))
   (<CHAR-STRING> "\"Bohdan Sokolovskyi\"" (62 . 5))
   (<DELIMITER> SEMICOLON (83 . 5)) (<ID> "version" (84 . 6))
   (<DELIMITER> COLON (91 . 6)) (<CHAR-STRING> "\"0.0.1\"" (93 . 6))
   (<DELIMITER> SEMICOLON (101 . 6)) (<DELIMITER> C-FIG-BRACKET (102 . 7)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.6
    "[1, 2, 3, 4]"
  ((<DELIMITER> O-SQ-BRACKET (1 . 1)) (<NUMBER-STRING> 1 (2 . 1))
   (<DELIMITER> COMMA (3 . 1)) (<NUMBER-STRING> 2 (5 . 1))
   (<DELIMITER> COMMA (6 . 1)) (<NUMBER-STRING> 3 (8 . 1))
   (<DELIMITER> COMMA (9 . 1)) (<NUMBER-STRING> 4 (11 . 1))
   (<DELIMITER> C-SQ-BRACKET (12 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.keywords.1
    "bot @codegen vars start from options state-points state-actions
     gotov say save to if else none"
  ((<KEYWORD> BOT (1 . 1)) (<KEYWORD> @CODEGEN (5 . 1)) (<KEYWORD> VARS (14 . 1))
   (<KEYWORD> START (19 . 1)) (<KEYWORD> FROM (25 . 1))
   (<KEYWORD> OPTIONS (30 . 1)) (<KEYWORD> STATE-POINTS (38 . 1))
   (<KEYWORD> STATE-ACTIONS (51 . 1)) (<KEYWORD> GOTOV (69 . 2))
   (<KEYWORD> SAY (75 . 2)) (<KEYWORD> SAVE (79 . 2)) (<KEYWORD> TO (84 . 2))
   (<KEYWORD> IF (87 . 2)) (<KEYWORD> ELSE (90 . 2)) (<KEYWORD> NONE (95 . 2)))
  :with-pos t)

(define-lexer-test bs-lexer.delimiters.1
    "{}[], :;=="
  ((<DELIMITER> O-FIG-BRACKET (1 . 1)) (<DELIMITER> C-FIG-BRACKET (2 . 1))
   (<DELIMITER> O-SQ-BRACKET (3 . 1)) (<DELIMITER> C-SQ-BRACKET (4 . 1))
   (<DELIMITER> COMMA (5 . 1)) (<DELIMITER> COLON (7 . 1))
   (<DELIMITER> SEMICOLON (8 . 1)) (<DELIMITER> LOGIC-EQUAL (9 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.string.1
    "\"js\" \"\"\"\""
  ((<CHAR-STRING> "\"js\"" (1 . 1)) (<CHAR-STRING> "\"\"" (7 . 1))
   (<CHAR-STRING> "\"\"" (10 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.string.2
    "\" hello
world\""
  ((<CHAR-STRING> "\" hello
world\""
                  (1 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.ids.1
    "state actions state-actions  state:actions state:1 a == b"
  ((<ID> "state" (1 . 1)) (<ID> "actions" (7 . 1))
   (<KEYWORD> STATE-ACTIONS (15 . 1)) (<ID> "state" (30 . 1))
   (<DELIMITER> COLON (35 . 1)) (<ID> "actions" (36 . 1)) (<ID> "state" (44 . 1))
   (<DELIMITER> COLON (49 . 1)) (<NUMBER-STRING> 1 (50 . 1)) (<ID> "a" (52 . 1))
   (<DELIMITER> LOGIC-EQUAL (54 . 1)) (<ID> "b" (57 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.single-comment.1
    "a b c // * b &*^ ade-act"
  ((<ID> "a" (1 . 1)) (<ID> "b" (3 . 1)) (<ID> "c" (5 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.single-comment.2
    "a b c // smth test /* */
d e f g h"
  ((<ID> "a" (1 . 1)) (<ID> "b" (3 . 1)) (<ID> "c" (5 . 1)) (<ID> "d" (25 . 2))
   (<ID> "e" (27 . 2)) (<ID> "f" (29 . 2)) (<ID> "g" (31 . 2))
   (<ID> "h" (33 . 2)))
  :with-pos t)

(define-lexer-test bs-lexer.multi-comment.1
    "a b c /* d e */ f g h"
  ((<ID> "a" (1 . 1)) (<ID> "b" (3 . 1)) (<ID> "c" (5 . 1)) (<ID> "f" (17 . 1))
   (<ID> "g" (19 . 1)) (<ID> "h" (21 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.multi-comment.2
    "a b c /* d
e */ f g h"
  ((<ID> "a" (1 . 1)) (<ID> "b" (3 . 1)) (<ID> "c" (5 . 1)) (<ID> "f" (16 . 2))
   (<ID> "g" (18 . 2)) (<ID> "h" (20 . 2)))
  :with-pos t)

(define-lexer-test bs-lexer.multi-comment.3
    "a /********/ /* * * * / /* */ b"
  ((<ID> "a" (1 . 1)) (<ID> "b" (31 . 1)))
  :with-pos t)
