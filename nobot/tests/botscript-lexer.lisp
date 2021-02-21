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
    "letv a [act = act-a, type = in]"
  ((<KEYWORD> LETV (1 . 1)) (<ID> "a" (6 . 1)) (<DELIMITER> O-SQ-BRACKET (8 . 1))
   (<KEYWORD> ACT (9 . 1)) (<DELIMITER> ASSIGN (13 . 1)) (<ID> "act-a" (15 . 1))
   (<DELIMITER> COMMA (20 . 1)) (<KEYWORD> TYPE (22 . 1))
   (<DELIMITER> ASSIGN (27 . 1)) (<KEYWORD> IN (29 . 1))
   (<DELIMITER> C-SQ-BRACKET (31 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.3
    "def-act act-default {}"
  ((<KEYWORD> DEF-ACT (1 . 1)) (<ID> "act-default" (9 . 1))
   (<DELIMITER> O-FIG-BRACKET (21 . 1)) (<DELIMITER> C-FIG-BRACKET (22 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.4
    "c-opts { lang: \"js\" }"
  ((<KEYWORD> C-OPTS (1 . 1)) (<DELIMITER> O-FIG-BRACKET (8 . 1))
   (<ID> "lang" (10 . 1)) (<DELIMITER> COLON (14 . 1))
   (<CHAR-STRING> "\"JS\"" (16 . 1)) (<DELIMITER> C-FIG-BRACKET (22 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.5
  "c-opts {
lang: \"js\",
arch: \"chat\",
project-dir: \"~/dev-bots/mybot/\",
extension: \"tg\"
}"
  ((<KEYWORD> C-OPTS (1 . 1)) (<DELIMITER> O-FIG-BRACKET (8 . 1))
   (<DELIMITER> NEWLINE (8 . 2)) (<ID> "lang" (9 . 2))
   (<DELIMITER> COLON (13 . 2)) (<CHAR-STRING> "\"JS\"" (15 . 2))
   (<DELIMITER> COMMA (20 . 2)) (<DELIMITER> NEWLINE (20 . 3))
   (<ID> "arch" (21 . 3)) (<DELIMITER> COLON (25 . 3))
   (<CHAR-STRING> "\"CHAT\"" (27 . 3)) (<DELIMITER> COMMA (34 . 3))
   (<DELIMITER> NEWLINE (34 . 4)) (<ID> "project-dir" (35 . 4))
   (<DELIMITER> COLON (46 . 4)) (<CHAR-STRING> "\"~/DEV-BOTS/MYBOT/\"" (48 . 4))
   (<DELIMITER> COMMA (68 . 4)) (<DELIMITER> NEWLINE (68 . 5))
   (<ID> "extension" (69 . 5)) (<DELIMITER> COLON (78 . 5))
   (<CHAR-STRING> "\"TG\"" (80 . 5)) (<DELIMITER> NEWLINE (84 . 6))
   (<DELIMITER> C-FIG-BRACKET (85 . 6)))
  :with-pos t)

(define-lexer-test bs-lexer.simple.6
    "act = in"
  ((<KEYWORD> ACT (1 . 1)) (<DELIMITER> ASSIGN (5 . 1)) (<KEYWORD> IN (7 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.keywords.1
    "c-opts bot-opts start from letd letv def-act act type in out"
  ((<KEYWORD> C-OPTS (1 . 1)) (<KEYWORD> BOT-OPTS (8 . 1))
   (<KEYWORD> START (17 . 1)) (<KEYWORD> FROM (23 . 1)) (<KEYWORD> LETD (28 . 1))
   (<KEYWORD> LETV (33 . 1)) (<KEYWORD> DEF-ACT (38 . 1))
   (<KEYWORD> ACT (46 . 1)) (<KEYWORD> TYPE (50 . 1)) (<KEYWORD> IN (55 . 1))
   (<KEYWORD> OUT (58 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.delimiters.1
    "{}[],   =:"
  ((<DELIMITER> O-FIG-BRACKET (1 . 1)) (<DELIMITER> C-FIG-BRACKET (2 . 1))
   (<DELIMITER> O-SQ-BRACKET (3 . 1)) (<DELIMITER> C-SQ-BRACKET (4 . 1))
   (<DELIMITER> COMMA (5 . 1)) (<DELIMITER> ASSIGN (9 . 1))
   (<DELIMITER> COLON (10 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.string.1
    "\"js\" \"\"\"\""
  ((<CHAR-STRING> "\"JS\"" (1 . 1)) (<CHAR-STRING> "\"\"" (7 . 1))
   (<CHAR-STRING> "\"\"" (10 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.string.2
    "\" hello
world\""
  ((<CHAR-STRING> "\" HELLO
WORLD\""
                  (1 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.ids.1
    "def act def-act def:act act:1 a = b"
  ((<ID> "def" (1 . 1)) (<KEYWORD> ACT (5 . 1)) (<KEYWORD> DEF-ACT (9 . 1))
   (<ID> "def" (17 . 1)) (<DELIMITER> COLON (20 . 1)) (<KEYWORD> ACT (21 . 1))
   (<KEYWORD> ACT (25 . 1)) (<DELIMITER> COLON (28 . 1))
   (<NUMBER-STRING> 1 (29 . 1)) (<ID> "a" (31 . 1)) (<DELIMITER> ASSIGN (33 . 1))
   (<ID> "b" (35 . 1)))
  :with-pos t)

(define-lexer-test bs-lexer.single-comment.1
    "a b c // * b &*^ ade-act"
  ((<ID> "a" (1 . 1)) (<ID> "b" (3 . 1)) (<ID> "c" (5 . 1)))
  :with-pos t)

;;TODO: see issue #11
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
