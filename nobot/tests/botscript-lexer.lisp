;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests/botscript-lexer
    (:use :cl
          :lisp-unit
          :nobot/botscript/lexer
          :nobot/botscript/lexer/token))

(in-package :nobot/tests/botscript-lexer)

(defmacro define-lexer-test (name
                             &rest input-strings
                             &key
                               (expected-tokens-seq
                                (error "not passed expected tokens seq"))
                               with-pos)
  `(define-test ,name
     (assert-true (same-tokens-seq-?
                   (disassemble-string (format nil "~{~%~a~}" ,input-strings)
                                       :convert-with-pos ,with-pos)
                   ',expected-tokens-seq
                   :without-pos ,(not with-pos)))))


(define-lexer-test bs-lexer.1
    )

(define-lexer-test bs-lexer.2
    )

(define-lexer-test bs-lexer.3
    )
