;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests/botscript-parser
    (:use :cl
          :lisp-unit
          :nobot/botscript/parser))

(in-package :nobot/tests/botscript-parser)

(defmacro define-parser-test (name input-string expected-tree)
  `(define-test ,name
     (assert-true
      (same-parse-tree))))
