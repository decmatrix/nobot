(uiop:define-package :nobot/tests/parser
    (:use :cl
          :lisp-unit
          :nobot/botscript/tree-utils
          :nobot/botscript/parser))

(in-package :nobot/tests/parser)

(defmacro define-parser-test (name input-string expected-tree)
  `(define-test ,name
     (assert-true (equalp))))
