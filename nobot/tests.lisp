(uiop:define-package :nobot/tests
    (:use :cl :lisp-unit
          :nobot/tests/botscript-lexer)
  (:export :test-suite))

(in-package :nobot/tests)

(defun test-suite ()
  (run-tests))
