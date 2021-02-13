;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests
    (:use :cl
          :lisp-unit
          :nobot/tests/botscript-lexer
          :nobot/tests/botscript-parser)
  (:nicknames :nobot-tests)
  (:export #:run-unit-tests))

(in-package :nobot/tests)

(defun run-unit-tests ()
  (mapc
   (lambda (package)
     (run-tests :all package))
   '(:nobot/tests/botscript-lexer
     :nobot/tests/botscript-parser)))
