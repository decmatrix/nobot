;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser
    (:nicknames :nobot-bs-parser)
  (:use :cl
        :nobot/botscript/parser/parser-impl)
  (:export #:parse-source
           #:parse-string
           #:parse-file))
