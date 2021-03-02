;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/types
    (:use :cl
          :nobot/botscript/types/lexer-types
          :nobot/botscript/types/parser-types
          :nobot/botscript/types/types-utils)
  (:export #:get-from-type
           #:get-type-value-list
           #:convert-type))
