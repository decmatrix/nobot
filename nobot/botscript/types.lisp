;;;; Copyright (c) 2021 NOBOT
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/types
    (:use :cl
          :nobot/botscript/types/types-utils)
  (:export #:get-type
           #:get-type-value-list))
