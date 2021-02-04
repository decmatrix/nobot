;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/common
    (:use :cl)
  (:export #:project))

(in-package :nobot/projectgen/common)

(defclass project ())
