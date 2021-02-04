;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen
    (:use :cl
          :nobot/codegen/top)
  (:reexport :nobot/codegen/top))
