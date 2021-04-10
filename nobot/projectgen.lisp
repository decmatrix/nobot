;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen
    (:use :cl
          :nobot/projectgen/common
          :nobot/projectgen/npm)
  (:reexport :nobot/projectgen/common))
