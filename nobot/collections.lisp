;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/collections
    (:use :cl
          :nobot/collections/lazy-calculus)
  (:nicknames :nobot-collections)
  (:export
   ;; lazy calculus
   #:calc-lazy-node
   #:lazy!
   ))
