;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/post
    (:use :cl)
  (:export #:bot-project-info
           #:botscript-post-processing))

(in-package :nobot/botscript/post)

(defclass bot-project-info () ())

(defun botscript-post-processing ()
  )
