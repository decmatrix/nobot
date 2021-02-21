;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/post
    (:use :cl)
  (:export #:bot-project-info
           #:botscript-post-processing
           #:botscript-post-process-info))

(in-package :nobot/botscript/post)

(defclass botscript-post-process-info () ())

(defun botscript-post-processing ()
  )
