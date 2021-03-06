;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen/top
    (:use :cl)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-projectgen-info
                #:get-post-processing-result)
  (:export #:generate-code
           #:generate-output-code
           #:codegen-info))

(in-package :nobot/codegen/top)

(defclass codegen-info () ())

(defgeneric generate-output-code (tree project-path lang))

(defun generate-code ()
  (generate-output-code)
  (make-codegen-info))

(defun make-codegen-info ()
  (make-instance 'codegen-info))
