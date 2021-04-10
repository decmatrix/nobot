;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen/top
    (:use :cl)
  (:import-from :nobot/projectgen/top
                #:get-project-path
                #:get-project-lang)
  (:import-from :nobot/botscript/types
                #:type->keyword)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-projectgen-info
                #:get-post-processing-result
                #:get-projectgen-result)
  (:export #:*post-process-result*
           #:generate-code
           #:generate-output-code
           #:codegen-info
           #:translate))

(in-package :nobot/codegen/top)

(defvar *post-process-result*)

(defclass codegen-info () ())

(defgeneric generate-output-code (project-path lang))

(defun generate-code ()
  (let ((*post-process-result*
         (get-post-processing-result *context*))
        (projectgen-result
         (get-projectgen-result *context*)))
    (generate-output-code
     (get-project-type projectgen-result)
     (get-project-lang projectgen-result))
    (make-codegen-info)))

(defun make-codegen-info ()
  (make-instance 'codegen-info))


(defgeneric translate (lang sort tree))

(defmethod translate :before (lang sort tree)
  (call-next-method lang (type->keyword sort) tree))
