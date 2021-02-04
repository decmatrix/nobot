;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/toplevel
    (:use :cl
          :nobot/toplevel/translator
          :nobot/toplevel/context)
  (:export
   ;; toplevel forms
   #:*run-and-burn*
   #:*run-and-burn-in-runtime*
   #:*run-and-burn-as-server*
   ;; from context
   #:add-info-log
   #:add-warn-log
   #:add-error-log
   #:is-empty-info-stack-?
   #:is-empty-warn-stack-?
   #:is-empty-error-stack-?))
