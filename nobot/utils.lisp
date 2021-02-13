;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/utils
    (:use :cl
          :nobot/utils/program-utils
          :nobot/utils/common-utils)
  (:export
   ;; program utils
   #:when-option
   #:it-opt
   #:is-valid-file-format-?
   #:get-progam-version
   ;; common utils
   #:equals
   #:split-list
   #:reintern
   #:let-when
   #:to-symbol))
