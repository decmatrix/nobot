;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


;; load nobot system
(load "nobot/nobot.asd")
(asdf:load-system :nobot)

;; application parameters
(defparameter *nobot-application-name* "nobot")
(defparameter *nobot-version* "0.1")
(defparameter *nobot-spec-name* "mega-trash")

;; make image
(sb-ext:save-lisp-and-die 
 (make-pathname 
  :directory '(:relative "release")
  :name (format nil "~a-~a-~a"
                *nobot-application-name*
                *nobot-version*
                *nobot-spec-name*)
  :type "bin")
 :toplevel #'nobot/startup:*run*
 :save-runtime-options t
 :executable t)

;; quit from sbcl
(quit)
