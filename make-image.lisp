;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


;; load nobot system
(load "nobot/nobot.asd")
(asdf:load-system :nobot)

;; make image
(sb-ext:save-lisp-and-die 
 (make-pathname 
  :directory '(:relative "release" "bin")
  :name "nobot-pt"
  :type "bin")
 :toplevel #'nobot/startup:*run*
 :save-runtime-options t
 :executable t)

;; quit from sbcl
(quit)
