;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/codegen/js/project-generator
    (:use :cl))

(in-package :nobot/botscript/codegen/js/project-generator)

;;NPM
(defclass npm-project-configuration ()
  ())
