;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/construction
    (:use :cl)
  (:export #:rule->))

(in-package :nobot/botscript/parser/acacia/construction)

(defgeneric rule-> (rule-name)
  (:method (rule-name)
    (error "unknown rule: [~a]" rule-name)))
