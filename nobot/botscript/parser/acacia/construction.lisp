;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/construction
    (:use :cl)
  (:export #:rule->))

(in-package :nobot/botscript/parser/acacia/construction)

(defgeneric rule-> (rule-name &key first-fail-no-error)
  (:method (rule-name &key first-fail-no-error)
    (error "unknown rule: [~a]" rule-name)))
