;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/types/parser-types
    (:use :cl
          :nobot/botscript/types/types-utils)
  (:import-from :alexandria
                #:hash-table-values)
  (:import-from :anaphora
                #:aif
                #:it))

(in-package :nobot/botscript/types/parser-types)

(defparameter *botscript-sort-types*
  (define-types
    (:value "script" :description "script")
    (:value "compiler-options" :description "compiler options")
    (:value "bot-options" :description "bot options")
    (:value "data-decl-list" :description "data declarations")
    (:value "vertex-decl-list" :description "vertex declarations")
    (:value "action-decl-list" :description "action declarations")
    (:value "start-stmt" :description "start statement")
    (:value "c-opts-block" :description "compiler's block options")
    (:value "bot-opts-block" :description "bot's block options")
    (:value "data-decl" :description "data declaration")
    (:value "vertex-decl" :description "vertex declaration")
    (:value "action-decl" :description "action declaration")
    (:value "stmt-block" :description "statement block")
    (:value "opts-list" :description "option list")
    (:value "opt" :description "option")
    (:value "opts-tail-list" :description "tail of option list")
    (:value "string-or-num" :description "string or number")
    (:value "data-expr" :description "data expression")
    (:value "vertex-options" :description "vertex options")
    (:value "stmt-list" :description "statement list")
    (:value "stmt" :description "statement")
    (:value "data-seq" :description "data sequence")
    (:value "vertex-option-list" :description "vertex option list")
    (:value "expr" :description "expression")
    (:value "item-list" :description "item list")
    (:value "item-tail-list" :description "tail of item list")
    (:value "item" :description "item")
    (:value "vertex-option" :description "vertex option")
    (:value "vertex-option-name" :description "name of vertex option")
    (:value "vertex-option-val" :description "value of vertex option")
    (:value "literal" :description "literal")))

(defmethod get-from-type (type (class-type (eql :sort)) what-need)
  (let ((converted-sort-type (convert-type type)))
    (aif (gethash converted-sort-type *botscript-sort-types*)
      (case what-need
        (:value ($get-value it))
        (:description ($get-description it))
        (t
         (error "unexpected value of what-need arg: ~a, expected [:value or :description]"
                what-need)))
      (error "unknown sort type: ~a"
             (symbol-name converted-sort-type)))))

(defmethod get-type-value-list ((class-type (eql :sort)))
  (mapcar #'$get-value (hash-table-values *botscript-sort-types*)))
