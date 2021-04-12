;;;; Copyright (c) 2021 Bohdan Sokolovskyi
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
    (:value "compiler-option" :description "compiler option")
    (:value "compiler-option-name" :description "compiler option name")
    (:value "bot-declaration" :description "bot declaration")
    (:value "bot-options" :description "bot options")
    (:value "bot-options-list" :description "bot options list")
    (:value "bot-option" :description "bot option")
    (:value "var-declarations" :description "var declarations")
    (:value "var-decls-list" :description "var declarations list")
    (:value "var-decl" :description "var declaration")
    (:value "start-from-stmt" :description "start from statement")
    (:value "state-points-declarations" :description "state points declarations")
    (:value "state-points-decls" :description "state points declaration list")
    (:value "state-point-decl" :description "state point declaration")
    (:value "state-point-options" :description "state point options")
    (:value "state-point-option" :description "state point option")
    (:value "state-actions-declarations" :description "state actions declarations")
    (:value "state-actions-decls" :description "state actions declaration list")
    (:value "state-decl" :description "state declaration")
    (:value "stmt-list" :description "statement list")
    (:value "stmt" :description "statement")
    (:value "expr" :description "expression")
    (:value "gotov-expr" :description "go to vertex expression")
    (:value "say-expr" :description "say expression")
    (:value "say-expr-args" :description "arguments of say expression")
    (:value "say-expr-arg" :description "argument of say expression")
    (:value "rest-say-expr-args" :description "rest of arguments of say expression")
    (:value "save-to-expr" :description "expression save to")
    (:value "if-stmt" :description "if statement")
    (:value "else-block" :description "else block")
    (:value "cond-expr" :description "condition expression")
    (:value "logic-expr" :description "logical expression")
    (:value "equal-expr" :description "equal expression")
    (:value "eq-sub-expr" :description "equal sub expression")
    (:value "literal-or-id-or-input" :description "literal or id or ?input")
    (:value "string-or-number" :description "string or number")
    (:value "literal" :description "literal")
    (:value "item-list" :description "list of items")
    (:value "literal-list" :description "list of literals")
    (:value "rest-literal-list" :description "rest of list of literals")
    (:value "in-expr" :description "in expression")
    (:value "left-in-expr" :description "left part of in-expression")
    (:value "right-in-expr" :description "right part of in-expression")
    (:value "gotov-arg" :description "gotov arg: id or keyword self")
    ))

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
