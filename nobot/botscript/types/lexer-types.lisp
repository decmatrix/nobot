;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/types/lexer-types
    (:use :cl
          :nobot/botscript/types/types-utils)
  (:import-from :alexandria
                #:hash-table-values)
  (:import-from :anaphora
                #:aif
                #:it))

(in-package :nobot/botscript/types/lexer-types)

(defparameter *botscript-token-types*
  (define-types
    (:value "keyword" :description "keyword")
    (:value "number-string" :description "number")
    (:value "id" :description "id")
    (:value "char-string" :description "string")
    (:value "delimiter" :description "delimiter")))

(defmethod get-from-type (type (class-type (eql :token)) what-need)
  (let ((converted-token-type (convert-type type)))
    (aif (gethash converted-token-type *botscript-token-types*)
      (case what-need
        (:value ($get-value it))
        (:description ($get-description it))
        (t
         (error "unexpected value of what-need arg: ~a, expected [:value or :description]"
                what-need)))
      (error "unknown token type: ~a"
             (symbol-name converted-token-type)))))

(defmethod get-type-value-list ((class-type (eql :token)))
  (mapcar #'$get-value (hash-table-values *botscript-token-types*)))
