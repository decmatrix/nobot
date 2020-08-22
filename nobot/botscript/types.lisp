(uiop:define-package :nobot/botscript/types
    (:use :cl
          :anaphora)
  (:import-from :nobot/utils
                #:set-<>
                #:define-constant-?
                #:reintern)
  (:export #:+token-types+
           #:+sort-types+
           #:get-sort-symbol
           #:convert-sort-type
           #:use-sort-type-class))

(in-package :nobot/botscript/types)

(defvar *sort-types*)

(defun convert-sort-type (type &key only-to-symbol)
  (let ((package (if only-to-symbol
                     :cl-user
                     :keyword)))
    (cond
      ((stringp type) (intern type package))
      ((keywordp type) type)
      ((symbolp type) (reintern type package))
      (t (error "Representation of sort type maybe only is string, keyword or symbol")))))

(defun define-types (&rest types-list)
  (let ((table (make-hash-table :test #'eq)))
    (mapcar
     (lambda (type)
       (let ((up-string (string-upcase type)))
         (setf (gethash (intern up-string :keyword) table)
               (intern (set-<> up-string) :cl-user))))
     types-list)
    table))

(defun get-sort-symbol (type)
  (let ((converted-sort-type (convert-sort-type type)))
    (aif (gethash converted-sort-type *sort-types*)
      it
      (error "Unknown sort type ~A" (symbol-name converted-sort-type)))))

(define-constant-? +token-types+
    (define-types
      "keyword"
      "unknown"
      "number-string"
      "id"))

(define-constant-? +botscript-sort-types+
    (define-types
      "script"
      "macros-block"
      "predefined-block"
      "definition/combo-block"
      "graph-logic"
      "exe-macros"
      "use-predefined"
      "call-definition/combo"
      "call-definition"
      "call-combo"
      "predefines"
      "predefined-list"
      "args-list"
      "arg"
      "id"))

(defun get-sort-table-by-sort-type-class (name)
  (case name
    (:botscript-sort-types
     +botscript-sort-types+)
    (t (error "Unknown sort type class: ~A" name))))

(defun use-sort-type-class (sort-type-class)
  (setf *sort-types* (get-sort-table-by-sort-type-class sort-type-class)))
