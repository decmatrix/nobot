(uiop:define-package :nobot/botscript/types
    (:use :cl
          :anaphora)
  (:import-from :alexandria
                #:hash-table-values)
  (:import-from :nobot/utils
                #:set-<>
                #:define-constant-?
                #:reintern)
  (:export #:+token-types+
           #:+sort-types+
           #:get-sort-type-symbol
           #:get-token-type-symbol
           #:convert-type
           #:get-token-type-list
           #:get-sort-type-list
           #:use-sort-type-class
           #:use-token-type-class))

(in-package :nobot/botscript/types)

(defvar *sort-types*)
(defvar *token-types*)

(defun convert-type (type &key only-to-symbol)
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

(defun get-sort-type-symbol (type)
  (let ((converted-sort-type (convert-type type)))
    (aif (gethash converted-sort-type *sort-types*)
      it
      (error "Unknown sort type ~A" (symbol-name converted-sort-type)))))

(defun get-token-type-symbol (type)
  (let ((converted-token-type (convert-type type)))
    (aif (gethash converted-token-type *token-types*)
      it
      (error "Unknown token type ~A" (symbol-name converted-token-type)))))

(defun get-sort-type-list ()
  (hash-table-values *sort-types*))

(defun get-token-type-list ()
  (hash-table-values *token-types*))

(define-constant-? +botscript-token-types+
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

(defun get-token-table-by-token-type-class (name)
  (case name
    (:botscript-token-types
     +botscript-token-types+)
    (t (error "Unknown token type class: ~A" name))))

(defun use-sort-type-class (sort-type-class)
  (setf *sort-types* (get-sort-table-by-sort-type-class sort-type-class)))

(defun use-token-type-class (token-type-class)
  (setf *token-types* (get-token-table-by-token-type-class token-type-class)))
