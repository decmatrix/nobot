;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/tree-tools
    (:use :cl)
  (:import-from :anaphora
                #:awhen
                #:it)
  (:import-from :nobot/utils
                #:equals
                #:reintern)
  (:import-from :nobot/botscript/parser/acacia/result-packaging
                #:acacia-packed-result
                #:acacia-get-parse-tree)
  (:export #:same-parse-tree-?
           #:get-sub-tree))

(in-package :nobot/botscript/parser/acacia/tree-tools)

(defvar *sort-type*)
(defvar *result*)

(defgeneric same-parse-tree-? (obj1 obj2))
(defgeneric normolize-tree (tree))
(defgeneric get-sub-tree-aux (tree)
  (:method (tree) tree))

(defmethod normolize-tree ((tree list))
  (mapcar (lambda (elm)
            (cond
              ((symbolp elm)
               (reintern elm :cl-user))
              ((listp elm)
               (normolize-tree elm))
              (t elm)))
          tree))

(defmethod same-parse-tree-? ((obj1 acacia-packed-result)
                              (obj2 acacia-packed-result))
  (same-parse-tree-? (acacia-get-parse-tree obj1)
                     (acacia-get-parse-tree obj2)))

(defmethod same-parse-tree-? ((obj1 list) (obj2 list))
  (equals (normolize-tree obj1)
          (normolize-tree obj2)))

(defun get-sub-tree (tree sort-type &key (convert-sort-type-fn #'identity))
  (let ((*sort-type* sort-type)
        (*result* nil))
    (get-sub-tree-aux tree)
    *result*))

;;TODO: try get rid of push fn
(defmethod get-sub-tree-aux ((tree list))
  (when (eq (car tree) *sort-type*)
    (push tree *result*))
  (awhen (cdr tree)
    (mapc #'get-sub-tree-aux it)))

