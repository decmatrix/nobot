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
           #:get-sub-tree
           #:get-custom-sub-tre-getter))

(in-package :nobot/botscript/parser/acacia/tree-tools)

(defvar *sort-type*)
(defvar *is-all*)
(defvar *result*)

(defgeneric same-parse-tree-? (obj1 obj2))
(defgeneric normolize-tree (tree))
(defgeneric get-all-sub-trees (tree)
  (:method (tree) tree))
(defgeneric get-first-sub-tree (tree)
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

(defun get-custom-sub-tree-getter (convert-sort-type-fn)
  (lambda (tree sort-type &key all)
    (get-sub-tree tree sort-type
                  :all all
                  :convert-sort-type-fn convert-sort-type-fn)))

(defun get-sub-tree (tree sort-type &key all (convert-sort-type-fn #'identity))
  (let ((*sort-type* (funcall convert-sort-type-fn sort-type))
        (*is-all* all)
        (*result* nil))
    (if *is-all*
        (get-all-sub-trees tree)
        (get-first-sub-tree tree))
    *result*))

;;TODO: try get rid of push
(defmethod get-all-sub-trees ((tree list))
  (when (eq (car tree) *sort-type*)
    (push tree *result*))
  (awhen (cdr tree)
    (mapc #'get-all-sub-trees it)))

;;TODO: try get rid of setf
(defmethod get-first-sub-tree ((tree list))
  (if (eq (car tree) *sort-type*)
      (setf *result* tree)
      (awhen (cdr tree)
        (mapc #'get-first-sub-tree it))))

