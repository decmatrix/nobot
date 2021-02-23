;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/post
    (:use :cl)
  (:import-from :nobot/utils/common-utils
                #:to-keyword)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-post-process-error)
  (:import-from :nobot/botscript/parser/acacia/tree-tools
                #:get-sub-tree)
  (:export #:bot-project-info
           #:botscript-post-processing
           #:botscript-post-process-info))

(in-package :nobot/botscript/post)

(defclass botscript-post-process-info ()
  ((bot-options
    :initarg :bot-options
    :type hash-table
    :reader get-bot-options)
   (compiler-options
    :initarg :compiler-options
    :type hash-table
    :reader get-compiler-options)
   (parse-tree
    :initarg :parse-tree
    :type list
    :reader get-parse-tree)))

(defun botscript-post-processing ()
  (make-instance
   'botscript-post-process-info
   :bot-options (make-bot-options-table
                 (get-sub-tree tree :bot-options))
   :compiler-options (make-compiler-options-table
                      (get-sub-tree tree :compiler-options))
   :parse-tree (get-sub-tree tree :script-rest)))

(defun make-bot-options-table (bot-options-tree)
  (let ((table (make-hash-table :test #'eq)))
    (mapc
     (lambda (option)
       (let ((id (to-keyword (second (second option))))
             (value (second (fourth option))))
         (if (is-avaliable-bot-option-? id)
             (setf (gethash id table)
                   (check-option-value value))
             ())))
     (get-sub-tree bot-options-tree :opt :all))
    table))

(defun make-compiler-options-table (compiler-options-tree)
  (let ((table (make-hash-table :test #'eq)))
    (mapc
     (lambda (option)
       (let ((id (to-keyword (second (second option))))
             (value (second (fourth option))))
         (if (is-avaliable-compiler-option-? id)
             (setf (gethash id table)
                   (check-option-value value))
             ())))
     (get-sub-tree bot-options-tree :opt :all))
    table))

(defun is-avaliable-compiler-option-? (option)
  (find option '(:lang) :test #'eq))

(defun is-avaliable-bot-option-? (option)
  (find option '(:name :port :host) :test #'eq))

(defun check-option-value (value type)
  (case type
    (t (raise-bs-post-process-error
        "expected ~a type, but got ~a for option in file: "))))
