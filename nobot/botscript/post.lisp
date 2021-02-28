;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/post
    (:use :cl)
  (:import-from :nobot/utils/common-utils
                #:to-keyword)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-parser-result)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-post-process-error)
  (:import-from :nobot/botscript/parser/acacia/result-packaging
                #:acacia-get-parse-tree
                #:acacia-get-source-type
                #:acacia-get-source)
  (:import-from :nobot/botscript/parser/acacia/tree-tools
                #:get-sub-tree)
  (:export #:bot-project-info
           #:botscript-post-process
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

(defparameter *avaliable-compiler-options*
  '(:lang))

(defparameter *avaliable-bot-options*
  '(:name :port :host))

(defvar *parser-result*)

(defun botscript-post-process ()
  (let* ((*parser-result* (get-parser-result *context*))
         (parse-tree (acacia-get-parse-tree *parser-result*)))
    (make-instance
     'botscript-post-process-info
     :bot-options (make-bot-options-table
                   (get-sub-tree parse-tree :bot-options))
     :compiler-options (make-compiler-options-table
                        (get-sub-tree parse-tree :compiler-options))
     :parse-tree (get-sub-tree parse-tree :script-rest))))

(defun make-bot-options-table (bot-options-tree)
  (let ((table (make-hash-table :test #'eq)))
    (mapc
     (lambda (option)
       (let ((id (to-keyword (second (second option))))
             (value (second (fourth option))))
         (if (is-avaliable-bot-option-? id)
             (setf (gethash id table)
                   (check-option-value value))
             (raise-bs-post-process-error
              "not avaliable compiler option: ~a~a"
              id
              (make-source-msg)))))
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
             (raise-bs-post-process-error
              "not avaliable bot option: ~a~a"
              id
              (make-source-msg)))))
     (get-sub-tree bot-options-tree :opt :all))
    table))

(defun is-avaliable-compiler-option-? (option)
  (find option *avaliable-compiler-options* :test #'eq))

(defun is-avaliable-bot-option-? (option)
  (find option *avaliable-bot-options* :test #'eq))

(defun check-option-value (value type)
  (case type
    (t (raise-bs-post-process-error
        "expected ~a type, but got ~a for option~a"
        type
        (make-source-msg)))))

(defun type-check (value))

(defun make-source-msg ()
  (if (eq (acacia-get-source-type *parser-result*) :file)
      (format nil " in file: ~a"
              (acacia-get-source *parser-result*))
      ""))
