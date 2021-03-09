;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/lexer/delimiter
    (:use :cl)
  (:import-from :anaphora
                #:awhen
                #:it)
  (:import-from :nobot/utils
                #:to-symbol
                #:reintern)
  (:export #:is-delimiter-?
           #:delimiter-to))

(in-package :nobot/botscript/lexer/delimiter)

(defparameter *char-delimiter-table*
  (make-hash-table :test #'eq))

(defparameter *sym-delimiter-table*
  (make-hash-table :test #'eq))

(defclass delimiter ()
  ((del
    :initarg :del
    :type string
    :reader $del-get-del)
   (sym
    :initarg :sym
    :type symbol
    :reader $del-get-sym)
   (description
    :initarg :description
    :type string
    :reader $del-get-description)))

(defun define-delimiter (char-table sym-table &key del sym-idea description)
  (assert (and char-table sym-table del sym-idea description))
  (let* ((sym (to-symbol sym-idea))
         (del
          (make-instance 'delimiter
                         :del del
                         :sym sym
                         :description description)))
    (setf (gethash sym sym-table) del)
    (setf (gethash del char-table) del)))

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del "{"
  :sym-idea "o-fig-bracket"
  :description "open figurate bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del "}"
  :sym-idea "c-fig-bracket"
  :description "close figurate bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del "["
  :sym-idea "o-sq-bracket"
  :description "open square bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del "]"
  :sym-idea "c-sq-bracket"
  :description "close sqaure bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del ","
  :sym-idea "comma"
  :description "comma")

;; (define-delimiter *char-delimiter-table* *sym-delimiter-table*
;;   :del "="
;;   :sym-idea "assign"
;;   :description "assign")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del ":"
  :sym-idea "colon"
  :description "colon")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del ";"
  :sym-idea "semicolon"
  :description "semicolon")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :del "=="
  :sym-idea "logic-equal"
  :description "logic equal")

(defun get-delimiter (sym-or-str)
  (or (gethash sym-or-str *char-delimiter-table*)
      (gethash sym-or-str *sym-delimiter-table*)))

(defun is-delimiter-? (sym-or-str)
  (get-delimiter sym-or-str))

(defun delimiter-to (sym-or-str to)
  "to arg it :del or :sym or :description, or sym representations"
  (awhen (get-delimiter sym-or-str)
    (case to
      (:del
       ($del-get-del it))
      (:sym
       ($del-get-sym it))
      (:description
       ($del-get-description it))
      (t (error "unknown value of `to` arg: ~a, func doc" to)))))
