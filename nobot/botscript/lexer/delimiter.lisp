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
  ((char
    :initarg :char
    :type character
    :reader $del-get-char)
   (sym
    :initarg :sym
    :type symbol
    :reader $del-get-sym)
   (description
    :initarg :description
    :type string
    :reader $del-get-description)))

(defun define-delimiter (char-table sym-table &key ch sym-idea description)
  (assert (and char-table sym-table ch sym-idea description))
  (let* ((sym (to-symbol sym-idea))
         (del
          (make-instance 'delimiter
                         :char ch
                         :sym sym
                         :description description)))
    (setf (gethash sym sym-table) del)
    (setf (gethash ch char-table) del)))

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\{
  :sym-idea "o-fig-bracket"
  :description "open figurate bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\}
  :sym-idea "c-fig-bracket"
  :description "close figurate bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\[
  :sym-idea "o-sq-bracket"
  :description "open square bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\]
  :sym-idea "c-sq-bracket"
  :description "close sqaure bracket")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\,
  :sym-idea "comma"
  :description "comma")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\=
  :sym-idea "assign"
  :description "assign")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\:
  :sym-idea "colon"
  :description "colon")

(define-delimiter *char-delimiter-table* *sym-delimiter-table*
  :ch #\Newline
  :sym-idea "newline"
  :description "newline")

(defun get-delimiter (sym-or-char)
  (or (gethash sym-or-char *char-delimiter-table*)
      (gethash sym-or-char *sym-delimiter-table*)))

(defun is-delimiter-? (sym-or-char)
  (get-delimiter sym-or-char))

(defun delimiter-to (sym-or-char to)
  "to arg it :sym or :value or :description, or sym representations"
  (awhen (get-delimiter sym-or-char)
    (case to
      (:char
       ($del-get-char it))
      (:sym
       ($del-get-sym it))
      (:description
       ($del-get-description it))
      (t (error "unknown value of `to` arg: ~a, func doc" to)))))
