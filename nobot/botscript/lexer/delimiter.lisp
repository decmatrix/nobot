;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/lexer/delimiter
    (:use :cl)
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
  (let ((del
         (make-instance 'delimiter
                        :char ch
                        :sym (to-symbol sym-idea)
                        :description description)))
    (setf (gethash sym-idea sym-table) del)
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

(defun get-delimiter (sym-or-char-or-key)
  (or (gethash sym-or-char-or-key *char-delimiter-table*)
      (gethash sym-or-char-or-key *sym-delimiter-table*)))

(defun is-delimiter-? (sym-or-char-or-key)
  (get-delimiter sym-or-char-or-key))

(defun delimiter-to (sym-or-char-or-key to)
  "to arg it :sym or :value or :description, or sym representations"
  (let ((del (get-delimiter sym-or-char-or-key)))
    (assert del)
    (case to
      (:char
       ($del-get-char del))
      (:sym
       ($del-get-sym del))
      (:description
       ($del-get-description del))
      (t (error "unknown value of `to` arg: ~a, func doc" to)))))
