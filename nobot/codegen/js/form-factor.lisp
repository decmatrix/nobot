;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen/js/form-factor
    (:use :cl)
  (:export #:with-build-output
           ;; builder from pattern
           #:build-require-stmt
           #:build-chain-call
           ;; base builder
           #:build-new-instance-expr
           #:build-multi-comment
           #:build-new-line
           #:build-strict-expr
           #:build-call-fun-expr
           #:build-storage-location
           #:build-str-literal))

(in-package :nobot/codegen/js/form-factor)

(defvar *stream*)

(defmacro with-build-output ((stream) &body body)
  `(let ((*stream* ,stream))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;; builder from patterns ;;;;;;;;;;;;;;;;;;;;;;
(defun build-require-stmt (const-name path-lib &key (stream *stream*))
  (format stream
          (build-storage-location
           const-name
           :const
           :as-expression (build-call-fun-expr
                           "require"
                           `(,(build-str-literal path-lib)))
           :stream nil)))

(defun build-chain-call (lst &key (stream *stream*))
  (format stream "~{~a~^.~}" lst))

;;;;;;;;;;;;;;;;;;;;;; base builder ;;;;;;;;;;;;;;;;;;;;;;
(defun build-multi-comment (str &key (stream *stream*))
  (format "/*~%~a~%*/" str))

(defun build-new-line (&key (stream *stream*))
  (format stream "~%"))

(defun build-strict-expr (&key (stream *stream*) )
  (format "~a~%" (build-str-literal "use strict" :stream stream)))

(defun build-new-instance-expr (call-expr &key (stream *stream*))
  (format stream "new ~a" call-expr))

(defun build-call-fun-expr (fun-name args-list &key (stream *stream*))
  (format stream "~a(~{~a~^, ~})"
          fun-name
          args-list))

(defun build-storage-location (name type &key as-expression (stream *build-stream*))
  (format stream "~a ~a~a~%"
          (if as-expression
              (format nil " = ~a;" as-expression)
              ";")))

(defun build-str-literal (value &key (stream *stream*))
  (format stream "'~a'" value))

;;;;;;;;;;;;;;;;;;;;;; lang utils ;;;;;;;;;;;;;;;;;;;;;;
(defun is-storage-location-type-? (type)
  (find type '(:const :var :let) :test #'eq))



(defmacro build-js-code-from-tree (lisp-form)
  `(build-js-aux ',lisp-form))

(defun build-js-aux (js-tree)
  (let ((root (car js-tree)))
    (case root
      (:multi-comment)
      (:strict-mode)
      ((:cosnt :let :var)
       )
      (t (error "Unknown rule ~a" root)))))


;; example
;; ((:multi-comment "Source code of bot generated by NOBOT")
;;  (:strict-mode)
;;  (:const
;;   "wisteria"
;;   (:fun-call
;;    "require"
;;    (:args
;;     "./wisteria")))
;;  (:const
;;   "botOptions"
;;   (:new-instance
;;    (:expr-chain
;;     "wisteria"
;;     (:fun-call
;;      "BotOptions"
;;      (:args
;;       "TestBot"
;;       "8082"
;;       "localhost")))))
;;  (:const
;;   "bot"
;;   (:new-instance
;;    (:expr-chain
;;     "wisteria"
;;     (:fun-call
;;      "Bot"
;;      (:args
;;       "botOptions"))))))