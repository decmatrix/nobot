;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/post
    (:use :cl)
  (:import-from :anaphora
                #:aif
                #:awhen
                #:it)
  (:import-from :alexandria
                #:rcurry)
  (:import-from :nobot/botscript/lexer
                #:terminal-to)
  (:import-from :nobot/utils
                #:to-keyword)
  (:import-from :nobot/botscript/types/types-utils
                #:get-from-type)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-parser-result)
  (:import-from :nobot/botscript/types
                #:convert-type)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-post-process-error)
  (:import-from :nobot/botscript/parser/acacia/result-packaging
                #:acacia-get-parse-tree
                #:acacia-get-source-type
                #:acacia-get-source)
  (:import-from :nobot/botscript/parser/acacia/tree-tools
                #:get-sub-tree
                #:get-custom-sub-tree-getter)
  (:export #:bot-project-info
           #:botscript-post-process
           #:botscript-post-process-info
           #:get-compiler-options
           #:get-bot-options
           #:get-var-declarations
           #:get-state-points-declarations
           #:get-state-actions-declarations
           #:get-start-from-id))

(in-package :nobot/botscript/post)

(defclass botscript-post-process-info ()
  ((compiler-options
    :initarg :compiler-options
    :type hash-table
    :reader get-compiler-options)
   (bot-options
    :initarg :bot-options
    :type hash-table
    :reader get-bot-options)
   (var-declarations
    :initarg :var-declarations
    :type hash-table
    :reader get-var-declarations)
   (state-points-declarations
    :initarg :state-points-declarations
    :type hash-table
    :reader get-state-points-declarations)
   (state-actions-declarations
    :initarg :state-actions-declarations
    :type hash-table
    :reader get-state-actions-declarations)
   (start-from-id
    :initarg :start-from-id
    :type keyword
    :reader get-start-from-id)))

(defgeneric check-start-from-id (obj))

(defparameter *avaliable-compiler-options*
  '(:@codegen :@platform :@arch-type))

(defparameter *avaliable-bot-common-options*
  '(:name :author :version))

(defparameter *avaliable-bot-web-options*
  '(:port :host))

(defparameter *avaliable-bot-telegram-options*
  '(:token))

(defparameter *avaliable-values-of-codegen-opt*
  '("js"))

(defparameter *avaliable-values-of-platform-opt*
  '("web" "telegram" "tg"))

(defparameter *avaliable-values-of-arch-type-opt*
  '("chat"))

(defparameter *avaliable-state-point-options*
  '(:act))

(defvar *parser-result*)
(defvar *table*)
(defvar *custom-get-sub-tree*)
(defvar *compiler-options*)

(defun botscript-post-process ()
  (let* ((*custom-get-sub-tree*
          (get-custom-sub-tree-getter
           (rcurry #'terminal-to :sym)))
         (*parser-result* (get-parser-result *context*))
         (parse-tree (acacia-get-parse-tree *parser-result*))
         (*compiler-options*
          (make-declaration-table
           (funcall
            *custom-get-sub-tree*
            parse-tree
            :compiler-option
            :all t)
           #'process-compiler-option)))
    (awhen (make-instance
            'botscript-post-process-info
            :compiler-options *compiler-options*
            :bot-options (make-declaration-table
                          (funcall
                           *custom-get-sub-tree*
                           parse-tree
                           :bot-option
                           :all t)
                          #'process-bot-option)
            :var-declarations (make-declaration-table
                               (funcall
                                *custom-get-sub-tree*
                                parse-tree
                                :var-decl
                                :all t)
                               #'process-var-decl)
            :state-points-declarations (make-declaration-table
                                        (funcall
                                         *custom-get-sub-tree*
                                         parse-tree
                                         :state-point-decl
                                         :all t)
                                        #'process-state-point-decl)
            :state-actions-declarations (make-declaration-table
                                         (funcall
                                          *custom-get-sub-tree*
                                          parse-tree
                                          :state-decl
                                          :all t)
                                         #'process-state-decl)
            :start-from-id (from-tree-get-start-from-id parse-tree))
      (check-start-from-id it)
      it)))

(defun make-declaration-table (declarations-list process-fn)
  "process fn args: declaration"
  (let ((*table* (make-hash-table :test #'eq)))
    (mapc process-fn declarations-list)
    *table*))

;;TODO: optimized it 
(defun process-compiler-option (option)
  (let ((id (to-keyword
             (second
              (second
               (second option)))))
        (value (second
                (second
                 (third option)))))
    (when (is-avaliable-option-? id :compiler)
      (setf (gethash id *table*)
            (awhen (is-avaliable-value-and-type-? id value)
              it)))))

(defun process-bot-option (bot-option)
  (let ((id (to-keyword
             (second
              (second bot-option))))
        (value (second
                (third bot-option))))
    (when (is-avaliable-option-? id :bot)
      (setf (gethash id *table*)
            (awhen (is-avaliable-value-and-type-? id value)
              it)))))

(defun process-var-decl (var-decl)
  (setf (gethash (to-keyword
                  (second
                   (second var-decl)))
                 *table*)
        (third var-decl)))

(defun process-state-point-decl (state-point-decl)
  (let ((id (to-keyword
             (second
              (second state-point-decl)))))
    (is-avaliable-state-point-opt-? id)
    (setf (gethash id *table*)
          (third state-point-decl))))

(defun process-state-decl (state-decl)
  (setf (gethash (to-keyword
                  (second
                   (second state-decl)))
                 *table*)
        (third state-decl)))

(defun from-tree-get-start-from-id (parse-tree)
  (to-keyword
   (second
    (second
     (funcall *custom-get-sub-tree* parse-tree
              :start-from-stmt)))))

(defmethod check-start-from-id ((obj botscript-post-process-info))
  (let ((start-from-id (get-start-from-id obj)))
    (unless (gethash (get-state-points-declarations obj) start-from-id)
      (raise-bs-post-process-error
       "undefined state point ~a~a"
       start-from-id
       (make-source-msg)))))

(defun is-avaliable-option-? (option type)
  (case type
    (:bot
     (if (find option *avaliable-bot-options* :test #'eq)
         (is-avaliable-option-for-platform-? option)
         (raise-bs-post-process-error
          "not avaliable bot option: ~a~a"
          option
          (make-source-msg))))
    (:compiler
     (if (find option *avaliable-compiler-options* :test #'eq)
         t
         (raise-bs-post-process-error
          "not avaliable compiler option ~a~a"
          option
          (make-source-msg))))
    (t (error "unknown type ~a" type))))

(defun is-avaliable-value-and-type-? (id value)
  (let ((converted-type
         (convert-type (first value)))
        (value (string-trim '(#\Space #\Tab #\Newline)
                            (string-downcase (second value)))))
    (case id
      (:@codegen
       (is-char-string-? converted-type id)
       (if (find value *avaliable-values-of-codegen-opt* :test #'equal)
           value
           (raise-bs-post-process-error
            "unavailable value of @codegen option: ~a"
            value)))
      (:@platform
       (is-char-string-? converted-type id)
       (if (find value *avaliable-values-of-platform-opt* :test #'equal)
           value
           (raise-bs-post-process-error
            "unavaliable value of @platform option: ~a"
            value)))
      (:@arch-type
       (is-char-string-? converted-type id)
       (if (find value *avaliable-values-of-arch-type-opt* :test #'equal)
           value
           (raise-bs-post-process-error
            "unavaliable value of @arch-type option: ~a"
            value)))
      ((:name :host :version :author :token)
       (is-char-string-? converted-type id)
       value)
      (:port
       (is-number-string-? converted-type id)
       value)
      (t (error "unknown option, ~a" id)))))

(defun is-char-string-? (converted-type id)
  (or (eq converted-type :char-string)
      (raise-type-error
       converted-type
       :char-string
       id)))

(defun is-number-string-? (converted-type id)
  (or (eq converted-type :number-string)
      (raise-type-error
       converted-type
       :number-string
       id)))

(defun is-avaliable-option-for-platform-? (id)
  (case (to-keyword (gethash :@platform *compiler-options*))
    (:web
     (or (find id *avaliable-bot-web-options* :test #'eq)
         (raise-bs-post-process-error
          "unavaliable bot option ~a for web platform"
          id)))
    ((:telegram :tg)
     (or (find id *avaliable-bot-telegram-options* :test #'eq)
         (raise-bs-post-process-error
          "unavaliable bot option ~a for telegram platform"
          id)))))

(defun is-avaliable-state-point-opt-? (id)
  (or (find id *avaliable-state-point-options* :test #'eq)
      (raise-bs-post-process-error
       "unavaliable state point options: ~a"
       id)))

(defun raise-type-error (input-type expected-type option)
  (raise-bs-post-process-error
   "expected ~a type, but got ~a for option ~a~a"
   (get-from-type input-type :token :description)
   (get-from-type expected-type :token :description)
   option
   (make-source-msg)))

(defun make-source-msg ()
  (if (eq (acacia-get-source-type *parser-result*) :file)
      (format nil " in file: ~a"
              (acacia-get-source *parser-result*))
      ""))
