;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/lexer/lexer-utils
    (:use :cl
          :alexandria
          :nobot/botscript/lexer/lexer-nodes
          :nobot/botscript/lexer/token)
  (:import-from :nobot/utils
                #:to-symbol
                #:reintern)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-lexer-error)
  (:export #:*source*
           #:with-source-code
           #:is-keyword-char-?
           #:is-white-space-char-?
           #:is-keyword-?
           #:is-delimiter-?
           #:is-dquote-?
           #:is-locked-lexical-analysis-?
           #:switch-ls-state
           #:get-ls-state
           #:with-lexical-switcher
           #:no-term-to
           #:raise-lexer-error
           ))

(in-package :nobot/botscript/lexer/lexer-utils)

(defvar *lexical-switcher*)
(defvar *source*)

;; also newline
(defparameter +delimiter-table+
  "{}[],=:")

(defparameter +keyword-table+
  '("c-opts"
    "bot-opts"
    "start"
    "from"
    "letd"
    "letv"
    "def-act"
    "act"
    "type"
    "in"
    "out"))

(defclass lexical-switcher ()
  ((comment-state
    :type boolean
    :initform nil
    :accessor get-comment-state)
   (multi-comment-state
    :type boolean
    :initform nil
    :accessor get-multi-comment-state)
   (string-state
    :type boolean
    :initform nil
    :accessor get-string-state)))

(defmacro with-lexical-switcher (() &body body)
  `(let ((*lexical-switcher* (make-instance 'lexical-switcher)))
     ,@body))


(defmacro with-source-code ((type source &key
                                  convert-tokens
                                  convert-with-pos
                                  use-lazy-tokens
                                  return-instance) &body body)
  (with-gensyms (tokens-source-instance)
    `(progn
       (when (and ,return-instance ,convert-tokens)
         (error "Expected ~A with other keys or ~A"
                "<convert-tokens>"
                "<return-instance>"))
       (when (and (or ,convert-with-pos ,use-lazy-tokens)
                  (not ,convert-tokens))
         (error "Expected ~A key before using the keys ~A and ~A"
                "<convert-tokens>"
                "<convert-with-pos>"
                "<use-lazy-tokens>"))
       (let* ((*source* (case ,type
                          (:file
                           (make-instance
                            'from-file-source-node
                            :fstream (open
                                      ,source
                                      :direction :input
                                      :if-does-not-exist :error)
                            :source ,source
                            :type ,type))
                          (:string
                           (make-instance 'from-string-source-node
                                          :source ,source
                                          :type ,type))
                          (t (error "Unknown type ~A" ,type)))))
         ,@body
         ,(when (eq type :file)
            '(close (get-fstream *source*)))
         (let ((,tokens-source-instance (make-tokens-source *source*)))
           (if ,return-instance
               ,tokens-source-instance
               (if ,convert-tokens
                   (convert-tokens ,tokens-source-instance
                                   :with-pos ,convert-with-pos
                                   :lazy ,use-lazy-tokens)
                   (get-tokens-seq ,tokens-source-instance))))))))

(defun is-delimiter-? (ch)
  (or (find ch +delimiter-table+)
      (eq ch #\Newline)))

(defun is-white-space-char-? (ch)
  (find ch
        '(#\space #\Tab #\newline #\Backspace #\Return #\Linefeed #\Page)
        :test #'eq))

(defun is-keyword-? (word)
  (find (string-upcase word) +keyword-table+
        :test #'equal
        :key #'string-upcase))

(defun is-dquote-? (ch)
  (eq ch #\"))

(defun no-term-to (to key val)
  "to: :sym || :description or sym || description"
  (let ((to (if (keywordp to)
                to
                (reintern to :keyword))))
    (if (or (eq to :sym)
            (eq to :description))
        (case key
          (:delimiter
           (case val
             (#\{
              (if (eq to :sym)
                  (to-symbol "o-bracket")
                  "open bracket"))
             (#\}
              (if (eq to :sym)
                  (to-symbol "c-bracket")
                  "close bracket"))
             (#\[
              (if (eq to :sym)
                  (to-symbol "o-sq-bracket")
                  "open square bracket"))
             (#\]
              (if (eq to :sym)
                  (to-symbol "c-sq-bracket")
                  "close square bracket"))
             (#\,
              (if (eq to :sym)
                  (to-symbol "comma")
                  "comma"))
             (#\=
              (if (eq to :sym)
                  (to-symbol "assign")
                  "assign"))
             (#\:
              (if (eq to :sym)
                  (to-symbol "colon")
                  "colon"))
             (#\Newline
              (if (eq to :sym)
                  (to-symbol "newline")
                  "new line"))))
          (:keyword
           (when (is-keyword-? val)
             (let ((sym (to-symbol val)))
               (if (eq to :sym)
                   sym
                   (format nil "~a keyword" val)))))
          (t (error "unknown no term sym: ~a" key)))
        (error "unknown `to` value arg: ~a, expected: :sym ot :description" to))))

(defun raise-lexer-error (on-error &optional val)
  ":on-char, :on-close-comment, :on-string"
  (raise-bs-lexer-error
   (concatenate
    'string
    (case on-error
      (:on-char
       (assert val)
       (format nil "unknown symbol \"~a\"" val))
      (:on-close-comment
       "comment closing expected")
      (:on-string
       "double quotes expected at position")
      (t
       (error "unknown type of arg `on-error`: ~a, see fun doc"
              on-error)))
    " at position: line - ~a, column - ~a~a")
   (get-position-y *source*)
   (1+ (get-position-x *source*))
   (if (eq (get-source-type *source*) :file)
       (format nil ", file: ~a"
               (get-source *source*))
       "")))

(defun switch-ls-state (state)
  ":comment, :multi-comment, :string"
  (case state
    (:comment
     (setf (get-comment-state *lexical-switcher*)
           (not (get-comment-state *lexical-switcher*))))
    (:multi-comment
     (setf (get-multi-comment-state *lexical-switcher*)
           (not (get-multi-comment-state *lexical-switcher*))))
    (:string
     (setf (get-string-state *lexical-switcher*)
           (not (get-string-state *lexical-switcher*))))
    (t (error "unknown state of lexical switcher: ~a, see fun doc"
              state))))

(defun get-ls-state (state)
  ":comment, :multi-comment, :string"
  (case state
    (:comment
     (get-comment-state *lexical-switcher*))
    (:multi-comment
     (get-multi-comment-state *lexical-switcher*))
    (:string
     (get-string-state *lexical-switcher*))
    (t (error "unknown state of lexical switcher: ~a, see fun doc"
              state))))
