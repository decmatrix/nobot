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
  (:export #:*source*
           #:with-source-code
           #:is-keyword-char-?
           #:is-white-space-char-?
           #:is-keyword-?
           #:is-delimiter-?
           #:is-dquote-?
           #:is-locked-lexical-analysis-?
           #:wrap-word-in-dquotes
           #:lock-lexical-analysis
           #:unlock-lexical-analysis
           #:no-term-to
           ))

(in-package :nobot/botscript/lexer/lexer-utils)

(defvar *source*)

(defparameter *lock-lexical-analysis* nil)

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
  (find word +keyword-table+
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
                  "o-bracket"
                  "open bracket"))
             (#\}
              (if (eq to :sym)
                  "c-bracket"
                  "close bracket"))
             (#\[
              (if (eq to :sym)
                  "o-sq-bracket"
                  "open square bracket"))
             (#\]
              (if (eq to :sym)
                  "c-sq-bracket"
                  "close square bracket"))
             (#\,
              (if (eq to :sym)
                  "comma"
                  "symbol comma"))
             (#\=
              (if (eq to :sym)
                  "assign"
                  "symbol assign"))
             (#\:
              (if (eq to :sym)
                  "colon"
                  "symbol colon"))
             (#\Newline
              (if (eq to :sym)
                  "newline"
                  "new line"))))
          (:keyword
           (when (is-keyword-? val)
             (let ((sym (to-symbol val)))
               (if (eq to :sym)
                   sym
                   (format nil "~a keyword" val)))))
          (t (error "unknown no term sym: ~a" key)))
        (error "unknown `to` value arg: ~a, expected: :sym ot :description" to))))

(defun wrap-word-in-dquotes (word)
  (format nil "\"~a\"" word))

(defun lock-lexical-analysis ()
  (setf *lock-lexical-analysis* t))

(defun unlock-lexical-analysis ()
  (setf *lock-lexical-analysis* nil))

(defun is-locked-lexical-analysis-? ()
  *lock-lexical-analysis*)
