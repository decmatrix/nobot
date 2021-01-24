(uiop:define-package :nobot/botscript/lexer-utils
    (:use :cl
          :alexandria
          :nobot/botscript/nodes
          :nobot/botscript/token-utils)
  (:import-from :nobot/utils
                #:defcontextvar
                #:define-constant-?)
  (:export #:*source*
           #:with-source-code
           #:is-keyword-char-?
           #:is-white-space-char-?
           #:is-keyword-?
           #:is-delimiter-?
           #:is-dquote-?
           #:is-locked-lexical-analysis-?
           #:get-symbol-for-keyword
           #:get-symbol-for-delimiter
           #:wrap-word-in-dquotes
           #:lock-lexical-analysis
           #:unlock-lexical-analysis))

(in-package :nobot/botscript/lexer-utils)

(defcontextvar *source*)

(defparameter *lock-lexical-analysis* nil)

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
    "out"
    ))

;; also newline
(defparameter +delimiter-table+
  "{}[],=:")

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
        :key #'eq))

(defun is-keyword-? (word)
  (find word +keyword-table+
        :test #'equal
        :key #'string-upcase))

(defun is-dquote-? (ch)
  (eq ch #\"))

(defun get-symbol-for-keyword (str)
  (when (is-keyword-? str)
    (intern (string-upcase str) :cl-user)))

(defun get-symbol-for-delimiter (del)
  (intern
   (string-upcase
    (case del
      (#\{ "o-bracket")
      (#\} "c-bracket")
      (#\[ "o-sq-bracket")
      (#\] "c-sq-bracket")
      (#\, "comma")
      (#\= "assign")
      (#\: "colon")
      (#\Newline "newline")))
   :cl-user))

(defun wrap-word-in-dquotes (word)
  (format nil "\"~a\"" word))

(defun lock-lexical-analysis ()
  (setf *lock-lexical-analysis* t))

(defun unlock-lexical-analysis ()
  (setf *lock-lexical-analysis* nil))

(defun is-locked-lexical-analysis-? ()
  *lock-lexical-analysis*)
