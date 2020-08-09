(uiop:define-package :nobot/botscript/lexer-utils
    (:use :cl
          :alexandria
          :nobot/botscript/nodes
          :nobot/botscript/token-utils)
  (:export #:*source*
           #:with-source-code
           #:is-keyword-char-?
           #:is-whitespace-char-?
           #:is-keyword-?))

(in-package :nobot/botscript/lexer-utils)

(defparameter *source* nil)

(defmacro with-source-code ((type source &key
                                  convert-tokens
                                  convert-with-pos
                                  use-lazy-tokens
                                  return-instance) &body body)
  (with-gensyms (tokens-source-instance)
    `(progn
       (when (and ,return-instance ,convert-tokens)
         (error "Use ~A with other keys or ~A"
                "<convert-tokens>"
                "<return-instance>"))
       (when (and (or ,convert-with-pos ,use-lazy-tokens)
                  (not ,convert-tokens))
         (error "Use ~A key before using the keys ~A and ~A"
                "<convert-tokens>"
                "<convert-with-pos>"
                "<use-lazy-tokens>"))
       (let* ((*source* (case ,type
                          (:file
                           (make-instance 'from-file-source-node
                                          :fstream (open ,source
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


(defun is-keyword-char-? (ch)
  (find ch "#!@$"))

(defun is-white-space-char-? (ch)
  (some (curry #'eq ch)
        '(#\space #\Tab #\newline #\Backspace #\Return #\Linefeed #\Page)))

(defun is-keyword-? (word)
  (some (curry #'equal word)
        '("#EXE" "!USE" "$COMBO" "@DEF")))
