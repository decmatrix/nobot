;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/lexer/lexer-impl
    (:use :cl
          :anaphora
          :alexandria
          :nobot/botscript/lexer/token
          :nobot/botscript/lexer/lexer-nodes
          :nobot/botscript/lexer/lexer-utils)
  (:import-from :nobot/botscript/types
                #:use-token-type-class)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-lexer-error)
  (:export #:disassemble-source
           #:disassemble-string
           #:disassemble-file
           #:with-disassembled-source
           #:get-tokens-source))

(in-package :nobot/botscript/lexer/lexer-impl)

(defvar *tokens-source*)

(defmacro with-disassembled-source ((source type) &body body)
  `(let ((*tokens-source*
          (disassemble-source ,source
                              :type ,type
                              :return-instance t)))
     ,@body))


(defun disassemble-source (source &key (type :file)
                                    convert-tokens
                                    convert-with-pos
                                    use-lazy-tokens
                                    return-instance)
  (unless source
    (error "Can't read source if source is nil"))
  (with-source-code (type source
                          :convert-tokens convert-tokens
                          :convert-with-pos convert-with-pos
                          :use-lazy-tokens use-lazy-tokens
                          :return-instance return-instance)
    (loop for ch = (next-char *source*)
       while ch
       do (do-char ch))))

(defun disassemble-string (str &key convert-with-pos)
  (disassemble-source str
                      :type :string
                      :convert-tokens t
                      :convert-with-pos convert-with-pos))

(defun disassemble-file (path &key convert-with-pos)
  (disassemble-source path
                      :type :file
                      :convert-tokens t
                      :convert-with-pos convert-with-pos))

;;TODO: implement multi comments
;;TODO: implement handle error end of file in string case
(defmacro read-chars (prev-char type)
  ":id-or-keyword, :num-string, :char-string, :single-comment"
  (with-gensyms (ch word is-keyword buff)
    `(progn
       (clear-chars-buffer *source*)
       (push-char-to-buffer ,prev-char *source*)
       (block in-read-chars-loop
         (loop for ,ch = (next-char *source*)
            while ,ch
            do  (if ,(case type
                       ((:id-or-keyword)
                        `(or (alphanumericp ,ch)
                             (eq ,ch #\-)))
                       (:num-string
                        `(digit-char-p ,ch))
                       (:char-string
                        `(not (eq ,ch #\")))
                       (:single-comment
                        `(eq ,ch #\\)))
                    ,(if (eq type :single-comment)
                         `(progn
                            (update-pos ,ch *source*)
                            (lock-lexical-analysis)
                            (return-from in-read-chars-loop))
                         `(progn
                            (update-pos ,ch *source*)
                            (push-char-to-buffer ,ch *source*)))
                    (progn
                      ,(if (eq type :id-or-keyword)
                           `(update-pos ,ch *source*) 
                           `(undo-next-char ,ch *source*))
                      (return-from in-read-chars-loop)))))
       (let* ((,word
               (let ((,buff (concatenate 'string (get-chars-buffer *source*))))
                 ,(if (eq type :id-or-keyword)
                      `,buff
                      `(string-upcase ,buff))))
              (,is-keyword (is-keyword-? ,word)))
         (declare (ignorable ,is-keyword))
         (push-token-to-buffer
          ,(case type
             (:id-or-keyword
              `(new-token
                :type (if ,is-keyword
                          :keyword
                          :id)
                :value (if ,is-keyword
                           (intern ,word :cl-user)
                           ,word)
                :position (get-fixed-cur-position *source*)))
             (:num-string
              `(new-token
                :type :number-string
                :value (parse-integer ,word)
                :position (get-fixed-cur-position *source*)))
             (:char-string
              `(new-token
                :type :string
                :value (wrap-word-in-dquotes ,word)
                :position (get-fixed-cur-position *source*)))
             (:single-comment nil)
             (t (error "unknown token type for read chars (see func doc): ~a" type)))
          *source*)))))

(defun make-delimiter-token (ch)
  (push-token-to-buffer
   (new-token
    :type :delimiter
    :value (get-symbol-for-delimiter ch)
    :position (get-fixed-cur-position *source*))
   *source*))

(defun do-char (ch)
  (cond
    ((and (eq ch #\Newline) (is-locked-lexical-analysis-?))
     (update-pos ch *source*)
     (unlock-lexical-analysis))
    ((eq ch #\\)
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :single-comment)
     (unless (is-locked-lexical-analysis-?)
       (raise-lexer-error ch)))
    ((is-delimiter-? ch)
     ;; :delimiter
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (make-delimiter-token ch))
    ((alpha-char-p ch)
     ;; :id-or-keyword
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :id-or-keyword))
    ((digit-char-p ch)
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :num-string))
    ((is-dquote-? ch)
     ;; char-string
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :char-string))
    ((is-white-space-char-? ch)
     (update-pos ch *source*))
    (t (raise-lexer-error ch))))

(defun raise-lexer-error (ch)
  (raise-bs-lexer-error
   "unknown symbol \"~a\" at position: line - ~a, column - ~a"
   ch
   (get-position-y *source*)
   (1+ (get-position-x *source*))))


;; util for with-disassembled-source macros
(defun get-tokens-source ()
  *tokens-source*)
