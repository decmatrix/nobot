;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/lexer/lexer-impl
    (:use :cl
          :anaphora
          :alexandria
          :nobot/botscript/lexer/token
          :nobot/botscript/lexer/lexer-nodes
          :nobot/botscript/lexer/lexer-utils
          :nobot/botscript/lexer/delimiter)
  (:import-from :nobot/botscript/types
                #:use-token-type-class)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-lexer-error)
  (:export #:disassemble-source
           #:disassemble-string
           #:disassemble-file
           #:with-disassembled-source
           #:get-tokens-source
           #:terminal-to))

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
    (with-lexical-switcher ()
      (loop for ch = (next-char *source*)
         while ch
         do (do-char ch))
      (when (get-ls-state :multi-comment)
        (raise-lexer-error :on-close-comment)))))

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

(defmacro read-chars (prev-char type)
  ":id-or-keyword, :num-string, :char-string, :comment, :multi-comment-end"
  (with-gensyms (ch word is-keyword buff)
    `(progn
       (clear-chars-buffer *source*)
       (push-char-to-buffer ,prev-char *source*)
       (block in-read-chars-loop
         (loop for ,ch = (next-char *source*)
            while ,ch
            do  (progn
                  (update-pos ,ch *source*)
                  (if ,(case type
                         (:id-or-keyword
                          `(or (alphanumericp ,ch)
                               (eq ,ch #\-)))
                         (:num-string
                          `(digit-char-p ,ch))
                         (:char-string
                          `(not (eq ,ch #\")))
                         (:multi-comment-end
                          `(eq ,ch #\/))
                         (:comment
                          `(or (eq ,ch #\/)
                               (eq ,ch #\*))))
                      ,(case type
                         (:comment
                          `(progn
                             (when (eq ,ch #\*)
                               (switch-ls-state :multi-comment))
                             (switch-ls-state :comment)
                             (return-from in-read-chars-loop)))
                         (:multi-comment-end
                          `(progn
                             (switch-ls-state :comment)
                             (switch-ls-state :multi-comment)
                             (return-from in-read-chars-loop)))
                         (t
                          `(push-char-to-buffer ,ch *source*)))
                      (progn
                        ,(if (find type '(:char-string :comment))
                             `(progn
                                ,(when (eq type :char-string)
                                   `(switch-ls-state :string))
                                (update-pos ,ch *source*)) 
                             `(undo-next-char ,ch *source*))
                        (return-from in-read-chars-loop))))))
       ,(unless (or (eq type :comment)
                    (eq type :multi-comment-end))
          `(let* ((,word
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
                               (terminal-to :sym :keyword ,word)
                               ,word)
                    :position (get-fixed-cur-position *source*)))
                 (:num-string
                  `(new-token
                    :type :number-string
                    :value (parse-integer ,word)
                    :position (get-fixed-cur-position *source*)))
                 (:char-string
                  `(new-token
                    :type :char-string
                    :value (format nil "~a\"" ,word)
                    :position (get-fixed-cur-position *source*)))
                 (t (error "unknown token type for read chars (see func doc): ~a" type)))
              *source*))))))

(defun make-delimiter-token (ch)
  (push-token-to-buffer
   (new-token
    :type :delimiter
    :value (terminal-to :sym :delimiter ch)
    :position (get-fixed-cur-position *source*))
   *source*))

(defun do-char (ch)
  (cond
    ((get-ls-state :comment)
     (update-pos ch *source*)
     (when (and (eq ch #\Newline)
                (not (get-ls-state :multi-comment)))
       (switch-ls-state :comment))
     (when (and (eq ch #\*)
                (get-ls-state :multi-comment))
       (read-chars ch
                   :multi-comment-end)))
    ((eq ch #\/)
     (update-pos ch *source*)
     (read-chars ch
                 :comment)
     (unless (get-ls-state :comment)
       (raise-lexer-error :on-char ch)))
    ((is-delimiter-? ch)
     ;; :delimite
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
     ;; :char-string
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (switch-ls-state :string)
     (read-chars ch
                 :char-string)
     (when (get-ls-state :string)
       (raise-lexer-error :on-string)))
    ((is-white-space-char-? ch)
     (update-pos ch *source*))
    (t (raise-lexer-error :on-char ch))))

;; util for with-disassembled-source macros
(defun get-tokens-source ()
  *tokens-source*)
