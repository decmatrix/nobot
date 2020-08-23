(uiop:define-package :nobot/botscript/lexer
    (:use :cl
          :anaphora
          :alexandria
          :nobot/botscript/nodes
          :nobot/botscript/lexer-utils)
  (:import-from :nobot/botscript/types
                #:use-token-type-class)
  (:export #:disassemble-source
           #:disassemble-string
           #:disassemble-file))

(in-package :nobot/botscript/lexer)

(use-token-type-class :botscript-token-types)

(defun disassemble-source (source &key (type :file)
                                    convert-tokens
                                    convert-with-pos
                                    use-lazy-tokens
                                    return-instance)
  (unless source
    (error "Expected source"))
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



(defmacro read-chars (prev-char type)
  ":id, :keyword, :num-string"
  (with-gensyms (ch word)
    `(progn
       (clear-chars-buffer *source*)
       (push-char-to-buffer ,prev-char *source*)
       (block in-read-chars-loop
         (loop for ,ch = (next-char *source*)
            while ,ch
            do  (if ,(case type
                       ((:id :keyword)
                        `(or (alphanumericp ,ch)
                             (eq ,ch #\-)))
                       (:num-string
                        `(digit-char-p ,ch)))
                    (progn
                      (update-pos ,ch *source*)
                      (push-char-to-buffer ,ch *source*))
                    (progn
                      (undo-next-char ,ch *source*)
                      (return-from in-read-chars-loop)))))
       (let ((,word (string-upcase (concatenate 'string (get-chars-buffer *source*)))))
         (push-token-to-buffer
          ,(case type
             (:id
              `(new-token
                :type :id
                :value ,word
                :position (get-fixed-cur-position *source*)))
             (:keyword
              `(new-token
                :type (if (is-keyword-? ,word)
                          :keyword
                          :unknown)
                :value (intern ,word :cl-user)
                :position (get-fixed-cur-position *source*)))
             (:num-string
              `(new-token
                :type :number-string
                :value (parse-integer ,word)
                :position (get-fixed-cur-position *source*))))
          *source*)))))

(defun do-char (ch)
  (cond
    ((is-keyword-char-? ch)
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :keyword))
    ((alpha-char-p ch)
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :id))
    ((digit-char-p ch)
     (update-pos ch *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :num-string))
    ((eq ch #\newline)
     (update-pos ch *source*)) 
    ((is-white-space-char-? ch)
     (update-pos ch *source*))
    (t
     (error "Unknown symbol [~A] in pos ~A ~A~%"
            ch
            (1+ (get-position-x *source*))
            (get-position-y *source*)))))
