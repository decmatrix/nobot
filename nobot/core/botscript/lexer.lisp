(uiop:define-package :nobot/core/botscript/lexer
    (:use :cl
          :anaphora
          :alexandria
          :nobot/core/botscript/nodes)
  (:export :disassemble-source))

(in-package :nobot/core/botscript/lexer)

(defun disassemble-source (source &key (type :file))
  (unless source
    (error "Expected source"))
  (with-source ((:type type)
                (:source source))
    (do-char it)))

(defun do-char (ch)
  (cond
    ((is-keyword-char-? ch)
     (update-pos-x)
     (fix-cur-pos)
     (read-chars ch
                 :keyword))
    ((alpha-char-p ch)
     (update-pos-x)
     (fix-cur-pos)
     (read-chars ch
                 :id))
    ((digit-char-p ch)
     (update-pos-x)
     (fix-cur-pos)
     (read-chars ch
                 :num-string))
    ((eq ch #\newline)
     (update-pos-y)) 
    ((is-white-space-char-? ch)
     (update-pos-x))
    (t
     (error "Unknown symbol [~A] in ~A in pos ~A ~A~%"
            ch
            *path-to-file*
            (1+ *pos-x*)
            *pos-y*))))


;;TODO: do benchmark with this macros
(defmacro read-chars (prev-char type)
  ":id, :keyword, :num-string"
  (with-gensyms (ch word)
    `(clear-buffer)
    `(push-char-to-buffer ,prev-char)
    `(loop for (if ,ch = (read-char *fstream* nil 'eof))
        if (eq ,ch #\newline) do (update-pos-y)
        else do (update-pos-x)  
        while (and ,ch
                   ,(case type
                      (:id :keyword
                           `(or
                             (alphanumericp ,ch)
                             (eq ,ch #\-)))
                      (:num-string
                       `(digit-char-p ,ch))))
        do (push-char-to-buffer ,ch))
    `(let ((,word (string-upcase (concatenate 'string *buffer*))))
       (push-token-to-buffer
        ,(case type
           (:id
            `(list '<id> (intern ,word :botscript)))
           (:keyword
            `(list (if (is-keyword-? ,word)
                       '<keyword>
                       '<unknown>)
                   (intern ,word :botscript)))
           (:num-string
            `(list '<number-string> (parse-integer ,word))))))))



(defun is-keyword-char-? (ch)
  (find ch "#!@$"))

(defun is-white-space-char-? (ch)
  (some (curry #'eq ch)
        '(#\space #\Tab #\newline #\Backspace #\Return #\Linefeed #\Page)))

(defun is-keyword-? (word)
  (some (curry #'eq word)
        '(|#EXE| !use $combo @def)))
