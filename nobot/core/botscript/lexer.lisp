(uiop:define-package :botscript/lexer
    (:use :cl
          :anaphora
          :alexandria
          :botscript/nodes)
  (:export :disassemble-source))

(in-package :botscript/lexer)

(defparameter *non-terminals* (make-hash-table :test #'eq))
(setf (gethash :keyword  *non-terminals*) (intern "<KEYWORD>" :cl-user))
(setf (gethash :id *non-terminals*) (intern "<ID>" :cl-user))
(setf (gethash :number-string *non-terminals*) (intern "<NUMBER-STRING>" :cl-user))
(setf (gethash :unknown *non-terminals*) (intern "<UNKNOWN>" :cl-user))

(defun disassemble-source (source &key (type :file))
  (unless source
    (error "Expected source"))
  (with-source (type source)
    (do-char it)))

(defun do-char (ch)
  (cond
    ((is-keyword-char-? ch)
     (update-pos-x *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :keyword))
    ((alpha-char-p ch)
     (update-pos-x *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :id))
    ((digit-char-p ch)
     (update-pos-x *source*)
     (fix-cur-position *source*)
     (read-chars ch
                 :num-string))
    ((eq ch #\newline)
     (update-pos-y *source*)) 
    ((is-white-space-char-? ch)
     (update-pos-x *source*))
    (t
     (error "Unknown symbol [~A] in pos ~A ~A~%"
            ch
            (1+ (get-position-x *source*))
            (get-position-y *source*)))))


(defmacro read-chars (prev-char type)
  ":id, :keyword, :num-string"
  (with-gensyms (ch word)
    `(progn
       (clear-char-buffer *source*)
       (push-char-to-buffer ,prev-char *source*)
       (loop for ,ch = (next-char *source*)
          if (eq ,ch #\newline) do (update-pos-y *source*)
          else do (update-pos-x *source*)  
          while (and ,ch
                     ,(case type
                        ((:id :keyword)
                         `(or
                           (alphanumericp ,ch)
                           (eq ,ch #\-)))
                        (:num-string
                         `(digit-char-p ,ch))))
          do (push-char-to-buffer ,ch *source*))
       (let ((,word (string-upcase (concatenate 'string (get-char-buffer *source*)))))
         (push-token-to-buffer
          ,(case type
             (:id
              `(list (get-non-terminal :id) ,word))
             (:keyword
              `(list (if (is-keyword-? ,word)
                         (get-non-terminal :keyword)
                         (get-non-terminal :unknown))
                     (intern ,word :cl-user)))
             (:num-string
              `(list (get-non-terminal :number-string) (parse-integer ,word))))
          *source*)))))

(defun get-non-terminal (kword)
  (gethash kword *non-terminals*))

(defun is-keyword-char-? (ch)
  (find ch "#!@$"))

(defun is-white-space-char-? (ch)
  (some (curry #'eq ch)
        '(#\space #\Tab #\newline #\Backspace #\Return #\Linefeed #\Page)))

(defun is-keyword-? (word)
  (some (curry #'equal word)
        '("#EXE" "!USE" "$COMBO" "@DEF")))
