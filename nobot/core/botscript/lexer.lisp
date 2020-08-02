(uiop:define-package :nobot/core/botscript/lexer
    (:use :cl
          :anaphora
          :alexandria
          :nobot/core/botscript/nodes)
  (:export :disassemble-source))

(in-package :nobot/core/botscript/lexer)

;;TODO optimize for new-token
(defparameter *non-terminals* (make-hash-table :test #'eq))
(setf (gethash :keyword  *non-terminals*) (intern "<KEYWORD>" :cl-user))
(setf (gethash :id *non-terminals*) (intern "<ID>" :cl-user))
(setf (gethash :number-string *non-terminals*) (intern "<NUMBER-STRING>" :cl-user))
(setf (gethash :unknown *non-terminals*) (intern "<UNKNOWN>" :cl-user))

(defun disassemble-source (source &key (type :file)
                                    convert-tokens
                                    convert-with-pos
                                    convert-to-lazy-tokens
                                    return-instance)
  (unless source
    (error "Expected source"))
  (with-source-code (type source
                          :convert-tokens convert-tokens
                          :convert-with-pos convert-with-pos
                          :convert-to-lazy-tokens convert-to-lazy-tokens
                          :return-instance return-instance)
    (loop for ch = (next-char *source*)
       while ch
       do (do-char ch))))

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
       (clear-chars-buffer *source*)
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
       (let ((,word (string-upcase (concatenate 'string (get-chars-buffer *source*)))))
         (push-token-to-buffer
          ;;TODO: optimize it
          ,(case type
             (:id
              `(new-token
                :type (get-non-terminal :id)
                :value ,word
                :position (get-cur-position *source*)))
             (:keyword
              `(new-token
                :type (if (is-keyword-? ,word)
                          (get-non-terminal :keyword)
                          (get-non-terminal :unknown))
                :value (intern ,word :cl-user)
                :position (get-cur-position *source*)))
             (:num-string
              `(new-token
                :type (get-non-terminal :number-string)
                :value (parse-integer ,word)
                :position (get-cur-position *source*))))
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
