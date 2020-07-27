(uiop:define-package :nobot/core/botscript/lexer
    (:use :cl
          :anaphora
          :alexandria
          :nobot/core/utils)
  (:export :parse-file))

(in-package :nobot/core/botscript/lexer)

;; TODO: too many lexical vars, maybe there use CLOS
(defparameter *pos-x* nil)
(defparameter *pos-y* nil)
(defparameter *fix-cur-pos* nil)
(defparameter *fstream* nil)
(defparameter *buffer* nil)
(defparameter *token-buff* nil)
(defparameter *path-to-file* nil)


(defmacro with-file-process ((fstream path-to-file) &body body)
  `(let ((*pos-x* 1)
         (*pos-y* 1)
         (*buffer* nil)
         (*token-buff* nil)
         (*fix-cur-pos* nil)
         (*path-to-file* path-to-file)
         (*fstream* ,fstream))
     ,@body))

(defun fix-cur-pos ()
  (setf *fix-cur-pos* (cons *pos-x* *pos-y*)))

(defun clear-buffer ()
  (setf *buffer* nil))

(defun update-pos-x ()
  (incf *pos-x*))

(defun update-pos-y ()
  (incf *pos-y*))

(defun push-char-to-buffer (ch)
  (setf *buffer* (append *buffer* (list ch)))) ;;nconc ?

(defun push-token-to-buffer (token)
  (setf *token-buff* (append *token-buff* (list token)))) ;; nconc ?

(defun parse-file (path-to-file)
  (unless path-to-file
    (error "Expected path to file"))
  (with-open-file (fstream path-to-file
                           :direction :input
                           :if-does-not-exist :error)
    (with-file-process (fstream path-to-file)
      (loop for ch = (read-char *fstream* nil 'eof)
         while (not (eq ch 'eof))
         do (cond
              ((is-keyword-char-? ch)
               (update-pos-x)
               (fix-cur-pos)
               (read-chars ch
                           :keyword))
              ((alpha-char-p)
               (update-pos-x)
               (fix-cur-pos)
               (read-chars ch
                           :id))
              ((digit-char-p)
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
                      *pos-y*)))))))

(defun is-keyword-char-? (ch)
  (find ch "#!@$"))

(defun is-white-space-char-? (ch)
  (some (curry #'eq ch)
        '(#\space #Tab #\newline #Backspace #\Return #\Linefeed #\Page)))

(defun is-keyword-? (word)
  (some (curry #'eq word)
        '(|#EXE| !use $combo @def)))

;;TODO: do benchmark with this macros
(defmacro read-chars (prev-char type &key before-char)
  ":id, :keyword, :num-string"
  (with-gensyms (ch word) ;; TODO: gensym macros no need, see alexandria lib
    `(clear-buffer)
    `(push-char-to-buffer ,prev-char)
    `(loop for ,ch = (read-char *fstream* nil 'eof)
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
