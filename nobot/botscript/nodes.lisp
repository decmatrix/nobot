(uiop:define-package :nobot/botscript/nodes
    (:use :cl
          :anaphora
          :alexandria)
  (:import-from :nobot/utils
                #:defcontextvar)
  (:export #:new-token
           #:make-tokens-source
           ;; from source node API
           #:get-source
           #:get-source-type
           ;; from tokens source node API
           #:get-tokens-seq
           #:set-tokens-seq
           #:set-converted-tokens-seq
           #:get-converted-tokens-seq
           ;; token node API
           #:get-token-type
           #:value-of-token
           #:get-position
           ;; from source code node API
           #:get-position-x
           #:get-position-y
           #:update-pos
           #:undo-update-pos
           #:fix-cur-position
           #:clear-chars-buffer
           #:push-char-to-buffer
           #:push-token-to-buffer
           #:get-cur-position
           #:get-fixed-cur-position
           #:get-chars-buffer
           #:get-tokens-buffer
           ;; from file source node API
           #:get-file-stream
           ;; from string source node API
           #:get-cur-index
           ;; from file and string source node common API
           #:next-char
           #:undo-next-char
           ;; token pointer API
           #:make-token-pointer
           #:reset-pointer
           #:get-next-token
           ;; nodes
           #:token-node
           #:from-source-node
           #:from-tokens-source-node
           #:from-source-code-node
           #:from-file-source-node
           #:from-string-source-node))


(in-package :nobot/botscript/nodes)

(defclass from-source-node ()
  ((source
    :initarg :source
    :accessor get-source)
   (type
    :initarg :type
    :accessor get-source-type)))

(defclass from-tokens-source-node (from-source-node)
  ((tokens-seq
    :initarg :tokens-seq
    :reader get-tokens-seq
    :writer set-tokens-seq)
   (converted-token-seq
    :initform nil
    :writer set-converted-tokens-seq
    :reader get-converted-tokens-seq)))

(defclass token-node ()
  ((type
    :initarg :type
    :accessor get-token-type)
   (value
    :initarg :value
    :accessor value-of-token)
   (position
    :initarg :position
    :accessor get-position)))

(defclass from-source-code-node (from-source-node)
  ((pos-x
    :initform 0
    :accessor get-position-x)
   (pos-y
    :initform 1
    :accessor get-position-y)
   (fixed-cur-pos
    :initform nil
    :accessor get-fixed-cur-position)
   (token-buff
    :initform nil
    :accessor get-tokens-buffer)
   (chars-buffer
    :initform nil
    :accessor get-chars-buffer)))

(defclass from-file-source-node (from-source-code-node)
  ((fstream
    :initarg :fstream
    :accessor get-file-stream)))

(defclass from-string-source-node (from-source-code-node)
  ((cur-index
    :initform 0
    :accessor get-cur-index)))

(defclass token-pointer ()
  ((pointer
    :initform -1
    :accessor get-index)
   (tokens-seq
    :initarg :tokens-seq
    :accessor get-tokens-seq)
   (toknes-seq-limit
    :initarg :token-seq-limit
    :accessor get-limit)))


;; for lexer
(defgeneric fix-cur-position (obj))
(defgeneric clear-chars-buffer (obj))
(defgeneric update-pos (ch obj))
(defgeneric undo-update-pos (ch obj))
(defgeneric push-char-to-buffer (ch obj))
(defgeneric push-token-to-buffer (token obj))
(defgeneric get-cur-position (obj))
(defgeneric next-char (obj))
(defgeneric undo-next-char (ch obj))

;; for parser
(defgeneric get-next-token (obj))
(defgeneric reset-pointer (obj))
(defgeneric make-token-pointer (obj))

;; for lexer
(defmethod fix-cur-position ((obj from-source-code-node))
  (setf (get-fixed-cur-position obj)
        (cons (get-position-x obj)
              (get-position-y obj))))

(defmethod clear-chars-buffer ((obj from-source-code-node))
  (setf (get-chars-buffer obj) nil))

(defmethod update-pos (ch (obj from-source-code-node))
  (if (eq ch #\newline)
      (incf (get-position-y obj))
      (incf (get-position-x obj))))

(defmethod undo-update-pos (ch (obj from-source-code-node))
  (if (eq ch #\newline)
      (decf (get-position-y obj))
      (decf (get-position-x obj))))

(defmethod push-char-to-buffer (ch (obj from-source-code-node))
  (setf (get-chars-buffer obj)
        (nconc (get-chars-buffer obj) (list ch))))

(defmethod push-token-to-buffer (token (obj from-source-code-node))
  (setf (get-tokens-buffer obj)
        (nconc (get-tokens-buffer obj) (list token))))

(defmethod get-cur-position ((obj from-source-code-node))
  (values (get-position-x obj)
          (get-position-y obj)))

(defmethod next-char ((obj from-file-source-node))
  (read-char (get-file-stream obj) nil nil))

(defmethod next-char ((obj from-string-source-node))
  (let ((idx (get-cur-index obj))
        (source-str (get-source obj)))
    (unless (eql idx (length source-str))
      (incf (get-cur-index obj))
      (aref source-str idx))))

(defmethod undo-next-char :before (ch (obj from-source-code-node))
  (undo-update-pos ch obj))

(defmethod undo-next-char (ch (obj from-file-source-node))
  (unread-char ch (get-file-stream obj)))

(defmethod undo-next-char (ch (obj from-string-source-node))
  (decf (get-cur-index obj)))


;; for parser
(defmethod get-next-token ((pointer token-pointer))
  (let ((idx (get-index pointer)))
    (unless (eql idx (get-limit pointer))
      (nth idx (get-tokens-seq pointer)))))

(defmethod reset-pointer ((pointer token-pointer))
  (setf (get-index pointer) 0))

(defmethod make-token-pointer ((obj from-tokens-source-node))
  (let ((tokens-seq (get-tokens-seq obj)))
    (make-instance 'token-pointer
                   :tokens-seq tokens-seq
                   :token-seq-limit (length tokens-seq))))
