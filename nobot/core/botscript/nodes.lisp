(uiop:define-package :nobot/core/botscript/nodes
    (:use :cl
          :anaphora
          :alexandria)
  (:export #:*source*
           #:with-source-code
           #:new-token
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
           #:update-pos-x
           #:update-pos-y
           #:fix-cur-position
           #:clear-char-buffer
           #:push-char-to-buffer
           #:push-token-to-bufferы
           #:get-cur-position
           #:get-char-buffer
           #:get-token-buffer
           ;; from file source node API
           #:get-file-stream
           ;; from string source node API
           #:get-cur-index
           ;; from file and string source node common API
           #:next-char
           ;; nodes
           #:token-node
           #:from-tokens-source-node))


;;TODO optimize close, accsessor divide to reader and writeser if as need
(in-package :nobot/core/botscript/nodes)

(defclass from-source-node ()
  ((source
    :initarg :source
    :accessor get-source)
   (type
    :initarg :type
    :accessor get-source-type)))

(defclass from-tokens-source-node (from-source-node)
  ((token-seq
    :initform nil
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
    :initform 1
    :accessor get-position-x)
   (pos-y
    :initform 1
    :accessor get-position-y)
   (fixed-cur-pos
    :initform nil
    :accessor get-fixed-cur-pos)
   (token-buff
    :initform nil
    :accessor get-token-buffer)
   (char-buffer
    :initform nil
    :accessor get-char-buffer)))

(defclass from-file-source-node (from-source-code-node)
  ((fstream
    :initarg :fstream
    :accessor get-file-stream)))

(defclass from-string-source-node (from-source-code-node)
  ((cur-index
    :initform 0
    :accessor get-cur-index)))


(defparameter *source* nil)

(defmacro with-source-code ((type source) &body body)
  `(let* ((*source* (case ,type
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
     (get-token-buffer *source*)))


;;TODO: optimize methods and withput using *source* 
(defgeneric fix-cur-position (obj))
(defgeneric clear-char-buffer (obj))
(defgeneric update-pos-x (obj))
(defgeneric update-pos-y (obj))
(defgeneric push-char-to-buffer (ch obj))
(defgeneric push-token-to-buffer (token obj))
(defgeneric get-cur-position (obj))
(defgeneric next-char (obj))


(defmethod fix-cur-position ((obj from-source-code-node))
  (setf (get-fixed-cur-pos obj)
        (cons (get-position-x obj)
              (get-position-y obj))))

(defmethod clear-char-buffer ((obj from-source-code-node))
  (setf (get-char-buffer obj) nil))

(defmethod update-pos-x ((obj from-source-code-node))
  (incf (get-position-x obj)))

(defmethod update-pos-y ((obj from-source-code-node))
  (incf (get-position-y obj)))

(defmethod push-char-to-buffer (ch (obj from-source-code-node))
  (setf (get-char-buffer obj)
        (append (get-char-buffer obj) (list ch)))) ;; TODO: maybe nconc ?

(defmethod push-token-to-buffer (token (obj from-source-code-node))
  (setf (get-token-buffer obj)
        (append (get-token-buffer obj) (list token)))) ;; TODO: maybe nconc ?

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

(defun new-token (&rest args)
  (apply (curry #'make-instance 'token-node)
         args))
