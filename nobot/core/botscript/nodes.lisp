(uiop:define-package :botscript/nodes
    (:use :cl
          :anaphora
          :alexandria)
  (:export #:*source*
           #:with-source
           #:fix-cur-position
           #:clear-char-buffer
           #:update-pos-x
           #:update-pos-y
           #:get-position-x
           #:get-position-y
           #:push-char-to-buffer
           #:push-token-to-buffer
           #:get-cur-position
           #:get-source
           #:get-char-buffer
           #:get-file-stream
           #:next-char))

(in-package :botscript/nodes)

(defclass from-source-node ()
  ((pos-x
    :initform 1
    :accessor get-pos-x)
   (pos-y
    :initform 1
    :accessor get-pos-y)
   (fixed-cur-pos
    :initform nil
    :accessor get-fixed-cur-pos)
   (token-buff
    :initform nil
    :accessor get-token-buff)
   (char-buffer
    :initform nil
    :accessor get-char-buff)))

(defclass from-file-source-node (from-source-node)
  ((fstream
    :initarg :init-fstream
    :accessor get-fstream)
   (path
    :initarg :init-path
    :accessor get-path)))

(defclass from-string-source-node (from-source-node)
  ((string-source-sstring
    :initarg :init-source-string
    :accessor get-source-string)
   (cur-index
    :initform 0
    :accessor get-cur-index)))



(defparameter *source* nil)

(defmacro with-source ((type source) &body body)
  `(let* ((*source* (case ,type
                      (:file
                       (make-instance 'from-file-source-node
                                      :init-fstream (open ,source
                                                          :direction :input
                                                          :if-does-not-exist :error)
                                      :path ,source))
                      (:string
                       (make-instance 'from-string-source-node
                                      :init-source-string ,source))
                      (t (error "Unknown type ~A" ,type)))))
     (loop for it = (next-char *source*)
        while it
        do (progn
             ,@body))
     ,(when (eq type :file)
        '(close (get-fstream *source*)))
     (get-token-buff *source*)))


(defgeneric fix-cur-position (obj))
(defgeneric clear-char-buffer (obj))
(defgeneric update-pox-x (obj))
(defgeneric update-pos-y (obj))
(defgeneric get-position-x (obj))
(defgeneric get-position-y (obj))
(defgeneric push-char-to-buffer (ch obj))
(defgeneric push-token-to-buffer (token obj))
(defgeneric get-char-buffer (obj))
(defgeneric get-cur-position (obj))
(defgeneric get-source (obj))
(defgeneric get-file-stream (obj))
(defgeneric next-char (obj))

(defmethod fix-cur-position ((obj from-source-node))
  (setf (get-fixed-cur-pos obj)
        (cons (get-pos-x obj)
              (get-pos-y obj))))

(defmethod clear-char-buffer ((obj from-source-node))
  (setf (get-char-buff obj) nil))

(defmethod update-pos-x ((obj from-source-node))
  (incf (get-pos-x obj)))

(defmethod update-pos-y ((obj from-source-node))
  (incf (get-pos-y obj)))

(defmethod get-position-x ((obj from-source-node))
  (get-pos-x obj))

(defmethod get-position-y ((obj from-source-node))
  (get-pos-y obj))

(defmethod push-char-to-buffer (ch (obj from-source-node))
  (setf (get-char-buff obj)
        (append (get-char-buff obj) (list ch)))) ;; TODO: maybe nconc ?

(defmethod push-token-to-buffer (token (obj from-source-node))
  (setf (get-token-buff obj)
        (append (get-token-buff obj) (list token)))) ;; TODO: maybe nconc ?

(defmethod get-cur-position ((obj from-source-node))
  (values (get-pos-x obj)
          (get-pos-y obj)))

(defmethod get-source ((obj from-file-source-node))
  (get-path obj))

(defmethod get-source ((obj from-string-source-node))
  (get-source-string obj))

(defmethod get-file-stream ((obj from-file-source-node))
  (get-fstream obj))

(defmethod next-char ((obj from-file-source-node))
  (read-char (get-fstream obj) nil nil))

(defmethod next-char ((obj from-string-source-node))
  (awhen (aref (get-source-string obj) (get-cur-index obj))
    (incf (get-cur-index obj))
    it))

(defmethod next-char ((obj from-string-source-node))
  (let ((idx (get-cur-index obj))
        (source-str (get-source-string obj)))
    (unless (eql idx (length source-str))
      (incf (get-cur-index obj))
      (aref source-str idx))))

(defmethod get-char-buffer ((obj from-source-node))
  (get-char-buff obj))

