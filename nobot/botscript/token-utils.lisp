(uiop:define-package :nobot/botscript/token-utils
    (:use :cl
          :anaphora
          :alexandria
          :nobot/utils
          :nobot/collections
          :nobot/botscript/nodes)
  (:import-from :nobot/botscript/types
                #:get-token-type-symbol)
  (:export
   ;; common token utils API
   #:convert-tokens
   #:convert-token
   #:same-tokens-?
   #:same-tokens-seq-?
   #:make-tokens-source
   #:new-token
   ;; token pointer API
   #:make-token-pointer
   #:mv-ptr-to-prev-token
   #:mv-ptr-to-next-token
   #:get-next-token
   #:get-prev-token
   #:get-prev-token
   #:get-curr-token
   #:reset-pointer
   #:ptr-is-out-of-bound-?))

(in-package :nobot/botscript/token-utils)

;; main utils API
(defgeneric convert-tokens (obj &key with-pos re-cached lazy))
(defgeneric convert-token (obj &key with-pos))
(defgeneric same-tokens-? (obj1 obj2 &key without-pos without-value without-type))
(defgeneric same-tokens-seq-? (obj1 obj2 &key without-pos without-value without-type))
(defgeneric make-tokens-source (obj))

;; token pointer utils API
(defgeneric make-token-pointer (obj))
(defgeneric mv-ptr-to-prev-token (obj))
(defgeneric mv-ptr-to-next-token (obj))
(defgeneric ptr-is-out-of-bound-? (obj))
(defgeneric get-next-token (obj))
(defgeneric get-prev-token (obj))
(defgeneric get-curr-token (obj))
(defgeneric reset-pointer (obj))


;; maker API
(defun new-token (&rest args)
  (let* ((copy-args (copy-list args))
         (token-type (prog1
                         (getf copy-args :type)
                       (remf copy-args :type))))
    (apply (curry #'make-instance 'token-node)
           (nconc (list :type (get-token-type-symbol token-type))
                  copy-args))))


(defmethod make-token-pointer ((obj from-tokens-source-node))
  (let ((tokens-seq (get-tokens-seq obj)))
    (make-instance 'token-pointer
                   :tokens-seq tokens-seq
                   :token-seq-limit (length tokens-seq))))


;; token utils API impl
(defmethod convert-tokens ((obj from-tokens-source-node) &key (with-pos t) re-cached lazy)
  (aif (and (not re-cached) (get-converted-tokens-seq obj))
       it
       (set-tokens-seq
        (mapcar (if lazy
                    (lambda (token)
                      (lazy! ("token-node")
                        (convert-token token
                                       :with-pos with-pos)))
                    (lambda (token)
                      (convert-token token
                                      :with-pos with-pos)))
                (get-tokens-seq obj))
        obj)))

(defmethod convert-token ((obj token-node) &key with-pos)
  (let ((converted-token (list (get-token-type obj)
                               (value-of-token obj))))
    (if with-pos
        (append converted-token
                (list (get-position obj)))
        converted-token)))

(defmethod same-tokens-? ((obj1 token-node) (obj2 token-node) &key
                                                                without-pos
                                                                without-value
                                                                without-type)
  (and (if without-type
           t
           (eq (get-token-type obj1)
               (get-token-type obj2)))
       (if without-value
           t
           (equals
            (value-of-token obj1)
            (value-of-token obj2)))
       (if without-pos
           t
           (equals
            (get-position obj1)
            (get-position obj2)))))

(defmethod same-tokens-? ((obj1 list) (obj2 list) &key
                                                    without-pos
                                                    without-value
                                                    without-type)
  (let ((len (if without-pos
                 2
                 3)))
    (and (if without-pos
             t
             (and (eql (length obj1) len)
                  (eql (length obj2) len)))
         (let ((type-of-token-1 (first obj1))
               (type-of-token-2 (first obj2))
               (value-of-token-1 (second obj1))
               (value-of-token-2 (second obj2)))
           (and (if without-type
                    t
                    (equal (symbol-name type-of-token-1)
                            (symbol-name type-of-token-2)))
                (if without-value
                    t
                    (cond ((and (typep value-of-token-1 'symbol)
                                (typep value-of-token-2 'symbol))
                           (equals (string-upcase (symbol-name value-of-token-1))
                                   (string-upcase (symbol-name value-of-token-2))))
                          ((and (typep value-of-token-1 'string)
                                (typep value-of-token-2 'string))
                           (equals (string-upcase value-of-token-1)
                                   (string-upcase value-of-token-2)))
                          (t (equals value-of-token-1 value-of-token-2))))
                (if without-pos
                    t
                    (equals (third obj1) (third obj2))))))))

(defmethod same-tokens-seq-? ((obj1 list) (obj2 list) &key
                                                        without-pos
                                                        without-value
                                                        without-type)
  (and
   (eql (length obj1)
        (length obj2))
   (every (lambda (token-1 token-2)
            (and (eq (type-of token-1)
                     (type-of token-2))
                 (same-tokens-? token-1 token-2
                                :without-pos without-pos
                                :without-value without-value
                                :without-type without-type)))
          obj1
          obj2)))

(defmethod same-tokens-seq-? ((obj1 from-tokens-source-node)
                              (obj2 from-tokens-source-node)
                              &key
                                without-pos
                                without-value
                                without-type)
  (same-tokens-seq-? (get-tokens-seq obj1) (get-tokens-seq obj2)
                     :without-pos without-pos
                     :without-value without-value
                     :without-type without-type))

(defmethod make-tokens-source ((obj from-source-code-node))
  (make-instance 'from-tokens-source-node
                 :source (get-source obj)
                 :type (get-source-type obj)
                 :tokens-seq (get-tokens-buffer obj)))


;; token pointer API impl
(defmethod mv-ptr-to-next-token ((pointer token-pointer))
  (incf (get-index pointer)))

(defmethod mv-ptr-to-prev-token ((pointer token-pointer))
  (decf (get-index pointer)))

(defmethod ptr-is-out-of-bound-? ((pointer token-pointer))
  (let ((idx (get-index pointer))
        (limit (get-limit pointer)))
    (or (>= idx limit)
        (<= idx -1))))

(defmethod get-next-token ((pointer token-pointer))
  (let ((idx (mv-ptr-to-next-token pointer)))
    (unless (ptr-is-out-of-bound-? pointer)
      (nth idx (get-tokens-seq pointer)))))

(defmethod get-prev-token ((pointer token-pointer))
  (let ((idx (mv-ptr-to-prev-token pointer)))
    (unless (ptr-is-out-of-bound-? pointer)
      (nth idx (get-tokens-seq pointer)))))

(defmethod get-cur-index ((pointer token-pointer))
  (when (ptr-is-out-of-bound-? pointer)
    (nth (get-index pointer) (get-tokens-seq pointer))))

(defmethod reset-pointer ((pointer token-pointer))
  (setf (get-index pointer) 0))
