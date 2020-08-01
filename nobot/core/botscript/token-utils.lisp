(uiop:define-package :nobot/core/botscript/token-utils
    (:use :cl
          :anaphora
          :alexandria
          :nobot/core/utils 
          :nobot/core/botscript/nodes)
  (:export #:convert-tokens
           #:convert-token
           #:same-tokens-?))

(in-package :nobot/core/botscript/token-utils)


(defgeneric convert-tokens (obj &key with-pos re-cached lazy))
(defgeneric convert-token (obj &key with-pos))
(defgeneric same-tokens-? (obj1 obj2 &key with-pos))

(defmethod convert-token ((obj from-tokens-source-node) &key (with-pos t) re-cached lazy)
  (aif (and (not re-cached) (get-converted-tokens-seq obj))
       it
       (let ((func (curry #'apply
                          (compose #'convert-token obj)
                          (list :with-pos with-pos))))
         (mapcar (if lazy
                     (lambda (token)
                       (lazy!
                         (funcall func token)))
                     func)
                 (get-tokens-seq obj)))))

(defmethod convert-token ((obj token-node) &key with-pos)
  (let ((converted-token (list (get-token-type obj)
                               (value-of-token obj))))
    (if with-pos
        (append converted-token
                (get-position obj))
        converted-token)))

(defmethod same-tokens-? ((obj1 token-node) (obj2 token-node) &key (with-pos t))
  (and (eq (get-token-type obj1)
           (get-token-type obj2))
       (equals
        (value-of-token obj1)
        (value-of-token obj2))
       (and
        with-pos
        (equals
         (get-position obj1)
         (get-position obj2)))))


