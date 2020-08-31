(uiop:define-package :nobot/botscript/parser-utils
    (:use :cl
          :alexandria
          :nobot/utils
          :nobot/botscript/tree-utils
          :nobot/botscript/token-utils
          :nobot/botscript/lexer
          :nobot/botscript/nodes
          :nobot/botscript/types)
  (:import-from :nobot/utils
                #:define-context-var)
  (:export #:with-disassembled-source
           #:defun-state
           #:with-state-context-run
           #:@goto
           #:@next-token
           #:@prev-token
           #:@curr-token
           #:@put-back-token
           #:@is-token-of-type
           #:@is-token-of-value
           #:@revert-state-action
           #:get-tokens-source
           #:with-token))

(in-package :nobot/botscript/parser-utils)

(defcontextvar *tokens-source*)
(defcontextvar *token-pointer*)
(defcontextvar *state-table*)

(defgeneric make-parse-tree-source (tree obj))
(defgeneric @is-token-of-value (obj token-value))
(defgeneric @is-token-of-type (obj token-type))

(defmethod make-parse-tree-source ((tree list) (obj from-tokens-source-node))
  (make-instance 'from-parse-tree-source-node
                 :parse-tree tree
                 :source (get-source obj)
                 :type (get-source-type obj)))

;;TODO optimize imports
(defmacro with-disassembled-source ((source type) &body body)
  `(let* ((*tokens-source*
          (disassemble-source ,source
                              :type ,type
                              :return-instance t)))
     ,@body))

(defun get-tokens-source ()
  *tokens-source*)

(defmacro defun-state (sort-type (&rest args) &body body)
  (let ((state-fun-name (intern (concatenate 'string "@" (string sort-type)))))
    `(prog1
         (defun ,state-fun-name ,args
              (with-tree (,(get-sort-type-symbol sort-type))
                ,@body))
       (setf (gethash ',sort-type *state-table*)
             (function ,state-fun-name)))))

(defmacro with-state-context-run ((sort-type-class tokens-source
                                                   &key
                                                   entry-state
                                                   return-instance)
                                  &body body)
  (use-sort-type-class sort-type-class)
  `(let ((*state-table* (make-hash-table :test #'eq))
         (*token-pointer* (make-token-pointer ,tokens-source)))
     ,@body
     (let ((tree
            (with-tree ()
              (@goto ,(or entry-state
                          (car (get-sort-type-list)))))))
       (if ,return-instance
           (make-parse-tree-source tree ,tokens-source)
           tree))))

(defmacro @goto (state-fun-name &rest args)
  `(apply (gethash ',state-fun-name *state-table*) ,args))

(defun @next-token ()
  (get-next-token *token-pointer*))

(defun @prev-token ()
  (get-prev-token *token-pointer*))

(defun @curr-token ()
  (get-curr-token *token-pointer*))

(defun @put-back-token ()
  (mv-ptr-to-prev-token *token-pointer*))

(defun @revert-state-action ()
  (@revert-tree)
  (@put-back-token)
  nil)

(defmethod @is-token-of-value ((obj token-node) token-value)
  (when (equals (value-of-token obj)
                token-value)
    (@insert-new-tree (make-tree-from-token obj))))

(defmethod @is-token-of-value ((obj (eql nil)) token-value)
  nil)

(defmethod @is-token-of-type ((obj token-node) token-type)
  (when (eq (get-token-type obj)
            (get-token-type-symbol (convert-type token-type)))
    (@insert-new-tree (make-tree-from-token obj))))

(defmethod @is-token-of-type ((obj (eql nil)) token-value)
  nil)

(defmacro with-token ((next/prev) &body body)
  "next or prev"
  `(let ((,(intern "IT")
          ,(case next/prev
             (:next
              '(@next-token))
             (:prev
              '(@prev-token))
             (t
              (error "Unknown type of token in with-token macros: ~A" next/prev)))))
     ,@body))
