(uiop:define-package :nobot/botscript/parser-utils
    (:use :cl
          :alexandria
          :nobot/utils
          :nobot/botscript/tree-utils
          :nobot/botscript/token-utils
          :nobot/botscript/lexer
          :nobot/botscript/nodes
          :nobot/botscript/types)
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

(defvar *tokens-source*)
(defvar *token-pointer*)
(defvar *rule-table*)

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

(defmacro define-rule (sort-type () rule-lisp-form)
  (let ((rule-fun-name (intern (concatenate 'string "rule@" (string sort-type)))))
    `(prog1
         (defun ,rule-fun-name ()
           (with-tree (,(get-sort-type-symbol sort-type))
             ,(build-rule-body lisp-form)))
       (setf (gethash ',sort-type *rule-table*)
             (function ,rule-fun-name)))))

(defmacro build-rule-body-aux (lisp-form)
  `(build-rule-body ',lisp-form))

(defun build-rule-body (lisp-form)
  (let ((root (car lisp-form)))
    (case root
      (:and
       `(and
         ,@ (mapcar #'build-rule-body (cdr lisp-form))))
      (:or
       `(or
         ,@ (mapcar #'build-rule-body (cdr lisp-form))))
      (:rule
       `(@goto ,(second lisp-form)))
      (:empty)
      (:id)
      (:delimiter)
      (:string)
      (:num-string)
      (t (error "unknown rule  type: ~a" root)))))

(defmacro @goto (state-fun-name)
  `(apply (gethash ',state-fun-name *rule-table*)))



(defmacro build-body-of-rule-fun-from-lisp-form (lisp-form)
  (with-gensym (root)
    `(let ((,root (car lisp-form)))
       (case ,root
         (:and
          ())
         (:or)
         (:rule)
         (:empty)
         (:id)
         (:delimiter)
         (:string)
         (:num-string)
         (t (error "unknown rule type: ~a" ,root))))))

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
