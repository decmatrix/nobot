(uiop:define-package :nobot/botscript/parser-utils
    (:use :cl
          :alexandria
          :nobot/utils
          :nobot/botscript/tree-utils
          :nobot/botscript/lexer
          :nobot/botscript/nodes
          :nobot/botscript/types)
  (:import-from :nobot/utils
                #:define-context-var)
  (:export #:with-disassembled-source
           #:defun-state
           #:with-state-context
           #:@goto
           #:@next-token))

(in-package :nobot/botscript/parser-utils)

(defcontextvar *tokens-source*)
(defcontextvar *token-pointer*)
(defcontextvar *state-table*)

;;TODO optimize imports
(defmacro with-disassembled-source ((source type) &body body)
  `(let* ((*tokens-source*
          (disassemble-source ,source
                              :type ,type
                              :return-instance t)))
     ,@body))

(defmacro defun-state (sort-type (&rest args) &body body)
  (let ((state-fun-name (intern (concatenate 'string "@" (string sort-type)))))
    `(prog1
         (defun ,state-fun-name ,args
              (with-tree (,(get-sort-symbol sort-type))
                ,@body))
       (setf (gethash ',sort-type *state-table*)
             (function ,state-fun-name)))))

(defmacro with-state-context ((sort-type-class entry-state &optional tokens-source) &body body)
  (use-sort-type-class sort-type-class)
  `(let ((*state-table* (make-hash-table :test #'eq))
         (*token-pointer* (make-token-pointer (or ,tokens-source *tokens-source*))))
     ,@body
     (@goto ,entry-state)))

(defmacro @goto (state-fun-name &rest args)
  `(apply (gethash ',state-fun-name *state-table*) ,args))

(defun @next-token ()
  (get-next-token *token-pointer*))
