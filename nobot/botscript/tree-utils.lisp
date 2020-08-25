(uiop:define-package :nobot/botscript/tree-utils
    (:use :cl
          :anaphora)
  (:import-from :nobot/utils
                #:defcontextvar
                #:setf-context-var
                #:equals)
  (:import-from :nobot/botscript/nodes
                #:token-node
                #:get-token-type
                #:value-of-token)
  (:export #:with-tree
           #:@revert-current-tree
           #:@revert-new-tree
           #:@insert-new-tree
           #:@is-token-of-value
           #:@is-token-of-type
           #:make-tree-from-token))

(in-package :nobot/botscript/tree-utils)

(defcontextvar *current-tree*)
(defcontextvar *current-sort-type*)
(defcontextvar *new-tree*)

(defgeneric make-tree-from-token (obj))

(defmacro with-tree ((&optional sort-type) &body body)
  `(multiple-value-bind (res-body new-tree)
       (let* ((*current-sort-type* ',sort-type)
              (*new-tree* (when *current-sort-type*
                               (list *current-sort-type*)))
              (*current-tree* *new-tree*))
         (values (progn ,@body) *current-tree*))
     (when ',sort-type
       (@insert-new-tree new-tree))
     (if ',sort-type
         res-body
         new-tree)))

(defun @revert-current-tree ()
  (setf-context-var *current-tree* nil))

(defun @revert-new-tree ()
  (setf-context-var *current-tree* *new-tree*))

(defun @insert-new-tree (new-tree)
  (labels ((%build-new-tree (tree)
             (when tree
               (let ((root (car tree)))
                 (if (eq root *current-sort-type*)
                     (aif (cdr tree)
                          (cons root (append it (list new-tree)))
                          (list root new-tree))
                     (cons root (mapcar #'%build-new-tree (cdr tree))))))))
    (setf-context-var *current-tree* (if *current-tree*
                                         (%build-new-tree *current-tree*)
                                         new-tree))))

(defmethod make-tree-from-token ((obj token-node))
  (list (get-token-type obj) (value-of-token obj)))
