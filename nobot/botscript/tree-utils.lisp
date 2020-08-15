(uiop:define-package :nobot/botscript/tree-utils
    (:use :cl
          :anaphora)
  (:import-from :nobot/utils
                #:define-context-var
                #:setf-context-var)
  (:import-from :nobot/botscript/types
                #:get-sort-symbol)
  (:export #:with-tree
           #:@revert-tree
           #:@revert-new-tree
           #:@insert-new-tree))

(in-package :nobot/botscript/tree-utils)

(define-context-var *current-tree*)
(define-context-var *current-sort-type*)
(define-context-var *new-tree*)

(defmacro with-tree ((sort-type) &body body)
  `(multiple-value-bind (res-body new-tree)
       (let* ((*current-sort-type*
               (get-sort-symbol ',sort-type))
              (*new-tree* (when *current-sort-type*
                               (list *current-sort-type*)))
              (*current-tree* *new-tree*))
         (values (progn ,@body) *current-tree*))
     (when ',sort-type
       (@insert-new-tree new-tree))
     res-body))

(defun @revert-tree ()
  (setf-context-var *current-tree* nil))

(defun @revert-new-tree ()
  (setf-context-var *new-tree* nil))

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
