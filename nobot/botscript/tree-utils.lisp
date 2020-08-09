(uiop:define-package :nobot/botscript/tree-utils
    (:use :cl
          :anaphora
          :nobot/utils)
  (:export #:get-sort-symbol
           #:with-tree
           #:@revert-tree
           #:@revert-new-tree
           #:@insert-new-tree))

(in-package :nobot/botscript/tree-utils)

(defparameter *sort-types* (make-hash-table :test #'eq))

(define-context-var *current-tree*)
(define-context-var *current-sort-type*)
(define-context-var *new-tree*)

;; init leaf types
(mapcar
 (lambda (str)
   (let ((up-string (string-upcase str)))
     (setf (gethash (intern up-string :keyword) *sort-types*)
           (intern (set-<> up-string) :cl-user))))
 '("script"
   "macros-block"
   "predefined-block"
   "definition/combo-block"
   "graph-logic"
   "exe-macros"
   "use-predefined"
   "call-definition/combo"
   "call-definition"
   "call-combo"
   "predefines"
   "predefined-list"
   "args-list"
   "arg"))


(defun get-sort-symbol (type)
  (gethash type *sort-types*))

(defmacro with-tree ((sort-type) &body body)
  `(multiple-value-bind (res-body new-tree)
       (let* ((*current-sort-type*
               (get-sort-symbol ,sort-type))
              (*new-tree* (when *current-sort-type*
                               (list *current-sort-type*)))
              (*current-tree* *new-tree*))
         (values (progn ,@body) *current-tree*))
     (when ,sort-type
       (@insert-new-tree new-tree))))

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
    (if (or (is-outside-context-? *current-tree*)
            (is-outside-context-? *current-sort-type*))
        (tree-context-warn)
        (setf-context-var *current-tree* (if *current-tree*
                                 (%build-new-tree *current-tree*)
                                 new-tree)))))

(defun tree-context-warn ()
  (warn "Using tree tool function in outside 'with-tree' macros,
this operation was canceled"))

