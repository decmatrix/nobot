;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen/js/printer
    (:use :cl)
  (:import-from :anaphora
                #:aif
                #:it)
  (:import-from :nobot/utils
                #:to-string)
  (:export #:print-js-code-from-tree))

(in-package :nobot/codegen/js/printer)

(defparameter *indent-ch* #\Space)
(defparameter *indent-step* 2)
(defvar *stream*)
(defvar *indent* 0)

;;;;;;;;;;;;;;;;;;;;;; js code printer ;;;;;;;;;;;;;;;;;;;;;;
(defmacro print-js-code-from-tree ((stream) lisp-form)
  `(let ((*stream* ,stream))
     ;;(print ,lisp-form)
     (pprint-js-tree ,lisp-form)))

;;TODO check extra args
;;TODO add newlines
;;TODO add whitespaces
(defun pprint-js-tree (tree)
  (if (not (listp tree))
      tree
      (let ((root (car tree)))
        (case root
          (:js
           (mapc #'pprint-js-tree (cdr tree)))
          (:multi-comment
           (format *stream* "/* ~%~a */~%" (second tree)))
          (:import
           (format *stream* "import {~{~a~^, ~}} from \"~a\";~%"
                   (second tree)
                   (third tree)))
          (:const
           (format *stream* "const ~a = ~a;~%"
                   (second tree)
                   (let ((*stream* nil))
                     (pprint-js-tree (third tree)))))
          (:new-object
           (format *stream* "new ~a(~{~a~^, ~})"
                   (second tree)
                   (let ((*stream* nil))
                     (mapcar #'pprint-js-tree (cddr tree)))))
          (:object
           (format *stream* "{~{~a~^, ~}}"
                   (mapcar
                    (lambda (key-val)
                      (format nil "~a: ~a"
                              (car key-val)
                              (let ((*stream* nil))
                                (pprint-js-tree (second key-val)))))
                    (cdr tree))))
          (:stmt
           ;;TODO: TOOOOOOO BAD!
           (format *stream* "~a~a;~%"
                   (if (eq (car (second tree)) :if-stmt)
                       ""
                       (repeat-n *indent* *indent-ch*))
                   (let ((*stream* nil))
                     (pprint-js-tree (second tree)))))
          (:chain-expr
           (format *stream* "~a.~{~a~^~%.~}"
                   (second tree)
                   (let ((*stream* nil))
                     (mapcar #'pprint-js-tree (cddr tree)))))
          (:list
           (format *stream* "[~{~a~^, ~}]"
                   (let ((*stream* nil))
                     (mapcar #'pprint-js-tree (cdr tree)))))
          (:call-expr
           (format *stream* "~a(~{~a~^, ~})"
                   (second tree)
                   (let ((*stream* nil))
                     (mapcar #'pprint-js-tree (cddr tree)))))
          (:if-stmt
           ;;TODO: fix this many lets
           (format *stream* "~aif(~a) {~%~{~a~^~}~a} ~a"
                   (repeat-n *indent* *indent-ch*)
                   (let ((*stream* nil))
                     (pprint-js-tree (second tree)))
                   (let ((*stream* nil)
                         (*indent* (+ *indent* *indent-step*)))
                     (mapcar #'pprint-js-tree (third tree)))
                   (repeat-n *indent* *indent-ch*)
                   (let ((else (fourth tree)))
                     (if (null else)
                         ""
                         (format nil "else {~%~{~a~^~}~a}"
                                 (let ((*stream* nil)
                                       (*indent* (+ *indent* *indent-step*)))
                                   (mapcar #'pprint-js-tree else))
                                 (repeat-n *indent* *indent-ch*))))))
          (:eq
           ;;TODO: fix this many lets
           (format *stream* "~a === ~a"
                   (let ((*stream* nil))
                     (pprint-js-tree (second tree)))
                   (let ((*stream* nil))
                     (pprint-js-tree (third tree)))))
          (:arrow-fun
           (format *stream* "(~{~a~^, ~}) => {~%~{~a~^~}}"
                   (second tree)
                   (let ((*stream* nil)
                         ;;TODO: hard code
                         (*indent* (+ *indent* *indent-step*)))
                     (mapcar #'pprint-js-tree (cddr tree)))))
          (:template-str
           (format *stream* "`~{~a~^~}`"
                   (let ((*stream* nil))
                     (mapcar
                      (lambda (sub-tree)
                        (if (and (listp sub-tree) (eq :str (car sub-tree)))
                            (second sub-tree)
                            (format nil "${~a}"
                                    (pprint-js-tree sub-tree))))
                      (cdr tree)))))
          (:str
           (format *stream* "\"~a\"" (second tree)))
          (:num
           (format *stream* "~a" (second tree)))
          (:null
           (format *stream* "null"))
          (t (error "unknown js rule ~a" root))))))

(defun repeat-n (n obj)
  (format nil "~v@{~a~:*~}" n obj))
