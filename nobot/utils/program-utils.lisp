;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/utils/program-utils
    (:use :cl)
  (:import-from :cl-fad
                #:pathname-directory-pathname)
  (:import-from :unix-opts)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :cl-ppcre
                #:scan)
  (:export #:is-valid-file-format-?
           #:get-program-version
           #:when-option
           #:it-opt
           #:get-pwd
           #:get-root-dir
           #:with-time))

(in-package :nobot/utils/program-utils)


(defun is-valid-file-format-? (arg)
  (scan "^[\\d\\w\\-\\.\\(\\):/]+(\\.bs)$"
        arg))

(defmacro when-option ((options opt) &body body)
  `(let ((it-opt (getf ,options ,opt)))
     (when it-opt
       ,@body)))

(defun get-program-version ()
  (let ((system (asdf:find-system :nobot nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

(defun get-pwd ()
  (truename "."))

(defun get-root-dir ()
  (pathname-directory-pathname sb-ext:*core-pathname*))

(defmacro with-time (() &body body)
  (with-gensyms (start-time res)
    `(let ((,start-time (get-internal-real-time))
           (,res (progn ,@body)))
       (values
        (/ (- (get-internal-real-time) ,start-time)
           internal-time-units-per-second)
        ,res))))
