;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/common
    (:use :cl)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-projectgen-result)
  (:import-from :nobot/projectgen/utils
                #:make-path)
  (:import-from :nobot/botscript
                #:botscript-post-process-info)
  (:export #:create-project
           #:generate-project
           #:*project*
           #:get-project-path
           #:get-project-type
           #:get-project-name
           #:get-project-version
           #:get-project-author
           ;; projectgen info
           #:projectgen-get-path
           #:projectgen-info))

(in-package :nobot/projectgen/common)

(defvar *project*)

(defclass bot-project ()
  ((type
    :initarg :project-type
    :type keyword
    :reader get-project-type)
   (name
    :initarg :project-name
    :type string
    :reader get-project-name)
   (version
    :initarg :project-version
    :type string
    :reader get-project-version)
   (author
    :initarg :project-author
    :type string
    :reader get-project-author)
   (path
    :initarg :project-path
    :type pathname
    :reader get-project-path)))

(defclass projectgen-info ()
  ((path
    :initarg :projectgen-path
    :type pathname
    :reader projectgen-get-path)))

(defgeneric create-project (post-process-instance))

(defun generate-project ()
  (let ((*project*
         (make-bot-project
          (get-projectgen-result *context*))))
    (create-project (get-project-type *project*))
    (make-projectgen-info)))

(defun make-bot-project (post-process-instance)
  (declare (ignore post-process-instance)))

(defun make-projectgen-info ()
  (make-instance
   'projectgen-info
   :projectgen-path (get-project-path *project*)))




