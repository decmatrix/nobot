;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/common
    (:use :cl
          :nobot/botscript/post)
  (:import-from :cl-fad
                #:delete-directory-and-files
                #:directory-exists-p)
  (:import-from :nobot/utils
                #:to-keyword
                #:get-pwd)
  (:import-from :nobot/toplevel/error-handling
                #:raise-projectgen-error)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-post-processing-result
                #:get-parser-result)
  (:import-from :nobot/projectgen/utils
                #:make-path)
  (:import-from :nobot/botscript/parser/acacia/result-packaging
                #:acacia-get-source
                #:acacia-get-source-type)
  (:export #:create-project
           #:generate-project
           #:*project*
           ;; projectgen info
           #:get-project-path
           #:get-project-type
           #:get-project-lang
           #:get-project-name
           #:get-project-version
           #:get-project-author))

(in-package :nobot/projectgen/common)

(defvar *project*)
(defvar *parser-result*)

(defclass bot-project ()
  ((type
    :initarg :project-type
    :type keyword
    :reader get-project-type)
   (lang
    :initarg :project-lang
    :type keyword
    :reader get-project-lang)
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

(defgeneric create-project (project-type))

(defun generate-project ()
  (let ((*project*
         (make-bot-project
          (get-post-processing-result *context*))))
    (create-project (get-project-type *project*))
    *project*))

(defun make-bot-project (post-process-instance)
  (let* ((*parser-result* (get-parser-result *context*))
         (compiler-options (get-compiler-options post-process-instance))
         (bot-options (get-bot-options post-process-instance))
         (project-lang (to-keyword
                        (or (gethash :@codegen compiler-options)
                            (raise-projectgen-error
                             "undefined compiler option: @codegen~a"
                             (make-source-msg)))))
         (project-name (make-project-name
                        (or (gethash :name bot-options)
                            (raise-projectgen-error
                             "undefined bot option: name~a"
                             (make-source-msg))))))
    (make-instance
     'bot-project
     :project-type (determine-project-type project-lang)
     :project-lang project-lang
     :project-name project-name
     ;;TODO: check version
     :project-version (gethash :version bot-options "")
     :project-author (gethash :author bot-options "")
     :project-path (if (eq (acacia-get-source-type *parser-result*)
                           :string)
                       (error "string as source type in projectgen")
                       (make-project-path project-name)))))

(defun determine-project-type (project-lang)
  (case project-lang
    (:js :npm)
    (t (raise-projectgen-error
        "unknown value of lang option: ~a~a"
        project-lang
        (make-source-msg)))))

(defun make-project-name (bot-name)
  (format nil "~a-project" bot-name))

(defun make-project-path (project-name)
  (let ((project-path
         (pathname (format nil "~a./~a/"
                           (get-pwd)
                           project-name))))
    (when (directory-exists-p project-path)
      (delete-directory-and-files project-path))
    (ensure-directories-exist project-path)
    project-path))

(defun make-source-msg ()
  (if (eq (acacia-get-source-type *parser-result*) :file)
      (format nil " in file: ~a"
              (acacia-get-source *parser-result*))
      ""))
