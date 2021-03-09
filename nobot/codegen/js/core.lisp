;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen/js/core
    (:use :cl
          :nobot/codegen/js/form-factor)
  (:import-from :nobot/toplevel/context
                #:*context*
                #:get-projectgen-result
                #:get-post-processing-result)
  (:import-from :nobot/codegen/utils
                #:with-new-file
                #:$file-stream)
  (:import-from :nobot/projectgen/common
                #:projectgen-info)
  (:import-from :nobot/botscript/post
                #:botscript-post-process-info)
  (:import-from :nobot/codegen/top
                #:generate-output-code))

(in-package :nobot/codegen/js/core)

(defvar *project-path*)
(defvar *post-process-result*)
(defvar *projectgen-result*)

(defmethod generate-output-code ((post-processing-result botscript-post-process-info)
                                 (projectgen-result )
                                 (lang (eql :js)))
  (let ((*project-path* project-path))
    (generate-index-file)))

(defun generate-index-file ()
  (with-new-file (*project-path* "index" :js)
    (with-build-output ($file-stream)
      )))

(defun generate-js-tree ()
  )

;;TODO: generate comment with full description about project
(defun generate-header ()
  )

(defun generate-bot-instance ()
  )

(defun generate-bot-options-instance ()
  )


