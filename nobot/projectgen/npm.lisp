;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/npm
    (:use :cl)
  (:import-from :alexandria
                #:hash-table-plist)
  (:import-from :yason
                #:encode)
  (:import-from :nobot/projectgen/utils
                #:make-path)
  (:import-from :nobot/projectgen/common
                #:generate-project
                #:*project*
                #:get-project-path))

(in-package :nobot/projectgen/npm)

(defmethod generate-project ((project-type (eql :npm)))
  (let ((file-path (get-file-path
                    :name "package"
                    :type "json")))
    (with-open-file
        (stream
         file-path
         :direction :output)
      (enocode
       (hash-table-plist
        `(
          "name"        ,(get-project-name *project*)
          "version"     ,(get-project-version *project*)
          "description" "generatet bot by NOBOT platform"
          "main"        "index.js"
          "author"      ,(get-project-author *project*)
          "scripts"     ,(hash-table-plist
                          "test" "echo \"Error: no test specified\" && exit 1")
          "license"     "<none>"))
       stream))))

(defun get-file-path (name type)
  (merge-pathnames
   (get-project-path *project*)
   (make-pathname
    :name name
    :type type)))
