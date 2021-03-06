;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/codegen/utils
    (:use :cl)
  (:import-from :nobot/utils
                #:to-string)
  (:export #:with-new-file
           #:$file-stream))

(in-package :nobot/codegen/utils)

(defmacro with-new-file ((path name type) &body)
  `(with-open-file ($file-stream (merge-pathnames
                                 path
                                 (make-pathname
                                  :name name
                                  :type (if (keywordp type)
                                            (to-string type)
                                            type)))
                                :direction :output
                                :if-does-not-exist :create)
     ,@body))
