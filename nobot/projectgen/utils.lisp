;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/projectgen/utils
    (:use :cl)
  (:import-from :alexandria
                #:plist-hash-table)
  (:import-from :yason
                #:encode)
  (:export #:make-path))

(defun make-path (path)
  (merge-pathnames
   *default-pathname-defaults*
   path))
