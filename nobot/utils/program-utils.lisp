(uiop:define-package :nobot/utils/program-utils
    (:use :cl)
  (:import-from :unix-opts)
  (:import-from :cl-ppcre
                #:scan)
  (:export #:is-valid-file-format-?
           #:get-program-version
           #:when-option
           #:it-opt))

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
