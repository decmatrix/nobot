(uiop:define-package :nobot/utils/logger
    (:use :cl)
  (:import-from :alexandria
                #:define-constant)
  (:export #:logg
           #:log-info
           #:log-debug
           #:log-warn
           #:log-error
           #:with-log-level-context
           #:log-context))

(in-package :nobot/utils/logger)

(defvar *log-level*)

(defmacro logg (level msg &rest rest)
  `(format t (format nil "[NOBOT] - ~A: ~A~%"
                     (get-log-suffixes ,level)
                     ,msg)
           ,@rest))

(defmacro log-info (msg &rest rest)
  `(logg :info ,msg ,@rest))

(defmacro log-debug (msg &rest rest)
  `(logg :debug ,msg ,@rest))

(defmacro log-warn (msg &rest rest)
  `(logg :warn ,msg ,@rest))

(defmacro log-error (msg &rest rest)
  `(logg :error ,msg ,@rest))

(defmacro log-context (msg &rest rest)
  `(logg ,*log-level* ,msg ,@rest))

(defmacro with-log-level-context ((log-level) &body body)
  `(let ((*log-level* ,log-level))
     ,@body))

(defun get-log-suffixes (suf)
  (case suf
    (:info "<INFO>")
    (:degub "<DEGUB>")
    (:warn "<WARN>")
    (:error "<ERROR>")))
