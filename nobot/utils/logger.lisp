(uiop:define-package :nobot/utils/logger
    (:use :cl)
  (:import-from :alexandria
                #:define-constant
                #:with-gensyms)
  (:export #:logg
           #:log-info
           #:log-debug
           #:log-warn
           #:log-error
           #:with-log-level-context
           #:log-context
           #:with-logger
           #:logger-configuration))

(in-package :nobot/utils/logger)

(defclass logger-configuration ()
  ((prefix
    :initarg :prefix
    :initform "NOBOT"
    :type string
    :reader get-prefix)
   (on-info
    :initarg :on-info
    :initform #'identity
    :type function
    :reader get-on-info)
   (on-debug
    :initarg :on-debug
    :initform #'identity
    :type function
    :reader get-on-debug)
   (on-warn
    :initarg :on-warn
    :initform #'identity
    :type function
    :reader get-on-warn)
   (on-error
    :initarg :on-error
    :initform #'identity
    :type function
    :reader get-on-error)
   (printable
    :initarg :printable
    :initform t
    :type boolean
    :reader is-printable-?
    )))

(defvar *log-level*)
(defvar *currnet-logger-configuration*)
(defvar *logger-configuration*)

(defparameter +default-logger-configuration+
  (make-instance 'logger-configuration))

(defmacro logg (level msg &rest rest)
  (with-gensyms (log-msg)
    `(let* ((*currnet-logger-configuration* (get-logger-configuration))
            (,log-msg (make-log-msg ,msg ,level)))
       (case ,level
         (:info (funcall (get-on-info *currnet-logger-configuration*) ,log-msg))
         (:debug (funcall (get-on-debug *currnet-logger-configuration*) ,log-msg))
         (:warn (funcall (get-on-warn *currnet-logger-configuration*) ,log-msg))
         (:error (funcall (get-on-error *currnet-logger-configuration*) ,log-msg))
         (t (error "Expected: [:info, :debug, :warn, :error], but got ~a"
              ,level)))
       (when (is-printable-? *currnet-logger-configuration*)
         (format t ,log-msg ,@rest)))))

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

(defmacro with-logger ((logger-config) &body body)
  (let ((logger-config logger-config))
    `(let ((*logger-configuration* (if (typep ,logger-config 'logger-configuration)
                                       ,logger-config
                                       (error "Expected logger configuration, but got ~a"
                                              (type-of ,logger-config)))))
       ,@body)))

(defun default-logger-handler (msg)
  (format t msg))

(defun make-log-msg (msg level)
  (format nil "[~a] - <~a>: ~a"
          (get-prefix *currnet-logger-configuration*)
          level
          msg))

(defun get-logger-configuration ()
  (if (boundp *logger-configuration*)
      *logger-configuration*
      +default-logger-configuration+))

(defun get-log-suffixes (suf)
  (case suf
    (:info "INFO")
    (:debug "DEBUG")
    (:warn "WARN")
    (:error "ERROR")
    (t (error "Expected: [:info, :debug, :warn, :error], but got ~a"
              suf))))
