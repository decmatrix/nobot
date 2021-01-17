(uiop:define-package :nobot/toplevel/error-handling
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :nobot/utils
                #:log-error)
  (:export #:raise-bs-lexer-error
           #:raise-bs-parser-error
           #:lexer-error-handler
           #:toplevel-error-handler))

(in-package :nobot/toplevel/error-handling)

;; Level 1 errors
(define-condition bs-lexer-error (error) ())
(define-condition bs-parser-error (error) ())

(defmacro raise-bs-lexer-error (msg &rest rest)
  `(progn
     (log-error ,msg ,@rest)
     (make-condition 'bs-lexer-error)))

(defmacro raise-bs-parser-error (msg &rest rest)
  `(progn
     (log-error ,msg ,@rest)
     (make-condition 'bs-parser-error)))

(defmacro lexer-error-handler ((action-on-catch) &body body)
  `(handler-case
       (progn ,@body)
     (bs-lexer-error (c)
       (funcall ,action-on-catch))))

(defmacro toplevel-error-handler (&body body)
  `(handler-case
       (progn ,@body)
     (bs-lexer-error (c)
       nil)
     (bs-parser-error (c)
       nil)))

