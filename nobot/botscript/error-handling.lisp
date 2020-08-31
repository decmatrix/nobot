(uiop:define-package :nobot/botscript/error-handling
    (:use :cl)
  (:import-from :nobot/utils
                #:defcontextvar))

(in-package :nobot/botscript/error-handling)

;; type errors
(define-condition lexer-error (error) ())
(define-condition parser-error (error) ())
