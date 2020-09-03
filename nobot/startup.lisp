(uiop:define-package :nobot/startup
    (:use :cl)
  (:import-from :anaphora
                #:awhen)
  (:import-from :unix-opts
                #:define-opts
                #:get-opts)
  (:import-from :cl-ppcre
                #:scan)
  (:export #:run-nobot))

(in-package :nobot/startup)

(define-opts
  (:name :help
         :description "print avaliable nobot options"
         :short #\h
         :long "help"))

(defun run-nobot ()
  (multiple-value-bind (options free-args)
      (get-opts)
    (awhen (getf options :help)
      (format t "NOBOT PLATFORM v.1.0 alpha~%"))
    (when (and free-args (not (cdr free-args)))
      (let ((file (car free-args)))
        (if (is-valid-file-format-? file)
            (format t "File done!~%")
            (format t "WRANG FILE FORMAT!~%"))))))

(defun is-valid-file-format-? (arg)
  (scan "^[\\d\\w\\-\\.\\(\\)\\:]+(\\.bs)|(\\.predef-bs)$"
        arg))
