(uiop:define-package :nobot/main
    (:use :cl)
  (:export #:run-nobot))

(in-package :nobot/main)

(defun run-nobot ()
  (format t "NOBOT v.1.2~%"))
