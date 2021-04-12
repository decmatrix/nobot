;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/startup
    (:use :cl
          :nobot/toplevel)
  (:import-from :anaphora
                #:it
                #:awhen)
  (:import-from :unix-opts
                #:define-opts
                #:get-opts
                #:unknown-option)
  (:import-from :nobot/utils
                #:log-info
                #:get-pwd
                #:when-option
                #:it-opt
                #:is-valid-file-format-?
                #:get-program-version
                #:get-root-dir)
  (:export #:*run*))

(in-package :nobot/startup)

(defun parse-file (str)
  (if (is-valid-file-format-? str)
      str
      (progn
        (format t "fatal: unknown extension of file, expected '<file-name>.bs'~%")
        (opts:exit 1))))

(define-opts
  (:name :help
   :description "print avaliable nobot options"
   :short #\h
   :long "help")
  (:name :version
   :description "print version of app"
   :short #\v
   :long "version")
  (:name :description
   :description "print description about program"
   :short #\a
   :long "about")
  (:name :run-server
   :description "run translator as server"
   :short #\s
   :long "server"))

(defun *run* ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((unknown-option #'unknown-option-handler))
            (get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs as argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: can't parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition)))
        (opts:missing-required-option (condition)
          (format t "fatal: ~a~%"
                  condition)
          (opts:exit 1)))
    (when-option (options :help)
      (print-help-description))
    (when-option (options :version)
      (print-program-version))
    (when-option (options :description)
      (print-program-description))
    (when-option (options :run-server)
      ;; TODO: check port argument
      (format t "sorry, feature not implemented ;(~%")
      (*run-and-burn-as-server*))
    (when (and options free-args)
      (format t "fatal: extra arguments: ~{~a~^, ~}~%"
              free-args)
      (opts:exit 1))
    (when (null options)
      (unless free-args
        (format t "fatal: expected file of BotScript: '<file-name>.bs'~%")
        (opts:exit 1))
      (awhen (cdr free-args)
        (format t "fatal: extra arguments: ~{~a~^, ~}~%" it)
        (opts:exit 1))
      (*run-and-burn* (parse-file (car free-args))))))

(defun unknown-option-handler (condition)
  (format t "warning: unknown option ~s~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun print-program-description ()
  (format t "~A~%~A"
          (get-text-logo)
          (get-text-of-program-description)))

(defun print-help-description ()
  (opts:describe
   :prefix (get-text-logo)))

(defun print-program-version ()
  (format t "~A~%"
          (get-text-logo)))

(defun get-text-of-program-description ()
  (format nil
          "
NOBOT platform is engine for creation bots\
for different ebsites or social networks. \
Platform consist of own declarative description \
language of  bot's logic - BotScript and translator \
whose translate BotScript to popular programming \
languages for creation bots like JavaScipt and etc.\
For more detail about platform see documentation at \
website  http://nobot.space. Also exist web version of platform.
If you want to help improve platform write me to \
email -> <sokol.chemist@gmail.com> Bohdan Sokolovskyi. \
I would be glad to receive any help.\
Enjoy using the platform ;).~%"))

(defun get-text-author ()
  (format nil "designed by Bohdan Sokolovskyi~%"))

(defun get-text-of-program-version ()
  (format nil "platform v.~A"
          (get-program-version)))

(defun get-text-year ()
  (format nil "copyright (c) NOBOT 2021~%"))

(defun get-text-logo ()
  (format nil "~a~a~%~a~%~a"
          "
███╗   ██╗ ██████╗ ██████╗  ██████╗ ████████╗
████╗  ██║██╔═══██╗██╔══██╗██╔═══██╗╚══██╔══╝
██╔██╗ ██║██║   ██║██████╔╝██║   ██║   ██║   
██║╚██╗██║██║   ██║██╔══██╗██║   ██║   ██║   
██║ ╚████║╚██████╔╝██████╔╝╚██████╔╝   ██║   
╚═╝  ╚═══╝ ╚═════╝ ╚═════╝  ╚═════╝    ╚═╝
"
          (get-text-of-program-version)
          (get-text-author)
          (get-text-year)))
