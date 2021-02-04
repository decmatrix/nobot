;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/toplevel/logger
    (:use :cl)
  (:import-from :nobot/toplevel/context
                #:add-info-log
                #:add-warn-log
                #:add-debug-log
                #:add-error-log)
  (:import-from :nobot/utils
                #:logger-configuration)
  (:export #:configure-logger))

(in-package :nobot/toplevel/logger)

(defun configure-logger ()
  (make-instance 'logger-configuration
                 :on-info #'add-info-log
                 :on-debug #'add-debug-log
                 :on-warn #'add-warn-log
                 :on-error #'add-error-log))
