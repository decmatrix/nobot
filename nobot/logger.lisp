;;;; Copyright (c) 2021 NOBOT-S
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/logger
    (:use :cl
          :nobot/logger/logger-impl)
  (:export #:logg
           #:log-info
           #:log-debug
           #:log-warn
           #:log-error
           #:with-log-level-context
           #:log-context
           #:with-logger
           #:logger-configuration))

(in-package :nobot/logger)
