;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/server
    (:use :cl
          :usocket)
  (:export :run-server))

(in-package :nobot/server)

;;;;;;;;;;;;;;;;;; WARNING: IT'S TESTING MODULE ;;;;;;;;;;;;;;;;;;

(defconstant +default-port+ 8086)
(defconstant +default-host+ "127.0.0.1")

;;TODO: write own logger module for this module
(defun run-server (&key (port +default-port+))
  (let* ((socket (socket-listen +default-host+))
         (connection (socket-accept socket
                                    :element-type 'character)))
    (format t "nobot srver runned on port ~A" port)
    (unwind-protect
         (progn
           ;; TODO: implement working with translator
           )
      (progn
        (socket-close connection)
        (socket-close socket)
        (format t  "nobot server stoped!")))))
