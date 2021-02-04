;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(asdf:defsystem :nobot
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Engine for creating chat bots"
    :version "0.1"
    :author "Sokolovskyi Bohdan"
    :depends-on ("nobot/startup"
                 "nobot/toplevel"
                 "nobot/botscript"
                 "nobot/codegen"
                 "nobot/projectgen"
                 "nobot/utils"
                 "nobot/collections"
                 "nobot/logger"
                 ;;"nobot/server"
                 ;; 3th part libs
                 "alexandria"
                 "anaphora"
                 "lisp-unit"
                 "cl-ppcre"
                 "unix-opts"
                 "yason")
    :in-order-to ((asdf:test-op (asdf:load-op :nobot/tests)))
    :perform (asdf:test-op (o c) (uiop:symbol-call :nobot/tests :run-unit-tests)))
