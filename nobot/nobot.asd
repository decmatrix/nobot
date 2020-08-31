(asdf:defsystem "nobot"
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Engine for creating chat bots"
    :version "0.0.1"
    :author "Sokolovskyi Bohdan"
    :depends-on ("nobot/main"
                 "nobot/core"
                 "nobot/botscript"
                 "nobot/utils"
                 "nobot/collections"
                 ;; other libs
                 "alexandria"
                 "anaphora"
                 "lisp-unit")
    :in-order-to ((test-op (load-op "nobot/tests")))
    ;;:perform (test-op (o c) (uiop:symbol-call :nobot/tests :run-unit-tests))
    :build-operation "program-op" ;;???
    :build-pathname "nobot-app"
    :entry-point "nobot/main:run-nobot")
