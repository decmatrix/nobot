(asdf:defsystem "nobot"
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Engine for creating chat bots"
    :version "0.1.0"
    :author "Sokolovskyi Bohdan"
    :depends-on ("nobot/startup"
                 "nobot/core"
                 "nobot/botscript"
                 "nobot/utils"
                 "nobot/collections"
                 ;; other libs
                 "alexandria"
                 "anaphora"
                 "lisp-unit"
                 "unix-opts"
                 "cl-ppcre")
    :in-order-to ((test-op (load-op "nobot/tests")))
    ;;:perform (test-op (o c) (uiop:symbol-call :nobot/tests :run-unit-tests))
    )
