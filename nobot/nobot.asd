(asdf:defsystem "nobot"
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Engine for creating chat bots"
    :version "1.0"
    :author "Sokolovskyi Bohdan"
    :depends-on ("nobot/startup"
                 "nobot/toplevel"
                 "nobot/botscript"
                 "nobot/utils"
                 "nobot/collections"
                 ;; other libs
                 "alexandria"
                 "anaphora"
                 "lisp-unit"
                 "cl-ppcre"
                 "unix-opts")
    :in-order-to ((test-op (load-op "nobot/tests")))
    ;;:perform (test-op (o c) (uiop:symbol-call :nobot/tests :run-unit-tests))
    )
