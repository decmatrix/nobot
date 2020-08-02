(asdf:defsystem "nobot"
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Engine for creating chat bots"
    :version "0.0.1"
    :author "Sokolovskyi Bohdan"
    :depends-on ("nobot/core")
    :in-order-to ((test-op (load-op "nobot/tests")))
    :perform (test-op (o c) (uiop:symbol-call :nobot/tests :test-suite)))
