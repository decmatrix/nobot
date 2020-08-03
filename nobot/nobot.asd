(asdf:defsystem "nobot"
    :class :package-inferred-system
    :defsystem-depends-on (:asdf-package-system)
    :description "Engine for creating chat bots"
    :version "0.0.1"
    :author "Sokolovskyi Bohdan"
    :depends-on ("nobot/core"
                 "nobot/botscript"
                 "nobot/utils"
                 "nobot/collections"
                 ;; other libs
                 "alexandria"
                 "anaphora"
                 "lisp-unit"))
