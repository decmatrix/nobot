(uiop:define-package :nobot/tests/parser
    (:use :cl
          :lisp-unit
          :nobot/botscript/tree-utils
          :nobot/botscript/parser)
  (:import-from :nobot/botscript/tree-utils
                #:same-parse-tree-?))

(in-package :nobot/tests/parser)

(defmacro define-parser-test (name input-string expected-tree)
  `(define-test ,name
     (assert-true (same-parse-tree-?
                   (parse-string ,input-string)
                   ',expected-tree))))


(define-parser-test parser.1
    ""
  (<script>
   (<macros-block>)
   (<predefined-block>)
   (<definition/combo-block>)
   (<graph-logic>)))

(define-parser-test parser.2
    "#exe set-lang js
     #exe reset-lang"
  (<script>
   (<macros-block>
    (<exe-macros> (<keyword> |#EXE|) (<id> "SET-LANG")
                  (<args-list> (<arg> (<id> "JS"))))
    (<exe-macros> (<keyword> |#EXE|) (<id> "RESET-LANG")
                  (<args-list>)))
   (<predefined-block>)
   (<definition/combo-block>)
   (<graph-logic>)))

(define-parser-test parser.3
    "#exe set-lang python
     @def port 8081"
  (<script>
   (<macros-block>
    (<exe-macros> (<keyword> |#EXE|) (<id> "SET-LANG")
                  (<args-list> (<arg> (<id> "PYTHON")))))
   (<predefined-block>)
   (<definition/combo-block>
    (<call-definition> (<keyword> @DEF) (<id> "PORT")
                       (<args-list> (<arg> (<number-string> 8081)))))
   (<graph-logic>)))

(define-parser-test parser.4
    "#exe set-lang python
     @def port 8081
     $combo local-bot"
  (<script>
   (<macros-block>
    (<exe-macros> (<keyword> |#EXE|) (<id> "SET-LANG")
                  (<args-list> (<arg> (<id> "PYTHON")))))
   (<predefined-block>)
   (<definition/combo-block>
    (<call-definition> (<keyword> @DEF) (<id> "PORT")
                       (<args-list> (<arg> (<number-string> 8081))))
    (<call-combo> (<keyword> $COMBO) (<id> "LOCAL-BOT")))
   (<graph-logic>)))

(define-parser-test parser.5
    "#exe set-lang python
     @def port 8081
     $combo local-bot
     @def host-std"
  (<script>
   (<macros-block>
    (<exe-macros> (<keyword> |#EXE|) (<id> "SET-LANG")
                  (<args-list> (<arg> (<id> "PYTHON")))))
   (<predefined-block>)
   (<definition/combo-block>
    (<call-definition> (<keyword> @DEF) (<id> "PORT")
                       (<args-list> (<arg> (<number-string> 8081))))
    (<call-combo> (<keyword> $COMBO) (<id> "LOCAL-BOT"))
    (<call-definition> (<keyword> @DEF) (<id> "HOST-STD")
                       (<args-list>)))
   (<graph-logic>)))


