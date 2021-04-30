;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/parser-impl
    (:use :cl
          :nobot/botscript/parser/acacia)
  (:nicknames :bs-parser)
  (:import-from :alexandria
                #:rcurry
                #:curry)
  (:import-from :nobot/utils
                #:to-symbol)
  (:import-from :nobot/botscript/types
                #:get-from-type)
  (:import-from :nobot/botscript/lexer
                #:with-disassembled-source
                #:get-tokens-source
                #:get-source
                #:get-source-type
                #:terminal-to)
  (:export #:parse-source
           #:parse-string
           #:parse-file))

(in-package :nobot/botscript/parser/parser-impl)

(defun parse-string (str  &key return-instance (sort-type :script))
  (parse-source str
                :string
                :return-instance return-instance
                :sort-type sort-type))

(defun parse-file (path &key return-instance (sort-type :script))
  (parse-source path :file
                :return-instance return-instance
                :sort-type sort-type))


(defun parse-source (source type &key return-instance (sort-type :script))
  (with-disassembled-source (source type)
    (with-acacia-runner ((:start-from            sort-type
                          :fun/rule->term-sym    (rcurry #'get-from-type :sort :value)
                          :fun/rule->description (rcurry #'get-from-type :sort :description)
                          :fun/token-rule->token-sym (rcurry #'get-from-type :token :value)
                          :fun/token-rule->description (rcurry
                                                        #'get-from-type
                                                        :token
                                                        :description)
                          :fun/terminal->sym (curry #'terminal-to :sym)
                          :fun/terminal->description (curry #'terminal-to :description)
                          :tokens-source (get-tokens-source)
                          :source-type (get-source-type (get-tokens-source))
                          :source (get-source (get-tokens-source)))
                         :pack-result return-instance)
      
      (define-rule script ()
        (:and
         (:rule compiler-options)
         (:rule bot-declaration)))

      (define-rule compiler-options ()
        (:or
         (:and
          (:rule compiler-option)
          (:rule* compiler-options))
         (:empty)))

      (define-rule compiler-option ()
        (:and
         (:rule compiler-option-name)
         (:rule string-or-number)
         (:terminal* delimiter ";")))

      (define-rule compiler-option-name ()
        (:or
         (:terminal keyword "@codegen")
         (:terminal keyword "@platform")
         (:terminal keyword "@arch-type")))

      (define-rule bot-declaration ()
        (:and
         (:terminal* keyword "bot")
         (:terminal* delimiter "{")
         (:rule bot-options)
         (:rule var-declarations)
         (:rule start-from-stmt)
         (:rule state-points-declarations)
         (:rule state-actions-declarations)
         (:terminal* delimiter "}")))

      (define-rule bot-options ()
        (:and
         (:terminal* keyword "options")
         (:terminal* delimiter "{")
         (:rule* bot-options-list)
         (:terminal* delimiter "}")))

      (define-rule bot-options-list ()
        (:or
         (:and
          (:rule bot-option)
          (:rule* bot-options-list))
         (:empty)))

      (define-rule bot-option ()
        (:and
         (:terminal id)
         (:terminal* delimiter ":")
         (:rule string-or-number)
         (:terminal* delimiter ";")))

      (define-rule var-declarations ()
        (:and
         (:terminal* keyword "vars")
         (:terminal* delimiter "{")
         (:rule* var-decls-list)
         (:terminal* delimiter "}")))

      (define-rule var-decls-list ()
        (:or
         (:and
          (:rule var-decl)
          (:rule* var-decls-list))
         (:empty)))

      (define-rule var-decl ()
        (:and
         (:terminal id)
         (:terminal* delimiter ":")
         (:rule* literal)
         (:terminal* delimiter ";")))

      (define-rule start-from-stmt ()
        (:and
         (:terminal* keyword "start")
         (:terminal* keyword "from")
         (:terminal id)
         (:terminal* delimiter ";")))

      (define-rule state-points-declarations ()
        (:and
         (:terminal* keyword "state-points")
         (:terminal* delimiter "{")
         (:rule* state-points-decls)
         (:terminal* delimiter "}")))

      (define-rule state-points-decls ()
        (:or
         (:and
          (:rule state-point-decl)
          (:rule* state-points-decls))
         (:empty)))

      (define-rule state-point-decl ()
        (:and
         (:terminal id)
         (:terminal* delimiter ":")
         (:terminal* delimiter "{")
         (:rule state-point-options)
         (:terminal* delimiter "}")))

      (define-rule state-point-options ()
        (:or
         (:and
          (:rule state-point-option)
          (:rule* state-point-options))
         (:empty)))

      (define-rule state-point-option ()
        (:and
         (:terminal id)
         (:terminal* delimiter ":")
         (:terminal id)
         (:terminal* delimiter ";")))

      (define-rule state-actions-declarations ()
        (:and
         (:terminal* keyword "state-actions")
         (:terminal* delimiter "{")
         (:rule* state-actions-decls)
         (:terminal* delimiter "}")))

      (define-rule state-actions-decls ()
        (:or
         (:and
          (:rule state-decl)
          (:rule* state-actions-decls))
         (:empty)))

      (define-rule state-decl ()
        (:and
         (:terminal id)
         (:terminal* delimiter ":")
         (:terminal* delimiter "{")
         (:rule stmt-list)
         (:terminal* delimiter "}")))

      (define-rule stmt-list ()
        (:or
         (:and
          (:rule stmt)
          (:rule* stmt-list))
         (:empty)))

      (define-rule stmt ()
        (:or
         (:rule if-stmt)
         (:and
          (:rule expr)
          (:terminal* delimiter ";"))))

      (define-rule expr ()
        (:or
         (:rule gotov-expr)
         (:rule say-expr)
         (:rule save-to-expr)))

      (define-rule gotov-expr ()
        (:and
         (:terminal* keyword "gotov")
         (:rule* gotov-arg)))

      (define-rule gotov-arg ()
        (:or
         (:terminal id)
         (:terminal keyword "self")))

      (define-rule say-expr ()
        (:and
         (:terminal* keyword "say")
         (:rule* say-expr-args)))

      (define-rule say-expr-args ()
        (:and
         (:rule* say-expr-arg)
         (:rule* rest-say-expr-args)))

      (define-rule say-expr-arg ()
        (:or
         (:terminal char-string)
         (:terminal number-string)
         (:terminal id)
         (:terminal keyword "?input")))

      (define-rule rest-say-expr-args ()
        (:or
         (:and
          (:rule* say-expr-arg)
          (:rule* rest-say-expr-args))
         (:empty)))

      (define-rule save-to-expr ()
        (:and
         (:terminal* keyword "save")
         (:rule* literal-or-id-or-input)
         (:terminal* keyword "to")
         (:terminal id)))

      (define-rule if-stmt ()
        (:and
         (:terminal* keyword "if")
         (:rule cond-expr)
         (:terminal* delimiter "{")
         (:rule stmt-list)
         (:terminal* delimiter "}")
         (:rule else-block)))

      (define-rule else-block ()
        (:or
         (:and
          (:terminal* keyword "else")
          (:terminal* delimiter "{")
          (:rule stmt-list)
          (:terminal* delimiter "}"))
         (:empty)))

      ;;TODO: redundant AND rule for single RULE rule
      (define-rule cond-expr ()
        (:and
         (:rule logic-expr)))

      (define-rule logic-expr ()
        (:or
         (:rule equal-expr)
         (:rule in-expr)))

      (define-rule equal-expr ()
        (:and
         (:rule eq-sub-expr)
         (:terminal* delimiter "==")
         (:rule eq-sub-expr)))

      (define-rule eq-sub-expr ()
        (:or
         (:terminal id)
         (:terminal char-string)
         (:terminal number-string)
         (:terminal keyword "?input")))

      (define-rule in-expr ()
        (:and
         (:rule* left-in-expr)
         (:terminal* keyword "in")
         (:rule* right-in-expr)))

      (define-rule left-in-expr ()
        (:or
         (:terminal char-string)
         (:terminal number-string)
         (:terminal id)
         (:terminal keyword "?input")))

      (define-rule right-in-expr ()
        (:or
         (:terminal char-string)
         (:terminal keyword "?input")
         (:rule item-list)))

      (define-rule literal-or-id-or-input ()
        (:or
         (:rule literal)
         (:terminal id)
         (:terminal keyword "?input")))

      (define-rule string-or-number ()
        (:or
         (:terminal char-string)
         (:terminal number-string)))

      (define-rule literal ()
        (:or
         (:terminal char-string)
         (:terminal number-string)
         (:rule item-list)
         (:terminal keyword "none")))

      (define-rule item-list ()
        (:and
         (:terminal* delimiter "[")
         (:rule* literal-list)
         (:terminal* delimiter "]")))

      (define-rule literal-list ()
        (:or
         (:and
          (:rule literal)
          (:rule* rest-literal-list))
         (:empty)))

      (define-rule rest-literal-list ()
        (:or
         (:and
          (:terminal* delimiter ",")
          (:rule literal)
          (:rule* rest-literal-list))
         (:empty)))
      )))
