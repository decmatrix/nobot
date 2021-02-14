;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/parser-impl
    (:nicknames :nobot-bs-parser)
    (:use :cl
          :nobot/botscript/parser/acacia)
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
                #:no-term-to)
  (:export #:parse-source
           #:parse-string
           #:parse-file))

(in-package :nobot/botscript/parser/parser-impl)

(defun parse-string (str  &key return-instance (start-from :script))
  (parse-source str
                :string
                :return-instance return-instance
                :start-from start-from))

(defun parse-file (path &key return-instance (start-from :script))
  (parse-source path :file
                :return-instance return-instance
                :start-from start-from))


(defun parse-source (source type &key return-instance (start-from :script))
  (with-disassembled-source (source type)
    (with-acacia-process ((:start-from            start-from
                           :fun/rule->term-sym    (rcurry #'get-from-type :value :sort)
                           :fun/rule->description (rcurry #'get-from-type :description :sort)
                           :fun/token-rule->token-sym (rcurry #'get-from-type :value :token)
                           :fun/token-rule->token->description (rcurry #'get-from-type
                                                                       :description :token)
                           :fun/no-term->sym (curry #'no-term-to :sym)
                           :fun/no-term->description (curry #'no-term-to :description)
                           :tokens-source (get-tokens-source)
                           :source-type (get-source-type (get-tokens-source))
                           :source (get-source (get-tokens-source)))
                          :pack-result return-instance)
      
      (define-rule script ()
        (:and
         (:rule compiler-options)
         (:rule bot-options)
         (:rule data-decl-list)
         (:rule vertex-decl-list)))

      (define-rule compiler-options ()
        (:and
         (:no-term-sym keyword "c-opts")
         (:rule c-opts-block)))

      (define-rule bot-options ()
        (:and
         (:no-term-sym keyword "bot-opts")
         (:rule bot-opts-block)))

      (define-rule data-decl-list ()
        (:or
         (:and
          (:rule data-decl)
          (:rule data-decl-list))
         (:empty)))

      (define-rule vertex-decl-list ()
        (:or
         (:and
          (:rule vertex-decl)
          (:rule vertex-decl-list))
         (:empty)))

      (define-rule action-decl-list ()
        (:or
         (:and
          (:rule action-decl)
          (:rule action-decl-list))
         (:empty)))

      (define-rule start-stmt ()
        (:and
         (:no-term-sym keyword "start")
         (:no-term-sym keyword "from")
         (:no-term-sym id)))

      (define-rule c-opts-block ()
        (:and
         (:no-term-sym delimiter "{")
         (:rule opts-list)
         (:no-term-sym delimiter "}")))

      (define-rule bot-opts-block ()
        (:and
         (:no-term-sym delimiter "{")
         (:rule opts-list)
         (:no-term-sym delimiter "}")))

      (define-rule data-decl ()
        (:and
         (:no-term-sym keyword "letd")
         (:no-term-sym id)
         (:rule data-expr)))

      (define-rule vertex-decl ()
        (:and
         (:no-term-sym keyword "letv")
         (:no-term-sym id)
         (:rule vertex-options)))

      (define-rule action-decl ()
        (:and
         (:no-term-sym keyword "def-act")
         (:no-term-sym id)
         (:rule stmt-block)))

      (define-rule stmt-block ()
        (:and
         (:no-term-sym delimiter "{")
         (:rule stmt-list)
         (:no-term-sym delimiter "}")))

      (define-rule opts-list ()
        (:or
         (:and
          (:rule opt)
          (:rule opts-tail-list))
         (:empty)))

      (define-rule opt ()
        (:and
         (:no-term-sym id)
         (:no-term-sym delimiter ":")
         (:rule string-or-num)))

      (define-rule opts-tail-list ()
        (:or
         (:and
          (:no-term-sym delimiter ",")
          (:rule opt)
          (:rule opts-tail-list))
         (:empty)))

      (define-rule string-or-num ()
        (:or
         (:no-term-sym string)
         (:no-term-sym number-string)))

      (define-rule data-expr ()
        (:rule data-seq))

      (define-rule vertex-options ()
        (:and
         (:no-term-sym delimiter "[")
         (:rule vertex-option-list)
         (:no-term-sym delimiter "]")))

      (define-rule stmt-list ()
        (:or
         (:and
          (:rule stmt)
          (:rule stmt-list))
         (:empty)))

      (define-rule stmt ()
        (:and
         (:rule expr)
         (:no-term-sym delimiter "newline")))

      (define-rule data-seq ()
        (:and
         (:no-term-sym delimiter "[")
         (:rule item-list)
         (:no-term-sym delimiter "]")))

      (define-rule vertex-option-list ()
        (:or
         (:and
          (:rule vertex-option)
          (:no-term-sym delimiter ",")
          (:rule vertex-option-list))
         (:empty)))

      (define-rule expr ()
        (:empty))

      (define-rule item-list ()
        (:and
         (:rule item)
         (:rule item-tail-list)))

      (define-rule item-tail-list ()
        (:or
         (:and
          (:no-term-sym delimiter ",")
          (:rule item)
          (:rule item-tail-list))
         (:empty)))

      (define-rule item ()
        (:rule literal))

      (define-rule vertex-option ()
        (:and
         (:rule vertex-option-name)
         (:no-term-sym delimiter "=")
         (:rule vertex-option-val)))

      (define-rule vertex-option-name ()
        (:or
         (:no-term-sym keyword "act")
         (:no-term-sym keyword "type")))

      (define-rule vertex-option-val ()
        (:or
         (:no-term-sym id)
         (:no-term-sym keyword "in")
         (:no-term-sym keyword "out")))

      (define-rule literal ()
        (:or
         (:no-term-sym char-string)
         (:no-term-sym number-string)))
      )))
