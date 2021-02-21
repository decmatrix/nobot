;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/parser-impl
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
                          :fun/token-rule->description (rcurry #'get-from-type :token :description)
                          :fun/terminal->sym (curry #'terminal-to :sym)
                          :fun/terminal->description (curry #'terminal-to :description)
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
         (:terminal keyword "c-opts")
         (:rule c-opts-block)))

      (define-rule bot-options ()
        (:and
         (:terminal keyword "bot-opts")
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
         (:terminal keyword "start")
         (:terminal keyword "from")
         (:terminal id)))

      ;;TODO: redundant rule this and next
      (define-rule c-opts-block ()
        (:and
         (:terminal delimiter #\{)
         (:rule opts-list)
         (:terminal delimiter #\})))

      (define-rule bot-opts-block ()
        (:and
         (:terminal delimiter #\{)
         (:rule opts-list)
         (:terminal delimiter #\})))

      (define-rule data-decl ()
        (:and
         (:terminal keyword "letd")
         (:terminal id)
         (:rule data-expr)))

      (define-rule vertex-decl ()
        (:and
         (:terminal keyword "letv")
         (:terminal id)
         (:rule vertex-options)))

      (define-rule action-decl ()
        (:and
         (:terminal keyword "def-act")
         (:terminal id)
         (:rule stmt-block)))

      (define-rule stmt-block ()
        (:and
         (:terminal delimiter #\{)
         (:rule stmt-list)
         (:terminal delimiter #\})))

      (define-rule opts-list ()
        (:or
         (:and
          (:rule opt)
          (:rule opts-tail-list))
         (:empty)))

      (define-rule opt ()
        (:and
         (:terminal id)
         (:terminal delimiter #\:)
         (:rule string-or-num)))

      (define-rule opts-tail-list ()
        (:or
         (:and
          (:terminal delimiter #\,)
          (:rule opt)
          (:rule opts-tail-list))
         (:empty)))

      (define-rule string-or-num ()
        (:or
         (:terminal string)
         (:terminal number-string)))

      (define-rule data-expr ()
        (:and
         (:rule data-seq)))

      (define-rule vertex-options ()
        (:and
         (:terminal delimiter #\[)
         (:rule vertex-option-list)
         (:terminal delimiter #\])))

      (define-rule stmt-list ()
        (:or
         (:and
          (:rule stmt)
          (:rule stmt-list))
         (:empty)))

      (define-rule stmt ()
        (:and
         (:rule expr)
         (:terminal delimiter #\Newline)))

      (define-rule data-seq ()
        (:and
         (:terminal delimiter #\[)
         (:rule item-list)
         (:terminal delimiter #\])))

      (define-rule vertex-option-list ()
        (:or
         (:and
          (:rule vertex-option)
          (:terminal delimiter #\,)
          (:rule vertex-option-list))
         (:empty)))

      (define-rule expr ()
        (:and
         (:empty)))

      (define-rule item-list ()
        (:and
         (:rule item)
         (:rule item-tail-list)))

      ;;TODO: bug with 
      (define-rule item-tail-list ()
        (:or
         (:and
          (:terminal delimiter #\,)
          (:rule item)
          (:rule item-tail-list))
         (:empty)))

      (define-rule item ()
        (:and
         (:rule literal)))

      (define-rule vertex-option ()
        (:and
         (:rule vertex-option-name)
         (:terminal delimiter #\=)
         (:rule vertex-option-val)))

      (define-rule vertex-option-name ()
        (:or
         (:terminal keyword "act")
         (:terminal keyword "type")))

      (define-rule vertex-option-val ()
        (:or
         (:terminal id)
         (:terminal keyword "in")
         (:terminal keyword "out")))

      (define-rule literal ()
        (:or
         (:terminal char-string)
         (:terminal number-string)))
      )))
