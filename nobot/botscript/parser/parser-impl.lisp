(uiop:define-package :nobot/botscript/parser-impl
    (:use :cl
          :nobot/botscript/parser-utils)
  (:import-from :nobot/botscript/tree-utils
                #:@revert-tree)
  (:import-from :nobot/botscript/parser-utils
                #:@goto
                #:@is-token-of-type
                #:@is-token-of-value
                #:@next-token
                #:@revert-state-action
                #:get-tokens-source)
  (:import-from :nobot/botscript/types
                #:get-sort-type-symbol)
  (:import-from :nobot/botscript/lexer-utils
                #:get-symbol-for-keyword)
  (:export #:parse-source
           #:parse-string
           #:parse-file))

(in-package :nobot/botscript/parser-impl)

(defun parse-string (str &key return-instance)
  (parse-source str :string
                :return-instance return-instance))

(defun parse-file (path &key return-instance)
  (parse-source path :file
                :return-instance return-instance))

(defun parse-source (source type &key return-instance)
  (with-disassembled-source (source type)   
    (with-state-context-run (:botscript-sort-types (get-tokens-source)
                                                   :entry-state script
                                                   :return-instance return-instance)
      (define-rule script ()
        (:and
         (:rule compiler-options)
         (:rule bot-options)
         (:rule data-decl-list)
         (:rule vertex-decl-list)))

      (define-rule compiler-options ()
        (:and
         (:keyword "c-opts")
         (:rule c-opts-block)))

      (define-rule bot-options ()
        (:and
         (:keyword "bot-opts")
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
         (:keyword "start")
         (:keyword "from")
         (:id)))

      (define-rule c-opts-block ()
        (:and
         (:delimiter "{")
         (:rule opts-list)
         (:delimiter "}")))

      (define-rule bot-opts-block ()
        (:and
         (:delimiter "{")
         (:rule opts-list)
         (:delimiter "}")))

      (define-rule data-decl ()
        (:and
         (:keyword "letd")
         (:id)
         (:rule data-expr)))

      (define-rule vertex-decl ()
        (:and
         (:keyword "letv")
         (:id)
         (:rule vertex-options)))

      (define-rule action-decl ()
        (:and
         (:keyword "def-act")
         (:id)
         (:rule stmt-block)))

      (define-rule stmt-block ()
        (:and
         (:delimiter "{")
         (:rule stmt-list)
         (:delimiter "}")))

      (define-rule opts-list ()
        (:or
         (:and
          (:rule opt)
          (:rule opts-tail-list))
         (:empty)))

      (define-rule opt ()
        (:and
         (:id)
         (:delimiter ":")
         (:rule string-or-num)))

      (define-rule opts-tail-list ()
        (:or
         (:and
          (:delimiter ",")
          (:rule opt)
          (:rule opts-tail-list))
         (:empty)))

      (define-rule string-or-num ()
        (:or
         (:string)
         (:num-string)))

      (define-rule data-expr ()
        (:rule data-seq))

      (define-rule vertex-options ()
        (:and
         (:delimiter "[")
         (:rule vertex-option-list)
         (:delimiter "]")))

      (define-rule stmt-list ()
        (:or
         (:and
          (:rule stmt)
          (:rule stmt-list))
         (:empty)))

      (define-rule  stmt ()
        (:and
         (:rule expr)
         (:delimiter :! "newline")))

      (define-rule data-seq ()
        (:and
         (:delimiter "[")
         (:rule item-list)
         (:delimiter "]")))

      (define-rule vertex-option-list ()
        (:or
         (:and
          (:rule vertex-option)
          (:delimiter ",")
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
          (:delimiter ",")
          (:rule item)
          (:rule item-tail-list))
         (:empty)))

      (define-rule item ()
        (:rule literal))

      (define-rule vertex-option ()
        (:and
         (:rule vertex-option-name)
         (:delimiter "=")
         (:rule vertex-option-val)))

      (define-rule vertex-option-name ()
        (:or
         (:keyword "act")
         (:keyword "type")))

      (define-rule vertex-option-val ()
        (:or
         (:id)
         (:keyword "in")
         (:keyword "out")))

      (define-rule literal ()
        (:or
         (:char-string)
         (:number-string)))

)))
