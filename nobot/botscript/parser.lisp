(uiop:define-package :nobot/botscript/parser
    (:use :cl
          :nobot/botscript/parser-utils)
  (:import-from :nobot/botscript/parser-utils
                #:@goto
                #:@revert-current-tree
                #:@is-token-of-type
                #:@is-token-of-value
                #:@next-token
                #:@prev-token)
  (:import-from :nobot/botscript/types
                #:get-sort-type-symbol)
  (:import-from :nobot/botscript/lexer-utils
                #:get-symbol-for-keyword)
  (:export #:parse-source
           #:parse-string
           #:parse-file))

(in-package :nobot/botscript/parser)

(defun parse-string (str)
  (parse-source str :string))

(defun parse-file (path)
  (parse-source path :file))

(defun parse-source (source type)
  (with-disassembled-source (source type)   
    (with-state-context-run (:botscript-sort-types script)

      (defun-state script ()
        (and
         (@goto macros-block)
         ;;(@goto predefined-block)
         ;;(@goto definition/combo-block)
         ;;(@goto graph-logic)
         ))

      ;;TODO: error check
      (defun-state macros-block ()
        (@goto exe-macros-list))

      ;;TODO: error check
      (defun-state predefined-block ()
        (labels ((%predefined-block ()
                   (if (@goto use-predefined)
                       (%predefined-block)
                       t)))))

      ;;TODO: error check
      (defun-state definition/combo-block ()
        (labels ((%definition/combo-block ()
                   (if (@goto call-definition/combo)
                       (%definition/combo-block)
                       t)))))

      (defun-state graph-logic ()
        t)

      (defun-state exe-macros-list ()
        (labels ((%exe-macros-list ()
                   (if (@goto exe-macros)
                       (%exe-macros-list)
                       t)))
          (%exe-macros-list)))
      
      (defun-state exe-macros ()
        (with-token (:next)
          (if (@is-token-of-value it (get-symbol-for-keyword "#EXE"))
              (and (@is-token-of-type (@next-token) :id)
                   (@goto args-list))
              (@revert-current-tree))))

      (defun-state use-predefined ()
        (and
         (@is-token-of-value (@next-token) '!use)
         (@goto predefines))) ;;TODO: rename sort predefines

      (defun-state call-definition/combo ()
        (or
         (@goto call-definition)
         (@goto call-combo)))

      (defun-state call-definition ()
        (and
         (@is-token-of-value (@next-token) '@def)
         (@is-token-of-type (@next-token) :id)
         (@goto args-list)))

      (defun-state call-combo ()
        (and
         (@is-token-of-value (@next-token) '$combo)
         (@is-token-of-type (@next-token) :id)))

      (defun-state predefines ()
        (@is-token-of-type (@next-token) :id))

      (defun-state predefined-list ()
        t)

      (defun-state args-list ()
        (labels ((%args-list ()
                   (if (@goto arg)
                       (%args-list)
                       t)))
          (%args-list)))

      (defun-state arg ()
        (let ((current-token (@next-token)))
          (or
           (@is-token-of-type current-token :id)
           (@is-token-of-type current-token :number-string))))

      )))
