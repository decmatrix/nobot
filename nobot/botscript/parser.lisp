(uiop:define-package :nobot/botscript/parser
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

(in-package :nobot/botscript/parser)

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

     (defun-state script ()
       (and
        (@goto macros-block)
        (@goto predefined-block)
        (@goto definition/combo-block)
        (@goto graph-logic)))

     (defun-state macros-block ()
       (labels ((%macros-block ()
                  (if (@goto exe-macros)
                      (%macros-block)
                      t)))
         (%macros-block)))

     (defun-state predefined-block ()
       (labels ((%predefined-block ()
                  (if (@goto use-predefined)
                      (%predefined-block)
                      t)))
         (%predefined-block)))

     (defun-state definition/combo-block ()
       (labels ((%definition/combo-block ()
                  (if (or (@goto call-definition)
                          (@goto call-combo))
                      (%definition/combo-block)
                      t)))
         (%definition/combo-block)))

     (defun-state graph-logic ()
       t)
           
     (defun-state exe-macros ()
       (with-token (:next)
         (or
          (when (@is-token-of-value it (get-symbol-for-keyword "#EXE"))
            (and (@is-token-of-type (@next-token) :id)
                 (@goto args-list)))
          (@revert-state-action))))

     (defun-state use-predefined ()
       (or
        (when (@is-token-of-value (@next-token) (get-symbol-for-keyword "!USE"))
          (@is-token-of-type (@next-token) :id))
        (@revert-state-action)))

     (defun-state call-definition ()
       (or
        (when (@is-token-of-value (@next-token) (get-symbol-for-keyword "@DEF"))
          (and (@is-token-of-type (@next-token) :id)
               (@goto args-list)))
        (@revert-state-action)))

     (defun-state call-combo ()
       (or
        (when (@is-token-of-value (@next-token) (get-symbol-for-keyword "$COMBO"))
          (@is-token-of-type (@next-token) :id))
        (@revert-state-action)))

     (defun-state args-list ()
       (labels ((%args-list ()
                  (if (@goto arg)
                      (%args-list)
                      t)))
         (%args-list)))

     (defun-state arg ()
       (with-token (:next)
         (or (@is-token-of-type it :id)
             (@is-token-of-type it :number-string)
             (@revert-state-action)))))))
