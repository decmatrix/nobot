(uiop:define-package :nobot/botscript/parser
    (:use :cl
          :nobot/botscript/parser-utils)
  (:import-from :nobot/botscript/parser-utils
                #:@goto
                #:@is-token-of-type
                #:@is-token-of-value
                #:@next-token)
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
         (@goto predefined-block)
         (@goto definition/combo-block)
         (@goto graph-logic)))

      ;;TODO: error check
      (defun-state macros-block ()
        (labels ((%macros-block ()
                   (if (@goto exe-macros)
                       (%macros-block)
                       t)))
          (%macros-block)))

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
      
      (defun-state exe-macros ()
        (and
         (@is-token-of-value (@next-token) '|#exe|)
         (@is-token-of-type (@next-token) :id)
         (@goto args-list)))

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
