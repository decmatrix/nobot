(uiop:define-package :nobot/botscript/parser
    (:use :cl
          :nobot/botscript/parser-utils)
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
    ))

(defun-state script ()
  )

(defun-state macros-block ()
  )

(defun-state predefined-block ()
  )

(defun-state definition/combo-block ()
  )

(defun-state graph-logic ()
  )

(defun-state exe-macros ()
  )

(defun-state use-predefined ()
  )

(defun-state call-definition/combo ()
  )

(defun-state call-definition ()
  )

(defun-state call-combo ()
  )

(defun-state predefines ()
  )

(defun-state predefined-list ()
  )

(defun-state args-list ()
  )

(defun-state arg ()
  )

(defun-state id ()
  )
