(uiop:define-package :nobot/botscript/parser
    (:use :cl)
  (:export #:parse-source
           #:parse-string
           #:parse-file))

(in-package :nobot/botscript/parser)

(defun parse-string (str)
  (parse-source str :type :string))

(defun parse-file (path)
  (parse-source path :type :file))

(defun parse-source (source type)
  (with-disassembled-source (source type)
    ))

(defun !main ()
  )

(defun @script ()
  )

(defun @macros-block ()
  )

(defun @predefined-block ()
  )

(defun @definition/combo-block ()
  )

(defun @graph-logic ()
  )

(defun @exe-macros ()
  )

(defun @use-predefined ()
  )

(defun @call-definition/combo ()
  )

(defun @call-definition ()
  )

(defun @call-combo ()
  )

(defun @predefines ()
  )

(defun @predefined-list ()
  )

(defun @args-list ()
  )

(defun @arg ()
  )
