(uiop:define-package :nobot/toplevel/translator
    (:use :cl)
  (:import-from :nobot/botscript
                #:parse-file)
  (:export #:*run-and-burn*))

(in-package :nobot/toplevel/translator)

(defun *run-and-burn* (file)
  )
