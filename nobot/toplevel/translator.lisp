;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/toplevel/translator
    (:use :cl)
  (:import-from :nobot/botscript
                #:parse-file
                #:botscript-post-processing)
  (:import-from :nobot/toplevel/context
                #:with-translator-context
                #:regist)
  (:import-from :nobot/projectgen
                #:generate-project)
  (:import-from :nobot/codegen
                #:generate-code)
  (:import-from :nobot/toplevel/logger
                #:configure-logger)
  (:import-from :nobot/logger
                #:with-logger)
  (:import-from :nobot/toplevel/error-handling
                #:toplevel-error-handler)
  (:export #:*run-and-burn*
           #:*run-and-burn-in-runtime*
           #:*run-and-burn-as-server*))

(in-package :nobot/toplevel/translator)

(defun *run-and-burn* (file)
  (with-translator-context (:source-type :file
                            :source file)
    (with-logger ((configure-logger))
      (toplevel-error-handler
        ;; Level 1: parse source file and get instance with tree of code
        (regist :parser (parse-file file :return-instance t)) ;; remove passing args, use context
        ;; Level 2: post parsing processing
        (regist :post-processing (botscript-post-processing))
        ;; Level 3: generate project
        (regist :project-generation (generate-project))
        ;; Level 4: generate code
        (regist :code-generation (generate-code))
        ;; Level 5: final processing
        ;; ???
        ))))

(defun *run-and-burn-in-runtime* ()
  ;; WIP
  )

(defun *run-and-burn-as-server* (&key (port 8086))
  (declare (ignore port))
  ;; WIP
  )
