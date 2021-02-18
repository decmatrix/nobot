;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/botscript/parser/acacia/acacia-runner
    (:use :cl)
  (:import-from :alexandria
                #:with-gensyms)
  (:import-from :nobot/botscript/lexer/token
                #:get-position)
  (:import-from :nobot/botscript/parser/acacia/result-packaging
                #:pack-parse-tree)
  (:import-from :nobot/toplevel/error-handling
                #:raise-bs-parser-error)
  (:import-from :nobot/botscript/parser/acacia/parser-generator
                #:rule->)
  (:import-from :nobot/botscript/parser/acacia/configuration
                #:*acacia-configuration*
                #:make-acacia-configuration
                #:$conf-get-start-rule
                #:$conf-get-source
                #:$conf-get-source-type
                #:$conf-pointer-at-end-?)
  (:export #:with-acacia-runner))

(in-package :nobot/botscript/parser/acacia/acacia-runner)

(defmacro with-acacia-runner (((&rest config-args) &key pack-result) &body body)
  (with-gensyms (parse-tree)
    `(let ((*acacia-configuration*
            (make-acacia-configuration
             ,@config-args)))
       ,@body
       (let ((,parse-tree
              (parse
               ($conf-get-start-rule))))
         (if ,pack-result
             (pack-parse-tree ,parse-tree)
             ,parse-tree)))))

(defun parse (start-rule)
  (let ((res (rule-> start-rule)))
    (unless ($conf-pointer-at-end-?)
      (raise-bs-parser-error
       "expected end of source~a"
       (if (eq ($conf-get-source-type) :file)
           (format nil ", file: ~a."
                   ($conf-get-source))
           ".")))
    res))
