;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/tests/utils
    (:nicknames :nobot-test-utils)
  (:import-from :nobot/utils
                :reintern)
  (:use :cl
        :lisp-unit)
  (:export #:run-test))

(in-package :nobot/tests/utils)

(defun run-test (name package)
  (let ((*summarize-results* nil)
        (*print-errors* t)
        (*print-failures* t))
    (run-tests (list (reintern name package)) package)))
