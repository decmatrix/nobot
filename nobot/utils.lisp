;;;; Copyright (c) 2021 Bohdan Sokolovskyi
;;;; Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


(uiop:define-package :nobot/utils
    (:nicknames :nobot-utils)
  (:use :cl
        :nobot/utils/program-utils
        :nobot/utils/common-utils)
  (:reexport :nobot/utils/program-utils
             :nobot/utils/common-utils))
