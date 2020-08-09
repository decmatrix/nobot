(uiop:define-package :nobot/utils
    (:use :cl
          :nobot/utils/common-utils
          :nobot/utils/context-variables)
  (:nicknames :nobot-utils)
  (:export
   ;; common utils package
   :equals
   :set-<>
   :split-list
   ;; context variables package
   #:define-context-var
   #:setf-context-var))
