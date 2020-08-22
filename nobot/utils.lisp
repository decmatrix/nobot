(uiop:define-package :nobot/utils
    (:use :cl
          :nobot/utils/common-utils
          :nobot/utils/context-variables)
  (:export
   ;; common utils package
   #:equals
   #:set-<>
   #:split-list
   #:define-constant-?
   #:reintern
   ;; context variables package
   #:defcontextvar
   #:setf-context-var))
