(uiop:define-package :nobot/collections
    (:use :cl
          :nobot/collections/lazy-calculus)
  (:nicknames :nobot-collections)
  (:export
   ;; lazy calculus
   #:calc-lazy-node
   #:lazy!
   ))
