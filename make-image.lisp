;; load nobot system
(load "nobot/nobot.asd")
(asdf:load-system :nobot)

;; make image
(sb-ext:save-lisp-and-die
 #p "nobot-pt"
 :toplevel #'nobot/startup:*run*
 :save-runtime-options t
 :executable t)

;; quit from sbcl
(quit)
