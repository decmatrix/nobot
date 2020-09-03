build: clean
	sbcl --load nobot/nobot.asd \
	     --eval '(asdf:load-system :nobot)' \
		 --eval "(sb-ext:save-lisp-and-die #p\"nobot-app\":toplevel #'nobot/startup:run-nobot :executable t :compression 5)"
clean:
	rm nobot-app
