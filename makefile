.PHONY: clean

build:
	sbcl --load nobot/nobot.asd \
	     --eval '(asdf:load-system :nobot)' \
		 --eval "(sb-ext:save-lisp-and-die #p\"nobot-app\" :toplevel #'nobot/startup:start :executable t :compression 5)"
clean:
	rm nobot-app
