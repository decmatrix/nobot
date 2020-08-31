build:
	sbcl --load nobot/nobot.asd \
		--eval '(asdf:load-system :nobot)' \
		--eval '(asdf:make :nobot)' \
		--eval '(quit)' \
clean:
	rm **/nobot-app
