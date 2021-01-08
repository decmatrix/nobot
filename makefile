.PHONY: clean

build:
	sbcl --load make-image.lisp

clean:
	rm nobot-app
