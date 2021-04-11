# Copyright (c) 2021 Bohdan Sokolovskyi
# Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


# config variables
project-dir=nobot/
SHELL=/bin/bash

.PHONY: clean

all: build install

build: clean
	mkdir release
	cp -r ./nobot/botlib release/
	sbcl --disable-debugger \
		 --load make-image.lisp

help:
	@echo "all   build   help   install   uninstall   clean"

install:

uninstall:

clean:
	(rm -rf release || :)
	(find $(project-dir) -name '*.fasl' -print0 | xargs -0 rm || :)
