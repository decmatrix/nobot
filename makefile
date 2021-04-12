# Copyright (c) 2021 Bohdan Sokolovskyi
# Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>


# config variables
project-dir=nobot/

OS:=$(shell uname -s)

.PHONY: all build install uninstall clean

all: build install

build: clean
	mkdir release
	mkdir release/bin
	cp -r ./nobot/botlib release/
	sbcl --disable-debugger \
		 --load make-image.lisp

install: build
	mv release nobot-platform
ifeq ($(OS),Darwin)
	sudo cp -r nobot-platform /usr/local/Cellar/
	sudo ln -s /usr/local/Cellar/nobot-platform/bin/nobot-pt.bin /usr/local/bin/nobot
else ifeq ($(OS),Linux)
	sudo cp -r nobot-platform /usr/local/
	sudo ln -s /usr/local/nobot-platform/nobot-pt.bin /usr/local/bin/nobot
else
	$(error unsupported operation system: $(OS))
endif

uninstall: clean
ifeq ($(OS),Darwin)
	sudo rm -rf /usr/local/Cellar/nobot-platform/ && rm -rf /usr/local/bin/nobot
else ifeq ($(OS),Linux)
	sudo rm -rf /usr/local/nobot-platform/ && rm -rf /usr/local/bin/nobot
else
	$(error unsupported operation system)
endif

clean:
	(rm -rf release nobot-platform || :)
	(find $(project-dir) -name '*.fasl' -print0 | xargs -0 rm || :)
