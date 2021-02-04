# Copyright (c) 2021 Bohdan Sokolovskyi
# Author: Bohdan Sokolovskyi <sokol.chemist@gmail.com>

.PHONY: clean

project-dir=nobot/

build:
	sbcl --load make-image.lisp

clean:
	rm nobot-app || :
	(find $(project-dir) -name '*.fasl' -print0 | xargs -0 rm ||:)
