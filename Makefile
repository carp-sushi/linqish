.PHONY: all format build test clean

all: format build test

format:
	purs-tidy format-in-place src/**/*.purs test/**/*.purs

build:
	spago build

test:
	spago test

clean:
	rm -rf index.js output .spago
