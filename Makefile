NAME="$(shell basename `pwd`)"

.PHONY: all config build doc demo clean clobber

all: build doc test

config: dist/setup-config

dist/setup-config:
	cabal configure

build: config
	cabal build | cat

doc:
	find src demo -name '*.hs' | xargs haddock --no-warnings --odir=doc --html

test: build
	find src demo -name '*.hs' | xargs doctest
	@echo

demo: build
	./dist/build/$(NAME)-demo/$(NAME)-demo


clean:
clobber: clean
	rm -rf dist
