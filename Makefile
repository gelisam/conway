NAME="$(shell basename `pwd`)"

.PHONY: all build doc demo clean clobber

all: build doc test

build:
	cabal build

doc:
	find src demo -name '*.hs' | xargs haddock --odir=doc --html

test:
	find src demo -name '*.hs' | xargs doctest

demo: build
	./dist/build/$(NAME)-demo/$(NAME)-demo


clean:
clobber: clean
	rm -rf dist
