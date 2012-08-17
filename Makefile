.PHONY: all build dist install test clean doc p

all: build test

build: dist/setup-config
	grep -E "$    " Memo.md | sed 's/$     //' > Memo.hs
	cabal build

dist: test
	cabal sdist

install: build
	cabal install

test: build
	cabal test

clean:
	cabal clean

dist/setup-config: daemons.cabal
	cabal configure --enable-tests

doc: build
	cabal haddock

p: clean
	permamake.sh $(shell find */ -name '*.hs') *.cabal Makefile *.md
