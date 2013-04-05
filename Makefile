CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)

all: build test

.PHONY: all build dist install clean doc p test ghci

build: dist/setup-config
	$(CABAL) build

dist: build
	cabal sdist

install: build
	cabal install

test: build
	$(CABAL) test

clean:
	$(CABAL) clean
	rm -rf cabal-dev/

dist/setup-config: daemons.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	$(CABAL) configure --enable-tests || $(CABAL) install --enable-tests

doc: build
	$(CABAL) haddock

p:
	permamake.sh $(shell find src/ -name '*.hs') \
	             $(shell find test/ -name '*.hs') \
	             *.cabal \
	             Makefile

ghci: build
	cabal-dev ghci
