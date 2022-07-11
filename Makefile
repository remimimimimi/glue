.PHONY: all build

all: build

build:
	agda -c --ghc-dont-call-ghc --no-main app/Test.agda
	cabal build
