all: build

build:
	agda -c --ghc-dont-call-ghc --no-main app/Test.agda
	cabal build

clean:
	rm -rf dist-newstlye
	rm -rf app/MAlonzo
	find app -type f -name '*.agdai' -delete
