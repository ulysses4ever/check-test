all:
	if [ ! -d "./.cabal-sandbox" ]; then cabal sandbox init; fi
	cabal install -j

install:
	cp ./.cabal-sandbox/bin/check-test ~/bin/check-test

