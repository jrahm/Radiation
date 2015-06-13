profile:
	git apply patches/cabal-profile.patch
	cabal build
	cp dist/build/radiation/radiation ~/.cabal/bin

optimize:
	git apply patches/cabal-optimize.patch
	cabal build
	cp dist/build/radiation/radiation ~/.cabal/bin
