profile:
	- git apply patches/cabal-profile.patch
	cabal build

optimize:
	- git apply patches/cabal-optimize.patch
	cabal build

install:
	cp dist/build/radiation/radiation ~/.cabal/bin
