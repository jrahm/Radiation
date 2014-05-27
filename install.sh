#!/bin/bash

cabal configure
cabal build

if [ ! $PREFIX ] ; then
    PREFIX=/usr/
fi

cp dist/build/radiation/radiation $PREFIX/bin/

cp -rv assets/* ~/.vim/
