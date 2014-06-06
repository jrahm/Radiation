#!/bin/bash

cabal install

prefix=~/.vim
if [ -d $HOME/.vim/bundle ] ; then
    # Install with Pathogen
    prefix=$HOME/.vim/bundle
    echo Pathogen install detected\; copy to: $prefix
fi
cp -rv assets/* $prefix
