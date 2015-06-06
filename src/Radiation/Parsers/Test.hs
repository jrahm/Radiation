{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Test(parser) where

import Radiation.Parsers

import Control.Applicative
import Control.Monad.IO.Class

import Vim
import Prelude hiding (log)

import Data.ByteString.Char8 as BS (readFile, lines, ByteString)

{- Very basic test parser. Reads data from crack lib and highlights
 - all the words from it -}
parser :: Parser
parser = Parser (const []) $ \_ -> do
    openLogFilePortable "test_radiation.log" Debug

    logs Info "Start test parser"

    highlight ("Userdef1"::ByteString) =<<
        liftIO (BS.lines <$> BS.readFile "/usr/share/dict/cracklib-small")
