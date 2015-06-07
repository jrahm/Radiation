{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers (runParser, highlight, Parser(..)) where

import Vim (Variable, VimM(..), vlog, post, LogLevel(..))

import Control.Monad (unless)
import Data.Char (isAlphaNum)

import My.Utils ((+>+))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BSC (all)

import Data.Convertible (Convertible, convert)
import Data.List (intersperse)

{- A parser has a couple of things. First is a list
 - of the variables it requires to complete. Second is
 - the function that actually radiates the file -}
data Parser = Parser {
    {- Get the dependencies for the the file to be run.
     - This means return the list of variables a parser
     - needs to complete. -}
      _dependencies :: FilePath -> [Variable]

    {- Run the parser and highlight all the commands -}
    , _highlight :: FilePath -> VimM ()
}

{- Take a parser and run it on the file passed to it. -}
runParser :: FilePath -> Parser -> VimM () 
runParser str (Parser _ func) = func str

highlight :: (Convertible a ByteString, Convertible b ByteString) => a -> [b] -> VimM ()
highlight high word' =
    let highlight' highlighting words = 
            let word = filter (BSC.all isAlphaNum) words
                wordbs = mconcat $ intersperse " "  word
                in
            unless (null word) $ do
                let command = "syn keyword " +>+ highlighting +>+ " " +>+ wordbs 
                vlog Debug $ "[RunningCommand]: " +>+ command
                post command
    in highlight' (convert high) (map convert word')
