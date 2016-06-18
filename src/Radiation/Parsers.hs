{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Radiation.Parsers (
    runParser, highlight, Parser(..),
    syn, Keyword(..), SynArg(..),
    Link(..), HiArg(..), hiLink, synKeywordS) where

import Vim (Variable, VimM(..), vlog, post, LogLevel(..), openLogFilePortable)

import Control.Monad (unless)
import Data.Monoid (mconcat, mappend)
import Data.Char (isAlphaNum, toLower)

import My.Utils ((+>+))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BSC (all, null, putStrLn, pack)
import qualified Data.ByteString.Char8 as BSC (map)

import Data.Convertible (Convertible, convert)
import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO)

bscToLower :: BS.ByteString -> BS.ByteString
bscToLower = BSC.map toLower

{- A parser has a couple of things. First is a list
 - of the variables it requires to complete. Second is
 - the function that actually radiates the file -}
data Parser = Parser {
    {- The filetype this parser works on -}
      _fttype :: String

    {- Get the dependencies for the the file to be run.
     - This means return the list of variables a parser
     - needs to complete. -}
    , _dependencies :: FilePath -> [Variable]

    {- Run the parser and highlight all the commands -}
    , _highlight :: FilePath -> VimM ()
}

{- Take a parser and run it on the file passed to it. -}
runParser :: FilePath -> Parser -> VimM () 
runParser str (Parser _ _ func) = func str

isIdentifier :: ByteString -> Bool
isIdentifier bs = not (BSC.null bs) && (BSC.all $ \c -> isAlphaNum c || c == '_') bs

data Keyword = Keyword
data Link = Link

class SynArg a r where
    next :: a -> r

class HiArg a r where
    hi :: a -> r

instance (Convertible a ByteString, Convertible b ByteString) => 
            SynArg Keyword (a -> [b] -> VimM()) where
    next _ = highlight

instance HiArg Link (ByteString -> ByteString -> VimM()) where
    hi _ highlight link = 
            let (highlight', link') = (convert highlight::ByteString, convert link::ByteString)
                command = "hi def link " +>+ highlight' +>+ " " +>+ link'
            in do
                vlog Debug $ "[RunningCommand]: " +>+ command
                post command
    

syn :: SynArg x r => x -> r
syn = next

hiLink :: ByteString -> ByteString -> VimM()
hiLink = hi Link

synKeywordS :: String -> [String] -> VimM () 
synKeywordS group vals = syn Keyword (BSC.pack group) (map BSC.pack vals)

highlight :: (Convertible a ByteString, Convertible b ByteString) => a -> [b] -> VimM ()
highlight high word' =
    let highlight' highlighting words = 
            let word = filter (\word -> isIdentifier word && bscToLower word /= "contains") words
                wordbs = mconcat $ intersperse " "  word
                in do

            liftIO $ BSC.putStrLn $ "\" " +>+ highlighting +>+ " " +>+ wordbs
            liftIO $ BSC.putStrLn $ "\" word=" +>+ BSC.pack (show word)
            unless (Prelude.null word) $ do
                let command = "syn keyword " +>+ highlighting +>+ " " +>+ wordbs 
                vlog Debug $ "[RunningCommand]: " +>+ command
                liftIO $ BSC.putStrLn command
                post command
    in highlight' (convert high) (map convert word')
