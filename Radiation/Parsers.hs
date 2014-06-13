module Radiation.Parsers where

import Vim
import Debug.Trace

import Control.Monad
import qualified Data.Char as C

-- run the file provided
data Parser = Parser (String -> VimM ())

runParser :: String -> Parser -> VimM () 
runParser str (Parser func) = trace "Running parser" $ func str

highlight :: String -> [String] -> VimM ()
highlight high word' =
    let word = filter (not . allSpace) word' in
    unless (null word) $ do
        vlog Debug $ "[RunningCommand]: " ++ "syn keyword " ++ high ++ " "  ++ unwords word
        post ("syn keyword " ++ high ++ " "  ++ unwords word)
    where
        allSpace = all C.isSpace
