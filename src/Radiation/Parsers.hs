module Radiation.Parsers where

import Vim
import Debug.Trace

import Control.Monad
import qualified Data.Char as C

{- A parser has a couple of things. First is a list
 - of the variables it requires to complete. Second is
 - the function that actually radiates the file -}
data Parser = Parser (String -> [String]) (String -> VimM ())

runParser :: String -> Parser -> VimM () 
runParser str (Parser _ func) = func str

highlight :: String -> [String] -> VimM ()
highlight high word' =
    let word = filter (not . allSpace) word' in
    unless (null word) $ do
        vlog Debug $ "[RunningCommand]: " ++ "syn keyword " ++ high ++ " "  ++ unwords word
        post ("syn keyword " ++ high ++ " "  ++ unwords word)
    where
        allSpace = all C.isSpace
