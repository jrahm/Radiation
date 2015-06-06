module Radiation.Parsers (runParser, highlight) where

import Vim
import Debug.Trace

import Control.Monad
import qualified Data.Char as C

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

highlight :: String -> [String] -> VimM ()
highlight high word' =
    let word = filter (not . allSpace) word' in
    unless (null word) $ do
        vlog Debug $ "[RunningCommand]: " ++ "syn keyword " ++ high ++ " "  ++ unwords word
        post ("syn keyword " ++ high ++ " "  ++ unwords word)
    where
        allSpace = all C.isSpace
