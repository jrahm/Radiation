module Radiation.Parsers where

import Vim
import Debug.Trace

-- run the file provided
data Parser = Parser (String -> VimM ())

runParser :: String -> Parser -> VimM () 
runParser str (Parser func) = trace "Running parser" $ func str

highlight :: String -> [String] -> VimM ()
highlight high word = do
    vlog Debug $ "[RunningCommand]: " ++ "syn keyword " ++ high ++ " "  ++ unwords word
    post ("syn keyword " ++ high ++ " "  ++ unwords word)
