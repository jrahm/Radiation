module Main where

import Vim
import System.Environment
import Control.Monad

import System.Exit
import System.IO

import qualified Data.Map as Map
import Radiation.Parsers

import Data.Maybe (isNothing,fromJust)
import Control.Applicative

import qualified Radiation.Parsers.Test as Test
import qualified Radiation.Parsers.CPP as CPP

availableParsers :: Map.Map String Parser
availableParsers = Map.fromList
    [  ("test",Test.parser)
     , ("cpp", CPP.parser) ]

runWithDataPipe :: String -> String -> DataPipe -> IO ()
runWithDataPipe file typ pipe =
    let test = runParser file <$> Map.lookup typ availableParsers in do
    when (isNothing test)  $
        putStrLn ("No parser for filetype " ++ typ) >> 
        exitWith (ExitFailure 117)
    void $ runVimM pipe (fromJust test)

main :: IO ()
main = (>>=) getArgs $ \argv -> do
    when (length argv < 2) $
        putStrLn "Not enough arguments. Takes filename and type" >>
        exitWith (ExitFailure 1)

    let (file:typ:_) = argv

    runWithDataPipe file typ $ case argv of
        (_:_:addr:_) -> openServerDataPipe addr
        (_:_:_) -> openSocketDataPipe stdout stdin
        _ -> Prelude.error "The impossible!"
    
