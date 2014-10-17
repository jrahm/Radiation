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
import qualified Radiation.Parsers.C as C
import qualified Radiation.Parsers.Python as Python

import My.Utils

availableParsers :: Map.Map String Parser
availableParsers = Map.fromList
    [  ("test",Test.parser)
     , ("c", C.parser)
     , ("python", Python.parser)
     , ("cpp", CPP.parser) ]

{- run a vim under the context of a data pipe. This data pipe
 - defines the way for the Vim monad to communicate with the host
 - program -}
runWithDataPipe :: Handle -> String -> String -> DataPipe -> IO ()
runWithDataPipe logh file typ pipe =
    let logf = hPutStrLn logh . ("[Info] - "++)
        test = runParser file <$> Map.lookup typ availableParsers in do

    when (isNothing test)  $
        putStrLn ("No parser for filetype " ++ typ) >> 
        exitWith (ExitFailure 117)

    logf $ "Found parser for file type " ++ typ ++ ": Running parser."
    void $ runVimM pipe (fromJust test)

main :: IO ()
main = withFile "/tmp/radiation.log" WriteMode $ \flog ->
       (>>=) getArgs $ \argv -> do
        let logf = hPutStrLn flog . ("[INFO] - "++)


        {- Start of main function -}
        logf "Starting Radiation"

        {- Checking argument length -}
        when (length argv < 2) $
            putStrLn "Not enough arguments. Takes filename and type" >>
            logf "Not enough arguments specified" >>
            exitWith (ExitFailure 1)

        {-Get the filename and the type of the file -}
        let (file:typ:_) = argv
    
        runWithDataPipe flog file typ =<< case argv of
            {- We need to figure out what type of connection to open to Vim. Is Vim running
             - as a server, or not. If it is a server, then we may take advantage of the
             - asynchonous nature and avoid from blocking the user. -}

            (_:_:addr:_as) -> 
                {- 3 arguments => server -}
                logf "Vim server detected. Running asynchronously" $>
                openServerDataPipe addr

            (_:_:_as) ->
                {- 2 arguments => sequential -}
                logf "No Vim server detected. Running sequentially" $>
                openSocketDataPipe stdout stdin

            _ ->
                {- < 2 arguments => WTF I checked for this earlier! -}
                logf "There was a nonsensical error." >>
                Prelude.error "The impossible!"

