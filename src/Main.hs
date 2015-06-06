{-# LANGUAGE CPP #-}
module Main where

import Debug.Trace

import Control.Applicative
import Control.Arrow (second)
import Control.Monad
import Data.Maybe (isNothing,fromJust)
import My.Utils
import Radiation.Parsers
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.IO
import Vim

import qualified Data.Map as Map
import qualified Radiation.Parsers.C as C
import qualified Radiation.Parsers.CPP as CPP
import qualified Radiation.Parsers.Test as Test

availableParsers :: Map.Map String Parser
availableParsers = Map.fromList
    [  ("test",Test.parser)
     , ("c", C.parser)
     , ("cpp", CPP.parser)
     ]


withLogFile :: (Handle -> IO ()) -> IO ()
withLogFile fn = do
    dirname <- tempFolder
    withFile (dirname </> "radiation.log") WriteMode fn

main :: IO ()
main = do
    createDirectoryIfMissing True =<< tempFolder
    withLogFile $ \flog ->
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
        case argv of
            [_,_,"--requires"] ->
                {- The client code is attempting to figure out what
                 - this parser will need to complete its task -}
                let parser = Map.lookup typ availableParsers in

                case parser of
                    Nothing -> putStrLn "no such parser" >> exitWith (ExitFailure 1)
                    Just (Parser req _) ->
                        mapM_ putStrLn $ req file
            
            (_:_:arguments) ->
                let arguments' = map (break (=='=')) arguments
                    argmap = Map.fromList (map (second tail) arguments')
                    parser = Map.lookup typ availableParsers
                    in

                case parser of
                    Nothing -> putStrLn "no such parser" >> exitWith (ExitFailure 1)
                    Just (Parser _ fn) -> runVimM argmap file fn

            _ -> putStrLn "Error parsing command line parameters"
