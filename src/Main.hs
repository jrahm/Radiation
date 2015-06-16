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

import Data.Map (fromList)
import qualified Radiation.Registry as Registry

withLogFile :: (Handle -> IO ()) -> IO ()
withLogFile fn = do
    dirname <- tempFolder
    withFile (dirname </> "radiation.log") WriteMode fn

printHelp :: IO ()
printHelp = mapM_ putStrLn 
    [ "Radiation: context aware syntax highlighting for Vim"
    , "Usage: radiation --available"
    , "       radiation <filename> <file type> --requires"
    , "       radiation <filename> <file type> [[variables] ...]"
    ]

main :: IO ()
main = do
    createDirectoryIfMissing True =<< tempFolder
    withLogFile $ \flog ->
       (>>=) getArgs $ \argv -> do
        let logf = hPutStrLn flog . ("[INFO] - "++)

        {- Start of main function -}
        logf "Starting Radiation"


        {-Get the filename and the type of the file -}
        case argv of
            ["--available"] -> mapM_ putStrLn Registry.available

            [file,typ',"--requires"] -> do
                {- The client code is attempting to figure out what
                 3a- this parser will need to complete its task -}
                typ <- getType file typ'
                (Parser _ req _) <- Registry.lookupIO typ
                mapM_ putStrLn $ req file
            
            (file:typ':arguments) ->
                let arguments' = map (break (=='=')) arguments
                    argmap = fromList (map (second tail) arguments')
                    in do
                typ <- getType file typ'
                (Parser typ _ fn) <- Registry.lookupIO typ

                runVimM argmap file $ \fp -> do
                    openLogFilePortable ("radiation_"++typ++".log") Debug
                    fn fp

            _ -> putStrLn "Error parsing command line parameters" >> printHelp
    where
        getType fname "--detect" = Registry.ftDetect fname
        getType _ t = return t
