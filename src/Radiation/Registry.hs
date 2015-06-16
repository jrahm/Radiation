module Radiation.Registry where

import Data.Map as M (Map, fromList, lookup, toList)
import Radiation.Parsers (Parser(..))
import System.IO
import System.Exit

import System.FilePath

import qualified Radiation.Parsers.Languages.C
import qualified Radiation.Parsers.Languages.CPP
import qualified Radiation.Parsers.Languages.Test
import qualified Radiation.Parsers.Languages.JavaScript

availabiltyMap :: Map String Parser
availabiltyMap = fromList $ map (\p@(Parser typ _ _) -> (typ, p)) [ 
          Radiation.Parsers.Languages.Test.parser
        , Radiation.Parsers.Languages.C.parser
        , Radiation.Parsers.Languages.CPP.parser
        , Radiation.Parsers.Languages.JavaScript.parser
    ]

lookup :: String -> Maybe Parser
lookup = flip M.lookup availabiltyMap

lookupIO :: String -> IO Parser
lookupIO str = case M.lookup str availabiltyMap of
    Nothing -> hPutStrLn stderr ("Error: " ++ str ++ " no such parser") >> exitWith (ExitFailure 1) >> undefined
    Just x -> return x

ftDetect :: FilePath -> IO String
ftDetect fname =
    let (fname, ext) = splitExtension fname in
    if ext == "js" then return "javascript"
    else return ext

available :: [String]
available = map fst $ toList availabiltyMap
