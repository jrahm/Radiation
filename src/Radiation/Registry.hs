module Radiation.Registry where

import Data.Map as M (Map, fromList, lookup, toList)
import Radiation.Parsers (Parser)
import System.IO
import System.Exit

import qualified Radiation.Parsers.Languages.C
import qualified Radiation.Parsers.Languages.CPP
import qualified Radiation.Parsers.Languages.Test
import qualified Radiation.Parsers.Languages.JavaScript

availabiltyMap :: Map String Parser
availabiltyMap = fromList
    [ 
          ("test", Radiation.Parsers.Languages.Test.parser)
        , ("c",    Radiation.Parsers.Languages.C.parser)
        , ("cpp",  Radiation.Parsers.Languages.CPP.parser)

        , ("javascript",  Radiation.Parsers.Languages.JavaScript.parser)
    ]

lookup :: String -> Maybe Parser
lookup = flip M.lookup availabiltyMap

lookupIO :: String -> IO Parser
lookupIO str = case M.lookup str availabiltyMap of
    Nothing -> hPutStrLn stderr ("Error: " ++ str ++ " no such parser") >> exitWith (ExitFailure 1) >> undefined
    Just x -> return x

available :: [String]
available = map fst $ toList availabiltyMap
