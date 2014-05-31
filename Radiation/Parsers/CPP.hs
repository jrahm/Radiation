{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.CPP where

import qualified Radiation.Parsers as R

import Control.Applicative
import Control.Monad.IO.Class

import Vim
import Prelude hiding (log)

import Data.Char as C
import qualified Data.Map as Map

import Data.Attoparsec.ByteString.Char8 as BP
import Data.Attoparsec.ByteString.Lazy as Lazy

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Text.Printf
import System.Process
import System.IO

import qualified Data.Set as Set

import My.Utils

{- Parser the C++ file. Look for classes, typedefs and namespaces -}
parseCPP :: Parser (Map.Map String (Set.Set BS.ByteString))
parseCPP = 
    let
        word = skipSpace >> BP.takeWhile sat >>= ((>>) skipSpace . return)
            where sat ch = C.isDigit ch || C.isAlpha ch || ch == '_'

        parseClass = string "class" *> fmap ("RadiationCppClass",) word

        parseTypedef = string "typedef" *> (((,)"RadiationCppTypedef" . last . BSC.words) <$> BP.takeWhile (/=';'))

        parseNamespace = string "namespace" *> fmap ("RadiationCppNamespace",) word

        one = choice [ fmap return parseClass,
                     fmap return parseTypedef,
                     fmap return parseNamespace,
                     anyChar >> return [] ] in

   (fromList' . concat) <$> many one

   where fromList' :: (Ord a, Ord b) => [(a,b)] -> Map.Map a (Set.Set b)
         fromList' = foldl (\mp (k,v) ->
            Map.insertWith Set.union k (Set.singleton v) mp) Map.empty

parser :: R.Parser
parser = R.Parser $ \filename -> do
    openLogFile "/tmp/cpp_radiation.log" Debug
    log Info "Start cpp parser"

    cc <- queryDefault "g:radiation_cc" "gcc"
    flags <- queryDefault "g:radiation_c_flags" ""

    {- The command to run -}
    let command = printf "%s %s -E %s" cc flags filename
    log Info $ "[RunningCommand]: " ++ command

    {- Get the utilities to parse the output -}
    (_, stout, sterr, _) <- liftIO $ runInteractiveCommand command
    filestr <- liftIO (BSL.hGetContents stout)

    {- Parse the cpp file after it has run through the preprocessor -}
    case Lazy.parse parseCPP filestr of
        
        {- The parser failed and we report that -}
        Lazy.Fail _ _ err -> do
            liftIO (hGetContents sterr) >>= log Error
            log Error $ "Unable to parse: " ++ err

        {- The processor finished and worked,
         - so we report that too -}
        Lazy.Done _ val -> do
            log Info $ "Match " ++ show val
            Map.toList val <<? \(hi,v) -> R.highlight hi (map BSC.unpack (Set.toList v))

