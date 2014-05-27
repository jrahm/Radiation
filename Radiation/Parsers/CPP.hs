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
import Data.Attoparsec.Combinator
import Data.Maybe (catMaybes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Text.Printf
import System.Process
import System.IO

import Data.Foldable (forM_)
import qualified Data.Set as Set

parseCPP :: Parser (Map.Map String (Set.Set BS.ByteString))
parseCPP = 
    let
        word = skipSpace >> BP.takeWhile sat >>= ((>>) skipSpace . return)
            where sat ch = C.isDigit ch || C.isAlpha ch || ch == '_'
        parseClass = string "class" >> word >>= return . ("RadiationCppClass",)
        parseTypedef = string "typedef" >> word >> word >>= return . ("RadiationCppTypedef",)

        one = do
            choice [ fmap return parseClass,
                     fmap return parseTypedef,
                     anyChar >> return [] ] in

   fmap (fromList' . concat) $ many one

   where fromList' :: (Ord a, Ord b) => [(a,b)] -> Map.Map a (Set.Set b)
         fromList' = foldl (\mp (k,v) ->
            Map.insertWith Set.union k (Set.singleton v) mp) Map.empty

parser :: R.Parser
parser = R.Parser $ \filename -> do
    openLogFile "/tmp/radiation.log" Debug
    log Info "Start cpp parser"

    cc <- queryDefault "g:radiation_cc" "gcc"
    flags <- queryDefault "g:radiation_c_flags" ""

    let command = printf "%s %s -E %s" cc flags filename
    log Info $ "[RunningCommand]: " ++ command
    (_, stout, sterr, _) <- liftIO $ runInteractiveCommand command
    filestr <- liftIO (BS.hGetContents stout)

    case parseOnly parseCPP filestr of
        Left err -> do
            liftIO (hGetContents sterr) >>= log Error
            log Error $ "Unable to parse: " ++ err
        Right val -> do
            log Info $ "Match " ++ show val
            forM_ (Map.toList val) $ \(hi,v) -> R.highlight hi (map BSC.unpack (Set.toList v))
