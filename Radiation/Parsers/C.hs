{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.C where

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

import Text.Printf
import My.Utils

blacklist :: Set.Set BS.ByteString
blacklist = Set.fromList [
                "int", "char", "unsigned", "signed",
                "long", "short", "float", "double"
            ]

parseC :: Parser (Map.Map String (Set.Set BS.ByteString))
parseC = let
        identifier = skipSpace >> BP.takeWhile sat >>= ((>>) skipSpace . return)
            where sat ch = C.isDigit ch || C.isAlpha ch || ch == '_'

        body = skipSpace *> char '{' *> (body <|> BP.takeWhile (\c->c/='{' && c/='}')) <* char '}'

        parseTypedef :: Parser [(String,BSC.ByteString)]
        parseTypedef = do
                _ <- string "typedef"

                (do 
                    let typMap = Map.fromList [
                            ("struct","RadiationCStruct"),
                            ("union","RadiationCUnion"),
                            ("enum","RadiationCEnum")]

                    typ <- skipSpace *> (choice . map string) ["struct","enum","union"]
                    id1' <- (option Nothing (Just <$> identifier))
                    let id1 = (,) <$> Map.lookup (BSC.unpack typ) typMap <*> id1'

                    (addJust id1 . return . ("RadiationCTypedef",)) <$> (skipSpace *> (option "" body) *> identifier)
                    ) <|> ((return . ("RadiationCTypedef",) . last . BSC.words) <$> BP.takeWhile (/=';'))

        parseFunction :: Parser [(String, BSC.ByteString)]
        parseFunction = do
            _ <- identifier
            _ <- many (skipSpace *> char '*')
            name <- identifier
            _ <- skipSpace *> char '('
            return [("RadiationCFunction",name)]
        in
        (fromList' . concat) <$> many (choice
            [ parseTypedef,
              parseFunction,
              anyChar $> []])
   where fromList' :: (Ord a, Ord b) => [(a,b)] -> Map.Map a (Set.Set b)
         fromList' = foldl (\mp (k,v) ->
            Map.insertWith Set.union k (Set.singleton v) mp) Map.empty
         addJust :: Maybe a -> [a] -> [a]
         addJust Nothing = id
         addJust (Just x) = (x:)

parser :: R.Parser
parser = R.Parser $ \filename -> do
    openLogFile "/tmp/c_radiation.log" Debug
    vlog Info "Starting C Parser"

    cc <- queryDefault "g:radiation_c_cc" "cc"
    flags <- queryDefault "g:radiation_c_flags" ""

    let command = printf "%s %s -E %s" cc flags filename
    vlog Info $ "Running command: " ++ command

    (_, stout, sterr, _) <- liftIO $ runInteractiveCommand command
    filestr <- liftIO (BSL.hGetContents stout)

    {- Parse the cpp file after it has run through the preprocessor -}
    case Lazy.parse parseC filestr of
        
        {- The parser failed and we report that -}
        Lazy.Fail _ _ err -> do
            liftIO (hGetContents sterr) >>= log Error
            vlog Error $ "Unable to parse: " ++ err

        {- The processor finished and worked,
         - so we report that too -}
        Lazy.Done _ val' -> do
            let val = Map.map (Set.\\blacklist) val'
            vlog Info $ "Match " ++ show val
            Map.toList val <<? \(hi,v) -> R.highlight hi (map BSC.unpack (Set.toList v))
