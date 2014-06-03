{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.C(parser) where

import qualified Radiation.Parsers as R

import Control.Applicative
import Control.Monad

import Vim
import Prelude hiding (log)

import qualified Data.Map as Map

import Data.Attoparsec.ByteString.Char8 as BP

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set

import My.Utils

import Radiation.Parsers.Internal.CStyle
import Radiation.Parsers.Internal.CommandParser
import Radiation.Parsers.Internal.InternalIO
import Radiation.Parsers.Internal.WithAttoparsec

{- Keep radiation from highlighting already
 - defined keywords -}
blacklist :: Set.Set BS.ByteString
blacklist = Set.fromList [
            "auto"    ,"else"  ,"long"    ,"switch",
            "break"   ,"enum"  ,"register","typedef",
            "case"    ,"extern","return"  ,"union",
            "char"    ,"float" ,"short"   ,"unsigned",
            "const"   ,"for"   ,"signed"  ,"void",
            "continue","goto"  ,"sizeof"  ,"volatile",
            "default" ,"if"    ,"static"  ,"while",
            "do"      ,"int"   ,"struct"  ,"_Packed",
            "double" ]

typMap :: Map.Map String String
typMap = Map.fromList [
        ("struct","RadiationCStruct"),
        ("union","RadiationCUnion"),
        ("enum","RadiationCEnum")]
                

{- Parsec Monad for C files -}
parseC :: Parser (Map.Map String (Set.Set BS.ByteString))
parseC = let
        {- Parse a typedef and return pairs of highlights
         - to keywords -}
        parseTypedef :: Parser [(String,BSC.ByteString)]
        parseTypedef = do
                _ <- string "typedef"

                (do 
                    {- parse the typedef of template:
                     - typedef struct [name] { ... } [ident] -}
                    typ <- skipSpace *> (choice . map string) (map BSC.pack $ Map.keys typMap)
                    id1' <- (option Nothing (Just <$> identifier))
                    let id1 = (,) <$> Map.lookup (BSC.unpack typ) typMap <*> id1'

                    (addJust id1 . return . ("RadiationCTypedef",)) <$> (skipSpace *> (option "" body) *> identifier)
                    ) <|>
                    {- Or as the original typedef ... [ident]; -}
                    ((return . ("RadiationCTypedef",) . last . BSC.words) <$> BP.takeWhile (/=';'))

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

    pipes <- sequence
        [queryDefault "g:radiation_c_cc" "cc",
         queryDefault "g:radiation_c_flags" "",
         pure "-E", pure filename] >>= runCommand

    reportErrors pipes $
        withParsingMap (Map.map (Set.\\blacklist) <$> parseC) <=< vGetHandleContents