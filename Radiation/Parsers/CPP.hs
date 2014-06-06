{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.CPP(parser) where

import qualified Radiation.Parsers as R

import Control.Applicative
import Control.Monad

import Vim
import Prelude hiding (log)

import Data.Char as C
import qualified Data.Map as Map

import Data.Attoparsec.ByteString.Char8 as BP

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Set as Set

import My.Utils

import Radiation.Parsers.Internal.CommandParser
import Radiation.Parsers.Internal.InternalIO
import Radiation.Parsers.Internal.WithAttoparsec


blacklist :: Set.Set BS.ByteString
blacklist = Set.fromList [
            "auto"    ,"else"  ,"long"        ,"switch",
            "break"   ,"enum"  ,"register"    ,"typedef",
            "case"    ,"extern","return"      ,"union",
            "char"    ,"float" ,"short"       ,"unsigned",
            "const"   ,"for"   ,"signed"      ,"void",
            "continue","goto"  ,"sizeof"      ,"volatile",
            "default" ,"if"    ,"static"      ,"while",
            "do"      ,"int"   ,"struct"      ,"_Packed",
            "double"  ,"asm"   ,"dynamic_cast","namespace","reinterpret_cast",  "try",
            "bool",        "explicit",      "new",        "static_cast",       "typeid",
            "catch",       "false",         "operator",   "template",          "typename",
            "class",       "friend",        "private",    "this",              "using",
            "const_cast",  "inline",        "public",     "throw",             "virtual",
            "delete",      "mutable",       "protected",  "true",              "wchar_t" ]
{- Parser the C++ file. Look for classes, typedefs and namespaces -}
parseCPP :: Parser (Map.Map String (Set.Set BS.ByteString))
parseCPP = 
    let
        word = skipSpace >> BP.takeWhile sat >>= ((>>) skipSpace . return)
            where sat ch = C.isDigit ch || C.isAlpha ch || ch == '_'

        {- Parse a class -}
        parseClass = string "class" *>
                     fmap ("RadiationCppClass",) word

        parseTypedef = string "typedef" *>
                       (fun <$> BP.takeWhile (/=';'))
                       where fun = ("RadiationCppTypedef",) . last . BSC.words

        parseNamespace = string "namespace" *>
                         fmap ("RadiationCppNamespace",) word

        one = choice [ return <$> parseClass,
                       return <$> parseTypedef,
                       return <$> parseNamespace,
                       anyChar $> [] ] in

   (fromList' . concat) <$> many one

   where fromList' :: (Ord a, Ord b) => [(a,b)] -> Map.Map a (Set.Set b)
         fromList' = foldl (\mp (k,v) ->
            Map.insertWith Set.union k (Set.singleton v) mp) Map.empty

parser :: R.Parser
parser = R.Parser $ \filename -> do
    openLogFile "/tmp/cpp_radiation.log" Debug
    log Info "Start cpp parser"

    {- Get the utilities to parse the output -}
    pipes <- runCommand =<< sequence
        [queryDefault "g:radiation_cpp_cc" "g++",
         queryDefault "g:radiation_cpp_flags" "",
         pure "-E", pure filename]
    
    reportErrors pipes $
        withParsingMap (Map.map (Set.\\blacklist) <$> parseCPP)
            <=< vGetHandleContents
