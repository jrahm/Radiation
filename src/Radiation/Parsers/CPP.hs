{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.CPP(parser) where

import Control.Applicative ((<$>), (<|>), many)
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as BP
import Data.Char (isAlphaNum, isDigit, isAlpha)
import My.Utils
import Prelude hiding (log)
import Radiation.Parsers.Internal.CStyle
import Radiation.Parsers.Internal.CommandParser
import Radiation.Parsers.Internal.InternalIO
import Radiation.Parsers.Internal.WithAttoparsec
import Vim

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Radiation.Parsers as R

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

typMap :: Map.Map BS.ByteString String
typMap = Map.fromList [
        ("struct","RadiationCppStruct"),
        ("union","RadiationCppUnion"),
        ("class","RadiationCppClass"),
        ("enum","RadiationCppEnum")]

{- Parser the C++ file. Look for classes, typedefs and namespaces -}
parseCPP :: Parser (Map.Map String (Set.Set BS.ByteString))
parseCPP = 
    let
        word = skipSpace >> BP.takeWhile sat >>= ((>>) skipSpace . return)
            where sat ch = isAlphaNum ch || ch == '_'

        {- Parse a class -}
        parseClass =
            (string "class"  >> ("RadiationCppClass",)  <$> word) <|>
            (string "struct" >> ("RadiationCppStruct",) <$> word) <|>
            (string "union"  >> ("RadiationCppUnion",)  <$> word) <|>
            (string "enum"   >> ("RadiationCppEnum",)   <$> word)

        parseTypedef :: Parser [(String,BSC.ByteString)]
        parseTypedef = do
                _ <- string "typedef"

                (do 
                    {- parse the typedef of template:
                     - typedef struct [name] { ... } [ident] -}
                    typ <- skipSpace *> (choice . map string) (Map.keys typMap)
                    id1' <- (option Nothing (Just <$> identifier))
                    let id1 = (,) <$> Map.lookup typ typMap <*> id1'

                    (addJust id1 . return . ("RadiationCTypedef",))
                        <$> (skipSpace *> (option "" body) *> identifier)) <|>

                    {- Or as the original typedef ... [ident]; -}
                    ((return . ("RadiationCTypedef",) . last . BSC.words) <$> BP.takeWhile (/=';'))

        parseNamespace = string "namespace" *>
                         fmap ("RadiationCppNamespace",) word

        one =
              {- Try to parse a class -}
              (return <$> parseClass) <|> 
              {- Try to parse a typedef -}
              parseTypedef <|>
              {- Try to parse an namespace -}
              (return <$> parseNamespace) <|>
              {- take until the next space -}
              (BP.takeWhile1 (not . isSpace) >> BP.takeWhile1 isSpace $> []) <|>
              {- take any char otherwise -}
              (anyChar $> [])
        in

    subparse (removePattern attribute) $
                (fromList' . concat) <$> many one

   where fromList' :: (Ord a, Ord b) => [(a,b)] -> Map.Map a (Set.Set b)
         fromList' = foldl (\mp (k,v) ->
            Map.insertWith Set.union k (Set.singleton v) mp) Map.empty
         addJust Nothing = id
         addJust (Just x) = (x:)

parser :: R.Parser
parser = R.Parser (const ["g:radiation_cpp_cc", "g:radiation_cpp_flags"]) $ \filename -> do
    openLogFilePortable "cpp_radiation.log" Debug
    log Info "Start cpp parser"

    {- Get the utilities to parse the output -}
    pipes <- runCommand =<< sequence
        [queryDefault "g:radiation_cpp_cc" "g++",
         queryDefault "g:radiation_cpp_flags" "",
         pure "-E", pure filename]
    
    reportErrors pipes $
        withParsingMap (Map.map (Set.filter (BSC.all isAlphaNum) . (Set.\\blacklist)) <$> parseCPP)
            <=< vGetHandleContents
