{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.Languages.CPP(parser) where

import Control.Applicative ((<$>), (<|>), many, (<*), pure)
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as BP
import Data.Char (isAlphaNum, isDigit, isAlpha)
import Debug.Trace
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
        maybeP parser = (Just <$> parser) <|> return Nothing
        spaced parser = skipSpace >> parser <* skipSpace
        word = skipSpace >> BP.takeWhile sat <* skipSpace
            where sat ch = isAlphaNum ch || ch == '_'

        {- Parse a class -}
        parseElement =
            (string "class"       >> maybeP (spaced attribute) >> ("RadiationCppClass",)     <$> word) <|>
            (string "struct"      >> maybeP (spaced attribute) >> ("RadiationCppStruct",)    <$> word) <|>
            (string "union"       >> maybeP (spaced attribute) >> ("RadiationCppUnion",)     <$> word) <|>
            (string "enum"        >> maybeP (spaced attribute) >> ("RadiationCppEnum",)      <$> word) <|>
            (string "namespace"   >> maybeP (spaced attribute) >> ("RadiationCppNamespace",) <$> word) <|>
            (string "typedef"     >> maybeP (spaced attribute) >> ("RadiationCppTypedef",) <$> typedef <* trace "Made it here!" (return ()))

        typedef = trace "Typedef..." $ do
            void (skipSpace >> string "struct" >> skipSpace >> (body <|> (identifier >> body) <|> identifier)) <|> return ()
            bs <- BP.takeWhile (/=';')
            trace (show (last $ BSC.words bs)) $
                return $ last $ BSC.words bs

        one =
              {- Try to parse a class -}
              (return <$> parseElement) <|> 
              {- take until the next space -}
              (BP.takeWhile1 (not . isSpace) >> BP.takeWhile1 isSpace $> []) <|>
              {- take any char otherwise -}
              (anyChar $> [])
        in

        (map_fromList2 . concat) <$> many one

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
        withParsingMap (Map.map (Set.\\blacklist) <$> parseCPP)
            <=< vGetHandleContents
