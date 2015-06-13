{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Internal.CppStyle (cppType, reservedWords) where

import Data.Attoparsec.ByteString.Char8 as BP
import Control.Applicative
import Control.Monad

import qualified Data.ByteString as BS
import Radiation.Parsers.Internal.CStyle (identifier)

import qualified Data.Set as Set

reservedWords :: Set.Set BS.ByteString
reservedWords = Set.fromList [
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

reservedWordsNotType :: Set.Set BS.ByteString
reservedWordsNotType = Set.fromList [
            "else"  ,"switch",
            "break"   ,"enum"  ,"register"    ,"typedef",
            "case"    ,"extern","return"      ,"union",
            "const"   ,"for",
            "continue","goto"  ,"sizeof"      ,"volatile",
            "default" ,"if"    ,"static"      ,"while",
            "do"  ,"struct"      ,"_Packed",
            "asm"   ,"dynamic_cast","namespace","reinterpret_cast",  "try",
            "explicit",      "new",        "static_cast",       "typeid",
            "catch",       "false",         "operator",   "template",          "typename",
            "class",       "friend",        "private",    "this",              "using",
            "const_cast",  "inline",        "public",     "throw",             "virtual",
            "delete",      "mutable",       "protected",  "true"]


(>++>) :: Parser BS.ByteString -> Parser BS.ByteString -> Parser BS.ByteString
(>++>) = liftM2 BS.append

ppointer2 = (>>) skipSpace $ (string "*" >++> ppointer1) <|> (string "&" >++> ppointer1) <|> return ""
ppointer1 = (>>) skipSpace $ string "const " >++> ppointer2 <|> ppointer2
ppointer = ppointer1 --(string " " >++> ppointer1) <|> ppointer2
ptemplateInside = (>>) skipSpace $ cppType >++> ((string "," >++> ptemplateInside) <|> return "")
ptemplate = (>>) skipSpace $ (string "<" >++> ptemplateInside >++> string ">" >++> ppointer) <|> ppointer
pretype = (>>)skipSpace $ choice (map string ["struct", "union", "enum", ""])
typename = (>>) skipSpace $ pretype >++> identifier >++> ptemplate
pnamespace = (>>) skipSpace $ (typename >++> string "::" >++> pnamespace) <|> typename
pnamespaceStart = (>>) skipSpace $ (string "::" >++> pnamespace) <|> pnamespace
pmodifier = (>>) skipSpace $ (choice (map string ["const", "static", "typename"]) >++> (" " <* skipSpace) >++> pmodifier) <|> return ""
cppType = do
    typ <- (>>) skipSpace $ pmodifier >++> pnamespaceStart
    if Set.member typ reservedWordsNotType then fail ""
        else return typ

