{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Internal.CppStyle (cppType) where

import Data.Attoparsec.ByteString.Char8 as BP
import Control.Applicative
import Control.Monad

import qualified Data.ByteString as BS
import Radiation.Parsers.Internal.CStyle (identifier)


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
cppType = (>>) skipSpace $ pmodifier >++> pnamespaceStart

