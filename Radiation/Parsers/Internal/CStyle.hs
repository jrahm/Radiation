{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Internal.CStyle where

import Data.Attoparsec.ByteString.Char8 as BP
import Data.Attoparsec.ByteString.Lazy as Lazy

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Char as C

import Control.Applicative
import Control.Monad

import My.Utils

(+>) :: Parser BS.ByteString -> Parser BS.ByteString -> Parser BS.ByteString
(+>) p1 p2 = BS.append <$> p1 <*> p2

{- Take an identifier from the parser -}
identifier :: Parser BS.ByteString
identifier = skipSpace *> BP.takeWhile sat <* skipSpace
    where sat ch = C.isDigit ch || C.isAlpha ch || ch == '_'

between :: Char -> Char -> Parser BS.ByteString
between open close = skipSpace *> char open *> (between open close <|> BP.takeWhile sat) <* char close
    where sat ch = ch /= open && ch /= close

body :: Parser BS.ByteString
body = between '{' '}'

parens :: Parser BS.ByteString
parens = between '(' ')' 

nextWord :: BS.ByteString -> Parser BS.ByteString
nextWord str = BP.takeWhile C.isSpace +> string str

primitive :: Parser BS.ByteString
primitive  = skipSpace *> choice [integral, floating] -- >>= (\bs -> if BS.null bs then fail "" else return bs)
    where integral = option "" (option "" (string "un") +> nextWord "signed") +> choice (map nextWord ["int","char"])
          floating = string "float"


data CTypeHeader = CTypeHeader (Maybe (BS.ByteString,BS.ByteString)) 

{- Parses a type in C. Tries to do all possibilities, including
 - anonymous structures, but alas it proves difficult -}
ctype :: Parser CTypeHeader
ctype = (<|>) (primitive $> CTypeHeader Nothing) $ do

    typeoftype <- optional (choice $ fmap string ["struct","enum","union"])
    id1 <- (body $> Nothing) <|> (Just <$> (identifier <* body))

    identifier `mplus` (BSC.pack <$> many (skipSpace *> char '*' <* skipSpace))

    return $ CTypeHeader ((,) <$> typeoftype <*> id1)

{- Parses C++ types. These inculde the apersaned for
 - references -}
cpptype :: Parser BS.ByteString
cpptype = identifier `mplus` (BSC.pack <$> many (skipSpace *> (char '*' <|> char '&') <* skipSpace))
