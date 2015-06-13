{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Internal.CStyle where

import Data.Attoparsec.ByteString.Char8 as BP
import Data.Attoparsec.ByteString.Lazy as Lazy
import qualified Data.Attoparsec.ByteString as BB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Char as C

import Control.Applicative
import Control.Monad

import My.Utils

import Debug.Trace

spaced :: Parser BS.ByteString -> Parser BS.ByteString
spaced p = skipSpace *> p <* skipSpace

attribute :: Parser BS.ByteString
attribute = do
    string "__attribute__" 
    skipSpace
    balancedParens

removePattern :: Parser BS.ByteString -> Parser BS.ByteString
removePattern pattern  = BS.concat <$> many ((pattern >> return BS.empty) <|>
                                             (BS.singleton <$> BB.anyWord8))

subparse :: Parser a -> BS.ByteString -> Parser a
subparse myParser bs =
    case parseOnly myParser bs of
        Left err -> fail err
        Right map -> return map
    
(+>) :: Parser BS.ByteString -> Parser BS.ByteString -> Parser BS.ByteString
(+>) p1 p2 = BS.append <$> p1 <*> p2

{- Take an identifier from the parser -}
identifier :: Parser BS.ByteString
identifier = skipSpace *> BP.takeWhile1 isIdentifierChar <* skipSpace

notIdentifier :: Parser BS.ByteString
notIdentifier = skipSpace *> BP.takeWhile1 (\c -> not (isIdentifierChar c || isSpace c)) <* skipSpace

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = C.isDigit ch || C.isAlpha ch || ch == '_'

between :: Char -> Char -> Parser BS.ByteString
between open close = skipSpace *> char open *> (between open close <|> BP.takeWhile sat) <* char close
    where sat ch = ch /= open && ch /= close

nextToken :: Parser BS.ByteString
nextToken = identifier <|> notIdentifier

token :: BS.ByteString -> Parser BS.ByteString
token str = do
    tkn <- nextToken
    if tkn == str then return str else fail "Could not match token"

balanced :: Char -> Char -> Parser BS.ByteString
balanced c1 c2 = 
    let
        looseBalanced :: BS.ByteString -> Int -> Parser BS.ByteString
        looseBalanced cur 0 = return cur
        looseBalanced cur n = do
            rest <- BP.takeWhile (\ch -> ch /= c1 && ch /= c2)
            ch <- char c1 <|> char c2
            let cur' = cur `mappend` rest `mappend` BSC.singleton ch
            case () of 
                () | ch == c1  -> looseBalanced cur' (n + 1)
                   | ch == c2  -> looseBalanced cur' (n - 1)
                   | otherwise -> looseBalanced cur' n
        in

    BP.char c1 >> looseBalanced (BSC.singleton c1) 1

balancedParens :: Parser BS.ByteString
balancedParens = balanced '(' ')'

body :: Parser BS.ByteString
body = balanced '{' '}'

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
