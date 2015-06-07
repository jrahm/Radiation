{-# LANGUAGE OverloadedStrings #-}

module Radiation.Parsers.Languages.JavaScript(parser) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Attoparsec.ByteString.Char8 (string, Parser, parseOnly, many1, anyChar)
import Data.ByteString as BS (ByteString, readFile)
import Control.Monad.IO.Class

import Radiation.Parsers.Internal.CStyle (identifier, notIdentifier)
import Vim
import qualified Radiation.Parsers as R

import My.Utils
import Data.Maybe (catMaybes)

parseJavaScript :: Parser (VimM ())
parseJavaScript =
    let parseFunction = string "function" >> identifier
        nextToken = identifier <|> notIdentifier
        in
        fmap (R.highlight ("RadiationJavaScriptFunction"::ByteString)) $
            catMaybes <$> many1 ((Just <$> parseFunction) <|> (anyChar $> Nothing))


runParser :: Parser (VimM()) -> ByteString -> VimM()
runParser bs parser =
    case parseOnly bs parser of
        Left err -> logs Error ("Error parsing: " ++ err)
        Right vim -> vim

parser :: R.Parser
parser = R.Parser (const ["g:radiation_javascript_includes"]) $ \filename -> do
    openLogFilePortable "js_radiation.log" Debug
    logs Info "Start JavaScript parser"

    runParser parseJavaScript =<< liftIO (BS.readFile filename)
