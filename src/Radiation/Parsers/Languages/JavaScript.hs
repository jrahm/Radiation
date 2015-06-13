{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Radiation.Parsers.Languages.JavaScript(parser) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad
import Data.Attoparsec.ByteString.Char8 (string, Parser, parseOnly, many1, anyChar, skipSpace)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy as BSL (readFile)
import Control.Monad.IO.Class

import Radiation.Parsers.Internal.CStyle (identifier, notIdentifier, body, balancedParens, spaced, nextToken, token)
import Radiation.Parsers.Internal.WithAttoparsec (withParsingMap)
import Vim
import Radiation.Parsers hiding (Parser)
import qualified Radiation.Parsers as R

import My.Utils
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State.Lazy

parseJavaScript :: Parser (Map String (Set ByteString))
parseJavaScript = 
    let parseElement :: Parser (String, ByteString)
        parseElement =
            (token "function" >> ("RadiationJavaScriptFunction",) <$> identifier <*
                spaced balancedParens <* body) <|>
            (token "var" >> ("RadiationJavaScriptVar",) <$> identifier)

        one = (return <$> parseElement) <|>
              (nextToken >> skipSpace $> []) <|>
              (anyChar $> [])
              
        in

        (map_fromList2 . concat) <$> many1 one


runParser :: Parser (VimM()) -> ByteString -> VimM ()
runParser bs parser =
    case parseOnly bs parser of
        Left err -> logs Error ("Error parsing: " ++ err)
        Right vim -> vim

parser :: R.Parser
parser = R.Parser (const ["g:radiation_javascript_includes"]) $ \filename -> do
    openLogFilePortable "js_radiation.log" Debug
    logs Info "Start JavaScript parser"

    c "JavaScript Parser for Radiation"
    c "@Author Josh Rahm (joshuarahm@gmail.com)"

    hiLink "RadiationJavaScriptVar" "Identifier"
    hiLink "RadiationJavaScriptFunction" "Function"

    withParsingMap parseJavaScript =<< liftIO (BSL.readFile filename)
