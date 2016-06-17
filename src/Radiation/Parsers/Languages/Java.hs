{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Languages.Java (parser) where

import qualified Radiation.Parsers as R
import Prelude hiding (log)

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.List
import Radiation.Parsers.Languages.JavaLang

import Data.Char
import Data.List.Split

import Vim
import qualified Data.ByteString.Char8 as BSC

parser :: R.Parser
parser =
        R.Parser "java"
                (const ["g:radiation_java_classpath"])
                $ \filename -> do

                log Info "Start Java Parser"
                let or_ f1 f2 = \x -> f1 x || f2 x 

                let scrub str =
                     case dropWhile isSpace str of 
                        s | "abstract" `isPrefixOf` s -> scrub (drop 8 s)
                        s | "public" `isPrefixOf` s -> scrub (drop 6 s)
                        s | "private" `isPrefixOf` s -> scrub (drop 7 s)
                        s | "protected" `isPrefixOf` s -> scrub (drop 9 s)
                        s | "static" `isPrefixOf` s -> scrub (drop 6 s)
                        s -> s

                -- very simple to start with. Just look at the imports
                imports <- liftIO $ do
                              lines <- map scrub <$> (lines <$> readFile filename)
                              return $ filter (
                                isPrefixOf "import " `or_`
                                isPrefixOf "class "  `or_`
                                isPrefixOf "interface " `or_`
                                isPrefixOf "enum ") lines

                let endOfImports = map (last . concatMap words . map (filter $ isAlphaNum `or_` isSpace) . (splitOn ".")) imports

                R.syn R.Keyword ("javaType" :: BSC.ByteString) (map BSC.pack $ javaLang ++ endOfImports)
