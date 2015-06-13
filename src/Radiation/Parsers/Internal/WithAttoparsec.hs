{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Radiation.Parsers.Internal.WithAttoparsec where

import Control.Monad (forM_)
    
import Data.Attoparsec.ByteString.Char8 as BP
import Data.Attoparsec.ByteString.Lazy as Lazy

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

import Radiation.Parsers hiding (Parser)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Vim
import My.Utils

withParsing :: Parser a -> (a -> VimM ()) -> BSL.ByteString -> VimM ()
withParsing parser trans filestr =
    case Lazy.parse parser filestr of
        
        {- The parsing failed. -}
        Lazy.Fail _ _ err ->
            vlog Error $ "Unable to parse: " +>+ BSC.pack err

        {- The parsing succeeded and handle the correct result -}
        Lazy.Done _ val ->
            vlog Info "Matches found" >> trans val >>
            vlog Debug "Finished running parser"

withParsingMap :: Parser (Map.Map String (Set.Set BS.ByteString)) ->
                  BSL.ByteString -> VimM ()
withParsingMap parserm = withParsing parserm def
    where
        def :: Map.Map String (Set.Set BS.ByteString) -> VimM()
        def val = forM_ (Map.toList val) $ \(hi,v) ->
                    (syn Keyword (BSC.pack hi) (Set.toList v) :: VimM())
