module Radiation.Parsers.Test where

import Radiation.Parsers

import Control.Applicative
import Control.Monad.IO.Class

import Vim
import Prelude hiding (log)

{- Very basic test parser. Reads data from crack lib and highlights
 - all the words from it -}
parser :: Parser
parser = Parser $ \_ -> do
    openLogFilePortable "test_radiation.log" Debug

    log Info "Start test parser"

    highlight "Userdef1" =<<
        liftIO (lines <$> readFile "/usr/share/dict/cracklib-small")
