module Radiation.Parsers.Test where

import Radiation.Parsers

import Control.Applicative
import Control.Monad.IO.Class

import Vim
import Prelude hiding (log)

parser :: Parser
parser = Parser $ \_ -> do
    openLogFile "/tmp/radiation.log" Debug

    log Info "Start test parser"

    liftIO (lines <$> readFile "/usr/share/dict/cracklib-small") >>=
        (mapM_ . highlight) "Userdef1" . return
