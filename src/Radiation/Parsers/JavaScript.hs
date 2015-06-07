{-# LANGUAGE OverloadedStrings #-}

module Radiation.Parsers.JavaScript(parser) where

import Control.Monad

import Vim
import qualified Radiation.Parsers as R

parser :: R.Parser
parser = R.Parser (const ["g:radiation_javascript_includes"]) $ \filename -> do
    openLogFilePortable "js_radiation.log" Debug
    logs Info "Start JavaScript parser"
