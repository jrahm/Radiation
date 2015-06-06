{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Internal.CommandParser where

import Vim
import System.IO
import System.Process

import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack)
import My.Utils ((+>+))

runCommand :: [String] -> VimM (Handle, Handle)
runCommand args =
    let command = unwords args in do
        vlog Info $ "[RunningCommand]: " +>+ pack command
        (_, stout, sterr, _) <- liftIO $ runInteractiveCommand command
        return (stout,sterr)
