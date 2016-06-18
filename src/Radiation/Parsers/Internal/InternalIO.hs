{-# LANGUAGE FlexibleInstances #-}
module Radiation.Parsers.Internal.InternalIO where

import Vim
import System.IO

import Control.Monad.IO.Class
import Control.Applicative

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS

class HasHandleContents a where
    getHandleContents :: Handle -> IO a

instance HasHandleContents [Char] where
    getHandleContents = hGetContents

instance HasHandleContents BSL.ByteString where
    getHandleContents = BSL.hGetContents

instance HasHandleContents BS.ByteString where
    getHandleContents = BS.hGetContents

vGetHandleContents :: (HasHandleContents a) => Handle -> VimM a
vGetHandleContents = liftIO . getHandleContents

reportErrors :: (Handle,Handle,x) -> (Handle -> VimM a) -> VimM a
reportErrors (stout,sterr,_) func = do
    ret <- func stout
    mapM_ (vlog Error) =<< (BSC.lines <$> vGetHandleContents sterr)
    return ret
