{-# LANGUAGE CPP #-}
module My.Utils where

import Control.Applicative
import Data.Foldable (forM_,Foldable)

import System.Environment

($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)

{- Infix versions of for M -}
(<<?) :: (Foldable f, Monad m) => f a -> (a -> m b) -> m ()
(<<?) = forM_

(?>>) :: (Foldable f, Monad m) => (a -> m b) -> f a -> m ()
(?>>) = flip (<<?)

tempFolder :: IO FilePath
tempFolder = 
#ifdef mingw32_HOST_OS
    getEnv "TEMP"
#else
    return "/tmp"
#endif
