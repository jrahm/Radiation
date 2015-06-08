{-# LANGUAGE MultiParamTypeClasses #-}
module My.Utils where

import Data.Foldable (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Convertible
import Data.Hash.MD5 (Str(..))

instance Convertible Str ByteString where
    safeConvert (Str str) = Right (pack str)

instance Convertible ByteString ByteString where
    safeConvert = Right

(+>+) :: Monoid m => m -> m -> m
(+>+) = mappend

($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)

{- Infix versions of for M -}
(<<?) :: (Foldable f, Monad m) => f a -> (a -> m b) -> m ()
(<<?) = forM_

(?>>) :: (Foldable f, Monad m) => (a -> m b) -> f a -> m ()
(?>>) = flip (<<?)
