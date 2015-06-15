{-# LANGUAGE MultiParamTypeClasses #-}
module My.Utils where

import Prelude hiding (foldl)

import Data.Foldable (forM_, Foldable, foldl)
import Control.Applicative ((<$))
import Data.Monoid (Monoid, mappend, mempty)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Convertible
import Data.Hash.MD5 (Str(..))

import Data.Map (Map, insertWith)
import qualified Data.Set as Set

class HasSingleton t where
    singleton :: a -> t a

instance HasSingleton Set.Set where
    singleton = Set.singleton

instance HasSingleton [] where
    singleton a = [a]

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

map_fromList2 :: (Foldable f, Ord a, Ord b, Monoid (m b), HasSingleton m) => f (a, b) -> Map a (m b)
map_fromList2 = foldl (\mp (k, v) -> insertWith mappend k (singleton v) mp) mempty

appendJust :: (Monoid (m a), HasSingleton m) => Maybe a -> m a -> m a
appendJust Nothing = id
appendJust (Just x) = flip mappend (singleton x)
