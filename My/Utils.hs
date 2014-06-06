module My.Utils where

import Control.Applicative
import Data.Foldable (forM_,Foldable)

($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)

{- Infix versions of for M -}
(<<?) :: (Foldable f, Monad m) => f a -> (a -> m b) -> m ()
(<<?) = forM_

(?>>) :: (Foldable f, Monad m) => (a -> m b) -> f a -> m ()
(?>>) = flip (<<?)
