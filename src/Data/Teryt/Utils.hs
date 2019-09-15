module Data.Teryt.Utils
  ( inverse
  , liftMaybe
  ) where

import Control.Monad (MonadPlus(..))

import Data.Foldable (find)

inverse :: (Enum a, Bounded a, Eq b) => (a -> b) -> b -> Maybe a
inverse f y = fmap fst $ find (\(_, y') -> y' == y) $ zip lst (map f lst)
  where
    lst = [minBound..maxBound]

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero pure
