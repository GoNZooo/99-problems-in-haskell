module Monoid where

import ConsUncons
import Core
import Foldable
import Maybe
import Semigroup

class (Semigroup m) => Monoid m where
  identity :: m

concatenate :: (Foldable m, Monoid a) => m a -> a
concatenate = foldr (<>) identity

-- Can we zip something together based only on general type classes?
zip :: (Monoid (m (Tuple a b)), Cons m, Uncons m) => m a -> m b -> m (Tuple a b)
zip as bs = case Tuple (uncons as) (uncons bs) of
  Tuple (Just (Tuple a as')) (Just (Tuple b bs')) -> cons (Tuple a b) (zip as' bs')
  Tuple Nothing _unconsBs -> identity
  Tuple _unconsAs Nothing -> identity

instance (Semigroup a) => Monoid (Maybe a) where
  identity = Nothing
