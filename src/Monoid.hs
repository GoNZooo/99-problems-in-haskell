module Monoid where

import Foldable
import Semigroup

class (Semigroup m) => Monoid m where
  identity :: m

concatenate :: (Foldable m, Monoid a) => m a -> a
concatenate = foldr (<>) identity
