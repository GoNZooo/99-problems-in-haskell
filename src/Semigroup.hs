module Semigroup where

import Core
import Maybe

class Semigroup m where
  (<>) :: m -> m -> m

instance (Semigroup a) => Semigroup (Maybe a) where
  ma <> Nothing = ma
  Nothing <> mb = mb
  Just a <> Just b = Just $ a <> b
