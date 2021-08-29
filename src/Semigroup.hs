module Semigroup where

class Semigroup m where
  (<>) :: m -> m -> m
