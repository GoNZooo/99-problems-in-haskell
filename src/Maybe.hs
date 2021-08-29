module Maybe where

import Core
import Foldable
import Functor

data Maybe a
  = Nothing
  | Just a
  deriving (Show)

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just a == Just b = a == b
  _ == _ = False

instance Foldable Maybe where
  foldr _f nil Nothing = nil
  foldr f nil (Just a) = f a nil

instance Functor Maybe where
  map _f Nothing = Nothing
  map f (Just a) = Just $ f a
