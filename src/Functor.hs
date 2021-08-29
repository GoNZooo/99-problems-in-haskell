module Functor where

class Functor f where
  map :: (a -> b) -> f a -> f b

instance Functor [] where
  map _f [] = []
  map f (x : xs) = f x : map f xs
