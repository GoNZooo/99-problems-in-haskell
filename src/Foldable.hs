module Foldable where

import Core

class Foldable f where
  foldr :: (a -> b -> b) -> b -> f a -> b

instance Foldable [] where
  foldr _f nil [] = nil
  foldr f nil (a : as) = a `f` foldr f nil as

sum :: (Foldable f, Num a) => f a -> a
sum = foldr (+) 0

product :: (Foldable f, Num a) => f a -> a
product = foldr (*) 0

length :: (Foldable f) => f a -> Int
length = foldr (k (+ 1)) 0

foldl :: Foldable f => (b -> a -> b) -> b -> f a -> b
foldl f nil xs = foldr f' i xs nil
  where
    f' a b c = b (c `f` a)
