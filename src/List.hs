module List where

import Core
import Foldable
import Functor
import Monoid
import Semigroup

infixr 5 :.

data List a
  = a :. (List a)
  | Nil
  deriving (Show)

instance Foldable List where
  foldr _f nil Nil = nil
  foldr f nil (a :. as) = a `f` foldr f nil as

instance Functor List where
  map _f Nil = Nil
  map f (a :. as) = f a :. map f as

instance (Eq a) => Eq (List a) where
  Nil == Nil = True
  (a :. as) == (b :. bs) = a == b && as == bs
  _ == _ = False

instance Semigroup (List a) where
  Nil <> as = as
  (a :. as) <> bs = a :. as <> bs

instance Monoid (List a) where
  identity = Nil

reverse :: List a -> List a
reverse = foldl (flip (:.)) Nil

flatten :: List (List a) -> List a
flatten = concatenate

fromHaskellList :: [a] -> List a
fromHaskellList = foldr (:.) Nil

toHaskellList :: List a -> [a]
toHaskellList = foldr (:) []

all :: (a -> Bool) -> List a -> Bool
all p = foldr (\a r -> p a && r) True

any :: (a -> Bool) -> List a -> Bool
any p = foldr (\a r -> p a || r) False

replicate :: Int -> a -> List a
replicate 0 _x = Nil
replicate n x = x :. replicate (n - 1) x
