module List where

import Core
import Foldable
import Functor

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

reverse :: List a -> List a
reverse = foldl (flip (:.)) Nil

fromHaskellList :: [a] -> List a
fromHaskellList = foldr (:.) Nil

toHaskellList :: List a -> [a]
toHaskellList = foldr (:) []