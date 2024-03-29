module List where

import ConsUncons
import Core
import Foldable
import Functor
import Maybe
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

instance Cons List where
  cons = (:.)

instance Uncons List where
  uncons Nil = Nothing
  uncons (a :. as) = Just $ Tuple a as

filter :: (a -> Bool) -> List a -> List a
filter p = foldr (\a as -> bool (a :. as) as (p a)) Nil

unzip :: List (Tuple a b) -> Tuple (List a) (List b)
unzip = foldr (\(Tuple a b) (Tuple as bs) -> Tuple (a :. as) (b :. bs)) (Tuple Nil Nil)

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

take :: (Integral a) => a -> List a -> List a
take n xs = foldr f (k Nil) xs n
  where
    f _a _fas 0 = Nil
    f a fas n' = a :. fas (n' - 1)

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p = foldr (\a as -> bool (a :. as) Nil (p a)) Nil

drop :: Int -> List a -> List a
drop n xs = foldr f (k Nil) xs n
  where
    f a fas 0 = a :. fas 0
    f _a fas n' = fas (n' - 1)

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p xs = foldr f (k Nil) xs True
  where
    f a fas True = bool (fas True) (a :. fas False) (p a)
    f a fas False = a :. fas False

infinite :: a -> List a
infinite a = a :. infinite a

range :: (Integral a) => a -> a -> List a
range from to = iterate (+ 1) from & take (to - from)

iterate :: (a -> a) -> a -> List a
iterate f a = a :. iterate f (f a)

iota :: Int -> List Int
iota n = iterate (+ 1) 1 & take n

span :: (a -> Bool) -> List a -> Tuple (List a) (List a)
span _p Nil = Tuple Nil Nil
span p list@(a :. as) = bool f (Tuple Nil list) $ p a
  where
    f =
      let Tuple matching notMatching = span p as
       in Tuple (a :. matching) notMatching

break :: (a -> Bool) -> List a -> Tuple (List a) (List a)
break p = span (p >>> not)
