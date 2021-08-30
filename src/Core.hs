module Core
  ( module Prelude,
    Bool (..),
    Eq (..),
    Ord (..),
    Tuple (..),
    s,
    k,
    i,
    flip,
    (&&),
    (||),
    not,
    ($),
    bool,
    (.),
    (>>>),
    (&),
    (<),
    (<=),
    (>),
    (>=),
    even,
    odd,
    first,
    second,
  )
where

import Prelude (Int, Integral, Num, Show, (*), (+), (-))
import qualified Prelude

data Bool
  = True
  | False
  deriving (Show)

infix 4 ==

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Eq Int where
  a == b = if a Prelude.== b then True else False

data Ordering
  = LessThan
  | EqualTo
  | GreaterThan
  deriving (Show)

instance Eq Ordering where
  LessThan == LessThan = True
  EqualTo == EqualTo = True
  GreaterThan == GreaterThan = True
  _ == _ = False

data Tuple a b = Tuple a b
  deriving (Show)

first :: Tuple a b -> a
first (Tuple a _b) = a

second :: Tuple a b -> b
second (Tuple _a b) = b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

class Ord a where
  compare :: a -> a -> Ordering

instance Ord Int where
  compare a b
    | a Prelude.< b = LessThan
    | a Prelude.== b = EqualTo
    | Prelude.otherwise = GreaterThan

(<) :: Ord a => a -> a -> Bool
a < b = compare a b == LessThan

(<=) :: Ord a => a -> a -> Bool
a <= b = compare a b == LessThan || compare a b == EqualTo

(>) :: Ord a => a -> a -> Bool
a > b = compare a b == GreaterThan

(>=) :: Ord a => a -> a -> Bool
a >= b = compare a b == GreaterThan || compare a b == EqualTo

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

infixr 2 ||

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

not :: Bool -> Bool
not True = False
not False = True

bool :: a -> a -> Bool -> a
bool true _false True = true
bool _true false False = false

s :: (z -> a -> b) -> (z -> a) -> z -> b
s x y z = x z $ y z

k :: a -> b -> a
k a _ = a

i :: a -> a
i a = a

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

infixr 0 $

($) :: (a -> b) -> a -> b
f $ a = f a

infixl 0 .

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f a = g $ f a

infixr 1 >>>

(>>>) :: (a -> b) -> (b -> c) -> a -> c
f >>> g = g . f

infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x

even :: Int -> Bool
even x = x `Prelude.rem` 2 == 0

odd :: Int -> Bool
odd = even >>> not
