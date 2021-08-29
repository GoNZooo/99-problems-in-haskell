module Core
  ( module Prelude,
    Bool (..),
    Eq (..),
    k,
    i,
    flip,
    (&&),
    (||),
    ($),
  )
where

import Prelude (Int, Num, Show, (*), (+), (-))
import qualified Prelude

data Bool
  = True
  | False
  deriving (Show)

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Eq Int where
  a == b = if a Prelude.== b then True else False

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

infixr 2 ||

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

k :: a -> b -> a
k a _ = a

i :: a -> a
i a = a

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

infixr 0 $

($) :: (a -> b) -> a -> b
f $ a = f a
