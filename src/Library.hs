-- | To make this module more interesting I will be implementing almost everything internally, which
-- means we can't depend on anything except what we have in this and other local modules.
-- Exceptions to this:
-- `String`, `Show`, `Eq`, `Ord`, `Integral`, `Num` and basic operators
module Library where

import Core
import Foldable
import List
import Maybe

-- | P01 (*) Find the last box of a list.
-- Example:
-- >>> p01_last (1 :. 2 :. 3 :. Nil :: List Int)
-- Just 3
-- >>> p01_last (Nil :: List Int)
-- Nothing
last :: List a -> Maybe a
last Nil = Nothing
last (a :. Nil) = Just a
last (_ :. xs) = last xs

-- (D)
-- P02 (*) Find the last but one box of a list.
-- Example:

-- * (my-but-last '(a b c d))

-- (C D)

butLast :: List a -> Maybe (List a)
butLast Nil = Nothing
butLast (_ :. Nil) = Nothing
butLast xs@(_ :. _ :. Nil) = Just xs
butLast (_ :. xs) = butLast xs

-- P03 (*) Find the K'th element of a list.
-- The first element in the list is number 1.
-- Example:
-- >>> elementAt 3 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Just 3
--

-- >>> elementAt 3 Nil
-- Nothing
--

elementAt :: Int -> List a -> Maybe a
elementAt _ Nil = Nothing
elementAt 1 (x :. _) = Just x
elementAt n (_ :. xs) = elementAt (n - 1) xs

-- P04 (*) Find the number of elements of a list.
numberOfElements :: List a -> Int
numberOfElements = length

-- P05 (*) Reverse a list.
-- Example:
-- >>> reverse (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- 1 :.
--     ( 2 :.
--         ( 3 :.
--             ( 4 :. ( 5 :. Nil ) )
--         )
--     )
--
reverseList :: List a -> List a
reverseList = reverse

-- P06 (*) Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => List a -> Bool
isPalindrome xs = xs == reverse xs

-- P07 (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- Example:

-- * (my-flatten '(a (b (c d) e)))

-- (A B C D E)
-- Hint: Use the predefined functions list and append.

-- P08 (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
-- Example:

-- * (compress '(a a a a b c c a a d e e e e))

-- (A B C A D E)

-- P09 (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
-- Example:

-- * (pack '(a a a a b c c a a d e e e e))

-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
