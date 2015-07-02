module Pair
( Pair(..)
, zip
) where

import           Prelude hiding (snd, fst, zip)
import           Data.Monoid ((<>))

data Pair a b = Pair { fst :: a, snd :: b }

instance (Eq a, Eq b) => Eq (Pair a b) where
    Pair x1 y1 == Pair x2 y2 = x1 == x2 && y1 == y2

instance (Ord a, Ord b) => Ord (Pair a b) where
    compare (Pair x1 y1) (Pair x2 y2) = compare x1 x2 <> compare y1 y2

zip :: [a] -> [b] -> [Pair a b]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = Pair x y : zip xs ys
