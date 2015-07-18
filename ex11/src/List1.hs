module List1
( List1(..)
) where

import           Data.Foldable

data List1 a = List1 a [a]
  deriving (Read, Show, Eq, Ord)

instance Functor List1 where
    fmap f (List1 x xs) = List1 (f x) (map f xs)

instance Foldable List1 where
    fold = fold . toList
    foldMap f = foldMap f . toList
    foldr f acc = foldr f acc . toList
    foldr' f acc = foldr' f acc . toList
    foldl f acc = foldl f acc . toList
    foldl' f acc = foldl' f acc . toList
    foldr1 f = foldr1 f . toList
    foldl1 f = foldl1 f . toList
    toList (List1 x xs) = x : xs
    null _ = False
    length (List1 _ xs) = 1 + length xs
    elem e = elem e . toList
    maximum = maximum . toList
    minimum = minimum . toList
    sum = sum . toList
    product = product . toList

instance Traversable List1 where
    sequenceA = fmap unsafeFromList . sequenceA . toList

unsafeFromList :: [a] -> List1 a
unsafeFromList (x:xs) = List1 x xs
unsafeFromList _      = error "List1.unsafeFromList: called with empty list"
