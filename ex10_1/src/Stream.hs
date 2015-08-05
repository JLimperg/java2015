module Stream
( words
, upperCase
, replace
, remove
) where

import           Data.Char (isSpace, toUpper)
import           Prelude hiding (words)

words :: String -> [String]
words = breakOn isSpace

breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn _ [] = []
breakOn p xs =
    let (prefix, suffix) = break p xs
    in  prefix : breakOn p (dropWhile p suffix)

upperCase :: [String] -> [String]
upperCase = map (map toUpper) -- [1]

replace :: (Eq a) => a -> a -> [a] -> [a]
replace from to = map (\x -> if x == from then to else x)

remove :: (Eq a) => a -> [a] -> [a]
remove bad = foldr f []
  where
    f x xs | x == bad  = xs
           | otherwise = x : xs

-- [1] This is technically incorrect because toUpper on individual chars is
-- not the same as toUpper on a whole string, thanks to difficulties with
-- Unicode combining chars and such. But for demonstration purposes, it'll do.
