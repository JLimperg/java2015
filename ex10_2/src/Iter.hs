module Iter
( map
, scanr
) where

import           Prelude hiding (map, scanr)

map :: (a -> b) -> [a] -> [b]
map f = foldr g []
  where
    g x xs = f x : xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _  initial []     = [initial]
scanr op initial (x:xs) =
  let accs@(acc : _) = scanr op initial xs
  in  x `op` acc : accs
