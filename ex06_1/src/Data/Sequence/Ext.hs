module Data.Sequence.Ext
( dropR
) where

import           Data.Sequence

dropR :: Int -> Seq a -> Seq a
dropR n xs
    | n <= 0    = xs
    | otherwise
    = case viewr xs of
        EmptyR   -> empty
        xs' :> _ -> dropR (n - 1) xs'
