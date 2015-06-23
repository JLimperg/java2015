module Data.Sequence.Ext
( dropR
) where

import           Data.Sequence

splitAtR :: Int -> Seq a -> (Seq a, Seq a)
splitAtR i s = splitAt (length s - i) s

dropR :: Int -> Seq a -> Seq a
dropR i = fst . splitAtR i
