{-# LANGUAGE CPP #-}

module Sheet where

import           Control.Monad.ST
import           Data.List
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Vector.Unboxed as VI
import           Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as V

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>))
#endif

newtype Index = Index Int
  deriving (Eq, Ord, Read, Show)

type Sheet s = STVector s Int

mkSheet :: Int -> ST s (Sheet s)
mkSheet len = V.replicate len 0

put :: Sheet s -> Index -> Int -> ST s ()
put sheet (Index ix) = V.write sheet ix

get :: Sheet s -> Index -> ST s Int
get sheet (Index ix) = V.read sheet ix

display :: Char -> Sheet s -> ST s Text
display sep sheet =
    T.concat . intersperse (T.singleton sep) . map (T.pack . show) . VI.toList
      <$> VI.freeze sheet
