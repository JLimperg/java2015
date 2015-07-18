module Eval.Common
( list1P
, ppList1
) where

import           Data.Foldable
import           Data.List (intersperse)
import           Data.Text.Lazy.Builder
import           Text.Parsec.Combinator
import           Text.Parsec.Prim hiding ((<|>))

import           List1

list1P :: (Stream s m t)
       => ParsecT s u m a -> ParsecT s u m op -> ParsecT s u m (List1 a)
list1P elemParser opParser =
    (\(x:xs) -> List1 x xs) <$> elemParser `sepBy1` opParser

ppList1 :: Builder -> (a -> Builder) -> List1 a -> Builder
ppList1 sep ppElem = mconcat . intersperse sep . map ppElem . toList
