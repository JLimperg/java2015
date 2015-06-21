{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SpreadSheet
( SpreadSheet
, mkSpreadSheet
, sheet
, Cmd(..)
, Index(..)
, apply
, undo
, S.display
, HistorySize(..)
, setHistorySize
) where

import           Control.Monad.ST
import           Data.Sequence (Seq, ViewL(..), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Ext as Seq
import           Data.STRef
import           Sheet (Sheet, mkSheet, Index)
import qualified Sheet as S

newtype HistorySize = HistorySize { getHistorySize :: Int }
  deriving (Read, Show, Eq, Ord, Num)

-- | A @SpreadSheet@ stores both a "Sheet" and a history of modifications
-- performend on the @Sheet@, as well as the maximum number of elements in
-- the history.
data SpreadSheet s =
    SpreadSheet !(Sheet s) !(STRef s (Seq Cmd)) !(Maybe HistorySize)
    -- [1]

-- [1] The history is represented as a list of inverses of the operations that
-- were previously performed on the sheet. See 'invert' for details.

mkSpreadSheet :: Maybe HistorySize -> Int -> ST s (SpreadSheet s)
mkSpreadSheet histSize n =
    SpreadSheet <$> mkSheet n <*> newSTRef Seq.empty <*> pure histSize

sheet :: SpreadSheet s -> Sheet s
sheet (SpreadSheet s _ _) = s

data Cmd = Put Index Int
  deriving (Read, Show, Eq, Ord)

-- | Inverts a command, meaning that for any command @cmd@ and sheet
-- @sheet@,
--
-- > applyToSheet cmd sheet >> applyToSheet (invert cmd) sheet === return ()
--
-- if applying cmd does not lead to an error.
invert :: Cmd -> Sheet s -> ST s Cmd
invert (Put ix _) s = Put ix <$> S.get s ix

applyToSheet :: Cmd -> Sheet s -> ST s ()
applyToSheet (Put ix val) s = S.put s ix val

limitHistorySize :: Maybe HistorySize -> Seq Cmd -> Seq Cmd
limitHistorySize Nothing        hist = hist
limitHistorySize (Just maxSize) hist
    | overSize <= 0 = hist
    | otherwise     = Seq.dropR overSize hist
  where
    overSize = Seq.length hist - getHistorySize maxSize

apply :: Cmd -> SpreadSheet s -> ST s ()
apply cmd (SpreadSheet s historyRef maxHistSize) = do
    invertedCmd <- invert cmd s
    modifySTRef historyRef $ limitHistorySize maxHistSize . (invertedCmd <|)
    applyToSheet cmd s

undo :: SpreadSheet s -> ST s ()
undo (SpreadSheet s historyRef _) = do
    history <- readSTRef historyRef
    case Seq.viewl history of
      EmptyL        -> error "undo: empty history" -- [1]
      (cmd :< cmds) -> applyToSheet cmd s >> writeSTRef historyRef cmds

-- [1] This is not proper error handling, but we don't care about that
-- here.

setHistorySize :: Maybe HistorySize -> SpreadSheet s -> ST s (SpreadSheet s)
setHistorySize maxHistorySize (SpreadSheet s historyRef _) = do
    modifySTRef historyRef $ limitHistorySize maxHistorySize
    return $ SpreadSheet s historyRef maxHistorySize
