{-# LANGUAGE TupleSections #-}

module SpreadSheet
( SpreadSheet
, mkSpreadSheet
, sheet
, Cmd(..)
, Index(..)
, apply
, undo
, S.display
) where

import           Control.Monad.ST
import           Data.STRef
import           Sheet (Sheet, mkSheet, Index)
import qualified Sheet as S

-- | A @SpreadSheet@ stores both a "Sheet" and a history of modifications
-- performend on the @Sheet@.
data SpreadSheet s = SpreadSheet !(Sheet s) !(STRef s [Cmd]) -- [1]

-- [1] The history is represented as a list of inverses of the operations that
-- were previously performed on the sheet. See 'invert' for details.

mkSpreadSheet :: Int -> ST s (SpreadSheet s)
mkSpreadSheet n = SpreadSheet <$> mkSheet n <*> newSTRef []

sheet :: SpreadSheet s -> Sheet s
sheet (SpreadSheet s _) = s

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

apply :: Cmd -> SpreadSheet s -> ST s ()
apply cmd (SpreadSheet s historyRef) = do
    invertedCmd <- invert cmd s
    modifySTRef historyRef (invertedCmd :)
    applyToSheet cmd s

undo :: SpreadSheet s -> ST s ()
undo (SpreadSheet s historyRef) = do
    history <- readSTRef historyRef
    case history of
      []           -> error "undo: empty history" -- [1]
      (cmd : cmds) -> applyToSheet cmd s >> writeSTRef historyRef cmds

-- [1] This is not proper error handling, but we don't care about that
-- here.
