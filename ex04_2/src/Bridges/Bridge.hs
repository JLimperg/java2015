{-# LANGUAGE FlexibleContexts #-}

module Bridges.Bridge
( Workers
, WorkUnits
, BridgeID
, Bridge
, BridgeState(..)
, BridgeCommand(..)
, bridge
, printBridgeState
)
where

import           Bridges.Auto
import           Control.Auto
import           Control.Monad.Writer.Strict
import           Control.Monad.Except
import           Prelude hiding ((.), id)

type Workers = Int
type WorkUnits = Int
type BridgeID = Int
type Bridge =
    Auto (Either String) (Workers, Maybe BridgeCommand) (BridgeState, Workers)

data BridgeState
    = Open
    | Closed
    | Opening Int
    | Closing Int
  deriving (Eq, Ord, Read, Show)

data BridgeCommand
    = DoOpen
    | DoClose
  deriving (Eq, Ord, Read, Show, Enum)

bridge :: (Show name) => name -> WorkUnits -> WorkUnits -> Bridge
bridge name openWorkUnits closeWorkUnits =
    mkStateZoomM_ stateTransformer id (Closed, 0)
  where
    stateTransformer (workerDiff, cmd) (currentState, currentWorkers) = do
        s <- processCommand cmd currentState
        w <- assignWorkers s currentWorkers workerDiff
        return (performWork w s, w)

    performWork :: Workers -> BridgeState -> BridgeState
    performWork workers state = case state of
        Opening n -> finishOrContinue Open   Opening (n - workers)
        Closing n -> finishOrContinue Closed Closing (n - workers)
        _         -> state
      where
        finishOrContinue done continue remainingWork
            | remainingWork <= 0 = done
            | otherwise          = continue remainingWork

    processCommand :: (MonadError String m)
                   => Maybe BridgeCommand -> BridgeState -> m BridgeState
    processCommand (Just DoOpen ) Closed = return $ Opening openWorkUnits
    processCommand (Just DoClose) Open   = return $ Closing closeWorkUnits
    processCommand Nothing        s      = return s
    processCommand _              _      = throwBridgeError "Other command in progress."

    assignWorkers :: (MonadError String m)
                  => BridgeState -> Workers -> Workers -> m Workers
    assignWorkers state currentWorkers workerDiff = case state of
        Open   -> ensureGE 0
        Closed -> ensureGE 0
        _      -> ensureGE 1
      where
        newWorkers = currentWorkers + workerDiff
        ensureGE n | newWorkers >= n = return newWorkers
                   | otherwise       = throwBridgeError "Illegal worker assignment."

    throwBridgeError :: (MonadError String m) => String -> m a
    throwBridgeError msg = throwError $ "B[" ++ show name ++ "]: " ++ msg

printBridgeState :: BridgeID -> (BridgeState, Workers) -> String
printBridgeState bid (state, workers) =
    "B" ++ show bid ++ ": S='" ++ show state ++ "' W=" ++ show workers
