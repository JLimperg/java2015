module Bridges.Traveller
( Plan
, Patience
, TravellerID
, TravellerCommand(..)
, TravellerState(..)
, Traveller
, traveller
, printTravellerState
) where

import           Bridges.Auto
import           Bridges.Bridge
import           Control.Auto
import           Prelude hiding ((.), id)

data TravellerCommand
    = DoRest
    | DoContinue
  deriving (Eq, Ord, Read, Show)

data TravellerState
    = Resting
    | Waiting
    | Aborted
    | Done
  deriving (Eq, Ord, Read, Show)

type Plan        = [BridgeID]
type Patience    = Int
type TravellerID = Int
type Traveller   =
    Auto (Either String) (BridgeID -> Maybe BridgeState, Maybe TravellerCommand)
      (Plan, TravellerState, Patience)

traveller :: (Show name) => name -> [BridgeID] -> Patience -> Traveller
traveller name initialPlan initialPatience =
    mkStateZoomM_ stateTransformer id (initialPlan, Resting, initialPatience)
  where
    stateTransformer (bridgeStatus, cmd) (plan, state, patience) = do
        let state' = maybe state (executeCommand plan patience) cmd
        (state'', plan') <- advanceMaybe state' plan
        return (plan', state'', patience - 1)
      where
        executeCommand []    _ _          = Done    -- [1]
        executeCommand _     0 _          = Aborted
        executeCommand _     _ DoRest     = Resting
        executeCommand (_:_) _ DoContinue = Waiting

        advanceMaybe Waiting p@(b:bs)
            | bridgeStatus b == Just Open && not (null bs) = Right (Waiting, bs)
            | bridgeStatus b == Just Open                  = Right (Done, [])
            | bridgeStatus b == Nothing                    = Left $ errNoBridge
            | otherwise                                    = Right (Waiting, p)
          where
            errNoBridge =
              errPrefix ++ "Could not retrieve status for bridge " ++ show b
        advanceMaybe s p = Right (s, p)

        errPrefix = "T[" ++ show name ++ "]: "

-- [1] Note the somewhat tricky semantics here:
--
--   - If we don't wish to visit any more bridges, we are done, even if
--     the current command orders us to rest or we have run out of
--     patience.
--   - If we have run out of patience but wish to visit more bridges, we
--     abort, even if the current command orders us to rest.
--   - Otherwise, we rest or wait depending on the current command.
--
-- The exercise description is pretty vague here, so there are other
-- reasonable interpretations.

printTravellerState :: TravellerID -> (Plan, TravellerState, Patience) -> String
printTravellerState tid (plan, state, patience) =
    "T" ++ show tid ++ ": Pat=" ++ show patience ++ " S='" ++ show state ++
    "' Plan=" ++ show plan
