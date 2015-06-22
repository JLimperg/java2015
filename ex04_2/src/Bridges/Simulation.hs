{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Bridges.Simulation where

import           Bridges.Auto
import           Bridges.Bridge
import           Bridges.Traveller
import           Control.Auto
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Prelude hiding (id, (.))

testBridges :: [(Bridge, [(Workers, Maybe BridgeCommand)])]
testBridges = [ ( bridge (0 :: Int) 5 5
                , [(5, Just DoOpen), (0, Nothing), (0, Nothing), (0, Nothing)])
              , ( bridge (1 :: Int) 6 6
                , [(5, Just DoOpen), (1, Nothing), (0, Nothing), (0, Nothing)])
              ]

testTravellers :: [(Traveller, [Maybe TravellerCommand])]
testTravellers = [ ( traveller (0 :: Int) [0, 1] 10
                   , [Just DoContinue, Nothing, Nothing, Nothing])
                 ]

simulate :: [(Bridge, [(Workers, Maybe BridgeCommand)])]
         -> [(Traveller, [Maybe TravellerCommand])]
         -> Either String [( Map BridgeID (BridgeState, Workers)
                           , Map TravellerID (Plan, TravellerState, Patience))]
simulate bs ts =
    let bridgeMap :: Map BridgeID Bridge
        bridgeMap = fromListWithIndex . map fst $ bs
        bridgeInputs :: [Map BridgeID (Workers, Maybe BridgeCommand)]
        bridgeInputs = inputListToMaps . map snd $ bs
        travellerMap :: Map TravellerID Traveller
        travellerMap = fromListWithIndex . map fst $ ts
        travellerInputs :: [Map TravellerID (Maybe TravellerCommand)]
        travellerInputs = inputListToMaps . map snd $ ts
        inputs = zip bridgeInputs travellerInputs

        bridges = associateAutos_ bridgeMap
        travellers = associateAutos_ travellerMap

        network = proc (bridgeIn, travellerIn) -> do
          bridgeOutputs <- bridges -< bridgeIn
          let bridgeStates = M.map fst bridgeOutputs
          travellerOutputs <- travellers
                           -< M.map (flip M.lookup bridgeStates,) travellerIn
          id -< (bridgeOutputs, travellerOutputs)

    in  streamAuto network inputs
  where
    inputListToMaps :: [[input]] -> [Map Int input]
    inputListToMaps = map M.fromDistinctAscList
                    . transpose
                    . mapWithIndex (\ix -> zip (repeat ix))

printSimulationResult :: ( Map BridgeID (BridgeState, Workers)
                         , Map TravellerID (Plan, TravellerState, Patience))
                      -> String
printSimulationResult (bridgeStates, travellerStates) =
    concat . intersperse "\n" $
      M.elems (M.mapWithKey printBridgeState bridgeStates) ++
      M.elems (M.mapWithKey printTravellerState travellerStates)

fromListWithIndex :: [a] -> Map Int a
fromListWithIndex = M.fromDistinctAscList . zip [0..]

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]
