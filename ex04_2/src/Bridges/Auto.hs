{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Bridges.Auto
( mkStateZoomM_
, associateAutos_
) where

import           Control.Auto
import           Control.Auto.Collection
import           Control.Auto.Core
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Control.Monad.Error.Class

-- A variant of 'mkStateM_' where the output of the generated 'Auto' is
-- constructed from the state via a zooming function.
mkStateZoomM_ :: (Monad m)
              => (input -> state -> m state) -- State transformer
              -> (state -> output)           -- Zooming function
              -> state                       -- Initial state
              -> Auto m input output
mkStateZoomM_ stateTransformer zoom initialState =
    mkStateM_ stateTransformer' initialState
  where
    stateTransformer' input state =
        (zoom state, ) <$> stateTransformer input state

associateAutos_ :: (MonadError String m, Ord ix, Show ix)
                => Map ix (Auto m a b) -> Auto m (Map ix a) (Map ix b)
associateAutos_ autos = muxMany_ newAuto
  where
    newAuto ix   = fromMaybe (errorAuto ix) $ M.lookup ix autos
    errorAuto ix = mkConstM $ throwError $ "No Auto with index " ++ show ix
