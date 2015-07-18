{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MonadRepl
( ReplT
, runReplT
, liftBase
, lookupVarE
, assignVar
, getEnv
, getWatchList
, addToWatchList
, removeFromWatchList
, replError
, handleErrors
, getInputLine
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Console.Haskeline (InputT, MonadException)
import qualified System.Console.Haskeline as HL

import           Eval.Env

newtype ReplT m a
    = ReplT { fromReplT :: StateT (Env, Set Name) (ExceptT Text (InputT m)) a }
  deriving (Functor, Applicative, Monad)

runReplT :: (MonadException m)
         => (Env, Set Name) -> ReplT m a -> m (Either Text (a, (Env, Set Name)))
runReplT initialState action
    = HL.runInputT HL.defaultSettings
    . runExceptT
    . runStateT (fromReplT action)
    $ initialState

-------------------------------------------------------------------------------
-- Helpers (a.k.a. reinvention of mtl)

getState :: (Monad m) => ReplT m (Env, Set Name)
getState = ReplT get

modifyState :: (Monad m) => ((Env, Set Name) -> (Env, Set Name)) -> ReplT m ()
modifyState = ReplT . modify

modifyEnv :: (Monad m) => (Env -> Env) -> ReplT m ()
modifyEnv = modifyState . first

modifyWatchList :: (Monad m) => (Set Name -> Set Name) -> ReplT m ()
modifyWatchList = modifyState . second

putState :: (Monad m) => (Env, Set Name) -> ReplT m ()
putState = modifyState . const

getEnv :: (Monad m) => ReplT m Env
getEnv = fst <$> getState

getWatchList :: (Monad m) => ReplT m (Set Name)
getWatchList = snd <$> getState

liftBase :: (Monad m) => m a -> ReplT m a
liftBase = ReplT . lift . lift . lift

-------------------------------------------------------------------------------
-- MonadRepl-specific functionality

lookupVar :: (Monad m) => Name -> ReplT m (Maybe Integer)
lookupVar varname = lookupEnv varname <$> getEnv

lookupVarE :: (Monad m) => Name -> ReplT m Integer
lookupVarE varname = lookupVar varname >>= maybe
    (ReplT . lift . throwE $ "undefined variable: " <> fromName varname)
    return

assignVar :: (Monad m) => Name -> Integer -> ReplT m ()
assignVar varname val = modifyEnv (assignEnv varname val)

addToWatchList :: (Monad m) => Name -> ReplT m ()
addToWatchList = modifyWatchList . Set.insert

removeFromWatchList :: (Monad m) => Name -> ReplT m ()
removeFromWatchList = modifyWatchList . Set.delete

replError :: (Monad m) => Text -> ReplT m a
replError = ReplT . lift . throwE

-- | @handleErrors handler action@ executes @action@. If it succeeds, the
-- result of this function is equivalent to @action@.
-- If it fails, @handler@ is called with the error message produced
-- by @action@ and its result is returned. In this case, the effects of
-- @action@ (specifically state modifications) are not applied by the resulting
-- action.
handleErrors :: (MonadException m)
             => (Text -> ReplT m a) -> ReplT m a -> ReplT m a
handleErrors handler action = do
    s <- getState
    result <- liftBase $ runReplT s action
    case result of
      Left err              -> handler err
      Right (res, newState) -> putState newState >> return res

getInputLine :: (MonadException m) => String -> ReplT m Text
getInputLine prompt = do
    maybeLine <- ReplT . lift . lift . HL.getInputLine $ prompt
    case maybeLine of
      Nothing   -> getInputLine prompt
      Just line -> do
        let line'            = T.strip . T.pack $ line
            (prefix, suffix) = T.splitAt (T.length line' - 1) line'
        if suffix /= "\\"
           then return line'
           else (prefix <>) <$> getInputLine ""
