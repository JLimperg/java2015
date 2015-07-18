module Eval.Env
( Name(..)
, Env
, emptyEnv
, assignEnv
, lookupEnv
) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

newtype Name = Name { fromName :: Text }
  deriving (Read, Show, Eq, Ord)

newtype Env = Env { fromEnv :: Map Name Integer }

emptyEnv :: Env
emptyEnv = Env M.empty

assignEnv :: Name -> Integer -> Env -> Env
assignEnv varname val = Env . M.insert varname val . fromEnv

lookupEnv :: Name -> Env -> Maybe Integer
lookupEnv varname = M.lookup varname . fromEnv

