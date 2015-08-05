{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Menu
( ID(..)
, Eurocents(..)
, Allergen(..)
, Category(..)
, MenuEntry(..)
, name
, price
, allergens
, description
, category
, Menu
, menuEntryAt
)
where

import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           Lens.Family
import           Lens.Family.Stock
import           Lens.Family.TH

newtype ID = ID { getID :: Int }
  deriving (Eq, Ord, Read, Show)

newtype Eurocents = Eurocents { getEurocents :: Integer }
  deriving (Eq, Ord, Read, Show, Num)

newtype Allergen = Allergen { getAllergen :: String }
  deriving (Eq, Ord, Read, Show)

newtype Category = Category { getCategory :: String }
  deriving (Eq, Ord, Read, Show)

data MenuEntry
    = MenuEntry
    { _name :: String
    , _price :: Eurocents
    , _allergens :: Set Allergen
    , _description :: String
    , _category :: Category
    }
  deriving (Eq, Ord, Read, Show)

$(makeLenses ''MenuEntry)

type Menu = Map ID MenuEntry

menuEntryAt :: (Functor f) => ID -> LensLike' f Menu (Maybe MenuEntry)
menuEntryAt = at
