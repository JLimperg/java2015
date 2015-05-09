{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Newsfeed
( ID(..)
, UserName(..)
, Msg(..)
, msgUserName
, msgCreationTime
, msgInteractionData
, msgSpecifics
, InteractionData(..) 
, numLikes
, comments
, Comment(..)
, commentAuthor
, commentCreationTime
, commentText
, TextMsg
, TextMsgData(..)
, FotoMsg
, FotoMsgData(..)
, fotoFileName
, fotoCaption
, EventMsg
, EventMsgData(..)
, SomeMsg(..)
, Newsfeed
, someMsgUserName
, someMsgCreationTime
, msgAt
, textMsgAt
, fotoMsgAt
, eventMsgAt
, emptyFeed
, addMsg
, addTextMsg
, addFotoMsg
, addEventMsg
, removeMsg
, msgsByTime
) where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Time
import           Lens.Family2
import           Lens.Family2.Unchecked
import           Lens.Family2.Stock
import           Lens.Family2.TH

newtype ID = ID { getID :: Integer }
  deriving (Eq, Ord, Read, Show)

newtype UserName = UserName { getUserName :: String }
  deriving (Eq, Ord, Read, Show)

data Msg i s
    = Msg
    { _msgUserName :: UserName
    , _msgCreationTime :: UTCTime
    , _msgInteractionData :: i
    , _msgSpecifics :: s
    }
  deriving (Eq, Ord, Read, Show)

$(makeLenses ''Msg)

data Comment
    = Comment
    { _commentAuthor :: UserName
    , _commentCreationTime :: UTCTime
    , _commentText :: String
    }
  deriving (Eq, Ord, Read, Show)

$(makeLenses ''Comment)

data InteractionData
    = InteractionData
    { _numLikes :: Integer
    , _comments :: [Comment]
    }
  deriving (Eq, Ord, Read, Show)

$(makeLenses ''InteractionData)

newtype TextMsgData = TextMsgData { getText :: String }
  deriving (Eq, Ord, Read, Show)

$(makeLenses ''TextMsgData)

data FotoMsgData
    = FotoMsgData
    { _fotoFileName :: FilePath
    , _fotoCaption :: String
    }
  deriving (Eq, Ord, Read, Show)

$(makeLenses ''FotoMsgData)

data EventMsgData
    = Login
    | Logout
  deriving (Eq, Ord, Read, Show)

type TextMsg  = Msg InteractionData TextMsgData
type FotoMsg  = Msg InteractionData FotoMsgData
type EventMsg = Msg ()              EventMsgData

data SomeMsg
    = TextMsg  TextMsg
    | FotoMsg  FotoMsg
    | EventMsg EventMsg
  deriving (Eq, Ord, Read, Show)

type Newsfeed = Map ID SomeMsg

-------------------------------------------------------------------------------
-- Additional lenses

someMsgLens :: forall a. (forall i s. Lens' (Msg i s) a) -> Lens' SomeMsg a
someMsgLens l = lens (someMsgDispatchGet $ view l) (someMsgDispatchSet $ set l)
  where
    someMsgDispatchGet :: (forall i s. Msg i s -> a) -> SomeMsg -> a
    someMsgDispatchGet f (TextMsg  m) = f m
    someMsgDispatchGet f (FotoMsg  m) = f m
    someMsgDispatchGet f (EventMsg m) = f m

    someMsgDispatchSet :: (forall i s. a -> Msg i s -> Msg i s)
                       -> SomeMsg -> a -> SomeMsg
    someMsgDispatchSet f (TextMsg  m) new = TextMsg  (f new m)
    someMsgDispatchSet f (FotoMsg  m) new = FotoMsg  (f new m)
    someMsgDispatchSet f (EventMsg m) new = EventMsg (f new m)

someMsgUserName :: Lens' SomeMsg UserName
someMsgUserName = someMsgLens msgUserName

someMsgCreationTime :: Lens' SomeMsg UTCTime
someMsgCreationTime = someMsgLens msgCreationTime

-- Produces a lens iff `f' >=> f == return`.
lensM :: forall a b c m. (Monad m)
      => (b -> m c)
      -> (c -> m b)
      -> Lens' a (m b)
      -> Lens' a (m c)
lensM f f' l = lens getter setter
  where
    getter :: a -> m c
    getter = f <=< view l
    setter :: a -> m c -> a
    setter old new = set l (new >>= f') old

toTextMsg :: SomeMsg -> Maybe TextMsg
toTextMsg (TextMsg m) = Just m
toTextMsg _           = Nothing

toFotoMsg :: SomeMsg -> Maybe FotoMsg
toFotoMsg (FotoMsg m) = Just m
toFotoMsg _           = Nothing

toEventMsg :: SomeMsg -> Maybe EventMsg
toEventMsg (EventMsg m) = Just m
toEventMsg _            = Nothing

msgAt :: ID -> Lens' Newsfeed (Maybe SomeMsg)
msgAt = at

textMsgAt :: ID -> Lens' Newsfeed (Maybe TextMsg)
textMsgAt i = lensM toTextMsg (Just . TextMsg) (at i)

fotoMsgAt :: ID -> Lens' Newsfeed (Maybe FotoMsg)
fotoMsgAt i = lensM toFotoMsg (Just . FotoMsg) (at i)

eventMsgAt :: ID -> Lens' Newsfeed (Maybe EventMsg)
eventMsgAt i = lensM toEventMsg (Just . EventMsg) (at i)

-------------------------------------------------------------------------------
-- Newsfeed manipulation

emptyFeed :: Newsfeed
emptyFeed = M.empty

addMsg :: ID -> SomeMsg -> Newsfeed -> Newsfeed
addMsg = M.insert

addTextMsg :: ID -> TextMsg -> Newsfeed -> Newsfeed
addTextMsg i m = addMsg i (TextMsg m)

addFotoMsg :: ID -> FotoMsg -> Newsfeed -> Newsfeed
addFotoMsg i m = addMsg i (FotoMsg m)

addEventMsg :: ID -> EventMsg -> Newsfeed -> Newsfeed
addEventMsg i m = addMsg i (EventMsg m)

removeMsg :: ID -> Newsfeed -> Newsfeed
removeMsg = M.delete

msgsByTime :: Newsfeed -> [SomeMsg]
msgsByTime = sortBy (compare `on` view someMsgCreationTime) . M.elems
