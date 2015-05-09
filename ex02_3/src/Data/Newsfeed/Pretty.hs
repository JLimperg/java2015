module Data.Newsfeed.Pretty
( pUserName
, pTime
, pInteraction
, pTextMsg
, pEventMsg
, pFotoMsg
, pSomeMsg
, pNewsfeed
, pNewsfeedNow
, render
)
where

import           Data.List
import           Data.Time
import           Data.Time.Format.Human
import           Lens.Family2
import           Text.PrettyPrint

import           Data.Newsfeed

pUserName :: UserName -> Doc
pUserName = text . getUserName

pTime :: UTCTime -> UTCTime -> Doc
pTime now = text . humanReadableTime' now

pMsgWith :: UTCTime -> (i -> Doc) -> (s -> Doc) -> Msg i s -> Doc
pMsgWith now pInter pSpecifics m
    =   pTime now (m ^. msgCreationTime)
    $+$ pUserName (m ^. msgUserName)
    $+$ (nest 2 . pInter $ m ^. msgInteractionData)
    $+$ pSpecifics (m ^. msgSpecifics)

pInteraction :: InteractionData -> Doc
pInteraction i
    =   braces (integer (i ^. numLikes) <> space <> text "likes")
    $+$ braces ((int . length $ i ^. comments) <> space <> text "comments")

pTextMsg :: UTCTime -> TextMsg -> Doc
pTextMsg now = pMsgWith now pInteraction pTextMsgData
  where
    pTextMsgData = text . getText

pFotoMsg :: UTCTime -> FotoMsg -> Doc
pFotoMsg now = pMsgWith now pInteraction pFotoMsgData
  where
    pFotoMsgData d =   brackets (text $ d ^. fotoFileName)
                   $+$ text (d ^. fotoCaption)

pEventMsg :: UTCTime -> EventMsg -> Doc
pEventMsg now = pMsgWith now (const empty) pEventMsgData
  where
    pEventMsgData Login  = text "<login>"
    pEventMsgData Logout = text "<logout>"

pSomeMsg :: UTCTime -> SomeMsg -> Doc
pSomeMsg now (TextMsg  m) = pTextMsg  now m
pSomeMsg now (FotoMsg  m) = pFotoMsg  now m
pSomeMsg now (EventMsg m) = pEventMsg now m

pNewsfeed :: UTCTime -> Newsfeed -> Doc
pNewsfeed now = vcat . intersperse (text "") . map (pSomeMsg now) . msgsByTime

pNewsfeedNow :: Newsfeed -> IO Doc
pNewsfeedNow feed = fmap (\now -> pNewsfeed now feed) getCurrentTime
