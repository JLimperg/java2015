module Main where

import           Data.Time
import           Data.Newsfeed
import           Data.Newsfeed.Pretty

main :: IO ()
main = do
    now <- getCurrentTime
    let time1    = addUTCTime (negate 30) now
        time2    = addUTCTime (negate 120) now
        time3    = addUTCTime (negate 10000) now
        textMsg  = Msg (UserName "user1") time1 (InteractionData 8 [])
                     (TextMsgData "text")
        fotoMsg  = Msg (UserName "user2") time2 (InteractionData 10 [])
                     (FotoMsgData "fname" "caption")
        eventMsg = Msg (UserName "user3") time3 () Login
        feed     = addTextMsg  (ID 1) textMsg
                 . addFotoMsg  (ID 2) fotoMsg
                 . addEventMsg (ID 3) eventMsg
                 $ emptyFeed
    putStrLn . render $ pNewsfeed now feed
