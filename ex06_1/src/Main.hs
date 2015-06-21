module Main where

import qualified Data.Text.Lazy.IO as T (putStrLn)
import           Control.Monad.ST (stToIO)
import           SpreadSheet

main :: IO ()
main = do
    sheetStr <- stToIO $ do
      s <- mkSpreadSheet Nothing 2
      apply (Put (Index 0) 0) s
      apply (Put (Index 0) 1) s
      undo s
      apply (Put (Index 1) 1) s
      display ',' $ sheet s

    T.putStrLn sheetStr
