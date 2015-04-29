module Main where

import           Test.Tasty (defaultMain)
import qualified Geometry

main :: IO ()
main = defaultMain Geometry.tests
