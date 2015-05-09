module Main where

import           Test.Tasty (defaultMain)
import qualified Data.Geometry.Ellipse as Ellipse

main :: IO ()
main = defaultMain Ellipse.tests
