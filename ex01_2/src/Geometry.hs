{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Geometry
( Ellipse
, mkEllipse
, contains
, offset
#ifdef TEST
, tests
#endif
)
where

import           Data.Maybe
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>), (<*>))
#endif

#ifdef TEST
import           Test.Tasty
import           Test.Tasty.QuickCheck
#endif

data Ellipse =
    Ellipse { ellX :: ! Double
            , ellY :: ! Double
            , ellRadiusX :: ! Double
            , ellRadiusY :: ! Double
            }
  deriving (Eq, Ord, Read, Show)

mkEllipse :: Double -> Double -> Double -> Double -> Maybe Ellipse
mkEllipse x y radiusX radiusY
    | radiusX >= 0 && radiusY >= 0 = Just $ Ellipse x y radiusX radiusY
    | otherwise                    = Nothing

contains :: Ellipse -> Double -> Double -> Bool
contains Ellipse {..} x y
    | ellRadiusX == 0
        && ellRadiusY == 0 = x == ellX && y == ellY
    | ellRadiusX == 0      = x == ellX && abs (y - ellY) <= ellRadiusY
    | ellRadiusY == 0      = y == ellY && abs (x - ellX) <= ellRadiusX
    | otherwise            =
        (x - ellX)^2 / ellRadiusX^2 + (y - ellY)^2 / ellRadiusY^2 <= 1

offset :: Ellipse -> Double -> Double -> Ellipse
offset e x y | x == 0 && y == 0 = e
             | otherwise        = e { ellX = ellX e + x, ellY = ellY e + y }

-------------------------------------------------------------------------------
-- Testing

#ifdef TEST

instance Arbitrary Ellipse where
    arbitrary = fromJust <$> (
        mkEllipse <$> arbitrary <*> arbitrary <*> arbPosDouble <*> arbPosDouble)
      where
        arbPosDouble = arbitrary `suchThat` (>= 0)

    shrink = shrinkNothing

tests :: TestTree
tests = props

props :: TestTree
props = testGroup "properties"
    [ testProperty "ellipsis contains own center point" $
        \e@Ellipse {..} -> contains e ellX ellY
    , testProperty "ellipsis does not contain point outside enclosing rectangle" $
        \e@Ellipse {..} x y ->
          abs (x - ellX) > ellRadiusX ||
          abs (y - ellY) > ellRadiusY ==>
          not $ contains e x y
    , testProperty "offset in X direction leaves Y invariant" $
        \e offsetX -> ellY (offset e offsetX 0) == ellY e
    , testProperty "offset in Y direction leaves X invariant" $
        \e offsetY -> ellX (offset e 0 offsetY) == ellX e
    , testProperty "offset leaves radii invariant" $
        \e offsetX offsetY ->
          ellRadiusX e == ellRadiusX (offset e offsetX offsetY) &&
          ellRadiusY e == ellRadiusY (offset e offsetX offsetY)
    ]

#endif
