{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Geometry.Ellipse
( Ellipse(..)
, ellX
, ellY
, ellRadiusX
, ellRadiusY
, mkEllipse
, mkEllipse'
, contains
, contains'
, offset
, offset'
#ifdef TEST
, tests
#endif
)
where

#ifdef TEST
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.QuickCheck
#endif

import           Data.Geometry.Basics

data Ellipse =
    Ellipse { center :: Point
            , radii :: Vector
            }
  deriving (Eq, Ord, Read, Show)

ellX :: Ellipse -> Double
ellX (Ellipse (Point x _) _) = x

ellY :: Ellipse -> Double
ellY (Ellipse (Point _ y) _) = y

ellRadiusX :: Ellipse -> Double
ellRadiusX (Ellipse _ (Vector x _)) = x

ellRadiusY :: Ellipse -> Double
ellRadiusY (Ellipse _ (Vector _ y)) = y

mkEllipse' :: Point -> Vector -> Maybe Ellipse
mkEllipse' c r@(Vector rx ry)
    | rx >= 0 && ry >= 0 = Just $ Ellipse c r
    | otherwise          = Nothing

mkEllipse :: Double -> Double -> Double -> Double -> Maybe Ellipse
mkEllipse x y radiusX radiusY =
    mkEllipse' (Point x y) (Vector radiusX radiusY)

contains' :: Ellipse -> Point -> Bool
contains' e (Point x y) = contains e x y

contains :: Ellipse -> Double -> Double -> Bool
contains (Ellipse (Point cx cy) (Vector rx ry)) x y
    | rx == 0 && ry == 0 = x == cx && y == cy
    | rx == 0            = x == cx && abs (y - cy) <= ry
    | ry == 0            = y == cy && abs (x - cx) <= rx
    | otherwise          = (x - cx)^^^2 / rx^^^2 + (y - cy)^^^2 / ry^^^2 <= 1
  where
    (^^^) :: Double -> Int -> Double
    (^^^) = (^)

offset' :: Ellipse -> Vector -> Ellipse
offset' e v = e { center = center e .+> v }

offset :: Ellipse -> Double -> Double -> Ellipse
offset e x y = offset' e $ Vector x y

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
        \e -> contains e (ellX e) (ellY e)
    , testProperty "ellipsis does not contain point outside enclosing rectangle" $
        \e@(Ellipse (Point cx cy) (Vector rx ry)) x y ->
          abs (x - cx) > rx ||
          abs (y - cy) > ry ==>
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
