{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Geometry.Basics
( Point(..)
, Vector
, pattern Vector
, (.+>)
, (.->)
, (.-.)
, (.*>)
, (>+>)
, (>->)
, vectorLength
) where

#ifdef TEST
import           Test.Tasty.QuickCheck
#endif

data Point =
    Point { ptX :: ! Double
          , ptY :: ! Double
          }
  deriving (Eq, Ord, Read, Show)

newtype Vector = MkVector Point
  deriving (Eq, Ord, Read, Show
#ifdef TEST
           , Arbitrary
#endif
           )

pattern Vector x y = MkVector (Point x y)

(..+..) :: Point -> Point -> Point
(Point x1 y1) ..+.. (Point x2 y2) = Point (x1 + x2) (y1 + y2)

(..-..) :: Point -> Point -> Point
(Point x1 y1) ..-.. (Point x2 y2) = Point (x1 - x2) (y1 - y2)

(.+>) :: Point -> Vector -> Point
p .+> (MkVector v) = p ..+.. v

(.->) :: Point -> Vector -> Point
p .-> (MkVector v) = p ..-.. v

(.*>) :: Point -> Vector -> Point
(Point x1 y1) .*> (Vector x2 y2) = Point (x1 * x2) (y1 * y2)

(.-.) :: Point -> Point -> Vector
p1 .-. p2 = MkVector $ p1 ..-.. p2

(>+>) :: Vector -> Vector -> Vector
(MkVector p1) >+> (MkVector p2) = MkVector $ p1 ..+.. p2

(>->) :: Vector -> Vector -> Vector
(MkVector p1) >-> (MkVector p2) = p1 .-. p2

vectorLength :: Vector -> Double
vectorLength (Vector x y) = sqrt $ x^^^2 + y^^^2
  where
    (^^^) :: Double -> Int -> Double
    (^^^) = (^)

#ifdef TEST
instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary
#endif
