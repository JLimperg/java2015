{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Data.Geometry.Basics
( Point(..)
, Vector(..)
, mkVector
, (.+>)
, (.->)
, (.-.)
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

newtype Vector = Vector { getVector :: Point }
  deriving (Eq, Ord, Read, Show
#ifdef TEST
           , Arbitrary
#endif
           )

mkVector :: Double -> Double -> Vector
mkVector x y = Vector $ Point x y

(..+..) :: Point -> Point -> Point
(Point x1 y1) ..+.. (Point x2 y2) = Point (x1 + x2) (y1 + y2)

(..-..) :: Point -> Point -> Point
(Point x1 y1) ..-.. (Point x2 y2) = Point (x1 - x2) (y1 - y2)

(.+>) :: Point -> Vector -> Point
p .+> (Vector v) = p ..+.. v

(.->) :: Point -> Vector -> Point
p .-> (Vector v) = p ..-.. v

(.-.) :: Point -> Point -> Vector
p1 .-. p2 = Vector $ p1 ..-.. p2

(>+>) :: Vector -> Vector -> Vector
(Vector p1) >+> (Vector p2) = Vector $ p1 ..+.. p2

(>->) :: Vector -> Vector -> Vector
(Vector p1) >-> (Vector p2) = p1 .-. p2

vectorLength :: Vector -> Double
vectorLength (Vector (Point x y)) = sqrt $ x^2 + y^2

#ifdef TEST
instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary
#endif
