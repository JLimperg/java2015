module Data.Geometry.Rectangle where

import           Data.Geometry.Basics

data Rectangle
    = Rectangle
    { topLeft :: Point
    , sideLengths :: Vector
    }
  deriving (Eq, Ord, Read, Show)

mkRectangle :: Point -> Vector -> Maybe Rectangle
mkRectangle anchor sides@(Vector x y)
    | x >= 0 && y >= 0 = Just $ Rectangle anchor sides
    | otherwise        = Nothing

contains :: Rectangle -> Point -> Bool
contains (Rectangle (Point topx topy) (Vector lx ly)) (Point x y) =
    x >= topx && y >= topy &&
    x <= topx + lx && y <= topy + ly

offset :: Rectangle -> Vector -> Rectangle
offset r v = r { topLeft = topLeft r .+> v }
