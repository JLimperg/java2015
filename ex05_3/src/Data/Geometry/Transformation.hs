module Data.Geometry.Transformation where

import           Data.Geometry.Basics
import           Data.Geometry.Shape

data ShapeTrans shape = ShapeTrans !(Point -> Point) !shape

instance (Shape shape) => Shape (ShapeTrans shape) where
    offset (ShapeTrans trans shape) off = ShapeTrans trans (offset shape off)
    contains (ShapeTrans trans shape) pt = contains shape (trans pt)

mkOffset :: Vector -> shape -> ShapeTrans shape
mkOffset v = ShapeTrans (.-> v)

mkScale :: Vector -> shape -> ShapeTrans shape
mkScale v = ShapeTrans (.*> v)

mkRotate :: Double -> shape -> ShapeTrans shape
mkRotate angle = ShapeTrans $ rotate (2 * pi - angle)
  where
    rotate ang (Point x y) =
        let x' = x * (cos ang) - y * (sin ang)
            y' = x * (sin ang) + y * (cos ang)
        in Point x' y'
