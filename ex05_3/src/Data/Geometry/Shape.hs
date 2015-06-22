module Data.Geometry.Shape where

import           Data.Geometry.Basics
import qualified Data.Geometry.Ellipse as Ell
import qualified Data.Geometry.Rectangle as Rect

class Shape shape where
    contains :: shape -> Point -> Bool
    offset :: shape -> Vector -> shape

instance Shape Ell.Ellipse where
    contains = Ell.contains'
    offset = Ell.offset'

instance Shape Rect.Rectangle where
    contains = Rect.contains
    offset = Rect.offset
